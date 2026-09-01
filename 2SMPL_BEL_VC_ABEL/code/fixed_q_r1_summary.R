# Fixed-Q calibration for the r = 1 bridge and convergence experiments.
# It reuses saved outer simulations and computes the Gaussian reference law.

source("fixed_q_reference.R")

outer_dir <- file.path(FIXEDQ_ROOT, "outer_B10000")
ff <- fixedq_outer_files(outer_dir)

if (length(ff) != 46L) {
  stop("Expected 46 saved outer RDS files, found ", length(ff))
}

# Recompile only the wrapper engine for the new R session.
# This does not rerun the outer Monte Carlo.
compile_fixedq_engine(rebuild = TRUE)
validate_fixedq_engine(n_tests = 50L)

# Check the true relative block scales in the saved design.
# In the bridge and convergence designs both series use the same AR(1)
# dependence, the same block length, and the same long-run variance scale.
# Therefore both oracle r values should be 1, up to floating-point error.
objs <- lapply(ff, readRDS)

truth_check <- do.call(rbind, lapply(objs, function(a) {
  data.frame(
    scenario_id = a$meta$scenario_id,
    Q1 = a$meta$Q1,
    Q2 = a$meta$Q2,
    M1 = a$meta$M1,
    M2 = a$meta$M2,
    r_true_asym = as.numeric(a$truth$r_true_asym),
    r_true_block = as.numeric(a$truth$r_true_block),
    stringsAsFactors = FALSE
  )
}))

write.csv(
  truth_check,
  file.path(FIXEDQ_ROOT, "FIXEDQ_TRUE_R_CHECK.csv"),
  row.names = FALSE
)


if (max(abs(truth_check$r_true_asym - 1), na.rm = TRUE) > 1e-10 ||
    max(abs(truth_check$r_true_block - 1), na.rm = TRUE) > 1e-10) {
  stop("At least one saved design does not have oracle r=1. Stop and inspect FIXEDQ_TRUE_R_CHECK.csv.")
}

# The corrected primary reference grid.
#
# Only ONE reference value, r=1, is needed for every distinct (Q1,Q2) pair in
# the simulation design.  This avoids the pathological grid created
# by rare rhat_block values near zero.
qpairs <- unique(truth_check[, c("Q1", "Q2")])
qpairs <- qpairs[order(qpairs$Q1, qpairs$Q2), , drop = FALSE]

grid <- data.frame(
  Q1 = qpairs$Q1,
  Q2 = qpairs$Q2,
  r = 1,
  stringsAsFactors = FALSE
)

write.csv(
  grid,
  file.path(FIXEDQ_ROOT, "REFERENCE_R_GRID_R1_ONLY.csv"),
  row.names = FALSE
)


# Reference simulation.
#
# 100,000 draws are enough for this stage.  The binomial MCSE of a CDF value
# near 0.95 is about 0.00069, comfortably below the outer B=10,000 MCSE
# (~0.00218).  We can later increase only the final few critical values if
# needed.
B_ref <- 100000L
threads <- max(1L, parallel::detectCores(logical = TRUE) - 1L)

ref_file <- file.path(
  FIXEDQ_ROOT,
  paste0("REFERENCE_CRITICAL_VALUES_R1_B", B_ref, ".csv")
)

ref <- run_fixedq_reference_grid(
  grid = grid,
  B_ref = B_ref,
  threads = threads,
  out_file = ref_file,
  allow_small_B = FALSE
)

# Exact ordinary-BEL convex-hull check at r=1.

if (any(abs(ref$BEL_failure_z) > 5, na.rm = TRUE)) {
  warning("At least one r=1 BEL failure estimate is more than 5 MCSE from the exact formula.")
}

# Summarize the 46 SAVED outer scenarios.
# Same VC+ABEL statistic, two reference laws:
#   1) chi-square
#   2) fixed-(Q1,Q2) Gaussian law at the correct oracle r=1
chi95 <- qchisq(.95, 1)

get_ref_row <- function(Q1, Q2) {
  z <- ref[ref$Q1 == Q1 & ref$Q2 == Q2 & abs(ref$r - 1) < 1e-12, , drop = FALSE]
  if (nrow(z) != 1L) {
    stop("Expected exactly one r=1 reference row for Q=(", Q1, ",", Q2, ")")
  }
  z
}

rows <- vector("list", length(objs))

for (i in seq_along(objs)) {
  a <- objs[[i]]
  m <- a$meta
  S <- a$stats
  W <- S[, "VCABEL_feasible"]

  rr <- get_ref_row(m$Q1, m$Q2)
  cfix <- rr$crit95[1]

  rows[[i]] <- data.frame(
    scenario_id = m$scenario_id,
    group = m$group,
    size_pattern = if ("size_pattern" %in% names(m)) m$size_pattern else NA_character_,
    dist_pattern = if ("dist_pattern" %in% names(m)) m$dist_pattern else NA_character_,
    N1 = m$N1,
    N2 = m$N2,
    M1 = m$M1,
    M2 = m$M2,
    Q1 = m$Q1,
    Q2 = m$Q2,
    innov1 = m$innov1,
    innov2 = m$innov2,
    phi1 = m$p11,
    phi2 = m$p21,
    B = m$B,
    r_true = 1,
    crit_chisq = chi95,
    crit_fixedQ_r1 = cfix,
    coverage_chisq = mean(is.finite(W) & W <= chi95),
    coverage_fixedQ_r1 = mean(is.finite(W) & W <= cfix),
    fixedQ_predicted_chisq_coverage = rr$fixedQ_chi_coverage[1],
    BEL_failure = mean(S[, "bel_fail"] > .5, na.rm = TRUE),
    ABEL_failure = mean(S[, "abel_feasible_fail"] > .5, na.rm = TRUE),
    ABEL_fallback = mean(S[, "abel_feasible_solver_fallback"] > .5, na.rm = TRUE),
    JK_fallback = mean(S[, "jk_fallback"] > .5, na.rm = TRUE),
    VC_safeguard = mean(S[, "vc_safeguard"] > .5, na.rm = TRUE),
    alpha_hat_lt_025 = mean(S[, "alpha_hat_lt_025"] > .5, na.rm = TRUE),
    alpha_hat_mean = mean(S[, "alpha_hat"], na.rm = TRUE),
    nuhat_mean = mean(S[, "nuhat_JK"], na.rm = TRUE),
    rhat_asym_mean = mean(S[, "rhat_asym"], na.rm = TRUE),
    rhat_asym_sd = sd(S[, "rhat_asym"], na.rm = TRUE),
    rhat_asym_q01 = unname(quantile(S[, "rhat_asym"], .01, na.rm = TRUE, type = 8)),
    rhat_asym_q99 = unname(quantile(S[, "rhat_asym"], .99, na.rm = TRUE, type = 8)),
    rhat_block_mean = mean(S[, "rhat_block"], na.rm = TRUE),
    rhat_block_sd = sd(S[, "rhat_block"], na.rm = TRUE),
    mcse_at_95 = sqrt(.95 * .05 / m$B),
    stringsAsFactors = FALSE
  )
}

out <- do.call(rbind, rows)
out <- out[order(out$group, out$Q1, out$Q2, out$M1, out$dist_pattern), ]
rownames(out) <- NULL

detailed_file <- file.path(
  FIXEDQ_ROOT,
  "FIXEDQ_CALIBRATION_DETAILED_R1.csv"
)
write.csv(out, detailed_file, row.names = FALSE)

# Bridge averages across the three innovation pairs.
bridge <- out[out$group == "smallQ_bridge", , drop = FALSE]

mean_or_na <- function(x) {
  if (all(!is.finite(x))) NA_real_ else mean(x[is.finite(x)])
}

if (nrow(bridge)) {
  key <- interaction(
    bridge$size_pattern,
    bridge$Q1,
    bridge$Q2,
    drop = TRUE
  )
  spl <- split(bridge, key)

  agg <- do.call(rbind, lapply(spl, function(z) {
    data.frame(
      size_pattern = z$size_pattern[1],
      Q1 = z$Q1[1],
      Q2 = z$Q2[1],
      M = z$M1[1],
      mean_BEL_failure = mean_or_na(z$BEL_failure),
      mean_chisq_coverage = mean_or_na(z$coverage_chisq),
      mean_fixedQ_r1_coverage = mean_or_na(z$coverage_fixedQ_r1),
      mean_predicted_chisq_coverage = mean_or_na(z$fixedQ_predicted_chisq_coverage),
      stringsAsFactors = FALSE
    )
  }))

  rownames(agg) <- NULL
  agg <- agg[order(agg$size_pattern, -agg$Q1), ]

  bridge_file <- file.path(
    FIXEDQ_ROOT,
    "FIXEDQ_BRIDGE_AVERAGES_R1.csv"
  )
  write.csv(agg, bridge_file, row.names = FALSE)

}

# Fixed-Q convergence table.
conv <- out[out$group == "fixedQ_convergence", , drop = FALSE]

if (nrow(conv)) {
  conv <- conv[order(conv$Q1, conv$dist_pattern, conv$M1), ]

  conv_file <- file.path(
    FIXEDQ_ROOT,
    "FIXEDQ_CONVERGENCE_R1.csv"
  )
  write.csv(conv, conv_file, row.names = FALSE)

}

