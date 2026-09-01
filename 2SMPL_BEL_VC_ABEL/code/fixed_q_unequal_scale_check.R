# Focused fixed-Q check for unequal block scales.
# It covers r = 0.5 and r = 2 and the long-block convergence cases.

source("fixed_q_reference.R")

compile_fixedq_engine(rebuild = TRUE)
validate_fixedq_engine(n_tests = 50L)

threads <- max(1L, parallel::detectCores(logical = TRUE) - 1L)
B_outer <- 10000L
B_ref <- 50000L

FINAL_ROOT <- file.path(FIXEDQ_ROOT, "focused_validation")
dir.create(FINAL_ROOT, showWarnings = FALSE, recursive = TRUE)

# 1. Build focused outer design
rows <- list()
k <- 0L

add_row <- function(z, size_pattern, dist_pattern) {
  k <<- k + 1L
  z$size_pattern <- size_pattern
  z$dist_pattern <- dist_pattern
  rows[[k]] <<- z
}

# A. Extend Q=(4,4) convergence to longer blocks.
for (M in c(192L, 384L)) {
  for (dd in list(
    list(label = "normal_normal", i1 = "normal", i2 = "normal"),
    list(label = "chisq1_chisq1", i1 = "chisq1", i2 = "chisq1")
  )) {
    z <- new_fixedq_row(
      id = sprintf("convext_Q4_4_M%d_%s", M, dd$label),
      group = "convergence_extension",
      Q1 = 4L, Q2 = 4L,
      M1 = M, M2 = M,
      p11 = 0.8, p21 = 0.8,
      innov1 = dd$i1, innov2 = dd$i2
    )
    add_row(z, "fixedQ", dd$label)
  }
}

# B. Unequal-block tests.
#
# With the same process scale in the two samples,
#     r = sqrt{ (sigma1^2/M1) / (sigma2^2/M2) }
#       = sqrt(M2/M1).
#
# Hence (M1,M2)=(384,96) gives r=0.5 and the reverse gives r=2.
# The long blocks make the fixed-Q Gaussian approximation relevant, while this
# directly tests the unequal-block formulation used in the paper.
for (qq in list(c(4L, 4L), c(6L, 6L))) {
  for (mm in list(
    list(label = "r05", M1 = 384L, M2 = 96L),
    list(label = "r20", M1 = 96L,  M2 = 384L)
  )) {
    for (dd in list(
      list(label = "normal_normal", i1 = "normal", i2 = "normal"),
      list(label = "normal_chisq1", i1 = "normal", i2 = "chisq1")
    )) {
      z <- new_fixedq_row(
        id = sprintf(
          "plugin_Q%d_%d_%s_M%d_%d_%s",
          qq[1], qq[2], mm$label, mm$M1, mm$M2, dd$label
        ),
        group = "plugin_r",
        Q1 = qq[1], Q2 = qq[2],
        M1 = mm$M1, M2 = mm$M2,
        p11 = 0.8, p21 = 0.8,
        innov1 = dd$i1, innov2 = dd$i2
      )
      add_row(z, mm$label, dd$label)
    }
  }
}

# Harmonize and number.
all_names <- unique(unlist(lapply(rows, names)))
rows <- lapply(rows, function(z) {
  miss <- setdiff(all_names, names(z))
  for (nm in miss) z[[nm]] <- NA
  z[all_names]
})
design <- do.call(rbind, rows)
rownames(design) <- NULL
design$scenario_no <- seq_len(nrow(design))

write.csv(
  design,
  file.path(FINAL_ROOT, "FINAL_FOCUSED_DESIGN.csv"),
  row.names = FALSE
)


# 2. Run only these focused outer simulations
outer_dir <- file.path(FINAL_ROOT, "outer_B10000")

run_fixedq_outer_design(
  design = design,
  B = B_outer,
  threads = threads,
  out_dir = outer_dir,
  base_seed = 2026083191,
  overwrite = FALSE,
  allow_small_B = FALSE
)

ff <- fixedq_outer_files(outer_dir)
if (length(ff) != nrow(design)) {
  stop("Expected ", nrow(design), " focused outer files, found ", length(ff))
}
objs <- lapply(ff, readRDS)

# Check truth.
truth <- do.call(rbind, lapply(objs, function(a) {
  data.frame(
    scenario_id = a$meta$scenario_id,
    Q1 = a$meta$Q1, Q2 = a$meta$Q2,
    M1 = a$meta$M1, M2 = a$meta$M2,
    r_true_asym = as.numeric(a$truth$r_true_asym),
    r_true_block = as.numeric(a$truth$r_true_block),
    stringsAsFactors = FALSE
  )
}))
write.csv(truth, file.path(FINAL_ROOT, "FINAL_FOCUSED_TRUE_R.csv"), row.names = FALSE)


# 3. Small controlled reference grid for Q=(4,4) and Q=(6,6)
#
# A fixed log grid [0.2,5] prevents rare nuisance estimates from creating an
# enormous grid.  We explicitly insert r=0.5,1,2.
rgrid <- sort(unique(c(
  exp(seq(log(0.2), log(5), length.out = 17L)),
  0.5, 1, 2
)))

grid <- do.call(rbind, lapply(
  list(c(4L, 4L), c(6L, 6L)),
  function(qq) data.frame(Q1 = qq[1], Q2 = qq[2], r = rgrid)
))
rownames(grid) <- NULL

write.csv(
  grid,
  file.path(FINAL_ROOT, "FINAL_FOCUSED_R_GRID.csv"),
  row.names = FALSE
)


ref_file <- file.path(
  FINAL_ROOT,
  paste0("FINAL_FOCUSED_REFERENCE_B", B_ref, ".csv")
)

ref <- run_fixedq_reference_grid(
  grid = grid,
  B_ref = B_ref,
  threads = threads,
  base_seed = 2026083197,
  out_file = ref_file,
  allow_small_B = TRUE
)

# 4. Coverage summary
chi95 <- qchisq(.95, 1)

summary_rows <- vector("list", length(objs))

for (i in seq_along(objs)) {
  a <- objs[[i]]
  m <- a$meta
  S <- a$stats
  W <- S[, "VCABEL_feasible"]

  q1 <- m$Q1
  q2 <- m$Q2
  rtrue <- as.numeric(a$truth$r_true_asym)
  rhat <- S[, "rhat_asym"]

  c_oracle <- fixedq_interp(ref, q1, q2, rtrue, "crit95")
  c_hat <- fixedq_interp(ref, q1, q2, rhat, "crit95")

  grid_miss <- mean(!is.finite(c_hat))
  covered_grid <- is.finite(c_hat)

  # We do not clamp or extrapolate.  Grid misses are reported explicitly.
  cov_hat <- if (all(covered_grid)) {
    mean(is.finite(W) & W <= c_hat)
  } else {
    NA_real_
  }

  summary_rows[[i]] <- data.frame(
    scenario_id = m$scenario_id,
    group = m$group,
    dist_pattern = m$dist_pattern,
    Q1 = q1, Q2 = q2,
    M1 = m$M1, M2 = m$M2,
    N1 = m$N1, N2 = m$N2,
    r_true_asym = rtrue,
    r_true_block = as.numeric(a$truth$r_true_block),
    rhat_asym_mean = mean(rhat, na.rm = TRUE),
    rhat_asym_sd = sd(rhat, na.rm = TRUE),
    rhat_asym_q001 = unname(quantile(rhat, .001, na.rm = TRUE, type = 8)),
    rhat_asym_q01 = unname(quantile(rhat, .01, na.rm = TRUE, type = 8)),
    rhat_asym_q50 = unname(quantile(rhat, .50, na.rm = TRUE, type = 8)),
    rhat_asym_q99 = unname(quantile(rhat, .99, na.rm = TRUE, type = 8)),
    rhat_asym_q999 = unname(quantile(rhat, .999, na.rm = TRUE, type = 8)),
    crit_chisq = chi95,
    crit_fixedQ_oracle = as.numeric(c_oracle),
    coverage_chisq = mean(is.finite(W) & W <= chi95),
    coverage_fixedQ_oracle = mean(is.finite(W) & W <= c_oracle),
    coverage_fixedQ_rhat = cov_hat,
    rhat_grid_miss = grid_miss,
    BEL_failure = mean(S[, "bel_fail"] > .5, na.rm = TRUE),
    ABEL_failure = mean(S[, "abel_feasible_fail"] > .5, na.rm = TRUE),
    ABEL_fallback = mean(S[, "abel_feasible_solver_fallback"] > .5, na.rm = TRUE),
    JK_fallback = mean(S[, "jk_fallback"] > .5, na.rm = TRUE),
    VC_safeguard = mean(S[, "vc_safeguard"] > .5, na.rm = TRUE),
    mcse_at_95 = sqrt(.95 * .05 / m$B),
    stringsAsFactors = FALSE
  )
}

ans <- do.call(rbind, summary_rows)
ans <- ans[order(ans$group, ans$Q1, ans$r_true_asym, ans$M1, ans$dist_pattern), ]
rownames(ans) <- NULL

out_file <- file.path(FINAL_ROOT, "FINAL_FOCUSED_RESULTS.csv")
write.csv(ans, out_file, row.names = FALSE)

# 5. Console summaries


