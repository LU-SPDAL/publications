# Small reproducibility checks for the numerical values reported in the paper.
# The script reruns a few selected cells and the ABEL solver validation.

source("simulation_driver.R")

dc <- parallel::detectCores(logical = TRUE)
if (is.na(dc)) dc <- 1L
N_THREADS <- max(1L, as.integer(dc) - 1L)

OUT_ROOT <- file.path(SIM_DIR, "..", "results", "validation", "reported_values_check")
dir.create(OUT_ROOT, showWarnings = FALSE, recursive = TRUE)

compile_abel_engine(rebuild = TRUE)

# Solver comparison used for the reported numerical validation.
solver_check <- validate_abel_solver(
  n_tests = 500L,
  seed = 29082026,
  tol = 5e-7,
  save_worst = TRUE
)
write.csv(solver_check,
          file.path(OUT_ROOT, "SOLVER_VALIDATION_500.csv"),
          row.names = FALSE)

# Helpers
get_cov <- function(cov, id, method, nominal = 0.95, field = "coverage_fail_as_miss") {
  ii <- cov$scenario_id == id & cov$method == method &
    abs(as.numeric(cov$nominal) - nominal) < 1e-12
  if (!any(ii)) return(NA_real_)
  as.numeric(cov[ii, field][1L])
}

check_row <- function(section, item, observed, expected, digits = 3L) {
  data.frame(
    section = section,
    item = item,
    observed = observed,
    expected_manuscript = expected,
    difference = observed - expected,
    rounded_match = isTRUE(round(observed, digits) == round(expected, digits)),
    digits_checked = digits,
    stringsAsFactors = FALSE
  )
}

# Primary strong-persistence and bandwidth checks

# Keep the original scenario order so the saved seeds are reproduced.
core <- build_core_design()

# Four N=216, phi=(0.8,0.8) cells used in the reported average.
sel_primary <- core[
  core$group == "coverage_main" &
    core$N1 == 216L & core$N2 == 216L &
    abs(core$p11 - 0.8) < 1e-12 & abs(core$p21 - 0.8) < 1e-12,
  , drop = FALSE
]

# Five bandwidth-sensitivity cells.
sel_bw <- core[core$group == "bandwidth_sensitivity", , drop = FALSE]

# One large-N skewed cell for the ABEL and BC comparison.
sel_equiv <- core[
  core$group == "coverage_main" &
    core$N1 == 1728L & core$N2 == 1728L &
    abs(core$p11 - 0.8) < 1e-12 & abs(core$p21 - 0.8) < 1e-12 &
    core$innov1 == "chisq1" & core$innov2 == "chisq1",
  , drop = FALSE
]

sent_core <- rbind(sel_primary, sel_bw, sel_equiv)
# Remove duplicate row if any.
sent_core <- sent_core[!duplicated(sent_core$scenario_id), , drop = FALSE]

core_dir <- file.path(OUT_ROOT, "core_checks")
run_design(
  sent_core,
  out_dir = core_dir,
  B = 10000L,
  threads = N_THREADS,
  base_seed = 20260829,
  overwrite = TRUE,
  allow_small_B = FALSE
)

cov_core <- summarize_coverage(core_dir, levels = c(0.90, 0.95, 0.99))
diag_core <- summarize_diagnostics(core_dir)
eq_core <- summarize_equivalence(core_dir)

# Main-text strong-persistence average over the four innovation pairs.
prim95 <- cov_core[
  cov_core$group == "coverage_main" & cov_core$N1 == 216L &
    abs(cov_core$p11 - 0.8) < 1e-12 & abs(cov_core$p21 - 0.8) < 1e-12 &
    abs(as.numeric(cov_core$nominal) - 0.95) < 1e-12,
  , drop = FALSE
]
prim_avg <- aggregate(
  coverage_fail_as_miss ~ method,
  data = prim95,
  FUN = mean
)
write.csv(prim_avg,
          file.path(OUT_ROOT, "PRIMARY_N216_PHI08_AVERAGES.csv"),
          row.names = FALSE)

# Bandwidth values reported in the manuscript.
bw95 <- cov_core[
  cov_core$group == "bandwidth_sensitivity" &
    cov_core$method == "VCABEL_feasible" &
    abs(as.numeric(cov_core$nominal) - 0.95) < 1e-12,
  , drop = FALSE
]
bw95 <- bw95[match(c("M23", "log1", "loglog", "log2", "old4log"), bw95$L_rule), , drop = FALSE]
write.csv(bw95,
          file.path(OUT_ROOT, "BANDWIDTH_VCABEL_95.csv"),
          row.names = FALSE)

# Small-Q Q1=4 checks

build_smallQ_design_local <- function() {
  M_values <- c(12L, 18L, 24L, 36L, 54L)
  size_specs <- list(
    list(label = "balanced", N1 = 216L, N2 = 216L),
    list(label = "imbalance_1to2", N1 = 216L, N2 = 432L)
  )
  dist_specs <- list(
    list(label = "normal_normal", i1 = "normal", i2 = "normal"),
    list(label = "chisq1_chisq1", i1 = "chisq1", i2 = "chisq1"),
    list(label = "normal_chisq1", i1 = "normal", i2 = "chisq1")
  )
  rows <- list(); k <- 0L
  for (ss in size_specs) {
    for (M in M_values) {
      for (dd in dist_specs) {
        k <- k + 1L
        q1 <- ss$N1 %/% M; q2 <- ss$N2 %/% M
        z <- new_scenario(
          id = sprintf("smallQ_%s_Q%d_%d_M%d_phi0.80_%s",
                       ss$label, q1, q2, M, dd$label),
          group = "smallQ_feasibility",
          N1 = ss$N1, N2 = ss$N2,
          M1 = M, M2 = M,
          model1 = "ar1", p11 = 0.8,
          model2 = "ar1", p21 = 0.8,
          innov1 = dd$i1, innov2 = dd$i2,
          lrv_sd1 = 1, lrv_sd2 = 1,
          effect_c = 0, delta0 = 0,
          L_rule = "loglog",
          theory_scope = "feasibility_stress_small_Q"
        )
        z$size_pattern <- ss$label
        z$dist_pattern <- dd$label
        z$Q1_design <- q1
        z$Q2_design <- q2
        rows[[k]] <- z
      }
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out$base_id <- out$scenario_id
  # reproduce the original small-Q full run.
  out$scenario_no <- seq_len(nrow(out))
  out
}

small_all <- build_smallQ_design_local()
small_worst <- small_all[small_all$M1 == 54L, , drop = FALSE]

small_dir <- file.path(OUT_ROOT, "smallQ_Q1eq4_checks")
run_design(
  small_worst,
  out_dir = small_dir,
  B = 10000L,
  threads = N_THREADS,
  base_seed = 2026082909,
  overwrite = TRUE,
  allow_small_B = FALSE
)

cov_small <- summarize_coverage(small_dir, levels = 0.95)
diag_small <- summarize_diagnostics(small_dir)

small_rows <- list()
for (sp in c("balanced", "imbalance_1to2")) {
  ids <- diag_small$scenario_id[diag_small$size_pattern == sp]
  dd <- diag_small[diag_small$scenario_id %in% ids, , drop = FALSE]
  cc <- cov_small[cov_small$scenario_id %in% ids &
                    abs(as.numeric(cov_small$nominal) - 0.95) < 1e-12,
                  , drop = FALSE]
  vcabel <- cc[cc$method == "VCABEL_feasible", "coverage_fail_as_miss"]
  vcbc <- cc[cc$method == "VCBC_feasible", "coverage_fail_as_miss"]
  small_rows[[sp]] <- data.frame(
    size_pattern = sp,
    Q1 = unique(dd$Q1), Q2 = unique(dd$Q2), M = unique(dd$M1),
    mean_BEL_fail = mean(dd$bel_fail),
    mean_ABEL_fail = mean(dd$abel_feasible_fail),
    mean_ABEL_global_fallback = mean(dd$abel_feasible_solver_fallback),
    mean_VCABEL_95 = mean(as.numeric(vcabel)),
    mean_VCBC_95 = mean(as.numeric(vcbc)),
    stringsAsFactors = FALSE
  )
}
small_agg <- do.call(rbind, small_rows)
rownames(small_agg) <- NULL
write.csv(small_agg,
          file.path(OUT_ROOT, "SMALLQ_Q1EQ4_AVERAGES.csv"),
          row.names = FALSE)

# STEP 4. Automatic comparison with reported values
checks <- list(); kk <- 0L

get_prim <- function(method) {
  x <- prim_avg$coverage_fail_as_miss[prim_avg$method == method]
  if (!length(x)) NA_real_ else x[1L]
}
kk <- kk + 1L; checks[[kk]] <- check_row("primary_N216_phi08", "BEL mean 95%", get_prim("BEL"), 0.800, 3L)
kk <- kk + 1L; checks[[kk]] <- check_row("primary_N216_phi08", "ABEL feasible mean 95%", get_prim("ABEL_feasible"), 0.808, 3L)
kk <- kk + 1L; checks[[kk]] <- check_row("primary_N216_phi08", "VC+ABEL feasible mean 95%", get_prim("VCABEL_feasible"), 0.940, 3L)

bw_expected <- c(M23=0.906, log1=0.923, loglog=0.945, log2=0.947, old4log=0.954)
for (rr in names(bw_expected)) {
  obs <- bw95$coverage_fail_as_miss[bw95$L_rule == rr]
  kk <- kk + 1L
  checks[[kk]] <- check_row("bandwidth", paste0("VC+ABEL 95% ", rr), obs[1L], bw_expected[[rr]], 3L)
}

# Small-Q manuscript averages over the same three innovation pairs.
for (sp in c("balanced", "imbalance_1to2")) {
  z <- small_agg[small_agg$size_pattern == sp, , drop = FALSE]
  if (sp == "balanced") {
    exp_fail <- 0.0330; exp_fb <- 0.476; exp_vca <- 0.892; exp_vcb <- 0.853
  } else {
    exp_fail <- 0.0048; exp_fb <- 0.370; exp_vca <- 0.895; exp_vcb <- 0.882
  }
  kk <- kk + 1L; checks[[kk]] <- check_row("smallQ_Q1eq4", paste0(sp," BEL fail"), z$mean_BEL_fail, exp_fail, 4L)
  kk <- kk + 1L; checks[[kk]] <- check_row("smallQ_Q1eq4", paste0(sp," ABEL fail"), z$mean_ABEL_fail, 0.000, 3L)
  kk <- kk + 1L; checks[[kk]] <- check_row("smallQ_Q1eq4", paste0(sp," fallback"), z$mean_ABEL_global_fallback, exp_fb, 3L)
  kk <- kk + 1L; checks[[kk]] <- check_row("smallQ_Q1eq4", paste0(sp," VC+ABEL 95%"), z$mean_VCABEL_95, exp_vca, 3L)
  kk <- kk + 1L; checks[[kk]] <- check_row("smallQ_Q1eq4", paste0(sp," VC+BC 95%"), z$mean_VCBC_95, exp_vcb, 3L)
}

checks <- do.call(rbind, checks)
write.csv(checks,
          file.path(OUT_ROOT, "FINAL_MANUSCRIPT_VALUE_CHECKS.csv"),
          row.names = FALSE)


