# Dynamic misspecification experiment with AR(2), MA(1), and ARMA(1,1) data

source(file.path("R", "simulation_tools.R"))

threads <- as.integer(Sys.getenv("AR1_BEL_CORES", "1"))
load_simulation_engine(threads)

dgps <- read.csv(file.path("reference", "dgps.csv"))
N_values <- c(256L, 512L, 1024L)
reps <- 10000L
base_seed <- 2026080903
sn_critical <- sn_critical_value(base_seed + 9000000000)
out_dir <- file.path("results", "recomputed", "misspecification")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

seed_for <- function(dgp_code, N) {
  base_seed + dgp_code * 1e10 + N * 1e6 + 50
}

rows <- list()
theory_rows <- list()
k <- 1L

for (i in seq_len(nrow(dgps))) {
  th <- cpp_process_theory(dgps$ar1[i], dgps$ar2[i], dgps$ma1[i], 32L, 1024L)
  theory_rows[[i]] <- data.frame(
    dgp_id = dgps$dgp_id[i],
    dgp_label = dgps$dgp_label[i],
    pseudo_phi = th$pseudo_phi,
    true_lrv = th$true_lrv,
    ar1_implied_lrv = th$ar1_implied_lrv,
    ar1_lrv_ratio = th$ar1_lrv_ratio,
    b_true = th$b,
    b_ar_pseudo = th$b_ar_pseudo,
    delta_b = th$delta_b,
    predicted_ar_wald_coverage = 2 * pnorm(qnorm(0.975) * sqrt(th$ar1_lrv_ratio)) - 1
  )

  for (N in N_values) {
    M <- common_block_length(N)

    sim <- run_refocused_design_cpp(
      replications = reps,
      n = N,
      ar1 = dgps$ar1[i],
      ar2 = dgps$ar2[i],
      ma1 = dgps$ma1[i],
      innovation_code = 1L,
      process_mean = 0,
      mu0 = 0,
      burn_in = 1500L,
      bootstrap_reps = 0L,
      M = M,
      seed = seed_for(i, N),
      rep_offset = 0
    )

    s <- summarize_simulation(as.data.frame(sim), sn_critical)
    s$dgp_id <- dgps$dgp_id[i]
    s$dgp_label <- dgps$dgp_label[i]
    s$family <- dgps$family[i]
    s$N <- N
    s$M <- M
    s$Nstar <- floor(N / M) * M
    rows[[k]] <- s
    k <- k + 1L
  }
}

coverage <- do.call(rbind, rows)
coverage <- coverage[, c("dgp_id", "dgp_label", "family", "N", "M", "Nstar",
                         "method", "coverage", "mcse", "failure_rate")]
theory <- do.call(rbind, theory_rows)

write.csv(coverage, file.path(out_dir, "misspecification_coverage.csv"), row.names = FALSE)
write.csv(theory, file.path(out_dir, "misspecification_theory.csv"), row.names = FALSE)
