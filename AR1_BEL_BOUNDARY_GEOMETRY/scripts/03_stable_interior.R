# Stable-interior coverage experiment reported in the main paper

source(file.path("R", "simulation_tools.R"))

threads <- as.integer(Sys.getenv("AR1_BEL_CORES", "1"))
load_simulation_engine(threads)

innovations <- read.csv(file.path("reference", "innovations.csv"))
N_values <- c(256L, 512L, 1024L)
phi_values <- c(0.5, 0.8)
reps <- 10000L
base_seed <- 2026080901
sn_critical <- sn_critical_value(base_seed + 9000000000)
out_dir <- file.path("results", "recomputed", "stable_interior")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

seed_for <- function(innovation_code, N, phi) {
  base_seed + innovation_code * 1e10 + N * 1e6 +
    round((phi + 1) * 1000) * 100 + 50
}

rows <- list()
k <- 1L

for (i in seq_len(nrow(innovations))) {
  for (phi in phi_values) {
    for (N in N_values) {
      M <- common_block_length(N)

      sim <- run_refocused_design_cpp(
        replications = reps,
        n = N,
        ar1 = phi,
        ar2 = 0,
        ma1 = 0,
        innovation_code = innovations$innovation_code[i],
        process_mean = 0,
        mu0 = 0,
        burn_in = 1000L,
        bootstrap_reps = 0L,
        M = M,
        seed = seed_for(innovations$innovation_code[i], N, phi),
        rep_offset = 0
      )

      sim <- as.data.frame(sim)
      s <- summarize_simulation(sim, sn_critical)
      s$innovation <- innovations$innovation[i]
      s$role <- innovations$role[i]
      s$phi <- phi
      s$N <- N
      s$M <- M
      s$Nstar <- floor(N / M) * M
      rows[[k]] <- s
      k <- k + 1L
    }
  }
}

coverage <- do.call(rbind, rows)
coverage <- coverage[, c("innovation", "role", "phi", "N", "M", "Nstar",
                         "method", "coverage", "mcse", "failure_rate")]

write.csv(coverage, file.path(out_dir, "stable_interior_coverage.csv"), row.names = FALSE)
