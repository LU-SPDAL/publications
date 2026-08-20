# Helpers for the stable-interior and misspecification simulations

load_simulation_engine <- function(threads = 1L) {
  Rcpp::sourceCpp(file.path("src", "simulation_core.cpp"), rebuild = FALSE)
  RcppParallel::setThreadOptions(numThreads = threads)
}

common_block_length <- function(N) {
  floor(sqrt(N))
}

sn_critical_value <- function(seed, reps = 250000L, kl_terms = 250L,
                              alpha = 0.05) {
  draws <- simulate_sn_limit_cpp(reps, kl_terms, seed)
  as.numeric(quantile(draws, 1 - alpha, type = 1, names = FALSE))
}

coverage_summary <- function(values, critical) {
  accepted <- values <= critical
  coverage <- mean(accepted, na.rm = TRUE)

  c(
    coverage = coverage,
    mcse = sqrt(coverage * (1 - coverage) / sum(!is.na(accepted))),
    failure_rate = mean(!is.finite(values))
  )
}

summarize_simulation <- function(sim, sn_critical, alpha = 0.05) {
  chi_critical <- qchisq(1 - alpha, 1)
  methods <- c("raw_bel", "general_full", "ar1_full", "ar_wald", "self_normalized")
  labels <- c("Raw", "General", "AR(1)", "AR-Wald", "SN")

  rows <- lapply(seq_along(methods), function(i) {
    critical <- if (methods[i] == "self_normalized") sn_critical else chi_critical
    s <- coverage_summary(sim[, methods[i]], critical)

    data.frame(
      method = labels[i],
      coverage = s["coverage"],
      mcse = s["mcse"],
      failure_rate = s["failure_rate"]
    )
  })

  do.call(rbind, rows)
}
