# Finite-Q reference laws along the long-block part of the VIX path

source(file.path("R", "bel_core.R"))
source(file.path("R", "boundary_limits.R"))
source(file.path("R", "vix_tools.R"))

alpha <- 0.05
chi_critical <- qchisq(1 - alpha, 1)
M_values <- c(12, 15, 20, 28, 35, 42, 60, 70, 84)
critical_reps <- 300000L
size_reps <- 100000L
seed_interior <- 17082710L
seed_boundary <- 17082720L
seed_exact <- 17082730L
out_dir <- file.path("results", "recomputed", "vix_reference_laws")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

vix <- read_monthly_vix()
X <- vix$log_vix
N <- length(X)
phi_hat <- burg_phi_bias_reduced(X)["phi_tilde"]
mom <- innovation_moments(X, phi_hat)

fit_summary <- data.frame(
  N = N,
  mean_log_vix = mean(X),
  geometric_mean_vix = exp(mean(X)),
  phi_tilde = phi_hat
)
write.csv(fit_summary, file.path(out_dir, "00_vix_fit_summary.csv"), row.names = FALSE)

interior_rows <- lapply(seq_along(M_values), function(i) {
  Q <- N / M_values[i]
  lr <- simulate_lr_from_covariance(diag(Q), critical_reps,
                                    seed_interior + 1000L * i + Q)
  sm <- extended_cutoff(lr, alpha)

  data.frame(
    Q = Q,
    critical_interior_finite_Q = sm["cutoff"],
    hull_failure_simulated = sm["p_failure"],
    hull_failure_theoretical = 2^(1 - Q),
    chi2_rejection_of_interior_limit = mean(lr > chi_critical),
    reps = critical_reps
  )
})
interior <- unique(do.call(rbind, interior_rows))
write.csv(interior, file.path(out_dir, "01_interior_finite_Q_critical_values.csv"), row.names = FALSE)

boundary_rows <- lapply(seq_along(M_values), function(i) {
  M <- M_values[i]
  Q <- N / M
  x <- M * (1 - phi_hat)
  obj <- boundary_covariance(x, Q, "positive", "even")
  lr <- simulate_lr_from_covariance(obj$Sigma, critical_reps,
                                    seed_boundary + 1000L * i + M + 10L * Q)
  stat <- obj$parameters$nu * lr
  sm <- extended_cutoff(stat, alpha)

  data.frame(
    M = M,
    Q = Q,
    x = x,
    Vplus = obj$parameters$nu,
    adjacent_correlation = obj$parameters$u1 / obj$parameters$nu,
    critical_xQ_boundary = sm["cutoff"],
    hull_failure_boundary = sm["p_failure"],
    chi2_rejection_of_boundary_limit = mean(stat > chi_critical),
    reps = critical_reps
  )
})
boundary <- do.call(rbind, boundary_rows)
write.csv(boundary, file.path(out_dir, "02_xQ_boundary_critical_values.csv"), row.names = FALSE)

references <- merge(boundary, interior, by = "Q", all.x = TRUE, sort = FALSE)
references <- references[match(M_values, references$M), ]
references$critical_chi2 <- chi_critical
write.csv(references, file.path(out_dir, "03_three_reference_critical_values.csv"), row.names = FALSE)

size_rows <- lapply(seq_along(M_values), function(i) {
  M <- M_values[i]
  Q <- N / M
  x <- M * (1 - phi_hat)
  nu <- nu_ar1(N, M, phi_hat)
  G <- exact_ar1_block_covariance(M, Q, phi_hat)
  lr <- simulate_lr_from_covariance(G, size_reps,
                                    seed_exact + 1000L * i + M + 10L * Q)
  stat <- nu * lr
  ref <- references[references$M == M, ]

  size_int <- if (is.finite(ref$critical_interior_finite_Q))
    mean(stat > ref$critical_interior_finite_Q) else NA_real_
  size_bnd <- if (is.finite(ref$critical_xQ_boundary))
    mean(stat > ref$critical_xQ_boundary) else NA_real_

  data.frame(
    M = M,
    Q = Q,
    x = x,
    phi = phi_hat,
    nu = nu,
    exact_hull_failure = mean(!is.finite(lr)),
    size_VC_chi2 = mean(stat > chi_critical),
    size_VC_interior_finite_Q = size_int,
    size_VC_xQ_boundary = size_bnd,
    reps = size_reps,
    critical_chi2 = chi_critical,
    critical_interior_finite_Q = ref$critical_interior_finite_Q,
    critical_xQ_boundary = ref$critical_xQ_boundary
  )
})
sizes <- do.call(rbind, size_rows)
write.csv(sizes, file.path(out_dir, "04_exact_finite_sample_sizes_three_references.csv"), row.names = FALSE)

ci_rows <- list()
k <- 1L
for (M in M_values) {
  Q <- N / M
  x <- M * (1 - phi_hat)
  nu <- nu_ar1(N, M, phi_hat)
  a <- aK_ar1(N, M, phi_hat, mom["skewness"], mom["excess_kurtosis"])
  combined <- nu * (1 - a["aK"] / N)
  ref <- references[references$M == M, ]

  methods <- data.frame(
    calibration = c(
      "VC + chi-squared",
      "VC + interior finite-Q",
      "VC + xQ boundary fitted-x",
      "Combined + chi-squared"
    ),
    multiplier = c(nu, nu, nu, combined),
    cutoff = c(
      chi_critical,
      ref$critical_interior_finite_Q,
      ref$critical_xQ_boundary,
      chi_critical
    )
  )

  for (j in seq_len(nrow(methods))) {
    if (is.finite(methods$cutoff[j])) {
      ci <- bel_ci(X, M, methods$multiplier[j], methods$cutoff[j])
    } else {
      ci <- c(lower = NA_real_, upper = NA_real_)
    }

    ci_rows[[k]] <- data.frame(
      M = M,
      Q = Q,
      x = x,
      calibration = methods$calibration[j],
      multiplier = methods$multiplier[j],
      cutoff = methods$cutoff[j],
      lower_log = ci["lower"],
      upper_log = ci["upper"],
      lower_geometric_vix = exp(ci["lower"]),
      upper_geometric_vix = exp(ci["upper"]),
      sample_geometric_vix = exp(mean(X))
    )
    k <- k + 1L
  }
}

intervals <- do.call(rbind, ci_rows)
write.csv(intervals, file.path(out_dir, "05_vix_confidence_intervals_three_references.csv"), row.names = FALSE)
