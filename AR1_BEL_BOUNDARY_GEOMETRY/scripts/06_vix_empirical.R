# VIX working-model fit, correction path, and observed-data intervals

source(file.path("R", "bel_core.R"))
source(file.path("R", "vix_tools.R"))

alpha <- 0.05
chi_critical <- qchisq(1 - alpha, 1)
M_path <- c(2, 3, 4, 5, 6, 7, 10, 12, 14, 15, 20, 21, 28, 30, 35, 42, 60, 70, 84)
reps_path <- 30000L
base_seed <- 812026L
out_dir <- file.path("results", "recomputed", "vix")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

vix <- read_monthly_vix()
X <- vix$log_vix
N <- length(X)

write.csv(vix, file.path(out_dir, "01_vix_monthly_log.csv"), row.names = FALSE)

burg <- burg_phi_bias_reduced(X)
phi_hat <- burg["phi_tilde"]
mom <- innovation_moments(X, phi_hat)

xbar <- mean(X)
eps_hat <- (X[-1] - xbar) - phi_hat * (X[-length(X)] - xbar)
eps_centered <- eps_hat - mean(eps_hat)

bic <- ar_bic_table(X)
write.csv(bic, file.path(out_dir, "03_ar_bic_table.csv"), row.names = FALSE)

adf <- urca::ur.df(X, type = "drift", lags = 0, selectlags = "Fixed")
kpss <- tseries::kpss.test(X, null = "Level")
extra <- additional_ar_lags_test(X)
lb <- Box.test(eps_hat, lag = 12, type = "Ljung-Box", fitdf = 1)
lb2 <- Box.test(eps_centered^2, lag = 12, type = "Ljung-Box")
arch <- arch_lm_test(eps_centered)
cusum <- cusum_ar1_test(X)

diagnostics <- data.frame(
  statistic = c(
    "N", "mean_log_vix", "sd_log_vix", "geometric_mean_vix",
    "phi_burg", "phi_br_unprojected", "phi_tilde", "BIC_selected_AR_order",
    "ADF_statistic_lag0_drift", "ADF_1pct_critical", "KPSS_statistic", "KPSS_p_value",
    "additional_AR_lags_2_to_4_F", "additional_AR_lags_2_to_4_p",
    "residual_Ljung_Box_12_stat", "residual_Ljung_Box_12_p",
    "squared_residual_Ljung_Box_12_stat", "squared_residual_Ljung_Box_12_p",
    "ARCH_LM_12_stat", "ARCH_LM_12_p", "CUSUM_stat", "CUSUM_p",
    "residual_skewness", "residual_excess_kurtosis"
  ),
  value = c(
    N, mean(X), sd(X), exp(mean(X)),
    burg["phi_burg"], burg["phi_br"], phi_hat, bic$p[which.min(bic$BIC)],
    adf@teststat[1], adf@cval[1, "1pct"], unname(kpss$statistic), kpss$p.value,
    extra["F"], extra["p_value"], unname(lb$statistic), lb$p.value,
    unname(lb2$statistic), lb2$p.value, arch["statistic"], arch["p_value"],
    cusum["statistic"], cusum["p_value"], mom["skewness"], mom["excess_kurtosis"]
  )
)
write.csv(diagnostics, file.path(out_dir, "02_diagnostics.csv"), row.names = FALSE)

geometry <- do.call(rbind, lapply(M_path, function(M) {
  Q <- N / M
  nu <- nu_ar1(N, M, phi_hat)
  a <- aK_ar1(N, M, phi_hat, mom["skewness"], mom["excess_kurtosis"])

  data.frame(
    M = M,
    Q = Q,
    N_eff = N,
    phi = phi_hat,
    x = M * (1 - phi_hat),
    nu = nu,
    aG = a["aG"],
    eta3_data = a["eta3"],
    eta4_data = a["eta4"],
    aK_data = a["aK"],
    gaussian_bartlett_multiplier = 1 - a["aG"] / N,
    data_bartlett_multiplier = 1 - a["aK"] / N,
    leading_bartlett_multiplier = 1 - 3 / (2 * Q),
    gaussian_combined_multiplier = nu * (1 - a["aG"] / N),
    data_combined_multiplier = nu * (1 - a["aK"] / N)
  )
}))
write.csv(geometry, file.path(out_dir, "04_correction_geometry.csv"), row.names = FALSE)

calibration <- do.call(rbind, lapply(seq_along(M_path), function(i) {
  M <- M_path[i]
  Q <- N / M
  G <- exact_ar1_block_covariance(M, Q, phi_hat)
  lr <- simulate_lr_from_covariance(G, reps_path, base_seed + 1000L * i + 17L * M + Q, chunk = 1000L)
  g <- geometry[geometry$M == M, ]

  scales <- c(
    Raw = 1,
    Variance = g$nu,
    Bartlett = g$gaussian_bartlett_multiplier,
    Combined = g$gaussian_combined_multiplier,
    LeadingCombined = g$nu * g$leading_bartlett_multiplier
  )
  rejection <- sapply(scales, function(s) mean(s * lr > chi_critical))

  data.frame(
    M = M,
    Q = Q,
    N_eff = N,
    x = g$x,
    phi = phi_hat,
    nu = g$nu,
    aG = g$aG,
    convex_hull_failure = mean(!is.finite(lr)),
    size_raw = rejection["Raw"],
    size_vc = rejection["Variance"],
    size_bartlett = rejection["Bartlett"],
    size_combined = rejection["Combined"],
    size_leading_combined = rejection["LeadingCombined"],
    reps = reps_path
  )
}))
write.csv(calibration, file.path(out_dir, "05_calibration_path.csv"), row.names = FALSE)

ci_rows <- list()
k <- 1L
for (M in M_path) {
  g <- geometry[geometry$M == M, ]
  multipliers <- c(
    Raw = 1,
    Variance = g$nu,
    Bartlett = g$data_bartlett_multiplier,
    Combined = g$data_combined_multiplier
  )

  for (method in names(multipliers)) {
    ci <- bel_ci(X, M, multipliers[method], chi_critical)
    ci_rows[[k]] <- data.frame(
      M = M,
      Q = N / M,
      x = g$x,
      procedure = method,
      multiplier = multipliers[method],
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
write.csv(intervals, file.path(out_dir, "08_bel_confidence_intervals.csv"), row.names = FALSE)

sigma2_eps <- mean(eps_centered^2)
Omega_N <- sigma2_eps / (1 - phi_hat^2) * D_ar1(N, phi_hat)
se_ar <- sqrt(Omega_N / N)
z <- qnorm(0.975)
ar_ci <- mean(X) + c(-1, 1) * z * se_ar

mean_fit <- lm(X ~ 1)
se_hac <- sqrt(sandwich::NeweyWest(mean_fit, prewhite = FALSE, adjust = FALSE)[1, 1])
hac_ci <- mean(X) + c(-1, 1) * z * se_hac

sn_file <- file.path("results", "vix", "09b_self_normalized_details.csv")
if (file.exists(sn_file)) {
  sn_critical <- read.csv(sn_file)$SN_critical_95[1]
} else {
  sn_critical <- as.numeric(quantile(simulate_sn_limit(), 0.95, names = FALSE))
}
Wn <- self_normalizer(X)
sn_half <- sqrt(sn_critical * Wn / N)
sn_ci <- mean(X) + c(-1, 1) * sn_half

benchmarks <- data.frame(
  method = c("AR-Wald", "HAC-Wald", "Self-normalized"),
  lower_log = c(ar_ci[1], hac_ci[1], sn_ci[1]),
  upper_log = c(ar_ci[2], hac_ci[2], sn_ci[2]),
  lower_geometric_vix = exp(c(ar_ci[1], hac_ci[1], sn_ci[1])),
  upper_geometric_vix = exp(c(ar_ci[2], hac_ci[2], sn_ci[2]))
)
write.csv(benchmarks, file.path(out_dir, "09_benchmark_intervals.csv"), row.names = FALSE)
