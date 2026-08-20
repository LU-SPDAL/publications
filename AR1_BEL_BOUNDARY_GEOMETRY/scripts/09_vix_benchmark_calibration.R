# Gaussian calibration of the AR-Wald and self-normalized VIX benchmarks

vix_fit <- read.csv(file.path("results", "vix_reference_laws", "00_vix_fit_summary.csv"))
N <- vix_fit$N[1]
phi_true <- vix_fit$phi_tilde[1]
reps <- 200000L
seed <- 2026082009L
rho <- 0.995
chi_critical <- qchisq(0.95, 1)
sn_critical <- read.csv(file.path("results", "vix", "09b_self_normalized_details.csv"))$SN_critical_95[1]
out_dir <- file.path("results", "recomputed", "vix_benchmarks")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

D_vector <- function(r, phi) {
  (1 + phi) / (1 - phi) -
    2 * phi * (1 - phi^r) / (r * (1 - phi)^2)
}

simulate_stationary_ar1 <- function(batch, n, phi) {
  X <- matrix(0, nrow = batch, ncol = n)
  X[, 1] <- rnorm(batch, sd = 1 / sqrt(1 - phi^2))
  for (t in 2:n) X[, t] <- phi * X[, t - 1L] + rnorm(batch)
  X
}

burg_rows <- function(X) {
  Y <- X - rowMeans(X)
  n <- ncol(Y)
  num <- 2 * rowSums(Y[, 2:n] * Y[, 1:(n - 1L)])
  den <- rowSums(Y[, 2:n]^2 + Y[, 1:(n - 1L)]^2)
  phi_burg <- num / den
  phi_br <- (phi_burg + 1 / n) / (1 - 3 / n)
  pmin(rho, pmax(-rho, phi_br))
}

set.seed(seed)
reject_ar <- 0L
reject_sn <- 0L
batch_size <- 1000L

for (start in seq(1L, reps, by = batch_size)) {
  b <- min(batch_size, reps - start + 1L)
  X <- simulate_stationary_ar1(b, N, phi_true)
  xbar <- rowMeans(X)
  Y <- X - xbar
  phi_hat <- burg_rows(X)

  residuals <- Y[, 2:N] - Y[, 1:(N - 1L)] * phi_hat
  residuals <- residuals - rowMeans(residuals)
  sigma2 <- rowMeans(residuals^2)
  Omega <- sigma2 / (1 - phi_hat^2) * D_vector(N, phi_hat)
  ar_stat <- N * xbar^2 / Omega
  reject_ar <- reject_ar + sum(ar_stat > chi_critical)

  centered <- X - xbar
  partial <- centered
  for (t in 2:N) partial[, t] <- partial[, t] + partial[, t - 1L]
  bridge <- partial - partial[, N] * rep(seq_len(N) / N, each = b)
  Wn <- rowSums(bridge^2) / N^2
  sn_stat <- N * xbar^2 / Wn
  reject_sn <- reject_sn + sum(sn_stat > sn_critical)
}

p_ar <- reject_ar / reps
p_sn <- reject_sn / reps

result <- data.frame(
  method = c("AR-Wald", "Self-normalized"),
  rejection = c(p_ar, p_sn),
  mcse = sqrt(c(p_ar, p_sn) * (1 - c(p_ar, p_sn)) / reps),
  reps = reps,
  seed = seed
)
write.csv(result, file.path(out_dir, "vix_benchmark_calibration.csv"), row.names = FALSE)
