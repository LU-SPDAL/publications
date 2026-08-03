library(Rcpp)
library(data.table)
library(ggplot2)

Rcpp::sourceCpp("src/bel_simulation.cpp")

dgp_catalog <- list(
  AR1_05_G = list(
    label = "Gaussian AR(1), phi = 0.5",
    ar = c(0.5, 0), ma = c(0, 0), innovation = "gaussian"
  ),
  AR1_08_G = list(
    label = "Gaussian AR(1), phi = 0.8",
    ar = c(0.8, 0), ma = c(0, 0), innovation = "gaussian"
  ),
  AR1_08_E = list(
    label = "Centered-exponential AR(1), phi = 0.8",
    ar = c(0.8, 0), ma = c(0, 0), innovation = "centered_exponential"
  ),
  ARMA11_06_05_G = list(
    label = "Gaussian ARMA(1,1), phi = 0.6, theta = 0.5",
    ar = c(0.6, 0), ma = c(0.5, 0), innovation = "gaussian"
  ),
  ARMA11_08_m05_G = list(
    label = "Gaussian ARMA(1,1), phi = 0.8, theta = -0.5",
    ar = c(0.8, 0), ma = c(-0.5, 0), innovation = "gaussian"
  ),
  AR2_13_m04_G = list(
    label = "Gaussian AR(2), phi1 = 1.3, phi2 = -0.4",
    ar = c(1.3, -0.4), ma = c(0, 0), innovation = "gaussian"
  ),
  AR2_13_m04_E = list(
    label = "Centered-exponential AR(2), phi1 = 1.3, phi2 = -0.4",
    ar = c(1.3, -0.4), ma = c(0, 0), innovation = "centered_exponential"
  ),
  MA2_m04_025_G = list(
    label = "Gaussian MA(2), theta1 = -0.4, theta2 = 0.25",
    ar = c(0, 0), ma = c(-0.4, 0.25), innovation = "gaussian"
  )
)

innovation_table <- data.table(
  innovation = c(
    "gaussian", "laplace", "centered_exponential", "gamma4",
    "bernoulli", "t5", "contaminated_normal"
  ),
  code = 1:7,
  skewness = c(0, 0, 2, 1, 0, 0, 0),
  excess_kurtosis = c(0, 3, 6, 1.5, -2, 6, 5.333333333333333),
  theorem = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE)
)

truth_cache <- new.env(parent = emptyenv())

innovation_code <- function(name) {
  innovation_table[innovation == name, code]
}

innovation_kappa <- function(name) {
  row <- innovation_table[innovation == name]
  0.5 * row$excess_kurtosis - row$skewness^2 / 3
}

impulse_response <- function(ar, ma, length_out = 131072L) {
  psi <- numeric(length_out)
  psi[1] <- 1

  for (j in 2:length_out) {
    lag <- j - 1L
    value <- if (lag <= length(ma)) ma[lag] else 0

    for (k in seq_along(ar)) {
      if (lag >= k) value <- value + ar[k] * psi[lag - k + 1L]
    }
    psi[j] <- value
  }
  psi
}

population_truth <- function(dgp, n_max = 4096L) {
  key <- paste(c(dgp$ar, dgp$ma, dgp$innovation, n_max), collapse = "_")
  if (exists(key, envir = truth_cache, inherits = FALSE)) {
    return(get(key, envir = truth_cache, inherits = FALSE))
  }

  psi <- impulse_response(dgp$ar, dgp$ma)
  n_fft <- 2^ceiling(log2(2 * length(psi)))
  spectrum <- fft(c(psi, numeric(n_fft - length(psi))))
  autocov <- Re(fft(Conj(spectrum) * spectrum, inverse = TRUE)) / n_fft
  autocov <- autocov[seq_len(length(psi))]

  innovation_scale <- 1 / sqrt(autocov[1])
  autocov <- autocov / autocov[1]
  sigma2 <- sum(psi)^2 / sum(psi^2)

  tail_limit <- min(50000L, length(autocov) - 1L)
  h <- seq_len(tail_limit)
  b2 <- 2 * sum(h * autocov[h + 1L])
  b <- b2 / sigma2

  result <- list(
    gamma = autocov[seq_len(n_max + 1L)],
    sigma2 = sigma2,
    B2 = b2,
    b = b,
    kappa_c = innovation_kappa(dgp$innovation),
    innovation_scale = innovation_scale
  )
  assign(key, result, envir = truth_cache)
  result
}

omega_from_gamma <- function(gamma, r) {
  if (r == 1L) return(gamma[1])
  h <- seq_len(r - 1L)
  gamma[1] + 2 * sum((1 - h / r) * gamma[h + 1L])
}

c_star <- function(b, kappa_c, alpha, c_min = 0.05, c_max = 2.50) {
  z <- qchisq(1 - alpha, df = 1)
  value <- 8 * (b * (z / 2 - 1) - kappa_c) / (45 * z - 1)
  sqrt(pmin(c_max^2, pmax(c_min^2, value)))
}

run_fixed_setting <- function(dgp_name, n, m_values, alpha, reps, seed) {
  dgp <- dgp_catalog[[dgp_name]]
  truth <- population_truth(dgp, n)

  ans <- as.data.table(simulate_fixed_blocks_cpp(
    n = n,
    m_values = as.integer(m_values),
    alpha = alpha,
    reps = reps,
    seed = seed,
    ar1 = dgp$ar[1],
    ar2 = dgp$ar[2],
    ma1 = dgp$ma[1],
    ma2 = dgp$ma[2],
    innovation = innovation_code(dgp$innovation),
    innovation_scale = truth$innovation_scale,
    true_gamma = truth$gamma,
    c0 = ratio_tuning$c0,
    lower = ratio_tuning$lower,
    upper = ratio_tuning$upper,
    eta0 = ratio_tuning$eta0,
    burnin = 1000L,
    n_threads = n_threads
  ))

  ans[, `:=`(
    dgp = dgp_name,
    dgp_label = dgp$label,
    innovation = dgp$innovation,
    N = n,
    true_b = truth$b,
    true_kappa_c = truth$kappa_c,
    seed = seed
  )]
  ans[]
}

run_oracle_grid <- function(dgp_name, n, m_values, alpha, reference_m, reps, seed) {
  dgp <- dgp_catalog[[dgp_name]]
  truth <- population_truth(dgp, n)

  ans <- as.data.table(simulate_oracle_grid_cpp(
    n = n,
    m_values = as.integer(m_values),
    alpha = alpha,
    reference_m = as.integer(reference_m),
    reps = reps,
    seed = seed,
    ar1 = dgp$ar[1],
    ar2 = dgp$ar[2],
    ma1 = dgp$ma[1],
    ma2 = dgp$ma[2],
    innovation = innovation_code(dgp$innovation),
    innovation_scale = truth$innovation_scale,
    true_gamma = truth$gamma,
    burnin = 1000L,
    n_threads = n_threads
  ))

  ans[, `:=`(
    dgp = dgp_name,
    dgp_label = dgp$label,
    innovation = dgp$innovation,
    N = n,
    true_b = truth$b,
    true_kappa_c = truth$kappa_c,
    seed = seed
  )]
  ans[]
}

run_selected_setting <- function(dgp_name, n, alpha, reps, seed) {
  dgp <- dgp_catalog[[dgp_name]]
  truth <- population_truth(dgp, n)
  c_value <- c_star(
    truth$b, truth$kappa_c, alpha,
    selector_tuning$c_min, selector_tuning$c_max
  )

  ans <- as.data.table(simulate_selected_blocks_cpp(
    n = n,
    alpha = alpha,
    c_star = c_value,
    q0_values = selector_tuning$q0_values,
    reps = reps,
    seed = seed,
    ar1 = dgp$ar[1],
    ar2 = dgp$ar[2],
    ma1 = dgp$ma[1],
    ma2 = dgp$ma[2],
    innovation = innovation_code(dgp$innovation),
    innovation_scale = truth$innovation_scale,
    true_gamma = truth$gamma,
    c_min = selector_tuning$c_min,
    c_max = selector_tuning$c_max,
    c0 = ratio_tuning$c0,
    lower = ratio_tuning$lower,
    upper = ratio_tuning$upper,
    eta0 = ratio_tuning$eta0,
    burnin = 1000L,
    n_threads = n_threads
  ))

  ans[, `:=`(
    dgp = dgp_name,
    dgp_label = dgp$label,
    innovation = dgp$innovation,
    N = n,
    true_b = truth$b,
    true_kappa_c = truth$kappa_c,
    seed = seed
  )]
  ans[]
}
