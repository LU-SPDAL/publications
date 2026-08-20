# Boundary-limit functions used in the (x, Q) simulations

K_positive <- function(c, x) {
  one_minus_exp <- -expm1(-c * x)
  2 / x - 2 * one_minus_exp / (c * x^2)
}

L_negative <- function(c, parity_sign, x) {
  0.5 * (x + (1 - parity_sign * exp(-c * x)) / c)
}

positive_boundary_parameters <- function(x, Q) {
  K1 <- K_positive(1, x)
  KQ <- K_positive(Q, x)

  nu <- K1 / KQ
  u1 <- (1 - exp(-x))^2 / (x^2 * KQ)
  block_decay <- exp(-x)
  theta <- (1 - exp(-x))^2 / (x^2 * K1)
  aG_over_N <- (2 - 1 / (2 * nu) + 4 * theta^2) / Q

  list(
    nu = nu,
    u1 = u1,
    block_decay = block_decay,
    theta = theta,
    aG_over_N = aG_over_N,
    bartlett_multiplier = 1 - aG_over_N
  )
}

negative_boundary_parameters <- function(x, Q, M_parity = c("even", "odd")) {
  M_parity <- match.arg(M_parity)
  sM <- if (M_parity == "even") 1 else -1
  sN <- sM^Q

  L1 <- L_negative(1, sM, x)
  LQ <- L_negative(Q, sN, x)

  nu <- L1 / LQ
  u1 <- -(1 - sM * exp(-x))^2 / (4 * LQ)
  block_decay <- sM * exp(-x)
  theta <- -(1 - sM * exp(-x))^2 / (4 * L1)
  aG_over_N <- (2 - 1 / (2 * nu) + 4 * theta^2) / Q

  list(
    nu = nu,
    u1 = u1,
    block_decay = block_decay,
    theta = theta,
    aG_over_N = aG_over_N,
    bartlett_multiplier = 1 - aG_over_N,
    sM = sM,
    sN = sN
  )
}

boundary_covariance <- function(x, Q, boundary = c("positive", "negative"),
                                M_parity = c("even", "odd")) {
  boundary <- match.arg(boundary)
  M_parity <- match.arg(M_parity)

  pars <- if (boundary == "positive") {
    positive_boundary_parameters(x, Q)
  } else {
    negative_boundary_parameters(x, Q, M_parity)
  }

  first_row <- c(
    pars$nu,
    pars$u1 * pars$block_decay^(seq_len(Q - 1L) - 1L)
  )

  Sigma <- toeplitz(first_row)
  Sigma <- (Sigma + t(Sigma)) / 2
  diag(Sigma) <- diag(Sigma) + 1e-13

  list(Sigma = Sigma, parameters = pars)
}

gaussian_bartlett_coefficient <- function(G) {
  Q <- nrow(G)
  one <- rep(1, Q)
  c0 <- G[1, 1]
  total <- drop(crossprod(one, G %*% one))
  G2 <- G %*% G

  -2 * drop(crossprod(one, G2 %*% one)) / (c0 * total) +
    2 * sum(diag(G2)) / (c0^2 * Q) +
    3 * total / (2 * c0 * Q)
}

extended_cutoff <- function(statistic, alpha = 0.05) {
  p_failure <- mean(!is.finite(statistic))
  if (p_failure >= alpha) return(c(cutoff = Inf, p_failure = p_failure))

  cutoff <- as.numeric(
    quantile(statistic, 1 - alpha, type = 7, names = FALSE)
  )

  c(cutoff = cutoff, p_failure = p_failure)
}
