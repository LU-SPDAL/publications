# Core functions for the AR(1) BEL calculations

D_ar1 <- function(r, phi) {
  r <- as.integer(r)
  if (r == 1L) return(1)

  h <- seq_len(r - 1L)
  1 + 2 * sum((1 - h / r) * phi^h)
}

nu_ar1 <- function(N, M, phi) {
  D_ar1(M, phi) / D_ar1(N, phi)
}

burg_phi <- function(x) {
  x <- as.numeric(x)
  y <- x - mean(x)
  n <- length(y)

  numerator <- 2 * sum(y[2:n] * y[1:(n - 1L)])
  denominator <- sum(y[2:n]^2 + y[1:(n - 1L)]^2)

  numerator / denominator
}

burg_phi_bias_reduced <- function(x, rho = 0.995) {
  n <- length(x)
  phi_burg <- burg_phi(x)
  phi_br <- (phi_burg + 1 / n) / (1 - 3 / n)
  phi_tilde <- min(rho, max(-rho, phi_br))

  c(
    phi_burg = phi_burg,
    phi_br = phi_br,
    phi_tilde = phi_tilde
  )
}

innovation_moments <- function(x, phi) {
  x <- as.numeric(x)
  xbar <- mean(x)

  e <- (x[2:length(x)] - xbar) -
    phi * (x[1:(length(x) - 1L)] - xbar)

  e <- e - mean(e)
  m2 <- mean(e^2)

  c(
    variance = m2,
    skewness = mean(e^3) / m2^(3 / 2),
    excess_kurtosis = mean(e^4) / m2^2 - 3
  )
}

theta_ar1 <- function(M, phi) {
  phi * (1 - phi^M)^2 /
    (M * (1 - phi)^2 * D_ar1(M, phi))
}

aG_ar1 <- function(N, M, phi) {
  nu <- nu_ar1(N, M, phi)
  theta <- theta_ar1(M, phi)

  M * (2 - 1 / (2 * nu) + 4 * theta^2)
}

block_weight_sums_ar1 <- function(M, phi) {
  H2 <- M * D_ar1(M, phi) / (1 - phi^2)

  if (abs(phi) < 1e-12) {
    return(c(H2 = M, H3 = M, H4 = M))
  }

  v <- 1 - phi^M
  d <- 1 - phi

  Gk <- function(k) {
    if (k == 0L) return(M)
    phi^k * (1 - phi^(k * M)) / (1 - phi^k)
  }

  G0 <- Gk(0L)
  G1 <- Gk(1L)
  G2 <- Gk(2L)
  G3 <- Gk(3L)
  G4 <- Gk(4L)

  H3 <- (
    v^3 * phi^3 / (1 - phi^3) +
      G0 - 3 * G1 + 3 * G2 - G3
  ) / d^3

  H4 <- (
    v^4 * phi^4 / (1 - phi^4) +
      G0 - 4 * G1 + 6 * G2 - 4 * G3 + G4
  ) / d^4

  c(H2 = H2, H3 = H3, H4 = H4)
}

aK_ar1 <- function(N, M, phi, gamma3, gamma4) {
  H <- block_weight_sums_ar1(M, phi)

  eta3 <- gamma3 * H["H3"] / H["H2"]^(3 / 2)
  eta4 <- gamma4 * H["H4"] / H["H2"]^2

  aG <- aG_ar1(N, M, phi)
  aK <- aG + M * (0.5 * eta4 - eta3^2 / 3)

  c(
    aG = unname(aG),
    eta3 = unname(eta3),
    eta4 = unname(eta4),
    aK = unname(aK)
  )
}

exact_ar1_block_covariance <- function(M, Q, phi) {
  N <- M * Q
  DN <- D_ar1(N, phi)
  nu <- D_ar1(M, phi) / DN

  c1 <- phi * (1 - phi^M)^2 /
    (M * (1 - phi)^2 * DN)

  h <- seq_len(Q - 1L)
  first_row <- c(
    nu,
    c1 * phi^((h - 1L) * M)
  )

  G <- toeplitz(first_row)
  G <- (G + t(G)) / 2
  diag(G) <- diag(G) + 1e-13
  G
}

bel_lr <- function(s, tol = 1e-11, maxit = 80L) {
  s <- as.numeric(s)

  if (!(min(s) < 0 && max(s) > 0)) return(Inf)

  scale_sum <- max(1, sum(abs(s)))
  if (abs(sum(s)) <= tol * scale_sum) return(0)

  lo <- -1 / max(s)
  hi <- -1 / min(s)

  lo <- lo + 1e-12 * (1 + abs(lo))
  hi <- hi - 1e-12 * (1 + abs(hi))
  lambda <- 0

  for (i in seq_len(maxit)) {
    den <- 1 + lambda * s
    f <- sum(s / den)
    fp <- -sum((s / den)^2)

    if (abs(f) <= tol * scale_sum) break

    if (f > 0) {
      lo <- lambda
    } else {
      hi <- lambda
    }

    candidate <- lambda - f / fp

    if (!is.finite(candidate) || candidate <= lo || candidate >= hi) {
      candidate <- (lo + hi) / 2
    }

    lambda <- candidate
  }

  den <- 1 + lambda * s
  if (any(den <= 0)) return(Inf)

  max(0, 2 * sum(log1p(lambda * s)))
}

bel_lr_matrix <- function(S, n_bisect = 45L) {
  S <- as.matrix(S)
  nr <- nrow(S)

  row_min <- apply(S, 1L, min)
  row_max <- apply(S, 1L, max)
  valid <- row_min < 0 & row_max > 0

  lr <- rep(Inf, nr)
  if (!any(valid)) return(lr)

  Z <- S[valid, , drop = FALSE]
  lo <- -1 / row_max[valid]
  hi <- -1 / row_min[valid]

  lo <- lo + 1e-12 * (1 + abs(lo))
  hi <- hi - 1e-12 * (1 + abs(hi))

  for (i in seq_len(n_bisect)) {
    mid <- (lo + hi) / 2
    den <- 1 + Z * mid
    f <- rowSums(Z / den)

    move_right <- f > 0
    lo[move_right] <- mid[move_right]
    hi[!move_right] <- mid[!move_right]
  }

  lambda <- (lo + hi) / 2
  den <- 1 + Z * lambda

  values <- 2 * rowSums(log1p(Z * lambda))
  values[!is.finite(values)] <- Inf

  lr[valid] <- pmax(0, values)
  lr
}

bel_lr_data <- function(x, mu0, M) {
  Q <- floor(length(x) / M)
  N_eff <- Q * M
  x <- x[seq_len(N_eff)]

  blocks <- matrix(
    x - mu0,
    nrow = Q,
    ncol = M,
    byrow = TRUE
  )

  bel_lr(rowSums(blocks))
}

simulate_lr_from_covariance <- function(Sigma, reps, seed, chunk = 5000L) {
  set.seed(seed)

  Q <- nrow(Sigma)
  R <- chol((Sigma + t(Sigma)) / 2)

  out <- numeric(reps)
  start <- 1L

  while (start <= reps) {
    n_chunk <- min(chunk, reps - start + 1L)
    Z <- matrix(rnorm(n_chunk * Q), nrow = n_chunk) %*% R

    out[start:(start + n_chunk - 1L)] <- bel_lr_matrix(Z)
    start <- start + n_chunk
  }

  out
}

bel_ci <- function(x, M, multiplier, cutoff) {
  Q <- floor(length(x) / M)
  N_eff <- Q * M
  x <- x[seq_len(N_eff)]

  blocks <- matrix(x, nrow = Q, ncol = M, byrow = TRUE)
  block_means <- rowMeans(blocks)

  center <- mean(x)
  eps <- 1e-10 * max(1, diff(range(block_means)))

  target <- function(mu) {
    stat <- multiplier * bel_lr_data(x, mu, M)
    if (is.finite(stat)) stat - cutoff else 1e12
  }

  lower <- uniroot(
    target,
    c(min(block_means) + eps, center),
    tol = 1e-10
  )$root

  upper <- uniroot(
    target,
    c(center, max(block_means) - eps),
    tol = 1e-10
  )$root

  c(lower = lower, upper = upper)
}
