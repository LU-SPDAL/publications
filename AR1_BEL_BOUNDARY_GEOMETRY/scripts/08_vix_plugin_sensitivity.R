# Same-sample sensitivity check for the fitted local coordinate x = M(1 - phi)

source(file.path("R", "bel_core.R"))
source(file.path("R", "boundary_limits.R"))

vix_fit <- read.csv(file.path("results", "vix_reference_laws", "00_vix_fit_summary.csv"))
N <- vix_fit$N[1]
phi_true <- vix_fit$phi_tilde[1]
M_values <- c(20, 28, 35, 42, 60, 70, 84)
reps <- 50000L
rho <- 0.995
alpha <- 0.05
chi_critical <- qchisq(1 - alpha, 1)
seed <- 2026082008L
out_dir <- file.path("results", "recomputed", "vix_plugin_sensitivity")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

reference <- read.csv(
  file.path("results", "vix_reference_laws", "03_three_reference_critical_values.csv")
)
reference <- reference[match(M_values, reference$M), ]

D_vector <- function(r, phi) {
  (1 + phi) / (1 - phi) -
    2 * phi * (1 - phi^r) / (r * (1 - phi)^2)
}

burg_rows <- function(X) {
  center <- rowMeans(X)
  Y <- X - center
  n <- ncol(Y)
  num <- 2 * rowSums(Y[, 2:n] * Y[, 1:(n - 1L)])
  den <- rowSums(Y[, 2:n]^2 + Y[, 1:(n - 1L)]^2)
  phi_burg <- num / den
  phi_br <- (phi_burg + 1 / n) / (1 - 3 / n)
  pmin(rho, pmax(-rho, phi_br))
}

simulate_stationary_ar1 <- function(batch, n, phi) {
  X <- matrix(0, nrow = batch, ncol = n)
  X[, 1] <- rnorm(batch, sd = 1 / sqrt(1 - phi^2))
  for (t in 2:n) X[, t] <- phi * X[, t - 1L] + rnorm(batch)
  X
}

block_sums <- function(X, M) {
  Q <- ncol(X) / M
  B <- matrix(0, nrow = nrow(X), ncol = Q)
  for (q in seq_len(Q)) {
    j <- ((q - 1L) * M + 1L):(q * M)
    B[, q] <- rowSums(X[, j, drop = FALSE])
  }
  B
}

# The original lookup-grid seed was not retained in the development folders.
# This reconstructed grid is deterministic and is anchored to the published
# 300,000-draw critical value at the fitted x for each M.
x_grid <- seq(0.25, 15, by = 0.10)
lookup_reps <- 10000L
lookup_seed <- 20260820081L
lookup <- list()

for (i in seq_along(M_values)) {
  M <- M_values[i]
  Q <- N / M
  anchor <- reference$critical_xQ_boundary[i]

  if (!is.finite(anchor)) {
    lookup[[i]] <- data.frame(M = M, Q = Q, x = x_grid, cutoff = Inf)
    next
  }

  cutoffs <- vapply(seq_along(x_grid), function(j) {
    obj <- boundary_covariance(x_grid[j], Q, "positive", "even")
    lr <- simulate_lr_from_covariance(
      obj$Sigma, lookup_reps, lookup_seed + 100000L * i + j, chunk = 2000L
    )
    extended_cutoff(obj$parameters$nu * lr, alpha)["cutoff"]
  }, numeric(1))

  x0 <- M * (1 - phi_true)
  grid_at_x0 <- approx(x_grid, cutoffs, xout = x0, rule = 2)$y
  cutoffs <- cutoffs * anchor / grid_at_x0
  lookup[[i]] <- data.frame(M = M, Q = Q, x = x_grid, cutoff = cutoffs)
}

lookup <- do.call(rbind, lookup)
write.csv(lookup, file.path(out_dir, "vix_boundary_cutoff_lookup.csv"), row.names = FALSE)

set.seed(seed)
counts <- data.frame(
  M = M_values,
  Q = N / M_values,
  x = M_values * (1 - phi_true),
  reject_chi = 0,
  reject_fixed = 0,
  reject_estimated = 0,
  finite_estimated = 0
)

batch_size <- 1000L
for (start in seq(1L, reps, by = batch_size)) {
  b <- min(batch_size, reps - start + 1L)
  X <- simulate_stationary_ar1(b, N, phi_true)
  phi_hat <- burg_rows(X)

  for (i in seq_along(M_values)) {
    M <- M_values[i]
    Q <- N / M
    lr <- bel_lr_matrix(block_sums(X, M))
    nu_hat <- D_vector(M, phi_hat) / D_vector(N, phi_hat)
    stat <- nu_hat * lr

    fixed_cutoff <- reference$critical_xQ_boundary[i]
    counts$reject_chi[i] <- counts$reject_chi[i] + sum(stat > chi_critical)

    if (is.finite(fixed_cutoff)) {
      counts$reject_fixed[i] <- counts$reject_fixed[i] + sum(stat > fixed_cutoff)
    }

    grid_i <- lookup[lookup$M == M, ]
    x_hat <- M * (1 - phi_hat)
    estimated_cutoff <- approx(grid_i$x, grid_i$cutoff, xout = x_hat, rule = 2)$y
    finite <- is.finite(estimated_cutoff)
    counts$finite_estimated[i] <- counts$finite_estimated[i] + sum(finite)
    counts$reject_estimated[i] <- counts$reject_estimated[i] + sum(stat[finite] > estimated_cutoff[finite])
  }
}

result <- data.frame(
  M = counts$M,
  Q = counts$Q,
  x = counts$x,
  VC_chi2 = counts$reject_chi / reps,
  boundary_fixed_x = ifelse(is.finite(reference$critical_xQ_boundary), counts$reject_fixed / reps, NA),
  boundary_estimated_x = ifelse(counts$finite_estimated > 0,
                                counts$reject_estimated / counts$finite_estimated, NA),
  finite_cutoff_rate = counts$finite_estimated / reps,
  reps = reps,
  seed = seed
)

write.csv(result, file.path(out_dir, "vix_plugin_sensitivity.csv"), row.names = FALSE)
