# Exact finite-M Gaussian validation of the limiting (x, Q) map

source(file.path("R", "bel_core.R"))
source(file.path("R", "boundary_limits.R"))

x_grid <- c(0.25, 1, 3, 6)
Q_grid <- c(10, 20, 50)
M_even <- c(20, 50, 100, 200)
M_odd <- c(21, 51, 101, 201)
reps <- 10000L
base_seed <- 910000L
out_dir <- file.path("results", "recomputed", "finiteM_validation")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

summarize_lr <- function(lr, scales) {
  do.call(rbind, lapply(names(scales), function(method) {
    stat <- scales[[method]] * lr
    rejection <- mean(stat > qchisq(0.95, 1))

    data.frame(
      method = method,
      scale = scales[[method]],
      rejection = rejection,
      mcse = sqrt(rejection * (1 - rejection) / length(lr)),
      hull_failure = mean(!is.finite(lr))
    )
  }))
}

one_exact <- function(x, Q, M, boundary, seed) {
  phi <- if (boundary == "positive") 1 - x / M else -1 + x / M
  G <- exact_ar1_block_covariance(M, Q, phi)
  lr <- simulate_lr_from_covariance(G, reps, seed)
  nu <- nu_ar1(M * Q, M, phi)
  aG <- aG_ar1(M * Q, M, phi)
  B <- 1 - aG / (M * Q)

  out <- summarize_lr(lr, c(Raw = 1, Variance = nu, Bartlett = B, Combined = nu * B))
  out$source <- "finite"
  out$boundary <- boundary
  out$parity <- if (boundary == "positive") "not_applicable" else if (M %% 2L == 0L) "even" else "odd"
  out$x <- x
  out$Q <- Q
  out$M <- M
  out$phi <- phi
  out
}

one_limit <- function(x, Q, boundary, parity, seed) {
  obj <- boundary_covariance(x, Q, boundary, parity)
  lr <- simulate_lr_from_covariance(obj$Sigma, reps, seed)
  p <- obj$parameters
  B <- p$bartlett_multiplier

  out <- summarize_lr(lr, c(Raw = 1, Variance = p$nu, Bartlett = B, Combined = p$nu * B))
  out$source <- "limit"
  out$boundary <- boundary
  out$parity <- if (boundary == "positive") "not_applicable" else parity
  out$x <- x
  out$Q <- Q
  out$M <- NA_integer_
  out$phi <- NA_real_
  out
}

rows <- list()
k <- 1L

for (x in x_grid) {
  for (Q in Q_grid) {
    for (M in M_even) {
      rows[[k]] <- one_exact(x, Q, M, "positive", base_seed + k)
      k <- k + 1L
      rows[[k]] <- one_exact(x, Q, M, "negative", base_seed + k)
      k <- k + 1L
    }

    for (M in M_odd) {
      rows[[k]] <- one_exact(x, Q, M, "negative", base_seed + k)
      k <- k + 1L
    }

    rows[[k]] <- one_limit(x, Q, "positive", "even", base_seed + k)
    k <- k + 1L
    rows[[k]] <- one_limit(x, Q, "negative", "even", base_seed + k)
    k <- k + 1L
    rows[[k]] <- one_limit(x, Q, "negative", "odd", base_seed + k)
    k <- k + 1L
  }
}

out <- do.call(rbind, rows)
rownames(out) <- NULL
write.csv(out, file.path(out_dir, "finiteM_validation.csv"), row.names = FALSE)

finite <- subset(out, source == "finite")
limit <- subset(out, source == "limit", select = c(boundary, parity, x, Q, method, rejection, hull_failure))
names(limit)[6:7] <- c("rejection_limit", "hull_failure_limit")
comparison <- merge(finite, limit, by = c("boundary", "parity", "x", "Q", "method"), sort = FALSE)
comparison$abs_rejection_difference <- abs(comparison$rejection - comparison$rejection_limit)
comparison$abs_hull_difference <- abs(comparison$hull_failure - comparison$hull_failure_limit)
write.csv(comparison, file.path(out_dir, "finiteM_comparison.csv"), row.names = FALSE)
