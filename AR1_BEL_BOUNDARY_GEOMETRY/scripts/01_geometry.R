# Rectangular (x, Q) limit experiment used in the main simulation section

source(file.path("R", "bel_core.R"))
source(file.path("R", "boundary_limits.R"))

x_grid <- c(0.25, 0.5, 1, 2, 3, 4, 6, 8, 12)
Q_grid <- c(5, 6, 8, 10, 15, 20, 30, 50, 100, 200)
reps <- 20000L
base_seed <- 2608160L
out_dir <- file.path("results", "recomputed", "geometry")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_file <- file.path(out_dir, "geometry_results_long.csv")

summarize_cell <- function(x, Q, boundary, parity, seed) {
  obj <- boundary_covariance(x, Q, boundary, parity)
  lr <- simulate_lr_from_covariance(obj$Sigma, reps, seed)
  p <- obj$parameters

  scales <- c(
    Raw = 1,
    Variance = p$nu,
    Bartlett = p$bartlett_multiplier,
    Combined = p$nu * p$bartlett_multiplier
  )

  rows <- lapply(names(scales), function(method) {
    stat <- scales[[method]] * lr
    rejection <- mean(stat > qchisq(0.95, 1))

    data.frame(
      boundary = boundary,
      parity = if (boundary == "positive") "not_applicable" else parity,
      x = x,
      Q = Q,
      method = method,
      scale = scales[[method]],
      nu = p$nu,
      theta_block = p$block_decay,
      theta_bartlett = p$theta,
      aG_over_N = p$aG_over_N,
      bartlett_multiplier = p$bartlett_multiplier,
      rejection = rejection,
      coverage = 1 - rejection,
      mcse = sqrt(rejection * (1 - rejection) / reps),
      q95 = as.numeric(quantile(stat, 0.95, names = FALSE)),
      q95_over_chisq = as.numeric(quantile(stat, 0.95, names = FALSE)) / qchisq(0.95, 1),
      hull_failure = mean(!is.finite(lr)),
      in_4_6_band = rejection >= 0.04 & rejection <= 0.06,
      reps = reps,
      seed = seed
    )
  })

  do.call(rbind, rows)
}

cells <- list()
k <- 1L

for (x in x_grid) {
  for (Q in Q_grid) {
    cells[[k]] <- list(x = x, Q = Q, boundary = "positive", parity = "even")
    k <- k + 1L
    cells[[k]] <- list(x = x, Q = Q, boundary = "negative", parity = "even")
    k <- k + 1L
    cells[[k]] <- list(x = x, Q = Q, boundary = "negative", parity = "odd")
    k <- k + 1L
  }
}

run_one <- function(i) {
  d <- cells[[i]]
  summarize_cell(d$x, d$Q, d$boundary, d$parity, base_seed + i)
}

n_cores <- as.integer(Sys.getenv("AR1_BEL_CORES", "1"))

if (n_cores > 1L) {
  cl <- parallel::makeCluster(n_cores)
  parallel::clusterExport(
    cl,
    c(
      "cells", "reps", "base_seed", "summarize_cell",
      "boundary_covariance", "positive_boundary_parameters",
      "negative_boundary_parameters", "K_positive", "L_negative",
      "simulate_lr_from_covariance", "bel_lr_matrix"
    ),
    envir = environment()
  )
  results <- parallel::parLapply(cl, seq_along(cells), run_one)
  parallel::stopCluster(cl)
} else {
  results <- lapply(seq_along(cells), run_one)
}

geometry <- do.call(rbind, results)
rownames(geometry) <- NULL
write.csv(geometry, out_file, row.names = FALSE)
