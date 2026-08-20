# Fixed-Q and non-Gaussian checks for the boundary limit laws

source(file.path("R", "bel_core.R"))
source(file.path("R", "boundary_limits.R"))

Rcpp::sourceCpp(file.path("src", "simulation_core.cpp"), rebuild = FALSE)
RcppParallel::setThreadOptions(numThreads = as.integer(Sys.getenv("AR1_BEL_CORES", "1")))

alpha <- 0.05
chi_critical <- qchisq(1 - alpha, 1)
out_dir <- file.path("results", "recomputed", "boundary_validations")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ks_extended <- function(a, b) {
  grid <- sort(unique(c(a[is.finite(a)], b[is.finite(b)])))
  if (!length(grid)) return(abs(mean(is.finite(a)) - mean(is.finite(b))))
  Fa <- vapply(grid, function(z) mean(a <= z), numeric(1))
  Fb <- vapply(grid, function(z) mean(b <= z), numeric(1))
  max(abs(Fa - Fb), abs(mean(is.finite(a)) - mean(is.finite(b))))
}

ks_to_chi <- function(x) {
  finite <- sort(x[is.finite(x)])
  n <- length(x)
  m <- length(finite)
  d <- 1 - m / n
  if (m) {
    F0 <- pchisq(finite, 1)
    i <- seq_len(m)
    d <- max(d, abs(i / n - F0), abs((i - 1) / n - F0))
  }
  d
}

# Direct fixed-Q Gaussian check used for Supplementary Table S3.
Q_fixed <- 20L
reps_fixedQ <- 10000L
base_seed <- 2026081202L

designs <- list(
  list(regime = "Positive", boundary = "positive", parity = "even",
       M = c(10, 20, 40, 80), x = c(0.25, 1, 4)),
  list(regime = "Negative, even M", boundary = "negative", parity = "even",
       M = c(10, 20, 40, 80), x = c(0.25, 1, 2)),
  list(regime = "Negative, odd M", boundary = "negative", parity = "odd",
       M = c(11, 21, 41, 81), x = c(0.8, 1.8, 3))
)

finite_rows <- list()
k <- 1L
for (d in designs) {
  for (x in d$x) {
    for (M in d$M) {
      phi <- if (d$boundary == "positive") 1 - x / M else -1 + x / M
      raw <- simulate_gaussian_ar1_block_lr_cpp(
        reps_fixedQ, M, Q_fixed, phi, base_seed + k
      )
      vc <- nu_ar1(M * Q_fixed, M, phi) * raw

      finite_rows[[k]] <- data.frame(
        regime = d$regime,
        boundary = d$boundary,
        parity = d$parity,
        x = x,
        M = M,
        coverage = mean(vc <= chi_critical),
        failure = mean(!is.finite(vc)),
        KS_chi = ks_to_chi(vc),
        q95_ratio = as.numeric(quantile(vc, 0.95, type = 1, names = FALSE)) / chi_critical,
        stringsAsFactors = FALSE
      )
      finite_rows[[k]]$draws <- I(list(vc))
      k <- k + 1L
    }
  }
}
finite <- do.call(rbind, finite_rows)

summary_rows <- list()
k <- 1L
for (regime in unique(finite$regime)) {
  for (x in unique(finite$x[finite$regime == regime])) {
    z <- finite[finite$regime == regime & finite$x == x, ]
    pairwise <- combn(seq_len(nrow(z)), 2, function(ii) {
      ks_extended(z$draws[[ii[1]]], z$draws[[ii[2]]])
    })

    summary_rows[[k]] <- data.frame(
      regime = regime,
      x = x,
      coverage = mean(z$coverage),
      coverage_min = min(z$coverage),
      coverage_max = max(z$coverage),
      failure = max(z$failure),
      KS_chi1 = mean(z$KS_chi),
      pairwise_KS = max(pairwise),
      q95_ratio = mean(z$q95_ratio)
    )
    k <- k + 1L
  }
}
fixedQ_summary <- do.call(rbind, summary_rows)
write.csv(fixedQ_summary, file.path(out_dir, "S3_fixedQ_boundary.csv"), row.names = FALSE)

# Non-Gaussian finite-M check used for Supplementary Table S2.
# The original development seed was not recoverable from the archived folders.
# This fixed seed makes the reconstructed experiment reproducible.
finite_reps <- 20000L
limit_reps <- 300000L
burn_in <- 3000L
seed_ng <- 2026082005L

innovation_draw <- function(n, law) {
  switch(
    law,
    t5 = rt(n, 5) / sqrt(5 / 3),
    chisq = (rchisq(n, 1) - 1) / sqrt(2),
    exponential = rexp(n) - 1
  )
}

simulate_non_gaussian_lr <- function(M, Q, phi, law, reps, seed, batch = 1000L) {
  set.seed(seed)
  out <- numeric(reps)
  pos <- 1L
  n <- M * Q

  while (pos <= reps) {
    b <- min(batch, reps - pos + 1L)
    y <- rep(0, b)

    for (t in seq_len(burn_in)) {
      y <- phi * y + innovation_draw(b, law)
    }

    block_sums <- matrix(0, nrow = b, ncol = Q)
    for (t in seq_len(n)) {
      y <- phi * y + innovation_draw(b, law)
      q <- (t - 1L) %/% M + 1L
      block_sums[, q] <- block_sums[, q] + y
    }

    out[pos:(pos + b - 1L)] <- bel_lr_matrix(block_sums)
    pos <- pos + b
  }

  out
}

ng_design <- data.frame(
  regime = c("Positive", "Negative, even M", "Negative, odd M, even Q", "Negative, odd M, odd Q"),
  boundary = c("positive", "negative", "negative", "negative"),
  parity = c("even", "even", "odd", "odd"),
  M = c(100L, 100L, 101L, 101L),
  Q = c(10L, 10L, 10L, 7L),
  stringsAsFactors = FALSE
)
laws <- c(t5 = "standardized t5", chisq = "centered chi-square(1)", exponential = "centered exponential")

ng_rows <- list()
k <- 1L
for (i in seq_len(nrow(ng_design))) {
  d <- ng_design[i, ]
  phi <- if (d$boundary == "positive") 1 - 1 / d$M else -1 + 1 / d$M
  obj <- boundary_covariance(1, d$Q, d$boundary, d$parity)
  limit_lr <- simulate_lr_from_covariance(obj$Sigma, limit_reps, seed_ng + 1000L * i)
  limit_stat <- obj$parameters$nu * limit_lr
  cutoff <- extended_cutoff(limit_stat, alpha)["cutoff"]
  hull_limit <- mean(!is.finite(limit_stat))

  for (j in seq_along(laws)) {
    raw <- simulate_non_gaussian_lr(
      d$M, d$Q, phi, names(laws)[j], finite_reps,
      seed_ng + 10000L * i + j
    )
    stat <- nu_ar1(d$M * d$Q, d$M, phi) * raw

    ng_rows[[k]] <- data.frame(
      regime = d$regime,
      Q = d$Q,
      M = d$M,
      innovation = laws[j],
      rejection = mean(stat > cutoff),
      hull_finite = mean(!is.finite(stat)),
      hull_limit = hull_limit,
      KS = ks_extended(stat, limit_stat)
    )
    k <- k + 1L
  }
}

non_gaussian <- do.call(rbind, ng_rows)
write.csv(non_gaussian, file.path(out_dir, "S2_non_gaussian_boundary.csv"), row.names = FALSE)
