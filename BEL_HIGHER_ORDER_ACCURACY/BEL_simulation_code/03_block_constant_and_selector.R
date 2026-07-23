# Block-constant and plug-in selector study.
#
# The grid is centred on the population value C_opt. Each design has its own
# fixed seed, so serial and parallel runs give the same Monte Carlo sample.

library(matrixStats)
library(dplyr)
library(ggplot2)

# Settings

phi_values <- c(0.2, 0.5, 0.8)
N_values <- c(256L, 512L, 1024L, 4096L)
r_values <- seq(0.5, 2, by = 0.1)

B <- 100000L
batch_size <- 500L
nominal_coverage <- 0.95
master_seed <- 2026051501L

n_cores <- max(1L, min(3L, parallel::detectCores() - 1L))

results_directory <- "bel_block_constant_results"
dir.create(results_directory, showWarnings = FALSE, recursive = TRUE)

RNGkind(
  kind = "Mersenne-Twister",
  normal.kind = "Inversion",
  sample.kind = "Rejection"
)

# Basic calculations for the standardized Gaussian AR(1) process

ar1_constants <- function(phi) {
  b <- 2 * phi / (1 - phi^2)
  C_opt <- sqrt(2 * b / 3)

  list(b = b, C_opt = C_opt)
}


simulate_ar1 <- function(B, N, phi) {
  x <- matrix(0, nrow = B, ncol = N)
  x[, 1L] <- rnorm(B)

  innovation_sd <- sqrt(1 - phi^2)

  for (t in 2L:N) {
    x[, t] <- phi * x[, t - 1L] + innovation_sd * rnorm(B)
  }

  x
}


# Block sums are obtained from row-wise cumulative sums. Dividing by sqrt(M)
# does not change the empirical likelihood ratio and improves numerical scale.

block_sums <- function(cumulative_sums, M) {
  N <- ncol(cumulative_sums)
  Q <- N %/% M
  endpoints <- seq.int(M, Q * M, by = M)

  sums <- cumulative_sums[, endpoints, drop = FALSE]

  if (Q > 1L) {
    sums[, 2L:Q] <-
      sums[, 2L:Q, drop = FALSE] -
      cumulative_sums[, endpoints[1L:(Q - 1L)], drop = FALSE]
  }

  sums / sqrt(M)
}


# Scalar empirical likelihood ratio for every row of a block-sum matrix.
# Rows whose convex hull does not contain zero are assigned Inf.

bel_ratio <- function(sums, tolerance = 1e-11, max_iterations = 60L) {
  if (is.null(dim(sums))) {
    sums <- matrix(sums, nrow = 1L)
  }

  n_rows <- nrow(sums)
  ratios <- rep(Inf, n_rows)

  row_min <- matrixStats::rowMins(sums)
  row_max <- matrixStats::rowMaxs(sums)
  all_zero <- matrixStats::rowSums2(abs(sums)) == 0

  ratios[all_zero] <- 0
  feasible <- row_min < 0 & row_max > 0

  if (!any(feasible)) {
    return(ratios)
  }

  z <- sums[feasible, , drop = FALSE]

  lower <- matrixStats::rowMaxs(ifelse(z > 0, -1 / z, -Inf))
  upper <- matrixStats::rowMins(ifelse(z < 0, -1 / z, Inf))

  boundary_adjustment <- sqrt(.Machine$double.eps)
  lower <- lower + boundary_adjustment * (1 + abs(lower))
  upper <- upper - boundary_adjustment * (1 + abs(upper))

  lambda <- numeric(nrow(z))
  scale <- 1 + matrixStats::rowSums2(abs(z))

  for (iteration in seq_len(max_iterations)) {
    denominator <- 1 + z * lambda
    score <- matrixStats::rowSums2(z / denominator)

    if (all(abs(score) <= tolerance * scale)) {
      break
    }

    move_right <- score > 0
    lower[move_right] <- lambda[move_right]
    upper[!move_right] <- lambda[!move_right]

    derivative <- -matrixStats::rowSums2((z / denominator)^2)
    proposal <- lambda - score / derivative

    outside <-
      !is.finite(proposal) |
      proposal <= lower |
      proposal >= upper

    proposal[outside] <- (lower[outside] + upper[outside]) / 2
    lambda <- proposal
  }

  value <- 2 * matrixStats::rowSums2(log1p(z * lambda))
  ratios[feasible] <- pmax(value, 0)
  ratios
}


# Pilot estimator of b = B2 / sigma^2. The series is demeaned because the
# mean is unknown in an actual application.

estimate_b <- function(x, pilot_M) {
  N <- ncol(x)
  L <- min(
    floor(log(N) * log(log(N))),
    floor(pilot_M / 2)
  )

  x_centered <- x - rowMeans(x)
  gamma_0 <- matrixStats::rowMeans2(x_centered^2)

  sigma_squared <- gamma_0
  B2 <- numeric(nrow(x_centered))

  for (h in seq_len(L)) {
    gamma_h <- matrixStats::rowSums2(
      x_centered[, 1L:(N - h), drop = FALSE] *
        x_centered[, (h + 1L):N, drop = FALSE]
    ) / N

    sigma_squared <- sigma_squared + 2 * gamma_h
    B2 <- B2 + 2 * h * gamma_h
  }

  b_raw <- ifelse(sigma_squared > 0, B2 / sigma_squared, 0)
  b_hat <- pmax(-L, pmin(b_raw, L))

  list(b_hat = b_hat, L = L)
}


make_C_grid <- function(phi, N, r_values) {
  constants <- ar1_constants(phi)
  C_opt <- constants$C_opt

  grid <- data.frame(
    source = "relative grid",
    r_nominal = r_values,
    C_nominal = r_values * C_opt
  )

  fixed_rule <- data.frame(
    source = "C = 1",
    r_nominal = 1 / C_opt,
    C_nominal = 1
  )

  grid <- bind_rows(grid, fixed_rule) %>%
    mutate(
      M = pmax(2L, as.integer(round(C_nominal * sqrt(N)))),
      Q = N %/% M,
      C_effective = M / sqrt(N),
      r_effective = C_effective / C_opt
    )

  grid
}


run_design <- function(phi, N, B, batch_size, r_values, seed,
                       nominal_coverage) {
  set.seed(seed)

  constants <- ar1_constants(phi)
  b <- constants$b
  C_opt <- constants$C_opt

  critical_value <- qchisq(nominal_coverage, df = 1)
  density_at_critical <- dchisq(critical_value, df = 1)

  grid <- make_C_grid(phi, N, r_values)
  M_values <- sort(unique(grid$M))
  n_M <- length(M_values)

  M_opt <- max(2L, as.integer(round(C_opt * sqrt(N))))
  M_fixed <- max(2L, as.integer(round(sqrt(N))))

  if (!M_opt %in% M_values) {
    M_values <- sort(c(M_values, M_opt))
    n_M <- length(M_values)
  }

  if (!M_fixed %in% M_values) {
    M_values <- sort(c(M_values, M_fixed))
    n_M <- length(M_values)
  }

  accepted <- numeric(n_M)
  failed <- numeric(n_M)
  paired_loss_sum <- numeric(n_M)
  paired_loss_squared_sum <- numeric(n_M)

  plugin_accepted <- 0
  plugin_failed <- 0
  plugin_loss_sum <- 0
  plugin_loss_squared_sum <- 0

  b_draws <- numeric(B)
  C_draws <- numeric(B)
  M_draws <- integer(B)
  Q_draws <- integer(B)
  fallback_draws <- logical(B)

  pilot_M <- max(2L, as.integer(round(sqrt(N))))
  batches <- split(seq_len(B), ceiling(seq_len(B) / batch_size))

  for (batch_number in seq_along(batches)) {
    batch_indices <- batches[[batch_number]]
    current_B <- length(batch_indices)
    x <- simulate_ar1(current_B, N, phi)
    cumulative_sums <- matrixStats::rowCumsums(x)

    acceptance_matrix <- matrix(FALSE, nrow = current_B, ncol = n_M)
    failure_matrix <- matrix(FALSE, nrow = current_B, ncol = n_M)

    for (j in seq_along(M_values)) {
      sums <- block_sums(cumulative_sums, M_values[j])
      ratio <- bel_ratio(sums)

      acceptance_matrix[, j] <- ratio <= critical_value
      failure_matrix[, j] <- !is.finite(ratio)
    }

    oracle_position <- match(M_opt, M_values)
    fixed_position <- match(M_fixed, M_values)
    oracle_acceptance <- acceptance_matrix[, oracle_position]

    accepted <- accepted + colSums(acceptance_matrix)
    failed <- failed + colSums(failure_matrix)

    paired_difference <- oracle_acceptance - acceptance_matrix
    paired_loss_sum <- paired_loss_sum + colSums(paired_difference)
    paired_loss_squared_sum <-
      paired_loss_squared_sum + colSums(paired_difference^2)

    pilot <- estimate_b(x, pilot_M)
    b_hat <- pilot$b_hat
    fallback <- b_hat <= 0

    C_hat <- rep(1, current_B)
    C_hat[!fallback] <- sqrt(2 * b_hat[!fallback] / 3)
    M_hat <- pmax(2L, as.integer(round(C_hat * sqrt(N))))
    Q_hat <- N %/% M_hat

    plugin_acceptance <- logical(current_B)
    plugin_failure <- logical(current_B)

    rows_by_M <- split(seq_len(current_B), M_hat)

    for (M_name in names(rows_by_M)) {
      rows <- rows_by_M[[M_name]]
      M <- as.integer(M_name)

      sums <- block_sums(cumulative_sums[rows, , drop = FALSE], M)
      ratio <- bel_ratio(sums)

      plugin_acceptance[rows] <- ratio <= critical_value
      plugin_failure[rows] <- !is.finite(ratio)
    }

    plugin_accepted <- plugin_accepted + sum(plugin_acceptance)
    plugin_failed <- plugin_failed + sum(plugin_failure)

    plugin_difference <- oracle_acceptance - plugin_acceptance
    plugin_loss_sum <- plugin_loss_sum + sum(plugin_difference)
    plugin_loss_squared_sum <-
      plugin_loss_squared_sum + sum(plugin_difference^2)

    b_draws[batch_indices] <- b_hat
    C_draws[batch_indices] <- C_hat
    M_draws[batch_indices] <- M_hat
    Q_draws[batch_indices] <- Q_hat
    fallback_draws[batch_indices] <- fallback
  }

  coverage_by_M <- accepted / B
  failure_by_M <- failed / B
  loss_by_M <- paired_loss_sum / B

  loss_variance <-
    (paired_loss_squared_sum - B * loss_by_M^2) / (B - 1)
  loss_mcse <- sqrt(pmax(loss_variance, 0) / B)

  grid_position <- match(grid$M, M_values)
  delta <- b / grid$C_effective + 3 * grid$C_effective / 2
  delta_opt <- 3 * C_opt

  grid_results <- grid %>%
    mutate(
      phi = phi,
      N = N,
      b = b,
      C_opt = C_opt,
      coverage = coverage_by_M[grid_position],
      coverage_error = coverage - nominal_coverage,
      coverage_mcse = sqrt(coverage * (1 - coverage) / B),
      convex_failure_rate = failure_by_M[grid_position],
      theoretical_delta = delta,
      theoretical_coverage =
        nominal_coverage -
        delta * critical_value * density_at_critical / sqrt(N),
      normalized_error =
        sqrt(N) * (nominal_coverage - coverage) /
        (critical_value * density_at_critical * delta_opt),
      normalized_theory = delta / delta_opt,
      oracle_minus_grid_coverage = loss_by_M[grid_position],
      paired_mcse = loss_mcse[grid_position],
      normalized_paired_loss =
        sqrt(N) * oracle_minus_grid_coverage /
        (critical_value * density_at_critical * delta_opt),
      normalized_paired_theory = delta / delta_opt - 1
    )

  plugin_coverage <- plugin_accepted / B
  plugin_loss <- plugin_loss_sum / B
  plugin_loss_variance <-
    (plugin_loss_squared_sum - B * plugin_loss^2) / (B - 1)

  selector_draws <- data.frame(
    phi = phi,
    N = N,
    b = b,
    C_opt = C_opt,
    b_hat = b_draws,
    C_hat = C_draws,
    r_hat = C_draws / C_opt,
    M_hat = M_draws,
    Q_hat = Q_draws,
    fallback = fallback_draws
  )

  plugin_summary <- data.frame(
    phi = phi,
    N = N,
    b = b,
    C_opt = C_opt,
    coverage = plugin_coverage,
    coverage_error = plugin_coverage - nominal_coverage,
    coverage_mcse = sqrt(plugin_coverage * (1 - plugin_coverage) / B),
    convex_failure_rate = plugin_failed / B,
    oracle_minus_plugin_coverage = plugin_loss,
    paired_mcse = sqrt(max(plugin_loss_variance, 0) / B),
    mean_C_hat = mean(C_draws),
    sd_C_hat = sd(C_draws),
    median_C_hat = median(C_draws),
    q25_C_hat = unname(quantile(C_draws, 0.25)),
    q75_C_hat = unname(quantile(C_draws, 0.75)),
    rmse_C_hat = sqrt(mean((C_draws - C_opt)^2)),
    mean_r_hat = mean(C_draws / C_opt),
    median_r_hat = median(C_draws / C_opt),
    q25_r_hat = unname(quantile(C_draws / C_opt, 0.25)),
    q75_r_hat = unname(quantile(C_draws / C_opt, 0.75)),
    fallback_rate = mean(fallback_draws),
    mean_M_hat = mean(M_draws),
    median_M_hat = median(M_draws),
    mean_Q_hat = mean(Q_draws),
    q10_Q_hat = unname(quantile(Q_draws, 0.10))
  )

  oracle_position <- match(M_opt, M_values)
  fixed_position <- match(M_fixed, M_values)

  comparison <- data.frame(
    phi = phi,
    N = N,
    method = c("Population optimum", "Plug-in", "C = 1"),
    block_constant = c(M_opt / sqrt(N), median(C_draws), M_fixed / sqrt(N)),
    block_length = c(M_opt, median(M_draws), M_fixed),
    coverage = c(
      coverage_by_M[oracle_position],
      plugin_coverage,
      coverage_by_M[fixed_position]
    ),
    coverage_mcse = c(
      sqrt(
        coverage_by_M[oracle_position] *
          (1 - coverage_by_M[oracle_position]) / B
      ),
      sqrt(plugin_coverage * (1 - plugin_coverage) / B),
      sqrt(
        coverage_by_M[fixed_position] *
          (1 - coverage_by_M[fixed_position]) / B
      )
    ),
    convex_failure_rate = c(
      failure_by_M[oracle_position],
      plugin_failed / B,
      failure_by_M[fixed_position]
    )
  )

  list(
    grid = grid_results,
    selector_draws = selector_draws,
    plugin_summary = plugin_summary,
    comparison = comparison
  )
}


# Run all designs

designs <- expand.grid(
  phi = phi_values,
  N = N_values,
  KEEP.OUT.ATTRS = FALSE
) %>%
  arrange(phi, N) %>%
  mutate(seed = master_seed + row_number())

design_list <- split(designs, seq_len(nrow(designs)))

run_one_design <- function(design) {
  run_design(
    phi = design$phi,
    N = design$N,
    B = B,
    batch_size = batch_size,
    r_values = r_values,
    seed = design$seed,
    nominal_coverage = nominal_coverage
  )
}

if (n_cores == 1L) {
  simulation_results <- lapply(design_list, run_one_design)
} else {
  cluster <- parallel::makeCluster(n_cores, outfile = "")

  parallel::clusterEvalQ(cluster, {
    library(matrixStats)
    library(dplyr)
  })

  parallel::clusterExport(
    cluster,
    c(
      "ar1_constants", "simulate_ar1", "block_sums", "bel_ratio",
      "estimate_b", "make_C_grid", "run_design", "run_one_design",
      "B", "batch_size", "r_values", "nominal_coverage"
    ),
    envir = .GlobalEnv
  )

  simulation_results <- parallel::parLapply(
    cluster,
    design_list,
    run_one_design
  )

  parallel::stopCluster(cluster)
}

# Combine and save results

grid_results <- bind_rows(lapply(simulation_results, `[[`, "grid"))
selector_draws <- bind_rows(
  lapply(simulation_results, `[[`, "selector_draws")
)
plugin_summary <- bind_rows(
  lapply(simulation_results, `[[`, "plugin_summary")
)
comparison_results <- bind_rows(
  lapply(simulation_results, `[[`, "comparison")
)

grid_best <- grid_results %>%
  filter(source == "relative grid") %>%
  group_by(phi, N) %>%
  slice_min(abs(coverage_error), n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  dplyr::select(
    phi, N, C_opt,
    grid_best_C = C_effective,
    grid_best_r = r_effective,
    grid_best_coverage = coverage
  )

summary_table <- plugin_summary %>%
  left_join(grid_best, by = c("phi", "N", "C_opt")) %>%
  dplyr::select(
    phi, N, C_opt,
    grid_best_C, grid_best_r, grid_best_coverage,
    median_C_hat, q25_C_hat, q75_C_hat,
    median_r_hat, q25_r_hat, q75_r_hat,
    coverage, coverage_mcse, convex_failure_rate,
    fallback_rate, mean_Q_hat, q10_Q_hat
  )

write.csv(
  grid_results,
  file.path(results_directory, "grid_results.csv"),
  row.names = FALSE
)

write.csv(
  plugin_summary,
  file.path(results_directory, "plugin_summary.csv"),
  row.names = FALSE
)

write.csv(
  comparison_results,
  file.path(results_directory, "coverage_comparison.csv"),
  row.names = FALSE
)

write.csv(
  summary_table,
  file.path(results_directory, "summary_table.csv"),
  row.names = FALSE
)

saveRDS(
  selector_draws,
  file.path(results_directory, "selector_draws.rds"),
  compress = "xz"
)

# Diagnostic figures

plot_grid <- grid_results %>%
  filter(source == "relative grid") %>%
  mutate(
    phi_label = paste0("phi == ", phi),
    N_label = paste0("N = ", format(N, big.mark = ","))
  )

coverage_figure <- ggplot(
  plot_grid,
  aes(x = r_effective, y = coverage)
) +
  geom_hline(yintercept = nominal_coverage, linetype = 3, colour = "grey40") +
  geom_vline(xintercept = 1, linetype = 3, colour = "grey40") +
  geom_line(linewidth = 0.55, colour = "black") +
  geom_point(size = 1.4, colour = "black") +
  geom_line(
    aes(y = theoretical_coverage),
    linewidth = 0.65,
    linetype = 2,
    colour = "#2B6CB0"
  ) +
  facet_grid(
    phi_label ~ N_label,
    scales = "free_y",
    labeller = labeller(phi_label = label_parsed)
  ) +
  labs(
    x = expression(r == C / C[opt]),
    y = "Empirical coverage"
  ) +
  theme_bw(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

ggsave(
  file.path(results_directory, "coverage_over_C_grid.pdf"),
  coverage_figure,
  width = 10,
  height = 7
)

ggsave(
  file.path(results_directory, "coverage_over_C_grid.png"),
  coverage_figure,
  width = 10,
  height = 7,
  dpi = 300
)


normalized_figure <- ggplot(
  plot_grid,
  aes(x = r_effective, y = normalized_error)
) +
  geom_vline(xintercept = 1, linetype = 3, colour = "grey40") +
  geom_line(linewidth = 0.55, colour = "black") +
  geom_point(size = 1.4, colour = "black") +
  geom_line(
    aes(y = normalized_theory),
    linewidth = 0.65,
    linetype = 2,
    colour = "#2B6CB0"
  ) +
  facet_grid(phi_label ~ N_label) +
  labs(
    x = expression(r == C / C[opt]),
    y = "Normalized coverage error"
  ) +
  theme_bw(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

ggsave(
  file.path(results_directory, "normalized_error_over_C_grid.pdf"),
  normalized_figure,
  width = 10,
  height = 7
)


paired_figure <- ggplot(
  plot_grid,
  aes(x = r_effective, y = normalized_paired_loss)
) +
  geom_hline(yintercept = 0, linetype = 3, colour = "grey40") +
  geom_vline(xintercept = 1, linetype = 3, colour = "grey40") +
  geom_line(linewidth = 0.55, colour = "black") +
  geom_point(size = 1.4, colour = "black") +
  geom_line(
    aes(y = normalized_paired_theory),
    linewidth = 0.65,
    linetype = 2,
    colour = "#2B6CB0"
  ) +
  facet_grid(phi_label ~ N_label) +
  labs(
    x = expression(r == C / C[opt]),
    y = "Normalized coverage loss relative to the population optimum"
  ) +
  theme_bw(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

ggsave(
  file.path(results_directory, "paired_coverage_loss.pdf"),
  paired_figure,
  width = 10,
  height = 7
)


selector_figure <- plugin_summary %>%
  mutate(phi_label = paste0("phi == ", phi)) %>%
  ggplot(aes(x = factor(N), y = median_r_hat, group = 1)) +
  geom_hline(yintercept = 1, linetype = 3, colour = "grey40") +
  geom_errorbar(
    aes(ymin = q25_r_hat, ymax = q75_r_hat),
    width = 0.12,
    linewidth = 0.5
  ) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.8) +
  facet_wrap(~ phi_label, nrow = 1) +
  labs(
    x = "Sample size",
    y = expression(hat(C) / C[opt])
  ) +
  theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank())

ggsave(
  file.path(results_directory, "plugin_constant_convergence.pdf"),
  selector_figure,
  width = 8,
  height = 3.2
)


comparison_figure <- comparison_results %>%
  mutate(
    phi_label = paste0("phi = ", phi),
    method = factor(
      method,
      levels = c("Population optimum", "Plug-in", "C = 1")
    )
  ) %>%
  ggplot(aes(x = N, y = coverage, colour = method, shape = method)) +
  geom_hline(yintercept = nominal_coverage, linetype = 3, colour = "grey40") +
  geom_line(linewidth = 0.55) +
  geom_point(size = 1.8) +
  facet_wrap(~ phi_label, nrow = 1) +
  scale_x_continuous(
    breaks = N_values,
    labels = format(N_values, big.mark = ",")
  ) +
  labs(
    x = "Sample size",
    y = "Empirical coverage",
    colour = NULL,
    shape = NULL
  ) +
  theme_bw(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

ggsave(
  file.path(results_directory, "plugin_coverage_comparison.pdf"),
  comparison_figure,
  width = 8,
  height = 3.4
)
