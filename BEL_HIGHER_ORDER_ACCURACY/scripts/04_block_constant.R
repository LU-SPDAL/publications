constant_n <- c(200L, 500L, 1000L, 2000L, 4096L)
constant_n_non_gaussian <- c(500L, 1000L, 4096L)
relative_grid <- c(
  0.25, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90,
  1.00, 1.10, 1.20, 1.30, 1.50, 1.75, 2.00
)

constant_designs <- list(
  list(phi = 0.2, innovation = "gaussian", N = constant_n),
  list(phi = 0.5, innovation = "gaussian", N = constant_n),
  list(phi = 0.8, innovation = "gaussian", N = constant_n),
  list(phi = 0.8, innovation = "laplace", N = constant_n_non_gaussian),
  list(phi = 0.8, innovation = "centered_exponential", N = constant_n_non_gaussian),
  list(phi = 0.8, innovation = "gamma4", N = constant_n_non_gaussian)
)

constant_list <- list()
selector_list <- list()
position <- 1L

for (design in constant_designs) {
  phi <- design$phi
  innovation <- design$innovation
  dgp_name <- paste("constant", innovation, format(phi, trim = TRUE), sep = "_")

  dgp_catalog[[dgp_name]] <- list(
    label = paste0(innovation, " AR(1), phi = ", phi),
    ar = c(phi, 0),
    ma = c(0, 0),
    innovation = innovation
  )

  for (n in design$N) {
    truth <- population_truth(dgp_catalog[[dgp_name]], n)

    mapping <- rbindlist(lapply(alpha_levels, function(alpha) {
      target <- c_star(
        truth$b,
        truth$kappa_c,
        alpha,
        selector_tuning$c_min,
        selector_tuning$c_max
      )

      data.table(
        alpha = alpha,
        relative_C = relative_grid,
        C_star = target,
        C = relative_grid * target,
        M = pmax(2L, as.integer(round(relative_grid * target * sqrt(n))))
      )
    }))
    mapping <- unique(mapping, by = c("alpha", "relative_C", "M"))

    reference_m <- mapping[
      , .SD[which.min(abs(relative_C - 1))],
      by = alpha
    ][match(alpha_levels, alpha), M]

    fixed <- run_oracle_grid(
      dgp_name = dgp_name,
      n = n,
      m_values = sort(unique(mapping$M)),
      alpha = alpha_levels,
      reference_m = reference_m,
      reps = constant_replications(n),
      seed = seeds$constant + position
    )
    fixed <- merge(fixed, mapping, by = c("alpha", "M"), allow.cartesian = TRUE)
    fixed[, phi := phi]
    constant_list[[position]] <- fixed

    selected <- run_selected_setting(
      dgp_name = dgp_name,
      n = n,
      alpha = alpha_levels,
      reps = replications(n),
      seed = seeds$selector + position
    )
    selected[, phi := phi]
    selector_list[[position]] <- selected
    position <- position + 1L
  }
}

block_constant_grid <- rbindlist(constant_list, use.names = TRUE)
block_constant_grid[, empirical_minimum := min(abs_error), by = .(dgp, N, alpha)]
block_constant_grid[, excess_error := abs_error - empirical_minimum]
block_selector_study <- rbindlist(selector_list, use.names = TRUE)
