phi_grid <- c(0, 0.2, 0.4, 0.6, 0.8, 0.9)
sample_sizes <- c(512L, 4096L)
dependence_list <- list()
position <- 1L

for (phi in phi_grid) {
  dgp_name <- paste0("AR1_phi_", gsub("\\.", "_", format(phi, trim = TRUE)))
  dgp_catalog[[dgp_name]] <- list(
    label = paste0("Gaussian AR(1), phi = ", phi),
    ar = c(phi, 0),
    ma = c(0, 0),
    innovation = "gaussian"
  )

  for (n in sample_sizes) {
    ans <- run_fixed_setting(
      dgp_name = dgp_name,
      n = n,
      m_values = as.integer(round(sqrt(n))),
      alpha = 0.05,
      reps = replications(n),
      seed = seeds$dependence + position
    )[method %in% figure_methods]

    ans[, phi := phi]
    dependence_list[[position]] <- ans
    position <- position + 1L
  }
}

dependence_sensitivity <- rbindlist(dependence_list, use.names = TRUE)
