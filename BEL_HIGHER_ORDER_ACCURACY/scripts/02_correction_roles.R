figure_methods <- c(
  "BEL", "B", "V_population", "V_hybrid",
  "VB_population", "VB_hybrid"
)

role_settings <- list(
  list(N = 512L, M = c(4L, 8L, 16L, 32L, 64L)),
  list(N = 4096L, M = c(8L, 16L, 32L, 64L, 128L, 256L))
)

role_list <- vector("list", length(role_settings))

for (i in seq_along(role_settings)) {
  setting <- role_settings[[i]]
  role_list[[i]] <- run_fixed_setting(
    dgp_name = "AR1_05_G",
    n = setting$N,
    m_values = setting$M,
    alpha = 0.05,
    reps = replications(setting$N),
    seed = seeds$roles + i
  )[method %in% figure_methods]
}

correction_roles <- rbindlist(role_list, use.names = TRUE)
