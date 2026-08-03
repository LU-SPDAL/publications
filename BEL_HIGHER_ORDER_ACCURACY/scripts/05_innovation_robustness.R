robust_phi <- c(0.5, 0.8)
robust_n <- c(500L, 1000L, 4096L)
robustness_list <- list()
position <- 1L

for (innovation in innovation_table$innovation) {
  for (phi in robust_phi) {
    dgp_name <- paste0("robust_", innovation, "_", format(phi, trim = TRUE))
    dgp_catalog[[dgp_name]] <- list(
      label = paste0(innovation, " AR(1), phi = ", phi),
      ar = c(phi, 0),
      ma = c(0, 0),
      innovation = innovation
    )

    for (n in robust_n) {
      ans <- run_fixed_setting(
        dgp_name = dgp_name,
        n = n,
        m_values = as.integer(round(sqrt(n))),
        alpha = alpha_levels,
        reps = replications(n),
        seed = seeds$robustness + position
      )

      ans[, `:=`(
        phi = phi,
        theorem_design = innovation_table[
          innovation == dgp_catalog[[dgp_name]]$innovation,
          theorem
        ]
      )]

      robustness_list[[position]] <- ans[
        method %in% c("BEL", "VB_population", "VB_hybrid")
      ]
      position <- position + 1L
    }
  }
}

innovation_robustness <- rbindlist(robustness_list, use.names = TRUE)

n_table2 <- 1024L
m_table2 <- 32L
reps_table2 <- 40000L
table2_list <- list()
position <- 1L

for (phi in robust_phi) {
  for (innovation in innovation_table$innovation) {
    dgp_name <- paste0(
      "table2_", innovation, "_phi_",
      gsub("\\.", "", format(phi, trim = TRUE))
    )

    dgp_catalog[[dgp_name]] <- list(
      label = paste0(innovation, " AR(1), phi = ", phi),
      ar = c(phi, 0),
      ma = c(0, 0),
      innovation = innovation
    )

    ans <- run_fixed_setting(
      dgp_name = dgp_name,
      n = n_table2,
      m_values = m_table2,
      alpha = 0.05,
      reps = reps_table2,
      seed = seeds$robustness + 50000L + position
    )

    ans[, `:=`(
      phi = phi,
      theorem_design = innovation_table[
        innovation == dgp_catalog[[dgp_name]]$innovation,
        theorem
      ]
    )]

    table2_list[[position]] <- ans[
      method %in% c("BEL", "VB_population", "VB_hybrid")
    ]
    position <- position + 1L
  }
}

innovation_robustness_table2 <- rbindlist(table2_list, use.names = TRUE)

innovation_labels <- c(
  gaussian = "Gaussian",
  laplace = "Laplace",
  centered_exponential = "Centered exponential",
  gamma4 = "Centered Gamma(4)",
  contaminated_normal = "Contaminated normal",
  bernoulli = "Centered Bernoulli",
  t5 = "Student t5"
)

method_labels_table2 <- c(
  BEL = "BEL",
  VB_population = "Population combined",
  VB_hybrid = "Hybrid combined"
)

innovation_robustness_table2[, innovation_label := factor(
  innovation_labels[innovation],
  levels = unname(innovation_labels)
)]
innovation_robustness_table2[, method_name := factor(
  method_labels_table2[method],
  levels = unname(method_labels_table2)
)]
setorder(innovation_robustness_table2, phi, innovation_label, method_name)

table2_coverage <- dcast(
  innovation_robustness_table2,
  phi + innovation_label + N + M + Q + reps + theorem_design ~ method_name,
  value.var = "coverage"
)

table2_mcse <- dcast(
  innovation_robustness_table2,
  phi + innovation_label + N + M + Q + reps + theorem_design ~ method_name,
  value.var = "mc_se"
)
