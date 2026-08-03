ratio_dgps <- names(dgp_catalog)
ratio_n <- c(200L, 500L, 1000L, 2000L, 4096L)
ratio_list <- vector("list", length(ratio_dgps) * length(ratio_n))
position <- 1L

for (dgp_name in ratio_dgps) {
  for (n in ratio_n) {
    block_table <- data.table(
      block_rule = c("N^(1/3)", "sqrt(N)"),
      M = pmax(2L, as.integer(round(c(n^(1 / 3), sqrt(n)))))
    )
    block_table <- unique(block_table, by = "M")

    ans <- run_fixed_setting(
      dgp_name = dgp_name,
      n = n,
      m_values = block_table$M,
      alpha = alpha_levels,
      reps = replications(n),
      seed = seeds$ratio + position
    )

    ans <- merge(ans, block_table, by = "M", all.x = TRUE)
    ratio_list[[position]] <- ans[
      method %in% c(
        "VB_population", "VB_b", "VB_direct", "VB_stabilized",
        "VB_ar_bic", "VB_ar_hq", "VB_hybrid"
      )
    ]
    position <- position + 1L
  }
}

variance_ratio_estimators <- rbindlist(ratio_list, use.names = TRUE)

primary_tuning <- ratio_tuning
screen_dgps <- c("AR1_08_G", "ARMA11_08_m05_G", "MA2_m04_025_G")
screen_n <- c(500L, 1000L)
tuning_list <- list()
position <- 1L

for (eta in c(0.25, 0.50, 0.75)) {
  for (threshold in c(0.10, 0.20)) {
    ratio_tuning$eta0 <- eta
    ratio_tuning$c0 <- threshold

    for (dgp_name in screen_dgps) {
      for (n in screen_n) {
        m_values <- unique(pmax(2L, as.integer(round(c(n^(1 / 3), sqrt(n))))))
        setting_id <- match(dgp_name, screen_dgps) * 10L + match(n, screen_n)

        ans <- run_fixed_setting(
          dgp_name = dgp_name,
          n = n,
          m_values = m_values,
          alpha = 0.05,
          reps = replications(n),
          seed = seeds$ratio + 10000L + setting_id
        )[method == "VB_hybrid"]

        ans[, `:=`(eta0 = eta, c0 = threshold)]
        tuning_list[[position]] <- ans
        position <- position + 1L
      }
    }
  }
}

ratio_tuning <- primary_tuning
variance_ratio_tuning <- rbindlist(tuning_list, use.names = TRUE)

table1_dgps <- c("AR1_08_G", "ARMA11_08_m05_G", "MA2_m04_025_G")
table1_sizes <- data.table(
  N = c(256L, 1024L),
  M = c(16L, 32L),
  reps = c(50000L, 40000L)
)

table1_list <- vector("list", length(table1_dgps) * nrow(table1_sizes))
position <- 1L

for (dgp_name in table1_dgps) {
  for (i in seq_len(nrow(table1_sizes))) {
    setting <- table1_sizes[i]

    ans <- run_fixed_setting(
      dgp_name = dgp_name,
      n = setting$N,
      m_values = setting$M,
      alpha = 0.05,
      reps = setting$reps,
      seed = seeds$ratio + 20000L + position
    )

    ans[, block_rule := "sqrt(N)"]
    table1_list[[position]] <- ans[
      method %in% c("VB_population", "VB_b", "VB_ar_hq", "VB_hybrid")
    ]
    position <- position + 1L
  }
}

results_4096 <- variance_ratio_estimators[
  alpha == 0.05 &
    block_rule == "sqrt(N)" &
    N == 4096L &
    dgp %in% table1_dgps &
    method %in% c("VB_population", "VB_b", "VB_ar_hq", "VB_hybrid")
]

variance_ratio_table1 <- rbindlist(
  list(rbindlist(table1_list), results_4096),
  use.names = TRUE,
  fill = TRUE
)

process_labels <- c(
  AR1_08_G = "AR(1)",
  ARMA11_08_m05_G = "ARMA(1,1)",
  MA2_m04_025_G = "MA(2)"
)

method_labels_table1 <- c(
  VB_population = "Population",
  VB_b = "b-based",
  VB_ar_hq = "AR-HQ",
  VB_hybrid = "Proposed hybrid"
)

variance_ratio_table1[, process := factor(
  process_labels[dgp],
  levels = c("AR(1)", "ARMA(1,1)", "MA(2)")
)]
variance_ratio_table1[, method_name := factor(
  method_labels_table1[method],
  levels = unname(method_labels_table1)
)]
setorder(variance_ratio_table1, process, N, method_name)

table1_coverage <- dcast(
  variance_ratio_table1,
  process + N + M + Q + reps ~ method_name,
  value.var = "coverage"
)

table1_mcse <- dcast(
  variance_ratio_table1,
  process + N + M + Q + reps ~ method_name,
  value.var = "mc_se"
)
