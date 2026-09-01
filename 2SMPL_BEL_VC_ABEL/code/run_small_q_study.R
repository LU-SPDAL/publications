# Small-block-count experiment used to study profile failure and the transition
# away from the chi-square reference law.

source("simulation_driver.R")

build_small_q_design <- function() {
  block_lengths <- c(12L, 18L, 24L, 36L, 54L)
  sample_sizes <- list(
    balanced = c(216L, 216L),
    one_to_two = c(216L, 432L)
  )
  innovations <- list(
    c("normal", "normal"),
    c("chisq1", "chisq1"),
    c("normal", "chisq1")
  )

  rows <- list()
  k <- 0L
  for (size_name in names(sample_sizes)) {
    nn <- sample_sizes[[size_name]]
    for (M in block_lengths) {
      for (iv in innovations) {
        k <- k + 1L
        rows[[k]] <- new_scenario(
          id = sprintf("smallq_%s_M%d_%s_%s", size_name, M, iv[1], iv[2]),
          group = "smallQ_feasibility",
          N1 = nn[1], N2 = nn[2], M1 = M, M2 = M,
          p11 = 0.8, p21 = 0.8,
          innov1 = iv[1], innov2 = iv[2],
          L_rule = "loglog",
          theory_scope = "small_block_count"
        )
        rows[[k]]$size_pattern <- size_name
        rows[[k]]$dist_pattern <- paste(iv, collapse = "_")
      }
    }
  }

  design <- do.call(rbind, rows)
  rownames(design) <- NULL
  design$base_id <- design$scenario_id
  design$scenario_no <- seq_len(nrow(design))
  design
}

compile_abel_engine(rebuild = TRUE)

design <- build_small_q_design()
out_dir <- "../results/small_q/raw"

run_design(
  design = design,
  out_dir = out_dir,
  B = 10000L,
  base_seed = 2026082909,
  overwrite = FALSE
)

summarize_coverage(out_dir, levels = 0.95)
summarize_diagnostics(out_dir)
