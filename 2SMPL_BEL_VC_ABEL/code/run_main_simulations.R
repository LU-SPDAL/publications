# Run the Monte Carlo designs used in the main simulation study.
# The full run uses 10,000 replications per cell.

source("simulation_driver.R")

compile_abel_engine(rebuild = TRUE)

design <- build_full_design()
out_dir <- "../results/main_study_raw"

run_design(
  design = design,
  out_dir = out_dir,
  B = 10000L,
  base_seed = 20260829,
  overwrite = FALSE
)

summarize_coverage(out_dir)
summarize_diagnostics(out_dir)
summarize_power(out_dir)
summarize_equivalence(out_dir)
