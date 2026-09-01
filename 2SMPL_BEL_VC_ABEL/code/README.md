# Code

The main Monte Carlo calculation uses `simulation_driver.R` together with the C++ engine `bel_mc.cpp`. The driver defines the simulation designs, runs the individual cells and creates the coverage, diagnostics, power and ABEL-versus-BC summaries.

The remaining scripts have narrower roles.

- `run_main_simulations.R` runs the main simulation study.
- `unequal_block_study.R` runs the focused unequal-block experiment.
- `run_small_q_study.R` runs the small-block-count study.
- `fixed_q_reference.R` contains the fixed-block-count Gaussian reference calculation.
- `fixed_q_r1_summary.R` gives the equal-scale bridge and convergence summaries.
- `fixed_q_unequal_scale_check.R` checks the plug-in scale ratio when the block scales differ.
- `check_adjacent_link_coefficient.R` checks the adjacent-block coefficient numerically.
- `check_reported_values.R` reruns a small set of manuscript values.
- `figure1_coverage.R` reruns the primary 80-cell design and redraws Figure 1.

Development backups and preliminary estimator-comparison scripts are not part of the public workflow.
