# File guide

The files are grouped by the part of the paper they support.

| Paper item | Main files |
| --- | --- |
| Main coverage study and Table 1 | `results/primary/coverage_summary.csv`, `figures/figure1_coverage.pdf` |
| Robustness and bandwidth checks | `results/primary/robustness_summary.csv`, `results/primary/bandwidth_summary.csv` |
| Strong-persistence example | `results/primary/strong_persistence_summary.csv` |
| Unequal block lengths | `results/unequal_blocks/block_ratio_coverage_95.csv`, `results/unequal_blocks/block_ratio_full_results.csv`, `code/unequal_block_study.R` |
| Adjacent-block coefficient check | `code/check_adjacent_link_coefficient.R`, `results/validation/adjacent_link_q_scaling_summary.csv` |
| Small block counts | `results/small_q/bridge_table.csv`, `results/fixed_q/bridge_summary.csv` |
| Fixed-block-count Gaussian reference law | `code/fixed_q_reference.R`, `code/fixed_q_r1_summary.R`, `code/fixed_q_unequal_scale_check.R` |
| Brent application and Table 3 | `results/application/brent_table.csv`, `results/application/application_summary.csv`, `figures/figure2_brent_profile.pdf` |
| Selected consistency checks | `code/check_reported_values.R` |

Files with date stamps, backup labels and preliminary estimator comparisons were part of the working directory but are not included here. They are not needed for the results reported in the paper.
