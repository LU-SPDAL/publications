# Reproducing the calculations

Run the R scripts from the `code` directory.

## Main Monte Carlo study

```r
source("run_main_simulations.R")
```

This runs the simulation designs defined in `simulation_driver.R` and writes the raw scenario files and summary tables to the results directory. The production setting is 10,000 replications per cell.

## Unequal block lengths

```r
source("unequal_block_study.R")
run_blockratio_experiment()
```

## Small block counts

```r
source("run_small_q_study.R")
```

## Fixed block counts

The fixed-block-count calculations are separated because the Gaussian reference simulation is more expensive than the ordinary time-series runs.

```r
source("fixed_q_reference.R")
source("fixed_q_r1_summary.R")
```

The unequal-scale check is in

```r
source("fixed_q_unequal_scale_check.R")
```

## Figure 1

Figure 1 can be regenerated from the primary 80-cell design with

```r
source("figure1_coverage.R")
```

The Brent profile used for Figure 2 is included in `figures/figure2_brent_profile.pdf`. The compact numerical values reported from the application are in `results/application/`.

## Numerical checks

`check_adjacent_link_coefficient.R` checks the linear adjacent-block coefficient in the unequal-block calculation. `check_reported_values.R` reruns a small collection of cells used to compare the stored results with the values quoted in the manuscript.
