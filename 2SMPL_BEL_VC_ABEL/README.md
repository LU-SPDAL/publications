# Two-sample blockwise empirical likelihood for a difference of means

This folder contains the numerical material for the paper **Variance-Calibrated Adjusted Two-Sample Blockwise Empirical Likelihood for the Difference of Means** by Reinis Alksnis and Janis Valeinis.

The paper studies two independent weakly dependent time series. The main calculations separate three finite-sample effects. The first is the difference between the variance scale of a finite block and that of the full sample. The second is the usual finite-block-count empirical-likelihood error. The third is the remaining dependence between neighbouring blocks. The adjusted statistic also avoids profile failure when the two ordinary empirical-likelihood ranges do not overlap.

The Bartlett factor used in the paper is the established profiled two-sample factor of Liu, Zou and Zhang evaluated with the finite-block moments. The half-Bartlett adjustment follows Liu and Yu. The new calculations concern the dependent block problem, variance calibration, the remaining adjacent-block term, unequal block lengths, and the fixed-block-count reference law.

## Contents

- `code/` contains the scripts used for the simulations, numerical checks and figures.
- `results/primary/` contains the main coverage and robustness summaries.
- `results/unequal_blocks/` contains the focused block-ratio experiment.
- `results/small_q/` contains the small-block-count bridge results.
- `results/fixed_q/` contains the fixed-block-count reference calculations.
- `results/application/` contains the Brent crude-oil application summaries.
- `results/validation/` contains compact checks of the theoretical and numerical calculations.
- `figures/` contains the two figures used in the manuscript in PDF and PNG format.

The repository keeps compact tables rather than every intermediate Monte Carlo object. Large RDS folders are not needed to read or check the paper and can be regenerated from the simulation scripts when required.

## Main numerical results

`results/primary/coverage_summary.csv` reproduces the aggregate coverage values in Table 1. The full unequal-block experiment is in `results/unequal_blocks/block_ratio_full_results.csv`. The small-block-count bridge table is in `results/small_q/bridge_table.csv`. The Brent application values are in `results/application/brent_table.csv`.

Figure 1 is stored as `figures/figure1_coverage.pdf` and Figure 2 as `figures/figure2_brent_profile.pdf`.

## Code

The computationally intensive simulation code is written in C++ and called from R through Rcpp. The public R scripts are deliberately split by task so that a reader does not need to run the full Monte Carlo study just to inspect one calculation.

The main simulation files are `code/simulation_driver.R` and `code/bel_mc.cpp`. The smaller scripts cover the unequal-block experiment, the fixed-block-count reference law, the adjacent-link coefficient check, selected numerical checks, and Figure 1. Figure 2 is supplied as the saved application profile used in the manuscript.

Production Monte Carlo cells use 10,000 replications. The fixed-block-count Gaussian reference calculations use larger reference samples where indicated in the corresponding scripts.

## Software

The analysis uses R, Rcpp and ggplot2. OpenMP is used by the C++ simulation engine when it is available. The Brent application is based on FRED series `DCOILBRENTEU`.

## Reproducibility

The random seeds are fixed in the simulation scripts. A short guide to the main calculations is in `REPRODUCIBILITY.md`, and `MANIFEST.md` maps the paper sections to the corresponding files.
