# AR(1) blockwise empirical likelihood reproducibility files

This repository accompanies the manuscript

**Block-Scale Persistence and Block Count in Blockwise Empirical Likelihood for a First-Order Autoregression**

by Reinis Alksnis and Janis Valeinis.

The repository contains the code and machine-readable results used for the simulation study, the VIX application, and the manuscript figures. It was assembled from the final development archives rather than from the earlier exploratory branches.

## Repository structure

```text
R/                         Core BEL, AR(1), boundary, and VIX functions
scripts/                   Reproduction scripts in manuscript order
src/                       C++ simulation engine for the larger Monte Carlo studies
data/raw/                  Frozen FRED VIXCLS data used in the paper
reference/                 Simulation designs and innovation definitions
results/geometry/          Final rectangular (x,Q) simulation workbook
results/vix/               Final VIX working-model calculations
results/vix_reference_laws/Finite-Q and local-boundary VIX reference laws
results/reported_tables/   Machine-readable copies of manuscript tables
results/recomputed/        Output created when the simulations are rerun
figures/                    Figure-generation target and instructions
manuscript/                 TeX sources used when this repository was assembled
docs/                       Provenance and manuscript-output mapping
```

## R packages

The main scripts use the following packages.

```r
install.packages(c(
  "Rcpp",
  "RcppParallel",
  "ggplot2",
  "patchwork",
  "scales",
  "urca",
  "tseries",
  "strucchange",
  "sandwich"
))
```

The final VIX run was made under R 4.4.1 on Windows 11. The saved session information is in `results/vix/sessionInfo.txt` and `results/vix_reference_laws/sessionInfo.txt`.

The scripts do not install packages automatically and the VIX analysis uses the frozen CSV in `data/raw/`, so a live internet connection is not required.

## Fast reproduction from saved Monte Carlo results

Run the scripts from the repository root. The quickest way to rebuild the manuscript figures is

```r
source("scripts/11_reproduce_from_saved_results.R")
```

This reads the archived final result tables and does not rerun the expensive simulations.

## Full simulation scripts

The scripts are ordered by their role in the manuscript.

```text
01_geometry.R                 rectangular positive and negative boundary maps
02_finiteM_validation.R       exact Gaussian finite-M versus limit comparison
03_stable_interior.R          correctly specified stable AR(1) study
04_misspecification.R         AR(2), MA(1), and ARMA working-model study
05_boundary_validations.R     fixed-Q and non-Gaussian boundary checks
06_vix_empirical.R            VIX fit, correction path, diagnostics, and intervals
07_vix_reference_laws.R       finite-Q and x-dependent VIX critical values
08_vix_plugin_sensitivity.R   same-sample sensitivity to estimating x
09_vix_benchmark_calibration.R AR-Wald and self-normalized benchmark calibration
10_make_figures.R             main and supplementary figures
```

Running

```r
source("scripts/run_full_study.R")
```

runs all of them in sequence and writes new Monte Carlo output under `results/recomputed/`. The archived manuscript values are left unchanged. This is computationally expensive. The boundary maps, 300,000-draw reference distributions, and 200,000-replication benchmark calculation are the longest parts.

The C++ simulations use `RcppParallel`. To use more than one thread, set an environment variable before running a script. For example

```r
Sys.setenv(AR1_BEL_CORES = "6")
```

The geometry script uses one R process by default. It can also use the same `AR1_BEL_CORES` setting through a Windows-safe PSOCK cluster.

## VIX data

`data/raw/VIXCLS_FRED.csv` is a frozen copy of the FRED series `VIXCLS`. The paper uses January 1990 through December 2024. Daily closes are averaged within calendar months and the monthly averages are then logged.

The frozen file is included so that the analysis can be rerun without a live FRED connection and without being affected by later revisions to the source file.

## Main correction functions

The central functions are in `R/bel_core.R`.

- `D_ar1()` computes the finite-sample AR(1) variance sum.
- `nu_ar1()` computes the exact variance calibration factor `D_M / D_N`.
- `aG_ar1()` computes the finite-M Gaussian Bartlett coefficient used in the stable interior.
- `aK_ar1()` adds the residual skewness and kurtosis contribution.
- `bel_lr()` evaluates the scalar empirical-likelihood ratio.
- `exact_ar1_block_covariance()` gives the exact Gaussian covariance matrix of the nonoverlapping block vector.

The local-boundary covariance formulas are in `R/boundary_limits.R`.

## Saved results and reported tables

`results/geometry/geometry_results_long.csv` is the final 20,000-replication rectangular boundary workbook used for the heatmaps.

`results/vix_reference_laws/` contains the 300,000-draw finite-Q reference critical values and their independent 100,000-draw exact finite-sample checks.

`results/README.md` explains which files are the archived manuscript values and which files are regenerated by the scripts.

`results/reported_tables/` contains compact machine-readable copies of the tables printed in the current manuscript. See `docs/manuscript_output_map.md` for the correspondence between manuscript items and code.

## Development provenance

The project went through many exploratory versions. Only the current branches were retained in this public-facing package. A source-by-source inventory is in `docs/source_inventory.csv`.

Three late supplementary checks did not have their original runner seed retained in the supplied development folders. Their reported numerical tables are archived exactly and clean reconstruction scripts with fixed seeds are provided. Details are in `docs/provenance_notes.md`.

## License

No software license has been added automatically. The authors should choose the intended license before making the GitHub repository public.
