# Computational files for the JTSA BEL manuscript

This folder accompanies the manuscript

**Variance calibration and higher-order accuracy for nonoverlapping blockwise empirical likelihood under weak dependence**

by Reinis Alksnis and Jānis Valeinis.

## Contents

```text
R/                 R functions used by the simulation scripts
src/               Rcpp/C++ simulation engine called from R
scripts/           scripts for the numerical studies and output files
results/           Excel workbooks containing the simulation results
figures/           final vector figures used in the manuscript
config.R           replication counts, tuning constants and random seeds
run_all.R          runs the complete numerical study
redraw_figures.R   redraws Figures 1 and 2 from the Excel workbook
```


## Result workbooks

### `results/article_results.xlsx`

This workbook contains the results used directly in the article.

- `Table1_reported` contains the coverage values reported in Table 1.
- `Table1_MCSE` contains the corresponding Monte Carlo standard errors.
- `Table1_raw` contains the long-format simulation output for Table 1.
- `Figure1_data` contains the values plotted in Figure 1.
- `Figure2_data` contains the values plotted in Figure 2.
- `Table2_reported` contains the coverage values reported in Table 2.
- `Table2_MCSE` contains the corresponding Monte Carlo standard errors.
- `Table2_raw` contains the long-format simulation output for Table 2.

### `results/additional_results.xlsx`

This workbook contains the broader numerical studies described in the manuscript and computational discussion.

- `Variance_ratio` contains the variance-calibration comparison.
- `Ratio_tuning` contains the tuning-parameter screen for the hybrid factor.
- `Block_constant` contains the block-constant grid study.
- `Block_selector` contains the data-based block-length selector study.
- `Innovation_robustness` contains the broader innovation-distribution study.

## Reproducing the figures

Figures 1 and 2 can be redrawn without rerunning the simulations:

```r
source("redraw_figures.R")
```

The resulting files are

```text
figures/correction_roles_512_4096.pdf
figures/dependence_sensitivity_512_4096.pdf
```

The figures are written with `cairo_pdf`, so the fonts are embedded in the PDF files.

## Rerunning the simulations

The complete numerical study can be rerun with

```r
source("run_all.R")
```

The production run uses tens of thousands of replications for each exact setting and may take a long time. For a short code test, change

```r
production_run <- TRUE
```

to

```r
production_run <- FALSE
```

in `config.R`.

A full run creates the two Excel workbooks in `results/` and redraws the two manuscript figures in `figures/`.

## Software

The production results were generated with R 4.4.1 on Windows 11. The main R packages were

- Rcpp 1.1.2
- data.table 1.16.4
- ggplot2 4.0.3
- openxlsx, used only to read and write the Excel workbooks

A C++17 compiler with OpenMP support is required for a full rerun. On Windows, this is supplied by Rtools.

## Main simulation settings

Figure 1 uses a Gaussian AR(1) process with `phi = 0.5`. The block-length grids are

- `N = 512`: `M = 4, 8, 16, 32, 64`, with 40,000 replications
- `N = 4096`: `M = 8, 16, 32, 64, 128, 256`, with 20,000 replications

Figure 2 uses Gaussian AR(1) processes with

```text
phi = 0, 0.2, 0.4, 0.6, 0.8, 0.9
```

and `M = round(sqrt(N))`.

- `N = 512`, `M = 23`, with 40,000 replications
- `N = 4096`, `M = 64`, with 20,000 replications

Table 1 uses Gaussian AR(1), ARMA(1,1) and MA(2) designs with `M = Q = sqrt(N)` at `N = 256, 1024, 4096`.

Table 2 uses seven innovation distributions for AR(1) processes with `N = 1024`, `M = Q = 32`, and `phi = 0.5, 0.8`. Each exact setting uses 40,000 replications.

All random-number seeds and tuning constants are recorded in `config.R`.
