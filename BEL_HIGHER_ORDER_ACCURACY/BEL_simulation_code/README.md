# BEL simulation code

The code is split by simulation task. Each simulation script is self-contained,
so it can be run without sourcing a separate file of helper functions.

## Files

### `01_main_simulations.R`

Runs the main Monte Carlo study:

- five Gaussian block-power regimes;
- constant sensitivity for \(M=C\sqrt{N}\);
- fixed-\(N\) block-length sweeps;
- regular and extreme innovation distributions;
- fixed-\(N\) distribution and dependence comparisons;
- pointwise coverage and confidence-interval length for all nine statistics.

Results are written to
`MASTER_BEL_point_and_CI_results_paper/` when `MODE <- "paper"`.

### `02_dependence_grid.R`

Runs the Gaussian AR(1) dependence study for
\(\phi=0,0.05,\ldots,0.95\). For each generated series, the three block
regimes \(M=N^{1/3}\), \(M=N^{1/2}\), and \(M=N^{2/3}\) are evaluated on the
same data.

Results are written to `BEL_phi_grid_three_regimes_results_paper/` when
`MODE <- "paper"`. The script uses the same descriptive filenames as the
organized dependence-grid tables. It does not create the former duplicate
`master_results_*` or `diagnostics_unique_designs.csv` aliases.

### `03_block_constant_and_selector.R`

Studies the theoretical block constant, a grid around that constant, the
plug-in selector, and the fixed rule \(C=1\). It also saves the selector draws
as an RDS file and produces the related diagnostic figures.

Results are written to `bel_block_constant_results/`.

### `04_manuscript_figures.R`

Reads these canonical CSV files:

- `BEL_simulation_tables/csv/main_simulations/master_results_unique_designs.csv`
- `BEL_simulation_tables/csv/dependence_grid/phi_grid_results_labeled.csv`

It creates the three final manuscript figures in both PDF and PNG format. The
output directory is `manuscript_figures/`.

The script also recognizes the result folders created by
`01_main_simulations.R` and `02_dependence_grid.R`, so figures can be rebuilt
directly after a new simulation run.

## Folder layout

Unzip the code and table packages into the same parent folder:

```text
BEL_project/
├── BEL_simulation_code/
│   ├── 01_main_simulations.R
│   ├── 02_dependence_grid.R
│   ├── 03_block_constant_and_selector.R
│   └── 04_manuscript_figures.R
└── BEL_simulation_tables/
    └── csv/
        ├── main_simulations/
        ├── dependence_grid/
        └── block_constant/
```

Then run the scripts from `BEL_simulation_code/`. The figure script finds the
organized tables in the sibling folder automatically.

## Reproducing the paper runs

Run the scripts from the repository root:

```r
source("01_main_simulations.R")
source("02_dependence_grid.R")
source("03_block_constant_and_selector.R")
source("04_manuscript_figures.R")
```

The first three scripts are independent. The fourth can use either the
organized tables or fresh output from the first two scripts.

The paper settings are already selected. The two large simulations also have a
`MODE <- "quick"` option for checking that the code works before starting the
full run. Quick mode is not used for the reported results.

## Seeds and numerical settings

The master seed is `2026051501` in all three simulation scripts.

| Script | Replications and main settings |
|---|---|
| Main simulation | 100,000 point replications; 20,000 interval replications; chunk size 1,000; nominal coverage 0.95 |
| Dependence grid | 100,000 point replications; 20,000 interval replications; chunk size 1,000; nominal coverage 0.95 |
| Block constant and selector | 100,000 replications; batch size 500; nominal coverage 0.95 |

The main and dependence-grid scripts use
`Mersenne-Twister`, `Inversion`, and `Rejection` as the R random-number
generators. Scenario and chunk seeds are deterministic and are saved with the
results. The block-constant script assigns seed
`2026051501 + design number` after sorting designs by \(\phi\) and \(N\).

The interval inversion settings are unchanged:

- 28 bisection iterations;
- 21 coarse-grid points;
- 201 dense-grid points;
- stationary-tail variance tolerance \(10^{-14}\).

Removing the progress messages and changing the folder layout do not alter the
order of random-number generation. Existing statistic identifiers, including
the older `oracle` labels, are retained so that the new scripts remain
compatible with the saved tables.

## R packages

- `01_main_simulations.R`: `Rcpp`, `ggplot2`
- `02_dependence_grid.R`: `Rcpp`, `ggplot2`
- `03_block_constant_and_selector.R`: `matrixStats`, `dplyr`, `ggplot2`
- `04_manuscript_figures.R`: `dplyr`, `ggplot2`, `scales`, `patchwork`

The scripts use the base R pipe `|>`, so R 4.1 or later is recommended. A C++
toolchain is needed because the two large simulations compile their numerical
engine with `Rcpp::sourceCpp()`.

## Saved chunks

The main and dependence-grid scripts save each Monte Carlo chunk as an RDS
file. With `REUSE_SAVED_CHUNKS <- TRUE`, an interrupted run can continue
without repeating completed chunks. The chunk filenames include the code
version, mode, scenario, replication counts, chunk size, and inversion
settings, which prevents results from different run configurations from being
mixed accidentally.
