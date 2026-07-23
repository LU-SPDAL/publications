# Simulation Tables for the BEL Higher-Order Accuracy Study

This directory contains the tabulated Monte Carlo results used in the study **“Coverage Error and Bartlett Correction for Nonoverlapping Blockwise Empirical Likelihood under Weak Dependence.”** The files have been consolidated from the three original table archives into a single, documented structure.

The CSV files are the canonical machine-readable results. The Excel workbooks contain the same values in a more convenient presentation format, with filters, frozen headers, consistent number formats, readable column widths, and a contents worksheet.

## Directory structure

```text
BEL_simulation_tables/
├── README.md
├── FILE_MANIFEST.csv
├── excel/
│   ├── 01_main_simulations.xlsx
│   ├── 02_dependence_grid.xlsx
│   └── 03_block_constant.xlsx
└── csv/
    ├── main_simulations/
    ├── dependence_grid/
    └── block_constant/
```

The Excel files are intended for reading and manual inspection. For R scripts, automated analysis, or long-term reproducibility, use the CSV files.

## Simulation settings

- Master seed: `2026051501`.
- Pointwise Monte Carlo replications: `B_point = 100000` in the master studies.
- Confidence-interval replications: `B_CI = 20000` in the master studies.
- Nominal coverage probability: 0.95.
- Innovation distributions in the main study: Gaussian, standardized Laplace, centered standardized Gamma(4), contaminated normal, centered standardized Gamma(1/2), and standardized Student `t(5)`.
- Dependence model: AR(1), with the values of `phi` recorded in each table.
- The dependence-grid study uses Gaussian innovations and `phi = 0, 0.05, ..., 0.95` under three block-length regimes.

Individual scenario seeds are reported in each `scenario_seeds.csv` file. A design appearing in more than one main-study module was simulated once and linked by `design_id`.

## Excel workbooks

### `01_main_simulations.xlsx`

Contains the master results and the six main simulation modules:

- block-length power regimes;
- the fixed-`N` sweep;
- fixed-`N` distribution and dependence comparisons;
- regular innovation distributions;
- extreme or robustness innovation distributions;
- sensitivity to the constant in `M = C sqrt(N)`.

It also contains design metadata, population constants, feasible-calibration diagnostics, paired method comparisons, and scenario seeds. Use the **Master results** worksheet when one row per unique design and statistic is required. The **Results by module** worksheet retains repeated module memberships and plotting labels.

### `02_dependence_grid.xlsx`

Contains the Gaussian AR(1) grid over `phi`, the three block-length regimes, population and feasible calibration diagnostics, paired method comparisons, interval-length results, and the dependence-neutralization summaries.

Use **Phi results all** as the canonical complete result table. **Phi results core** is the core-method subset, while **Phi results labeled** and **Results by regime** retain plotting and regime labels.

### `03_block_constant.xlsx`

Contains the population block-constant grid, the aggregated plug-in selector summaries, coverage comparisons, and the compact summary table.

## CSV directories

### `csv/main_simulations/`

| File | Contents |
|---|---|
| `master_results_unique_designs.csv` | Canonical master table with one row per distinct design and statistic. |
| `master_results_by_module.csv` | Master results joined to module, panel, series, and plotting metadata. A design can occur more than once if it belongs to several modules. |
| `results_block_power.csv` | Five block-length power regimes from `M` proportional to `N^(1/3)` through `N^(2/3)`. |
| `results_fixed_N_sweep.csv` | Detailed block-length sweep at fixed sample size. |
| `results_fixed_N_distribution_dependence.csv` | Fixed-`N` comparison across innovation distributions and dependence levels. |
| `results_regular_distribution_dependence.csv` | Gaussian, Laplace, and centered Gamma(4) results. |
| `results_extreme_distribution_dependence.csv` | Contaminated-normal, centered Gamma(1/2), and standardized `t(5)` robustness results. |
| `results_constant_sensitivity.csv` | Sensitivity to multiplicative constants in `M = C sqrt(N)`. |
| `diagnostics_unique_designs.csv` | Unique-design diagnostics for the feasible variance calibration and recorded failures. |
| `diagnostics_by_module.csv` | The same diagnostics joined to module and plotting metadata. |
| `paired_comparisons_unique_designs.csv` | Paired coverage and confidence-interval length differences between methods. |
| `paired_comparisons_by_module.csv` | Paired comparisons joined to module and plotting metadata. |
| `design_membership.csv` | Scenario-to-module membership and plotting metadata. |
| `population_design_constants.csv` | Long-run variance quantities, population calibration factors, Bartlett factors, and first-order benchmarks. |
| `scenario_seeds.csv` | Scenario-specific seeds. |

### `csv/dependence_grid/`

| File | Contents |
|---|---|
| `phi_grid_results_all_methods.csv` | Canonical complete results for all nine statistics. |
| `phi_grid_results_core_methods.csv` | Core-method subset of the complete grid. |
| `phi_grid_results_labeled.csv` | Complete results with regime and plotting labels. |
| `results_phi_grid_three_regimes.csv` | Results joined to the three block-length regimes and panel metadata. |
| `dependence_neutralization_core.csv` | Core summary of coverage and normalized length across `phi`. |
| `dependence_neutralization_summary.csv` | Expanded dependence-neutralization summary for the reported methods. |
| `phi_grid_diagnostics.csv` | Canonical feasible-calibration diagnostics. |
| `phi_grid_diagnostics_labeled.csv` | Diagnostics with regime and plotting labels. |
| `diagnostics_by_module.csv` | Diagnostics joined to module and panel metadata. |
| `paired_comparisons_unique_designs.csv` | Paired comparisons for unique dependence-grid designs. |
| `paired_comparisons_by_module.csv` | Paired comparisons joined to regime and plotting metadata. |
| `design_membership.csv` | Scenario, regime, and plotting metadata. |
| `population_design_constants.csv` | Population variance quantities and correction factors. |
| `scenario_seeds.csv` | Scenario-specific seeds. |

### `csv/block_constant/`

| File | Contents |
|---|---|
| `grid_results.csv` | Coverage over the relative constant grid and the `C = 1` reference. |
| `plugin_summary.csv` | Aggregated plug-in selector performance, including quantiles of the estimated constant and block length. |
| `coverage_comparison.csv` | Direct coverage comparison of the population, plug-in, and reference choices. |
| `summary_table.csv` | Compact comparison of the population constant, grid-best choice, and plug-in selector. |

## Main identifiers and columns

| Column | Meaning |
|---|---|
| `scenario_id` | Identifies an innovation distribution, dependence value, and sample size. |
| `design_id` | Identifies a complete design, including `N`, `M`, and `Q`. Use this key when joining result, diagnostic, and population-constant tables. |
| `N` | Sample size. |
| `M` | Nonoverlapping block length. |
| `Q` | Number of complete blocks used by BEL. |
| `L` | Lag-truncation bandwidth used for the feasible variance calibration. |
| `alpha_nominal` | Nominal block-power exponent in `M` proportional to `N^alpha`; it is not the test significance level. |
| `alpha_effective` | Exponent implied by the integer values of `N` and `M`. |
| `C_nominal`, `C_effective` | Nominal and integer-design constants in the block-length rule. |
| `B_point` | Number of pointwise Monte Carlo replications. |
| `B_CI` | Number of confidence-interval replications. |
| `coverage` | Empirical coverage probability; convex-hull failures are counted as noncoverage. |
| `coverage_error` | `coverage - 0.95`. |
| `coverage_mcse` | Monte Carlo standard error of empirical coverage. |
| `ci_subset_coverage` | Coverage computed on the confidence-interval replication subset. |
| `mean_length`, `median_length`, `q25_length`, ... | Confidence-interval length summaries among finite interval lengths. |
| `normalized_mean_length` | Mean length divided by the first-order length benchmark. |
| `relative_mean_length_to_raw` | Mean length relative to the raw BEL interval in the same design. |
| `convex_failure_rate` | Proportion of replications in which the empirical-likelihood convex-hull condition failed. |
| `interval_failure_rate` | Proportion of failed confidence-interval inversions. |
| `disconnected_rate` | Proportion of disconnected confidence sets. |
| `nonregular_rate` | Proportion of nonregular interval inversions. |
| `b_true` | Population value of `b = B2 / sigma^2`. |
| `mean_b_hat`, `bias_b_hat`, `rmse_b_hat` | Monte Carlo diagnostics for the estimated value of `b`. |
| `nu_true` | Population variance-calibration factor. |
| `mean_nu_hat`, `bias_nu_hat`, `rmse_nu_hat` | Monte Carlo diagnostics for the estimated calibration factor. |

## Statistic labels

The result tables preserve the original computational labels:

| Label | Interpretation |
|---|---|
| `raw` | Uncorrected BEL statistic. |
| `K_leading` | BEL with the leading Bartlett adjustment. |
| `K_asym_full` | BEL with the full asymptotic Bartlett factor used in the simulation code. |
| `V_oracle` | BEL with the population variance-calibration factor. |
| `V_feasible` | BEL with the estimated variance-calibration factor. |
| `VK_leading_oracle` | Population variance calibration combined with the leading Bartlett adjustment. |
| `VK_asym_full_oracle` | Population variance calibration combined with the full asymptotic Bartlett factor. |
| `VK_asym_full_feasible` | Estimated variance calibration combined with the full asymptotic Bartlett factor. |
| `F_feasible_leading` | Fully feasible leading-order combined correction: estimated variance calibration with the leading Bartlett adjustment. |

`oracle` is retained only as a legacy filename and code label. It means that population quantities were used. `feasible` means that the calibration quantity was estimated from the sample.

## Duplicate files removed during consolidation

Three files in the original dependence-grid archive were exact byte-for-byte duplicates. Only the more descriptive filename in each pair is retained:

| Removed duplicate | Retained canonical file |
|---|---|
| `master_results_unique_designs.csv` | `phi_grid_results_all_methods.csv` |
| `diagnostics_unique_designs.csv` | `phi_grid_diagnostics.csv` |
| `master_results_by_module.csv` | `results_phi_grid_three_regimes.csv` |

No nonidentical table was removed. Files with similar names in different directories belong to different simulation studies and must not be combined solely by filename.

## Important note on plug-in selector draws

The supplied block-constant archive contains aggregated selector summaries but no replication-level selector-draw file. In particular, `plugin_summary.csv` reports means, standard deviations, medians, quartiles, RMSE values, fallback rates, and selected block-length summaries. If the supplementary material states that “selector draws” are supplied, that wording should be changed unless a separate draw-level file is added.

## Data integrity and use

- The CSV values were preserved without transformation.
- The Excel workbooks are presentation copies of the CSV tables; no scientific result was recomputed or rounded in the stored cells.
- `FILE_MANIFEST.csv` reports the relative path, number of data rows, number of columns, source archive, and SHA-256 checksum of every retained CSV file.
- Blank or unavailable fields retain the representation used in the original output.

For analysis in R, read the relevant canonical CSV directly. For example:

```r
results <- read.csv(
  "csv/main_simulations/master_results_unique_designs.csv",
  check.names = FALSE
)
```

When the simulation scripts are added to the public repository, place them in a separate top-level `code/` directory and document the execution order, required R version, and package versions. A versioned repository release should be created for the exact manuscript submission.
