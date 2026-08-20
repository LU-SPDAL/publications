# Provenance notes

This repository was assembled from the final manuscript sources and the collection of development archives used during the project. The public-facing code keeps the current formulas and designs while removing abandoned empirical candidates and superseded implementations.

## Authoritative development sources

The rectangular boundary geometry comes from `BEL_XQ_GEOMETRY_v1.0.1`. Version 1.0.1 is the same mathematical design as 1.0.0 with a Windows-safe parallelization repair.

The stable-interior and misspecification simulation engine comes from `AR1BEL_SIM_v3_repaired`. This is the repaired version of the simulation package and contains the finite-M coefficient used in the current manuscript.

The current VIX outputs come from `run_vix_empirical_v2.R` and `run_vix_three_reference_regimes.R`. The clean scripts in this repository retain the formulas, sample period, block paths, and publication replication counts while removing development messages, repeated checks, and automatic package installation.

The frozen FRED file `data/raw/VIXCLS_FRED.csv` is included so the VIX analysis does not depend on a live internet connection.

## Monte Carlo seeds

The main rectangular geometry uses base seed `2608160`.

The stable-interior study uses base seed `2026080901`.

The misspecification study uses base seed `2026080903`.

The fixed-Q Gaussian boundary study uses base seed `2026081202`.

The main VIX Gaussian path uses base seed `812026`.

The three-reference VIX study uses bases `17082710`, `17082720`, and `17082730` for the interior reference, boundary reference, and exact finite-sample checks.

## Small follow-up checks reconstructed from the final design

Three small follow-up analyses were added late in manuscript development. Their final numerical tables were retained in the manuscript and are archived in `results/reported_tables`, but the development folders supplied for repository assembly did not contain their original runner scripts or random-number seeds.

These are the non-Gaussian finite-M boundary check in Supplementary Table S2, the same-sample estimated-x VIX check in Supplementary Table S5, and the 200,000-replication VIX benchmark calibration.

Clean reconstruction scripts are included with fixed repository seeds. They reproduce the stated designs and should produce values within ordinary Monte Carlo variation of the archived tables. The archived table values remain the numbers used in the submitted manuscript.

## ADF specification

An older VIX result file used the default lag choice from `tseries::adf.test`. The current manuscript instead reports the intercept-only lag-0 Dickey-Fuller regression. The clean VIX script uses `urca::ur.df` with `type = "drift"` and zero lag, giving a statistic of approximately `-5.11`. The machine-readable diagnostics in this repository have been aligned with the current manuscript specification.

## Excluded development branches

The public package does not include earlier CPI, equity-VIX, VXAPL, power-selection, or residual-bootstrap candidate studies. Those branches were used during example selection and robustness development but do not generate results in the submitted paper.

The exploratory long-memory diagnostics performed immediately before submission are also excluded because they are not part of the manuscript's empirical analysis.
