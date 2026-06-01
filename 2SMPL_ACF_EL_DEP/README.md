## Description of R code files

- `bel_functions.R`  
  Contains the blockwise empirical likelihood (BEL) implementation used for testing the difference in lag-1 autocorrelations between two independent time series. The function internally centers both series, constructs non-overlapping block moment averages, solves the empirical likelihood Lagrange multiplier equations, and profiles over the nuisance autocorrelation parameter.

- `wald_ar1_rho1_functions.R`  
  Contains the AR(1)-based Wald benchmark test. The function fits separate centered AR(1) models to the two series and uses the standard large-sample variance approximation for the AR(1) coefficient estimator to test the difference in lag-1 autocorrelations.

- `wald_hac_rho1_functions.R`  
  Contains the HAC-Wald benchmark test. The function estimates the difference in lag-1 autocorrelations and uses a scalar Bartlett/Newey--West HAC estimator to account for serial dependence in the estimating equations.

- `empirical_size_simulations.R`  
  Runs the empirical size simulations under the null hypothesis \(\Delta = 0\). The script generates two independent series from the same data-generating process and calculates empirical rejection probabilities and 95% coverage probabilities for FDEL, BEL, AR(1)-Wald, and HAC-Wald methods.

- `power_analysis_simulations.R`  
  Runs the power simulations under alternatives where the lag-1 autocorrelations differ between the two series. The script includes fixed AR(1) alternatives, local Pitman-type alternatives, unequal-sample settings, and mixed-memory settings.


- `nikkei_application_full.R`  
  Reproduces the empirical application to Nikkei 225 squared daily log returns. The script downloads the data from Yahoo Finance using `quantmod`, constructs daily log returns and squared log returns, performs the main pre-COVID versus COVID comparison, carries out the alternative-cutoff robustness check, computes descriptive statistics, and generates the rolling-window diagnostic figures.

- `data_README.md`  
  Provides a short description of the data source used in the empirical application. The Nikkei 225 data are not stored directly in the repository; they are downloaded from Yahoo Finance through the R package `quantmod`.
