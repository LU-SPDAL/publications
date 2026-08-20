# Full computational study. Several steps are intentionally expensive.

source(file.path("scripts", "01_geometry.R"))
source(file.path("scripts", "02_finiteM_validation.R"))
source(file.path("scripts", "03_stable_interior.R"))
source(file.path("scripts", "04_misspecification.R"))
source(file.path("scripts", "05_boundary_validations.R"))
source(file.path("scripts", "06_vix_empirical.R"))
source(file.path("scripts", "07_vix_reference_laws.R"))
source(file.path("scripts", "08_vix_plugin_sensitivity.R"))
source(file.path("scripts", "09_vix_benchmark_calibration.R"))
options(ar1bel.use_recomputed = TRUE)
source(file.path("scripts", "10_make_figures.R"))
