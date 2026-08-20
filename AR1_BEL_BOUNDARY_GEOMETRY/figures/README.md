# Figures

The manuscript figures are generated from the saved result tables by

```r
source("scripts/10_make_figures.R")
```

The script writes the following PDF files to this directory:

- `Figure1_correction_geometry_boundary_final.pdf`
- `Figure2_positive_boundary_BEL_combined.pdf`
- `Figure3_VIX_path_revised.pdf`
- `Figure_XQ_geometry_positive_all_methods.pdf`
- `Figure_XQ_hull_failure_positive.pdf`
- `Figure_XQ_geometry_negative_evenM_VC_combined.pdf`
- `Figure_XQ_geometry_negative_oddM_VC_combined.pdf`

The underlying Monte Carlo values are stored in `results/geometry/` and the fitted VIX quantities are stored in `results/vix_reference_laws/`.
