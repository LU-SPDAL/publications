source("R/plotting.R")

save_manuscript_figure(
  plot_correction_roles(correction_roles),
  "correction_roles_512_4096"
)

save_manuscript_figure(
  plot_dependence_sensitivity(dependence_sensitivity),
  "dependence_sensitivity_512_4096"
)
