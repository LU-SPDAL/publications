library(data.table)
library(openxlsx)
source("R/plotting.R")

roles <- as.data.table(read.xlsx(
  "results/article_results.xlsx",
  sheet = "Figure1_data"
))

dependence <- as.data.table(read.xlsx(
  "results/article_results.xlsx",
  sheet = "Figure2_data"
))

save_manuscript_figure(
  plot_correction_roles(roles),
  "correction_roles_512_4096"
)

save_manuscript_figure(
  plot_dependence_sensitivity(dependence),
  "dependence_sensitivity_512_4096"
)
