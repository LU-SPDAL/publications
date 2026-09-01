# Reproduce Figure 1 from the primary 80-cell coverage design.
# Run this file from the code directory.

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required.")
}

source("simulation_driver.R")

compile_abel_engine(rebuild = TRUE)

design <- build_main_coverage_design()
design$base_id <- design$scenario_id
design$scenario_no <- seq_len(nrow(design))

out_dir <- file.path("..", "results", "primary", "figure1_raw")

run_design(
  design = design,
  out_dir = out_dir,
  B = 10000L,
  base_seed = 20260829,
  overwrite = FALSE
)

coverage <- summarize_coverage(
  out_dir = out_dir,
  levels = c(0.90, 0.95, 0.99)
)

method_names <- c(
  BEL = "BEL",
  BC_feasible = "BC",
  ABEL_feasible = "ABEL",
  VC_feasible = "VC",
  VCBC_feasible = "VC+BC",
  VCABEL_feasible = "VC+ABEL",
  Wald_HAC = "HAC-Wald"
)

values <- coverage[
  coverage$group == "coverage_main" &
    abs(as.numeric(coverage$nominal) - 0.95) < 1e-12 &
    coverage$method %in% names(method_names),
  , drop = FALSE
]

values$coverage95 <- as.numeric(values$coverage_fail_as_miss)
values$Method <- unname(method_names[values$method])
values$Method <- factor(
  values$Method,
  levels = c("BEL", "BC", "ABEL", "VC", "VC+BC", "VC+ABEL", "HAC-Wald")
)

write.csv(
  values,
  file.path("..", "results", "primary", "figure1_coverage_values.csv"),
  row.names = FALSE
)

library(ggplot2)

p <- ggplot(values, aes(Method, coverage95)) +
  geom_boxplot(
    width = 0.56,
    outlier.shape = 1,
    outlier.size = 1.7,
    outlier.stroke = 0.45,
    linewidth = 0.60
  ) +
  stat_summary(fun = mean, geom = "point", shape = 17, size = 2.6) +
  geom_hline(yintercept = 0.95, linetype = "dashed", linewidth = 0.60) +
  coord_cartesian(ylim = c(0.74, 0.96), clip = "off") +
  labs(x = NULL, y = "Empirical coverage") +
  theme_classic(base_size = 11, base_family = "sans") +
  theme(
    axis.title.y = element_text(size = 11, margin = margin(r = 7)),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10, angle = 25, hjust = 1, vjust = 1),
    axis.ticks = element_line(linewidth = 0.45),
    axis.line = element_line(linewidth = 0.55),
    plot.margin = margin(7, 9, 7, 7)
  )

ggsave(
  "../figures/figure1_coverage.pdf",
  p,
  width = 7,
  height = 4.3,
  units = "in",
  device = grDevices::cairo_pdf
)

ggsave(
  "../figures/figure1_coverage.png",
  p,
  width = 7,
  height = 4.3,
  units = "in",
  dpi = 600,
  bg = "white"
)
