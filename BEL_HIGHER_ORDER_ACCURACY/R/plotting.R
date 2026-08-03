library(data.table)
library(ggplot2)

method_labels <- c(
  BEL = "BEL",
  B = "Bartlett only",
  V_population = "Population variance only",
  V_hybrid = "Hybrid variance only",
  VB_population = "Population combined",
  VB_hybrid = "Hybrid combined"
)

legend_order <- unname(method_labels)

method_colours <- c(
  "BEL" = "#333333",
  "Bartlett only" = "#E69F00",
  "Population variance only" = "#56B4E9",
  "Hybrid variance only" = "#009E73",
  "Population combined" = "#0072B2",
  "Hybrid combined" = "#CC79A7"
)

method_shapes <- c(
  "BEL" = 16,
  "Bartlett only" = 17,
  "Population variance only" = 15,
  "Hybrid variance only" = 18,
  "Population combined" = 8,
  "Hybrid combined" = 4
)

method_linetypes <- c(
  "BEL" = "dotted",
  "Bartlett only" = "solid",
  "Population variance only" = "longdash",
  "Hybrid variance only" = "dashed",
  "Population combined" = "dotdash",
  "Hybrid combined" = "twodash"
)

panel_labels <- c(
  "512" = "(a)",
  "4096" = "(b)"
)

figure_theme <- theme_bw(base_size = 11, base_family = "sans") +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = margin(t = 2),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.margin = margin(6, 7, 6, 7)
  )

figure_guides <- guides(
  colour = guide_legend(nrow = 2, byrow = TRUE),
  linetype = guide_legend(nrow = 2, byrow = TRUE),
  shape = guide_legend(nrow = 2, byrow = TRUE)
)

prepare_figure_data <- function(x) {
  x[, method_label := factor(
    unname(method_labels[method]),
    levels = legend_order
  )]
  x
}

plot_correction_roles <- function(roles) {
  roles <- prepare_figure_data(copy(roles))

  ggplot(
    roles,
    aes(
      x = M,
      y = coverage,
      colour = method_label,
      linetype = method_label,
      shape = method_label,
      group = method_label
    )
  ) +
    geom_hline(
      yintercept = 0.95,
      colour = "grey35",
      linetype = "dotted",
      linewidth = 0.4
    ) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 2.1) +
    scale_colour_manual(values = method_colours, breaks = legend_order, drop = FALSE) +
    scale_linetype_manual(values = method_linetypes, breaks = legend_order, drop = FALSE) +
    scale_shape_manual(values = method_shapes, breaks = legend_order, drop = FALSE) +
    scale_x_log10(breaks = c(4, 8, 16, 32, 64, 128, 256)) +
    facet_wrap(
      ~N,
      nrow = 1,
      scales = "free_x",
      labeller = as_labeller(panel_labels)
    ) +
    labs(
      x = "Block length",
      y = "Coverage",
      colour = NULL,
      linetype = NULL,
      shape = NULL
    ) +
    figure_guides +
    figure_theme
}

plot_dependence_sensitivity <- function(dependence) {
  dependence <- prepare_figure_data(copy(dependence))

  ggplot(
    dependence,
    aes(
      x = phi,
      y = coverage,
      colour = method_label,
      linetype = method_label,
      shape = method_label,
      group = method_label
    )
  ) +
    geom_hline(
      yintercept = 0.95,
      colour = "grey35",
      linetype = "dotted",
      linewidth = 0.4
    ) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 2.1) +
    scale_colour_manual(values = method_colours, breaks = legend_order, drop = FALSE) +
    scale_linetype_manual(values = method_linetypes, breaks = legend_order, drop = FALSE) +
    scale_shape_manual(values = method_shapes, breaks = legend_order, drop = FALSE) +
    scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 0.9)) +
    facet_wrap(
      ~N,
      nrow = 1,
      labeller = as_labeller(panel_labels)
    ) +
    labs(
      x = "φ",
      y = "Coverage",
      colour = NULL,
      linetype = NULL,
      shape = NULL
    ) +
    figure_guides +
    figure_theme
}

save_manuscript_figure <- function(plot, file_name) {
  ggsave(
    file.path("figures", paste0(file_name, ".pdf")),
    plot,
    device = grDevices::cairo_pdf,
    family = "sans",
    width = 7.8,
    height = 4.8,
    units = "in"
  )
}
