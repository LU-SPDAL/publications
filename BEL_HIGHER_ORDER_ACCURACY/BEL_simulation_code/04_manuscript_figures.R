# Three figures used in the simulation section.
#
# Abbreviations:
# BEL       uncorrected blockwise empirical likelihood
# B-L       leading Bartlett correction
# B-F       full Bartlett correction evaluated with population quantities
# V-P       population variance calibration
# V-E       estimated variance calibration
#
# B-F+V-E uses the population full Bartlett factor together with
# estimated variance calibration. It is therefore not fully feasible.

library(dplyr)
library(ggplot2)
library(scales)
library(patchwork)

project_dir <- getwd()

find_table <- function(paths, description) {
  found <- paths[file.exists(paths)]

  if (length(found) == 0L) {
    stop(
      description,
      " was not found. See README.md for the expected folder layout."
    )
  }

  normalizePath(found[1L], winslash = "/", mustWork = TRUE)
}

# The organized CSV package is checked first. The last two paths allow this
# script to be run immediately after rerunning the simulation scripts.
master_file <- find_table(
  c(
    file.path(
      project_dir, "BEL_simulation_tables", "csv", "main_simulations",
      "master_results_unique_designs.csv"
    ),
    file.path(
      project_dir, "..", "BEL_simulation_tables", "csv", "main_simulations",
      "master_results_unique_designs.csv"
    ),
    file.path(
      project_dir, "results", "main_simulations",
      "master_results_unique_designs.csv"
    ),
    file.path(
      project_dir, "MASTER_BEL_point_and_CI_results_paper", "tables",
      "master_results_unique_designs.csv"
    )
  ),
  "The main simulation table"
)

phi_file <- find_table(
  c(
    file.path(
      project_dir, "BEL_simulation_tables", "csv", "dependence_grid",
      "phi_grid_results_labeled.csv"
    ),
    file.path(
      project_dir, "..", "BEL_simulation_tables", "csv", "dependence_grid",
      "phi_grid_results_labeled.csv"
    ),
    file.path(
      project_dir, "results", "dependence_grid",
      "phi_grid_results_labeled.csv"
    ),
    file.path(
      project_dir, "BEL_phi_grid_three_regimes_results_paper", "tables",
      "phi_grid_results_labeled.csv"
    )
  ),
  "The dependence-grid table"
)

figure_dir <- file.path(project_dir, "manuscript_figures")

dir.create(figure_dir, showWarnings = FALSE, recursive = TRUE)

master_results <- read.csv(
  master_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

phi_results <- read.csv(
  phi_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)


# Method labels

method_labels <- c(
  raw = "BEL",
  K_leading = "B-L",
  K_asym_full = "B-F",
  V_oracle = "V-P",
  V_feasible = "V-E",
  VK_leading_oracle = "B-L+V-P",
  F_feasible_leading = "B-L+V-E",
  VK_asym_full_oracle = "B-F+V-P",
  VK_asym_full_feasible = "B-F+V-E"
)

method_colors <- c(
  raw = "#000000",
  K_leading = "#E69F00",
  K_asym_full = "#D55E00",
  V_oracle = "#0072B2",
  V_feasible = "#56B4E9",
  VK_leading_oracle = "#009E73",
  F_feasible_leading = "#CC79A7",
  VK_asym_full_oracle = "#006B4F",
  VK_asym_full_feasible = "#8E5A9F"
)

method_linetypes <- c(
  raw = "solid",
  K_leading = "dashed",
  K_asym_full = "dotdash",
  V_oracle = "solid",
  V_feasible = "dashed",
  VK_leading_oracle = "solid",
  F_feasible_leading = "dashed",
  VK_asym_full_oracle = "longdash",
  VK_asym_full_feasible = "twodash"
)

method_shapes <- c(
  raw = 16,
  K_leading = 17,
  K_asym_full = 8,
  V_oracle = 15,
  V_feasible = 3,
  VK_leading_oracle = 18,
  F_feasible_leading = 1,
  VK_asym_full_oracle = 7,
  VK_asym_full_feasible = 4
)

main_methods <- c(
  "raw",
  "K_leading",
  "K_asym_full",
  "V_oracle",
  "V_feasible",
  "VK_leading_oracle",
  "F_feasible_leading"
)

individual_methods <- c(
  "raw",
  "K_leading",
  "K_asym_full",
  "V_oracle",
  "V_feasible"
)

combined_methods <- c(
  "VK_leading_oracle",
  "F_feasible_leading",
  "VK_asym_full_oracle",
  "VK_asym_full_feasible"
)

regime_levels <- c(
  "alpha_1_3",
  "alpha_1_2",
  "alpha_2_3"
)

regime_labels <- c(
  alpha_1_3 = "M == N^{1/3}",
  alpha_1_2 = "M == N^{1/2}",
  alpha_2_3 = "M == N^{2/3}"
)


check_columns <- function(data, required, object_name) {
  missing <- setdiff(required, names(data))

  if (length(missing) > 0) {
    stop(
      object_name,
      " is missing: ",
      paste(missing, collapse = ", ")
    )
  }
}

check_columns(
  master_results,
  c(
    "innovation", "phi", "N", "M", "Q", "statistic",
    "coverage", "coverage_mcse"
  ),
  "master_results"
)

check_columns(
  phi_results,
  c(
    "regime", "N", "M", "Q", "phi", "statistic",
    "coverage", "coverage_mcse"
  ),
  "phi_results"
)


theme_paper <- function(base_size = 14) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(
        colour = "grey88",
        linewidth = 0.35
      ),
      axis.title = element_text(
        size = base_size + 1,
        colour = "black"
      ),
      axis.text = element_text(
        size = base_size - 2,
        colour = "black"
      ),
      strip.text = element_text(
        size = base_size,
        face = "bold",
        colour = "black"
      ),
      strip.background = element_rect(
        fill = "grey95",
        colour = "grey55",
        linewidth = 0.4
      ),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 2),
      legend.key.width = grid::unit(1.25, "cm"),
      legend.key.height = grid::unit(0.5, "cm"),
      plot.title = element_text(
        size = base_size,
        face = "bold",
        hjust = 0.5
      ),
      plot.margin = margin(8, 10, 8, 8)
    )
}


method_scales <- function(methods) {
  list(
    scale_colour_manual(
      values = method_colors[methods],
      breaks = methods,
      labels = method_labels[methods],
      drop = FALSE
    ),
    scale_linetype_manual(
      values = method_linetypes[methods],
      breaks = methods,
      labels = method_labels[methods],
      drop = FALSE
    ),
    scale_shape_manual(
      values = method_shapes[methods],
      breaks = methods,
      labels = method_labels[methods],
      drop = FALSE
    )
  )
}


save_figure <- function(plot, file_name, width, height) {
  pdf_device <- if (capabilities("cairo")) {
    grDevices::cairo_pdf
  } else {
    grDevices::pdf
  }

  ggsave(
    filename = file.path(figure_dir, paste0(file_name, ".pdf")),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    device = pdf_device,
    bg = "white"
  )

  ggsave(
    filename = file.path(figure_dir, paste0(file_name, ".png")),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 600,
    bg = "white"
  )
}


# Figure 1: coverage under the three principal block-length regimes

block_data <- master_results |>
  dplyr::mutate(
    innovation = as.character(innovation),
    statistic = as.character(statistic),
    phi = as.numeric(phi),
    N = as.numeric(N),
    M = as.numeric(M),
    Q = as.numeric(Q),
    coverage = as.numeric(coverage),
    regime = dplyr::case_when(
      N == M^3 & Q == M^2 ~ "alpha_1_3",
      N == M^2 & Q == M ~ "alpha_1_2",
      N == Q^3 & M == Q^2 ~ "alpha_2_3",
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::filter(
    innovation == "gaussian",
    abs(phi - 0.5) < 1e-10,
    !is.na(regime),
    N <= 8100,
    statistic %in% main_methods
  ) |>
  dplyr::distinct(
    regime, N, M, Q, statistic,
    .keep_all = TRUE
  ) |>
  dplyr::mutate(
    regime = factor(regime, levels = regime_levels),
    statistic = factor(statistic, levels = main_methods)
  ) |>
  dplyr::arrange(regime, statistic, N)

if (nrow(block_data) == 0) {
  stop("No observations were found for Figure 1.")
}

figure_1 <- ggplot(
  block_data,
  aes(
    x = N,
    y = coverage,
    colour = statistic,
    linetype = statistic,
    shape = statistic,
    group = statistic
  )
) +
  geom_hline(
    yintercept = 0.95,
    colour = "grey35",
    linewidth = 0.7,
    linetype = "dotted"
  ) +
  geom_line(linewidth = 1.05, na.rm = TRUE) +
  geom_point(size = 2.7, stroke = 0.9, na.rm = TRUE) +
  facet_wrap(
    ~ regime,
    nrow = 1,
    labeller = as_labeller(
      regime_labels,
      default = label_parsed
    )
  ) +
  scale_x_log10(
    limits = c(60, 8500),
    breaks = c(64, 256, 1024, 4096),
    labels = label_comma(),
    minor_breaks = NULL,
    expand = expansion(mult = c(0.03, 0.04))
  ) +
  scale_y_continuous(
    breaks = seq(0.75, 0.95, by = 0.05),
    labels = label_number(accuracy = 0.01)
  ) +
  coord_cartesian(
    ylim = c(0.74, 0.956),
    clip = "off"
  ) +
  labs(
    x = "Sample size, N",
    y = "Empirical coverage"
  ) +
  method_scales(main_methods) +
  guides(
    colour = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE),
    shape = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  theme_paper(14) +
  theme(
    panel.spacing.x = grid::unit(0.9, "lines")
  )

save_figure(
  figure_1,
  "Figure_1_block_regimes_coverage",
  width = 11.6,
  height = 5.2
)


# Figure 2: coverage over the AR(1) dependence grid

phi_data <- phi_results |>
  dplyr::mutate(
    regime = as.character(regime),
    statistic = as.character(statistic),
    phi = as.numeric(phi),
    N = as.numeric(N),
    M = as.numeric(M),
    Q = as.numeric(Q),
    coverage = as.numeric(coverage)
  ) |>
  dplyr::filter(
    regime %in% regime_levels,
    N %in% c(729, 4096),
    statistic %in% main_methods
  ) |>
  dplyr::distinct(
    regime, N, M, Q, phi, statistic,
    .keep_all = TRUE
  ) |>
  dplyr::mutate(
    regime = factor(regime, levels = regime_levels),
    statistic = factor(statistic, levels = main_methods),
    N_panel = factor(
      N,
      levels = c(729, 4096),
      labels = c("N = 729", "N = 4096")
    )
  ) |>
  dplyr::arrange(regime, N, statistic, phi)

if (nrow(phi_data) == 0) {
  stop("No observations were found for Figure 2.")
}

phi_points <- phi_data |>
  dplyr::filter(
    abs(phi * 10 - round(phi * 10)) < 1e-8 |
      abs(phi - 0.95) < 1e-8
  )

figure_2 <- ggplot(
  phi_data,
  aes(
    x = phi,
    y = coverage,
    colour = statistic,
    linetype = statistic,
    shape = statistic,
    group = statistic
  )
) +
  geom_hline(
    yintercept = 0.95,
    colour = "grey35",
    linewidth = 0.7,
    linetype = "dotted"
  ) +
  geom_line(linewidth = 1.05, na.rm = TRUE) +
  geom_point(
    data = phi_points,
    size = 2.35,
    stroke = 0.85,
    na.rm = TRUE
  ) +
  facet_grid(
    rows = vars(regime),
    cols = vars(N_panel),
    scales = "free_y",
    labeller = labeller(
      regime = as_labeller(
        regime_labels,
        default = label_parsed
      )
    )
  ) +
  scale_x_continuous(
    limits = c(0, 0.95),
    breaks = seq(0, 0.8, by = 0.2),
    minor_breaks = seq(0.1, 0.9, by = 0.2),
    expand = expansion(mult = c(0.01, 0.025))
  ) +
  scale_y_continuous(
    n.breaks = 5,
    labels = label_number(accuracy = 0.01),
    expand = expansion(mult = c(0.06, 0.07))
  ) +
  labs(
    x = expression("AR(1) coefficient, " * phi),
    y = "Empirical coverage"
  ) +
  method_scales(main_methods) +
  guides(
    colour = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE),
    shape = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  theme_paper(14) +
  theme(
    panel.spacing.x = grid::unit(0.65, "lines"),
    panel.spacing.y = grid::unit(0.85, "lines")
  )

save_figure(
  figure_2,
  "Figure_2_dependence_grid_coverage",
  width = 11.6,
  height = 8.8
)


# Figure 3: fixed N, varying block length

fixed_data <- master_results |>
  dplyr::mutate(
    innovation = as.character(innovation),
    statistic = as.character(statistic),
    phi = as.numeric(phi),
    N = as.numeric(N),
    M = as.numeric(M),
    Q = as.numeric(Q),
    coverage = as.numeric(coverage)
  ) |>
  dplyr::filter(
    innovation == "gaussian",
    abs(phi - 0.5) < 1e-10,
    N == 4096,
    M %in% c(8, 16, 32, 64, 128, 256)
  ) |>
  dplyr::distinct(
    N, M, Q, statistic,
    .keep_all = TRUE
  ) |>
  dplyr::arrange(statistic, M)

if (nrow(fixed_data) == 0) {
  stop("No observations were found for Figure 3.")
}

block_breaks <- c(8, 16, 32, 64, 128, 256)

block_labels <- c(
  "8\n(512)",
  "16\n(256)",
  "32\n(128)",
  "64\n(64)",
  "128\n(32)",
  "256\n(16)"
)


# Panel (a): individual corrections

fixed_individual <- fixed_data |>
  dplyr::filter(statistic %in% individual_methods) |>
  dplyr::mutate(
    statistic = factor(
      statistic,
      levels = individual_methods
    )
  )

figure_3a <- ggplot(
  fixed_individual,
  aes(
    x = M,
    y = coverage,
    colour = statistic,
    linetype = statistic,
    shape = statistic,
    group = statistic
  )
) +
  geom_hline(
    yintercept = 0.95,
    colour = "grey35",
    linewidth = 0.7,
    linetype = "dotted"
  ) +
  geom_vline(
    xintercept = 64,
    colour = "grey55",
    linewidth = 0.65,
    linetype = "longdash"
  ) +
  geom_line(linewidth = 1.05, na.rm = TRUE) +
  geom_point(size = 2.8, stroke = 0.9, na.rm = TRUE) +
  scale_x_continuous(
    trans = "log2",
    breaks = block_breaks,
    labels = block_labels,
    minor_breaks = NULL,
    expand = expansion(mult = c(0.04, 0.05))
  ) +
  scale_y_continuous(
    breaks = seq(0.925, 0.950, by = 0.005),
    labels = label_number(accuracy = 0.001)
  ) +
  coord_cartesian(
    ylim = c(0.922, 0.952),
    clip = "off"
  ) +
  labs(
    title = "Individual corrections",
    x = "Block length, M  (Q in parentheses)",
    y = "Empirical coverage"
  ) +
  method_scales(individual_methods) +
  guides(
    colour = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE),
    shape = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  theme_paper(13) +
  theme(
    axis.text.x = element_text(lineheight = 0.9),
    legend.text = element_text(size = 10)
  )


# Panel (b): combined corrections

fixed_combined <- fixed_data |>
  dplyr::filter(statistic %in% combined_methods) |>
  dplyr::mutate(
    statistic = factor(
      statistic,
      levels = combined_methods
    )
  )

figure_3b <- ggplot(
  fixed_combined,
  aes(
    x = M,
    y = coverage,
    colour = statistic,
    linetype = statistic,
    shape = statistic,
    group = statistic
  )
) +
  geom_hline(
    yintercept = 0.95,
    colour = "grey35",
    linewidth = 0.7,
    linetype = "dotted"
  ) +
  geom_vline(
    xintercept = 64,
    colour = "grey55",
    linewidth = 0.65,
    linetype = "longdash"
  ) +
  geom_line(linewidth = 1.05, na.rm = TRUE) +
  geom_point(size = 2.8, stroke = 0.9, na.rm = TRUE) +
  scale_x_continuous(
    trans = "log2",
    breaks = block_breaks,
    labels = block_labels,
    minor_breaks = NULL,
    expand = expansion(mult = c(0.04, 0.05))
  ) +
  scale_y_continuous(
    breaks = seq(0.925, 0.950, by = 0.005),
    labels = label_number(accuracy = 0.001)
  ) +
  coord_cartesian(
    ylim = c(0.922, 0.952),
    clip = "off"
  ) +
  labs(
    title = "Combined corrections",
    x = "Block length, M  (Q in parentheses)",
    y = "Empirical coverage"
  ) +
  method_scales(combined_methods) +
  guides(
    colour = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE),
    shape = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  theme_paper(13) +
  theme(
    axis.text.x = element_text(lineheight = 0.9),
    legend.text = element_text(size = 10)
  )

figure_3 <- figure_3a + figure_3b +
  plot_layout(widths = c(1, 1)) +
  plot_annotation(
    tag_levels = list(c("(a)", "(b)")),
    theme = theme(
      plot.tag = element_text(
        size = 14,
        face = "bold"
      ),
      plot.tag.position = c(0.01, 0.99)
    )
  )

save_figure(
  figure_3,
  "Figure_3_fixed_N_block_length_coverage",
  width = 12.4,
  height = 6.4
)
