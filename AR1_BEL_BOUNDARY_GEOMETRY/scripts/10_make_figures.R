# Figures used in the main paper and Supplementary Material

library(ggplot2)
library(patchwork)
library(scales)

source(file.path("R", "bel_core.R"))
source(file.path("R", "boundary_limits.R"))

dir.create("figures", showWarnings = FALSE)

use_recomputed <- isTRUE(getOption("ar1bel.use_recomputed", FALSE))

if (use_recomputed) {
  geometry_file <- file.path("results", "recomputed", "geometry", "geometry_results_long.csv")
  vix_fit_file <- file.path("results", "recomputed", "vix_reference_laws", "00_vix_fit_summary.csv")
} else {
  geometry_file <- file.path("results", "geometry", "geometry_results_long.csv")
  vix_fit_file <- file.path("results", "vix_reference_laws", "00_vix_fit_summary.csv")
}

geometry <- read.csv(geometry_file)
vix_fit <- read.csv(vix_fit_file)

paper_theme <- function(base_size = 11) {
  theme_classic(base_size = base_size, base_family = "serif") +
    theme(
      panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.35),
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.tag = element_text(face = "bold")
    )
}

save_pdf <- function(plot, filename, width, height) {
  ggsave(file.path("figures", filename), plot, width = width, height = height,
         device = cairo_pdf, bg = "white")
}

# Figure S1: exact finite-sample correction multipliers.
designs <- data.frame(
  panel = c("Even M", "Odd M, even Q", "Odd M, odd Q"),
  M = c(20, 21, 21),
  Q = c(20, 20, 21)
)

correction_curves <- function(phi_grid, M, Q, panel) {
  values <- t(vapply(phi_grid, function(phi) {
    G <- exact_ar1_block_covariance(M, Q, phi)
    V <- G[1, 1]
    beta <- gaussian_bartlett_coefficient(G)
    B <- 1 - beta / Q
    c(Variance = V, Bartlett = B, Total = V * B)
  }, numeric(3)))

  data.frame(
    phi = rep(phi_grid, 3),
    value = c(values[, 1], values[, 2], values[, 3]),
    curve = rep(c("Variance calibration", "Gaussian Bartlett refinement", "Total correction"),
                each = length(phi_grid)),
    panel = panel
  )
}

phi_main <- seq(-0.90, 0.90, length.out = 1000)
phi_negative <- seq(-0.9995, -0.90, length.out = 1200)
main_curves <- do.call(rbind, lapply(seq_len(nrow(designs)), function(i) {
  correction_curves(phi_main, designs$M[i], designs$Q[i], designs$panel[i])
}))
negative_curves <- do.call(rbind, lapply(seq_len(nrow(designs)), function(i) {
  correction_curves(phi_negative, designs$M[i], designs$Q[i], designs$panel[i])
}))

curve_colours <- c(
  "Variance calibration" = "#0072B2",
  "Gaussian Bartlett refinement" = "#D55E00",
  "Total correction" = "#009E73"
)
curve_types <- c(
  "Variance calibration" = "solid",
  "Gaussian Bartlett refinement" = "dotdash",
  "Total correction" = "dashed"
)

make_top <- function(panel, legend = FALSE) {
  d <- main_curves[main_curves$panel == panel, ]
  ggplot(d, aes(phi, value, colour = curve, linetype = curve)) +
    geom_hline(yintercept = 1, colour = "grey60", linetype = "dotted") +
    geom_line(linewidth = 1) +
    scale_colour_manual(values = curve_colours) +
    scale_linetype_manual(values = curve_types) +
    coord_cartesian(ylim = c(0.45, 1.60)) +
    labs(x = expression(phi), y = "Correction multiplier", title = panel) +
    paper_theme() +
    theme(legend.position = if (legend) "bottom" else "none")
}

make_bottom <- function(panel) {
  d <- negative_curves[negative_curves$panel == panel, ]
  p <- ggplot(d, aes(phi, value, colour = curve, linetype = curve)) +
    geom_hline(yintercept = 1, colour = "grey60", linetype = "dotted") +
    geom_line(linewidth = 1) +
    scale_colour_manual(values = curve_colours) +
    scale_linetype_manual(values = curve_types) +
    labs(x = expression(phi), y = "Correction multiplier") +
    paper_theme() +
    theme(legend.position = "none")

  if (panel == "Even M") return(p + coord_cartesian(ylim = c(0.85, 1.60)))
  if (panel == "Odd M, even Q") {
    return(p + geom_hline(yintercept = 0, colour = "grey75") +
             scale_y_continuous(trans = pseudo_log_trans(base = 10, sigma = 1)))
  }
  p + coord_cartesian(ylim = c(0, 22.5))
}

fig1 <-
  (make_top("Even M", TRUE) | make_top("Odd M, even Q") | make_top("Odd M, odd Q")) /
  (make_bottom("Even M") | make_bottom("Odd M, even Q") | make_bottom("Odd M, odd Q")) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(legend.position = "bottom")

save_pdf(fig1, "Figure1_correction_geometry_boundary_final.pdf", 10.3, 6.6)
write.csv(main_curves, file.path("figures", "Figure1_upper_curves.csv"), row.names = FALSE)
write.csv(negative_curves, file.path("figures", "Figure1_boundary_curves.csv"), row.names = FALSE)

# Heatmap helper for Figures 2 and S2-S5.
heatmap_plot <- function(d, methods, title = NULL) {
  d <- d[d$method %in% methods, ]
  d$x_f <- factor(d$x, levels = sort(unique(d$x)))
  d$Q_f <- factor(d$Q, levels = sort(unique(d$Q)))
  d$method <- factor(d$method, levels = methods)

  ggplot(d, aes(x_f, Q_f, fill = rejection)) +
    geom_tile() +
    geom_point(data = d[d$rejection >= 0.04 & d$rejection <= 0.06, ],
               shape = 4, size = 1.7, stroke = 0.7) +
    facet_wrap(~ method, nrow = 1) +
    scale_fill_viridis_c(name = "Rejection") +
    labs(x = "Block-scale persistence x", y = "Number of blocks Q", title = title) +
    theme_bw(base_size = 11, base_family = "serif") +
    theme(panel.grid = element_blank(), legend.position = "bottom")
}

positive <- geometry[geometry$boundary == "positive", ]
fig2 <- heatmap_plot(positive, c("Raw", "Combined"))
save_pdf(fig2, "Figure2_positive_boundary_BEL_combined.pdf", 9.5, 4.7)

figS2 <- heatmap_plot(positive, c("Raw", "Variance", "Bartlett", "Combined")) +
  facet_wrap(~ method, ncol = 2)
save_pdf(figS2, "Figure_XQ_geometry_positive_all_methods.pdf", 9.5, 7.5)

hull <- positive[positive$method == "Raw", ]
hull$x_f <- factor(hull$x, levels = sort(unique(hull$x)))
hull$Q_f <- factor(hull$Q, levels = sort(unique(hull$Q)))
figS3 <- ggplot(hull, aes(x_f, Q_f, fill = hull_failure)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Hull failure") +
  labs(x = "Block-scale persistence x", y = "Number of blocks Q") +
  theme_bw(base_size = 11, base_family = "serif") +
  theme(panel.grid = element_blank(), legend.position = "bottom")
save_pdf(figS3, "Figure_XQ_hull_failure_positive.pdf", 7.5, 5.5)

negative_even <- geometry[geometry$boundary == "negative" & geometry$parity == "even", ]
negative_odd <- geometry[geometry$boundary == "negative" & geometry$parity == "odd", ]
save_pdf(
  heatmap_plot(negative_even, c("Variance", "Combined")),
  "Figure_XQ_geometry_negative_evenM_VC_combined.pdf", 9.5, 4.7
)
save_pdf(
  heatmap_plot(negative_odd, c("Variance", "Combined")),
  "Figure_XQ_geometry_negative_oddM_VC_combined.pdf", 9.5, 4.7
)

# Figure 3: the observed VIX block path over the combined positive-boundary map.
background <- positive[positive$method == "Combined", ]
phi_hat <- vix_fit$phi_tilde[1]
N <- vix_fit$N[1]
path_constant <- N * (1 - phi_hat)
M_path <- c(2, 3, 4, 5, 6, 7, 10, 12, 14, 15, 20, 21, 28, 30, 35, 42, 60, 70, 84)
path <- data.frame(
  M = M_path,
  x = M_path * (1 - phi_hat),
  Q = N / M_path
)
path$logQ <- log10(path$Q)

# Rectangles are built on x and log10(Q), so the displayed vertical scale is logarithmic.
x_vals <- sort(unique(background$x))
q_vals <- sort(unique(background$Q))
x_mid <- c(x_vals[1] - diff(x_vals)[1] / 2,
           (x_vals[-1] + x_vals[-length(x_vals)]) / 2,
           tail(x_vals, 1) + tail(diff(x_vals), 1) / 2)
q_log <- log10(q_vals)
q_mid <- c(q_log[1] - diff(q_log)[1] / 2,
           (q_log[-1] + q_log[-length(q_log)]) / 2,
           tail(q_log, 1) + tail(diff(q_log), 1) / 2)
background$xmin <- x_mid[match(background$x, x_vals)]
background$xmax <- x_mid[match(background$x, x_vals) + 1L]
background$ymin <- q_mid[match(background$Q, q_vals)]
background$ymax <- q_mid[match(background$Q, q_vals) + 1L]
background$logQ <- log10(background$Q)

curve_x <- seq(max(min(x_vals), min(path$x)), min(max(x_vals), max(path$x)), length.out = 300)
curve <- data.frame(x = curve_x, logQ = log10(path_constant / curve_x))
labels <- path[path$M %in% c(2, 12, 42, 70, 84), ]

fig3 <- ggplot() +
  geom_rect(data = background,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = rejection)) +
  geom_contour(data = background,
               aes(x = x, y = logQ, z = rejection),
               breaks = c(0.04, 0.06), colour = "white", linetype = "dashed", linewidth = 0.45) +
  geom_contour(data = background,
               aes(x = x, y = logQ, z = rejection),
               breaks = c(0.045, 0.055), colour = "white", linewidth = 0.55) +
  geom_line(data = curve, aes(x, logQ), colour = "black", linewidth = 0.8) +
  geom_point(data = path, aes(x, logQ), shape = 21, fill = "white", size = 2.2) +
  geom_point(data = labels, aes(x, logQ), shape = 21, fill = "white", size = 3.2, stroke = 1) +
  geom_text(data = labels, aes(x, logQ, label = paste0("M=", M)),
            nudge_x = 0.20, nudge_y = 0.035, size = 3.1, hjust = 0) +
  scale_fill_viridis_c(name = "Rejection") +
  scale_y_continuous(
    breaks = log10(c(5, 10, 20, 50, 100, 200)),
    labels = c(5, 10, 20, 50, 100, 200)
  ) +
  coord_cartesian(xlim = c(min(path$x), max(x_vals)), ylim = range(log10(q_vals))) +
  labs(x = expression(x[M] == M * (1 - hat(phi))), y = "Number of complete blocks Q") +
  theme_bw(base_size = 11, base_family = "serif") +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

save_pdf(fig3, "Figure3_VIX_path_revised.pdf", 9.3, 6.0)
