setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("functions_1.R")

data_tub <- readRDS("tuberculosis_data_example.rds")

library(ggsurvfit)
library(ggplot2)
library(survminer)
library(dplyr)
library(cowplot)

intervals_surv <- function(dati1, dati2, t_0, method = "EL") {
  if (method == "EL") {
    c_rez <- suppressWarnings(try(EL_PV_SURV_11(dati1, dati2, t_0 = t_0), silent = TRUE))
  }
  else if (method == "EL S") {
    c_rez <- suppressWarnings(try(EL_PV_SURV_stute(dati1, dati2, t_0 = t_0), silent = TRUE))
  }
  else if (method == "PO") {
    c_rez <- suppressWarnings(try(PO_SURV(dati1, dati2, t_0 = t_0), silent = TRUE))
  }
  else if (method == "Normal") {
    c_rez <- suppressWarnings(try(crude_SURV(dati1, dati2, t_0 = t_0), silent = TRUE))
  }
  else if (method == "MZ") {
    c_rez <- suppressWarnings(try(EL_MZ_SURV_sb(dati1, dati2, t_0 = t_0, N = 1000, seed = 887)[1:5], silent = TRUE))
  }
  if (!inherits(c_rez, "try-error"))
    return(c_rez)
  else
    return(rep(NA, 5))
}

intervals_rmst <- function(dati1, dati2, t_0, method = "EL", lw_, up_) {
  if (method == "EL") {
    c_rez <- suppressWarnings(try(EL_PV_RMST_11(dati1, dati2, t_0 = t_0), silent = TRUE))
  }
  else if (method == "EL S") {
    c_rez <- suppressWarnings(try(EL_PV_RMST_stute(dati1, dati2, t_0 = t_0), silent = TRUE))
  }
  else if (method == "PO") {
    c_rez <- suppressWarnings(try(PO_RMST(dati1, dati2, t_0 = t_0), silent = TRUE))
  }
  else if (method == "Normal") {
    c_rez <- suppressWarnings(try(crude_RMST(dati1, dati2, t_0 = t_0), silent = TRUE))
  }
  else if (method == "Zhou") {
    c_rez <- suppressWarnings(try(EL_Zhou_RMST(dati1, dati2, t_0 = t_0, lw = lw_, up = up_), silent = TRUE))
  }
  if (!inherits(c_rez, "try-error"))
    return(c_rez)
  else
    return(rep(NA, 5))
}

intervals_surv_V <- Vectorize(intervals_surv, vectorize.args = "t_0")
intervals_rmst_V <- Vectorize(intervals_rmst, vectorize.args = "t_0")

#-------------------------------------------------------------------------------
# Tuberculosis data by GSTM1
#-------------------------------------------------------------------------------
surv_object <- Surv(time = data_tub$ztimes, event = data_tub$status)
fit1 <- survfit(surv_object ~ GSTM1, data = data_tub)

my_legend <- expression(paste("GSTM1", " genotype"))

g1 <- ggsurvplot(
    fit1,
    data = data_tub,
    fun = "event",
    palette = c("orchid3", "chartreuse3"),
    linetype = c("dashed", "solid"),
    xlab =
        "Time in days",
    ylab = "Cumulative probability of successful treatment response",
    legend.labs = c("null", "plus"),
    legend.title = my_legend,
    size = 0.8,
    conf.int.style =
        "step",
    censor.shape = "+",
    censor.size = 7
)

GSTM1_plot <- g1$plot + theme_light() + theme(legend.position = "top") +
    geom_text(aes(70, 0.1), label = expression(Log - rank ~ italic(p) ~"= 0.27")) +
    theme(
        legend.position = c(.97, .65),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.box.background = element_rect(color = "black", size = 0.5)
    )

data_tub %>%
  filter(GSTM1 == "plus") %>%
  select(ztimes, status) -> izl1
data_tub %>%
  filter(GSTM1 == "null") %>%
  select(ztimes, status) -> izl2

# Survival probability
time_sequence_1 <- seq(11, 196, by = 3)

surv_rez_EL <- intervals_surv_V(izl1, izl2, t_0 = time_sequence_1, "EL")
surv_data_EL <- data.frame(
    estimate = unlist(surv_rez_EL[4, ]),
    lower = unlist(surv_rez_EL[1, ]),
    upper = unlist(surv_rez_EL[2, ]),
    time = time_sequence_1
)
surv_rez_EL_S <- intervals_surv_V(izl1, izl2, t_0 = time_sequence_1, "EL S")
surv_data_EL_S <- data.frame(
    estimate = unlist(surv_rez_EL_S[4, ]),
    lower = unlist(surv_rez_EL_S[1, ]),
    upper = unlist(surv_rez_EL_S[2, ]),
    time = time_sequence_1
)
surv_rez_PO <- intervals_surv_V(izl1, izl2, t_0 = time_sequence_1, "PO")
surv_data_PO <- data.frame(
    estimate = unlist(surv_rez_PO[4, ]),
    lower = unlist(surv_rez_PO[1, ]),
    upper = unlist(surv_rez_PO[2, ]),
    time = time_sequence_1
)
surv_rez_crude <- intervals_surv_V(izl1, izl2, t_0 = time_sequence_1, "Normal")
surv_data_crude <- data.frame(
    estimate = unlist(surv_rez_crude[4, ]),
    lower = unlist(surv_rez_crude[1, ]),
    upper = unlist(surv_rez_crude[2, ]),
    time = time_sequence_1
)

surv_rez_EL_MZ <- intervals_surv_V(izl1, izl2, t_0 = time_sequence_1, "MZ")
surv_data_EL_MZ <- data.frame(
    estimate = unlist(surv_rez_EL_MZ[4, ]),
    lower = unlist(surv_rez_EL_MZ[1, ]),
    upper = unlist(surv_rez_EL_MZ[2, ]),
    time = time_sequence_1
)

colors <- c(
    "Norm. approx." = "orange2",
    "PO" = "chartreuse2",
    "EL" = "orchid3",
    "EL (Stute)" = "cadetblue",
    "EL (MZ)" = "brown1"
)
types <- c(
    "Norm. approx." = "dashed",
    "PO" = "dotted",
    "EL" = "dotdash",
    "EL (Stute)" = "twodash",
    "EL (MZ)" = "longdash"
)

GSTM1_surv_plot <- ggplot(surv_data_EL, aes(x = time, y = estimate)) +
    geom_abline(intercept = 0, slope = 0, lty = "dashed") +
    ylab("Difference of survival probabilities") +
    xlab("Time in days") +
    geom_line(data = surv_data_PO, aes(x = time, y = upper, color = "PO", lty = "PO"), size = 0.8) +
    geom_line(data = surv_data_PO, aes(y = estimate), lty = "solid", colour = "chartreuse3", size = 0.8) +
    geom_line(data = surv_data_PO, aes(y = lower), lty = "dotted", colour = "chartreuse3", lwd = 0.8) +
        
    geom_line(data = surv_data_crude, aes(x = time, y = upper, color = "Norm. approx.", lty = "Norm. approx."), size = 0.8) +
    geom_line(data = surv_data_crude, aes(y = estimate), lty = "solid", colour = "orange2", size = 0.8) +
    geom_line(data = surv_data_crude, aes(y = lower), lty = "dashed", colour = "orange2", lwd = 0.8) +
        
    geom_line(data = surv_data_EL_S, aes(x = time, y = upper, colour = "EL (Stute)", lty = "EL (Stute)"), size = 0.8) +
    geom_line(data = surv_data_EL_S, aes(y = estimate), lty = "solid", colour = "cadetblue", size = 0.8) +
    geom_line(data = surv_data_EL_S, aes(y = lower), lty = "twodash", colour = "cadetblue", size = 0.8) +
    
    geom_line(data = surv_data_EL, aes(x = time, y = upper, colour = "EL", lty = "EL"), size = 0.8) +    
    geom_line(data = surv_data_EL, aes(y = estimate), lty = "solid", colour = "orchid3", size = 0.8) +
    geom_line(data = surv_data_EL, aes(y = lower), lty = "dotdash", colour = "orchid3", lwd = 0.8) +
        
    geom_line(data = surv_data_EL_MZ, aes(x = time, y = upper, lty = "EL (MZ)", colour = "EL (MZ)"), size = 0.8) +
    geom_line(data = surv_data_EL_MZ, aes(y = estimate), lty = "solid", colour = "brown1", size = 0.8) +
    geom_line(data = surv_data_EL_MZ, aes(y = lower), lty = "longdash", colour = "brown1", lwd = 0.8) +
    scale_linetype_manual("", values = types) +
    scale_colour_manual("", values = colors) +
    theme_light() + 
    theme(
        legend.position = c(0.97, 0.97),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.box.background = element_rect(color = "black", size = 0.5)
    )

#RMST
time_sequence_2 <- seq(12, 185, by = 3)

rmst_rez_EL <- intervals_rmst_V(izl1, izl2, t_0 = time_sequence_2, "EL")
rmst_data_EL <- data.frame(
    estimate = unlist(rmst_rez_EL[4, ]),
    lower = unlist(rmst_rez_EL[1, ]),
    upper = unlist(rmst_rez_EL[2, ]),
    time = time_sequence_2
)

rmst_rez_EL_S <- intervals_rmst_V(izl1, izl2, t_0 = time_sequence_2, "EL S")
rmst_data_EL_S <- data.frame(
    estimate = unlist(rmst_rez_EL_S[4, ]),
    lower = unlist(rmst_rez_EL_S[1, ]),
    upper = unlist(rmst_rez_EL_S[2, ]),
    time = time_sequence_2
)

rmst_rez_PO <- intervals_rmst_V(izl1, izl2, t_0 = time_sequence_2, "PO")
rmst_data_PO <- data.frame(
    estimate = unlist(rmst_rez_PO[4, ]),
    lower = unlist(rmst_rez_PO[1, ]),
    upper = unlist(rmst_rez_PO[2, ]),
    time = time_sequence_2
)

rmst_rez_crude <- intervals_rmst_V(izl1, izl2, t_0 = time_sequence_2, "Normal")
rmst_data_crude <- data.frame(
    estimate = unlist(rmst_rez_crude[4, ]),
    lower = unlist(rmst_rez_crude[1, ]),
    upper = unlist(rmst_rez_crude[2, ]),
    time = time_sequence_2
)

rmst_rez_EL_Zhou <- intervals_rmst_V(izl1, izl2, t_0 = time_sequence_2, "Zhou", lw_ = -100, up_ = 100)
rmst_data_EL_Zhou <- data.frame(
    estimate = unlist(rmst_rez_EL_Zhou[4, ]),
    lower = unlist(rmst_rez_EL_Zhou[1, ]),
    upper = unlist(rmst_rez_EL_Zhou[2, ]),
    time = time_sequence_2
)

colors <- c(
    "Norm. approx." = "orange2",
    "PO" = "chartreuse2",
    "EL" = "orchid3",
    "EL (Stute)" = "cadetblue",
    "EL (Zhou)" = "brown1"
)
types <- c(
    "Norm. approx." = "dashed",
    "PO" = "dotted",
    "EL" = "dotdash",
    "EL (Stute)" = "twodash",
    "EL (Zhou)" = "longdash"
)

GSTM1_rmst_plot <- ggplot(rmst_data_EL, aes(x = time, y = estimate)) +
    geom_abline(intercept = 0, slope = 0, lty = "dashed") +
    ylab("Difference of RMST's") +
    xlab("Time in days") +
    geom_line(data = rmst_data_PO, aes(x = time, y = upper, color = "PO", lty = "PO"), size = 0.8) +
    geom_line(data = rmst_data_PO, aes(y = estimate), lty = "solid", colour = "chartreuse3", size = 0.8) +
    geom_line(data = rmst_data_PO, aes(y = lower), lty = "dotted", colour = "chartreuse3", lwd = 0.8) +
    
    geom_line(data = rmst_data_crude, aes(x = time, y = upper, color = "Norm. approx.", lty = "Norm. approx."), size = 0.8) +
    geom_line(data = rmst_data_crude, aes(y = estimate), lty = "solid", colour = "orange2", size = 0.8) +
    geom_line(data = rmst_data_crude, aes(y = lower), lty = "dashed", colour = "orange2", lwd = 0.8) +
    
    geom_line(data = rmst_data_EL_S, aes(x = time, y = upper, colour = "EL (Stute)", lty = "EL (Stute)"), size = 0.8) +
    geom_line(data = rmst_data_EL_S, aes(y = estimate), lty = "solid", colour = "cadetblue", size = 0.8) + 
    geom_line(data = rmst_data_EL_S, aes(y = lower), lty = "twodash", colour = "cadetblue", size = 0.8) +
    
    geom_line(data = rmst_data_EL, aes(y = upper, colour = "EL", lty = "EL"), size = 0.8) +    
    geom_line(data = rmst_data_EL, aes(y = estimate), lty = "solid", colour = "orchid3", size = 0.8) +
    geom_line(data = rmst_data_EL, aes(y = lower), lty = "dotdash", colour = "orchid3", lwd = 0.8) +
    
    geom_line(data = rmst_data_EL_Zhou, aes(x = time, y = upper, lty = "EL (Zhou)", colour = "EL (Zhou)"), size = 0.8) +
    geom_line(data = rmst_data_EL_Zhou, aes(y = estimate), lty = "solid", colour = "brown1", size = 0.8) +
    geom_line(data = rmst_data_EL_Zhou, aes(y = lower), lty = "longdash", colour = "brown1", lwd = 0.8) +
    scale_linetype_manual("", values = types) +
    scale_colour_manual("", values = colors) +
    theme_light() + 
    theme(
        legend.position = c(0.3, 0.35),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.box.background = element_rect(color = "black", size = 0.5)
    )


plot_grid(GSTM1_plot, GSTM1_surv_plot, GSTM1_rmst_plot, nrow = 1, align = "h", labels = "AUTO")
ggsave(paste0("GSTM1_plot_", as.numeric(Sys.time()), ".png"), device = "png", dpi = 600, width = 16, height = 5)

#-------------------------------------------------------------------------------
# Tuberculosis data by NAT2
#-------------------------------------------------------------------------------
surv_object <- Surv(time = data_tub$ztimes, event = data_tub$status)

fit2 <- survfit(surv_object ~ NAT2, data = data_tub)
my_legend2 <- expression(paste("NAT2", " acetylator"))

g2 <- ggsurvplot(
    fit2,
    data = data_tub,
    fun = "event",
    linetype = c("dashed", "solid"),
    palette = c("orchid3", "chartreuse3"),
    xlab = "Time in days",
    ylab = "Cumulative probability of successful treatment response",
    legend.labs = c("IA", "SA"),
    legend.title = my_legend2,
    size = 1,
    conf.int.style = "step",
    censor.shape = "+",
    censor.size = 7
)

NAT2_plot <- g2$plot + theme_light() + theme(legend.position = "top") +
    geom_text(aes(70, 0.1), label = expression(Log - rank ~ italic(p) ~"= 0.15")) +
    theme(
        legend.position = c(.97, .7),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.box.background = element_rect(color = "black", size = 0.5)
    )

data_tub %>%
  filter(NAT2 == "IA") %>%
  select(ztimes, status) -> izl1
data_tub %>%
  filter(NAT2 == "SA") %>%
  select(ztimes, status) -> izl2

# Survival probability
time_sequence_3 <- seq(12, 196, by = 3)

surv_rez_EL <- intervals_surv_V(izl1, izl2, t_0 = time_sequence_3, "EL")
surv_data_EL <- data.frame(
    estimate = unlist(surv_rez_EL[4, ]),
    lower = unlist(surv_rez_EL[1, ]),
    upper = unlist(surv_rez_EL[2, ]),
    time = time_sequence_3
)
surv_rez_EL_S <- intervals_surv_V(izl1, izl2, t_0 = time_sequence_3, "EL S")
surv_data_EL_S <- data.frame(
    estimate = unlist(surv_rez_EL_S[4, ]),
    lower = unlist(surv_rez_EL_S[1, ]),
    upper = unlist(surv_rez_EL_S[2, ]),
    time = time_sequence_3
)
surv_rez_PO <- intervals_surv_V(izl1, izl2, t_0 = time_sequence_3, "PO")
surv_data_PO <- data.frame(
    estimate = unlist(surv_rez_PO[4, ]),
    lower = unlist(surv_rez_PO[1, ]),
    upper = unlist(surv_rez_PO[2, ]),
    time = time_sequence_3
)
surv_rez_crude <- intervals_surv_V(izl1, izl2, t_0 = time_sequence_3, "Normal")
surv_data_crude <- data.frame(
    estimate = unlist(surv_rez_crude[4, ]),
    lower = unlist(surv_rez_crude[1, ]),
    upper = unlist(surv_rez_crude[2, ]),
    time = time_sequence_3
)
surv_rez_EL_MZ <- intervals_surv_V(izl1, izl2, t_0 = time_sequence_3, "MZ")
surv_data_EL_MZ <- data.frame(
    estimate = unlist(surv_rez_EL_MZ[4, ]),
    lower = unlist(surv_rez_EL_MZ[1, ]),
    upper = unlist(surv_rez_EL_MZ[2, ]),
    time = time_sequence_3
)

colors <- c(
    "Norm. approx." = "orange2",
    "PO" = "chartreuse2",
    "EL" = "orchid3",
    "EL (Stute)" = "cadetblue",
    "EL (MZ)" = "brown1"
)
types <- c(
    "Norm. approx." = "dashed",
    "PO" = "dotted",
    "EL" = "dotdash",
    "EL (Stute)" = "twodash",
    "EL (MZ)" = "longdash"
)

NAT2_surv_plot <- ggplot(surv_data_EL, aes(x = time, y = estimate)) +
    geom_abline(intercept = 0, slope = 0, lty = "dashed") +
    ylab("Difference of survival probabilities") +
    xlab("Time in days") +
    geom_line(data = surv_data_PO, aes(x = time, y = upper, color = "PO", lty = "PO"), size = 0.8) +
    geom_line(data = surv_data_PO, aes(y = estimate), lty = "solid", colour = "chartreuse3", size = 0.8) +
    geom_line(data = surv_data_PO, aes(y = lower), lty = "dotted", colour = "chartreuse3", lwd = 0.8) +
    
    geom_line(data = surv_data_crude, aes(x = time, y = upper, color = "Norm. approx.", lty = "Norm. approx."), size = 0.8) +
    geom_line(data = surv_data_crude, aes(y = estimate), lty = "solid", colour = "orange2", size = 0.8) +
    geom_line(data = surv_data_crude, aes(y = lower), lty = "dashed", colour = "orange2", lwd = 0.8) +
    
    geom_line(data = surv_data_EL_S, aes(x = time, y = upper, colour = "EL (Stute)", lty = "EL (Stute)"), size = 0.8) +
    geom_line(data = surv_data_EL_S, aes(y = estimate), lty = "solid", colour = "cadetblue", size = 0.8) + 
    geom_line(data = surv_data_EL_S, aes(y = lower), lty = "twodash", colour = "cadetblue", size = 0.8) +
    
    geom_line(data = surv_data_EL, aes(y = upper, colour = "EL", lty = "EL"), size = 0.8) +    
    geom_line(data = surv_data_EL, aes(y = estimate), lty = "solid", colour = "orchid3", size = 0.8) +
    geom_line(data = surv_data_EL, aes(y = lower), lty = "dotdash", colour = "orchid3", lwd = 0.8) +
    
    geom_line(data = surv_data_EL_MZ, aes(x = time, y = upper, lty = "EL (MZ)", colour = "EL (MZ)"), size = 0.8) +
    geom_line(data = surv_data_EL_MZ, aes(y = estimate), lty = "solid", colour = "brown1", size = 0.8) +
    geom_line(data = surv_data_EL_MZ, aes(y = lower), lty = "longdash", colour = "brown1", lwd = 0.8) +
    scale_linetype_manual("", values = types) +
    scale_colour_manual("", values = colors) +
    theme_light()+ 
    theme(
        legend.position = c(0.97, 0.98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.box.background = element_rect(color = "black", size = 0.5)
    )

#RMST
time_sequence_4 <- seq(12, 185, by = 3)

rmst_rez_EL <- intervals_rmst_V(izl1, izl2, t_0 = time_sequence_4, "EL")
rmst_data_EL <- data.frame(
    estimate = unlist(rmst_rez_EL[4, ]),
    lower = unlist(rmst_rez_EL[1, ]),
    upper = unlist(rmst_rez_EL[2, ]),
    time = time_sequence_4
)

rmst_rez_EL_S <- intervals_rmst_V(izl1, izl2, t_0 = time_sequence_4, "EL S")
rmst_data_EL_S <- data.frame(
    estimate = unlist(rmst_rez_EL_S[4, ]),
    lower = unlist(rmst_rez_EL_S[1, ]),
    upper = unlist(rmst_rez_EL_S[2, ]),
    time = time_sequence_4
)

rmst_rez_PO <- intervals_rmst_V(izl1, izl2, t_0 = time_sequence_4, "PO")
rmst_data_PO <- data.frame(
    estimate = unlist(rmst_rez_PO[4, ]),
    lower = unlist(rmst_rez_PO[1, ]),
    upper = unlist(rmst_rez_PO[2, ]),
    time = time_sequence_4
)

rmst_rez_crude <- intervals_rmst_V(izl1, izl2, t_0 = time_sequence_4, "Normal")
rmst_data_crude <- data.frame(
    estimate = unlist(rmst_rez_crude[4, ]),
    lower = unlist(rmst_rez_crude[1, ]),
    upper = unlist(rmst_rez_crude[2, ]),
    time = time_sequence_4
)

rmst_rez_EL_Zhou <- intervals_rmst_V(izl1, izl2, t_0 = time_sequence_4, "Zhou", lw_ = -1000, up_ = 1000)
rmst_data_EL_Zhou <- data.frame(
    estimate = unlist(rmst_rez_EL_Zhou[4, ]),
    lower = unlist(rmst_rez_EL_Zhou[1, ]),
    upper = unlist(rmst_rez_EL_Zhou[2, ]),
    time = time_sequence_4
)

colors <- c(
    "Norm. approx." = "orange2",
    "PO" = "chartreuse2",
    "EL" = "orchid3",
    "EL (Stute)" = "cadetblue",
    "EL (Zhou)" = "brown1"
)
types <- c(
    "Norm. approx." = "dashed",
    "PO" = "dotted",
    "EL" = "dotdash",
    "EL (Stute)" = "twodash",
    "EL (Zhou)" = "longdash"
)

NAT2_rmst_plot <- ggplot(rmst_data_EL, aes(x = time, y = estimate)) +
    geom_abline(intercept = 0, slope = 0, lty = "dashed") +
    ylab("Difference of RMST's") +
    xlab("Time in days") +
    geom_line(data = rmst_data_PO, aes(x = time, y = upper, color = "PO", lty = "PO"), size = 0.8) +
    geom_line(data = rmst_data_PO, aes(y = estimate), lty = "solid", colour = "chartreuse3", size = 0.8) +
    geom_line(data = rmst_data_PO, aes(y = lower), lty = "dotted", colour = "chartreuse3", lwd = 0.8) +
    
    geom_line(data = rmst_data_crude, aes(x = time, y = upper, color = "Norm. approx.", lty = "Norm. approx."), size = 0.8) +
    geom_line(data = rmst_data_crude, aes(y = estimate), lty = "solid", colour = "orange2", size = 0.8) +
    geom_line(data = rmst_data_crude, aes(y = lower), lty = "dashed", colour = "orange2", lwd = 0.8) +
    
    geom_line(data = rmst_data_EL_S, aes(x = time, y = upper, colour = "EL (Stute)", lty = "EL (Stute)"), size = 0.8) +
    geom_line(data = rmst_data_EL_S, aes(y = estimate), lty = "solid", colour = "cadetblue", size = 0.8) + 
    geom_line(data = rmst_data_EL_S, aes(y = lower), lty = "twodash", colour = "cadetblue", size = 0.8) +
    
    geom_line(data = rmst_data_EL, aes(y = upper, colour = "EL", lty = "EL"), size = 0.8) +    
    geom_line(data = rmst_data_EL, aes(y = estimate), lty = "solid", colour = "orchid3", size = 0.8) +
    geom_line(data = rmst_data_EL, aes(y = lower), lty = "dotdash", colour = "orchid3", lwd = 0.8) +
    
    geom_line(data = rmst_data_EL_Zhou, aes(x = time, y = upper, lty = "EL (Zhou)", colour = "EL (Zhou)"), size = 0.8) +
    geom_line(data = rmst_data_EL_Zhou, aes(y = estimate), lty = "solid", colour = "brown1", size = 0.8) +
    geom_line(data = rmst_data_EL_Zhou, aes(y = lower), lty = "longdash", colour = "brown1", lwd = 0.8) +
    scale_linetype_manual("", values = types) +
    scale_colour_manual("", values = colors) +
    theme_light()+ 
    theme(
        legend.position = c(0.3, 0.98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.box.background = element_rect(color = "black", size = 0.5)
    )

plot_grid(NAT2_plot, NAT2_surv_plot, NAT2_rmst_plot, nrow = 1, align = "h", labels = "AUTO")
ggsave(paste0("NAT2_plot_", as.numeric(Sys.time()), ".png"), device = "png", dpi = 600, width = 16, height = 5)

#-------------------------------------------------------------------------------
# Cancer data
#-------------------------------------------------------------------------------

cancer$ID <- 1:nrow(cancer) #228
cancer$status1 <- ifelse(cancer$status == 1, 0, 1)

data_canc <- data.frame(ID = cancer$ID,
                        ztimes = cancer$time/365.25,
                        status = cancer$status1,  # 0 - cenzēts
                        sex = cancer$sex)

data_canc <- order_times(data_canc, column = 2)

surv_object <- Surv(time = data_canc$ztimes, event = data_canc$status)

fit1 <- survfit(surv_object ~ sex, data = data_canc)

my_legend <- expression(paste("Sex"))

g_canc <- ggsurvplot(
    fit1,
    data = data_canc,
    palette = c("orchid3", "chartreuse3"),
    linetype = c("dashed", "solid"),
    xlab = "Time in years",
    legend.labs = c("Male", "Female"),
    legend.title = my_legend,
    size = 0.8,
    conf.int.style = "step",
    censor.shape = "+",
    censor.size = 5
)

gg <- g_canc$plot + theme_light() + theme(legend.position = "top") +
    geom_text(aes(800 / 365, 0.4), label = expression(Log - rank ~ italic(p) ~ "= 0.0013"))

Canc_plot <- gg + theme(
    legend.position = c(.97, .97),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.box.background = element_rect(color = "black", size = 0.5)
)

data_canc %>%
    filter(sex == 1) %>%
    select(ztimes, status) -> izl1
data_canc %>%
    filter(sex == 2) %>%
    select(ztimes, status) -> izl2

# Survival probability
time_sequence_5 <- seq(0.1, 2.0, by = 0.05)
#time_sequence_5 <- seq(0.1, 2.0, by = 0.4)

surv_rez_EL <- intervals_surv_V(izl1, izl2, t_0 = time_sequence_5, "EL")
surv_data_EL <- data.frame(
    estimate = unlist(surv_rez_EL[4, ]),
    lower = unlist(surv_rez_EL[1, ]),
    upper = unlist(surv_rez_EL[2, ]),
    time = time_sequence_5
)
surv_rez_EL_S <- intervals_surv_V(izl1, izl2, t_0 = time_sequence_5, "EL S")
surv_data_EL_S <- data.frame(
    estimate = unlist(surv_rez_EL_S[4, ]),
    lower = unlist(surv_rez_EL_S[1, ]),
    upper = unlist(surv_rez_EL_S[2, ]),
    time = time_sequence_5
)
surv_rez_PO <- intervals_surv_V(izl1, izl2, t_0 = time_sequence_5, "PO")
surv_data_PO <- data.frame(
    estimate = unlist(surv_rez_PO[4, ]),
    lower = unlist(surv_rez_PO[1, ]),
    upper = unlist(surv_rez_PO[2, ]),
    time = time_sequence_5
)
surv_rez_crude <- intervals_surv_V(izl1, izl2, t_0 = time_sequence_5, "Normal")
surv_data_crude <- data.frame(
    estimate = unlist(surv_rez_crude[4, ]),
    lower = unlist(surv_rez_crude[1, ]),
    upper = unlist(surv_rez_crude[2, ]),
    time = time_sequence_5
)
surv_rez_EL_MZ <- intervals_surv_V(izl1, izl2, t_0 = time_sequence_5, "MZ")
surv_data_EL_MZ <- data.frame(
    estimate = unlist(surv_rez_EL_MZ[4, ]),
    lower = unlist(surv_rez_EL_MZ[1, ]),
    upper = unlist(surv_rez_EL_MZ[2, ]),
    time = time_sequence_5
)

colors <- c(
    "Norm. approx." = "orange2",
    "PO" = "chartreuse2",
    "EL" = "orchid3",
    "EL (Stute)" = "cadetblue",
    "EL (MZ)" = "brown1"
)
types <- c(
    "Norm. approx." = "dashed",
    "PO" = "dotted",
    "EL" = "dotdash",
    "EL (Stute)" = "twodash",
    "EL (MZ)" = "longdash"
)

Canc_surv_plot <- ggplot(surv_data_EL, aes(x = time, y = estimate)) +
    geom_abline(intercept = 0, slope = 0, lty = "dashed") +
    ylab("Difference of survival probabilities") +
    xlab("Time in days") +
    geom_line(data = surv_data_PO, aes(x = time, y = upper, color = "PO", lty = "PO"), size = 0.8) +
    geom_line(data = surv_data_PO, aes(y = estimate), lty = "solid", colour = "chartreuse3", size = 0.8) +
    geom_line(data = surv_data_PO, aes(y = lower), lty = "dotted", colour = "chartreuse3", lwd = 0.8) +
    
    geom_line(data = surv_data_crude, aes(x = time, y = upper, color = "Norm. approx.", lty = "Norm. approx."), size = 0.8) +
    geom_line(data = surv_data_crude, aes(y = estimate), lty = "solid", colour = "orange2", size = 0.8) +
    geom_line(data = surv_data_crude, aes(y = lower), lty = "dashed", colour = "orange2", lwd = 0.8) +
    
    geom_line(data = surv_data_EL_S, aes(x = time, y = upper, colour = "EL (Stute)", lty = "EL (Stute)"), size = 0.8) +
    geom_line(data = surv_data_EL_S, aes(y = estimate), lty = "solid", colour = "cadetblue", size = 0.8) + 
    geom_line(data = surv_data_EL_S, aes(y = lower), lty = "twodash", colour = "cadetblue", size = 0.8) +
    
    geom_line(data = surv_data_EL, aes(y = upper, colour = "EL", lty = "EL"), size = 0.8) +    
    geom_line(data = surv_data_EL, aes(y = estimate), lty = "solid", colour = "orchid3", size = 0.8) +
    geom_line(data = surv_data_EL, aes(y = lower), lty = "dotdash", colour = "orchid3", lwd = 0.8) +
    
    geom_line(data = surv_data_EL_MZ, aes(x = time, y = upper, lty = "EL (MZ)", colour = "EL (MZ)"), size = 0.8) +
    geom_line(data = surv_data_EL_MZ, aes(y = estimate), lty = "solid", colour = "brown1", size = 0.8) +
    geom_line(data = surv_data_EL_MZ, aes(y = lower), lty = "longdash", colour = "brown1", lwd = 0.8) +
    scale_linetype_manual("", values = types) +
    scale_colour_manual("", values = colors) +
    theme_light()+ 
    theme(
        legend.position = c(0.97, 0.98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.box.background = element_rect(color = "black", size = 0.5)
    )

#RMST
time_sequence_6 <- seq(0.1, 2.2, by = 0.05)

rmst_rez_EL <- intervals_rmst_V(izl1, izl2, t_0 = time_sequence_6, "EL")
rmst_data_EL <- data.frame(
    estimate = unlist(rmst_rez_EL[4, ]),
    lower = unlist(rmst_rez_EL[1, ]),
    upper = unlist(rmst_rez_EL[2, ]),
    time = time_sequence_6
)

rmst_rez_EL_S <- intervals_rmst_V(izl1, izl2, t_0 = time_sequence_6, "EL S")
rmst_data_EL_S <- data.frame(
    estimate = unlist(rmst_rez_EL_S[4, ]),
    lower = unlist(rmst_rez_EL_S[1, ]),
    upper = unlist(rmst_rez_EL_S[2, ]),
    time = time_sequence_6
)

rmst_rez_PO <- intervals_rmst_V(izl1, izl2, t_0 = time_sequence_6, "PO")
rmst_data_PO <- data.frame(
    estimate = unlist(rmst_rez_PO[4, ]),
    lower = unlist(rmst_rez_PO[1, ]),
    upper = unlist(rmst_rez_PO[2, ]),
    time = time_sequence_6
)

rmst_rez_crude <- intervals_rmst_V(izl1, izl2, t_0 = time_sequence_6, "Normal")
rmst_data_crude <- data.frame(
    estimate = unlist(rmst_rez_crude[4, ]),
    lower = unlist(rmst_rez_crude[1, ]),
    upper = unlist(rmst_rez_crude[2, ]),
    time = time_sequence_6
)

rmst_rez_EL_Zhou <- intervals_rmst_V(izl1, izl2, t_0 = time_sequence_6, "Zhou", lw_ = -1000, up_ = 1000)
rmst_data_EL_Zhou <- data.frame(
    estimate = unlist(rmst_rez_EL_Zhou[4, ]),
    lower = unlist(rmst_rez_EL_Zhou[1, ]),
    upper = unlist(rmst_rez_EL_Zhou[2, ]),
    time = time_sequence_6
)

colors <- c(
    "Norm. approx." = "orange2",
    "PO" = "chartreuse2",
    "EL" = "orchid3",
    "EL (Stute)" = "cadetblue",
    "EL (Zhou)" = "brown1"
)
types <- c(
    "Norm. approx." = "dashed",
    "PO" = "dotted",
    "EL" = "dotdash",
    "EL (Stute)" = "twodash",
    "EL (Zhou)" = "longdash"
)

Canc_rmst_plot <- ggplot(rmst_data_EL, aes(x = time, y = estimate)) +
    geom_abline(intercept = 0, slope = 0, lty = "dashed") +
    ylab("Difference of RMST's") +
    xlab("Time in days") +
    geom_line(data = rmst_data_PO, aes(x = time, y = upper, color = "PO", lty = "PO"), size = 0.8) +
    geom_line(data = rmst_data_PO, aes(y = estimate), lty = "solid", colour = "chartreuse3", size = 0.8) +
    geom_line(data = rmst_data_PO, aes(y = lower), lty = "dotted", colour = "chartreuse3", lwd = 0.8) +
    
    geom_line(data = rmst_data_crude, aes(x = time, y = upper, color = "Norm. approx.", lty = "Norm. approx."), size = 0.8) +
    geom_line(data = rmst_data_crude, aes(y = estimate), lty = "solid", colour = "orange2", size = 0.8) +
    geom_line(data = rmst_data_crude, aes(y = lower), lty = "dashed", colour = "orange2", lwd = 0.8) +
    
    geom_line(data = rmst_data_EL_S, aes(x = time, y = upper, colour = "EL (Stute)", lty = "EL (Stute)"), size = 0.8) +
    geom_line(data = rmst_data_EL_S, aes(y = estimate), lty = "solid", colour = "cadetblue", size = 0.8) + 
    geom_line(data = rmst_data_EL_S, aes(y = lower), lty = "twodash", colour = "cadetblue", size = 0.8) +
    
    geom_line(data = rmst_data_EL, aes(y = upper, colour = "EL", lty = "EL"), size = 0.8) +    
    geom_line(data = rmst_data_EL, aes(y = estimate), lty = "solid", colour = "orchid3", size = 0.8) +
    geom_line(data = rmst_data_EL, aes(y = lower), lty = "dotdash", colour = "orchid3", lwd = 0.8) +
    
    geom_line(data = rmst_data_EL_Zhou, aes(x = time, y = upper, lty = "EL (Zhou)", colour = "EL (Zhou)"), size = 0.8) +
    geom_line(data = rmst_data_EL_Zhou, aes(y = estimate), lty = "solid", colour = "brown1", size = 0.8) +
    geom_line(data = rmst_data_EL_Zhou, aes(y = lower), lty = "longdash", colour = "brown1", lwd = 0.8) +
    scale_linetype_manual("", values = types) +
    scale_colour_manual("", values = colors) +
    theme_light()+ 
    theme(
        legend.position = c(0.3, 0.98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.box.background = element_rect(color = "black", size = 0.5)
    )

plot_grid(Canc_plot, Canc_surv_plot, Canc_rmst_plot, nrow = 1, align = "h", labels = "AUTO")
ggsave(paste0("Cancer_plot_", as.numeric(Sys.time()), ".png"), device = "png", dpi = 600, width = 16, height = 5)