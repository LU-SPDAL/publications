############################################################
# Nikkei 225 empirical application
# Main split, robustness check, and rolling-window diagnostics
############################################################
#
# This script reproduces the empirical application section of
# the manuscript.
#
# It performs:
#   1. Data retrieval from Yahoo Finance using quantmod.
#   2. Construction of daily log returns and squared log returns.
#   3. Main pre-COVID vs COVID comparison.
#   4. Alternative-cutoff robustness check.
#   5. Rolling-window FDEL p-value curves.
#   6. Rolling-window heatmap diagnostics.
#
# Required project functions:
#   - bel_functions.R
#   - wald_ar1_rho1_functions.R
#   - wald_hac_rho1_functions.R
#
# FDEL is called directly from the EL package through FDEL.acf().
#
############################################################


# ==========================================================
# 0. User settings
# ==========================================================

REPO_ROOT <- "."
setwd(REPO_ROOT)

BOOTSTRAP_SAMPLES <- 500
BOOTSTRAP_SEED <- 7

ALPHA <- 0.05
LAG <- 1
DELTA0 <- 0

# Confidence interval inversion settings.
CI_LEVEL <- 0.95
CI_LOWER <- -1.98
CI_UPPER <-  1.98
CI_STEP  <- 0.01

# Rolling-window settings.
WINDOW_LENGTHS <- seq(20, 200, by = 20)
ROLLING_STEP <- 10

# Set this to FALSE if you want to skip the bootstrap Bartlett-type
# confidence interval, which can take some time.
COMPUTE_BFDEL_CI <- TRUE


# ==========================================================
# 1. Packages and project functions
# ==========================================================

required_packages <- c(
  "quantmod",
  "xts",
  "ggplot2",
  "dplyr",
  "tidyr",
  "patchwork",
  "EL"
)

to_install <- required_packages[!required_packages %in% rownames(installed.packages())]

if (length(to_install) > 0) {
  install.packages(to_install)
}

invisible(lapply(required_packages, require, character.only = TRUE))

source("bel_functions.R")
source("wald_ar1_rho1_functions.R")
source("wald_hac_rho1_functions.R")

if (!dir.exists("outputs")) dir.create("outputs")
if (!dir.exists("outputs/tables")) dir.create("outputs/tables", recursive = TRUE)
if (!dir.exists("outputs/figures")) dir.create("outputs/figures", recursive = TRUE)
if (!dir.exists("outputs/data")) dir.create("outputs/data", recursive = TRUE)


# ==========================================================
# 2. Small helper functions
# ==========================================================

get_p_value <- function(obj) {
  if (is.list(obj) && !is.null(obj$p.value)) return(as.numeric(obj$p.value))
  if (is.list(obj) && !is.null(obj$p)) return(as.numeric(obj$p))
  if (is.numeric(obj) && length(obj) >= 2) return(as.numeric(obj[2]))
  NA_real_
}

get_statistic <- function(obj) {
  if (is.list(obj) && !is.null(obj$statistic)) return(as.numeric(obj$statistic)[1])
  if (is.list(obj) && !is.null(obj$statistika)) return(as.numeric(obj$statistika))
  if (is.numeric(obj) && length(obj) >= 1) return(as.numeric(obj[1]))
  NA_real_
}

get_delta_hat <- function(obj) {
  if (is.list(obj) && !is.null(obj$Delta_hat)) return(as.numeric(obj$Delta_hat))
  if (is.list(obj) && !is.null(obj$estimate)) {
    est <- obj$estimate
    if ("Delta_hat" %in% names(est)) return(as.numeric(est["Delta_hat"]))
  }
  NA_real_
}

acf1_centered <- function(z) {
  z <- as.numeric(z)
  z <- z[is.finite(z)]
  z <- z - mean(z)
  
  sum(z[-1] * z[-length(z)]) / sum(z[-length(z)]^2)
}

shape_stats <- function(z) {
  z <- as.numeric(z)
  z <- z[is.finite(z)]
  
  m <- mean(z)
  s <- stats::sd(z)
  
  skew <- mean((z - m)^3) / s^3
  exkurt <- mean((z - m)^4) / s^4 - 3
  
  c(skewness = skew, excess_kurtosis = exkurt)
}

gph_d <- function(x, m = NULL) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  n <- length(x)
  
  if (is.null(m)) {
    m <- floor(sqrt(n))
  }
  
  m <- max(10, min(m, floor(n / 2) - 1))
  
  x <- x - mean(x)
  fftx <- fft(x)
  
  I <- (Mod(fftx[2:(m + 1)])^2) / (2 * pi * n)
  j <- seq_len(m)
  lambda <- 2 * pi * j / n
  
  y <- log(I)
  X <- -2 * log(2 * sin(lambda / 2))
  
  fit <- stats::lm(y ~ X)
  
  list(
    d = as.numeric(stats::coef(fit)[["X"]]),
    se = summary(fit)$coef["X", "Std. Error"],
    m = m,
    n = n
  )
}


# ==========================================================
# 3. FDEL wrappers
# ==========================================================
#
# Different versions of FDEL.acf have used different argument names:
#   - X, Y, bootstrap.samples
#   - x, y, B
#
# The wrappers below try both forms.

call_fdel_acf <- function(x, y, Delta = 0, lag = 1,
                          bartlett = FALSE,
                          bootstrap.samples = 500,
                          seed = NULL,
                          center = TRUE) {
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # First try the X/Y and bootstrap.samples convention.
  out <- tryCatch(
    EL::FDEL.acf(
      X = x,
      Y = y,
      Delta = Delta,
      lag = lag,
      bartlett = bartlett,
      bootstrap.samples = bootstrap.samples,
      center = center,
      seed = seed
    ),
    error = function(e1) NULL
  )
  
  if (!is.null(out)) return(out)
  
  # Then try the x/y and B convention.
  out <- tryCatch(
    EL::FDEL.acf(
      x = x,
      y = y,
      Delta = Delta,
      lag = lag,
      bartlett = bartlett,
      B = bootstrap.samples,
      center = center,
      seed = seed
    ),
    error = function(e2) NULL
  )
  
  if (is.null(out)) {
    stop("FDEL.acf could not be evaluated. Check the EL package version and function arguments.")
  }
  
  out
}

fdel_statistic <- function(x, y, Delta = 0, bartlett = FALSE,
                           bootstrap.samples = BOOTSTRAP_SAMPLES,
                           seed = BOOTSTRAP_SEED) {
  
  out <- call_fdel_acf(
    x = x,
    y = y,
    Delta = Delta,
    lag = LAG,
    bartlett = bartlett,
    bootstrap.samples = bootstrap.samples,
    seed = seed,
    center = TRUE
  )
  
  get_statistic(out)
}

fdel_p_value <- function(x, y, Delta = 0, bartlett = FALSE,
                         bootstrap.samples = BOOTSTRAP_SAMPLES,
                         seed = BOOTSTRAP_SEED) {
  
  out <- call_fdel_acf(
    x = x,
    y = y,
    Delta = Delta,
    lag = LAG,
    bartlett = bartlett,
    bootstrap.samples = bootstrap.samples,
    seed = seed,
    center = TRUE
  )
  
  get_p_value(out)
}


# ==========================================================
# 4. BEL wrapper
# ==========================================================

bel_statistic <- function(x, y, M1, M2, Delta = 0) {
  
  out <- BEL.rho_diff(
    X = x,
    Y = y,
    M1 = M1,
    M2 = M2,
    Delta0 = Delta
  )
  
  get_statistic(out)
}

bel_p_value <- function(x, y, M1, M2, Delta = 0) {
  
  out <- BEL.rho_diff(
    X = x,
    Y = y,
    M1 = M1,
    M2 = M2,
    Delta0 = Delta
  )
  
  get_p_value(out)
}


# ==========================================================
# 5. Confidence interval by inversion
# ==========================================================

invert_chisq_test_ci <- function(stat_fun,
                                 center,
                                 lower_bound = CI_LOWER,
                                 upper_bound = CI_UPPER,
                                 step = CI_STEP,
                                 level = CI_LEVEL) {
  
  crit <- stats::qchisq(level, df = 1)
  
  accept <- function(delta0) {
    val <- suppressWarnings(stat_fun(delta0))
    is.finite(val) && !is.na(val) && val <= crit
  }
  
  # If the initial center is not accepted, find the nearest accepted grid point.
  if (!accept(center)) {
    grid <- seq(lower_bound, upper_bound, by = step)
    acc <- vapply(grid, accept, logical(1))
    
    if (!any(acc)) {
      return(list(
        ci = c(lower_bound, upper_bound),
        lower_truncated = TRUE,
        upper_truncated = TRUE
      ))
    }
    
    center <- grid[which.min(abs(grid[acc] - center))]
  }
  
  # Search to the left.
  left_acc <- center
  left_rej <- NA_real_
  
  for (cand in seq(center, lower_bound, by = -step)) {
    if (accept(cand)) {
      left_acc <- cand
    } else {
      left_rej <- cand
      break
    }
  }
  
  # Search to the right.
  right_acc <- center
  right_rej <- NA_real_
  
  for (cand in seq(center, upper_bound, by = step)) {
    if (accept(cand)) {
      right_acc <- cand
    } else {
      right_rej <- cand
      break
    }
  }
  
  lower <- left_acc
  lower_truncated <- FALSE
  
  if (is.finite(left_rej)) {
    f_left <- function(d0) stat_fun(d0) - crit
    lower <- tryCatch(
      stats::uniroot(f_left, c(left_rej, left_acc))$root,
      error = function(e) left_acc
    )
  } else {
    lower <- lower_bound
    lower_truncated <- TRUE
  }
  
  upper <- right_acc
  upper_truncated <- FALSE
  
  if (is.finite(right_rej)) {
    f_right <- function(d0) stat_fun(d0) - crit
    upper <- tryCatch(
      stats::uniroot(f_right, c(right_acc, right_rej))$root,
      error = function(e) right_acc
    )
  } else {
    upper <- upper_bound
    upper_truncated <- TRUE
  }
  
  list(
    ci = c(lower, upper),
    lower_truncated = lower_truncated,
    upper_truncated = upper_truncated
  )
}


# ==========================================================
# 6. Data retrieval and return construction
# ==========================================================

ticker <- "^N225"
from_date <- "2017-01-01"
to_date <- "2021-12-31"

px <- suppressWarnings(
  quantmod::getSymbols(
    Symbols = ticker,
    src = "yahoo",
    from = from_date,
    to = to_date,
    auto.assign = FALSE
  )
)

price <- tryCatch(quantmod::Ad(px), error = function(e) quantmod::Cl(px))
colnames(price) <- "price"

# Daily log returns.
r <- stats::na.omit(diff(log(price)))
colnames(r) <- "log_return"

# Squared daily log returns.
r2 <- r^2
colnames(r2) <- "sq_log_return"

saveRDS(price, "outputs/data/nikkei225_prices.rds")
saveRDS(r2, "outputs/data/nikkei225_squared_log_returns.rds")


# ==========================================================
# 7. Main and robustness splits
# ==========================================================

# Main split used in the manuscript.
r2_pre_main <- r2["2017-01-01/2020-02-28"]
r2_covid_main <- r2["2020-05-01/2021-12-31"]

x_main <- as.numeric(stats::na.omit(r2_pre_main))
y_main <- as.numeric(stats::na.omit(r2_covid_main))

# Alternative cutoff robustness split.
r2_pre_rob <- r2["2017-01-01/2020-01-31"]
r2_covid_rob <- r2["2020-06-01/2021-12-31"]

x_rob <- as.numeric(stats::na.omit(r2_pre_rob))
y_rob <- as.numeric(stats::na.omit(r2_covid_rob))


# ==========================================================
# 8. Descriptive statistics
# ==========================================================

make_descriptives <- function(x, y, split_name) {
  
  gph_x <- gph_d(x)
  gph_y <- gph_d(y)
  
  shape_x <- shape_stats(x)
  shape_y <- shape_stats(y)
  
  data.frame(
    split = split_name,
    period = c("pre-COVID", "COVID"),
    n = c(length(x), length(y)),
    rho1_centered = c(acf1_centered(x), acf1_centered(y)),
    d_gph = c(gph_x$d, gph_y$d),
    se_d_gph = c(gph_x$se, gph_y$se),
    gph_bandwidth = c(gph_x$m, gph_y$m),
    skewness = c(shape_x["skewness"], shape_y["skewness"]),
    excess_kurtosis = c(shape_x["excess_kurtosis"], shape_y["excess_kurtosis"]),
    stringsAsFactors = FALSE
  )
}

desc_main <- make_descriptives(x_main, y_main, "Main split")
desc_rob <- make_descriptives(x_rob, y_rob, "Robustness split")

descriptives <- dplyr::bind_rows(desc_main, desc_rob)

write.csv(
  descriptives,
  "outputs/tables/nikkei_descriptives.csv",
  row.names = FALSE
)

print(descriptives)


# ==========================================================
# 9. Run fixed-regime application tests
# ==========================================================

summarise_application_tests <- function(x, y,
                                        bel_blocks,
                                        include_bfdel = TRUE,
                                        label = "Main split") {
  
  results <- list()
  
  # --------------------------------------------------------
  # FDEL
  # --------------------------------------------------------
  
  fdel_stat_fun <- function(delta0) {
    fdel_statistic(
      x = x,
      y = y,
      Delta = delta0,
      bartlett = FALSE
    )
  }
  
  opt_fdel <- stats::optimize(
    fdel_stat_fun,
    lower = CI_LOWER,
    upper = CI_UPPER
  )
  
  delta_fdel <- opt_fdel$minimum
  ci_fdel <- invert_chisq_test_ci(fdel_stat_fun, center = delta_fdel)
  
  fdel0 <- call_fdel_acf(
    x = x,
    y = y,
    Delta = DELTA0,
    lag = LAG,
    bartlett = FALSE,
    center = TRUE
  )
  
  results[["FDEL"]] <- data.frame(
    split = label,
    Method = "FDEL",
    Statistic = get_statistic(fdel0),
    Delta_hat = delta_fdel,
    p_value = get_p_value(fdel0),
    CI_lower = ci_fdel$ci[1],
    CI_upper = ci_fdel$ci[2],
    stringsAsFactors = FALSE
  )
  
  # --------------------------------------------------------
  # Bootstrap Bartlett-type corrected FDEL
  # --------------------------------------------------------
  
  if (include_bfdel) {
    
    bfdel_stat_fun <- function(delta0) {
      fdel_statistic(
        x = x,
        y = y,
        Delta = delta0,
        bartlett = TRUE,
        bootstrap.samples = BOOTSTRAP_SAMPLES,
        seed = BOOTSTRAP_SEED
      )
    }
    
    opt_bfdel <- stats::optimize(
      bfdel_stat_fun,
      lower = CI_LOWER,
      upper = CI_UPPER
    )
    
    delta_bfdel <- opt_bfdel$minimum
    
    if (COMPUTE_BFDEL_CI) {
      ci_bfdel <- invert_chisq_test_ci(bfdel_stat_fun, center = delta_bfdel)
      ci_bfdel_values <- ci_bfdel$ci
    } else {
      ci_bfdel_values <- c(NA_real_, NA_real_)
    }
    
    bfdel0 <- call_fdel_acf(
      x = x,
      y = y,
      Delta = DELTA0,
      lag = LAG,
      bartlett = TRUE,
      bootstrap.samples = BOOTSTRAP_SAMPLES,
      seed = BOOTSTRAP_SEED,
      center = TRUE
    )
    
    results[["BFDEL"]] <- data.frame(
      split = label,
      Method = "Bootstrap Bartlett-type corrected FDEL",
      Statistic = get_statistic(bfdel0),
      Delta_hat = delta_bfdel,
      p_value = get_p_value(bfdel0),
      CI_lower = ci_bfdel_values[1],
      CI_upper = ci_bfdel_values[2],
      stringsAsFactors = FALSE
    )
  }
  
  # --------------------------------------------------------
  # AR(1)-Wald
  # --------------------------------------------------------
  
  ar1 <- ar1_wald_rho1_diff(
    X = x,
    Y = y,
    Delta0 = DELTA0,
    demean = TRUE
  )
  
  ar1_ci <- if (!is.null(ar1$conf.int)) {
    ar1$conf.int
  } else if (!is.null(ar1$ci)) {
    ar1$ci
  } else {
    delta_hat <- get_delta_hat(ar1)
    se <- ar1$se
    delta_hat + c(-1, 1) * stats::qnorm(0.975) * se
  }
  
  results[["AR1_Wald"]] <- data.frame(
    split = label,
    Method = "Wald test (AR(1) variance, centered)",
    Statistic = get_statistic(ar1),
    Delta_hat = get_delta_hat(ar1),
    p_value = get_p_value(ar1),
    CI_lower = ar1_ci[1],
    CI_upper = ar1_ci[2],
    stringsAsFactors = FALSE
  )
  
  # --------------------------------------------------------
  # HAC-Wald
  # --------------------------------------------------------
  
  hac <- hac_wald_rho1_diff(
    X = x,
    Y = y,
    Delta0 = DELTA0,
    demean = TRUE
  )
  
  hac_delta <- get_delta_hat(hac)
  hac_ci <- if (!is.null(hac$conf.int)) {
    hac$conf.int
  } else if (!is.null(hac$ci)) {
    hac$ci
  } else {
    hac_delta + c(-1, 1) * stats::qnorm(0.975) * hac$se
  }
  
  results[["HAC_Wald"]] <- data.frame(
    split = label,
    Method = "Wald test (HAC variance)",
    Statistic = get_statistic(hac),
    Delta_hat = hac_delta,
    p_value = get_p_value(hac),
    CI_lower = hac_ci[1],
    CI_upper = hac_ci[2],
    stringsAsFactors = FALSE
  )
  
  # --------------------------------------------------------
  # BEL block choices
  # --------------------------------------------------------
  
  for (i in seq_len(nrow(bel_blocks))) {
    
    M1 <- bel_blocks$M1[i]
    M2 <- bel_blocks$M2[i]
    block_label <- bel_blocks$label[i]
    
    bel_stat_fun <- function(delta0) {
      bel_statistic(
        x = x,
        y = y,
        M1 = M1,
        M2 = M2,
        Delta = delta0
      )
    }
    
    opt_bel <- stats::optimize(
      f = function(delta0) {
        val <- suppressWarnings(bel_stat_fun(delta0))
        if (!is.finite(val) || is.na(val)) 1e12 else val
      },
      lower = CI_LOWER,
      upper = CI_UPPER
    )
    
    delta_bel <- opt_bel$minimum
    ci_bel <- invert_chisq_test_ci(bel_stat_fun, center = delta_bel)
    
    bel0 <- BEL.rho_diff(
      X = x,
      Y = y,
      M1 = M1,
      M2 = M2,
      Delta0 = DELTA0
    )
    
    results[[paste0("BEL_", M1, "_", M2)]] <- data.frame(
      split = label,
      Method = paste0("BEL (M = (", M1, ",", M2, "))", block_label),
      Statistic = get_statistic(bel0),
      Delta_hat = delta_bel,
      p_value = get_p_value(bel0),
      CI_lower = ci_bel$ci[1],
      CI_upper = ci_bel$ci[2],
      stringsAsFactors = FALSE
    )
  }
  
  dplyr::bind_rows(results)
}


# Main application block choices.
main_blocks <- data.frame(
  M1 = c(2, 5, 10, 15, 20, 4, 13, 27),
  M2 = c(2, 5, 10, 15, 20, 3, 11, 22),
  label = c("", "", "", "", "", "^*", "^*", "^*"),
  stringsAsFactors = FALSE
)

# Robustness block choices:
# c * floor(n^(1/3)), with c in {1, 1.5}.
rob_blocks <- data.frame(
  M1 = c(floor(length(x_rob)^(1 / 3)),
         floor(1.5 * floor(length(x_rob)^(1 / 3)))),
  M2 = c(floor(length(y_rob)^(1 / 3)),
         floor(1.5 * floor(length(y_rob)^(1 / 3)))),
  label = c("^*", "^*"),
  stringsAsFactors = FALSE
)

main_results <- summarise_application_tests(
  x = x_main,
  y = y_main,
  bel_blocks = main_blocks,
  include_bfdel = TRUE,
  label = "Main split"
)

robustness_results <- summarise_application_tests(
  x = x_rob,
  y = y_rob,
  bel_blocks = rob_blocks,
  include_bfdel = TRUE,
  label = "Robustness split"
)

write.csv(
  main_results,
  "outputs/tables/nikkei_main_application_results.csv",
  row.names = FALSE
)

write.csv(
  robustness_results,
  "outputs/tables/nikkei_robustness_results.csv",
  row.names = FALSE
)

print(main_results)
print(robustness_results)


# ==========================================================
# 10. Approximate-separation diagnostic
# ==========================================================

ret_dates <- as.Date(index(r2))

pre_end <- as.Date("2020-02-28")
covid_start <- as.Date("2020-05-01")

pre_last_idx <- max(which(ret_dates <= pre_end))
covid_first_idx <- min(which(ret_dates >= covid_start))

gap_lag <- covid_first_idx - pre_last_idx

acf_full <- stats::acf(
  as.numeric(r2),
  lag.max = gap_lag + 10,
  plot = FALSE
)

acf_vals <- as.numeric(acf_full$acf)[-1]

gap_window <- (gap_lag - 3):(gap_lag + 3)
gap_window <- gap_window[gap_window >= 1]

gap_acf_table <- data.frame(
  lag = gap_window,
  acf = acf_vals[gap_window]
)

write.csv(
  gap_acf_table,
  "outputs/tables/nikkei_gap_acf_diagnostic.csv",
  row.names = FALSE
)

cat("\nEffective gap lag:", gap_lag, "\n")
print(gap_acf_table)


# ==========================================================
# 11. Rolling-window diagnostics
# ==========================================================

run_one_rolling_window <- function(values, dates, boundary_idx, L) {
  
  left_idx <- (boundary_idx - L + 1):boundary_idx
  right_idx <- (boundary_idx + 1):(boundary_idx + L)
  
  x_left <- values[left_idx]
  y_right <- values[right_idx]
  
  boundary_date <- dates[boundary_idx]
  
  rho_left <- acf1_centered(x_left)
  rho_right <- acf1_centered(y_right)
  delta_hat <- rho_right - rho_left
  
  fdel <- tryCatch(
    call_fdel_acf(
      x = x_left,
      y = y_right,
      Delta = 0,
      lag = LAG,
      bartlett = FALSE,
      center = TRUE
    ),
    error = function(e) NULL
  )
  
  hac <- tryCatch(
    hac_wald_rho1_diff(
      X = x_left,
      Y = y_right,
      Delta0 = 0,
      demean = TRUE
    ),
    error = function(e) NULL
  )
  
  fdel_p <- if (is.null(fdel)) NA_real_ else get_p_value(fdel)
  hac_p <- if (is.null(hac)) NA_real_ else get_p_value(hac)
  
  data.frame(
    boundary_index = boundary_idx,
    boundary_date = boundary_date,
    L = L,
    rho_left = rho_left,
    rho_right = rho_right,
    Delta_hat = delta_hat,
    FDEL_p_value = fdel_p,
    HAC_p_value = hac_p,
    FDEL_reject = is.finite(fdel_p) && fdel_p < ALPHA,
    HAC_reject = is.finite(hac_p) && hac_p < ALPHA,
    stringsAsFactors = FALSE
  )
}


run_rolling_diagnostics <- function(r2_xts,
                                    window_lengths = WINDOW_LENGTHS,
                                    step = ROLLING_STEP) {
  
  values <- as.numeric(r2_xts)
  dates <- as.Date(index(r2_xts))
  N <- length(values)
  
  out <- list()
  counter <- 1
  
  for (L in window_lengths) {
    
    boundary_indices <- seq(
      from = L,
      to = N - L,
      by = step
    )
    
    for (idx in boundary_indices) {
      
      out[[counter]] <- run_one_rolling_window(
        values = values,
        dates = dates,
        boundary_idx = idx,
        L = L
      )
      
      counter <- counter + 1
    }
  }
  
  dplyr::bind_rows(out)
}

rolling_results <- run_rolling_diagnostics(
  r2_xts = r2,
  window_lengths = WINDOW_LENGTHS,
  step = ROLLING_STEP
)

rolling_results <- rolling_results %>%
  mutate(
    decision = dplyr::case_when(
      FDEL_reject & HAC_reject ~ "Both reject",
      FDEL_reject & !HAC_reject ~ "FDEL only",
      !FDEL_reject & HAC_reject ~ "HAC-Wald only",
      TRUE ~ "Neither rejects"
    ),
    L_factor = factor(L, levels = WINDOW_LENGTHS)
  )

write.csv(
  rolling_results,
  "outputs/tables/nikkei_rolling_window_results.csv",
  row.names = FALSE
)


# ==========================================================
# 12. Figure: rolling FDEL p-value curves
# ==========================================================

period_rectangles <- data.frame(
  xmin = as.Date(c("2017-01-01", "2020-03-01", "2020-05-01")),
  xmax = as.Date(c("2020-02-29", "2020-04-30", "2021-12-31")),
  period = c("Pre-COVID", "Transition", "COVID")
)

p_value_curve <- ggplot(rolling_results, aes(x = boundary_date, y = FDEL_p_value)) +
  geom_rect(
    data = period_rectangles,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = period),
    inherit.aes = FALSE,
    alpha = 0.12
  ) +
  geom_hline(yintercept = ALPHA, linetype = "dashed") +
  geom_line(linewidth = 0.35, na.rm = TRUE) +
  facet_wrap(~ L_factor, ncol = 2, labeller = label_both) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = "Boundary date",
    y = "FDEL p-value",
    fill = "Period"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "grey90"),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = "outputs/figures/p_value_curve.pdf",
  plot = p_value_curve,
  width = 8.5,
  height = 10
)

ggsave(
  filename = "outputs/figures/p_value_curve.png",
  plot = p_value_curve,
  width = 8.5,
  height = 10,
  dpi = 300
)


# ==========================================================
# 13. Figure: rolling heatmap diagnostics
# ==========================================================

heat_delta <- ggplot(
  rolling_results,
  aes(x = boundary_date, y = L, fill = Delta_hat)
) +
  geom_tile() +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-01")), linewidth = 0.4) +
  geom_vline(xintercept = as.numeric(as.Date("2020-05-01")), linewidth = 0.4) +
  scale_y_continuous(breaks = WINDOW_LENGTHS) +
  labs(
    x = NULL,
    y = "Window length L",
    fill = expression(hat(Delta)(t)),
    title = "(a)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    legend.position = "right"
  )

decision_levels <- c(
  "Neither rejects",
  "Both reject",
  "FDEL only",
  "HAC-Wald only"
)

rolling_results$decision <- factor(
  rolling_results$decision,
  levels = decision_levels
)

heat_decision <- ggplot(
  rolling_results,
  aes(x = boundary_date, y = L, fill = decision)
) +
  geom_tile() +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-01")), linewidth = 0.4) +
  geom_vline(xintercept = as.numeric(as.Date("2020-05-01")), linewidth = 0.4) +
  scale_y_continuous(breaks = WINDOW_LENGTHS) +
  labs(
    x = "Boundary date",
    y = "Window length L",
    fill = NULL,
    title = "(b)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

heatmap_figure <- heat_delta / heat_decision +
  patchwork::plot_layout(heights = c(1, 1))

ggsave(
  filename = "outputs/figures/Heatmap.pdf",
  plot = heatmap_figure,
  width = 8.5,
  height = 7
)

ggsave(
  filename = "outputs/figures/Heatmap.png",
  plot = heatmap_figure,
  width = 8.5,
  height = 7,
  dpi = 300
)


# ==========================================================
# 14. Print the results reported in manuscript 
# ==========================================================

print(desc_main)

print(desc_rob)

print(main_results)

print(robustness_results)

