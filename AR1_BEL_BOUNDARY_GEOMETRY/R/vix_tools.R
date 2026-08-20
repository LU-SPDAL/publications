# Utilities for the VIX application

read_monthly_vix <- function(path = file.path("data", "raw", "VIXCLS_FRED.csv"),
                             start = as.Date("1990-01-01"),
                             end = as.Date("2024-12-31")) {
  raw <- read.csv(path, na.strings = c(".", "NA", ""), check.names = FALSE)
  date_name <- names(raw)[grepl("date", names(raw), ignore.case = TRUE)][1]
  if (is.na(date_name)) date_name <- names(raw)[1]
  value_name <- if ("VIXCLS" %in% names(raw)) "VIXCLS" else names(raw)[2]

  daily <- data.frame(
    date = as.Date(raw[[date_name]]),
    vix = as.numeric(raw[[value_name]])
  )
  daily <- daily[
    daily$date >= start & daily$date <= end & is.finite(daily$vix) & daily$vix > 0,
  ]

  daily$month <- format(daily$date, "%Y-%m")
  monthly <- aggregate(vix ~ month, daily, mean)
  monthly$date <- as.Date(paste0(monthly$month, "-01"))
  monthly <- monthly[order(monthly$date), ]
  monthly$log_vix <- log(monthly$vix)
  monthly
}

ar_bic_table <- function(x, pmax = 12L) {
  rows <- lapply(0:pmax, function(p) {
    fit <- arima(x, order = c(p, 0, 0), include.mean = TRUE, method = "ML")
    data.frame(p = p, logLik = as.numeric(logLik(fit)), AIC = AIC(fit), BIC = BIC(fit))
  })
  do.call(rbind, rows)
}

additional_ar_lags_test <- function(x, max_lag = 4L) {
  E <- embed(x, max_lag + 1L)
  d <- data.frame(y = E[, 1L], E[, -1L, drop = FALSE])
  names(d)[-1] <- paste0("lag", seq_len(max_lag))

  fit1 <- lm(y ~ lag1, d)
  fit4 <- lm(y ~ lag1 + lag2 + lag3 + lag4, d)
  a <- anova(fit1, fit4)

  c(F = a$F[2], p_value = a$`Pr(>F)`[2])
}

arch_lm_test <- function(e, lags = 12L) {
  E <- embed(e^2, lags + 1L)
  fit <- lm(E[, 1L] ~ E[, -1L, drop = FALSE])
  LM <- nrow(E) * summary(fit)$r.squared
  c(statistic = LM, p_value = pchisq(LM, lags, lower.tail = FALSE))
}

cusum_ar1_test <- function(x) {
  d <- data.frame(y = x[-1], lag1 = x[-length(x)])
  process <- strucchange::efp(y ~ lag1, data = d, type = "Rec-CUSUM")
  test <- strucchange::sctest(process)
  c(statistic = unname(test$statistic), p_value = test$p.value)
}

self_normalizer <- function(x) {
  n <- length(x)
  partial <- cumsum(x - mean(x))
  bridge <- partial - seq_len(n) / n * partial[n]
  sum(bridge^2) / n^2
}

simulate_sn_limit <- function(reps = 250000L, grid = 1000L, seed = 812028L,
                              chunk = 1000L) {
  set.seed(seed)
  out <- numeric(reps)
  r <- seq_len(grid) / grid
  start <- 1L

  while (start <= reps) {
    b <- min(chunk, reps - start + 1L)
    B <- matrix(rnorm(b * grid), nrow = b)

    for (j in 2:grid) B[, j] <- B[, j] + B[, j - 1L]

    B1 <- B[, grid]
    bridge <- B - B1 * rep(r, each = b)
    out[start:(start + b - 1L)] <- B1^2 / rowMeans(bridge^2)
    start <- start + b
  }

  out
}
