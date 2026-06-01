
# AR(1)-Wald test for the difference in lag-1 autocorrelations


ar1_wald_rho1_diff <- function(X, Y, Delta0 = 0, demean = TRUE,
                               conf.level = 0.95) {
  
  X <- as.numeric(X)
  Y <- as.numeric(Y)
  


  if (demean) {
    X <- X - mean(X)
    Y <- Y - mean(Y)
  }
  
  n <- length(X)
  m <- length(Y)
  

  
  fitX <- stats::arima(
    X,
    order = c(1, 0, 0),
    include.mean = FALSE
  )
  
  fitY <- stats::arima(
    Y,
    order = c(1, 0, 0),
    include.mean = FALSE
  )
  
  phiX_hat <- as.numeric(fitX$coef["ar1"])
  phiY_hat <- as.numeric(fitY$coef["ar1"])
  

  
  var_phiX <- (1 - phiX_hat^2) / (n - 1)
  var_phiY <- (1 - phiY_hat^2) / (m - 1)
  

  
  Delta_hat <- phiY_hat - phiX_hat
  se_delta <- sqrt(var_phiX + var_phiY)
  
  if (!is.finite(se_delta) || se_delta <= 0) {
    stop("Estimated standard error is non-positive or not finite.")
  }
  
  Z <- (Delta_hat - Delta0) / se_delta
  Chi2 <- Z^2
  p_value <- 1 - stats::pchisq(Chi2, df = 1)
  
  # Confidence interval for Delta.
  alpha <- 1 - conf.level
  zcrit <- stats::qnorm(1 - alpha / 2)
  
  ci <- c(
    Delta_hat - zcrit * se_delta,
    Delta_hat + zcrit * se_delta
  )
  
  names(ci) <- c("lower", "upper")
  
  list(
    statistic = Chi2,
    p.value = p_value,
    estimate = c(
      rhoX = phiX_hat,
      rhoY = phiY_hat,
      Delta_hat = Delta_hat
    ),
    null.value = c(Delta0 = Delta0),
    se = se_delta,
    conf.int = ci,
    conf.level = conf.level,
    method = "AR(1)-Wald test for the difference in lag-1 autocorrelations",
    
    p = p_value,
    statistika = Chi2,
    Delta_hat = Delta_hat,
    ci = ci
  )
}



