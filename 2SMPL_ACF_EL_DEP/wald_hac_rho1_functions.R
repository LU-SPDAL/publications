
# HAC-Wald test for the difference in lag-1 autocorrelations

hac_wald_rho1_diff <- function(X, Y, Delta0 = 0, demean = TRUE, bw = NULL) {
  

  X <- as.numeric(X)
  Y <- as.numeric(Y)
  

  if (demean) {
    X <- X - mean(X)
    Y <- Y - mean(Y)
  }
  

  nX <- length(X)
  nY <- length(Y)
  
  
  TX <- nX - 1L
  TY <- nY - 1L
  


  
  numX <- sum(X[2:nX] * X[1:(nX - 1)])
  denX <- sum(X[1:(nX - 1)]^2)
  rhoX <- numX / denX
  
  numY <- sum(Y[2:nY] * Y[1:(nY - 1)])
  denY <- sum(Y[1:(nY - 1)]^2)
  rhoY <- numY / denY
  

  
  mX <- X[2:nX] * X[1:(nX - 1)] - rhoX * X[1:(nX - 1)]^2
  mY <- Y[2:nY] * Y[1:(nY - 1)] - rhoY * Y[1:(nY - 1)]^2
  

  
  hac_scalar <- function(m, Tn, bw = NULL) {
    
    if (is.null(bw)) {
      bw <- floor(4 * (Tn / 100)^(2 / 9))
    }
    
    bw <- max(0L, as.integer(bw))

    S <- mean(m^2)
    
    # Add weighted autocovariances.
    if (bw >= 1L) {
      for (k in seq_len(bw)) {
        w <- 1 - k / (bw + 1)
        S <- S + 2 * w * mean(m[(k + 1):Tn] * m[1:(Tn - k)])
      }
    }
    
    list(S = S, bw = bw)
  }
  
  HX <- hac_scalar(mX, TX, bw)
  HY <- hac_scalar(mY, TY, bw)
  
  Sx <- HX$S
  Sy <- HY$S
  
  # Report the larger of the two bandwidths if the automatic rule is used.
  bw_used <- max(HX$bw, HY$bw)
  

  
  Gx <- -mean(X[1:(nX - 1)]^2)
  Gy <- -mean(Y[1:(nY - 1)]^2)
  

  # Asymptotic variance of the difference

  
  Vhat <- Sy / (Gy^2) / TY + Sx / (Gx^2) / TX
  
  if (!is.finite(Vhat) || Vhat <= 0) {
    stop("Estimated variance is non-positive or not finite.")
  }
  
  
  Delta_hat <- rhoY - rhoX
  se_delta <- sqrt(Vhat)
  
  Z <- (Delta_hat - Delta0) / se_delta
  Chi2 <- Z^2
  p_value <- 1 - pchisq(Chi2, df = 1)
  

  list(
    statistic = Chi2,
    p.value = p_value,
    estimate = c(
      rhoX = rhoX,
      rhoY = rhoY,
      Delta_hat = Delta_hat
    ),
    null.value = c(Delta0 = Delta0),
    se = se_delta,
    bandwidth = bw_used,
    method = "HAC-Wald test for the difference in lag-1 autocorrelations"
  )
}
