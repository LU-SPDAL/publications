# Blockwise empirical likelihood for autocorrelation difference

# Dependencies: nleqslv

BEL.rho_diff <- function(X, Y, M1, M2, Delta0 = 0,
                                  rho.lower = -0.999,
                                  rho.upper =  0.999,
                                  trace = FALSE) {
  
  if (!requireNamespace("nleqslv", quietly = TRUE)) {
    stop("Package 'nleqslv' is required. Please install it first.")
  }
  
  
  X <- as.numeric(X)
  Y <- as.numeric(Y)

  
  # The manuscript uses centered series throughout
  X <- X - mean(X)
  Y <- Y - mean(Y)
  
  
  block_moments_series <- function(z, M, rho_eff) {
    
    N <- length(z)
    Q <- floor((N - M) / M) + 1
    
    starts <- seq(1, by = M, length.out = Q)
    ends <- pmin(starts + M - 1, N)
    
    vapply(seq_len(Q), function(i) {
      
      s <- starts[i]
      e <- ends[i]
      
      if (e <= s) return(NA_real_)
      
      t_idx <- (s + 1):e
      
      mean(z[t_idx] * z[t_idx - 1] - rho_eff * z[t_idx - 1]^2)
      
    }, numeric(1))
  }
  
  # ----------------------------------------------------------
  # Scalar convex-hull  check
  # ----------------------------------------------------------

  is_feasible_scalar <- function(G) {
    all(is.finite(G)) && min(G) < 0 && max(G) > 0
  }
  
  # Lagrange multiplier solver

  
  solve_lambda <- function(G, lambda_start = 0) {
    
    if (!all(is.finite(G))) {
      return(list(converged = FALSE, lambda = NA_real_,
                  reason = "nonfinite_moments"))
    }
    
    if (!is_feasible_scalar(G)) {
      return(list(converged = FALSE, lambda = NA_real_,
                  reason = "convex_hull_failure"))
    }
    
    lambda_fun <- function(lambda) {
      denom <- 1 + lambda * G
      
      if (any(denom <= 0)) {
        return(1e6)
      }
      
      sum(G / denom)
    }
    
    sol <- tryCatch(
      nleqslv::nleqslv(
        lambda_start,
        lambda_fun,
        control = list(
          ftol = 1e-10,
          xtol = 1e-10,
          maxit = 200,
          trace = 0
        )
      ),
      error = function(e) NULL
    )
    
    if (is.null(sol)) {
      return(list(converged = FALSE, lambda = NA_real_,
                  reason = "solver_error"))
    }
    
    denom <- 1 + sol$x * G
    
    if (any(!is.finite(denom)) || any(denom <= 0)) {
      return(list(converged = FALSE, lambda = sol$x,
                  reason = "nonpositive_denominator"))
    }
    
    list(
      lambda = sol$x,
      converged = sol$termcd %in% c(1, 2, 3),
      fnorm = abs(lambda_fun(sol$x)),
      reason = if (sol$termcd %in% c(1, 2, 3)) {
        "ok"
      } else {
        "solver_nonconvergence"
      }
    )
  }
  
  # ----------------------------------------------------------
  # Profile objective as a function of rho
  # ----------------------------------------------------------

  
  elr_objective <- function(rho) {
    
    if (!is.finite(rho) || rho <= rho.lower || rho >= rho.upper) {
      return(1e12)
    }
    
    GX <- block_moments_series(X, M1, rho_eff = rho)
    GY <- block_moments_series(Y, M2, rho_eff = rho + Delta0)
    
    solX <- solve_lambda(GX, 0)
    solY <- solve_lambda(GY, 0)
    
    if (!solX$converged || !solY$converged) {
      return(1e11)
    }
    
    denomX <- 1 + solX$lambda * GX
    denomY <- 1 + solY$lambda * GY
    
    if (any(denomX <= 0) || any(denomY <= 0)) {
      return(1e10)
    }
    
    elr <- 2 * (sum(log(denomX)) + sum(log(denomY)))
    
    if (trace) {
      cat(sprintf("rho = %.4f | BEL statistic = %.4f\n", rho, elr))
    }
    
    if (!is.finite(elr)) {
      return(1e10)
    }
    
    elr
  }
  
  # ----------------------------------------------------------
  # One-dimensional profiling over rho
  # ----------------------------------------------------------
  
  opt <- tryCatch(
    optimize(
      f = elr_objective,
      lower = rho.lower + 1e-6,
      upper = rho.upper - 1e-6
    ),
    error = function(e) NULL
  )
  
  if (is.null(opt)) {
    return(list(
      statistic = NA_real_,
      p.value = NA_real_,
      estimate = c(rho_hat = NA_real_),
      null.value = c(Delta0 = Delta0),
      block.lengths = c(M1 = M1, M2 = M2),
      method = "Blockwise empirical likelihood test for autocorrelation difference",
      convergence = "optimizer_error"
    ))
  }
  
  ELR <- elr_objective(opt$minimum)
  pval <- 1 - stats::pchisq(ELR, df = 1)
  
  
  out <- list(
    statistic = ELR,
    p.value = pval,
    estimate = c(rho_hat = opt$minimum),
    null.value = c(Delta0 = Delta0),
    block.lengths = c(M1 = M1, M2 = M2),
    method = "Blockwise empirical likelihood test for autocorrelation difference",
    convergence = if (ELR >= 1e10) "penalty_or_infeasible" else "ok",
    
    ELR = ELR,
    pval = pval
  )
  
  return(out)
}


