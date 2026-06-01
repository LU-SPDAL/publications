############################################################
# Empirical size simulations
# Two-sample tests for equality of lag-1 autocorrelations
############################################################
#
# This script reproduces the empirical size simulations reported
# in the manuscript.
#
# The null hypothesis is
#
#   H0: Delta = rho_Y(1) - rho_X(1) = 0.
#
# Both series are generated from the same model parameters, so the
# null hypothesis is true in every replication.
#
# Methods:
#   - FDEL, using FDEL.acf() from the EL package
#   - AR(1)-Wald benchmark
#   - HAC-Wald benchmark
#   - BEL with fixed block lengths M in {2, 5, 10, 15, 20}
#
# Output:
#   - empirical rejection probabilities at alpha = 0.05
#   - empirical 95% coverage probabilities, computed as 1 - rejection rate
#
############################################################


# ==========================================================
# 0. User settings
# ==========================================================

RUN_TEST_VERSION <- TRUE
# Set RUN_TEST_VERSION <- FALSE for the full manuscript run.

if (RUN_TEST_VERSION) {
  N_REP <- 100
  SAMPLE_SIZES <- c(250, 500)
} else {
  N_REP <- 10000
  SAMPLE_SIZES <- seq(250, 4000, by = 250)
}

ALPHA <- 0.05
DELTA0 <- 0
LAG <- 1

BEL_BLOCKS <- c(2, 5, 10, 15, 20)

USE_PARALLEL <- TRUE
N_CORES <- max(1, min(7, parallel::detectCores() - 1))

RNGversion("4.2.0")
set.seed(7)


# ==========================================================
# 1. Packages and project functions
# ==========================================================

required_packages <- c("EL", "fracdiff", "parallel")
to_install <- required_packages[!required_packages %in% rownames(installed.packages())]

if (length(to_install) > 0) {
  install.packages(to_install)
}

invisible(lapply(required_packages, require, character.only = TRUE))

# Core functions from this repository.
source("bel_functions.R")
source("wald_ar1_rho1_functions.R")
source("wald_hac_rho1_functions.R")

# Create output directory if it does not exist.
if (!dir.exists("outputs")) dir.create("outputs")
if (!dir.exists("outputs/tables")) dir.create("outputs/tables", recursive = TRUE)


# ==========================================================
# 2. Safe wrappers for method outputs
# ==========================================================
#
# These wrappers make the simulation script robust to small differences
# in output names across function versions.

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


run_fdel <- function(x, y, Delta0 = 0, lag = 1) {
  
  
  out <- tryCatch(
    EL::FDEL.acf(
      X = x,
      Y = y,
      Delta = Delta0,
      lag = lag,
      center = TRUE,
      bartlett = FALSE
    ),
    error = function(e1) {
      tryCatch(
        EL::FDEL.acf(
          x = x,
          y = y,
          Delta = Delta0,
          lag = lag,
          center = TRUE,
          bartlett = FALSE
        ),
        error = function(e2) NULL
      )
    }
  )
  
  if (is.null(out)) {
    return(list(statistic = NA_real_, p.value = NA_real_))
  }
  
  list(
    statistic = get_statistic(out),
    p.value = get_p_value(out)
  )
}


run_ar1_wald <- function(x, y, Delta0 = 0) {
  
  out <- tryCatch(
    ar1_wald_rho1_diff(
      X = x,
      Y = y,
      Delta0 = Delta0,
      demean = TRUE
    ),
    error = function(e) NULL
  )
  
  if (is.null(out)) {
    return(list(statistic = NA_real_, p.value = NA_real_))
  }
  
  list(
    statistic = get_statistic(out),
    p.value = get_p_value(out)
  )
}


run_hac_wald <- function(x, y, Delta0 = 0) {
  
  out <- tryCatch(
    hac_wald_rho1_diff(
      X = x,
      Y = y,
      Delta0 = Delta0,
      demean = TRUE
    ),
    error = function(e) NULL
  )
  
  if (is.null(out)) {
    return(list(statistic = NA_real_, p.value = NA_real_))
  }
  
  list(
    statistic = get_statistic(out),
    p.value = get_p_value(out)
  )
}


run_bel <- function(x, y, M, Delta0 = 0) {
  
  out <- tryCatch(
    BEL.rho_diff(
      X = x,
      Y = y,
      M1 = M,
      M2 = M,
      Delta0 = Delta0
    ),
    error = function(e) NULL
  )
  
  if (is.null(out)) {
    return(list(statistic = NA_real_, p.value = NA_real_))
  }
  
  list(
    statistic = get_statistic(out),
    p.value = get_p_value(out)
  )
}


# ==========================================================
# 3. Innovation generators
# ==========================================================

r_innov_gaussian <- function(n) {
  stats::rnorm(n)
}


r_innov_mixture <- function(n, omega, mu, sigma) {
  #
  # Asymmetric heavy-tailed Gaussian mixture:
  #
  #   eps_t ~ (1 - omega) N(0, 1) + omega N(mu, sigma^2).
  #
  # The generated innovations are centered and scaled before use.
  # This keeps the DGP zero-mean while preserving skewness and kurtosis.
  #
  
  z <- stats::rnorm(n)
  ind <- stats::rbinom(n, size = 1, prob = omega)
  z[ind == 1] <- stats::rnorm(sum(ind == 1), mean = mu, sd = sigma)
  
  z <- z - mean(z)
  z <- z / stats::sd(z)
  
  z
}


# ----------------------------------------------------------
# Calibrated mixture parameters
# ----------------------------------------------------------
#
# The manuscript uses asymmetric high-kurtosis innovations calibrated
# to resemble the real-data setting. If exact calibrated parameters
# from the final manuscript run are available, replace the values below
# by those values.
#
# These defaults produce positively skewed and heavy-tailed innovations
# and are intended to make the script directly runnable.

mixture_parameters <- list(
  AR1    = list(omega = 0.05, mu = 8.0, sigma = 2.0),
  AR2    = list(omega = 0.05, mu = 8.0, sigma = 2.0),
  ARMA11 = list(omega = 0.05, mu = 8.0, sigma = 2.0),
  ARFIMA = list(omega = 0.05, mu = 8.0, sigma = 2.0)
)


generate_innovations <- function(n, innovation_type, model_name) {
  
  if (innovation_type == "gaussian") {
    return(r_innov_gaussian(n))
  }
  
  if (innovation_type == "asymmetric") {
    pars <- mixture_parameters[[model_name]]
    return(
      r_innov_mixture(
        n = n,
        omega = pars$omega,
        mu = pars$mu,
        sigma = pars$sigma
      )
    )
  }
  
  stop("Unknown innovation type.")
}


# ==========================================================
# 4. Data-generating processes
# ==========================================================

simulate_dgp <- function(n, model_name, innovation_type) {
  
  # A burn-in period is used for the short-memory models.
  burnin <- 500
  n_total <- n + burnin
  
  innov <- generate_innovations(
    n = n_total,
    innovation_type = innovation_type,
    model_name = model_name
  )
  
  if (model_name == "AR1") {
    z <- as.numeric(
      stats::arima.sim(
        n = n_total,
        model = list(ar = 0.5),
        innov = innov
      )
    )
    return(tail(z, n))
  }
  
  if (model_name == "AR2") {
    z <- as.numeric(
      stats::arima.sim(
        n = n_total,
        model = list(ar = c(0.5, -0.3)),
        innov = innov
      )
    )
    return(tail(z, n))
  }
  
  if (model_name == "ARMA11") {
    z <- as.numeric(
      stats::arima.sim(
        n = n_total,
        model = list(ar = 0.5, ma = 0.2),
        innov = innov
      )
    )
    return(tail(z, n))
  }
  
  if (model_name == "ARFIMA") {
    #
    # ARFIMA(2, d, 3) with
    #   ar = (0.36, -0.35)
    #   ma = (0.43, -0.25, -0.24)
    #   d  = 0.2
    #
    # fracdiff.sim can generate ARFIMA(p,d,q) processes.
    #
    
    z <- fracdiff::fracdiff.sim(
      n = n,
      ar = c(0.36, -0.35),
      ma = c(0.43, -0.25, -0.24),
      d = 0.2,
      rand.gen = function(nn) {
        generate_innovations(
          n = nn,
          innovation_type = innovation_type,
          model_name = model_name
        )
      }
    )$series
    
    return(as.numeric(z))
  }
  
  stop("Unknown model_name.")
}


# ==========================================================
# 5. One replication
# ==========================================================

run_one_replication <- function(n, model_name, innovation_type,
                                alpha = 0.05, Delta0 = 0, lag = 1,
                                bel_blocks = c(2, 5, 10, 15, 20)) {
  
  # Under H0 both series are generated from the same DGP.
  x <- simulate_dgp(
    n = n,
    model_name = model_name,
    innovation_type = innovation_type
  )
  
  y <- simulate_dgp(
    n = n,
    model_name = model_name,
    innovation_type = innovation_type
  )
  
  out <- list()
  
  # FDEL
  fdel <- run_fdel(x, y, Delta0 = Delta0, lag = lag)
  out[["FDEL"]] <- c(
    statistic = fdel$statistic,
    p_value = fdel$p.value,
    reject = as.numeric(is.finite(fdel$p.value) && fdel$p.value < alpha)
  )
  
  # AR(1)-Wald
  wald <- run_ar1_wald(x, y, Delta0 = Delta0)
  out[["Wald"]] <- c(
    statistic = wald$statistic,
    p_value = wald$p.value,
    reject = as.numeric(is.finite(wald$p.value) && wald$p.value < alpha)
  )
  
  # HAC-Wald
  hac <- run_hac_wald(x, y, Delta0 = Delta0)
  out[["HAC"]] <- c(
    statistic = hac$statistic,
    p_value = hac$p.value,
    reject = as.numeric(is.finite(hac$p.value) && hac$p.value < alpha)
  )
  
  # BEL for fixed block lengths
  for (M in bel_blocks) {
    bel <- run_bel(x, y, M = M, Delta0 = Delta0)
    
    out[[paste0("M=", M)]] <- c(
      statistic = bel$statistic,
      p_value = bel$p.value,
      reject = as.numeric(is.finite(bel$p.value) && bel$p.value < alpha)
    )
  }
  
  out_df <- do.call(
    rbind,
    lapply(names(out), function(method) {
      data.frame(
        method = method,
        statistic = out[[method]][["statistic"]],
        p_value = out[[method]][["p_value"]],
        reject = out[[method]][["reject"]],
        stringsAsFactors = FALSE
      )
    })
  )
  
  out_df
}


# ==========================================================
# 6. Run one simulation setting
# ==========================================================

run_one_setting <- function(n, model_name, innovation_type,
                            R = 10000, alpha = 0.05,
                            Delta0 = 0, lag = 1,
                            bel_blocks = c(2, 5, 10, 15, 20),
                            use_parallel = TRUE,
                            n_cores = 2,
                            seed = 7) {
  
  # ----------------------------------------------------------
  # Force evaluation of all arguments before starting workers.
  # This avoids errors such as "object 'BEL_BLOCKS' not found"
  # in parallel sessions.
  # ----------------------------------------------------------
  
  n <- as.integer(n)
  R <- as.integer(R)
  alpha <- as.numeric(alpha)
  Delta0 <- as.numeric(Delta0)
  lag <- as.integer(lag)
  bel_blocks <- as.numeric(bel_blocks)
  model_name <- as.character(model_name)
  innovation_type <- as.character(innovation_type)
  use_parallel <- isTRUE(use_parallel)
  n_cores <- as.integer(n_cores)
  seed <- as.integer(seed)
  
  message(
    "Running setting: model = ", model_name,
    ", innovations = ", innovation_type,
    ", n = ", n,
    ", R = ", R
  )
  
  # ----------------------------------------------------------
  # Worker function for one replication
  # ----------------------------------------------------------
  
  run_rep <- function(r) {
    
    # A separate seed for each replication. This makes the result
    # reproducible both in sequential and parallel execution.
    set.seed(seed + r)
    
    res <- run_one_replication(
      n = n,
      model_name = model_name,
      innovation_type = innovation_type,
      alpha = alpha,
      Delta0 = Delta0,
      lag = lag,
      bel_blocks = bel_blocks
    )
    
    res$replication <- r
    res$n <- n
    res$model <- model_name
    res$innovation <- innovation_type
    
    res
  }
  
  # ----------------------------------------------------------
  # Parallel or sequential execution
  # ----------------------------------------------------------
  
  if (use_parallel && n_cores > 1) {
    
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    
    # Load packages on every worker.
    parallel::clusterEvalQ(cl, {
      library(EL)
      library(fracdiff)
      library(nleqslv)
      NULL
    })
    
    # Export all required objects and functions.
    parallel::clusterExport(
      cl,
      varlist = c(
        # Current setting values
        "n",
        "R",
        "alpha",
        "Delta0",
        "lag",
        "bel_blocks",
        "model_name",
        "innovation_type",
        "seed",
        
        # One-replication function
        "run_one_replication",
        
        # DGP functions
        "simulate_dgp",
        "generate_innovations",
        "r_innov_gaussian",
        "r_innov_mixture",
        "mixture_parameters",
        
        # Method wrappers
        "run_fdel",
        "run_ar1_wald",
        "run_hac_wald",
        "run_bel",
        "get_p_value",
        "get_statistic",
        
        # Core test functions
        "BEL.rho_diff",
        "ar1_wald_rho1_diff",
        "hac_wald_rho1_diff"
      ),
      envir = environment()
    )
    
    rep_results <- parallel::parLapply(
      cl,
      X = seq_len(R),
      fun = run_rep
    )
    
  } else {
    
    rep_results <- lapply(seq_len(R), run_rep)
  }
  
  # ----------------------------------------------------------
  # Combine replication results
  # ----------------------------------------------------------
  
  raw <- do.call(rbind, rep_results)
  
  summary <- aggregate(
    reject ~ model + innovation + n + method,
    data = raw,
    FUN = mean
  )
  
  names(summary)[names(summary) == "reject"] <- "empirical_size"
  summary$coverage_95 <- 1 - summary$empirical_size
  summary$n_replications <- R
  
  list(
    raw = raw,
    summary = summary
  )
}

# ==========================================================
# 7. Run all empirical size settings
# ==========================================================

models <- c("AR1", "AR2", "ARMA11", "ARFIMA")
innovation_types <- c("gaussian", "asymmetric")

all_summaries <- list()
all_raw_paths <- character()

counter <- 1

for (innovation_type in innovation_types) {
  for (model_name in models) {
    for (n in SAMPLE_SIZES) {
      
      setting_out <- run_one_setting(
        n = n,
        model_name = model_name,
        innovation_type = innovation_type,
        R = N_REP,
        alpha = ALPHA,
        Delta0 = DELTA0,
        lag = LAG,
        bel_blocks = BEL_BLOCKS,
        use_parallel = USE_PARALLEL,
        n_cores = N_CORES,
        seed = 100000 + counter * 1000
      )
      
      all_summaries[[counter]] <- setting_out$summary
      
      raw_file <- file.path(
        "outputs",
        "tables",
        paste0(
          "raw_empirical_size_",
          innovation_type, "_",
          model_name, "_n", n, ".rds"
        )
      )
      
      saveRDS(setting_out$raw, raw_file)
      all_raw_paths[counter] <- raw_file
      
      counter <- counter + 1
    }
  }
}

empirical_size_results <- do.call(rbind, all_summaries)

# Order methods as in the manuscript figures.
method_order <- c("FDEL", "Wald", "HAC", "M=2", "M=5", "M=10", "M=15", "M=20")
empirical_size_results$method <- factor(
  empirical_size_results$method,
  levels = method_order
)

empirical_size_results <- empirical_size_results[
  order(
    empirical_size_results$innovation,
    empirical_size_results$model,
    empirical_size_results$n,
    empirical_size_results$method
  ),
]

# Save final summary.
saveRDS(
  empirical_size_results,
  file = "outputs/tables/empirical_size_results.rds"
)

write.csv(
  empirical_size_results,
  file = "outputs/tables/empirical_size_results.csv",
  row.names = FALSE
)

print(empirical_size_results)
