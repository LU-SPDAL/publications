############################################################
# Power analysis simulations
# Two-sample tests for equality of lag-1 autocorrelations
############################################################
#
# Designs:
#   1. Fixed AR(1) alternatives:
#        phi_X = 0.5, phi_Y = 0.5 + Delta,
#        Delta in {0.1, 0.2}.
#
#   2. Local AR(1) alternatives:
#        phi_X = 0.5, phi_Y = 0.5 + c / sqrt(n),
#        c in {1, 2}.
#
#   3. Mixed-memory design:
#        X follows a calibrated ARMA(1,1) process,
#        Y follows an ARFIMA(2,d,3) process.
#
#      The ARMA coefficient is calibrated so that the lag-1
#      autocorrelation difference between the ARMA and ARFIMA series
#      is moderate, with target values Delta in {0.05, 0.10}.
#
#      The mixed-memory design includes both balanced and unequal
#      sample sizes. Since Y is the ARFIMA process, cases with m > n
#      have the long-memory series as the longer sample, while cases
#      with m < n have the long-memory series as the shorter sample.
#
# In all cases the null hypothesis tested is
#
#        H0: Delta = rho_Y(1) - rho_X(1) = 0.
#
# The empirical rejection probability is interpreted as finite-sample
# power when the data are generated under alternatives.
#
############################################################


# ==========================================================
# 0. User settings
# ==========================================================

RUN_TEST_VERSION <- FALSE
# Set RUN_TEST_VERSION <- FALSE for the full manuscript run.

if (RUN_TEST_VERSION) {
  N_REP_AR1 <- 100
  N_REP_MIXED_MEMORY <- 100
  
  SAMPLE_SIZES_AR1 <- c(250, 500)
  
  # Test version includes one balanced case and both directions of imbalance.
  # n = ARMA sample size, m = ARFIMA sample size.
  MIXED_SAMPLE_PAIRS <- data.frame(
    n = c(250, 250, 750),
    m = c(250, 750, 250)
  )
  
} else {
  N_REP_AR1 <- 10000
  N_REP_MIXED_MEMORY <- 5000
  
  SAMPLE_SIZES_AR1 <- c(250, 500, 1000, 2000)
  
  # Full mixed-memory design.
  # n = ARMA sample size, m = ARFIMA sample size.
  #
  # Balanced cases:
  #   (250,250), (500,500), (750,750), (1000,1000)
  #
  # Long-memory sample longer:
  #   (250,750), (500,1500)
  #
  # Long-memory sample shorter:
  #   (750,250), (1500,500)
  MIXED_SAMPLE_PAIRS <- data.frame(
    n = c(250, 500, 750, 1000, 250, 500, 750, 1500),
    m = c(250, 500, 750, 1000, 750, 1500, 250, 500)
  )
}

ALPHA <- 0.05
DELTA0 <- 0
LAG <- 1

USE_PARALLEL <- TRUE
N_CORES <- max(1, min(7, parallel::detectCores() - 1))

RNGversion("4.2.0")
set.seed(7)

# Mixed-memory alternatives.
# These target differences are deliberately moderate.
DELTA_MIXED_MEMORY <- c(0.05, 0.10)

# Fixed ARFIMA(2,d,3) process used as the long-memory series.
MIXED_ARFIMA_AR <- c(0.36, -0.35)
MIXED_ARFIMA_MA <- c(0.43, -0.25, -0.24)
MIXED_ARFIMA_D  <- 0.20

# ARMA(1,1) short-memory comparison process.
# The MA coefficient is fixed and the AR coefficient is calibrated.
MIXED_ARMA_MA <- 0.20


# ==========================================================
# 1. Packages and project functions
# ==========================================================

required_packages <- c("EL", "fracdiff", "parallel", "nleqslv")

to_install <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(to_install) > 0) {
  install.packages(to_install)
}

invisible(lapply(required_packages, require, character.only = TRUE))

# Core functions from this repository.
source("bel_functions.R")
source("wald_ar1_rho1_functions.R")
source("wald_hac_rho1_functions.R")

if (!dir.exists("outputs")) dir.create("outputs")
if (!dir.exists("outputs/tables")) dir.create("outputs/tables", recursive = TRUE)


# ==========================================================
# 2. Safe wrappers for method outputs
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


run_bel <- function(x, y, M1, M2, Delta0 = 0) {
  
  out <- tryCatch(
    BEL.rho_diff(
      X = x,
      Y = y,
      M1 = M1,
      M2 = M2,
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


r_innov_mixture <- function(n, omega = 0.05, mu = 8.0, sigma = 2.0) {
  #
  # Asymmetric heavy-tailed Gaussian mixture:
  #
  #   eps_t ~ (1 - omega) N(0, 1) + omega N(mu, sigma^2).
  #
  # The innovations are centered and scaled before use. This keeps
  # the simulated processes zero-mean while preserving skewness and
  # excess kurtosis.
  #
  
  z <- stats::rnorm(n)
  ind <- stats::rbinom(n, size = 1, prob = omega)
  n_tail <- sum(ind == 1)
  
  if (n_tail > 0) {
    z[ind == 1] <- stats::rnorm(n_tail, mean = mu, sd = sigma)
  }
  
  z <- z - mean(z)
  z <- z / stats::sd(z)
  
  z
}


generate_innovations <- function(n, innovation_type) {
  
  if (innovation_type == "gaussian") {
    return(r_innov_gaussian(n))
  }
  
  if (innovation_type == "asymmetric") {
    return(r_innov_mixture(n))
  }
  
  stop("Unknown innovation_type.")
}


# ==========================================================
# Helpers for autocorrelation calibration
# ==========================================================

rho1_centered <- function(z) {
  z <- as.numeric(z)
  z <- z - mean(z)
  sum(z[-1] * z[-length(z)]) / sum(z[-length(z)]^2)
}


arma11_rho1 <- function(phi, theta) {
  #
  # Theoretical lag-1 autocorrelation of
  #
  #   X_t = phi X_{t-1} + eps_t + theta eps_{t-1}.
  #
  # rho(1) = ((phi + theta)(1 + phi theta)) /
  #          (1 + theta^2 + 2 phi theta).
  #
  
  ((phi + theta) * (1 + phi * theta)) /
    (1 + theta^2 + 2 * phi * theta)
}


solve_arma11_phi_for_rho1 <- function(target_rho,
                                      theta = 0.20,
                                      lower = -0.95,
                                      upper =  0.95) {
  
  f <- function(phi) {
    arma11_rho1(phi, theta) - target_rho
  }
  
  f_lower <- f(lower)
  f_upper <- f(upper)
  
  if (!is.finite(f_lower) || !is.finite(f_upper) || f_lower * f_upper > 0) {
    stop("Target ARMA(1,1) autocorrelation is outside the admissible search range.")
  }
  
  stats::uniroot(f, lower = lower, upper = upper)$root
}


# ==========================================================
# Data-generating processes
# ==========================================================

simulate_ar1 <- function(n, phi, innovation_type = "gaussian") {
  
  burnin <- 500
  n_total <- n + burnin
  
  innov <- generate_innovations(n_total, innovation_type)
  
  z <- as.numeric(
    stats::arima.sim(
      n = n_total,
      model = list(ar = phi),
      innov = innov
    )
  )
  
  tail(z, n)
}


simulate_arma11 <- function(n, innovation_type = "gaussian",
                            ar = 0.5, ma = 0.2) {
  
  burnin <- 500
  n_total <- n + burnin
  
  innov <- generate_innovations(n_total, innovation_type)
  
  z <- as.numeric(
    stats::arima.sim(
      n = n_total,
      model = list(ar = ar, ma = ma),
      innov = innov
    )
  )
  
  tail(z, n)
}


simulate_arfima_2d3 <- function(n, innovation_type = "gaussian",
                                ar = MIXED_ARFIMA_AR,
                                ma = MIXED_ARFIMA_MA,
                                d = MIXED_ARFIMA_D) {
  
  z <- fracdiff::fracdiff.sim(
    n = n,
    ar = ar,
    ma = ma,
    d = d,
    rand.gen = function(nn) {
      generate_innovations(nn, innovation_type)
    }
  )$series
  
  as.numeric(z)
}


estimate_arfima_rho1_reference <- function(n = 50000,
                                           innovation_type = "gaussian",
                                           seed = 2026) {
  set.seed(seed)
  
  z <- simulate_arfima_2d3(
    n = n,
    innovation_type = innovation_type,
    ar = MIXED_ARFIMA_AR,
    ma = MIXED_ARFIMA_MA,
    d = MIXED_ARFIMA_D
  )
  
  rho1_centered(z)
}


# ==========================================================
#  Mixed-memory calibration
# ==========================================================
#
# Y is the ARFIMA series and X is the ARMA series.
# We first estimate the ARFIMA lag-1 autocorrelation from a long
# pilot simulation. Then, for each target Delta, we choose the AR
# coefficient of the ARMA(1,1) series so that
#
#   rho_Y(1) - rho_X(1) = Delta.
#
# This makes the mixed-memory alternative moderate rather than
# trivially easy to detect.

rho1_arfima_reference <- estimate_arfima_rho1_reference(
  n = 50000,
  innovation_type = "gaussian",
  seed = 2026
)

mixed_memory_parameter_table <- data.frame(
  delta = DELTA_MIXED_MEMORY,
  rhoY_target = rho1_arfima_reference,
  rhoX_target = rho1_arfima_reference - DELTA_MIXED_MEMORY,
  arma_ma = MIXED_ARMA_MA,
  arma_phi = NA_real_
)

for (i in seq_len(nrow(mixed_memory_parameter_table))) {
  mixed_memory_parameter_table$arma_phi[i] <-
    solve_arma11_phi_for_rho1(
      target_rho = mixed_memory_parameter_table$rhoX_target[i],
      theta = MIXED_ARMA_MA
    )
}

cat("\nMixed-memory calibration:\n")
print(mixed_memory_parameter_table)

write.csv(
  mixed_memory_parameter_table,
  "outputs/tables/mixed_memory_parameter_calibration.csv",
  row.names = FALSE
)


#  Helper for running methods


make_rejection_row <- function(method, statistic, p_value, alpha = 0.05) {
  data.frame(
    method = method,
    statistic = statistic,
    p_value = p_value,
    reject = as.numeric(is.finite(p_value) && p_value < alpha),
    stringsAsFactors = FALSE
  )
}


run_fdel_and_wald <- function(x, y, alpha = 0.05, Delta0 = 0, lag = 1) {
  
  fdel <- run_fdel(x, y, Delta0 = Delta0, lag = lag)
  wald <- run_ar1_wald(x, y, Delta0 = Delta0)
  
  rbind(
    make_rejection_row("FDEL", fdel$statistic, fdel$p.value, alpha),
    make_rejection_row("Wald", wald$statistic, wald$p.value, alpha)
  )
}


run_mixed_memory_methods <- function(x, y, alpha = 0.05, Delta0 = 0, lag = 1) {
  
  n <- length(x)
  m <- length(y)
  
  M1_base <- floor(n^(1 / 3))
  M2_base <- floor(m^(1 / 3))
  
  fdel <- run_fdel(x, y, Delta0 = Delta0, lag = lag)
  hac <- run_hac_wald(x, y, Delta0 = Delta0)
  
  bel_1 <- run_bel(
    x, y,
    M1 = M1_base,
    M2 = M2_base,
    Delta0 = Delta0
  )
  
  bel_2 <- run_bel(
    x, y,
    M1 = 2 * M1_base,
    M2 = 2 * M2_base,
    Delta0 = Delta0
  )
  
  rbind(
    make_rejection_row("FDEL", fdel$statistic, fdel$p.value, alpha),
    make_rejection_row("HAC", hac$statistic, hac$p.value, alpha),
    make_rejection_row("BEL_floor_n13", bel_1$statistic, bel_1$p.value, alpha),
    make_rejection_row("BEL_2floor_n13", bel_2$statistic, bel_2$p.value, alpha)
  )
}



# Fixed AR(1) alternatives


run_one_fixed_ar1_replication <- function(n, delta,
                                          innovation_type = "gaussian",
                                          alpha = 0.05,
                                          Delta0 = 0,
                                          lag = 1) {
  
  x <- simulate_ar1(
    n = n,
    phi = 0.5,
    innovation_type = innovation_type
  )
  
  y <- simulate_ar1(
    n = n,
    phi = 0.5 + delta,
    innovation_type = innovation_type
  )
  
  out <- run_fdel_and_wald(
    x = x,
    y = y,
    alpha = alpha,
    Delta0 = Delta0,
    lag = lag
  )
  
  out$n <- n
  out$design <- "fixed_ar1"
  out$innovation <- innovation_type
  out$delta <- delta
  out$c_value <- NA_real_
  
  out
}


#  Local AR(1) alternatives


run_one_local_ar1_replication <- function(n, c_value,
                                          innovation_type = "gaussian",
                                          alpha = 0.05,
                                          Delta0 = 0,
                                          lag = 1) {
  
  delta_n <- c_value / sqrt(n)
  
  x <- simulate_ar1(
    n = n,
    phi = 0.5,
    innovation_type = innovation_type
  )
  
  y <- simulate_ar1(
    n = n,
    phi = 0.5 + delta_n,
    innovation_type = innovation_type
  )
  
  out <- run_fdel_and_wald(
    x = x,
    y = y,
    alpha = alpha,
    Delta0 = Delta0,
    lag = lag
  )
  
  out$n <- n
  out$design <- "local_ar1"
  out$innovation <- innovation_type
  out$delta <- delta_n
  out$c_value <- c_value
  
  out
}



#  Mixed-memory design


run_one_mixed_memory_replication <- function(n, m = n,
                                             delta_mixed = 0.05,
                                             innovation_type = "gaussian",
                                             alpha = 0.05,
                                             Delta0 = 0,
                                             lag = 1) {
  
  row_id <- which(abs(mixed_memory_parameter_table$delta - delta_mixed) < 1e-12)
  
  if (length(row_id) != 1) {
    stop("delta_mixed is not found in mixed_memory_parameter_table.")
  }
  
  arma_phi <- mixed_memory_parameter_table$arma_phi[row_id]
  arma_ma  <- mixed_memory_parameter_table$arma_ma[row_id]
  
  # X: short-memory ARMA(1,1), calibrated to be close to Y in rho(1).
  x <- simulate_arma11(
    n = n,
    innovation_type = innovation_type,
    ar = arma_phi,
    ma = arma_ma
  )
  
  # Y: long-memory ARFIMA(2,d,3).
  y <- simulate_arfima_2d3(
    n = m,
    innovation_type = innovation_type,
    ar = MIXED_ARFIMA_AR,
    ma = MIXED_ARFIMA_MA,
    d = MIXED_ARFIMA_D
  )
  
  out <- run_mixed_memory_methods(
    x = x,
    y = y,
    alpha = alpha,
    Delta0 = Delta0,
    lag = lag
  )
  
  out$n <- n
  out$m <- m
  out$design <- "mixed_memory"
  out$innovation <- innovation_type
  out$delta <- delta_mixed
  out$c_value <- NA_real_
  out$rhoY_target <- mixed_memory_parameter_table$rhoY_target[row_id]
  out$rhoX_target <- mixed_memory_parameter_table$rhoX_target[row_id]
  out$arma_phi <- arma_phi
  out$arma_ma <- arma_ma
  out$arfima_d <- MIXED_ARFIMA_D
  out$long_memory_sample <- ifelse(
    m > n,
    "longer",
    ifelse(m < n, "shorter", "same_length")
  )
  
  out
}



# Generic simulation runner


run_replications <- function(rep_fun, R, use_parallel = TRUE,
                             n_cores = 2, seed = 7, ...) {
  
  dots <- list(...)
  R <- as.integer(R)
  seed <- as.integer(seed)
  
  worker_fun <- function(r) {
    set.seed(seed + r)
    do.call(rep_fun, dots)
  }
  
  if (use_parallel && n_cores > 1) {
    
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    
    parallel::clusterEvalQ(cl, {
      library(EL)
      library(fracdiff)
      library(nleqslv)
      NULL
    })
    
    parallel::clusterExport(
      cl,
      varlist = c(
        # Worker control
        "worker_fun",
        "dots",
        "rep_fun",
        "seed",
        
        # Main replication functions
        "run_one_fixed_ar1_replication",
        "run_one_local_ar1_replication",
        "run_one_mixed_memory_replication",
        
        # DGP functions
        "simulate_ar1",
        "simulate_arma11",
        "simulate_arfima_2d3",
        "generate_innovations",
        "r_innov_gaussian",
        "r_innov_mixture",
        
        # Calibration helpers
        "rho1_centered",
        "arma11_rho1",
        "solve_arma11_phi_for_rho1",
        "estimate_arfima_rho1_reference",
        "mixed_memory_parameter_table",
        "DELTA_MIXED_MEMORY",
        "MIXED_ARFIMA_AR",
        "MIXED_ARFIMA_MA",
        "MIXED_ARFIMA_D",
        "MIXED_ARMA_MA",
        
        # Method wrappers
        "run_fdel_and_wald",
        "run_mixed_memory_methods",
        "run_fdel",
        "run_ar1_wald",
        "run_hac_wald",
        "run_bel",
        "get_p_value",
        "get_statistic",
        "make_rejection_row",
        
        # Core tests
        "BEL.rho_diff",
        "ar1_wald_rho1_diff",
        "hac_wald_rho1_diff",
        
        # Constants
        "ALPHA",
        "DELTA0",
        "LAG"
      ),
      envir = environment()
    )
    
    res <- parallel::parLapply(cl, seq_len(R), worker_fun)
    
  } else {
    res <- lapply(seq_len(R), worker_fun)
  }
  
  raw <- do.call(rbind, res)
  
  n_methods <- length(unique(raw$method))
  raw$replication <- rep(seq_len(R), each = n_methods)
  
  raw
}


summarise_power <- function(raw) {
  
  candidate_group_vars <- c(
    "design",
    "innovation",
    "n",
    "m",
    "delta",
    "c_value",
    "rhoY_target",
    "rhoX_target",
    "arma_phi",
    "arma_ma",
    "arfima_d",
    "long_memory_sample",
    "method"
  )
  
  group_vars <- intersect(candidate_group_vars, names(raw))
  
  group_vars <- group_vars[
    vapply(group_vars, function(v) {
      !all(is.na(raw[[v]]))
    }, logical(1))
  ]
  
  if (length(group_vars) == 0) {
    stop("No valid grouping variables found.")
  }
  
  raw$reject <- as.numeric(raw$reject)
  
  formula_text <- paste("reject ~", paste(group_vars, collapse = " + "))
  
  summary <- aggregate(
    stats::as.formula(formula_text),
    data = raw,
    FUN = mean,
    na.rm = TRUE
  )
  
  names(summary)[names(summary) == "reject"] <-
    "empirical_rejection_probability"
  
  n_rep <- aggregate(
    stats::as.formula(formula_text),
    data = raw,
    FUN = function(z) sum(!is.na(z))
  )
  
  names(n_rep)[names(n_rep) == "reject"] <- "n_valid_replications"
  
  summary <- merge(
    summary,
    n_rep,
    by = group_vars,
    all.x = TRUE
  )
  
  summary
}



# Run fixed AR(1) power simulations


fixed_power_raw_list <- list()
counter <- 1

for (innovation_type in c("gaussian", "asymmetric")) {
  for (n in SAMPLE_SIZES_AR1) {
    for (delta in c(0.1, 0.2)) {
      
      message(
        "Fixed AR(1) power: n = ", n,
        ", delta = ", delta,
        ", innovations = ", innovation_type
      )
      
      fixed_power_raw_list[[counter]] <- run_replications(
        rep_fun = run_one_fixed_ar1_replication,
        R = N_REP_AR1,
        use_parallel = USE_PARALLEL,
        n_cores = N_CORES,
        seed = 100000 + counter * 1000,
        n = n,
        delta = delta,
        innovation_type = innovation_type,
        alpha = ALPHA,
        Delta0 = DELTA0,
        lag = LAG
      )
      
      counter <- counter + 1
    }
  }
}

fixed_power_raw <- do.call(rbind, fixed_power_raw_list)
fixed_power_summary <- summarise_power(fixed_power_raw)

saveRDS(fixed_power_raw, "outputs/tables/power_fixed_ar1_raw.rds")
saveRDS(fixed_power_summary, "outputs/tables/power_fixed_ar1_summary.rds")

write.csv(
  fixed_power_summary,
  "outputs/tables/power_fixed_ar1_summary.csv",
  row.names = FALSE
)



#  Run local AR(1) power simulations


local_power_raw_list <- list()
counter <- 1

for (innovation_type in c("gaussian", "asymmetric")) {
  for (n in SAMPLE_SIZES_AR1) {
    for (c_value in c(1, 2)) {
      
      message(
        "Local AR(1) power: n = ", n,
        ", c = ", c_value,
        ", innovations = ", innovation_type
      )
      
      local_power_raw_list[[counter]] <- run_replications(
        rep_fun = run_one_local_ar1_replication,
        R = N_REP_AR1,
        use_parallel = USE_PARALLEL,
        n_cores = N_CORES,
        seed = 200000 + counter * 1000,
        n = n,
        c_value = c_value,
        innovation_type = innovation_type,
        alpha = ALPHA,
        Delta0 = DELTA0,
        lag = LAG
      )
      
      counter <- counter + 1
    }
  }
}

local_power_raw <- do.call(rbind, local_power_raw_list)
local_power_summary <- summarise_power(local_power_raw)

saveRDS(local_power_raw, "outputs/tables/power_local_ar1_raw.rds")
saveRDS(local_power_summary, "outputs/tables/power_local_ar1_summary.rds")

write.csv(
  local_power_summary,
  "outputs/tables/power_local_ar1_summary.csv",
  row.names = FALSE
)


# ==========================================================
# 14. Run mixed-memory power simulations
# ==========================================================

mixed_memory_raw_list <- list()
counter <- 1

for (innovation_type in c("gaussian", "asymmetric")) {
  for (pair_id in seq_len(nrow(MIXED_SAMPLE_PAIRS))) {
    for (delta_mixed in DELTA_MIXED_MEMORY) {
      
      n_pair <- MIXED_SAMPLE_PAIRS$n[pair_id]
      m_pair <- MIXED_SAMPLE_PAIRS$m[pair_id]
      
      message(
        "Mixed-memory power: n = ", n_pair,
        ", m = ", m_pair,
        ", target delta = ", delta_mixed,
        ", innovations = ", innovation_type
      )
      
      mixed_memory_raw_list[[counter]] <- run_replications(
        rep_fun = run_one_mixed_memory_replication,
        R = N_REP_MIXED_MEMORY,
        use_parallel = USE_PARALLEL,
        n_cores = N_CORES,
        seed = 300000 + counter * 1000,
        n = n_pair,
        m = m_pair,
        delta_mixed = delta_mixed,
        innovation_type = innovation_type,
        alpha = ALPHA,
        Delta0 = DELTA0,
        lag = LAG
      )
      
      counter <- counter + 1
    }
  }
}

mixed_memory_raw <- do.call(rbind, mixed_memory_raw_list)
mixed_memory_summary <- summarise_power(mixed_memory_raw)

saveRDS(mixed_memory_raw, "outputs/tables/power_mixed_memory_raw.rds")
saveRDS(mixed_memory_summary, "outputs/tables/power_mixed_memory_summary.rds")

write.csv(
  mixed_memory_summary,
  "outputs/tables/power_mixed_memory_summary.csv",
  row.names = FALSE
)


print(fixed_power_summary)


print(local_power_summary)

print(mixed_memory_parameter_table)

print(MIXED_SAMPLE_PAIRS)

print(mixed_memory_summary)




mixed_power_table <- mixed_memory_summary |>
  dplyr::select(
    innovation, n, m, delta, method,
    empirical_rejection_probability
  ) |>
  dplyr::mutate(
    method = dplyr::recode(
      method,
      "BEL_floor_n13" = "BEL_1",
      "BEL_2floor_n13" = "BEL_2"
    )
  ) |>
  tidyr::pivot_wider(
    names_from = c(innovation, method),
    values_from = empirical_rejection_probability
  ) |>
  dplyr::arrange(n, m, delta)

print(mixed_power_table, n = Inf)

write.csv(
  mixed_power_table,
  "outputs/tables/mixed_memory_power_wide_table.csv",
  row.names = FALSE
)
