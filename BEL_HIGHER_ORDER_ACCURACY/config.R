production_run <- TRUE

n_threads <- max(1L, parallel::detectCores(logical = FALSE) - 1L)
alpha_levels <- c(0.10, 0.05, 0.01)

ratio_tuning <- list(
  c0 = 0.20,
  lower = 0.05,
  upper = 5.00,
  eta0 = 0.50
)

selector_tuning <- list(
  c_min = 0.05,
  c_max = 2.50,
  q0_values = c(100L, 50L, 75L, 125L)
)

replications <- function(n) {
  if (!production_run) return(1000L)
  if (n <= 500) return(50000L)
  if (n <= 1000) return(40000L)
  if (n <= 2000) return(30000L)
  20000L
}

constant_replications <- function(n) {
  if (!production_run) return(1000L)
  if (n <= 1000) return(100000L)
  if (n <= 2000) return(75000L)
  50000L
}

seeds <- list(
  ratio = 2026080101L,
  roles = 2026080201L,
  dependence = 2026080301L,
  constant = 2026080401L,
  selector = 2026080501L,
  robustness = 2026080601L
)

dir.create("results", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)
