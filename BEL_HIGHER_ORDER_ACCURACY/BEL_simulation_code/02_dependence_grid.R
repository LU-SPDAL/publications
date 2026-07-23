# Gaussian dependence-grid study for the BEL paper.
#
# For each phi and N, the same simulated series is evaluated with
# M = N^(1/3), N^(1/2), and N^(2/3). This isolates the effect of dependence
# strength without changing the marginal variance.

# Settings

MODE <- "paper"                    # "paper" or "quick"

B_POINT_PAPER <- 100000L
B_CI_PAPER <- 20000L
B_POINT_QUICK <- 500L
B_CI_QUICK <- 100L

B_POINT <- if (MODE == "paper") B_POINT_PAPER else B_POINT_QUICK
B_CI <- if (MODE == "paper") B_CI_PAPER else B_CI_QUICK

if (B_CI > B_POINT) {
  stop("B_CI cannot exceed B_POINT.")
}

CHUNK_SIZE <- if (MODE == "paper") 1000L else 100L
REUSE_SAVED_CHUNKS <- TRUE
COMPUTE_CONFIDENCE_INTERVALS <- TRUE

MASTER_SEED <- 2026051501L
CODE_VERSION <- "bel_phi_grid_three_regimes_v1_20260713"
NOMINAL_COVERAGE <- 0.95

PHI_GRID <- round(seq(0, 0.95, by = 0.05), 2)
K_GRID <- c(2L, 3L, 4L)

# Interval inversion
CI_BISECTION_ITERATIONS <- 28L
CI_COARSE_GRID_POINTS <- 21L
CI_DENSE_GRID_POINTS <- 201L
STATIONARY_TAIL_VARIANCE_TOL <- 1e-14

# Methods used in the focused plots. The tables contain all nine methods.
CORE_METHODS <- c(
  "raw",
  "K_leading",
  "V_oracle",
  "V_feasible",
  "VK_leading_oracle",
  "F_feasible_leading"
)

MECHANISM_METHODS <- c(
  "raw",
  "K_leading",
  "K_asym_full",
  "V_oracle",
  "V_feasible"
)

FINAL_METHODS <- c(
  "raw",
  "K_leading",
  "VK_leading_oracle",
  "F_feasible_leading"
)

RUN_MODULES <- "phi_grid_three_regimes"

RESULTS_DIR <- file.path(
  getwd(),
  paste0("BEL_phi_grid_three_regimes_results_", MODE)
)

dir.create(RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(RESULTS_DIR, "chunks"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(RESULTS_DIR, "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(RESULTS_DIR, "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(RESULTS_DIR, "objects"), recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("Rcpp", quietly = TRUE)) {
  stop("Install Rcpp first: install.packages('Rcpp')")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Install ggplot2 first: install.packages('ggplot2')")
}

RNGkind(
  kind = "Mersenne-Twister",
  normal.kind = "Inversion",
  sample.kind = "Rejection"
)
set.seed(MASTER_SEED)

# Innovation distribution

# Gaussian innovations isolate the dependence effect. Distributional robustness
# has already been studied in the unified master simulation.
innovation_info <- data.frame(
  innovation = "gaussian",
  innovation_label = "Gaussian",
  dist_code = 1L,
  skewness = 0,
  excess_kurtosis = 0,
  assumption_status = "inside formal moment assumptions",
  severity_group = "regular",
  stringsAsFactors = FALSE
)

# 2. Dependence grid and block-length regimes

membership_parts <- list()

regime_info <- data.frame(
  regime = c("alpha_1_3", "alpha_1_2", "alpha_2_3"),
  regime_label = c(
    "M = N^(1/3)",
    "M = N^(1/2)",
    "M = N^(2/3)"
  ),
  alpha_nominal = c(1 / 3, 1 / 2, 2 / 3),
  stringsAsFactors = FALSE
)

for (k_now in K_GRID) {
  N_now <- as.integer(k_now^6)
  M_values <- as.integer(c(k_now^2, k_now^3, k_now^4))
  Q_values <- as.integer(c(k_now^4, k_now^3, k_now^2))

  stopifnot(all(M_values * Q_values == N_now))

  for (phi_now in PHI_GRID) {
    membership_parts[[length(membership_parts) + 1L]] <- data.frame(
      module = "phi_grid_three_regimes",
      panel = paste0("N_", N_now),
      series = regime_info$regime_label,
      regime = regime_info$regime,
      regime_label = regime_info$regime_label,
      innovation = "gaussian",
      phi = phi_now,
      N = rep(N_now, 3L),
      M = M_values,
      Q = Q_values,
      x_order = rep(phi_now, 3L),
      alpha_nominal = regime_info$alpha_nominal,
      C_nominal = NA_real_,
      note = c(
        "Short blocks; variance mismatch expected to dominate",
        "Balanced blocks; both leading errors have the same order",
        "Long blocks and few blocks; finite-Q error expected to dominate"
      ),
      stringsAsFactors = FALSE
    )
  }
}

membership <- do.call(rbind, membership_parts)
membership <- membership[membership$module %in% RUN_MODULES, ]
row.names(membership) <- NULL

membership <- merge(
  membership,
  innovation_info,
  by = "innovation",
  all.x = TRUE,
  sort = FALSE
)

membership$phi_label <- sprintf("phi = %.2f", membership$phi)
membership$N_label <- paste0("N = ", membership$N)
membership$design_id <- sprintf(
  "%s_phi%03d_N%d_M%d_Q%d",
  membership$innovation,
  as.integer(round(100 * membership$phi)),
  membership$N,
  membership$M,
  membership$Q
)
membership$scenario_id <- sprintf(
  "%s_phi%03d_N%d",
  membership$innovation,
  as.integer(round(100 * membership$phi)),
  membership$N
)
membership$alpha_effective <- log(membership$M) / log(membership$N)
membership$L <- pmin(
  pmax(1L, floor(log(membership$N) * log(log(membership$N)))),
  floor(membership$M / 2)
)

stopifnot(all(membership$N == membership$M * membership$Q))
stopifnot(all(membership$M >= 2L))
stopifnot(all(membership$Q >= 2L))
stopifnot(length(unique(membership$phi)) == length(PHI_GRID))

# Every distribution-phi-N-M design occurs once. The three block lengths for
# the same phi and N share the same generated time series.
design_table <- membership[
  !duplicated(membership$design_id),
  c(
    "design_id", "scenario_id", "innovation", "innovation_label",
    "dist_code", "skewness", "excess_kurtosis", "assumption_status",
    "severity_group", "phi", "phi_label", "N", "M", "Q", "L"
  )
]
row.names(design_table) <- NULL

scenario_table <- design_table[
  !duplicated(design_table$scenario_id),
  c(
    "scenario_id", "innovation", "innovation_label", "dist_code",
    "skewness", "excess_kurtosis", "assumption_status", "severity_group",
    "phi", "phi_label", "N"
  )
]
row.names(scenario_table) <- NULL

stable_scenario_seed <- function(master_seed, N, phi, dist_code) {
  modulus <- 2147483646
  phi_code <- as.integer(round(10000 * phi))
  value <- (
    as.double(master_seed) +
      1000003 * as.double(N) +
      10007 * as.double(phi_code) +
      101 * as.double(dist_code)
  ) %% modulus
  as.integer(value + 1)
}

scenario_table$seed <- mapply(
  stable_scenario_seed,
  MoreArgs = list(master_seed = MASTER_SEED),
  N = scenario_table$N,
  phi = scenario_table$phi,
  dist_code = scenario_table$dist_code
)

membership <- merge(
  membership,
  scenario_table[, c("scenario_id", "seed")],
  by = "scenario_id",
  all.x = TRUE,
  sort = FALSE
)
design_table <- merge(
  design_table,
  scenario_table[, c("scenario_id", "seed")],
  by = "scenario_id",
  all.x = TRUE,
  sort = FALSE
)

write.csv(
  membership,
  file.path(RESULTS_DIR, "tables", "design_membership.csv"),
  row.names = FALSE
)
write.csv(
  scenario_table,
  file.path(RESULTS_DIR, "tables", "scenario_seeds.csv"),
  row.names = FALSE
)

# 3. Population quantities

gamma_ar1_unit_variance <- function(h, phi) {
  phi^abs(h)
}

omega_ar1_unit_variance <- function(r, phi) {
  r <- as.integer(r)
  stopifnot(r >= 1L)
  if (r == 1L) return(1)
  h <- seq_len(r - 1L)
  1 + 2 * sum((1 - h / r) * phi^h)
}

sigma2_long_run <- function(phi) {
  (1 + phi) / (1 - phi)
}

B2_long_run <- function(phi) {
  2 * phi / (1 - phi)^2
}

b_true_ar1 <- function(phi) {
  2 * phi / (1 - phi^2)
}

aK_asym_full <- function(M, phi, skewness, excess_kurtosis) {
  1.5 * M -
    5 * b_true_ar1(phi) +
    0.5 * excess_kurtosis -
    (1 / 3) * skewness^2
}

block_sum_cov_unit_variance <- function(M, block_lag, phi) {
  d <- seq.int(-(M - 1L), M - 1L)
  sum(
    (M - abs(d)) *
      gamma_ar1_unit_variance(block_lag * M + d, phi)
  )
}

aK_gaussian_exact_diagnostic <- function(N, M, phi) {
  OmN <- omega_ar1_unit_variance(N, phi)
  r <- vapply(
    0:2,
    function(h) {
      block_sum_cov_unit_variance(M, h, phi) /
        (M^2 * OmN)
    },
    numeric(1)
  )

  r0 <- r[1L]
  r1 <- r[2L]
  r2 <- r[3L]

  M^3 * (
    1.5 * r0^2 -
      4 * r0 * r1 -
      2 * r0 * r2 -
      4 * r1^2 -
      8 * r1 * r2
  )
}

stationary_burn_in <- function(phi, tolerance) {
  if (abs(phi) == 0) return(0L)
  K <- ceiling(log(tolerance) / (2 * log(abs(phi))))
  as.integer(max(1, K))
}

design_table$sigma2_long_run <- sigma2_long_run(design_table$phi)
design_table$B2_long_run <- B2_long_run(design_table$phi)
design_table$b_true <- b_true_ar1(design_table$phi)
design_table$omega_M <- mapply(
  omega_ar1_unit_variance,
  r = design_table$M,
  phi = design_table$phi
)
design_table$omega_N <- mapply(
  omega_ar1_unit_variance,
  r = design_table$N,
  phi = design_table$phi
)
design_table$nu_oracle <- design_table$omega_M / design_table$omega_N
design_table$aK_asym_full <- mapply(
  aK_asym_full,
  M = design_table$M,
  phi = design_table$phi,
  skewness = design_table$skewness,
  excess_kurtosis = design_table$excess_kurtosis
)
design_table$factor_K_asym_full <-
  1 - design_table$aK_asym_full / design_table$N
design_table$factor_K_leading <-
  1 - 3 / (2 * design_table$Q)
design_table$variance_component <-
  design_table$b_true / design_table$M
design_table$EL_component <-
  3 / (2 * design_table$Q)
design_table$leading_total_component <-
  design_table$variance_component + design_table$EL_component
design_table$first_order_length_benchmark <-
  2 * sqrt(
    qchisq(NOMINAL_COVERAGE, df = 1) *
      design_table$omega_N / design_table$N
  )

design_table$aK_gaussian_exact <- NA_real_
is_gaussian <- design_table$innovation == "gaussian"
design_table$aK_gaussian_exact[is_gaussian] <- mapply(
  aK_gaussian_exact_diagnostic,
  N = design_table$N[is_gaussian],
  M = design_table$M[is_gaussian],
  phi = design_table$phi[is_gaussian]
)
design_table$aK_exact_minus_asym <-
  design_table$aK_gaussian_exact - design_table$aK_asym_full

if (any(design_table$factor_K_asym_full <= 0) ||
    any(design_table$factor_K_leading <= 0)) {
  stop("At least one correction factor is non-positive.")
}

write.csv(
  design_table,
  file.path(RESULTS_DIR, "tables", "population_design_constants.csv"),
  row.names = FALSE
)

# 4. Simulation and interval inversion

Rcpp::sourceCpp(code = '
#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <algorithm>
#include <limits>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

const int K_METHODS = 9;
const int K_PAIRS = 5;

struct FeasCoeffs {
  double sigma_a;
  double sigma_b;
  double sigma_c;
  double B2_a;
  double B2_b;
  double B2_c;
  int L;
  int M;
  int N;
};

struct FeasEval {
  double sigma2;
  double B2;
  double b;
  double nu;
  int sigma_nonpositive;
  int trunc_active;
};

inline double draw_standard_innovation(const int dist_code) {
  if (dist_code == 1) {
    return R::rnorm(0.0, 1.0);
  }

  if (dist_code == 2) {
    const double u = R::runif(0.0, 1.0) - 0.5;
    const double sign_u = (u > 0.0) ? 1.0 : ((u < 0.0) ? -1.0 : 0.0);
    const double b = 1.0 / std::sqrt(2.0);
    return -b * sign_u * std::log(1.0 - 2.0 * std::fabs(u));
  }

  if (dist_code == 3) {
    return (R::rgamma(4.0, 1.0) - 4.0) / 2.0;
  }

  if (dist_code == 4) {
    const double sd = (R::runif(0.0, 1.0) < 0.05) ? 5.0 : 1.0;
    return R::rnorm(0.0, sd) / std::sqrt(2.2);
  }

  if (dist_code == 5) {
    return (R::rgamma(0.5, 1.0) - 0.5) / std::sqrt(0.5);
  }

  if (dist_code == 6) {
    return std::sqrt(3.0 / 5.0) * R::rt(5.0);
  }

  stop("Unknown innovation distribution code.");
  return NA_REAL;
}

inline double scalar_el_ratio_mean(
    const std::vector<double>& x,
    const double mu) {

  const int q = static_cast<int>(x.size());
  if (q < 2) return R_PosInf;

  double zmin = x[0] - mu;
  double zmax = zmin;
  double s1 = 0.0;
  double s2 = 0.0;

  for (int i = 0; i < q; ++i) {
    const double z = x[i] - mu;
    zmin = std::min(zmin, z);
    zmax = std::max(zmax, z);
    s1 += z;
    s2 += z * z;
  }

  if (!(zmin < 0.0 && zmax > 0.0) || s2 <= 0.0) {
    return R_PosInf;
  }

  if (std::fabs(s1) < 1e-15 * (1.0 + std::sqrt(s2))) {
    return 0.0;
  }

  double lo = -1.0 / zmax;
  double hi = -1.0 / zmin;
  lo += 1e-13 * (1.0 + std::fabs(lo));
  hi -= 1e-13 * (1.0 + std::fabs(hi));

  double lambda = s1 / s2;
  if (!R_finite(lambda) || lambda <= lo || lambda >= hi) {
    lambda = 0.5 * (lo + hi);
  }

  for (int iter = 0; iter < 100; ++iter) {
    double f = 0.0;
    double fp = 0.0;
    bool valid = true;

    for (int i = 0; i < q; ++i) {
      const double z = x[i] - mu;
      const double den = 1.0 + lambda * z;
      if (den <= 0.0 || !R_finite(den)) {
        valid = false;
        break;
      }
      f += z / den;
      fp -= z * z / (den * den);
    }

    if (!valid || !R_finite(f) || !R_finite(fp) || fp >= 0.0) {
      lambda = 0.5 * (lo + hi);
      continue;
    }

    if (std::fabs(f) < 1e-12 * (1.0 + std::sqrt(s2))) {
      break;
    }

    if (f > 0.0) lo = lambda;
    else         hi = lambda;

    double candidate = lambda - f / fp;
    if (!R_finite(candidate) || candidate <= lo || candidate >= hi) {
      candidate = 0.5 * (lo + hi);
    }

    if (std::fabs(candidate - lambda) <
        1e-13 * (1.0 + std::fabs(lambda))) {
      lambda = candidate;
      break;
    }

    lambda = candidate;
  }

  double ell = 0.0;
  for (int i = 0; i < q; ++i) {
    const double z = x[i] - mu;
    const double den = 1.0 + lambda * z;
    if (den <= 0.0 || !R_finite(den)) return R_PosInf;
    ell += 2.0 * std::log(den);
  }

  if (!R_finite(ell) || ell < -1e-10) return R_PosInf;
  return std::max(0.0, ell);
}

inline FeasCoeffs compute_feasible_coeffs(
    const std::vector<double>& y,
    const int L_requested,
    const int M) {

  const int N = static_cast<int>(y.size());
  const int L = std::min(L_requested, N - 1);

  double total = 0.0;
  double sumsq = 0.0;
  for (int t = 0; t < N; ++t) {
    total += y[t];
    sumsq += y[t] * y[t];
  }

  double future_sum = 0.0;
  double future_weighted = 0.0;
  for (int h = 1; h <= L; ++h) {
    future_sum += y[h];
    future_weighted += static_cast<double>(h) * y[h];
  }

  double sum_cross = 0.0;
  double sum_weighted_cross = 0.0;

  for (int t = 0; t < N - 1; ++t) {
    sum_cross += y[t] * future_sum;
    sum_weighted_cross += y[t] * future_weighted;

    const int new_index = t + L + 1;
    const double incoming = (new_index < N) ? y[new_index] : 0.0;
    const double old_future_sum = future_sum;

    future_weighted =
      future_weighted - old_future_sum +
      ((new_index < N) ? static_cast<double>(L) * incoming : 0.0);
    future_sum = old_future_sum - y[t + 1] + incoming;
  }

  double first_cumulative = 0.0;
  double last_cumulative = 0.0;
  double sum_lr = 0.0;
  double weighted_sum_lr = 0.0;

  for (int h = 1; h <= L; ++h) {
    first_cumulative += y[h - 1];
    last_cumulative += y[N - h];

    const double left_sum = total - last_cumulative;
    const double right_sum = total - first_cumulative;
    const double lr = left_sum + right_sum;

    sum_lr += lr;
    weighted_sum_lr += static_cast<double>(h) * lr;
  }

  const double Nd = static_cast<double>(N);
  const double Ld = static_cast<double>(L);
  const double sum_h = Ld * (Ld + 1.0) / 2.0;
  const double sum_h2 = Ld * (Ld + 1.0) * (2.0 * Ld + 1.0) / 6.0;
  const double sum_N_minus_h = Ld * Nd - sum_h;
  const double sum_h_N_minus_h = Nd * sum_h - sum_h2;

  FeasCoeffs out;
  out.sigma_c = sumsq / Nd + 2.0 * sum_cross / Nd;
  out.sigma_b = -2.0 * total / Nd - 2.0 * sum_lr / Nd;
  out.sigma_a = 1.0 + 2.0 * sum_N_minus_h / Nd;

  out.B2_c = 2.0 * sum_weighted_cross / Nd;
  out.B2_b = -2.0 * weighted_sum_lr / Nd;
  out.B2_a = 2.0 * sum_h_N_minus_h / Nd;

  out.L = L;
  out.M = M;
  out.N = N;
  return out;
}

inline FeasEval evaluate_feasible(
    const FeasCoeffs& c,
    const double mu) {

  FeasEval out;
  out.sigma2 = c.sigma_a * mu * mu + c.sigma_b * mu + c.sigma_c;
  out.B2 = c.B2_a * mu * mu + c.B2_b * mu + c.B2_c;

  out.sigma_nonpositive =
    (out.sigma2 <= 0.0 || !R_finite(out.sigma2)) ? 1 : 0;

  double b0 = 0.0;
  if (!out.sigma_nonpositive) {
    b0 = out.B2 / out.sigma2;
  }

  const double lower = -static_cast<double>(c.L);
  const double upper =  static_cast<double>(c.L);
  out.b = std::max(lower, std::min(b0, upper));

  out.trunc_active =
    (!out.sigma_nonpositive && (b0 < lower || b0 > upper)) ? 1 : 0;

  out.nu =
    (1.0 - out.b / static_cast<double>(c.M)) /
    (1.0 - out.b / static_cast<double>(c.N));

  return out;
}

inline double method_scale(
    const int method,
    const double nu_oracle,
    const double nu_feasible,
    const double factor_full,
    const double factor_leading) {

  if (method == 0) return 1.0;
  if (method == 1) return factor_full;
  if (method == 2) return factor_leading;
  if (method == 3) return nu_oracle;
  if (method == 4) return nu_feasible;
  if (method == 5) return nu_oracle * factor_full;
  if (method == 6) return nu_oracle * factor_leading;
  if (method == 7) return nu_feasible * factor_full;
  if (method == 8) return nu_feasible * factor_leading;

  return NA_REAL;
}

inline bool method_uses_feasible(const int method) {
  return method == 4 || method == 7 || method == 8;
}

inline double corrected_statistic_at_mu(
    const std::vector<double>& block_means,
    const FeasCoeffs& coeffs,
    const double mu,
    const int method,
    const double nu_oracle,
    const double factor_full,
    const double factor_leading) {

  const double lr = scalar_el_ratio_mean(block_means, mu);
  if (!R_finite(lr)) return R_PosInf;

  double nu_feasible = 1.0;
  if (method_uses_feasible(method)) {
    nu_feasible = evaluate_feasible(coeffs, mu).nu;
  }

  const double scale = method_scale(
    method,
    nu_oracle,
    nu_feasible,
    factor_full,
    factor_leading
  );

  if (!R_finite(scale) || scale <= 0.0) return R_PosInf;
  return scale * lr;
}

inline double find_endpoint_bisection(
    const std::vector<double>& block_means,
    const FeasCoeffs& coeffs,
    const double center,
    const double boundary,
    const bool left_side,
    const int method,
    const double nu_oracle,
    const double factor_full,
    const double factor_leading,
    const double crit,
    const int iterations,
    int& failed) {

  failed = 0;
  const double range = std::max(1e-12, std::fabs(boundary - center));
  const double eps = 1e-12 * (1.0 + range);
  const double outer = left_side ? boundary + eps : boundary - eps;

  double a = center;
  double b = outer;

  double fa = corrected_statistic_at_mu(
    block_means, coeffs, a, method,
    nu_oracle, factor_full, factor_leading
  ) - crit;

  double fb = corrected_statistic_at_mu(
    block_means, coeffs, b, method,
    nu_oracle, factor_full, factor_leading
  ) - crit;

  if (!R_finite(fa)) fa = -crit;
  if (!R_finite(fb)) fb = R_PosInf;

  if (!(fa <= 0.0 && fb >= 0.0)) {
    failed = 1;
    return NA_REAL;
  }

  for (int iter = 0; iter < iterations; ++iter) {
    const double mid = 0.5 * (a + b);
    double fm = corrected_statistic_at_mu(
      block_means, coeffs, mid, method,
      nu_oracle, factor_full, factor_leading
    ) - crit;

    if (!R_finite(fm)) fm = R_PosInf;

    if (fm <= 0.0) a = mid;
    else           b = mid;
  }

  return 0.5 * (a + b);
}

inline int count_components(const std::vector<int>& inside) {
  int components = 0;
  bool in_component = false;

  for (size_t i = 0; i < inside.size(); ++i) {
    if (inside[i] && !in_component) {
      ++components;
      in_component = true;
    } else if (!inside[i]) {
      in_component = false;
    }
  }
  return components;
}

inline double approximate_total_length_dense(
    const std::vector<double>& block_means,
    const FeasCoeffs& coeffs,
    const double xmin,
    const double xmax,
    const int method,
    const double nu_oracle,
    const double factor_full,
    const double factor_leading,
    const double crit,
    const int grid_points,
    int& components) {

  const int G = std::max(5, grid_points);
  const double eps = 1e-11 * (1.0 + std::fabs(xmax - xmin));
  const double left = xmin + eps;
  const double right = xmax - eps;

  std::vector<double> x(G);
  std::vector<double> f(G);
  std::vector<int> inside(G);

  for (int g = 0; g < G; ++g) {
    const double w = static_cast<double>(g) / static_cast<double>(G - 1);
    x[g] = left + w * (right - left);
    f[g] = corrected_statistic_at_mu(
      block_means, coeffs, x[g], method,
      nu_oracle, factor_full, factor_leading
    ) - crit;

    if (!R_finite(f[g])) f[g] = R_PosInf;
    inside[g] = (f[g] <= 0.0) ? 1 : 0;
  }

  components = count_components(inside);
  double total_length = 0.0;

  for (int g = 0; g < G - 1; ++g) {
    const double dx = x[g + 1] - x[g];
    const bool in1 = inside[g] == 1;
    const bool in2 = inside[g + 1] == 1;

    if (in1 && in2) {
      total_length += dx;
    } else if (in1 != in2) {
      const double a = std::fabs(f[g]);
      const double b = std::fabs(f[g + 1]);
      double fraction = 0.5;
      if (R_finite(a) && R_finite(b) && (a + b) > 0.0) {
        fraction = a / (a + b);
      }

      const double crossing = x[g] + fraction * dx;
      if (in1) total_length += crossing - x[g];
      else     total_length += x[g + 1] - crossing;
    }
  }

  return total_length;
}

// [[Rcpp::export]]
List simulate_master_chunk_cpp(
    const int B,
    const int B_ci,
    const int N,
    const IntegerVector M_values,
    const IntegerVector L_values,
    const NumericVector nu_oracle,
    const NumericVector factor_full,
    const NumericVector factor_leading,
    const NumericVector b_true,
    const double omega_N,
    const double phi,
    const int dist_code,
    const int burn_in,
    const double crit,
    const int bisection_iterations,
    const int coarse_grid_points,
    const int dense_grid_points) {

  RNGScope scope;

  if (B <= 0 || B_ci < 0 || B_ci > B || N <= 0 || burn_in < 0) {
    stop("Invalid simulation inputs.");
  }
  if (std::fabs(phi) >= 1.0) stop("Need |phi| < 1.");

  const int D = M_values.size();
  if (L_values.size() != D || nu_oracle.size() != D ||
      factor_full.size() != D || factor_leading.size() != D ||
      b_true.size() != D) {
    stop("Design vectors must have the same length.");
  }

  NumericMatrix point_cover(D, K_METHODS);
  NumericMatrix point_stat_sum(D, K_METHODS);
  NumericMatrix point_stat_n_finite(D, K_METHODS);
  NumericMatrix ci_subset_cover(D, K_METHODS);

  NumericMatrix pair_sum(D, K_PAIRS);
  NumericMatrix pair_sum2(D, K_PAIRS);

  NumericVector convex_count(D);
  NumericVector sigma_nonpositive_count(D);
  NumericVector truncation_count(D);
  NumericVector b_sum(D);
  NumericVector b_error_sq_sum(D);
  NumericVector nu_sum(D);
  NumericVector nu_error_sq_sum(D);

  NumericMatrix gaussian_ref_diff_sum(D, K_METHODS);
  NumericMatrix gaussian_ref_diff_sum2(D, K_METHODS);

  const int total_ci_columns = D * K_METHODS;
  NumericMatrix ci_total_length(B_ci, total_ci_columns);
  NumericMatrix ci_component_length(B_ci, total_ci_columns);
  NumericMatrix ci_disconnected(B_ci, total_ci_columns);
  NumericMatrix ci_nonregular(B_ci, total_ci_columns);
  NumericMatrix ci_failure(B_ci, total_ci_columns);

  std::fill(ci_total_length.begin(), ci_total_length.end(), NA_REAL);
  std::fill(ci_component_length.begin(), ci_component_length.end(), NA_REAL);

  std::vector<double> y(N);
  std::vector< std::vector<double> > block_means(D);

  for (int d = 0; d < D; ++d) {
    const int M = M_values[d];
    if (M <= 0 || N % M != 0) {
      stop("Every M must satisfy exact divisibility N = M Q.");
    }
    const int Q = N / M;
    if (Q < 2) stop("At least two blocks are required.");
    block_means[d].resize(Q);
  }

  const double innovation_scale = std::sqrt(1.0 - phi * phi);

  const int pair_left[K_PAIRS]  = {1, 3, 5, 5, 6};
  const int pair_right[K_PAIRS] = {2, 4, 6, 7, 8};

  for (int rep = 0; rep < B; ++rep) {
    double x_state = 0.0;

    if (dist_code == 1) {
      x_state = R::rnorm(0.0, 1.0);
    } else {
      for (int k = 0; k < burn_in; ++k) {
        x_state =
          phi * x_state +
          innovation_scale * draw_standard_innovation(dist_code);
      }
    }

    double full_sum = 0.0;
    for (int t = 0; t < N; ++t) {
      x_state =
        phi * x_state +
        innovation_scale * draw_standard_innovation(dist_code);
      y[t] = x_state;
      full_sum += x_state;
    }

    const double sample_mean = full_sum / static_cast<double>(N);
    const double gaussian_reference =
      full_sum * full_sum /
      (static_cast<double>(N) * omega_N);
    const int gaussian_reference_cover =
      (dist_code == 1 && gaussian_reference <= crit) ? 1 : 0;

    for (int d = 0; d < D; ++d) {
      const int M = M_values[d];
      const int Q = N / M;

      double xmin = R_PosInf;
      double xmax = R_NegInf;

      for (int i = 0; i < Q; ++i) {
        double s = 0.0;
        const int start = i * M;
        for (int j = 0; j < M; ++j) {
          s += y[start + j];
        }
        const double bm = s / static_cast<double>(M);
        block_means[d][i] = bm;
        xmin = std::min(xmin, bm);
        xmax = std::max(xmax, bm);
      }

      const FeasCoeffs coeffs =
        compute_feasible_coeffs(y, L_values[d], M);
      const FeasEval feas0 = evaluate_feasible(coeffs, 0.0);

      const double raw0 = scalar_el_ratio_mean(block_means[d], 0.0);
      const bool convex_fail = !R_finite(raw0);
      if (convex_fail) convex_count[d] += 1.0;

      sigma_nonpositive_count[d] +=
        static_cast<double>(feas0.sigma_nonpositive);
      truncation_count[d] +=
        static_cast<double>(feas0.trunc_active);
      b_sum[d] += feas0.b;
      b_error_sq_sum[d] +=
        (feas0.b - b_true[d]) * (feas0.b - b_true[d]);
      nu_sum[d] += feas0.nu;
      nu_error_sq_sum[d] +=
        (feas0.nu - nu_oracle[d]) *
        (feas0.nu - nu_oracle[d]);

      double stats[K_METHODS];
      int covered[K_METHODS];

      for (int m = 0; m < K_METHODS; ++m) {
        const double scale = method_scale(
          m,
          nu_oracle[d],
          feas0.nu,
          factor_full[d],
          factor_leading[d]
        );

        stats[m] = R_finite(raw0) ? scale * raw0 : R_PosInf;
        covered[m] = (R_finite(stats[m]) && stats[m] <= crit) ? 1 : 0;

        point_cover(d, m) += static_cast<double>(covered[m]);
        if (R_finite(stats[m])) {
          point_stat_sum(d, m) += stats[m];
          point_stat_n_finite(d, m) += 1.0;
        }

        if (rep < B_ci) {
          ci_subset_cover(d, m) += static_cast<double>(covered[m]);
        }

        if (dist_code == 1) {
          const double diff =
            static_cast<double>(covered[m] - gaussian_reference_cover);
          gaussian_ref_diff_sum(d, m) += diff;
          gaussian_ref_diff_sum2(d, m) += diff * diff;
        }
      }

      for (int p = 0; p < K_PAIRS; ++p) {
        const double diff = static_cast<double>(
          covered[pair_left[p]] - covered[pair_right[p]]
        );
        pair_sum(d, p) += diff;
        pair_sum2(d, p) += diff * diff;
      }

      if (rep < B_ci) {
        for (int m = 0; m < K_METHODS; ++m) {
          const int column = d * K_METHODS + m;
          int fail_left = 0;
          int fail_right = 0;

          const double lower = find_endpoint_bisection(
            block_means[d], coeffs, sample_mean, xmin, true,
            m, nu_oracle[d], factor_full[d], factor_leading[d],
            crit, bisection_iterations, fail_left
          );

          const double upper = find_endpoint_bisection(
            block_means[d], coeffs, sample_mean, xmax, false,
            m, nu_oracle[d], factor_full[d], factor_leading[d],
            crit, bisection_iterations, fail_right
          );

          if (fail_left || fail_right || !R_finite(lower) ||
              !R_finite(upper) || upper < lower) {
            ci_failure(rep, column) = 1.0;
            continue;
          }

          const double central_length = upper - lower;
          ci_component_length(rep, column) = central_length;
          ci_total_length(rep, column) = central_length;

          if (method_uses_feasible(m)) {
            const int G = std::max(7, coarse_grid_points);
            const double eps = 1e-11 * (1.0 + std::fabs(xmax - xmin));
            const double grid_left = xmin + eps;
            const double grid_right = xmax - eps;
            std::vector<int> inside(G);
            bool central_violation = false;

            for (int g = 0; g < G; ++g) {
              const double w =
                static_cast<double>(g) / static_cast<double>(G - 1);
              const double mu = grid_left + w * (grid_right - grid_left);
              double value = corrected_statistic_at_mu(
                block_means[d], coeffs, mu, m,
                nu_oracle[d], factor_full[d], factor_leading[d]
              );
              inside[g] = (R_finite(value) && value <= crit) ? 1 : 0;

              const bool expected_inside =
                (mu >= lower && mu <= upper);
              if (inside[g] != static_cast<int>(expected_inside)) {
                central_violation = true;
              }
            }

            const int coarse_components = count_components(inside);
            if (coarse_components > 1 || central_violation) {
              ci_nonregular(rep, column) = 1.0;
              int dense_components = 0;
              const double total_length = approximate_total_length_dense(
                block_means[d], coeffs, xmin, xmax, m,
                nu_oracle[d], factor_full[d], factor_leading[d],
                crit, dense_grid_points, dense_components
              );

              if (R_finite(total_length) && total_length >= 0.0) {
                ci_total_length(rep, column) = total_length;
              } else {
                ci_failure(rep, column) = 1.0;
              }

              if (dense_components > 1) {
                ci_disconnected(rep, column) = 1.0;
              }
            }
          }
        }
      }
    }
  }

  return List::create(
    _["point_cover"] = point_cover,
    _["point_stat_sum"] = point_stat_sum,
    _["point_stat_n_finite"] = point_stat_n_finite,
    _["ci_subset_cover"] = ci_subset_cover,
    _["pair_sum"] = pair_sum,
    _["pair_sum2"] = pair_sum2,
    _["convex_count"] = convex_count,
    _["sigma_nonpositive_count"] = sigma_nonpositive_count,
    _["truncation_count"] = truncation_count,
    _["b_sum"] = b_sum,
    _["b_error_sq_sum"] = b_error_sq_sum,
    _["nu_sum"] = nu_sum,
    _["nu_error_sq_sum"] = nu_error_sq_sum,
    _["gaussian_ref_diff_sum"] = gaussian_ref_diff_sum,
    _["gaussian_ref_diff_sum2"] = gaussian_ref_diff_sum2,
    _["ci_total_length"] = ci_total_length,
    _["ci_component_length"] = ci_component_length,
    _["ci_disconnected"] = ci_disconnected,
    _["ci_nonregular"] = ci_nonregular,
    _["ci_failure"] = ci_failure
  );
}
')

# 5. Chunk and summary helpers

method_names <- c(
  "raw",
  "K_asym_full",
  "K_leading",
  "V_oracle",
  "V_feasible",
  "VK_asym_full_oracle",
  "VK_leading_oracle",
  "VK_asym_full_feasible",
  "F_feasible_leading"
)

method_labels <- c(
  raw = "Raw",
  K_asym_full = "Kitamura: asymptotic full",
  K_leading = "Kitamura: 3/(2Q)",
  V_oracle = "Variance: oracle",
  V_feasible = "Variance: feasible",
  VK_asym_full_oracle = "Oracle V + asymptotic full K",
  VK_leading_oracle = "Oracle V + 3/(2Q)",
  VK_asym_full_feasible = "Feasible V + asymptotic full K",
  F_feasible_leading = "Feasible V + 3/(2Q)"
)

comparison_pairs <- list(
  K_asym_full_minus_K_leading = c("K_asym_full", "K_leading"),
  V_oracle_minus_V_feasible = c("V_oracle", "V_feasible"),
  VK_asym_full_oracle_minus_VK_leading_oracle =
    c("VK_asym_full_oracle", "VK_leading_oracle"),
  VK_asym_full_oracle_minus_VK_asym_full_feasible =
    c("VK_asym_full_oracle", "VK_asym_full_feasible"),
  VK_leading_oracle_minus_F_feasible_leading =
    c("VK_leading_oracle", "F_feasible_leading")
)

safe_label <- function(x) {
  gsub("[^A-Za-z0-9_-]+", "_", x)
}

chunk_seed <- function(base_seed, chunk_index) {
  as.integer(
    (as.double(base_seed) + chunk_index - 1) %%
      .Machine$integer.max
  )
}

sample_variance_from_sums <- function(sum_x, sum_x2, n) {
  if (n <= 1) return(NA_real_)
  pmax(0, (sum_x2 - sum_x^2 / n) / (n - 1))
}

safe_quantile <- function(x, probability) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(NA_real_)
  unname(quantile(x, probs = probability, names = FALSE, type = 8))
}

# 6. Run one generated-data scenario

run_one_scenario <- function(scenario_row) {
  scenario_id <- as.character(scenario_row$scenario_id)
  innovation <- as.character(scenario_row$innovation)
  dist_code <- as.integer(scenario_row$dist_code)
  phi <- as.numeric(scenario_row$phi)
  N <- as.integer(scenario_row$N)
  base_seed <- as.integer(scenario_row$seed)

  rows <- design_table[design_table$scenario_id == scenario_id, ]
  rows <- rows[order(rows$M), ]
  row.names(rows) <- NULL

  M_values <- as.integer(rows$M)
  L_values <- as.integer(rows$L)
  D <- nrow(rows)
  K <- length(method_names)

  burn_in <- if (dist_code == 1L) {
    0L
  } else {
    stationary_burn_in(phi, STATIONARY_TAIL_VARIANCE_TOL)
  }

  n_chunks <- ceiling(B_POINT / CHUNK_SIZE)

  zero_DK <- matrix(0, nrow = D, ncol = K)
  point_cover <- zero_DK
  point_stat_sum <- zero_DK
  point_stat_n_finite <- zero_DK
  ci_subset_cover <- zero_DK
  gaussian_ref_diff_sum <- zero_DK
  gaussian_ref_diff_sum2 <- zero_DK

  P <- length(comparison_pairs)
  pair_sum <- matrix(0, nrow = D, ncol = P)
  pair_sum2 <- matrix(0, nrow = D, ncol = P)

  convex_count <- numeric(D)
  sigma_nonpositive_count <- numeric(D)
  truncation_count <- numeric(D)
  b_sum <- numeric(D)
  b_error_sq_sum <- numeric(D)
  nu_sum <- numeric(D)
  nu_error_sq_sum <- numeric(D)

  ci_total_chunks <- list()
  ci_component_chunks <- list()
  ci_disconnected_chunks <- list()
  ci_nonregular_chunks <- list()
  ci_failure_chunks <- list()

  point_done <- 0L
  ci_done <- 0L
  crit <- qchisq(NOMINAL_COVERAGE, df = 1)

  for (j in seq_len(n_chunks)) {
    Bj <- min(CHUNK_SIZE, B_POINT - point_done)
    Bci_j <- if (COMPUTE_CONFIDENCE_INTERVALS) {
      max(0L, min(Bj, B_CI - ci_done))
    } else {
      0L
    }

    ci_mode_tag <- if (COMPUTE_CONFIDENCE_INTERVALS) "ci_on" else "ci_off"

    chunk_file <- file.path(
      RESULTS_DIR,
      "chunks",
      sprintf(
        "%s_%s_%s_Bpoint_%d_Bci_%d_chunksize_%d_bisect_%d_chunk_%04d.rds",
        CODE_VERSION,
        ci_mode_tag,
        safe_label(scenario_id),
        B_POINT,
        B_CI,
        CHUNK_SIZE,
        CI_BISECTION_ITERATIONS,
        j
      )
    )

    if (REUSE_SAVED_CHUNKS && file.exists(chunk_file)) {
      out <- readRDS(chunk_file)
    } else {
      sj <- chunk_seed(base_seed, j)
      set.seed(sj)
      out <- simulate_master_chunk_cpp(
        B = Bj,
        B_ci = Bci_j,
        N = N,
        M_values = M_values,
        L_values = L_values,
        nu_oracle = rows$nu_oracle,
        factor_full = rows$factor_K_asym_full,
        factor_leading = rows$factor_K_leading,
        b_true = rows$b_true,
        omega_N = rows$omega_N[1L],
        phi = phi,
        dist_code = dist_code,
        burn_in = burn_in,
        crit = crit,
        bisection_iterations = CI_BISECTION_ITERATIONS,
        coarse_grid_points = CI_COARSE_GRID_POINTS,
        dense_grid_points = CI_DENSE_GRID_POINTS
      )

      saveRDS(out, chunk_file, compress = FALSE)
    }

    point_cover <- point_cover + out$point_cover
    point_stat_sum <- point_stat_sum + out$point_stat_sum
    point_stat_n_finite <-
      point_stat_n_finite + out$point_stat_n_finite
    ci_subset_cover <- ci_subset_cover + out$ci_subset_cover
    pair_sum <- pair_sum + out$pair_sum
    pair_sum2 <- pair_sum2 + out$pair_sum2
    convex_count <- convex_count + out$convex_count
    sigma_nonpositive_count <-
      sigma_nonpositive_count + out$sigma_nonpositive_count
    truncation_count <- truncation_count + out$truncation_count
    b_sum <- b_sum + out$b_sum
    b_error_sq_sum <- b_error_sq_sum + out$b_error_sq_sum
    nu_sum <- nu_sum + out$nu_sum
    nu_error_sq_sum <- nu_error_sq_sum + out$nu_error_sq_sum
    gaussian_ref_diff_sum <-
      gaussian_ref_diff_sum + out$gaussian_ref_diff_sum
    gaussian_ref_diff_sum2 <-
      gaussian_ref_diff_sum2 + out$gaussian_ref_diff_sum2

    if (Bci_j > 0L) {
      ci_total_chunks[[length(ci_total_chunks) + 1L]] <-
        out$ci_total_length
      ci_component_chunks[[length(ci_component_chunks) + 1L]] <-
        out$ci_component_length
      ci_disconnected_chunks[[length(ci_disconnected_chunks) + 1L]] <-
        out$ci_disconnected
      ci_nonregular_chunks[[length(ci_nonregular_chunks) + 1L]] <-
        out$ci_nonregular
      ci_failure_chunks[[length(ci_failure_chunks) + 1L]] <-
        out$ci_failure
    }

    point_done <- point_done + Bj
    ci_done <- ci_done + Bci_j
  }

  stopifnot(point_done == B_POINT)
  stopifnot(ci_done == if (COMPUTE_CONFIDENCE_INTERVALS) B_CI else 0L)

  if (ci_done > 0L) {
    ci_total <- do.call(rbind, ci_total_chunks)
    ci_component <- do.call(rbind, ci_component_chunks)
    ci_disconnected <- do.call(rbind, ci_disconnected_chunks)
    ci_nonregular <- do.call(rbind, ci_nonregular_chunks)
    ci_failure <- do.call(rbind, ci_failure_chunks)
  } else {
    ci_total <- matrix(numeric(0), nrow = 0, ncol = D * K)
    ci_component <- ci_total
    ci_disconnected <- ci_total
    ci_nonregular <- ci_total
    ci_failure <- ci_total
  }

  result_rows <- list()
  diagnostic_rows <- list()
  comparison_rows <- list()

  for (d in seq_len(D)) {
    raw_column <- (d - 1L) * K + 1L
    raw_lengths <- if (ci_done > 0L) ci_total[, raw_column] else numeric(0)
    raw_mean_length <- if (any(is.finite(raw_lengths))) {
      mean(raw_lengths[is.finite(raw_lengths)])
    } else {
      NA_real_
    }

    for (m in seq_len(K)) {
      column <- (d - 1L) * K + m
      coverage <- point_cover[d, m] / B_POINT
      coverage_mcse <- sqrt(coverage * (1 - coverage) / B_POINT)

      if (innovation == "gaussian") {
        paired_error <- gaussian_ref_diff_sum[d, m] / B_POINT
        paired_var <- sample_variance_from_sums(
          gaussian_ref_diff_sum[d, m],
          gaussian_ref_diff_sum2[d, m],
          B_POINT
        )
        paired_error_mcse <- sqrt(paired_var / B_POINT)
      } else {
        paired_error <- NA_real_
        paired_error_mcse <- NA_real_
      }

      if (ci_done > 0L) {
        lengths <- ci_total[, column]
        component_lengths <- ci_component[, column]
        finite_lengths <- lengths[is.finite(lengths)]
        finite_components <- component_lengths[is.finite(component_lengths)]
        n_length <- length(finite_lengths)

        mean_length <- if (n_length > 0L) mean(finite_lengths) else NA_real_
        sd_length <- if (n_length > 1L) sd(finite_lengths) else NA_real_
        mean_length_mcse <- if (n_length > 1L) {
          sd_length / sqrt(n_length)
        } else {
          NA_real_
        }

        median_length <- safe_quantile(finite_lengths, 0.5)
        q25_length <- safe_quantile(finite_lengths, 0.25)
        q75_length <- safe_quantile(finite_lengths, 0.75)
        q90_length <- safe_quantile(finite_lengths, 0.90)
        q95_length <- safe_quantile(finite_lengths, 0.95)

        mean_component_length <- if (length(finite_components) > 0L) {
          mean(finite_components)
        } else {
          NA_real_
        }

        ci_subset_coverage <- ci_subset_cover[d, m] / B_CI
        ci_subset_coverage_mcse <- sqrt(
          ci_subset_coverage * (1 - ci_subset_coverage) / B_CI
        )
        disconnected_rate <- mean(ci_disconnected[, column] == 1)
        nonregular_rate <- mean(ci_nonregular[, column] == 1)
        interval_failure_rate <- mean(ci_failure[, column] == 1)
      } else {
        n_length <- 0L
        mean_length <- NA_real_
        sd_length <- NA_real_
        mean_length_mcse <- NA_real_
        median_length <- NA_real_
        q25_length <- NA_real_
        q75_length <- NA_real_
        q90_length <- NA_real_
        q95_length <- NA_real_
        mean_component_length <- NA_real_
        ci_subset_coverage <- NA_real_
        ci_subset_coverage_mcse <- NA_real_
        disconnected_rate <- NA_real_
        nonregular_rate <- NA_real_
        interval_failure_rate <- NA_real_
      }

      result_rows[[length(result_rows) + 1L]] <- data.frame(
        design_id = rows$design_id[d],
        scenario_id = scenario_id,
        innovation = innovation,
        innovation_label = rows$innovation_label[d],
        assumption_status = rows$assumption_status[d],
        severity_group = rows$severity_group[d],
        phi = phi,
        phi_label = rows$phi_label[d],
        N = N,
        M = rows$M[d],
        Q = rows$Q[d],
        L = rows$L[d],
        B_point = B_POINT,
        B_CI = B_CI,
        statistic = method_names[m],
        coverage = coverage,
        coverage_error = coverage - NOMINAL_COVERAGE,
        coverage_mcse = coverage_mcse,
        gaussian_paired_coverage_error = paired_error,
        gaussian_paired_error_mcse = paired_error_mcse,
        rejection = 1 - coverage,
        mean_statistic_finite =
          point_stat_sum[d, m] / point_stat_n_finite[d, m],
        ci_subset_coverage = ci_subset_coverage,
        ci_subset_coverage_mcse = ci_subset_coverage_mcse,
        n_finite_lengths = n_length,
        mean_length = mean_length,
        mean_length_mcse = mean_length_mcse,
        sd_length = sd_length,
        median_length = median_length,
        q25_length = q25_length,
        q75_length = q75_length,
        q90_length = q90_length,
        q95_length = q95_length,
        mean_component_length = mean_component_length,
        normalized_mean_length =
          mean_length / rows$first_order_length_benchmark[d],
        relative_mean_length_to_raw =
          mean_length / raw_mean_length,
        disconnected_rate = disconnected_rate,
        nonregular_rate = nonregular_rate,
        interval_failure_rate = interval_failure_rate,
        convex_failure_rate = convex_count[d] / B_POINT,
        stringsAsFactors = FALSE
      )
    }

    diagnostic_rows[[length(diagnostic_rows) + 1L]] <- data.frame(
      design_id = rows$design_id[d],
      scenario_id = scenario_id,
      innovation = innovation,
      innovation_label = rows$innovation_label[d],
      assumption_status = rows$assumption_status[d],
      phi = phi,
      phi_label = rows$phi_label[d],
      N = N,
      M = rows$M[d],
      Q = rows$Q[d],
      L = rows$L[d],
      B_point = B_POINT,
      burn_in = burn_in,
      b_true = rows$b_true[d],
      mean_b_hat = b_sum[d] / B_POINT,
      bias_b_hat = b_sum[d] / B_POINT - rows$b_true[d],
      rmse_b_hat = sqrt(b_error_sq_sum[d] / B_POINT),
      nu_true = rows$nu_oracle[d],
      mean_nu_hat = nu_sum[d] / B_POINT,
      bias_nu_hat = nu_sum[d] / B_POINT - rows$nu_oracle[d],
      rmse_nu_hat = sqrt(nu_error_sq_sum[d] / B_POINT),
      sigma_nonpositive_rate =
        sigma_nonpositive_count[d] / B_POINT,
      truncation_rate = truncation_count[d] / B_POINT,
      convex_failure_rate = convex_count[d] / B_POINT,
      stringsAsFactors = FALSE
    )

    for (p in seq_along(comparison_pairs)) {
      pair <- comparison_pairs[[p]]
      left_m <- match(pair[1L], method_names)
      right_m <- match(pair[2L], method_names)
      point_mean_difference <- pair_sum[d, p] / B_POINT
      point_var_difference <- sample_variance_from_sums(
        pair_sum[d, p], pair_sum2[d, p], B_POINT
      )

      if (ci_done > 0L) {
        left_col <- (d - 1L) * K + left_m
        right_col <- (d - 1L) * K + right_m
        length_difference <-
          ci_total[, left_col] - ci_total[, right_col]
        length_difference <-
          length_difference[is.finite(length_difference)]

        mean_length_difference <- if (length(length_difference) > 0L) {
          mean(length_difference)
        } else {
          NA_real_
        }
        length_difference_mcse <- if (length(length_difference) > 1L) {
          sd(length_difference) / sqrt(length(length_difference))
        } else {
          NA_real_
        }
      } else {
        mean_length_difference <- NA_real_
        length_difference_mcse <- NA_real_
      }

      comparison_rows[[length(comparison_rows) + 1L]] <- data.frame(
        design_id = rows$design_id[d],
        scenario_id = scenario_id,
        innovation = innovation,
        innovation_label = rows$innovation_label[d],
        phi = phi,
        N = N,
        M = rows$M[d],
        Q = rows$Q[d],
        comparison = names(comparison_pairs)[p],
        paired_coverage_difference = point_mean_difference,
        paired_coverage_difference_mcse =
          sqrt(point_var_difference / B_POINT),
        paired_mean_length_difference = mean_length_difference,
        paired_mean_length_difference_mcse = length_difference_mcse,
        stringsAsFactors = FALSE
      )
    }
  }

  out_summary <- list(
    results = do.call(rbind, result_rows),
    diagnostics = do.call(rbind, diagnostic_rows),
    comparisons = do.call(rbind, comparison_rows)
  )

  saveRDS(
    out_summary,
    file.path(
      RESULTS_DIR,
      "objects",
      paste0(safe_label(scenario_id), "_summary.rds")
    )
  )

  out_summary
}

# 7. Run all scenarios

scenario_results <- vector("list", nrow(scenario_table))
names(scenario_results) <- scenario_table$scenario_id

for (i in seq_len(nrow(scenario_table))) {
  scenario_results[[i]] <- run_one_scenario(
    scenario_table[i, , drop = FALSE]
  )
}

results_unique <- do.call(
  rbind,
  lapply(scenario_results, `[[`, "results")
)
diagnostics_unique <- do.call(
  rbind,
  lapply(scenario_results, `[[`, "diagnostics")
)
comparisons_unique <- do.call(
  rbind,
  lapply(scenario_results, `[[`, "comparisons")
)

row.names(results_unique) <- NULL
row.names(diagnostics_unique) <- NULL
row.names(comparisons_unique) <- NULL

# Attach module membership. A unique design can appear in several modules.
results_by_module <- merge(
  membership,
  results_unique,
  by = "design_id",
  all.x = TRUE,
  sort = FALSE,
  suffixes = c("_module", "_result")
)

diagnostics_by_module <- merge(
  membership,
  diagnostics_unique,
  by = "design_id",
  all.x = TRUE,
  sort = FALSE,
  suffixes = c("_module", "_result")
)

comparisons_by_module <- merge(
  membership,
  comparisons_unique,
  by = "design_id",
  all.x = TRUE,
  sort = FALSE,
  suffixes = c("_module", "_result")
)

write.csv(
  diagnostics_by_module,
  file.path(RESULTS_DIR, "tables", "diagnostics_by_module.csv"),
  row.names = FALSE
)
write.csv(
  comparisons_unique,
  file.path(RESULTS_DIR, "tables", "paired_comparisons_unique_designs.csv"),
  row.names = FALSE
)
write.csv(
  comparisons_by_module,
  file.path(RESULTS_DIR, "tables", "paired_comparisons_by_module.csv"),
  row.names = FALSE
)

for (module_now in unique(membership$module)) {
  write.csv(
    results_by_module[results_by_module$module == module_now, ],
    file.path(
      RESULTS_DIR,
      "tables",
      paste0("results_", module_now, ".csv")
    ),
    row.names = FALSE
  )
}



# 8. Dependence summaries

# Descriptive names used in the released dependence-grid tables.
write.csv(
  results_unique,
  file.path(RESULTS_DIR, "tables", "phi_grid_results_all_methods.csv"),
  row.names = FALSE
)
write.csv(
  diagnostics_unique,
  file.path(RESULTS_DIR, "tables", "phi_grid_diagnostics.csv"),
  row.names = FALSE
)

core_results <- results_unique[
  results_unique$statistic %in% CORE_METHODS,
]
write.csv(
  core_results,
  file.path(RESULTS_DIR, "tables", "phi_grid_results_core_methods.csv"),
  row.names = FALSE
)

# Add regime labels to the unique result and diagnostic tables.
regime_key <- unique(
  membership[, c("design_id", "regime", "regime_label", "alpha_nominal", "N_label")]
)
results_labeled <- merge(
  regime_key,
  results_unique,
  by = "design_id",
  all.x = TRUE,
  sort = FALSE
)
diagnostics_labeled <- merge(
  regime_key,
  diagnostics_unique,
  by = "design_id",
  all.x = TRUE,
  sort = FALSE
)

write.csv(
  results_labeled,
  file.path(RESULTS_DIR, "tables", "phi_grid_results_labeled.csv"),
  row.names = FALSE
)
write.csv(
  diagnostics_labeled,
  file.path(RESULTS_DIR, "tables", "phi_grid_diagnostics_labeled.csv"),
  row.names = FALSE
)

value_at_phi <- function(phi, value, target) {
  index <- which(abs(phi - target) < 1e-10)
  if (length(index) == 0L) return(NA_real_)
  value[index[1L]]
}

summarize_neutralization_group <- function(dd) {
  dd <- dd[order(dd$phi), ]
  fit <- if (nrow(dd) >= 2L) lm(coverage ~ phi, data = dd) else NULL
  length_fit <- if (nrow(dd) >= 2L) {
    lm(normalized_mean_length ~ phi, data = dd)
  } else {
    NULL
  }

  data.frame(
    regime = dd$regime[1L],
    regime_label = dd$regime_label[1L],
    alpha_nominal = dd$alpha_nominal[1L],
    N = dd$N[1L],
    M = dd$M[1L],
    Q = dd$Q[1L],
    statistic = dd$statistic[1L],
    n_phi = nrow(dd),
    coverage_phi_000 = value_at_phi(dd$phi, dd$coverage, 0.00),
    coverage_phi_050 = value_at_phi(dd$phi, dd$coverage, 0.50),
    coverage_phi_080 = value_at_phi(dd$phi, dd$coverage, 0.80),
    coverage_phi_090 = value_at_phi(dd$phi, dd$coverage, 0.90),
    coverage_phi_095 = value_at_phi(dd$phi, dd$coverage, 0.95),
    coverage_range = max(dd$coverage, na.rm = TRUE) -
      min(dd$coverage, na.rm = TRUE),
    coverage_sd_across_phi = sd(dd$coverage, na.rm = TRUE),
    coverage_slope_per_unit_phi = if (is.null(fit)) NA_real_ else coef(fit)[2L],
    drop_phi000_to_095 =
      value_at_phi(dd$phi, dd$coverage, 0.95) -
      value_at_phi(dd$phi, dd$coverage, 0.00),
    drop_phi050_to_095 =
      value_at_phi(dd$phi, dd$coverage, 0.95) -
      value_at_phi(dd$phi, dd$coverage, 0.50),
    mean_absolute_coverage_error =
      mean(abs(dd$coverage - NOMINAL_COVERAGE), na.rm = TRUE),
    max_absolute_coverage_error =
      max(abs(dd$coverage - NOMINAL_COVERAGE), na.rm = TRUE),
    normalized_length_phi_000 =
      value_at_phi(dd$phi, dd$normalized_mean_length, 0.00),
    normalized_length_phi_050 =
      value_at_phi(dd$phi, dd$normalized_mean_length, 0.50),
    normalized_length_phi_080 =
      value_at_phi(dd$phi, dd$normalized_mean_length, 0.80),
    normalized_length_phi_090 =
      value_at_phi(dd$phi, dd$normalized_mean_length, 0.90),
    normalized_length_phi_095 =
      value_at_phi(dd$phi, dd$normalized_mean_length, 0.95),
    normalized_length_range =
      max(dd$normalized_mean_length, na.rm = TRUE) -
      min(dd$normalized_mean_length, na.rm = TRUE),
    normalized_length_slope_per_unit_phi =
      if (is.null(length_fit)) NA_real_ else coef(length_fit)[2L],
    stringsAsFactors = FALSE
  )
}

neutralization_split <- split(
  results_labeled,
  interaction(
    results_labeled$regime,
    results_labeled$N,
    results_labeled$statistic,
    drop = TRUE
  )
)
neutralization_summary <- do.call(
  rbind,
  lapply(neutralization_split, summarize_neutralization_group)
)
row.names(neutralization_summary) <- NULL
neutralization_summary <- neutralization_summary[
  order(
    neutralization_summary$N,
    neutralization_summary$alpha_nominal,
    match(neutralization_summary$statistic, method_names)
  ),
]
write.csv(
  neutralization_summary,
  file.path(RESULTS_DIR, "tables", "dependence_neutralization_summary.csv"),
  row.names = FALSE
)

# Direct comparison of the dependence ranges for the principal procedures.
neutralization_core <- neutralization_summary[
  neutralization_summary$statistic %in% CORE_METHODS,
]
write.csv(
  neutralization_core,
  file.path(RESULTS_DIR, "tables", "dependence_neutralization_core.csv"),
  row.names = FALSE
)

# 9. Diagnostic figures

results_labeled$statistic <- factor(
  results_labeled$statistic,
  levels = method_names
)
results_labeled$stat_label <- factor(
  method_labels[as.character(results_labeled$statistic)],
  levels = unname(method_labels[method_names])
)
results_labeled$regime_label <- factor(
  results_labeled$regime_label,
  levels = c("M = N^(1/3)", "M = N^(1/2)", "M = N^(2/3)")
)
results_labeled$N_label <- factor(
  results_labeled$N_label,
  levels = paste0("N = ", K_GRID^6)
)

diagnostics_labeled$regime_label <- factor(
  diagnostics_labeled$regime_label,
  levels = c("M = N^(1/3)", "M = N^(1/2)", "M = N^(2/3)")
)
diagnostics_labeled$N_label <- factor(
  diagnostics_labeled$N_label,
  levels = paste0("N = ", K_GRID^6)
)

coverage_theme <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "vertical",
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 9)
    )
}

save_plot <- function(plot_object, filename, width, height) {
  ggplot2::ggsave(
    filename = file.path(RESULTS_DIR, "figures", filename),
    plot = plot_object,
    width = width,
    height = height,
    dpi = 320
  )
}

# 9A. Mechanism figure: raw, Kitamura-only, and variance-only procedures

dd_mechanism <- results_labeled[
  as.character(results_labeled$statistic) %in% MECHANISM_METHODS,
]

p_mechanism <- ggplot2::ggplot(
  dd_mechanism,
  ggplot2::aes(
    x = phi,
    y = coverage,
    group = stat_label,
    linetype = stat_label,
    shape = stat_label
  )
) +
  ggplot2::geom_hline(
    yintercept = NOMINAL_COVERAGE,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  ggplot2::geom_line(linewidth = 0.55) +
  ggplot2::geom_point(size = 1.35) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = coverage - 2 * coverage_mcse,
      ymax = coverage + 2 * coverage_mcse
    ),
    width = 0,
    linewidth = 0.18,
    alpha = 0.6
  ) +
  ggplot2::scale_x_continuous(
    breaks = c(seq(0, 0.9, by = 0.1), 0.95),
    limits = c(0, 0.95)
  ) +
  ggplot2::facet_grid(regime_label ~ N_label) +
  ggplot2::labs(
    title = "Dependence sensitivity: Kitamura correction versus variance calibration",
    subtitle = "Gaussian AR(1), phi from 0 to 0.95; the same series is reused across block regimes",
    x = expression(phi),
    y = "Empirical coverage",
    linetype = NULL,
    shape = NULL
  ) +
  coverage_theme()

save_plot(
  p_mechanism,
  "phi_grid_coverage_mechanism.png",
  width = 15,
  height = 11
)

# 9B. Final-procedure figure

dd_final <- results_labeled[
  as.character(results_labeled$statistic) %in% FINAL_METHODS,
]

p_final <- ggplot2::ggplot(
  dd_final,
  ggplot2::aes(
    x = phi,
    y = coverage,
    group = stat_label,
    linetype = stat_label,
    shape = stat_label
  )
) +
  ggplot2::geom_hline(
    yintercept = NOMINAL_COVERAGE,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  ggplot2::geom_line(linewidth = 0.65) +
  ggplot2::geom_point(size = 1.5) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = coverage - 2 * coverage_mcse,
      ymax = coverage + 2 * coverage_mcse
    ),
    width = 0,
    linewidth = 0.18,
    alpha = 0.6
  ) +
  ggplot2::scale_x_continuous(
    breaks = c(seq(0, 0.9, by = 0.1), 0.95),
    limits = c(0, 0.95)
  ) +
  ggplot2::facet_grid(regime_label ~ N_label) +
  ggplot2::labs(
    title = "Dependence sensitivity of the main BEL procedures",
    subtitle = "Raw and Kitamura-only BEL are compared with oracle and feasible combined calibration",
    x = expression(phi),
    y = "Empirical coverage",
    linetype = NULL,
    shape = NULL
  ) +
  coverage_theme()

save_plot(
  p_final,
  "phi_grid_coverage_final_procedures.png",
  width = 15,
  height = 11
)

# 9C. All nine coverage curves, for the supplement

p_all <- ggplot2::ggplot(
  results_labeled,
  ggplot2::aes(
    x = phi,
    y = coverage,
    group = stat_label,
    linetype = stat_label,
    shape = stat_label
  )
) +
  ggplot2::geom_hline(
    yintercept = NOMINAL_COVERAGE,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  ggplot2::geom_line(linewidth = 0.42) +
  ggplot2::geom_point(size = 1.05) +
  ggplot2::scale_x_continuous(
    breaks = c(seq(0, 0.9, by = 0.1), 0.95),
    limits = c(0, 0.95)
  ) +
  ggplot2::facet_grid(regime_label ~ N_label) +
  ggplot2::labs(
    title = "Dependence grid: all nine BEL statistics",
    subtitle = "Gaussian AR(1), phi from 0 to 0.95",
    x = expression(phi),
    y = "Empirical coverage",
    linetype = NULL,
    shape = NULL
  ) +
  coverage_theme()

save_plot(
  p_all,
  "phi_grid_coverage_all_nine.png",
  width = 16,
  height = 12
)

# 9D. Normalized mean confidence-interval length

dd_length <- results_labeled[
  as.character(results_labeled$statistic) %in% CORE_METHODS,
]

p_length <- ggplot2::ggplot(
  dd_length,
  ggplot2::aes(
    x = phi,
    y = normalized_mean_length,
    group = stat_label,
    linetype = stat_label,
    shape = stat_label
  )
) +
  ggplot2::geom_hline(
    yintercept = 1,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  ggplot2::geom_line(linewidth = 0.58) +
  ggplot2::geom_point(size = 1.35) +
  ggplot2::scale_x_continuous(
    breaks = c(seq(0, 0.9, by = 0.1), 0.95),
    limits = c(0, 0.95)
  ) +
  ggplot2::facet_grid(regime_label ~ N_label) +
  ggplot2::labs(
    title = "Dependence sensitivity of normalized mean interval length",
    subtitle = "Length divided by the first-order Gaussian benchmark for the same phi and N",
    x = expression(phi),
    y = "Mean length / first-order benchmark",
    linetype = NULL,
    shape = NULL
  ) +
  coverage_theme()

save_plot(
  p_length,
  "phi_grid_normalized_mean_length.png",
  width = 15,
  height = 11
)

# 9E. Coverage error around zero

p_error <- ggplot2::ggplot(
  dd_length,
  ggplot2::aes(
    x = phi,
    y = coverage_error,
    group = stat_label,
    linetype = stat_label,
    shape = stat_label
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  ggplot2::geom_line(linewidth = 0.58) +
  ggplot2::geom_point(size = 1.35) +
  ggplot2::scale_x_continuous(
    breaks = c(seq(0, 0.9, by = 0.1), 0.95),
    limits = c(0, 0.95)
  ) +
  ggplot2::facet_grid(regime_label ~ N_label) +
  ggplot2::labs(
    title = "Coverage error across the dependence grid",
    subtitle = "Empirical coverage minus 0.95",
    x = expression(phi),
    y = "Coverage error",
    linetype = NULL,
    shape = NULL
  ) +
  coverage_theme()

save_plot(
  p_error,
  "phi_grid_coverage_error_core.png",
  width = 15,
  height = 11
)

# 9F. Oracle and feasible variance-factor tracking

nu_long <- rbind(
  data.frame(
    diagnostics_labeled[, c("design_id", "regime_label", "N_label", "phi", "M", "Q")],
    factor_type = "Oracle variance factor",
    factor_value = diagnostics_labeled$nu_true,
    stringsAsFactors = FALSE
  ),
  data.frame(
    diagnostics_labeled[, c("design_id", "regime_label", "N_label", "phi", "M", "Q")],
    factor_type = "Mean feasible variance factor",
    factor_value = diagnostics_labeled$mean_nu_hat,
    stringsAsFactors = FALSE
  )
)

nu_long$factor_type <- factor(
  nu_long$factor_type,
  levels = c("Oracle variance factor", "Mean feasible variance factor")
)

p_nu <- ggplot2::ggplot(
  nu_long,
  ggplot2::aes(
    x = phi,
    y = factor_value,
    group = factor_type,
    linetype = factor_type,
    shape = factor_type
  )
) +
  ggplot2::geom_line(linewidth = 0.65) +
  ggplot2::geom_point(size = 1.45) +
  ggplot2::scale_x_continuous(
    breaks = c(seq(0, 0.9, by = 0.1), 0.95),
    limits = c(0, 0.95)
  ) +
  ggplot2::facet_grid(regime_label ~ N_label) +
  ggplot2::labs(
    title = "Oracle and feasible variance factors across dependence strength",
    subtitle = "Separation of the curves identifies where feasible calibration ceases to mimic the oracle",
    x = expression(phi),
    y = expression(nu[N]),
    linetype = NULL,
    shape = NULL
  ) +
  coverage_theme()

save_plot(
  p_nu,
  "phi_grid_variance_factor_tracking.png",
  width = 15,
  height = 11
)

# 9G. True and estimated b = B2/sigma^2

b_long <- rbind(
  data.frame(
    diagnostics_labeled[, c("design_id", "regime_label", "N_label", "phi", "M", "Q")],
    b_type = "True b",
    b_value = diagnostics_labeled$b_true,
    stringsAsFactors = FALSE
  ),
  data.frame(
    diagnostics_labeled[, c("design_id", "regime_label", "N_label", "phi", "M", "Q")],
    b_type = "Mean estimated b",
    b_value = diagnostics_labeled$mean_b_hat,
    stringsAsFactors = FALSE
  )
)

b_long$b_type <- factor(
  b_long$b_type,
  levels = c("True b", "Mean estimated b")
)

p_b <- ggplot2::ggplot(
  b_long,
  ggplot2::aes(
    x = phi,
    y = b_value,
    group = b_type,
    linetype = b_type,
    shape = b_type
  )
) +
  ggplot2::geom_line(linewidth = 0.65) +
  ggplot2::geom_point(size = 1.45) +
  ggplot2::scale_x_continuous(
    breaks = c(seq(0, 0.9, by = 0.1), 0.95),
    limits = c(0, 0.95)
  ) +
  ggplot2::facet_grid(regime_label ~ N_label, scales = "free_y") +
  ggplot2::labs(
    title = "Tracking the dependence constant b = B2/sigma^2",
    subtitle = "The true value diverges rapidly as phi approaches one",
    x = expression(phi),
    y = expression(B[2] / sigma^2),
    linetype = NULL,
    shape = NULL
  ) +
  coverage_theme()

save_plot(
  p_b,
  "phi_grid_b_tracking.png",
  width = 15,
  height = 11
)

# 9H. Leading error components

population_labeled <- merge(
  regime_key,
  design_table,
  by = "design_id",
  all.x = TRUE,
  sort = FALSE
)
population_labeled$regime_label <- factor(
  population_labeled$regime_label,
  levels = c("M = N^(1/3)", "M = N^(1/2)", "M = N^(2/3)")
)
population_labeled$N_label <- factor(
  population_labeled$N_label,
  levels = paste0("N = ", K_GRID^6)
)

component_long <- rbind(
  data.frame(
    population_labeled[, c("regime_label", "N_label", "phi", "M", "Q")],
    component = "Variance mismatch: b/M",
    value = population_labeled$variance_component,
    stringsAsFactors = FALSE
  ),
  data.frame(
    population_labeled[, c("regime_label", "N_label", "phi", "M", "Q")],
    component = "Finite-Q term: 3/(2Q)",
    value = population_labeled$EL_component,
    stringsAsFactors = FALSE
  )
)

component_long$component <- factor(
  component_long$component,
  levels = c("Variance mismatch: b/M", "Finite-Q term: 3/(2Q)")
)

p_components <- ggplot2::ggplot(
  component_long,
  ggplot2::aes(
    x = phi,
    y = value,
    group = component,
    linetype = component,
    shape = component
  )
) +
  ggplot2::geom_line(linewidth = 0.65) +
  ggplot2::geom_point(size = 1.35) +
  ggplot2::scale_x_continuous(
    breaks = c(seq(0, 0.9, by = 0.1), 0.95),
    limits = c(0, 0.95)
  ) +
  ggplot2::facet_grid(regime_label ~ N_label, scales = "free_y") +
  ggplot2::labs(
    title = "The two leading theoretical error components",
    subtitle = "The finite-Q component is constant in phi; the variance mismatch rises sharply with persistence",
    x = expression(phi),
    y = "Leading component magnitude",
    linetype = NULL,
    shape = NULL
  ) +
  coverage_theme()

save_plot(
  p_components,
  "phi_grid_leading_error_components.png",
  width = 15,
  height = 11
)

# Record the run settings

manifest <- c(
  paste("Code version:", CODE_VERSION),
  paste("Mode:", MODE),
  paste("Master seed:", MASTER_SEED),
  paste("RNG kind:", paste(RNGkind(), collapse = ", ")),
  paste("Point replications per scenario:", B_POINT),
  paste("CI replications per scenario:", B_CI),
  paste("Chunk size:", CHUNK_SIZE),
  paste("Reuse saved chunks:", REUSE_SAVED_CHUNKS),
  paste("Nominal coverage:", NOMINAL_COVERAGE),
  paste("Phi grid:", paste(PHI_GRID, collapse = ", ")),
  paste("k values:", paste(K_GRID, collapse = ", ")),
  paste("N values:", paste(K_GRID^6, collapse = ", ")),
  paste("Block regimes: N^(1/3), N^(1/2), N^(2/3)"),
  paste("CI bisection iterations:", CI_BISECTION_ITERATIONS),
  paste("CI coarse grid points:", CI_COARSE_GRID_POINTS),
  paste("CI dense grid points:", CI_DENSE_GRID_POINTS),
  paste("Number of unique designs:", nrow(design_table)),
  paste("Number of generated-data scenarios:", nrow(scenario_table)),
  paste(
    "Bandwidth:",
    "L_N=min(floor(log(N)log(log(N))),floor(M/2))"
  ),
  paste(
    "Feasible CI implementation:",
    "nu_hat(mu) recomputed at each candidate mean"
  ),
  paste(
    "Results directory:",
    normalizePath(RESULTS_DIR, winslash = "/", mustWork = FALSE)
  ),
  paste("Completed UTC:", format(Sys.time(), tz = "UTC", usetz = TRUE))
)

writeLines(manifest, file.path(RESULTS_DIR, "run_manifest.txt"))
writeLines(
  capture.output(sessionInfo()),
  file.path(RESULTS_DIR, "sessionInfo.txt")
)

readme <- c(
  "BEL PHI-GRID SIMULATION: THREE BLOCK REGIMES",
  "",
  "Scientific target:",
  "  Compare how raw, Kitamura-only, variance-only, and combined BEL",
  "  coverage changes as phi increases from 0 to 0.95.",
  "",
  "Design:",
  "  Gaussian stationary AR(1) with Var(Y_t)=1 and mu_0=0.",
  "  N in {64,729,4096}.",
  "  For every N: M=N^(1/3), N^(1/2), N^(2/3).",
  "  The same generated series is reused across the three block lengths.",
  "",
  "Primary tables:",
  "  phi_grid_results_all_methods.csv",
  "  phi_grid_results_core_methods.csv",
  "  phi_grid_results_labeled.csv",
  "  phi_grid_diagnostics.csv",
  "  dependence_neutralization_summary.csv",
  "  dependence_neutralization_core.csv",
  "  population_design_constants.csv",
  "  paired_comparisons_unique_designs.csv",
  "  design_membership.csv",
  "  scenario_seeds.csv",
  "",
  "Coverage uses B_POINT replications.",
  "Interval length uses the first B_CI replications from the same streams.",
  "For feasible intervals, nu_hat(mu) is recomputed at every candidate mu.",
  "All nine methods are retained in the tables; main figures use focused",
  "subsets to reveal the dependence-neutralization mechanism."
)
writeLines(readme, file.path(RESULTS_DIR, "README.txt"))

script_argument <- grep("^--file=", commandArgs(), value = TRUE)
if (length(script_argument) == 1L) {
  script_path <- sub("^--file=", "", script_argument)
  if (file.exists(script_path)) {
    file.copy(
      script_path,
      file.path(RESULTS_DIR, basename(script_path)),
      overwrite = TRUE
    )
  }
}
