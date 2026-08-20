// Fast simulation core for the final AR(1) BEL simulation study.
//
// Data can be generated from stable AR(1), AR(2), MA(1), or ARMA(1,1)
// processes. The publication analysis uses a common deterministic block
// length to compare raw BEL, the feasible model-free variance calibration
// used in the companion paper, the exact AR(1) variance calibration, and
// the repaired AR(1) higher-order refinement. Conventional benchmark tests
// are computed from the same simulated series.
//
// The code uses RcppParallel for replication-level parallelism and a
// deterministic per-replication random-number stream. Hence results do not
// depend on the number of threads.
//
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::plugins(cpp11)]]

#include <Rcpp.h>
#include <RcppParallel.h>

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <limits>
#include <string>
#include <vector>

using Rcpp::IntegerVector;
using Rcpp::List;
using Rcpp::Named;
using Rcpp::NumericMatrix;
using Rcpp::NumericVector;
using Rcpp::CharacterVector;
using RcppParallel::RMatrix;
using RcppParallel::RVector;
using RcppParallel::Worker;
using RcppParallel::parallelFor;

namespace ar1bel {

constexpr double PI = 3.141592653589793238462643383279502884;
constexpr double Z975 = 1.95996398454005423552;
constexpr double INF = std::numeric_limits<double>::infinity();
constexpr double EPS = 1e-14;

// Deterministic fast RNG: SplitMix64 + Box--Muller normal generator.

inline std::uint64_t mix64(std::uint64_t x) {
  x += 0x9E3779B97F4A7C15ULL;
  x = (x ^ (x >> 30)) * 0xBF58476D1CE4E5B9ULL;
  x = (x ^ (x >> 27)) * 0x94D049BB133111EBULL;
  return x ^ (x >> 31);
}

inline std::uint64_t replication_seed(std::uint64_t base_seed,
                                      std::uint64_t replication_id) {
  return mix64(base_seed ^ mix64(replication_id + 0xD1B54A32D192ED03ULL));
}

class FastRNG {
 public:
  explicit FastRNG(std::uint64_t seed)
      : state_(mix64(seed)), has_spare_(false), spare_(0.0) {}

  inline std::uint64_t next_u64() {
    state_ += 0x9E3779B97F4A7C15ULL;
    std::uint64_t z = state_;
    z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ULL;
    z = (z ^ (z >> 27)) * 0x94D049BB133111EBULL;
    return z ^ (z >> 31);
  }

  inline double uniform() {
    // 53 random bits, in [0,1).
    return static_cast<double>(next_u64() >> 11) * 1.0 / 9007199254740992.0;
  }

  inline double uniform_open() {
    double u = uniform();
    if (u <= 0.0) u = 1.0 / 9007199254740992.0;
    return u;
  }

  inline int uniform_int(int n) {
    if (n <= 1) return 0;
    const std::uint64_t bound = static_cast<std::uint64_t>(n);
    const std::uint64_t threshold = (0ULL - bound) % bound;
    std::uint64_t r;
    do {
      r = next_u64();
    } while (r < threshold);
    return static_cast<int>(r % bound);
  }

  inline double normal() {
    if (has_spare_) {
      has_spare_ = false;
      return spare_;
    }
    const double u1 = uniform_open();
    const double u2 = uniform();
    const double radius = std::sqrt(-2.0 * std::log(u1));
    const double angle = 2.0 * PI * u2;
    spare_ = radius * std::sin(angle);
    has_spare_ = true;
    return radius * std::cos(angle);
  }

  inline int geometric(double p) {
    // Support {1,2,...}; E[L] = 1/p.
    if (p >= 1.0) return 1;
    if (p <= 0.0) return std::numeric_limits<int>::max();
    const double u = uniform_open();
    const double value = std::floor(std::log(u) / std::log1p(-p));
    if (!std::isfinite(value) || value > 2147483000.0) {
      return 2147483000;
    }
    return 1 + static_cast<int>(value);
  }

 private:
  std::uint64_t state_;
  bool has_spare_;
  double spare_;
};

// Basic utilities.

inline double vec_mean(const std::vector<double>& x) {
  long double s = 0.0L;
  for (double v : x) s += static_cast<long double>(v);
  return static_cast<double>(s / static_cast<long double>(x.size()));
}

inline double sample_variance_centered(const std::vector<double>& x,
                                       double xbar) {
  if (x.size() < 2) return 0.0;
  long double ss = 0.0L;
  for (double v : x) {
    const long double d = static_cast<long double>(v - xbar);
    ss += d * d;
  }
  return static_cast<double>(ss / static_cast<long double>(x.size() - 1));
}

inline double project_phi(double phi, int n) {
  const double lower = -1.0 + 2.0 / static_cast<double>(n);
  const double upper =  1.0 - 2.0 / static_cast<double>(n);
  return std::max(lower, std::min(upper, phi));
}

inline double burg_phi(const std::vector<double>& x, double xbar) {
  long double num = 0.0L;
  long double den = 0.0L;
  const int n = static_cast<int>(x.size());
  for (int t = 1; t < n; ++t) {
    const long double a = static_cast<long double>(x[t] - xbar);
    const long double b = static_cast<long double>(x[t - 1] - xbar);
    num += 2.0L * a * b;
    den += a * a + b * b;
  }
  if (den <= 0.0L) return 0.0;
  return static_cast<double>(num / den);
}

inline double bias_reduced_phi(double phi_raw, int n) {
  const double nn = static_cast<double>(n);
  const double corrected = (phi_raw + 1.0 / nn) / (1.0 - 3.0 / nn);
  return project_phi(corrected, n);
}

struct ResidualEstimates {
  double sigma2;
  double skewness;
  double excess_kurtosis;
};

inline ResidualEstimates residual_estimates(const std::vector<double>& x,
                                             double xbar,
                                             double phi_hat) {
  const int n = static_cast<int>(x.size());
  const int m = n - 1;
  std::vector<double> e(m);
  long double esum = 0.0L;
  for (int t = 1; t < n; ++t) {
    const double value = (x[t] - xbar) - phi_hat * (x[t - 1] - xbar);
    e[t - 1] = value;
    esum += static_cast<long double>(value);
  }
  const double ebar = static_cast<double>(esum / static_cast<long double>(m));

  long double s2 = 0.0L;
  long double s3 = 0.0L;
  long double s4 = 0.0L;
  for (double value : e) {
    const long double d = static_cast<long double>(value - ebar);
    const long double d2 = d * d;
    s2 += d2;
    s3 += d2 * d;
    s4 += d2 * d2;
  }

  const double sigma2 = (m > 1)
      ? static_cast<double>(s2 / static_cast<long double>(m - 1))
      : 0.0;

  const long double m2 = s2 / static_cast<long double>(m);
  if (m2 <= 1e-24L || m < 4) {
    return {sigma2, 0.0, 0.0};
  }

  const long double m3 = s3 / static_cast<long double>(m);
  const long double m4 = s4 / static_cast<long double>(m);
  const double g1 = static_cast<double>(m3 / std::pow(m2, 1.5L));
  const double g2 = static_cast<double>(m4 / (m2 * m2) - 3.0L);

  // Adjusted Fisher--Pearson skewness and adjusted excess kurtosis.
  const double mm = static_cast<double>(m);
  const double G1 = std::sqrt(mm * (mm - 1.0)) / (mm - 2.0) * g1;
  const double G2 = (mm - 1.0) / ((mm - 2.0) * (mm - 3.0)) *
                    ((mm + 1.0) * g2 + 6.0);
  return {sigma2, G1, G2};
}

// Stable ARMA(2,1) process utilities. The designs used by the R driver are
// AR(1), AR(2), MA(1), and ARMA(1,1), all nested in
//
//   X_t = a1 X_{t-1} + a2 X_{t-2} + eps_t + theta eps_{t-1}.
//
// The innovation standard deviation is chosen so that Var(X_t)=1. The
// impulse-response representation is also used to compute the true long-run
// variance, the pseudo-true AR(1) coefficient gamma(1)/gamma(0), the exact
// finite-sample variance ratio Omega_M/Omega_N, and b=B2/Omega.

inline double gamma_shape_gt_one(FastRNG& rng, double shape) {
  const double d = shape - 1.0 / 3.0;
  const double c = 1.0 / std::sqrt(9.0 * d);
  for (;;) {
    const double x = rng.normal();
    double v = 1.0 + c * x;
    if (v <= 0.0) continue;
    v = v * v * v;
    const double u = rng.uniform_open();
    const double x2 = x * x;
    if (u < 1.0 - 0.0331 * x2 * x2) return d * v;
    if (std::log(u) < 0.5 * x2 + d * (1.0 - v + std::log(v))) {
      return d * v;
    }
  }
}

inline double smooth_compact_innovation(FastRNG& rng) {
  constexpr double TARGET_A = 1.0e-4;
  constexpr double TARGET_B = 50.0;
  constexpr double BETA_ALPHA = 1.0769901810352462;
  constexpr double BETA_BETA = 55.71573028791429;
  constexpr double LOG_BETA_NORMALIZER = -4.370263457666309;
  constexpr double LOG_ENVELOPE = -53.95;
  constexpr double TARGET_MEAN = 0.018963525116287967;
  constexpr double TARGET_SD = 0.017941780687536192;

  for (;;) {
    const double gx = gamma_shape_gt_one(rng, BETA_ALPHA);
    const double gy = gamma_shape_gt_one(rng, BETA_BETA);
    const double u = gx / (gx + gy);
    if (!(u > 0.0 && u < 1.0)) continue;
    const double log_target = -TARGET_A / u - TARGET_B / (1.0 - u);
    const double log_proposal =
        (BETA_ALPHA - 1.0) * std::log(u) +
        (BETA_BETA - 1.0) * std::log1p(-u) - LOG_BETA_NORMALIZER;
    if (std::log(rng.uniform_open()) <=
        log_target - log_proposal - LOG_ENVELOPE) {
      return (u - TARGET_MEAN) / TARGET_SD;
    }
  }
}

inline double draw_unit_innovation(FastRNG& rng, int innovation_code) {
  if (innovation_code == 1) return rng.normal();
  if (innovation_code == 2) return smooth_compact_innovation(rng);
  if (innovation_code == 3) return -std::log(rng.uniform_open()) - 1.0;
  if (innovation_code == 4) {
    const double z = rng.normal();
    return (z * z - 1.0) / std::sqrt(2.0);
  }
  return rng.normal();
}

struct ProcessTheory {
  double innovation_sd;
  double true_lrv;
  double pseudo_phi;
  double ar1_implied_lrv;
  double ar1_lrv_ratio;
  double B2;
  double b;
  std::vector<double> gamma;
};

inline std::vector<double> impulse_response(double a1,
                                            double a2,
                                            double theta) {
  const int max_terms = 200000;
  const int quiet_required = 100;
  std::vector<double> psi;
  psi.reserve(1024);
  psi.push_back(1.0);
  psi.push_back(a1 + theta);
  int quiet = 0;
  for (int j = 2; j < max_terms; ++j) {
    const double value = a1 * psi[j - 1] + a2 * psi[j - 2];
    psi.push_back(value);
    if (std::abs(value) < 1e-15) ++quiet;
    else quiet = 0;
    if (j > 100 && quiet >= quiet_required) break;
  }
  if (static_cast<int>(psi.size()) >= max_terms) {
    Rcpp::stop("ARMA coefficients do not appear to define a stable process");
  }
  return psi;
}

inline ProcessTheory process_theory(double a1,
                                    double a2,
                                    double theta,
                                    int max_lag = 4096) {
  const std::vector<double> psi = impulse_response(a1, a2, theta);
  const int J = static_cast<int>(psi.size());
  long double sumsq = 0.0L;
  long double sumpsi = 0.0L;
  for (double v : psi) {
    sumsq += static_cast<long double>(v) * v;
    sumpsi += static_cast<long double>(v);
  }
  if (!(sumsq > 0.0L)) Rcpp::stop("invalid impulse-response variance");
  const double innovation_var = 1.0 / static_cast<double>(sumsq);
  const double innovation_sd = std::sqrt(innovation_var);

  const int H = std::max(1, max_lag);
  std::vector<double> gamma(H + 1, 0.0);
  gamma[0] = 1.0;
  for (int h = 1; h <= H; ++h) {
    if (h >= J) break;
    long double total = 0.0L;
    for (int j = 0; j + h < J; ++j) {
      total += static_cast<long double>(psi[j]) * psi[j + h];
    }
    gamma[h] = innovation_var * static_cast<double>(total);
  }

  const double true_lrv = innovation_var *
      static_cast<double>(sumpsi * sumpsi);
  const double pseudo_phi = gamma[1];
  if (!(std::abs(pseudo_phi) < 1.0)) {
    Rcpp::stop("pseudo-true AR(1) coefficient is outside (-1,1)");
  }
  const double ar1_implied_lrv = (1.0 + pseudo_phi) /
                                  (1.0 - pseudo_phi);
  const double ratio = ar1_implied_lrv / true_lrv;
  long double B2 = 0.0L;
  for (int h = 1; h <= H; ++h) {
    B2 += 2.0L * static_cast<long double>(h) * gamma[h];
  }
  const double B2d = static_cast<double>(B2);
  return {innovation_sd, true_lrv, pseudo_phi, ar1_implied_lrv,
          ratio, B2d, B2d / true_lrv, gamma};
}

inline double omega_r(const ProcessTheory& theory, int r) {
  if (r <= 1) return 1.0;
  long double out = theory.gamma[0];
  const int H = std::min(r - 1,
      static_cast<int>(theory.gamma.size()) - 1);
  for (int h = 1; h <= H; ++h) {
    out += 2.0L * (1.0L - static_cast<long double>(h) / r) *
           theory.gamma[h];
  }
  return static_cast<double>(out);
}

inline double population_variance_ratio(const ProcessTheory& theory,
                                        int M,
                                        int nstar) {
  const double denominator = omega_r(theory, nstar);
  if (!(denominator > 0.0)) return 1.0;
  return omega_r(theory, M) / denominator;
}

inline void simulate_process(std::vector<double>& x,
                             double a1,
                             double a2,
                             double theta,
                             int innovation_code,
                             double innovation_sd,
                             double process_mean,
                             int burn_in,
                             FastRNG& rng) {
  const int n = static_cast<int>(x.size());
  const int total = n + std::max(0, burn_in);
  double xlag1 = 0.0;
  double xlag2 = 0.0;
  double epslag1 = 0.0;
  for (int t = 0; t < total; ++t) {
    const double eps = innovation_sd *
        draw_unit_innovation(rng, innovation_code);
    const double state = a1 * xlag1 + a2 * xlag2 +
                         eps + theta * epslag1;
    xlag2 = xlag1;
    xlag1 = state;
    epslag1 = eps;
    if (t >= burn_in) x[t - burn_in] = state + process_mean;
  }
}

// Exact AR(1) finite-partial-sum variance factor.

inline double D_value_sum(int r, double phi) {
  long double out = 1.0L;
  long double ph = static_cast<long double>(phi);
  long double p = ph;
  for (int h = 1; h < r; ++h) {
    out += 2.0L * (1.0L - static_cast<long double>(h) /
                  static_cast<long double>(r)) * p;
    p *= ph;
  }
  return static_cast<double>(out);
}

inline double D_value(int r, double phi) {
  if (r <= 1) return 1.0;
  if (std::abs(phi) < 1e-12) return 1.0;
  if (std::abs(1.0 - phi) < 1e-5 || phi < -0.9) return D_value_sum(r, phi);

  const long double p = static_cast<long double>(phi);
  const long double one_minus = 1.0L - p;
  const long double power = std::pow(p, r);
  const long double value = (1.0L + p) / one_minus -
      2.0L * p * (1.0L - power) /
      (static_cast<long double>(r) * one_minus * one_minus);
  const double out = static_cast<double>(value);
  if (!std::isfinite(out) || out <= 0.0) return D_value_sum(r, phi);
  return out;
}

inline double variance_factor(double phi, int M, int nstar) {
  const double denominator = D_value(nstar, phi);
  if (!(denominator > 0.0)) return 1.0;
  return D_value(M, phi) / denominator;
}

// Repaired AR(1)-specific higher-order coefficient.
//
// The function name is retained for backward compatibility with the R
// validation wrapper.  The implementation no longer uses the discarded
// normalization-dependent finite-Q grouped contraction.  The Gaussian base
// is the finite-M compact-stability coefficient
//
//   M * {2 - 1/(2 nu_NM) + 4 vartheta_NM^2},
//
// whose expansion is 3M/2 - b_phi/2 + smaller terms.  The non-Gaussian part
// uses the exact standardized third and fourth cumulants of one AR(1) block.

inline double kitamura_coefficient(double phi,
                                   int M,
                                   int nstar,
                                   double gamma3,
                                   double gamma4) {
  if (M < 1 || nstar < M) return INF;
  const int Q = nstar / M;
  if (Q < 2) return INF;
  if (!(std::abs(phi) < 1.0)) return INF;

  const long double p = static_cast<long double>(phi);
  const long double DM = D_value(M, phi);
  const long double one_minus = 1.0L - p;
  const long double one_minus_p2 = 1.0L - p * p;
  if (!(DM > 0.0L) || !(one_minus_p2 > 0.0L)) return INF;

  const long double pM = std::pow(p, M);
  const long double v = 1.0L - pM;

  long double G[5];
  G[0] = static_cast<long double>(M);
  for (int k = 1; k <= 4; ++k) {
    const long double pk = std::pow(p, k);
    const long double den = 1.0L - pk;
    if (std::abs(static_cast<double>(den)) < 1e-14) return INF;
    G[k] = pk * (1.0L - std::pow(p, k * M)) / den;
  }

  const long double d3 = one_minus * one_minus * one_minus;
  const long double U3 =
      (v * v * v * std::pow(p, 3) / (1.0L - std::pow(p, 3)) +
       G[0] - 3.0L * G[1] + 3.0L * G[2] - G[3]) / d3;

  const long double d4 = d3 * one_minus;
  const long double U4 =
      (v * v * v * v * std::pow(p, 4) / (1.0L - std::pow(p, 4)) +
       G[0] - 4.0L * G[1] + 6.0L * G[2] - 4.0L * G[3] + G[4]) / d4;

  const long double H2 = static_cast<long double>(M) * DM / one_minus_p2;
  if (!(H2 > 0.0L)) return INF;

  const long double eta3 = static_cast<long double>(gamma3) * U3 /
      std::pow(H2, 1.5L);
  const long double eta4 = static_cast<long double>(gamma4) * U4 /
      (H2 * H2);

  const long double nu = static_cast<long double>(
      variance_factor(phi, M, nstar));
  if (!(nu > 0.0L)) return INF;

  long double vartheta = 0.0L;
  if (std::abs(phi) >= 1e-12) {
    vartheta = p * v * v /
        (static_cast<long double>(M) * one_minus * one_minus * DM);
  }

  const long double aG = static_cast<long double>(M) *
      (2.0L - 1.0L / (2.0L * nu) + 4.0L * vartheta * vartheta);

  const long double out = aG +
      static_cast<long double>(M) *
      (0.5L * eta4 - eta3 * eta3 / 3.0L);
  return static_cast<double>(out);
}

// Blockwise empirical likelihood statistic.

inline double bel_from_blocks(const std::vector<double>& g) {
  double max_positive = 0.0;
  double min_negative = 0.0;
  bool positive = false;
  bool negative = false;
  for (double value : g) {
    if (value > 0.0) {
      positive = true;
      max_positive = std::max(max_positive, value);
    }
    if (value < 0.0) {
      negative = true;
      min_negative = std::min(min_negative, value);
    }
  }
  if (!positive || !negative) return INF;

  double lower = -1.0 / max_positive + 1e-13;
  double upper = -1.0 / min_negative - 1e-13;
  for (int iter = 0; iter < 90; ++iter) {
    const double mid = 0.5 * (lower + upper);
    long double score = 0.0L;
    for (double value : g) {
      score += static_cast<long double>(value) /
               static_cast<long double>(1.0 + mid * value);
    }
    if (score > 0.0L) lower = mid;
    else upper = mid;
  }
  const double lambda = 0.5 * (lower + upper);
  long double lr = 0.0L;
  for (double value : g) {
    const double arg = 1.0 + lambda * value;
    if (!(arg > 0.0)) return INF;
    lr += 2.0L * std::log(arg);
  }
  const double out = static_cast<double>(lr);
  return (out >= -1e-10 && std::isfinite(out)) ? std::max(0.0, out) : INF;
}

inline double bel_statistic(const std::vector<double>& x,
                            double mu0,
                            int M) {
  const int n = static_cast<int>(x.size());
  const int Q = n / M;
  if (Q < 2) return INF;
  const int nstar = Q * M;
  std::vector<double> g(Q, 0.0);
  for (int i = 0; i < nstar; ++i) g[i / M] += x[i] - mu0;
  return bel_from_blocks(g);
}

// Exact Gaussian AR(1) block-sum simulation.  This avoids generating all
// N=MQ observations when only the Q block sums are needed.  With stationary
// marginal variance normalized to one, the block transition is generated
// from the joint Gaussian innovation in the next boundary state and block
// sum.  The resulting block-sum vector has exactly the AR(1) covariance used
// by the BEL statistic.

struct GaussianBlockLRWorker : public Worker {
  int replications;
  int M;
  int Q;
  double phi;
  double pM;
  double c_state;
  double sd_y;
  double cov_sy;
  double sd_s_cond;
  std::uint64_t base_seed;
  RVector<double> output;

  GaussianBlockLRWorker(int replications_, int M_, int Q_, double phi_,
                        double pM_, double c_state_, double sd_y_,
                        double cov_sy_, double sd_s_cond_,
                        std::uint64_t base_seed_, NumericVector output_)
      : replications(replications_), M(M_), Q(Q_), phi(phi_), pM(pM_),
        c_state(c_state_), sd_y(sd_y_), cov_sy(cov_sy_),
        sd_s_cond(sd_s_cond_), base_seed(base_seed_), output(output_) {}

  void operator()(std::size_t begin, std::size_t end) {
    std::vector<double> g(Q);
    for (std::size_t r = begin; r < end; ++r) {
      FastRNG rng(replication_seed(base_seed, static_cast<std::uint64_t>(r)));
      double y = rng.normal();
      for (int i = 0; i < Q; ++i) {
        const double z1 = rng.normal();
        const double z2 = rng.normal();
        const double e_y = sd_y * z1;
        const double e_s = (sd_y > 0.0 ? cov_sy / sd_y * z1 : 0.0) +
                           sd_s_cond * z2;
        g[i] = c_state * y + e_s;
        y = pM * y + e_y;
      }
      output[r] = bel_from_blocks(g);
    }
  }
};

// General weak-dependence variance calibration from the companion theory.
// For the effective sample nstar=QM, define
//   H=min(floor(log nstar log log nstar), floor(M/2)),
//   sigmahat^2 = gammahat(0)+2 sum_{h=1}^H gammahat(h),
//   B2hat = 2 sum_{h=1}^H h gammahat(h),
//   bhat = trunc_{[-H,H]}(B2hat/sigmahat^2),
// and nuhat=(1-bhat/M)/(1-bhat/nstar).
// Autocovariances use the null-centered observations X_t-mu0 and divisor
// nstar, exactly matching the stated plug-in estimator.

struct GeneralCalibration {
  double sigma2_hat;
  double B2_hat;
  double b_hat;
  double nu_hat;
  int H;
};


struct GeneralFullCorrection {
  GeneralCalibration vc;
  double Lambda3_hat;
  double Lambda4_hat;
  double gamma3_lr_hat;
  double gamma4_lr_hat;
  double a_full_hat;
  double full_multiplier_hat;
  int sigma_nonpositive;
};

inline GeneralCalibration general_calibration(
    const std::vector<double>& x,
    double mu0,
    int M) {
  const int n = static_cast<int>(x.size());
  const int Q = n / M;
  const int nstar = Q * M;
  if (Q < 2 || nstar < 8) return {0.0, 0.0, 0.0, 1.0, 0};

  int log_band = 1;
  if (nstar > 3) {
    const double value = std::log(static_cast<double>(nstar)) *
                         std::log(std::log(static_cast<double>(nstar)));
    if (std::isfinite(value)) log_band =
        std::max(1, static_cast<int>(std::floor(value)));
  }
  const int H = std::max(1, std::min(log_band, M / 2));
  std::vector<double> y(nstar);
  for (int i = 0; i < nstar; ++i) y[i] = x[i] - mu0;

  long double sigma2 = 0.0L;
  long double B2 = 0.0L;
  for (int h = 0; h <= H; ++h) {
    long double total = 0.0L;
    for (int t = 0; t < nstar - h; ++t) {
      total += static_cast<long double>(y[t]) * y[t + h];
    }
    const double gamma = static_cast<double>(
        total / static_cast<long double>(nstar));
    if (h == 0) sigma2 += gamma;
    else {
      sigma2 += 2.0L * gamma;
      B2 += 2.0L * static_cast<long double>(h) * gamma;
    }
  }

  const double sigma2d = static_cast<double>(sigma2);
  const double B2d = static_cast<double>(B2);
  double b0 = 0.0;
  if (sigma2d > 0.0 && std::isfinite(sigma2d) && std::isfinite(B2d)) {
    b0 = B2d / sigma2d;
  }
  const double bhat = std::max(-static_cast<double>(H),
                       std::min(static_cast<double>(H), b0));
  const double numerator = 1.0 - bhat / static_cast<double>(M);
  const double denominator = 1.0 - bhat / static_cast<double>(nstar);
  double nu = 1.0;
  if (numerator > 0.0 && denominator > 0.0) nu = numerator / denominator;
  return {sigma2d, B2d, bhat, nu, H};
}


// Model-free leading corrected comparator.
//
// The previous order-N^{-1} model-free coefficient is not used in the
// repaired manuscript.  We retain this structure and function name so the
// existing simulation interface remains stable.  The returned multiplier is
// the universal leading scalar EL factor 1 - 3/(2Q), combined elsewhere with
// the model-free variance calibration.
inline GeneralFullCorrection general_full_correction(
    const std::vector<double>& x,
    double mu0,
    int M) {
  const GeneralCalibration vc = general_calibration(x, mu0, M);
  const int n = static_cast<int>(x.size());
  const int Q = n / M;
  const int nstar = Q * M;

  if (Q < 2 || nstar < 1) {
    return {vc, 0.0, 0.0, 0.0, 0.0, INF, INF, 1};
  }

  const double a = 1.5 * static_cast<double>(M);
  const double mult = 1.0 - 3.0 / (2.0 * static_cast<double>(Q));
  return {vc, 0.0, 0.0, 0.0, 0.0, a, mult,
          (vc.sigma2_hat > 0.0 && std::isfinite(vc.sigma2_hat)) ? 0 : 1};
}

// AR-adjusted Wald benchmark.

inline double ar_wald_statistic(const std::vector<double>& x,
                                double mu0,
                                double xbar,
                                double phi_hat,
                                double sigma2_eps_hat) {
  const int n = static_cast<int>(x.size());
  const double den = 1.0 - phi_hat * phi_hat;
  if (!(den > 0.0) || !(sigma2_eps_hat > 0.0)) return INF;
  const double omega = sigma2_eps_hat / den * D_value(n, phi_hat);
  if (!(omega > 0.0) || !std::isfinite(omega)) return INF;
  const double d = xbar - mu0;
  return static_cast<double>(n) * d * d / omega;
}

// Newey--West (1994) automatic Bartlett bandwidth, without prewhitening,
// matching sandwich::bwNeweyWest(..., prewhite = 0) for a scalar score.

inline double autocovariance(const std::vector<double>& y, int lag) {
  const int n = static_cast<int>(y.size());
  long double s = 0.0L;
  for (int t = 0; t < n - lag; ++t) {
    s += static_cast<long double>(y[t]) * y[t + lag];
  }
  return static_cast<double>(s / static_cast<long double>(n));
}

inline int nw94_bandwidth(const std::vector<double>& x, double xbar) {
  const int n = static_cast<int>(x.size());
  std::vector<double> y(n);
  for (int i = 0; i < n; ++i) y[i] = x[i] - xbar;
  int m = static_cast<int>(std::floor(4.0 *
      std::pow(static_cast<double>(n) / 100.0, 2.0 / 9.0)));
  m = std::max(1, std::min(m, n - 1));
  const double g0 = autocovariance(y, 0);
  long double s0 = g0;
  long double s1 = 0.0L;
  for (int j = 1; j <= m; ++j) {
    const double gj = autocovariance(y, j);
    s0 += 2.0L * gj;
    s1 += 2.0L * static_cast<long double>(j) * gj;
  }
  if (std::abs(static_cast<double>(s0)) < 1e-14) return 0;
  const double ratio = static_cast<double>(s1 / s0);
  const double bw = 1.1447 * std::pow(static_cast<double>(n) * ratio * ratio,
                                      1.0 / 3.0);
  if (!std::isfinite(bw) || bw < 0.0) return 0;
  return std::max(0, std::min(n - 1, static_cast<int>(std::floor(bw))));
}

inline double hac_wald_statistic(const std::vector<double>& x,
                                 double mu0,
                                 double xbar,
                                 int& bandwidth_out) {
  const int n = static_cast<int>(x.size());
  std::vector<double> y(n);
  for (int i = 0; i < n; ++i) y[i] = x[i] - xbar;
  const int L = nw94_bandwidth(x, xbar);
  bandwidth_out = L;
  long double omega = autocovariance(y, 0);
  for (int h = 1; h <= L; ++h) {
    const double weight = 1.0 - static_cast<double>(h) /
                                  static_cast<double>(L + 1);
    omega += 2.0L * weight * autocovariance(y, h);
  }
  const double om = static_cast<double>(omega);
  if (!(om > 0.0) || !std::isfinite(om)) return INF;
  const double d = xbar - mu0;
  return static_cast<double>(n) * d * d / om;
}

// Shao self-normalized statistic.

inline double self_normalized_statistic(const std::vector<double>& x,
                                         double mu0,
                                         double xbar) {
  const int n = static_cast<int>(x.size());
  long double cumulative = 0.0L;
  long double sumsq = 0.0L;
  for (int t = 0; t < n; ++t) {
    cumulative += static_cast<long double>(x[t] - xbar);
    sumsq += cumulative * cumulative;
  }
  const long double nn = static_cast<long double>(n);
  const double V = static_cast<double>(sumsq / (nn * nn));
  if (!(V > 0.0) || !std::isfinite(V)) return INF;
  const double d = xbar - mu0;
  return static_cast<double>(n) * d * d / V;
}

// Patton--Politis--White stationary-bootstrap block selector.
// This mirrors np::b.star(..., round = TRUE), stationary-bootstrap column.

inline double flat_top(double s) {
  const double a = std::abs(s);
  if (a < 0.5) return 1.0;
  if (a <= 1.0) return 2.0 * (1.0 - a);
  return 0.0;
}

inline int stationary_bootstrap_block_length(const std::vector<double>& x,
                                              double xbar) {
  const int n = static_cast<int>(x.size());
  if (n < 4) return 1;
  const int Kn = std::max(5, static_cast<int>(std::ceil(std::log10(n))));
  const int mmax = std::min(n - 1,
      static_cast<int>(std::ceil(std::sqrt(static_cast<double>(n)))) + Kn);
  const int Bmax = std::max(1, static_cast<int>(std::ceil(std::min(
      3.0 * std::sqrt(static_cast<double>(n)), n / 3.0))));

  std::vector<double> y(n);
  for (int i = 0; i < n; ++i) y[i] = x[i] - xbar;
  const double g0 = autocovariance(y, 0);
  if (!(g0 > 0.0)) return 1;

  std::vector<double> rho(mmax + 1, 0.0);
  for (int k = 1; k <= mmax; ++k) rho[k] = autocovariance(y, k) / g0;
  const double threshold = Z975 * std::sqrt(std::log(static_cast<double>(n)) /
                                             static_cast<double>(n));

  int mhat = -1;
  if (mmax >= Kn) {
    for (int j = 1; j <= mmax - Kn + 1; ++j) {
      bool all_small = true;
      for (int k = j; k < j + Kn; ++k) {
        if (std::abs(rho[k]) >= threshold) {
          all_small = false;
          break;
        }
      }
      if (all_small) {
        mhat = j;
        break;
      }
    }
  }
  if (mhat < 0) {
    int last_sig = -1;
    for (int k = 1; k <= mmax; ++k) {
      if (std::abs(rho[k]) > threshold) last_sig = k;
    }
    mhat = (last_sig >= 1) ? last_sig : 1;
  }

  const int M = std::max(1, std::min(2 * mhat, mmax));
  long double G = 0.0L;
  long double S = 0.0L;
  for (int k = -M; k <= M; ++k) {
    const double gamma = autocovariance(y, std::abs(k));
    const double weight = flat_top(static_cast<double>(k) /
                                   static_cast<double>(M));
    G += static_cast<long double>(weight * std::abs(k)) * gamma;
    S += static_cast<long double>(weight) * gamma;
  }
  const long double Dsb = 2.0L * S * S;
  if (!(Dsb > 0.0L)) return 1;
  const long double ratio = 2.0L * G * G / Dsb;
  if (!(ratio > 0.0L)) return 1;
  const double bstar = std::pow(static_cast<double>(ratio), 1.0 / 3.0) *
                       std::pow(static_cast<double>(n), 1.0 / 3.0);
  if (!std::isfinite(bstar)) return 1;
  return std::max(1, std::min(Bmax, static_cast<int>(std::round(bstar))));
}

inline double circular_segment_sum(const std::vector<double>& prefix2,
                                   int start,
                                   int length) {
  return prefix2[start + length] - prefix2[start];
}

inline double stationary_bootstrap_pvalue(const std::vector<double>& x,
                                          double mu0,
                                          double xbar,
                                          int bootstrap_reps,
                                          int block_length,
                                          FastRNG& rng) {
  const int n = static_cast<int>(x.size());
  if (bootstrap_reps < 1) return 1.0;
  const double p = 1.0 / static_cast<double>(std::max(1, block_length));
  std::vector<double> prefix2(2 * n + 1, 0.0);
  for (int i = 0; i < 2 * n; ++i) {
    prefix2[i + 1] = prefix2[i] + x[i % n];
  }

  const double observed = std::sqrt(static_cast<double>(n)) * (xbar - mu0);
  int lower_count = 0;
  int upper_count = 0;
  for (int b = 0; b < bootstrap_reps; ++b) {
    int remaining = n;
    long double total = 0.0L;
    while (remaining > 0) {
      const int start = rng.uniform_int(n);
      int length = rng.geometric(p);
      if (length > remaining) length = remaining;
      total += circular_segment_sum(prefix2, start, length);
      remaining -= length;
    }
    const double mean_star = static_cast<double>(total /
        static_cast<long double>(n));
    const double zstar = std::sqrt(static_cast<double>(n)) *
                         (mean_star - xbar);
    if (zstar <= observed) ++lower_count;
    if (zstar >= observed) ++upper_count;
  }
  const double denominator = static_cast<double>(bootstrap_reps + 1);
  const double p_lower = (lower_count + 1.0) / denominator;
  const double p_upper = (upper_count + 1.0) / denominator;
  return std::min(1.0, 2.0 * std::min(p_lower, p_upper));
}

// Parallel workers.

// Fixed block-length sweep used for the main simulation figure.  The same
// simulated series is evaluated at every M so method differences are paired.
// The same simulated series is evaluated at every requested block length.
// The worker returns the raw statistic, both variance calibrations, and the
// full AR(1) grouped correction for each block length.

// Refocused fixed-design worker used by the final publication tables.  It
// deliberately contains only the methods used in the final study. Every
// BEL method uses the same deterministic block length.
struct RefocusedDesignWorker : public Worker {
  int n;
  double a1;
  double a2;
  double theta;
  int innovation_code;
  double innovation_sd;
  double process_mean;
  double mu0;
  int burn_in;
  int bootstrap_reps;
  int M;
  std::uint64_t base_seed;
  std::uint64_t rep_offset;
  RMatrix<double> output;

  RefocusedDesignWorker(int n_, double a1_, double a2_, double theta_,
                        int innovation_code_, double innovation_sd_,
                        double process_mean_, double mu0_, int burn_in_,
                        int bootstrap_reps_, int M_,
                        std::uint64_t base_seed_, std::uint64_t rep_offset_,
                        NumericMatrix output_)
      : n(n_), a1(a1_), a2(a2_), theta(theta_),
        innovation_code(innovation_code_), innovation_sd(innovation_sd_),
        process_mean(process_mean_), mu0(mu0_), burn_in(burn_in_),
        bootstrap_reps(bootstrap_reps_), M(M_), base_seed(base_seed_),
        rep_offset(rep_offset_), output(output_) {}

  void operator()(std::size_t begin, std::size_t end) {
    std::vector<double> x(n);
    for (std::size_t r = begin; r < end; ++r) {
      const std::uint64_t global_id =
          rep_offset + static_cast<std::uint64_t>(r);
      FastRNG rng(replication_seed(base_seed, global_id));
      simulate_process(x, a1, a2, theta, innovation_code, innovation_sd,
                       process_mean, burn_in, rng);

      const double xbar = vec_mean(x);
      const double phi_hat = bias_reduced_phi(burg_phi(x, xbar), n);
      const ResidualEstimates residual =
          residual_estimates(x, xbar, phi_hat);

      const int Q = n / M;
      const int nstar = Q * M;
      const double raw = bel_statistic(x, mu0, M);
      const GeneralFullCorrection general = general_full_correction(x, mu0, M);
      const double nu_ar = variance_factor(phi_hat, M, nstar);
      const double aK = kitamura_coefficient(
          phi_hat, M, nstar, residual.skewness, residual.excess_kurtosis);
      const double ar1_full_multiplier =
          1.0 - aK / static_cast<double>(nstar);

      double general_vc_stat = INF;
      double general_full_stat = INF;
      double ar1_vc_stat = INF;
      double ar1_full_stat = INF;
      if (std::isfinite(raw) && general.vc.nu_hat > 0.0) {
        general_vc_stat = general.vc.nu_hat * raw;
        if (std::isfinite(general.full_multiplier_hat) &&
            general.full_multiplier_hat > 0.0) {
          general_full_stat = general_vc_stat * general.full_multiplier_hat;
        }
      }
      if (std::isfinite(raw) && nu_ar > 0.0) {
        ar1_vc_stat = nu_ar * raw;
        if (std::isfinite(ar1_full_multiplier) && ar1_full_multiplier > 0.0) {
          ar1_full_stat = ar1_vc_stat * ar1_full_multiplier;
        }
      }

      const double ar = ar_wald_statistic(
          x, mu0, xbar, phi_hat, residual.sigma2);
      int hac_lag = 0;
      const double hac = hac_wald_statistic(x, mu0, xbar, hac_lag);
      const double sn = self_normalized_statistic(x, mu0, xbar);
      const int boot_block = stationary_bootstrap_block_length(x, xbar);
      const double boot_p = stationary_bootstrap_pvalue(
          x, mu0, xbar, bootstrap_reps, boot_block, rng);

      output(r, 0) = raw;
      output(r, 1) = general_vc_stat;
      output(r, 2) = general_full_stat;
      output(r, 3) = ar1_vc_stat;
      output(r, 4) = ar1_full_stat;
      output(r, 5) = ar;
      output(r, 6) = hac;
      output(r, 7) = sn;
      output(r, 8) = boot_p;
      output(r, 9) = static_cast<double>(M);
      output(r, 10) = static_cast<double>(nstar);
      output(r, 11) = phi_hat;
      output(r, 12) = residual.skewness;
      output(r, 13) = residual.excess_kurtosis;
      output(r, 14) = general.vc.b_hat;
      output(r, 15) = general.vc.nu_hat;
      output(r, 16) = static_cast<double>(general.vc.H);
      output(r, 17) = general.gamma3_lr_hat;
      output(r, 18) = general.gamma4_lr_hat;
      output(r, 19) = general.Lambda3_hat;
      output(r, 20) = general.Lambda4_hat;
      output(r, 21) = general.a_full_hat;
      output(r, 22) = general.full_multiplier_hat;
      output(r, 23) = general.vc.sigma2_hat;
      output(r, 24) = general.vc.B2_hat;
      output(r, 25) = static_cast<double>(general.sigma_nonpositive);
      output(r, 26) = nu_ar;
      output(r, 27) = ar1_full_multiplier;
      output(r, 28) = static_cast<double>(hac_lag);
      output(r, 29) = static_cast<double>(boot_block);
      output(r, 30) = std::isfinite(raw) ? 0.0 : 1.0;
      output(r, 31) = std::isfinite(general_vc_stat) ? 0.0 : 1.0;
      output(r, 32) = std::isfinite(general_full_stat) ? 0.0 : 1.0;
      output(r, 33) = std::isfinite(ar1_full_stat) ? 0.0 : 1.0;
    }
  }
};

struct RefocusedBlockGridWorker : public Worker {
  int n;
  double ar1;
  int innovation_code;
  double innovation_sd;
  int burn_in;
  double mu0;
  std::vector<int> M_values;
  std::uint64_t base_seed;
  std::uint64_t rep_offset;
  RMatrix<double> output;

  RefocusedBlockGridWorker(int n_, double ar1_, int innovation_code_,
                           double innovation_sd_, int burn_in_, double mu0_,
                           const std::vector<int>& M_values_,
                           std::uint64_t base_seed_,
                           std::uint64_t rep_offset_, NumericMatrix output_)
      : n(n_), ar1(ar1_), innovation_code(innovation_code_),
        innovation_sd(innovation_sd_), burn_in(burn_in_), mu0(mu0_),
        M_values(M_values_), base_seed(base_seed_), rep_offset(rep_offset_),
        output(output_) {}

  void operator()(std::size_t begin, std::size_t end) {
    const int K = static_cast<int>(M_values.size());
    std::vector<double> x(n);
    for (std::size_t r = begin; r < end; ++r) {
      const std::uint64_t global_id =
          rep_offset + static_cast<std::uint64_t>(r);
      FastRNG rng(replication_seed(base_seed, global_id));
      simulate_process(x, ar1, 0.0, 0.0, innovation_code, innovation_sd,
                       0.0, burn_in, rng);

      const double xbar = vec_mean(x);
      const double phi_hat = bias_reduced_phi(burg_phi(x, xbar), n);
      const ResidualEstimates residual =
          residual_estimates(x, xbar, phi_hat);

      for (int k = 0; k < K; ++k) {
        const int M = M_values[k];
        const int Q = n / M;
        const int nstar = Q * M;
        const double raw = bel_statistic(x, mu0, M);
        const GeneralFullCorrection general = general_full_correction(x, mu0, M);
        const double nu_ar = (Q >= 2)
            ? variance_factor(phi_hat, M, nstar) : 1.0;

        double general_vc_stat = INF;
        double general_full_stat = INF;
        double ar1_vc_stat = INF;
        double ar1_full_stat = INF;
        double ar1_full_multiplier = INF;
        if (std::isfinite(raw) && general.vc.nu_hat > 0.0) {
          general_vc_stat = general.vc.nu_hat * raw;
          if (std::isfinite(general.full_multiplier_hat) &&
              general.full_multiplier_hat > 0.0) {
            general_full_stat = general_vc_stat * general.full_multiplier_hat;
          }
        }
        if (std::isfinite(raw) && nu_ar > 0.0) {
          ar1_vc_stat = nu_ar * raw;
          const double aK = kitamura_coefficient(
              phi_hat, M, nstar, residual.skewness,
              residual.excess_kurtosis);
          ar1_full_multiplier = 1.0 - aK / static_cast<double>(nstar);
          if (std::isfinite(ar1_full_multiplier) && ar1_full_multiplier > 0.0) {
            ar1_full_stat = ar1_vc_stat * ar1_full_multiplier;
          }
        }

        output(r, k) = raw;
        output(r, K + k) = general_vc_stat;
        output(r, 2 * K + k) = general_full_stat;
        output(r, 3 * K + k) = ar1_vc_stat;
        output(r, 4 * K + k) = ar1_full_stat;
        output(r, 5 * K + k) = general.vc.nu_hat;
        output(r, 6 * K + k) = general.vc.b_hat;
        output(r, 7 * K + k) = static_cast<double>(general.vc.H);
        output(r, 8 * K + k) = general.full_multiplier_hat;
        output(r, 9 * K + k) = general.gamma3_lr_hat;
        output(r, 10 * K + k) = general.gamma4_lr_hat;
        output(r, 11 * K + k) = ar1_full_multiplier;
      }

      output(r, 12 * K + 0) = phi_hat;
      output(r, 12 * K + 1) = residual.skewness;
      output(r, 12 * K + 2) = residual.excess_kurtosis;
    }
  }
};

struct SNWorker : public Worker {
  int terms;
  std::uint64_t base_seed;
  RVector<double> output;

  SNWorker(int terms_, std::uint64_t base_seed_, NumericVector output_)
      : terms(terms_), base_seed(base_seed_), output(output_) {}

  void operator()(std::size_t begin, std::size_t end) {
    // Karhunen--Loeve: integral bridge^2 = sum Z_k^2/(pi^2 k^2).
    // Replace the tiny omitted tail by its mean.
    long double tail_mean = 1.0L / 6.0L;
    for (int k = 1; k <= terms; ++k) {
      tail_mean -= 1.0L / (PI * PI * static_cast<long double>(k) * k);
    }
    tail_mean = std::max(0.0L, tail_mean);

    for (std::size_t r = begin; r < end; ++r) {
      FastRNG rng(replication_seed(base_seed, static_cast<std::uint64_t>(r)));
      const double z0 = rng.normal();
      long double denominator = tail_mean;
      for (int k = 1; k <= terms; ++k) {
        const double z = rng.normal();
        denominator += static_cast<long double>(z * z) /
            (PI * PI * static_cast<long double>(k) * k);
      }
      output[r] = z0 * z0 / static_cast<double>(denominator);
    }
  }
};

}  // namespace ar1bel

// R exports.

// [[Rcpp::export]]
NumericVector simulate_gaussian_ar1_block_lr_cpp(int replications,
                                                  int M,
                                                  int Q,
                                                  double phi,
                                                  double seed) {
  if (replications < 1) Rcpp::stop("replications must be positive");
  if (M < 2) Rcpp::stop("M must be at least 2");
  if (Q < 2) Rcpp::stop("Q must be at least 2");
  if (!(std::abs(phi) < 1.0)) Rcpp::stop("phi must lie in (-1,1)");

  const long double p = static_cast<long double>(phi);
  const long double pM_ld = std::pow(p, M);
  const double pM = static_cast<double>(pM_ld);
  const long double one_minus = 1.0L - p;
  const long double c_ld = p * (1.0L - pM_ld) / one_minus;
  const double c_state = static_cast<double>(c_ld);

  const long double var_y_ld = 1.0L - pM_ld * pM_ld;
  if (!(var_y_ld > 0.0L)) Rcpp::stop("invalid block-state variance");
  const double sd_y = std::sqrt(static_cast<double>(var_y_ld));

  const long double var_s_total =
      static_cast<long double>(M) * ar1bel::D_value(M, phi);
  long double var_s_noise = var_s_total - c_ld * c_ld;
  if (var_s_noise < 0.0L && var_s_noise > -1e-11L) var_s_noise = 0.0L;
  if (!(var_s_noise >= 0.0L)) Rcpp::stop("invalid block-sum innovation variance");

  const long double sigma_eps2 = 1.0L - p * p;
  long double cov_sy_ld = 0.0L;
  for (int u = 1; u <= M; ++u) {
    const int k = M - u;
    const long double w = (1.0L - std::pow(p, k + 1)) / one_minus;
    cov_sy_ld += sigma_eps2 * w * std::pow(p, k);
  }
  const double cov_sy = static_cast<double>(cov_sy_ld);

  long double cond_var = var_s_noise -
      cov_sy_ld * cov_sy_ld / var_y_ld;
  if (cond_var < 0.0L && cond_var > -1e-10L) cond_var = 0.0L;
  if (!(cond_var >= 0.0L)) Rcpp::stop("invalid conditional block-sum variance");
  const double sd_s_cond = std::sqrt(static_cast<double>(cond_var));

  NumericVector out(replications);
  ar1bel::GaussianBlockLRWorker worker(
      replications, M, Q, phi, pM, c_state, sd_y, cov_sy, sd_s_cond,
      static_cast<std::uint64_t>(seed), out);
  parallelFor(0, replications, worker, 32);
  return out;
}

// [[Rcpp::export]]
NumericMatrix run_refocused_design_cpp(int replications,
                                       int n,
                                       double ar1,
                                       double ar2,
                                       double ma1,
                                       int innovation_code,
                                       double process_mean,
                                       double mu0,
                                       int burn_in,
                                       int bootstrap_reps,
                                       int M,
                                       double seed,
                                       double rep_offset = 0) {
  if (replications < 1) Rcpp::stop("replications must be positive");
  if (n < 20) Rcpp::stop("n must be at least 20");
  if (innovation_code < 1 || innovation_code > 4) {
    Rcpp::stop("innovation_code must be one of 1, 2, 3, 4");
  }
  if (M < 2 || n / M < 2) {
    Rcpp::stop("M must leave at least two complete blocks");
  }

  const ar1bel::ProcessTheory theory =
      ar1bel::process_theory(ar1, ar2, ma1, std::max(4096, n + 100));
  NumericMatrix out(replications, 34);
  ar1bel::RefocusedDesignWorker worker(
      n, ar1, ar2, ma1, innovation_code, theory.innovation_sd,
      process_mean, mu0, burn_in, bootstrap_reps, M,
      static_cast<std::uint64_t>(seed),
      static_cast<std::uint64_t>(rep_offset), out);
  parallelFor(0, replications, worker, 1);
  Rcpp::colnames(out) = CharacterVector::create(
      "raw_bel", "general_vc", "general_full", "ar1_vc", "ar1_full",
      "ar_wald", "hac_wald", "self_normalized", "bootstrap_p",
      "M_common", "Nstar", "phi_hat", "gamma3_hat", "gamma4_hat",
      "b_hat_general", "nu_hat_general", "general_H",
      "gamma3_lr_general", "gamma4_lr_general",
      "Lambda3_hat_general", "Lambda4_hat_general",
      "a_full_general", "full_multiplier_general",
      "sigma2_hat_general", "B2_hat_general", "general_sigma_nonpositive",
      "nu_ar1", "full_multiplier_ar1", "hac_lag", "bootstrap_block",
      "raw_fail", "general_vc_fail", "general_full_fail", "ar1_full_fail");
  return out;
}


// [[Rcpp::export]]
NumericMatrix run_block_grid_refocused_cpp(int replications,
                                           int n,
                                           double phi,
                                           int innovation_code,
                                           IntegerVector M_values,
                                           int burn_in,
                                           double mu0,
                                           double seed,
                                           double rep_offset = 0) {
  if (replications < 1) Rcpp::stop("replications must be positive");
  if (n < 20) Rcpp::stop("n must be at least 20");
  if (!(std::abs(phi) < 1.0)) Rcpp::stop("phi must be in (-1,1)");
  if (innovation_code < 1 || innovation_code > 4) {
    Rcpp::stop("innovation_code must be one of 1, 2, 3, 4");
  }
  std::vector<int> grid = Rcpp::as<std::vector<int>>(M_values);
  if (grid.empty()) Rcpp::stop("M_values is empty");
  for (int M : grid) {
    if (M < 2 || n / M < 2) Rcpp::stop("every M must leave at least two blocks");
  }

  const ar1bel::ProcessTheory theory =
      ar1bel::process_theory(phi, 0.0, 0.0, std::max(4096, n + 100));
  const int K = static_cast<int>(grid.size());
  NumericMatrix out(replications, 12 * K + 3);
  ar1bel::RefocusedBlockGridWorker worker(
      n, phi, innovation_code, theory.innovation_sd, burn_in, mu0, grid,
      static_cast<std::uint64_t>(seed),
      static_cast<std::uint64_t>(rep_offset), out);
  parallelFor(0, replications, worker, 1);

  CharacterVector names(12 * K + 3);
  for (int k = 0; k < K; ++k) {
    const std::string m = std::to_string(grid[k]);
    names[k] = "raw_M" + m;
    names[K + k] = "general_vc_M" + m;
    names[2 * K + k] = "general_full_M" + m;
    names[3 * K + k] = "ar1_vc_M" + m;
    names[4 * K + k] = "ar1_full_M" + m;
    names[5 * K + k] = "nu_general_M" + m;
    names[6 * K + k] = "bhat_general_M" + m;
    names[7 * K + k] = "H_general_M" + m;
    names[8 * K + k] = "general_full_multiplier_M" + m;
    names[9 * K + k] = "general_gamma3_M" + m;
    names[10 * K + k] = "general_gamma4_M" + m;
    names[11 * K + k] = "ar1_full_multiplier_M" + m;
  }
  names[12 * K + 0] = "phi_hat";
  names[12 * K + 1] = "gamma3_hat";
  names[12 * K + 2] = "gamma4_hat";
  Rcpp::colnames(out) = names;
  return out;
}

// [[Rcpp::export]]
NumericVector simulate_sn_limit_cpp(int replications,
                                    int kl_terms,
                                    double seed) {
  if (replications < 1 || kl_terms < 10) {
    Rcpp::stop("replications must be positive and kl_terms at least 10");
  }
  NumericVector out(replications);
  ar1bel::SNWorker worker(kl_terms, static_cast<std::uint64_t>(seed), out);
  parallelFor(0, replications, worker, 16);
  return out;
}

// Single-series helpers for the validation script.

// [[Rcpp::export]]
NumericVector cpp_draw_innovations(int n, int innovation_code, double seed) {
  if (n < 1) Rcpp::stop("n must be positive");
  if (innovation_code < 1 || innovation_code > 4) {
    Rcpp::stop("innovation_code must be one of 1, 2, 3, 4");
  }
  NumericVector out(n);
  ar1bel::FastRNG rng(static_cast<std::uint64_t>(seed));
  for (int i = 0; i < n; ++i) {
    out[i] = ar1bel::draw_unit_innovation(rng, innovation_code);
  }
  return out;
}

// [[Rcpp::export]]
NumericVector cpp_draw_process(int n,
                               double ar1,
                               double ar2,
                               double ma1,
                               int innovation_code,
                               int burn_in,
                               double process_mean,
                               double seed) {
  if (n < 1) Rcpp::stop("n must be positive");
  if (innovation_code < 1 || innovation_code > 4) {
    Rcpp::stop("innovation_code must be one of 1, 2, 3, 4");
  }
  const ar1bel::ProcessTheory theory =
      ar1bel::process_theory(ar1, ar2, ma1, std::max(4096, n + 100));
  std::vector<double> x(n);
  ar1bel::FastRNG rng(static_cast<std::uint64_t>(seed));
  ar1bel::simulate_process(x, ar1, ar2, ma1, innovation_code,
                           theory.innovation_sd, process_mean, burn_in, rng);
  return Rcpp::wrap(x);
}

// [[Rcpp::export]]
List cpp_process_theory(double ar1,
                        double ar2,
                        double ma1,
                        int M = 16,
                        int nstar = 256) {
  const ar1bel::ProcessTheory theory =
      ar1bel::process_theory(ar1, ar2, ma1, std::max(4096, nstar + 100));
  return List::create(
      Named("innovation_sd") = theory.innovation_sd,
      Named("marginal_variance") = 1.0,
      Named("true_lrv") = theory.true_lrv,
      Named("pseudo_phi") = theory.pseudo_phi,
      Named("ar1_implied_lrv") = theory.ar1_implied_lrv,
      Named("ar1_lrv_ratio") = theory.ar1_lrv_ratio,
      Named("B2") = theory.B2,
      Named("b") = theory.b,
      Named("b_ar_pseudo") =
          2.0 * theory.pseudo_phi /
          (1.0 - theory.pseudo_phi * theory.pseudo_phi),
      Named("delta_b") =
          theory.b - 2.0 * theory.pseudo_phi /
          (1.0 - theory.pseudo_phi * theory.pseudo_phi),
      Named("omega_M") = ar1bel::omega_r(theory, M),
      Named("omega_N") = ar1bel::omega_r(theory, nstar),
      Named("nu_ar1_pseudo") =
          ar1bel::variance_factor(theory.pseudo_phi, M, nstar),
      Named("nu_general_b") =
          (1.0 - theory.b / static_cast<double>(M)) /
          (1.0 - theory.b / static_cast<double>(nstar)),
      Named("nu_population_exact") =
          ar1bel::population_variance_ratio(theory, M, nstar));
}

// [[Rcpp::export]]
List cpp_general_calibration(NumericVector x,
                             double mu0,
                             int M) {
  const std::vector<double> xx = Rcpp::as<std::vector<double>>(x);
  const ar1bel::GeneralCalibration out =
      ar1bel::general_calibration(xx, mu0, M);
  return List::create(
      Named("sigma2_hat") = out.sigma2_hat,
      Named("B2_hat") = out.B2_hat,
      Named("b_hat") = out.b_hat,
      Named("nu_hat") = out.nu_hat,
      Named("H") = out.H);
}


// [[Rcpp::export]]
List cpp_general_full_correction(NumericVector x,
                                 double mu0,
                                 int M) {
  const std::vector<double> xx = Rcpp::as<std::vector<double>>(x);
  const ar1bel::GeneralFullCorrection out =
      ar1bel::general_full_correction(xx, mu0, M);
  return List::create(
      Named("sigma2_hat") = out.vc.sigma2_hat,
      Named("B2_hat") = out.vc.B2_hat,
      Named("b_hat") = out.vc.b_hat,
      Named("nu_hat") = out.vc.nu_hat,
      Named("H") = out.vc.H,
      Named("Lambda3_hat") = out.Lambda3_hat,
      Named("Lambda4_hat") = out.Lambda4_hat,
      Named("gamma3_lr_hat") = out.gamma3_lr_hat,
      Named("gamma4_lr_hat") = out.gamma4_lr_hat,
      Named("a_full_hat") = out.a_full_hat,
      Named("full_multiplier_hat") = out.full_multiplier_hat,
      Named("sigma_nonpositive") = out.sigma_nonpositive);
}

// [[Rcpp::export]]
double cpp_D_value(int r, double phi) {
  return ar1bel::D_value(r, phi);
}

// [[Rcpp::export]]
double cpp_variance_factor(double phi, int M, int nstar) {
  return ar1bel::variance_factor(phi, M, nstar);
}


// [[Rcpp::export]]
NumericVector cpp_kitamura_coefficient_vec(NumericVector phi,
                                            int M,
                                            int nstar,
                                            NumericVector gamma3,
                                            NumericVector gamma4) {
  const int n = phi.size();
  if (gamma3.size() != n || gamma4.size() != n) {
    Rcpp::stop("phi, gamma3 and gamma4 must have the same length");
  }
  NumericVector out(n);
  for (int i = 0; i < n; ++i) {
    out[i] = ar1bel::kitamura_coefficient(
        phi[i], M, nstar, gamma3[i], gamma4[i]);
  }
  return out;
}

// [[Rcpp::export]]
double cpp_bel_statistic(NumericVector x, double mu0, int M) {
  return ar1bel::bel_statistic(Rcpp::as<std::vector<double>>(x), mu0, M);
}

// [[Rcpp::export]]
int cpp_nw94_bandwidth(NumericVector x) {
  const std::vector<double> xx = Rcpp::as<std::vector<double>>(x);
  return ar1bel::nw94_bandwidth(xx, ar1bel::vec_mean(xx));
}

// [[Rcpp::export]]
int cpp_stationary_bootstrap_block_length(NumericVector x) {
  const std::vector<double> xx = Rcpp::as<std::vector<double>>(x);
  return ar1bel::stationary_bootstrap_block_length(xx, ar1bel::vec_mean(xx));
}

