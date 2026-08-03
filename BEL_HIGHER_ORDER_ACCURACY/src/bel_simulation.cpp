#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <limits>
#include <random>
#include <string>
#include <vector>

#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;

// [[Rcpp::plugins(cpp17)]]
// [[Rcpp::plugins(openmp)]]

namespace {

constexpr double tiny = 1e-12;

uint64_t splitmix64(uint64_t x) {
  x += 0x9e3779b97f4a7c15ULL;
  x = (x ^ (x >> 30)) * 0xbf58476d1ce4e5b9ULL;
  x = (x ^ (x >> 27)) * 0x94d049bb133111ebULL;
  return x ^ (x >> 31);
}

double draw_innovation(int code, std::mt19937_64& rng) {
  static thread_local std::normal_distribution<double> normal(0.0, 1.0);
  static thread_local std::exponential_distribution<double> exponential(1.0);
  static thread_local std::gamma_distribution<double> gamma4(4.0, 1.0);
  static thread_local std::gamma_distribution<double> chisq5(2.5, 2.0);
  static thread_local std::bernoulli_distribution bernoulli(0.5);
  static thread_local std::uniform_real_distribution<double> uniform(0.0, 1.0);

  if (code == 1) return normal(rng);
  if (code == 2) return (exponential(rng) - exponential(rng)) / std::sqrt(2.0);
  if (code == 3) return exponential(rng) - 1.0;
  if (code == 4) return (gamma4(rng) - 4.0) / 2.0;
  if (code == 5) return bernoulli(rng) ? 1.0 : -1.0;
  if (code == 6) return normal(rng) / std::sqrt(chisq5(rng) / 5.0) / std::sqrt(5.0 / 3.0);

  double z = uniform(rng) < 0.9 ? normal(rng) : 3.0 * normal(rng);
  return z / std::sqrt(1.8);
}

void simulate_arma(std::vector<double>& x,
                   int burnin,
                   double ar1,
                   double ar2,
                   double ma1,
                   double ma2,
                   int innovation,
                   double innovation_scale,
                   std::mt19937_64& rng) {
  double y1 = 0.0;
  double y2 = 0.0;
  double e1 = 0.0;
  double e2 = 0.0;

  for (int t = 0; t < burnin; ++t) {
    double e = innovation_scale * draw_innovation(innovation, rng);
    double y = ar1 * y1 + ar2 * y2 + e + ma1 * e1 + ma2 * e2;
    y2 = y1;
    y1 = y;
    e2 = e1;
    e1 = e;
  }

  for (int t = 0; t < static_cast<int>(x.size()); ++t) {
    double e = innovation_scale * draw_innovation(innovation, rng);
    double y = ar1 * y1 + ar2 * y2 + e + ma1 * e1 + ma2 * e2;
    y2 = y1;
    y1 = y;
    e2 = e1;
    e1 = e;
    x[t] = y;
  }
}

double empirical_likelihood_ratio(const std::vector<double>& z) {
  int q = static_cast<int>(z.size());
  if (q < 2) return R_PosInf;

  double zmin = z[0];
  double zmax = z[0];
  double sum1 = 0.0;
  double sum2 = 0.0;

  for (double v : z) {
    zmin = std::min(zmin, v);
    zmax = std::max(zmax, v);
    sum1 += v;
    sum2 += v * v;
  }

  if (!(zmin < 0.0 && zmax > 0.0 && sum2 > 0.0)) return R_PosInf;

  double lower = -1.0 / zmax;
  double upper = -1.0 / zmin;
  lower += 1e-13 * (1.0 + std::abs(lower));
  upper -= 1e-13 * (1.0 + std::abs(upper));

  double lambda = sum1 / sum2;
  if (!std::isfinite(lambda) || lambda <= lower || lambda >= upper) {
    lambda = 0.5 * (lower + upper);
  }

  for (int iter = 0; iter < 50; ++iter) {
    double score = 0.0;
    double derivative = 0.0;
    bool inside = true;

    for (double v : z) {
      double den = 1.0 + lambda * v;
      if (!(den > 0.0)) {
        inside = false;
        break;
      }
      score += v / den;
      derivative -= v * v / (den * den);
    }

    if (!inside || !std::isfinite(score) || !std::isfinite(derivative) || !(derivative < 0.0)) {
      lambda = 0.5 * (lower + upper);
      continue;
    }

    if (std::abs(score) < 1e-11 * (1.0 + std::sqrt(sum2))) break;

    if (score > 0.0) lower = lambda;
    else upper = lambda;

    double next = lambda - score / derivative;
    if (!std::isfinite(next) || next <= lower || next >= upper) {
      next = 0.5 * (lower + upper);
    }

    if (std::abs(next - lambda) < 1e-12 * (1.0 + std::abs(lambda))) {
      lambda = next;
      break;
    }
    lambda = next;
  }

  double value = 0.0;
  for (double v : z) {
    double den = 1.0 + lambda * v;
    if (!(den > 0.0)) return R_PosInf;
    value += 2.0 * std::log(den);
  }

  if (!std::isfinite(value)) return R_PosInf;
  return std::max(0.0, value);
}

void sample_autocovariances(const std::vector<double>& x,
                            int max_lag,
                            bool center,
                            std::vector<double>& gamma) {
  int n = static_cast<int>(x.size());
  double mean = 0.0;

  if (center) {
    for (double v : x) mean += v;
    mean /= n;
  }

  gamma.assign(max_lag + 1, 0.0);
  for (int h = 0; h <= max_lag; ++h) {
    long double total = 0.0;
    for (int t = 0; t < n - h; ++t) {
      total += static_cast<long double>(x[t] - mean) * (x[t + h] - mean);
    }
    gamma[h] = static_cast<double>(total / n);
  }
}

double finite_block_variance(const std::vector<double>& gamma, int r) {
  double value = gamma[0];
  int last = std::min(r - 1, static_cast<int>(gamma.size()) - 1);
  for (int h = 1; h <= last; ++h) {
    value += 2.0 * (1.0 - static_cast<double>(h) / r) * gamma[h];
  }
  return value;
}

struct ARFit {
  std::vector<double> gamma;
  int order = 0;
  bool regularized = false;
};

ARFit fit_ar_sieve(const std::vector<double>& x, int criterion) {
  int n = static_cast<int>(x.size());
  int pmax = std::min(30, std::max(3, static_cast<int>(std::floor(3.0 * std::pow(n, 0.25)))));
  pmax = std::min(pmax, n / 8);

  std::vector<double> sample_gamma;
  sample_autocovariances(x, pmax, true, sample_gamma);

  ARFit fit;
  fit.gamma.assign(n, 0.0);

  if (!(sample_gamma[0] > tiny)) {
    fit.gamma[0] = 1e-8;
    fit.regularized = true;
    return fit;
  }

  double best_score = n * std::log(sample_gamma[0]);
  double prediction_variance = sample_gamma[0];
  int best_order = 0;
  std::vector<double> previous;
  std::vector<double> current;
  std::vector<double> best_coefficients;

  for (int k = 1; k <= pmax; ++k) {
    long double numerator = sample_gamma[k];
    for (int j = 1; j < k; ++j) {
      numerator -= static_cast<long double>(previous[j - 1]) * sample_gamma[k - j];
    }

    double reflection = prediction_variance > 1e-14
      ? static_cast<double>(numerator / prediction_variance)
      : 0.0;

    if (!std::isfinite(reflection)) reflection = 0.0;
    reflection = std::max(-0.985, std::min(0.985, reflection));

    current.assign(k, 0.0);
    for (int j = 1; j < k; ++j) {
      current[j - 1] = previous[j - 1] - reflection * previous[k - j - 1];
    }
    current[k - 1] = reflection;

    prediction_variance *= std::max(1e-8, 1.0 - reflection * reflection);

    double penalty = criterion == 1
      ? std::log(static_cast<double>(n)) * k
      : 2.0 * k * std::log(std::max(1.0001, std::log(static_cast<double>(n))));

    double score = n * std::log(std::max(prediction_variance, 1e-14)) + penalty;
    if (score < best_score) {
      best_score = score;
      best_order = k;
      best_coefficients = current;
    }
    previous = current;
  }

  fit.order = best_order;
  fit.gamma[0] = sample_gamma[0];

  if (best_order > 0) {
    for (int h = 1; h <= best_order && h < n; ++h) {
      fit.gamma[h] = sample_gamma[h];
    }
    for (int h = best_order + 1; h < n; ++h) {
      long double value = 0.0;
      for (int j = 1; j <= best_order; ++j) {
        value += static_cast<long double>(best_coefficients[j - 1]) * fit.gamma[h - j];
      }
      fit.gamma[h] = static_cast<double>(value);
    }
  }

  return fit;
}

double ar_ratio(const ARFit& fit, int m, int n, bool& regularized) {
  double numerator = finite_block_variance(fit.gamma, m);
  double denominator = finite_block_variance(fit.gamma, n);
  double floor_value = 1e-8 * std::max(1.0, fit.gamma[0]);

  if (!(numerator > floor_value) || !std::isfinite(numerator)) {
    numerator = floor_value;
    regularized = true;
  }
  if (!(denominator > floor_value) || !std::isfinite(denominator)) {
    denominator = floor_value;
    regularized = true;
  }
  return numerator / denominator;
}

struct Factors {
  double population = NA_REAL;
  double b_based = NA_REAL;
  double direct = NA_REAL;
  double stabilized = NA_REAL;
  double ar_bic = NA_REAL;
  double ar_hq = NA_REAL;
  double hybrid = NA_REAL;
  bool direct_valid = false;
  bool stabilized_fallback = false;
  bool b_regularized = false;
  bool ar_bic_regularized = false;
  bool ar_hq_regularized = false;
  bool hybrid_direct_branch = false;
};

Factors calculate_factors(const std::vector<double>& sample_gamma,
                          int h_omega,
                          int m,
                          int n,
                          double population_factor,
                          const ARFit& bic,
                          const ARFit& hq,
                          double c0,
                          double lower,
                          double upper,
                          double eta0) {
  Factors out;
  out.population = population_factor;

  int hb = std::min(h_omega, std::max(1, m / 2));
  double sigma_hat = sample_gamma[0];
  double b2_hat = 0.0;
  for (int h = 1; h <= hb; ++h) {
    sigma_hat += 2.0 * sample_gamma[h];
    b2_hat += 2.0 * h * sample_gamma[h];
  }

  double b_hat = 0.0;
  if (sigma_hat > 0.0 && std::isfinite(sigma_hat)) {
    b_hat = b2_hat / sigma_hat;
    b_hat = std::max(-static_cast<double>(hb), std::min(static_cast<double>(hb), b_hat));
  } else {
    out.b_regularized = true;
  }

  out.b_based = (1.0 - b_hat / m) / (1.0 - b_hat / n);

  double omega_m = sample_gamma[0];
  double omega_n = sample_gamma[0];
  for (int h = 1; h <= h_omega; ++h) {
    omega_n += 2.0 * (1.0 - static_cast<double>(h) / n) * sample_gamma[h];
    if (h < m) {
      omega_m += 2.0 * (1.0 - static_cast<double>(h) / m) * sample_gamma[h];
    }
  }

  out.direct = omega_m / omega_n;
  out.direct_valid = omega_n > 0.0 && omega_m > 0.0 && std::isfinite(out.direct);

  bool bic_regularized = bic.regularized;
  bool hq_regularized = hq.regularized;
  out.ar_bic = ar_ratio(bic, m, n, bic_regularized);
  out.ar_hq = ar_ratio(hq, m, n, hq_regularized);
  out.ar_bic_regularized = bic_regularized;
  out.ar_hq_regularized = hq_regularized;

  bool direct_is_stable = omega_n > c0 * sample_gamma[0] &&
    std::isfinite(out.direct) && out.direct >= lower && out.direct <= upper;

  if (direct_is_stable) {
    out.stabilized = out.direct;
  } else {
    out.stabilized = std::max(lower, std::min(upper, out.ar_bic));
    out.stabilized_fallback = true;
  }

  out.hybrid_direct_branch = static_cast<double>(h_omega) / m > eta0;
  out.hybrid = out.hybrid_direct_branch ? out.stabilized : out.b_based;
  return out;
}

std::vector<double> block_sums(const std::vector<double>& x, int m) {
  int q = static_cast<int>(x.size()) / m;
  std::vector<double> sums(q, 0.0);
  for (int j = 0; j < q; ++j) {
    for (int t = 0; t < m; ++t) sums[j] += x[j * m + t];
  }
  return sums;
}

double population_omega(const NumericVector& gamma, int r) {
  double value = gamma[0];
  int last = std::min(r - 1, static_cast<int>(gamma.size()) - 1);
  for (int h = 1; h <= last; ++h) {
    value += 2.0 * (1.0 - static_cast<double>(h) / r) * gamma[h];
  }
  return value;
}

struct Accumulator {
  long long cover = 0;
  long long convex_fail = 0;
  long long factor_valid = 0;
  long long fallback = 0;
  long long direct_branch = 0;
  long double factor_sum = 0.0;
  long double factor_sum2 = 0.0;
  long double order_sum = 0.0;
  long long paired_sum = 0;
  long long paired_sum2 = 0;
};

struct SelectorAccumulator {
  long long cover = 0;
  long long convex_fail = 0;
  long long cap_count = 0;
  long long fallback = 0;
  long long direct_branch = 0;
  long double m_sum = 0.0;
  long double m_sum2 = 0.0;
  long double c_sum = 0.0;
  long double c_sum2 = 0.0;
  long double b_sum = 0.0;
  long double b_sum2 = 0.0;
  long double kappa_sum = 0.0;
  long double kappa_sum2 = 0.0;
  long long paired_sum = 0;
  long long paired_sum2 = 0;
};

int clipped_block_length(double c, int n) {
  int m = static_cast<int>(std::llround(c * std::sqrt(static_cast<double>(n))));
  return std::max(2, std::min(n / 2, m));
}

double estimate_kappa(const std::vector<double>& x, int l) {
  int n = static_cast<int>(x.size());
  int k = n / l;
  if (k < 2) return 0.0;

  std::vector<double> w(k, 0.0);
  double root_l = std::sqrt(static_cast<double>(l));
  for (int j = 0; j < k; ++j) {
    for (int t = 0; t < l; ++t) w[j] += x[j * l + t];
    w[j] /= root_l;
  }

  double mean = 0.0;
  for (double v : w) mean += v;
  mean /= k;

  long double m2 = 0.0;
  long double m3 = 0.0;
  long double m4 = 0.0;
  for (double v : w) {
    double u = v - mean;
    double u2 = u * u;
    m2 += u2;
    m3 += u2 * u;
    m4 += u2 * u2;
  }
  m2 /= k;
  m3 /= k;
  m4 /= k;

  if (!(m2 > tiny)) return 0.0;
  double eta3 = static_cast<double>(m3 / std::pow(m2, 1.5));
  double eta4 = static_cast<double>(m4 / (m2 * m2));
  return l * (0.5 * (eta4 - 3.0) - eta3 * eta3 / 3.0);
}

double estimate_b_for_selector(const std::vector<double>& gamma, int h) {
  double sigma_hat = gamma[0];
  double b2_hat = 0.0;
  for (int lag = 1; lag <= h; ++lag) {
    sigma_hat += 2.0 * gamma[lag];
    b2_hat += 2.0 * lag * gamma[lag];
  }
  if (!(sigma_hat > 0.0) || !std::isfinite(sigma_hat)) return 0.0;
  double b = b2_hat / sigma_hat;
  return std::max(-static_cast<double>(h), std::min(static_cast<double>(h), b));
}

} // namespace

// [[Rcpp::export]]
DataFrame simulate_fixed_blocks_cpp(int n,
                                    IntegerVector m_values,
                                    NumericVector alpha,
                                    int reps,
                                    int seed,
                                    double ar1,
                                    double ar2,
                                    double ma1,
                                    double ma2,
                                    int innovation,
                                    double innovation_scale,
                                    NumericVector true_gamma,
                                    double c0 = 0.20,
                                    double lower = 0.05,
                                    double upper = 5.0,
                                    double eta0 = 0.50,
                                    int burnin = 1000,
                                    int n_threads = 1) {
#ifdef _OPENMP
  omp_set_num_threads(std::max(1, n_threads));
#endif

  const std::vector<std::string> methods = {
    "BEL", "B", "V_population", "V_b", "V_direct", "V_stabilized",
    "V_ar_bic", "V_ar_hq", "V_hybrid", "VB_population", "VB_b",
    "VB_direct", "VB_stabilized", "VB_ar_bic", "VB_ar_hq", "VB_hybrid"
  };

  int nm = m_values.size();
  int na = alpha.size();
  int nj = methods.size();
  int h_omega = std::min(n - 2, std::max(1, static_cast<int>(std::floor(std::log(static_cast<double>(n)) * std::log(std::log(static_cast<double>(n)))))));

  std::vector<double> critical(na);
  for (int a = 0; a < na; ++a) critical[a] = R::qchisq(1.0 - alpha[a], 1.0, 1, 0);

  std::vector<double> true_factor(nm);
  double omega_n = population_omega(true_gamma, n);
  for (int im = 0; im < nm; ++im) {
    true_factor[im] = population_omega(true_gamma, m_values[im]) / omega_n;
  }

  int nt = 1;
#ifdef _OPENMP
  nt = std::max(1, omp_get_max_threads());
#endif

  int total = nm * na * nj;
  std::vector<std::vector<Accumulator>> thread_acc(nt, std::vector<Accumulator>(total));

#pragma omp parallel for schedule(static)
  for (int r = 0; r < reps; ++r) {
#ifdef _OPENMP
    int tid = omp_get_thread_num();
#else
    int tid = 0;
#endif
    std::mt19937_64 rng(splitmix64(static_cast<uint64_t>(seed) + static_cast<uint64_t>(r + 1)));
    std::vector<double> x(n);
    simulate_arma(x, burnin, ar1, ar2, ma1, ma2, innovation, innovation_scale, rng);

    std::vector<double> gamma_uncentered;
    sample_autocovariances(x, h_omega, false, gamma_uncentered);
    ARFit bic = fit_ar_sieve(x, 1);
    ARFit hq = fit_ar_sieve(x, 2);

    for (int im = 0; im < nm; ++im) {
      int m = m_values[im];
      int q = n / m;
      std::vector<double> sums = block_sums(x, m);
      double lr = empirical_likelihood_ratio(sums);
      bool convex_fail = !std::isfinite(lr);

      Factors f = calculate_factors(
        gamma_uncentered, h_omega, m, n, true_factor[im], bic, hq,
        c0, lower, upper, eta0
      );

      std::vector<double> factor = {
        1.0, 1.0,
        f.population, f.b_based, f.direct, f.stabilized, f.ar_bic, f.ar_hq, f.hybrid,
        f.population, f.b_based, f.direct, f.stabilized, f.ar_bic, f.ar_hq, f.hybrid
      };

      std::vector<unsigned char> bartlett = {
        0, 1,
        0, 0, 0, 0, 0, 0, 0,
        1, 1, 1, 1, 1, 1, 1
      };

      std::vector<unsigned char> fallback = {
        0, 0,
        0, static_cast<unsigned char>(f.b_regularized), static_cast<unsigned char>(!f.direct_valid), static_cast<unsigned char>(f.stabilized_fallback),
        static_cast<unsigned char>(f.ar_bic_regularized), static_cast<unsigned char>(f.ar_hq_regularized),
        static_cast<unsigned char>(f.hybrid_direct_branch ? f.stabilized_fallback : f.b_regularized),
        0, static_cast<unsigned char>(f.b_regularized), static_cast<unsigned char>(!f.direct_valid), static_cast<unsigned char>(f.stabilized_fallback),
        static_cast<unsigned char>(f.ar_bic_regularized), static_cast<unsigned char>(f.ar_hq_regularized),
        static_cast<unsigned char>(f.hybrid_direct_branch ? f.stabilized_fallback : f.b_regularized)
      };

      std::vector<int> ar_order = {
        0, 0,
        0, 0, 0, bic.order, bic.order, hq.order, bic.order,
        0, 0, 0, bic.order, bic.order, hq.order, bic.order
      };

      std::vector<unsigned char> indicator(na * nj, 0);
      double bartlett_factor = 1.0 - 3.0 / (2.0 * q);

      for (int a = 0; a < na; ++a) {
        for (int j = 0; j < nj; ++j) {
          int pos = (im * na + a) * nj + j;
          Accumulator& acc = thread_acc[tid][pos];

          if (convex_fail) acc.convex_fail++;
          if (fallback[j]) acc.fallback++;
          if ((j == 8 || j == 15) && f.hybrid_direct_branch) acc.direct_branch++;

          bool factor_ok = std::isfinite(factor[j]) && factor[j] > 0.0;
          if (factor_ok) {
            acc.factor_valid++;
            acc.factor_sum += factor[j];
            acc.factor_sum2 += factor[j] * factor[j];
            acc.order_sum += ar_order[j];
          }

          double statistic = R_PosInf;
          if (!convex_fail && factor_ok) {
            statistic = factor[j] * (bartlett[j] ? bartlett_factor : 1.0) * lr;
          }
          unsigned char covered = std::isfinite(statistic) && statistic <= critical[a];
          indicator[a * nj + j] = covered;
          acc.cover += covered;
        }

        int reference = indicator[a * nj + 9];
        for (int j = 0; j < nj; ++j) {
          int pos = (im * na + a) * nj + j;
          int difference = static_cast<int>(indicator[a * nj + j]) - reference;
          thread_acc[tid][pos].paired_sum += difference;
          thread_acc[tid][pos].paired_sum2 += difference * difference;
        }
      }
    }
  }

  std::vector<Accumulator> acc(total);
  for (int t = 0; t < nt; ++t) {
    for (int k = 0; k < total; ++k) {
      acc[k].cover += thread_acc[t][k].cover;
      acc[k].convex_fail += thread_acc[t][k].convex_fail;
      acc[k].factor_valid += thread_acc[t][k].factor_valid;
      acc[k].fallback += thread_acc[t][k].fallback;
      acc[k].direct_branch += thread_acc[t][k].direct_branch;
      acc[k].factor_sum += thread_acc[t][k].factor_sum;
      acc[k].factor_sum2 += thread_acc[t][k].factor_sum2;
      acc[k].order_sum += thread_acc[t][k].order_sum;
      acc[k].paired_sum += thread_acc[t][k].paired_sum;
      acc[k].paired_sum2 += thread_acc[t][k].paired_sum2;
    }
  }

  int rows = total;
  IntegerVector out_m(rows), out_q(rows), out_neff(rows), out_reps(rows);
  NumericVector out_alpha(rows), nominal(rows), coverage(rows), mc_se(rows), signed_error(rows), abs_error(rows);
  NumericVector convex_fail_rate(rows), factor_valid_rate(rows), mean_factor(rows), factor_bias(rows), factor_sd(rows), factor_rmse(rows);
  NumericVector fallback_rate(rows), direct_branch_rate(rows), mean_ar_order(rows), paired_diff(rows), paired_se(rows), paired_z(rows), true_factor_out(rows);
  CharacterVector method(rows);

  int row = 0;
  for (int im = 0; im < nm; ++im) {
    int m = m_values[im];
    int q = n / m;
    for (int a = 0; a < na; ++a) {
      for (int j = 0; j < nj; ++j, ++row) {
        int pos = (im * na + a) * nj + j;
        const Accumulator& z = acc[pos];
        double p = static_cast<double>(z.cover) / reps;
        double target = 1.0 - alpha[a];
        double diff = static_cast<double>(z.paired_sum) / reps;
        double diff_var = std::max(0.0, static_cast<double>(z.paired_sum2) / reps - diff * diff);

        out_m[row] = m;
        out_q[row] = q;
        out_neff[row] = q * m;
        out_reps[row] = reps;
        out_alpha[row] = alpha[a];
        nominal[row] = target;
        method[row] = methods[j];
        coverage[row] = p;
        mc_se[row] = std::sqrt(p * (1.0 - p) / reps);
        signed_error[row] = p - target;
        abs_error[row] = std::abs(p - target);
        convex_fail_rate[row] = static_cast<double>(z.convex_fail) / reps;
        factor_valid_rate[row] = static_cast<double>(z.factor_valid) / reps;
        fallback_rate[row] = static_cast<double>(z.fallback) / reps;
        direct_branch_rate[row] = static_cast<double>(z.direct_branch) / reps;
        true_factor_out[row] = true_factor[im];

        if (z.factor_valid > 0) {
          double mean = static_cast<double>(z.factor_sum / z.factor_valid);
          double variance = std::max(0.0, static_cast<double>(z.factor_sum2 / z.factor_valid) - mean * mean);
          mean_factor[row] = mean;
          factor_bias[row] = mean - true_factor[im];
          factor_sd[row] = std::sqrt(variance);
          factor_rmse[row] = std::sqrt(variance + factor_bias[row] * factor_bias[row]);
          mean_ar_order[row] = static_cast<double>(z.order_sum / z.factor_valid);
        } else {
          mean_factor[row] = NA_REAL;
          factor_bias[row] = NA_REAL;
          factor_sd[row] = NA_REAL;
          factor_rmse[row] = NA_REAL;
          mean_ar_order[row] = NA_REAL;
        }

        paired_diff[row] = diff;
        paired_se[row] = std::sqrt(diff_var / reps);
        paired_z[row] = paired_se[row] > 0.0 ? diff / paired_se[row] : 0.0;
      }
    }
  }

  return DataFrame::create(
    _["M"] = out_m,
    _["Q"] = out_q,
    _["N_eff"] = out_neff,
    _["reps"] = out_reps,
    _["alpha"] = out_alpha,
    _["nominal"] = nominal,
    _["method"] = method,
    _["coverage"] = coverage,
    _["mc_se"] = mc_se,
    _["signed_error"] = signed_error,
    _["abs_error"] = abs_error,
    _["convex_fail_rate"] = convex_fail_rate,
    _["factor_valid_rate"] = factor_valid_rate,
    _["true_factor"] = true_factor_out,
    _["mean_factor"] = mean_factor,
    _["factor_bias"] = factor_bias,
    _["factor_sd"] = factor_sd,
    _["factor_rmse"] = factor_rmse,
    _["fallback_rate"] = fallback_rate,
    _["direct_branch_rate"] = direct_branch_rate,
    _["mean_ar_order"] = mean_ar_order,
    _["paired_diff_vs_VB_population"] = paired_diff,
    _["paired_diff_se"] = paired_se,
    _["paired_z"] = paired_z
  );
}

// [[Rcpp::export]]
DataFrame simulate_selected_blocks_cpp(int n,
                                       NumericVector alpha,
                                       NumericVector c_star,
                                       IntegerVector q0_values,
                                       int reps,
                                       int seed,
                                       double ar1,
                                       double ar2,
                                       double ma1,
                                       double ma2,
                                       int innovation,
                                       double innovation_scale,
                                       NumericVector true_gamma,
                                       double c_min = 0.05,
                                       double c_max = 2.50,
                                       double c0 = 0.20,
                                       double lower = 0.05,
                                       double upper = 5.0,
                                       double eta0 = 0.50,
                                       int burnin = 1000,
                                       int n_threads = 1) {
#ifdef _OPENMP
  omp_set_num_threads(std::max(1, n_threads));
#endif

  int na = alpha.size();
  int nq0 = q0_values.size();
  int ns = 2 + 2 * nq0;
  int nc = 2;

  std::vector<std::string> selector_names;
  selector_names.push_back("oracle");
  selector_names.push_back("plugin");
  for (int i = 0; i < nq0; ++i) selector_names.push_back("plugin_q" + std::to_string(q0_values[i]));
  for (int i = 0; i < nq0; ++i) selector_names.push_back("oracle_q" + std::to_string(q0_values[i]));

  std::vector<std::string> calibration_names = {"population", "hybrid"};
  std::vector<double> critical(na);
  for (int a = 0; a < na; ++a) critical[a] = R::qchisq(1.0 - alpha[a], 1.0, 1, 0);

  int h_omega = std::min(n - 2, std::max(1, static_cast<int>(std::floor(std::log(static_cast<double>(n)) * std::log(std::log(static_cast<double>(n)))))));
  int h_selector = std::min(n - 2, std::max(1, static_cast<int>(std::floor(std::log(static_cast<double>(n))))));
  int l_selector = std::max(2, static_cast<int>(std::floor(std::pow(static_cast<double>(n), 0.2))));

  int nt = 1;
#ifdef _OPENMP
  nt = std::max(1, omp_get_max_threads());
#endif

  int total = na * ns * nc;
  std::vector<std::vector<SelectorAccumulator>> thread_acc(nt, std::vector<SelectorAccumulator>(total));

#pragma omp parallel for schedule(static)
  for (int r = 0; r < reps; ++r) {
#ifdef _OPENMP
    int tid = omp_get_thread_num();
#else
    int tid = 0;
#endif
    std::mt19937_64 rng(splitmix64(static_cast<uint64_t>(seed) + static_cast<uint64_t>(r + 1)));
    std::vector<double> x(n);
    simulate_arma(x, burnin, ar1, ar2, ma1, ma2, innovation, innovation_scale, rng);

    std::vector<double> gamma_uncentered;
    sample_autocovariances(x, h_omega, false, gamma_uncentered);
    ARFit bic = fit_ar_sieve(x, 1);

    double b_hat = estimate_b_for_selector(gamma_uncentered, h_selector);
    double kappa_hat = estimate_kappa(x, l_selector);

    for (int a = 0; a < na; ++a) {
      double z = critical[a];
      double a_alpha = z / 2.0 - 1.0;
      double d_alpha = 45.0 * z - 1.0;
      double x_hat = 8.0 * (b_hat * a_alpha - kappa_hat) / d_alpha;
      x_hat = std::max(c_min * c_min, std::min(c_max * c_max, x_hat));
      double c_hat = std::sqrt(x_hat);

      int oracle_m = clipped_block_length(c_star[a], n);
      int plugin_m = clipped_block_length(c_hat, n);
      std::vector<int> m_values;
      std::vector<bool> capped;
      m_values.push_back(oracle_m);
      capped.push_back(false);
      m_values.push_back(plugin_m);
      capped.push_back(false);
      for (int i = 0; i < nq0; ++i) {
        int value = std::min(plugin_m, std::max(2, n / q0_values[i]));
        m_values.push_back(value);
        capped.push_back(value < plugin_m);
      }
      for (int i = 0; i < nq0; ++i) {
        int value = std::min(oracle_m, std::max(2, n / q0_values[i]));
        m_values.push_back(value);
        capped.push_back(value < oracle_m);
      }

      std::vector<unsigned char> indicators(ns * nc, 0);

      for (int s = 0; s < ns; ++s) {
        int m = m_values[s];
        int q = n / m;
        std::vector<double> sums = block_sums(x, m);
        double lr = empirical_likelihood_ratio(sums);
        bool convex_fail = !std::isfinite(lr);
        double true_factor = population_omega(true_gamma, m) / population_omega(true_gamma, n);

        ARFit hq_dummy = bic;
        Factors f = calculate_factors(
          gamma_uncentered, h_omega, m, n, true_factor, bic, hq_dummy,
          c0, lower, upper, eta0
        );

        double factors[2] = {true_factor, f.hybrid};
        double bartlett = 1.0 - 3.0 / (2.0 * q);

        for (int c = 0; c < nc; ++c) {
          int pos = (a * ns + s) * nc + c;
          SelectorAccumulator& acc = thread_acc[tid][pos];
          if (convex_fail) acc.convex_fail++;
          if (capped[s]) acc.cap_count++;
          if (c == 1 && f.hybrid_direct_branch) acc.direct_branch++;
          if (c == 1 && f.hybrid_direct_branch && f.stabilized_fallback) acc.fallback++;

          double statistic = R_PosInf;
          if (!convex_fail && std::isfinite(factors[c]) && factors[c] > 0.0) {
            statistic = factors[c] * bartlett * lr;
          }
          unsigned char covered = std::isfinite(statistic) && statistic <= critical[a];
          indicators[s * nc + c] = covered;
          acc.cover += covered;
          acc.m_sum += m;
          acc.m_sum2 += static_cast<double>(m) * m;
          acc.c_sum += c_hat;
          acc.c_sum2 += c_hat * c_hat;
          acc.b_sum += b_hat;
          acc.b_sum2 += b_hat * b_hat;
          acc.kappa_sum += kappa_hat;
          acc.kappa_sum2 += kappa_hat * kappa_hat;
        }
      }

      int reference = indicators[0];
      for (int s = 0; s < ns; ++s) {
        for (int c = 0; c < nc; ++c) {
          int pos = (a * ns + s) * nc + c;
          int difference = static_cast<int>(indicators[s * nc + c]) - reference;
          thread_acc[tid][pos].paired_sum += difference;
          thread_acc[tid][pos].paired_sum2 += difference * difference;
        }
      }
    }
  }

  std::vector<SelectorAccumulator> acc(total);
  for (int t = 0; t < nt; ++t) {
    for (int k = 0; k < total; ++k) {
      acc[k].cover += thread_acc[t][k].cover;
      acc[k].convex_fail += thread_acc[t][k].convex_fail;
      acc[k].cap_count += thread_acc[t][k].cap_count;
      acc[k].fallback += thread_acc[t][k].fallback;
      acc[k].direct_branch += thread_acc[t][k].direct_branch;
      acc[k].m_sum += thread_acc[t][k].m_sum;
      acc[k].m_sum2 += thread_acc[t][k].m_sum2;
      acc[k].c_sum += thread_acc[t][k].c_sum;
      acc[k].c_sum2 += thread_acc[t][k].c_sum2;
      acc[k].b_sum += thread_acc[t][k].b_sum;
      acc[k].b_sum2 += thread_acc[t][k].b_sum2;
      acc[k].kappa_sum += thread_acc[t][k].kappa_sum;
      acc[k].kappa_sum2 += thread_acc[t][k].kappa_sum2;
      acc[k].paired_sum += thread_acc[t][k].paired_sum;
      acc[k].paired_sum2 += thread_acc[t][k].paired_sum2;
    }
  }

  int rows = total;
  NumericVector out_alpha(rows), nominal(rows), coverage(rows), mc_se(rows), signed_error(rows), abs_error(rows);
  NumericVector convex_fail_rate(rows), cap_rate(rows), fallback_rate(rows), direct_branch_rate(rows);
  NumericVector mean_m(rows), sd_m(rows), mean_c_hat(rows), sd_c_hat(rows), mean_b_hat(rows), sd_b_hat(rows), mean_kappa_hat(rows), sd_kappa_hat(rows);
  NumericVector paired_diff(rows), paired_se(rows), paired_z(rows), c_star_out(rows);
  IntegerVector out_reps(rows);
  CharacterVector selector(rows), calibration(rows);

  int row = 0;
  for (int a = 0; a < na; ++a) {
    for (int s = 0; s < ns; ++s) {
      for (int c = 0; c < nc; ++c, ++row) {
        int pos = (a * ns + s) * nc + c;
        const SelectorAccumulator& z = acc[pos];
        double p = static_cast<double>(z.cover) / reps;
        double target = 1.0 - alpha[a];
        double m_mean = static_cast<double>(z.m_sum / reps);
        double c_mean = static_cast<double>(z.c_sum / reps);
        double b_mean = static_cast<double>(z.b_sum / reps);
        double k_mean = static_cast<double>(z.kappa_sum / reps);
        double diff = static_cast<double>(z.paired_sum) / reps;
        double diff_var = std::max(0.0, static_cast<double>(z.paired_sum2) / reps - diff * diff);

        out_alpha[row] = alpha[a];
        nominal[row] = target;
        selector[row] = selector_names[s];
        calibration[row] = calibration_names[c];
        out_reps[row] = reps;
        c_star_out[row] = c_star[a];
        coverage[row] = p;
        mc_se[row] = std::sqrt(p * (1.0 - p) / reps);
        signed_error[row] = p - target;
        abs_error[row] = std::abs(p - target);
        convex_fail_rate[row] = static_cast<double>(z.convex_fail) / reps;
        cap_rate[row] = static_cast<double>(z.cap_count) / reps;
        fallback_rate[row] = static_cast<double>(z.fallback) / reps;
        direct_branch_rate[row] = static_cast<double>(z.direct_branch) / reps;
        mean_m[row] = m_mean;
        sd_m[row] = std::sqrt(std::max(0.0, static_cast<double>(z.m_sum2 / reps) - m_mean * m_mean));
        mean_c_hat[row] = c_mean;
        sd_c_hat[row] = std::sqrt(std::max(0.0, static_cast<double>(z.c_sum2 / reps) - c_mean * c_mean));
        mean_b_hat[row] = b_mean;
        sd_b_hat[row] = std::sqrt(std::max(0.0, static_cast<double>(z.b_sum2 / reps) - b_mean * b_mean));
        mean_kappa_hat[row] = k_mean;
        sd_kappa_hat[row] = std::sqrt(std::max(0.0, static_cast<double>(z.kappa_sum2 / reps) - k_mean * k_mean));
        paired_diff[row] = diff;
        paired_se[row] = std::sqrt(diff_var / reps);
        paired_z[row] = paired_se[row] > 0.0 ? diff / paired_se[row] : 0.0;
      }
    }
  }

  return DataFrame::create(
    _["alpha"] = out_alpha,
    _["nominal"] = nominal,
    _["selector"] = selector,
    _["calibration"] = calibration,
    _["reps"] = out_reps,
    _["C_star"] = c_star_out,
    _["coverage"] = coverage,
    _["mc_se"] = mc_se,
    _["signed_error"] = signed_error,
    _["abs_error"] = abs_error,
    _["convex_fail_rate"] = convex_fail_rate,
    _["cap_rate"] = cap_rate,
    _["fallback_rate"] = fallback_rate,
    _["direct_branch_rate"] = direct_branch_rate,
    _["mean_M"] = mean_m,
    _["sd_M"] = sd_m,
    _["mean_C_hat"] = mean_c_hat,
    _["sd_C_hat"] = sd_c_hat,
    _["mean_b_hat"] = mean_b_hat,
    _["sd_b_hat"] = sd_b_hat,
    _["mean_kappa_hat"] = mean_kappa_hat,
    _["sd_kappa_hat"] = sd_kappa_hat,
    _["paired_diff_vs_oracle_population"] = paired_diff,
    _["paired_diff_se"] = paired_se,
    _["paired_z"] = paired_z
  );
}

// [[Rcpp::export]]
DataFrame simulate_oracle_grid_cpp(int n,
                                   IntegerVector m_values,
                                   NumericVector alpha,
                                   IntegerVector reference_m,
                                   int reps,
                                   int seed,
                                   double ar1,
                                   double ar2,
                                   double ma1,
                                   double ma2,
                                   int innovation,
                                   double innovation_scale,
                                   NumericVector true_gamma,
                                   int burnin = 1000,
                                   int n_threads = 1) {
#ifdef _OPENMP
  omp_set_num_threads(std::max(1, n_threads));
#endif

  int nm = m_values.size();
  int na = alpha.size();
  int total = nm * na;

  std::vector<double> critical(na);
  std::vector<int> reference_index(na, 0);
  for (int a = 0; a < na; ++a) {
    critical[a] = R::qchisq(1.0 - alpha[a], 1.0, 1, 0);
    int best_distance = std::abs(m_values[0] - reference_m[a]);
    for (int im = 1; im < nm; ++im) {
      int distance = std::abs(m_values[im] - reference_m[a]);
      if (distance < best_distance) {
        best_distance = distance;
        reference_index[a] = im;
      }
    }
  }

  std::vector<double> true_factor(nm);
  double omega_n = population_omega(true_gamma, n);
  for (int im = 0; im < nm; ++im) {
    true_factor[im] = population_omega(true_gamma, m_values[im]) / omega_n;
  }

  int nt = 1;
#ifdef _OPENMP
  nt = std::max(1, omp_get_max_threads());
#endif

  std::vector<std::vector<Accumulator>> thread_acc(
    nt, std::vector<Accumulator>(total)
  );

#pragma omp parallel for schedule(static)
  for (int r = 0; r < reps; ++r) {
#ifdef _OPENMP
    int tid = omp_get_thread_num();
#else
    int tid = 0;
#endif
    std::mt19937_64 rng(
      splitmix64(static_cast<uint64_t>(seed) + static_cast<uint64_t>(r + 1))
    );
    std::vector<double> x(n);
    simulate_arma(
      x, burnin, ar1, ar2, ma1, ma2,
      innovation, innovation_scale, rng
    );

    std::vector<unsigned char> indicator(total, 0);

    for (int im = 0; im < nm; ++im) {
      int m = m_values[im];
      int q = n / m;
      std::vector<double> sums = block_sums(x, m);
      double lr = empirical_likelihood_ratio(sums);
      bool convex_fail = !std::isfinite(lr);
      double bartlett = 1.0 - 3.0 / (2.0 * q);

      for (int a = 0; a < na; ++a) {
        int pos = im * na + a;
        Accumulator& acc = thread_acc[tid][pos];
        if (convex_fail) acc.convex_fail++;

        double statistic = R_PosInf;
        if (!convex_fail && bartlett > 0.0 && true_factor[im] > 0.0) {
          statistic = true_factor[im] * bartlett * lr;
        }
        unsigned char covered = std::isfinite(statistic) && statistic <= critical[a];
        indicator[pos] = covered;
        acc.cover += covered;
      }
    }

    for (int a = 0; a < na; ++a) {
      int reference = indicator[reference_index[a] * na + a];
      for (int im = 0; im < nm; ++im) {
        int pos = im * na + a;
        int difference = static_cast<int>(indicator[pos]) - reference;
        thread_acc[tid][pos].paired_sum += difference;
        thread_acc[tid][pos].paired_sum2 += difference * difference;
      }
    }
  }

  std::vector<Accumulator> acc(total);
  for (int t = 0; t < nt; ++t) {
    for (int k = 0; k < total; ++k) {
      acc[k].cover += thread_acc[t][k].cover;
      acc[k].convex_fail += thread_acc[t][k].convex_fail;
      acc[k].paired_sum += thread_acc[t][k].paired_sum;
      acc[k].paired_sum2 += thread_acc[t][k].paired_sum2;
    }
  }

  IntegerVector out_m(total), out_q(total), out_neff(total), out_reps(total), out_reference_m(total);
  NumericVector out_alpha(total), nominal(total), coverage(total), mc_se(total);
  NumericVector signed_error(total), abs_error(total), convex_fail_rate(total);
  NumericVector paired_diff(total), paired_se(total), paired_z(total), true_factor_out(total);

  int row = 0;
  for (int im = 0; im < nm; ++im) {
    int m = m_values[im];
    int q = n / m;
    for (int a = 0; a < na; ++a, ++row) {
      int pos = im * na + a;
      const Accumulator& z = acc[pos];
      double p = static_cast<double>(z.cover) / reps;
      double target = 1.0 - alpha[a];
      double difference = static_cast<double>(z.paired_sum) / reps;
      double difference_variance = std::max(
        0.0,
        static_cast<double>(z.paired_sum2) / reps - difference * difference
      );

      out_m[row] = m;
      out_q[row] = q;
      out_neff[row] = q * m;
      out_reps[row] = reps;
      out_reference_m[row] = m_values[reference_index[a]];
      out_alpha[row] = alpha[a];
      nominal[row] = target;
      coverage[row] = p;
      mc_se[row] = std::sqrt(p * (1.0 - p) / reps);
      signed_error[row] = p - target;
      abs_error[row] = std::abs(p - target);
      convex_fail_rate[row] = static_cast<double>(z.convex_fail) / reps;
      true_factor_out[row] = true_factor[im];
      paired_diff[row] = difference;
      paired_se[row] = std::sqrt(difference_variance / reps);
      paired_z[row] = paired_se[row] > 0.0 ? difference / paired_se[row] : 0.0;
    }
  }

  return DataFrame::create(
    _["M"] = out_m,
    _["Q"] = out_q,
    _["N_eff"] = out_neff,
    _["alpha"] = out_alpha,
    _["nominal"] = nominal,
    _["reps"] = out_reps,
    _["reference_M"] = out_reference_m,
    _["coverage"] = coverage,
    _["mc_se"] = mc_se,
    _["signed_error"] = signed_error,
    _["abs_error"] = abs_error,
    _["convex_fail_rate"] = convex_fail_rate,
    _["true_factor"] = true_factor_out,
    _["paired_diff_vs_C_star"] = paired_diff,
    _["paired_diff_se"] = paired_se,
    _["paired_z"] = paired_z
  );
}
