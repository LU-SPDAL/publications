# Numerical check of the adjacent-block coefficient in the unequal-block design.
# This is a diagnostic calculation and does not change the statistic used in the paper.

if (!requireNamespace("Rcpp", quietly = TRUE)) {
  stop("Package 'Rcpp' is required for this diagnostic.")
}

cpp_code <- r"(
#include <Rcpp.h>
#include <vector>
#include <random>
#include <cmath>
#include <algorithm>
#include <numeric>
#include <limits>
using namespace Rcpp;
using std::vector;

static bool evalF(const vector<double>& u1, const vector<double>& u2,
                  double th, double m, double t,
                  double& F1, double& F2,
                  double& J11, double& J12, double& J21, double& J22) {
  const double th2 = 1.0 - th;
  F1 = F2 = J11 = J12 = J21 = J22 = 0.0;
  for (double u : u1) {
    const double g = u - m;
    const double d = 1.0 + (t / th) * g;
    if (!(d > 1e-12)) return false;
    const double id2 = 1.0 / (d * d);
    F1 += g / d;
    J11 -= id2;
    J12 -= g * g * id2 / th;
  }
  for (double u : u2) {
    const double g = u - m;
    const double d = 1.0 - (t / th2) * g;
    if (!(d > 1e-12)) return false;
    const double id2 = 1.0 / (d * d);
    F2 += g / d;
    J21 -= id2;
    J22 += g * g * id2 / th2;
  }
  return true;
}

static bool profileW_newton(const vector<double>& u1, const vector<double>& u2,
                            double th, double& W) {
  const int q1 = (int)u1.size();
  const int q2 = (int)u2.size();
  const double b1 = std::accumulate(u1.begin(), u1.end(), 0.0) / q1;
  const double b2 = std::accumulate(u2.begin(), u2.end(), 0.0) / q2;

  double v1 = 0.0, v2 = 0.0;
  for (double x : u1) v1 += (x - b1) * (x - b1);
  for (double x : u2) v2 += (x - b2) * (x - b2);
  v1 /= q1; v2 /= q2;
  if (!(v1 > 1e-12 && v2 > 1e-12)) return false;

  // Quadratic-profile starting point.
  double m = (q1 * b1 / v1 + q2 * b2 / v2) / (q1 / v1 + q2 / v2);
  double t = th * (b1 - m) / v1;

  double F1,F2,J11,J12,J21,J22;
  for (int it = 0; it < 60; ++it) {
    if (!evalF(u1,u2,th,m,t,F1,F2,J11,J12,J21,J22)) return false;
    const double norm = std::abs(F1) + std::abs(F2);
    if (norm < 1e-9) {
      const double th2 = 1.0 - th;
      W = 0.0;
      for (double u : u1) {
        const double d = 1.0 + (t/th) * (u-m);
        if (!(d > 0.0)) return false;
        W += 2.0 * std::log(d);
      }
      for (double u : u2) {
        const double d = 1.0 - (t/th2) * (u-m);
        if (!(d > 0.0)) return false;
        W += 2.0 * std::log(d);
      }
      return std::isfinite(W) && W >= -1e-8;
    }

    const double det = J11 * J22 - J12 * J21;
    if (std::abs(det) < 1e-18) return false;
    const double dm = (-F1 * J22 + J12 * F2) / det;
    const double dt = (-J11 * F2 + J21 * F1) / det;

    double step = 1.0;
    bool accepted = false;
    for (int bt = 0; bt < 35; ++bt) {
      const double nm = m + step * dm;
      const double nt = t + step * dt;
      double nF1,nF2,n11,n12,n21,n22;
      if (evalF(u1,u2,th,nm,nt,nF1,nF2,n11,n12,n21,n22)) {
        const double nnorm = std::abs(nF1) + std::abs(nF2);
        if (nnorm <= norm * (1.0 - 1e-4 * step) + 1e-12) {
          m = nm; t = nt; accepted = true; break;
        }
      }
      step *= 0.5;
    }
    if (!accepted) return false;
  }
  return false;
}

static bool lambda_at_m(const vector<double>& u, double m,
                        double& lambda, double& obj) {
  double lo = -1e300, hi = 1e300, sumg = 0.0;
  for (double x : u) {
    const double g = x - m;
    sumg += g;
    if (g > 0.0) lo = std::max(lo, -1.0/g);
    else if (g < 0.0) hi = std::min(hi, -1.0/g);
  }
  if (!(lo < hi)) return false;
  if (std::abs(sumg) < 1e-13) { lambda = 0.0; obj = 0.0; return true; }

  if (lo < -1e200) lo = -1e6;
  else lo += 1e-12 * (1.0 + std::abs(lo));
  if (hi > 1e200) hi = 1e6;
  else hi -= 1e-12 * (1.0 + std::abs(hi));

  auto score = [&](double l) {
    double s = 0.0;
    for (double x : u) {
      const double g = x - m;
      const double d = 1.0 + l*g;
      if (!(d > 0.0)) return std::numeric_limits<double>::quiet_NaN();
      s += g/d;
    }
    return s;
  };

  double slo = score(lo), shi = score(hi);
  if (!(std::isfinite(slo) && std::isfinite(shi) && slo >= 0.0 && shi <= 0.0))
    return false;

  for (int it = 0; it < 100; ++it) {
    const double mid = 0.5*(lo+hi);
    const double sm = score(mid);
    if (sm > 0.0) lo = mid; else hi = mid;
  }
  lambda = 0.5*(lo+hi);
  obj = 0.0;
  for (double x : u) {
    const double d = 1.0 + lambda*(x-m);
    if (!(d > 0.0)) return false;
    obj += 2.0*std::log(d);
  }
  return true;
}

static bool profileW_fallback(const vector<double>& u1, const vector<double>& u2,
                              double th, double& W) {
  double lo = std::max(*std::min_element(u1.begin(),u1.end()),
                       *std::min_element(u2.begin(),u2.end()));
  double hi = std::min(*std::max_element(u1.begin(),u1.end()),
                       *std::max_element(u2.begin(),u2.end()));
  if (!(lo < hi)) return false;
  const double e = 1e-10*(1.0 + std::max(std::abs(lo),std::abs(hi)));
  lo += e; hi -= e;
  if (!(lo < hi)) return false;

  auto f = [&](double m) {
    double l1,l2,o1,o2;
    if (!lambda_at_m(u1,m,l1,o1) || !lambda_at_m(u2,m,l2,o2)) return 1e300;
    return o1+o2;
  };

  const int G = 41;
  const double step = (hi-lo)/(G-1);
  int ib = 0; double fb = 1e300;
  for (int i=0;i<G;++i) {
    const double val = f(lo+i*step);
    if (val < fb) { fb=val; ib=i; }
  }
  double a = lo + std::max(0,ib-1)*step;
  double b = lo + std::min(G-1,ib+1)*step;
  const double gr = (std::sqrt(5.0)-1.0)/2.0;
  double c=b-gr*(b-a), d=a+gr*(b-a), fc=f(c), fd=f(d);
  for (int it=0;it<80;++it) {
    if (fc < fd) { b=d; d=c; fd=fc; c=b-gr*(b-a); fc=f(c); }
    else { a=c; c=d; fc=fd; d=a+gr*(b-a); fd=f(d); }
  }
  W = std::min(fc,fd);
  return std::isfinite(W) && W < 1e200;
}

static bool profileW(const vector<double>& u1, const vector<double>& u2,
                     double th, double& W) {
  if (profileW_newton(u1,u2,th,W)) return true;
  return profileW_fallback(u1,u2,th,W);
}

static void chol_bidiag(int q, double r, vector<double>& diag, vector<double>& sub) {
  diag.assign(q,0.0); sub.assign(q,0.0);
  diag[0] = 1.0;
  for (int i=1;i<q;++i) {
    sub[i] = r/diag[i-1];
    const double v = 1.0-sub[i]*sub[i];
    if (!(v > 0.0)) stop("Requested correlation gives non-positive covariance matrix.");
    diag[i] = std::sqrt(v);
  }
}

static void make_pair(const vector<double>& z, double r, double sd,
                      vector<double>& plus, vector<double>& minus) {
  const int q = (int)z.size();
  vector<double> d,s;
  chol_bidiag(q,r,d,s);
  plus.resize(q); minus.resize(q);
  plus[0] = minus[0] = sd*z[0];
  for (int i=1;i<q;++i) {
    plus[i]  = sd*(d[i]*z[i] + s[i]*z[i-1]);
    minus[i] = sd*(d[i]*z[i] - s[i]*z[i-1]);
  }
}

// [[Rcpp::export]]
List run_linear_design_cpp(int B, double seed_in, int Q1, int Q2,
                           double eps = 0.10,
                           double rho1_base = 0.18, double rho2_base = -0.108,
                           double omega1 = 3.0, double omega2 = 3.0) {
  const int Q = Q1+Q2;
  const double th = ((double)Q1)/Q;
  const double th2 = 1.0-th;
  const double H = omega1/th + omega2/th2;
  const double pi1 = (omega1/th)/H;
  const double pi2 = 1.0-pi1;
  const double K = 3.0*omega1*omega1/std::pow(th,3.0)
                 + 3.0*omega2*omega2/std::pow(th2,3.0);
  const double C = omega1*omega2/(th*th*th2*th2);
  const double b = K/(2.0*H*H) + C/(H*H);
  const double bart_denom = 1.0 + b/Q;

  std::mt19937_64 rng((uint64_t)seed_in);
  std::normal_distribution<double> norm(0.0,1.0);
  vector<double> z1(Q1),z2(Q2),u1p,u1m,u2p,u2m;

  long double sum=0.0L, sum2=0.0L;
  long long fail=0;

  // Two amplitudes; the replicate-level Richardson combination removes the
  // leading O(eps^2) error in the central difference derivative.
  const double e1 = eps;
  const double e2 = eps/2.0;

  for (int rep=0; rep<B; ++rep) {
    for (double& z : z1) z=norm(rng);
    for (double& z : z2) z=norm(rng);

    double y[2];
    bool ok=true;
    for (int k=0;k<2;++k) {
      const double e = (k==0 ? e1 : e2);
      const double r1 = e*rho1_base;
      const double r2 = e*rho2_base;
      make_pair(z1,r1,std::sqrt(omega1),u1p,u1m);
      make_pair(z2,r2,std::sqrt(omega2),u2p,u2m);

      double Wp,Wm;
      if (!profileW(u1p,u2p,th,Wp) || !profileW(u1m,u2m,th,Wm)) {
        ok=false; break;
      }

      // Exact population VC for the 1-dependent Gaussian block array.
      const double link = pi1*(1.0-1.0/Q1)*r1 + pi2*(1.0-1.0/Q2)*r2;
      const double nu_plus  = 1.0/(1.0+2.0*link);
      const double nu_minus = 1.0/(1.0-2.0*link);
      const double H2p = nu_plus *Wp/bart_denom - 1.0;
      const double H2m = nu_minus*Wm/bart_denom - 1.0;

      // Derivative with respect to the scalar path e, evaluated by a symmetric
      // common-random-number difference.
      y[k] = (H2p-H2m)/(2.0*e);
    }
    if (!ok) { ++fail; continue; }

    const double rich = (4.0*y[1]-y[0])/3.0;
    sum += rich;
    sum2 += rich*rich;
  }

  const long long n = ((long long)B)-fail;
  if (n < 2) stop("Too few successful profiles.");
  const double mean = (double)(sum/n);
  const double var = (double)((sum2 - n*(long double)mean*mean)/(n-1));
  const double se = std::sqrt(std::max(0.0,var)/n);

  return List::create(
    _["Q1"]=Q1, _["Q2"]=Q2, _["Q"]=Q,
    _["theta1"]=th, _["pi1"]=pi1, _["b"]=b,
    _["eps"]=eps, _["B"]=B, _["success"]=n, _["fail"]=fail,
    _["derivative_hat"]=mean, _["se"]=se,
    _["Q_times_derivative"]=Q*mean,
    _["Q_times_se"]=Q*se
  );
}
)"

Rcpp::sourceCpp(code = cpp_code, rebuild = TRUE, verbose = FALSE)

# ---- Design B constants from the focused audit ----
rho1_base <- 0.18
rho2_base <- -0.108
omega1 <- omega2 <- 3

# G.7 theory for a given Q, with theta1=.75 and pi1=.25.
theory_G7 <- function(Q) {
  theta1 <- 0.75
  theta2 <- 0.25
  pi1 <- 0.25
  pi2 <- 0.75
  JM <- pi1^2/theta1 + pi2^2/theta2
  Ctilde <- pi1*pi2/(theta1*theta2)
  Jrho <- pi1^2*rho1_base/theta1 + pi2^2*rho2_base/theta2
  Pirho <- pi1*rho1_base + pi2*rho2_base
  Xirho <- pi2*rho1_base + pi1*rho2_base
  (4*Jrho + 2*Ctilde*Xirho - 5*JM*Pirho)/Q
}

# The asymptotic 1/Q coefficient predicted by G.7.
c1_theory <- 120 * theory_G7(120)
stopifnot(abs(c1_theory + 0.276) < 1e-12)

run_design_b_qscaling <- function(B = 1000000L, eps = 0.10,
                                  seed120 = 260830120, seed240 = 260830240,
                                  run_half_Q = FALSE) {

  r120 <- as.data.frame(run_linear_design_cpp(
    B=B, seed_in=seed120, Q1=90L, Q2=30L, eps=eps,
    rho1_base=rho1_base, rho2_base=rho2_base,
    omega1=omega1, omega2=omega2
  ))
  r240 <- as.data.frame(run_linear_design_cpp(
    B=B, seed_in=seed240, Q1=180L, Q2=60L, eps=eps,
    rho1_base=rho1_base, rho2_base=rho2_base,
    omega1=omega1, omega2=omega2
  ))

  rows <- rbind(r120,r240)
  rows$theory_G7 <- theory_G7(rows$Q)
  rows$residual <- rows$derivative_hat - rows$theory_G7
  rows$z_vs_G7_at_finiteQ <- rows$residual/rows$se

  # If derivative(Q)=c1/Q+c2/Q^2+o(Q^-2), then
  # Y(Q)=Q*derivative(Q)=c1+c2/Q+o(Q^-1).
  # With Q2=2*Q1, 2*Y(240)-Y(120) removes c2/Q exactly.
  Y120 <- r120$Q_times_derivative
  Y240 <- r240$Q_times_derivative
  seY120 <- r120$Q_times_se
  seY240 <- r240$Q_times_se
  c1_hat <- 2*Y240 - Y120
  c1_se <- sqrt(4*seY240^2 + seY120^2)
  z_c1 <- (c1_hat-c1_theory)/c1_se

  summary <- data.frame(
    quantity = c("c1_theory_G7", "c1_hat_Q_extrapolated", "se_c1_hat", "z_vs_G7"),
    value = c(c1_theory, c1_hat, c1_se, z_c1)
  )

  if (isTRUE(run_half_Q)) {
    r60 <- as.data.frame(run_linear_design_cpp(
      B=B, seed_in=260830060, Q1=45L, Q2=15L, eps=eps,
      rho1_base=rho1_base, rho2_base=rho2_base,
      omega1=omega1, omega2=omega2
    ))
    r60$theory_G7 <- theory_G7(60)
    r60$residual <- r60$derivative_hat-r60$theory_G7
    r60$z_vs_G7_at_finiteQ <- r60$residual/r60$se
    rows <- rbind(r60,rows)
  }


  if (abs(z_c1) <= 2) {
  } else if (abs(z_c1) <= 3) {
  } else {
  }

  write.csv(rows, "DESIGN_B_QSCALING_ROWS.csv", row.names=FALSE)
  write.csv(summary, "DESIGN_B_QSCALING_SUMMARY.csv", row.names=FALSE)
  invisible(list(rows=rows, summary=summary))
}

# ---------------- HOW TO RUN ----------------
# 1) Quick smoke test:
#    test <- run_design_b_qscaling(B = 20000L, eps = 0.10)
#
# 2) Production diagnostic (recommended):
#    ans <- run_design_b_qscaling(B = 1000000L, eps = 0.10)
#
# 3) If |z_vs_G7| for the extrapolated c1 is still borderline, increase to:
#    ans <- run_design_b_qscaling(B = 2000000L, eps = 0.10)
#
# Optional smaller-Q point for the finite-Q extrapolation.
#    ans <- run_design_b_qscaling(B = 1000000L, eps = 0.10, run_half_Q = TRUE)
