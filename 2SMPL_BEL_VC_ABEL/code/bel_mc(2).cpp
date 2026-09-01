// Fast Monte Carlo engine for two-sample blockwise empirical likelihood
// [[Rcpp::depends(Rcpp)]]
// [[Rcpp::plugins(cpp17)]]
// [[Rcpp::plugins(openmp)]]

#include <Rcpp.h>
#include <algorithm>
#include <array>
#include <cmath>
#include <cstdint>
#include <limits>
#include <numeric>
#include <random>
#include <string>
#include <vector>
#ifdef _OPENMP
#include <omp.h>
#endif

using std::vector;

namespace {

constexpr double NA_D = std::numeric_limits<double>::quiet_NaN();
constexpr double PI_C = 3.141592653589793238462643383279502884;

inline bool finite_d(double x) { return std::isfinite(x); }

uint64_t splitmix64(uint64_t x) {
  x += 0x9e3779b97f4a7c15ULL;
  x = (x ^ (x >> 30)) * 0xbf58476d1ce4e5b9ULL;
  x = (x ^ (x >> 27)) * 0x94d049bb133111ebULL;
  return x ^ (x >> 31);
}

struct InnovCum {
  double k2, k3, k4;
};

InnovCum innov_cumulants(int code) {
  switch (code) {
  case 0: return {1.0, 0.0, 0.0};                 // standard normal
  case 1: return {1.0, 0.0, 3.0};                 // standardized Laplace
  case 2: return {1.0, 2.0*std::sqrt(2.0), 12.0}; // standardized chi-square(1)
  case 3: return {1.0, std::sqrt(2.0), 3.0};      // standardized Gamma(shape=2)
  case 4: return {1.0, 0.0, 1.5};                 // standardized t_8 (stress test)
  case 5: return {1.0, 0.0, -2.0};                // Rademacher (lattice stress test)
  default: Rcpp::stop("Unknown innovation code");
  }
}

template<class RNG>
double draw_innov(int code, RNG &rng) {
  if (code == 0) {
    std::normal_distribution<double> nd(0.0, 1.0);
    return nd(rng);
  }
  if (code == 1) {
    std::uniform_real_distribution<double> ud(-0.5, 0.5);
    double u = ud(rng);
    double x = -std::copysign(std::log1p(-2.0*std::abs(u)), u);
    return x / std::sqrt(2.0);
  }
  if (code == 2) {
    std::gamma_distribution<double> gd(0.5, 2.0);
    return (gd(rng) - 1.0) / std::sqrt(2.0);
  }
  if (code == 3) {
    std::gamma_distribution<double> gd(2.0, 1.0);
    return (gd(rng) - 2.0) / std::sqrt(2.0);
  }
  if (code == 4) {
    std::normal_distribution<double> nd(0.0, 1.0);
    std::gamma_distribution<double> gd(4.0, 2.0); // chi-square(8)
    double z = nd(rng) / std::sqrt(gd(rng) / 8.0);
    return z * std::sqrt(6.0 / 8.0);              // unit variance
  }
  if (code == 5) {
    std::uniform_int_distribution<int> bd(0, 1);
    return bd(rng) ? 1.0 : -1.0;
  }
  return NA_D;
}

struct DGP {
  int model;   // 0 AR(1), 1 MA(2)
  double p1;   // AR phi or MA theta1
  double p2;   // unused for AR; MA theta2
  int innov;
  double lrv_sd; // target long-run standard deviation
};

double filter_scale(const DGP &d) {
  if (d.model == 0) {
    if (std::abs(d.p1) >= 0.999999) return NA_D;
    return d.lrv_sd * (1.0 - d.p1);
  }
  if (d.model == 1) {
    double s = 1.0 + d.p1 + d.p2;
    if (std::abs(s) < 1e-10) return NA_D;
    return d.lrv_sd / std::abs(s);
  }
  return NA_D;
}

template<class RNG>
vector<double> simulate_series(int n, double mu, const DGP &d, int burn, RNG &rng) {
  vector<double> x(n);
  double sc = filter_scale(d);
  if (!finite_d(sc)) return vector<double>();

  if (d.model == 0) {
    int B = std::max(burn, 100);
    double y = 0.0;
    for (int t = 0; t < B + n; ++t) {
      double e = sc * draw_innov(d.innov, rng);
      y = d.p1 * y + e;
      if (t >= B) x[t-B] = mu + y;
    }
    return x;
  }

  // MA(2)
  vector<double> e(n + 2);
  for (int i = 0; i < n + 2; ++i) e[i] = draw_innov(d.innov, rng);
  for (int t = 0; t < n; ++t) {
    double y = sc * (e[t+2] + d.p1 * e[t+1] + d.p2 * e[t]);
    x[t] = mu + y;
  }
  return x;
}

// Sum of r-th powers of coefficients mapping innovations into a sum of M observations.
double coeff_power_sum_ar1(int M, double phi, double sc, int r) {
  if (M <= 0) return NA_D;
  double den = 1.0 - phi;
  double out = 0.0;

  // innovations inside the block: coefficients (1-phi^k)/(1-phi), k=1,...,M
  for (int k = 1; k <= M; ++k) {
    double c = sc * (1.0 - std::pow(phi, k)) / den;
    out += std::pow(c, r);
  }

  // innovations before the block: geometric tail
  double base = sc * (1.0 - std::pow(phi, M)) / den;
  double phir = std::pow(phi, r);
  double tail = std::pow(base, r) * phir / (1.0 - phir);
  out += tail;
  return out;
}

double coeff_power_sum_ma2(int M, double th1, double th2, double sc, int r) {
  std::array<double,3> a = {sc, sc*th1, sc*th2};
  double out = 0.0;
  // innovation index s ranges from -1 to M (when observations are t=1,...,M)
  for (int s = -1; s <= M; ++s) {
    double c = 0.0;
    for (int k = 0; k <= 2; ++k) {
      int t = s + k;
      if (t >= 1 && t <= M) c += a[k];
    }
    out += std::pow(c, r);
  }
  return out;
}

struct BlockTruth {
  double m2, m3, m4;
  double omega;
};

BlockTruth block_truth(const DGP &d, int M) {
  InnovCum ic = innov_cumulants(d.innov);
  double sc = filter_scale(d);
  double s2, s3, s4;
  if (d.model == 0) {
    s2 = coeff_power_sum_ar1(M, d.p1, sc, 2);
    s3 = coeff_power_sum_ar1(M, d.p1, sc, 3);
    s4 = coeff_power_sum_ar1(M, d.p1, sc, 4);
  } else {
    s2 = coeff_power_sum_ma2(M, d.p1, d.p2, sc, 2);
    s3 = coeff_power_sum_ma2(M, d.p1, d.p2, sc, 3);
    s4 = coeff_power_sum_ma2(M, d.p1, d.p2, sc, 4);
  }
  double c2 = ic.k2 * s2;
  double c3 = ic.k3 * s3;
  double c4 = ic.k4 * s4;
  BlockTruth bt;
  bt.m2 = c2 / (double(M)*double(M));
  bt.m3 = c3 / std::pow(double(M), 3.0);
  bt.m4 = (c4 + 3.0*c2*c2) / std::pow(double(M), 4.0);
  bt.omega = c2 / double(M);
  return bt;
}

double long_run_var(const DGP &d) {
  return d.lrv_sd * d.lrv_sd;
}

double B2_truth(const DGP &d) {
  double sc = filter_scale(d);
  if (d.model == 0) {
    double phi = d.p1;
    double var_eps = sc*sc;
    return 2.0 * var_eps * phi /
      ((1.0 - phi*phi) * (1.0 - phi) * (1.0 - phi));
  }
  std::array<double,3> a = {sc, sc*d.p1, sc*d.p2};
  double b2 = 0.0;
  for (int h = 1; h <= 2; ++h) {
    double gam = 0.0;
    for (int j = 0; j+h <= 2; ++j) gam += a[j]*a[j+h];
    b2 += 2.0 * double(h) * gam;
  }
  return b2;
}

vector<double> block_means(const vector<double> &x, int M) {
  int n = int(x.size());
  int Q = n / M;
  vector<double> u(Q, 0.0);
  for (int j = 0; j < Q; ++j) {
    double s = 0.0;
    int start = j*M;
    for (int k = 0; k < M; ++k) s += x[start+k];
    u[j] = s / double(M);
  }
  return u;
}

struct LamInfo {
  bool ok;
  double lam;
  double A; // sum z^2 / den^2
  double B; // sum 1 / den^2
};

LamInfo solve_lambda(const vector<double> &u, double m) {
  int n = int(u.size());
  double zmin = std::numeric_limits<double>::infinity();
  double zmax = -std::numeric_limits<double>::infinity();
  double f0 = 0.0, s2 = 0.0;
  for (double x : u) {
    double z = x - m;
    zmin = std::min(zmin, z);
    zmax = std::max(zmax, z);
    f0 += z;
    s2 += z*z;
  }
  if (!(zmin < 0.0 && zmax > 0.0) || s2 <= 0.0) return {false, NA_D, NA_D, NA_D};
  if (std::abs(f0) <= 1e-13 * (1.0 + std::sqrt(s2))) {
    return {true, 0.0, s2, double(n)};
  }

  double lo0 = -1.0 / zmax;
  double hi0 = -1.0 / zmin;
  double pad = 1e-12 * (1.0 + std::max(std::abs(lo0), std::abs(hi0)));
  double lo = lo0 + pad;
  double hi = hi0 - pad;
  if (!(lo < hi)) return {false, NA_D, NA_D, NA_D};

  double lam = f0 / s2;
  if (!(lam > lo && lam < hi)) lam = 0.5*(lo+hi);

  double A=NA_D, B=NA_D;
  for (int it = 0; it < 50; ++it) {
    double f = 0.0, fp = 0.0;
    A = 0.0; B = 0.0;
    bool good = true;
    for (double x : u) {
      double z = x - m;
      double den = 1.0 + lam*z;
      if (!(den > 0.0) || !finite_d(den)) { good = false; break; }
      double inv = 1.0 / den;
      f += z * inv;
      double inv2 = inv*inv;
      A += z*z*inv2;
      B += inv2;
      fp -= z*z*inv2;
    }
    if (!good || !(A > 0.0)) return {false, NA_D, NA_D, NA_D};
    if (std::abs(f) < 1e-12 * (1.0 + double(n))) return {true, lam, A, B};

    if (f > 0.0) lo = lam; else hi = lam;
    double cand = lam - f/fp;
    if (!(cand > lo && cand < hi) || !finite_d(cand)) cand = 0.5*(lo+hi);
    if (std::abs(cand-lam) < 1e-14*(1.0+std::abs(lam))) {
      lam = cand;
      break;
    }
    lam = cand;
  }

  // final evaluation
  double f = 0.0; A = 0.0; B = 0.0;
  for (double x : u) {
    double z = x-m;
    double den = 1.0 + lam*z;
    if (!(den > 0.0)) return {false, NA_D, NA_D, NA_D};
    double inv = 1.0/den;
    f += z*inv;
    double inv2=inv*inv;
    A += z*z*inv2;
    B += inv2;
  }
  if (std::abs(f) > 1e-7 * (1.0+double(n))) return {false, NA_D, NA_D, NA_D};
  return {true, lam, A, B};
}

struct BelResult {
  bool ok;
  double W;
  double mhat;
};

BelResult two_sample_bel(const vector<double> &u1, const vector<double> &u2,
                         double delta0) {
  vector<double> v2(u2.size());
  for (size_t j=0; j<u2.size(); ++j) v2[j] = u2[j] + delta0;

  auto mm1 = std::minmax_element(u1.begin(), u1.end());
  auto mm2 = std::minmax_element(v2.begin(), v2.end());
  double lo_raw = std::max(*mm1.first, *mm2.first);
  double hi_raw = std::min(*mm1.second, *mm2.second);
  if (!(lo_raw < hi_raw)) return {false, NA_D, NA_D};

  double range = hi_raw - lo_raw;
  double eps = 1e-10 * (1.0 + std::abs(lo_raw) + std::abs(hi_raw) + range);
  double lo = lo_raw + eps;
  double hi = hi_raw - eps;
  if (!(lo < hi)) return {false, NA_D, NA_D};

  auto gfun = [&](double m, double &g, double &gp, LamInfo &l1, LamInfo &l2)->bool {
    l1 = solve_lambda(u1, m);
    l2 = solve_lambda(v2, m);
    if (!l1.ok || !l2.ok) return false;
    g = double(u1.size())*l1.lam + double(v2.size())*l2.lam;
    gp = -double(u1.size())*l1.B/l1.A - double(v2.size())*l2.B/l2.A;
    return finite_d(g) && finite_d(gp) && gp < 0.0;
  };

  double gl, gpl, gh, gph; LamInfo a,b,c,d;
  if (!gfun(lo, gl, gpl, a, b) || !gfun(hi, gh, gph, c, d))
    return {false, NA_D, NA_D};
  if (!(gl > 0.0 && gh < 0.0)) {
    // Numerical fallback: search a bracket on a fine grid.
    bool found=false;
    double prevm=lo, prevg=gl;
    for (int k=1;k<=40;++k) {
      double m = lo + (hi-lo)*double(k)/40.0;
      double gg,gp; LamInfo l1,l2;
      if (!gfun(m,gg,gp,l1,l2)) continue;
      if (prevg>0.0 && gg<0.0) { lo=prevm; hi=m; gl=prevg; gh=gg; found=true; break; }
      prevm=m; prevg=gg;
    }
    if (!found) return {false, NA_D, NA_D};
  }

  double mean1 = std::accumulate(u1.begin(),u1.end(),0.0)/double(u1.size());
  double mean2 = std::accumulate(v2.begin(),v2.end(),0.0)/double(v2.size());
  double m = (double(u1.size())*mean1 + double(v2.size())*mean2) /
             double(u1.size()+v2.size());
  m = std::min(hi, std::max(lo, m));

  LamInfo l1,l2; double g,gp;
  for (int it=0; it<40; ++it) {
    if (!gfun(m,g,gp,l1,l2)) return {false, NA_D, NA_D};
    if (std::abs(g) < 1e-11*(1.0+double(u1.size()+v2.size()))) break;
    if (g>0.0) lo=m; else hi=m;
    double cand = m - g/gp;
    if (!(cand>lo && cand<hi) || !finite_d(cand)) cand=0.5*(lo+hi);
    if (std::abs(cand-m)<1e-13*(1.0+std::abs(m))) { m=cand; break; }
    m=cand;
  }
  if (!gfun(m,g,gp,l1,l2)) return {false, NA_D, NA_D};

  double W=0.0;
  for (double x:u1) {
    double den=1.0+l1.lam*(x-m);
    if (!(den>0.0)) return {false,NA_D,NA_D};
    W += 2.0*std::log(den);
  }
  for (double x:v2) {
    double den=1.0+l2.lam*(x-m);
    if (!(den>0.0)) return {false,NA_D,NA_D};
    W += 2.0*std::log(den);
  }
  if (W < 0.0 && W > -1e-9) W=0.0;
  if (!(W>=0.0) || !finite_d(W)) return {false,NA_D,NA_D};
  return {true,W,m};
}



// -----------------------------------------------------------------------------
// Two-sample adjusted blockwise empirical likelihood (ABEL)
// Pseudo-value in sample a: d0_a(m) = -alpha * theta_a * (bar(y_a)-m).
// The routine below solves the two sample score equations plus the profiled-m
// first-order condition by damped Newton.  A scalar-profile fallback is used
// only if Newton fails.  For alpha>0 the augmented scalar hull contains zero.
// -----------------------------------------------------------------------------

struct AugEval {
  bool ok;
  double score;
  double s_lam;
  double s_m;
  double prof;
  double p_lam;
  double p_m;
  double obj;
  double min_den;
};

AugEval eval_aug_sample(const vector<double> &y, double m, double lam,
                        double alpha, double theta) {
  const int Q = int(y.size());
  if (Q < 2 || !(alpha > 0.0) || !(theta > 0.0))
    return {false,NA_D,NA_D,NA_D,NA_D,NA_D,NA_D,NA_D,NA_D};
  const double c = alpha * theta;
  const double ybar = std::accumulate(y.begin(), y.end(), 0.0) / double(Q);
  const double b = ybar - m;
  const double d0 = -c * b;
  const double den0 = 1.0 + lam*d0;
  if (!(den0 > 1e-13) || !finite_d(den0))
    return {false,NA_D,NA_D,NA_D,NA_D,NA_D,NA_D,NA_D,NA_D};

  double score=0.0, sl=0.0, sm=0.0, obj=0.0;
  double mind=den0;
  for(double yy : y){
    double d = yy-m;
    double den = 1.0 + lam*d;
    if (!(den > 1e-13) || !finite_d(den))
      return {false,NA_D,NA_D,NA_D,NA_D,NA_D,NA_D,NA_D,NA_D};
    mind = std::min(mind, den);
    double inv=1.0/den, inv2=inv*inv;
    score += d*inv;
    sl -= d*d*inv2;
    sm -= inv2;
    obj += 2.0*std::log(den);
  }
  {
    double inv0=1.0/den0, inv02=inv0*inv0;
    score += d0*inv0;
    sl -= d0*d0*inv02;
    sm += c*inv02; // d d0 / d m = +c
    obj += 2.0*std::log(den0);
  }

  const double A = -double(Q+1) + (1.0+c)/den0;
  const double prof = lam*A;
  const double p_lam = A - lam*(1.0+c)*d0/(den0*den0);
  const double p_m = -lam*lam*c*(1.0+c)/(den0*den0);
  bool ok = finite_d(score)&&finite_d(sl)&&finite_d(sm)&&finite_d(prof)&&
            finite_d(p_lam)&&finite_d(p_m)&&finite_d(obj);
  return {ok,score,sl,sm,prof,p_lam,p_m,obj,mind};
}

bool solve3(double A[3][3], double b[3], double x[3]) {
  double M[3][4];
  for(int i=0;i<3;++i){for(int j=0;j<3;++j)M[i][j]=A[i][j];M[i][3]=b[i];}
  for(int k=0;k<3;++k){
    int piv=k;
    for(int i=k+1;i<3;++i) if(std::abs(M[i][k])>std::abs(M[piv][k])) piv=i;
    if(!(std::abs(M[piv][k])>1e-14) || !finite_d(M[piv][k])) return false;
    if(piv!=k) for(int j=k;j<4;++j) std::swap(M[k][j],M[piv][j]);
    double pk=M[k][k];
    for(int j=k;j<4;++j) M[k][j]/=pk;
    for(int i=0;i<3;++i) if(i!=k){
      double f=M[i][k];
      for(int j=k;j<4;++j) M[i][j]-=f*M[k][j];
    }
  }
  for(int i=0;i<3;++i){x[i]=M[i][3]; if(!finite_d(x[i]))return false;}
  return true;
}

struct AugLam { bool ok; double lam; double obj; };

AugLam solve_lambda_aug_fixed_m(const vector<double>&y,double m,double alpha,double theta){
  int Q=int(y.size()); double c=alpha*theta;
  double ybar=std::accumulate(y.begin(),y.end(),0.0)/double(Q);
  vector<double>d; d.reserve(Q+1);
  for(double yy:y)d.push_back(yy-m);
  d.push_back(-c*(ybar-m));
  double dmin=*std::min_element(d.begin(),d.end()), dmax=*std::max_element(d.begin(),d.end());
  double f0=std::accumulate(d.begin(),d.end(),0.0);
  if(std::abs(f0)<1e-13){return {true,0.0,0.0};}
  if(!(dmin<0.0 && dmax>0.0)) return {false,NA_D,NA_D};
  double lo=-1.0/dmax, hi=-1.0/dmin;
  double pad=1e-12*(1.0+std::max(std::abs(lo),std::abs(hi)));
  lo+=pad; hi-=pad; if(!(lo<hi))return {false,NA_D,NA_D};
  double s2=0;for(double z:d)s2+=z*z;
  double lam=(s2>0?f0/s2:0.0); if(!(lam>lo&&lam<hi))lam=0.5*(lo+hi);
  for(int it=0;it<60;++it){
    double f=0,fp=0;
    bool good=true;
    for(double z:d){double den=1+lam*z;if(!(den>0)){good=false;break;} double inv=1.0/den;f+=z*inv;fp-=z*z*inv*inv;}
    if(!good)return {false,NA_D,NA_D};
    if(std::abs(f)<1e-12*(1.0+Q))break;
    if(f>0)lo=lam;else hi=lam;
    double cand=lam-f/fp;
    if(!(cand>lo&&cand<hi)||!finite_d(cand))cand=0.5*(lo+hi);
    lam=cand;
  }
  double obj=0;
  for(double z:d){double den=1+lam*z;if(!(den>0))return {false,NA_D,NA_D};obj+=2.0*std::log(den);}
  return {finite_d(obj),lam,obj};
}

struct AelResult {
  bool ok;
  double W;
  double mhat;
  int iters;
  bool fallback;
  double max_resid;
};

AelResult two_sample_abel(const vector<double>&u1,const vector<double>&u2,
                          double delta0,double alpha){
  if(!(alpha>0.0) || !finite_d(alpha))return {false,NA_D,NA_D,0,false,NA_D};
  vector<double>v2(u2.size()); for(size_t j=0;j<u2.size();++j)v2[j]=u2[j]+delta0;
  int Q1=int(u1.size()),Q2=int(v2.size()),Q=Q1+Q2;
  double th1=double(Q1)/double(Q),th2=double(Q2)/double(Q);
  double mean1=std::accumulate(u1.begin(),u1.end(),0.0)/double(Q1);
  double mean2=std::accumulate(v2.begin(),v2.end(),0.0)/double(Q2);
  double s1=0,s2=0;for(double x:u1){double z=x-mean1;s1+=z*z;}for(double x:v2){double z=x-mean2;s2+=z*z;}
  s1=std::max(s1/double(Q1),1e-12);s2=std::max(s2/double(Q2),1e-12);
  double iw1=double(Q1)/s1,iw2=double(Q2)/s2;
  double m_iv=(iw1*mean1+iw2*mean2)/(iw1+iw2);

  auto normres=[&](const AugEval&a,const AugEval&b){
    return std::max({std::abs(a.score)/double(Q1+1),std::abs(b.score)/double(Q2+1),std::abs(a.prof+b.prof)/double(Q)});
  };

  // ------------------------------------------------------------------------
  // LOCAL SOLVER FROM A GIVEN START.
  // V2 accepted the first stationary point reached from a single start.  The
  // adjusted profile can have more than one local minimum, so a tiny score
  // residual does NOT certify the global profile minimum.  V3 solves from
  // several deterministic starts and retains the smallest locally-minimizing
  // stationary value.  Multipliers are initialized by the exact fixed-m
  // augmented score solves, which is materially more stable than lambda=0.
  // ------------------------------------------------------------------------
  auto local_newton=[&](double mstart)->AelResult{
    double m=mstart;
    AugLam il1=solve_lambda_aug_fixed_m(u1,m,alpha,th1);
    AugLam il2=solve_lambda_aug_fixed_m(v2,m,alpha,th2);
    if(!il1.ok||!il2.ok)return {false,NA_D,NA_D,0,false,NA_D};
    double l1=il1.lam,l2=il2.lam;
    AugEval e1=eval_aug_sample(u1,m,l1,alpha,th1),e2=eval_aug_sample(v2,m,l2,alpha,th2);
    if(!e1.ok||!e2.ok)return {false,NA_D,NA_D,0,false,NA_D};
    int used=0; double nr=normres(e1,e2);
    bool ok=true;
    for(int it=0;ok&&it<50;++it){
      used=it+1; nr=normres(e1,e2);
      if(nr<2e-11)break;
      double J[3][3]={{e1.s_lam,0.0,e1.s_m},{0.0,e2.s_lam,e2.s_m},{e1.p_lam,e2.p_lam,e1.p_m+e2.p_m}};
      double rhs[3]={-e1.score,-e2.score,-(e1.prof+e2.prof)},dx[3];
      if(!solve3(J,rhs,dx)){ok=false;break;}
      bool accepted=false; double step=1.0;
      for(int ls=0;ls<30;++ls){
        double nl1=l1+step*dx[0],nl2=l2+step*dx[1],nm=m+step*dx[2];
        AugEval a=eval_aug_sample(u1,nm,nl1,alpha,th1),b=eval_aug_sample(v2,nm,nl2,alpha,th2);
        if(a.ok&&b.ok&&a.min_den>1e-11&&b.min_den>1e-11){
          double nn=normres(a,b);
          if(nn < nr*(1.0-1e-4*step) || nn<2e-11){
            l1=nl1;l2=nl2;m=nm;e1=a;e2=b;accepted=true;break;
          }
        }
        step*=0.5;
      }
      if(!accepted){ok=false;break;}
    }
    if(!ok)return {false,NA_D,NA_D,used,false,NA_D};
    e1=eval_aug_sample(u1,m,l1,alpha,th1);e2=eval_aug_sample(v2,m,l2,alpha,th2);
    if(!e1.ok||!e2.ok)return {false,NA_D,NA_D,used,false,NA_D};
    nr=normres(e1,e2);
    double W=e1.obj+e2.obj;
    if(!finite_d(W)||W < -1e-8||nr>=1e-7)return {false,NA_D,NA_D,used,false,nr};

    // Profiled second derivative (up to the common factor 2).  Reject a
    // stationary point that is locally a maximum/saddle in m.
    if(!(e1.s_lam<0.0&&e2.s_lam<0.0))return {false,NA_D,NA_D,used,false,nr};
    double curv=(e1.p_m - e1.p_lam*e1.s_m/e1.s_lam) +
                (e2.p_m - e2.p_lam*e2.s_m/e2.s_lam);
    if(!(curv > -1e-9*(1.0+Q)) || !finite_d(curv))return {false,NA_D,NA_D,used,false,nr};
    if(W<0.0)W=0.0;
    return {true,W,m,used,false,nr};
  };

  // Deterministic multi-starts spanning the interval between the two sample
  // means.  The endpoint starts are essential: the V2 validation failure
  // had two genuine local minima; the inverse-variance start converged to
  // W=22.559..., while the start at sample 1's mean converged to the true
  // global value W=21.655....
  vector<double> starts;
  starts.reserve(7);
  starts.push_back(m_iv);
  starts.push_back(mean1);
  starts.push_back(mean2);
  starts.push_back(0.5*(mean1+mean2));
  starts.push_back(0.75*mean1+0.25*mean2);
  starts.push_back(0.25*mean1+0.75*mean2);

  // Remove numerically duplicate starts.
  vector<double> uniq;
  double sc=1.0+std::max(std::abs(mean1),std::abs(mean2));
  for(double z:starts){
    bool dup=false;for(double w:uniq)if(std::abs(z-w)<=1e-12*sc){dup=true;break;}
    if(!dup)uniq.push_back(z);
  }

  AelResult bestloc{false,NA_D,NA_D,0,false,NA_D};
  bool local_failure=false;
  for(double st:uniq){
    AelResult a=local_newton(st);
    if(a.ok){
      if(!bestloc.ok || a.W<bestloc.W)bestloc=a;
    }else local_failure=true;
  }

  // A cheap deterministic challenge set.  If a fixed-m profile value is
  // already below the best stationary value, then some lower basin has been
  // missed and we trigger the robust global fallback.  We check the two
  // means and three interior points; these solves are O(Q) and much cheaper
  // than doing a dense global grid on every Monte Carlo replication.
  auto objfun=[&](double mm)->double{
    AugLam a=solve_lambda_aug_fixed_m(u1,mm,alpha,th1),b=solve_lambda_aug_fixed_m(v2,mm,alpha,th2);
    if(!a.ok||!b.ok)return std::numeric_limits<double>::infinity();
    return a.obj+b.obj;
  };
  bool suspicious=!bestloc.ok;
  if(bestloc.ok){
    const double probe_t[5]={0.0,0.25,0.5,0.75,1.0};
    for(double tt:probe_t){
      double mm=(1.0-tt)*mean1+tt*mean2;
      double ww=objfun(mm);
      if(finite_d(ww) && ww < bestloc.W-1e-8*(1.0+std::abs(bestloc.W))){
        suspicious=true;break;
      }
    }
  }

  // In the ordinary null/local-alternative regime the multi-start solution
  // is normally enough.  If all starts behaved cleanly and no challenge
  // point beats it, accept the smallest local minimum.
  if(bestloc.ok && !suspicious && !local_failure)return bestloc;

  // ------------------------------------------------------------------------
  // ROBUST GLOBAL FALLBACK.
  // Used only for suspicious/failed multistart cases.  Scan a dense grid,
  // refine every grid-local minimum, and compare with the best local result.
  // ------------------------------------------------------------------------
  double ymin=std::min(*std::min_element(u1.begin(),u1.end()),*std::min_element(v2.begin(),v2.end()));
  double ymax=std::max(*std::max_element(u1.begin(),u1.end()),*std::max_element(v2.begin(),v2.end()));
  double range=std::max(ymax-ymin,std::sqrt(std::max(s1,s2)));
  range=std::max(range,1e-6);
  double lo=ymin-6.0*range,hi=ymax+6.0*range;
  const int NG=241;
  vector<double> gm(NG),gf(NG);
  double best=bestloc.ok?bestloc.W:std::numeric_limits<double>::infinity();
  double bestm=bestloc.ok?bestloc.mhat:m_iv; int bestk=-1;
  for(int k=0;k<NG;++k){
    gm[k]=lo+(hi-lo)*double(k)/double(NG-1);
    gf[k]=objfun(gm[k]);
    if(gf[k]<best){best=gf[k];bestm=gm[k];bestk=k;}
  }
  if(!finite_d(best))return {false,NA_D,NA_D,bestloc.iters,true,NA_D};

  const double gr=(std::sqrt(5.0)-1.0)/2.0;
  auto refine=[&](double gl,double gh)->void{
    if(!(gl<gh))return;
    double x1=gh-gr*(gh-gl),x2=gl+gr*(gh-gl),f1=objfun(x1),f2=objfun(x2);
    for(int it=0;it<100;++it){
      if(f1>f2){gl=x1;x1=x2;f1=f2;x2=gl+gr*(gh-gl);f2=objfun(x2);}
      else{gh=x2;x2=x1;f2=f1;x1=gh-gr*(gh-gl);f1=objfun(x1);}
      if(std::abs(gh-gl)<1e-12*(1.0+std::abs(0.5*(gl+gh))))break;
    }
    double mm=(f1<f2?x1:x2),ff=std::min(f1,f2);
    if(ff<best){best=ff;bestm=mm;}
  };
  for(int k=1;k<NG-1;++k){
    if(finite_d(gf[k])&&gf[k]<=gf[k-1]&&gf[k]<=gf[k+1])refine(gm[k-1],gm[k+1]);
  }
  if(bestk>=0)refine(gm[std::max(0,bestk-1)],gm[std::min(NG-1,bestk+1)]);
  if(best<0.0&&best>-1e-8)best=0.0;
  return {finite_d(best)&&best>=0.0,best,bestm,bestloc.iters,true,NA_D};
}

struct Moments4 { double m2,m3,m4; };
Moments4 central_moments_234(const vector<double> &x) {
  int n=int(x.size());
  if (n<2) return {NA_D,NA_D,NA_D};
  double mean=std::accumulate(x.begin(),x.end(),0.0)/double(n);
  double s2=0,s3=0,s4=0;
  for(double v:x){ double z=v-mean; double z2=z*z; s2+=z2; s3+=z2*z; s4+=z2*z2; }
  return {s2/double(n),s3/double(n),s4/double(n)};
}

double bartlett_factor_from_mom(const Moments4 &a, const Moments4 &b,
                                 int Q1,int Q2) {
  double Q=double(Q1+Q2), th1=double(Q1)/Q, th2=double(Q2)/Q;
  double H=a.m2/th1+b.m2/th2;
  if (!(H>0.0)) return NA_D;
  double G=a.m3/(th1*th1)-b.m3/(th2*th2);
  double K=a.m4/(th1*th1*th1)+b.m4/(th2*th2*th2);
  double C=a.m2*b.m2/(th1*th1*th2*th2);
  return -G*G/(3.0*H*H*H)+K/(2.0*H*H)+C/(H*H);
}

double mean_segment(const vector<double>&x,int start,int len){
  double s=0; for(int i=0;i<len;++i)s+=x[start+i]; return s/double(len);
}

struct LagStats { bool ok; double sig2; double B2; };
LagStats rectangular_lag_stats(const vector<double>&x,int start,int len,int L){
  if (len<=1 || L<0 || L>=len) return {false,NA_D,NA_D};
  double mu=mean_segment(x,start,len);
  double gam0=0.0;
  for(int t=0;t<len;++t){ double z=x[start+t]-mu; gam0+=z*z; }
  gam0/=double(len);
  double sig=gam0, bb=0.0;
  for(int h=1;h<=L;++h){
    double g=0.0;
    for(int t=0;t<len-h;++t) g+=(x[start+t]-mu)*(x[start+t+h]-mu);
    g/=double(len); // centered sample autocovariance with denominator n
    sig += 2.0*g;
    bb += 2.0*double(h)*g;
  }
  return {finite_d(sig)&&finite_d(bb),sig,bb};
}

double calibration_fun(const LagStats&a,const LagStats&b,
                       int N1,int N2,int M1,int M2){
  if(!a.ok||!b.ok)return NA_D;
  double num=(a.sig2-a.B2/double(M1))/double(N1) +
             (b.sig2-b.B2/double(M2))/double(N2);
  double den=(a.sig2-a.B2/double(N1))/double(N1) +
             (b.sig2-b.B2/double(N2))/double(N2);
  if(!finite_d(num)||!finite_d(den)||std::abs(den)<1e-14)return NA_D;
  return num/den;
}

double jackknife_nu(const vector<double>&x1,const vector<double>&x2,
                    int N1,int N2,int M1,int M2,int L){
  LagStats f1=rectangular_lag_stats(x1,0,N1,L);
  LagStats f2=rectangular_lag_stats(x2,0,N2,L);
  double nuF=calibration_fun(f1,f2,N1,N2,M1,M2);
  if(!finite_d(nuF))return NA_D;
  int n11=N1/2,n12=N1-n11,n21=N2/2,n22=N2-n21;
  if(L>=std::min(std::min(n11,n12),std::min(n21,n22)))return NA_D;
  LagStats a11=rectangular_lag_stats(x1,0,n11,L);
  LagStats a21=rectangular_lag_stats(x2,0,n21,L);
  LagStats a12=rectangular_lag_stats(x1,n11,n12,L);
  LagStats a22=rectangular_lag_stats(x2,n21,n22,L);
  double nu1=calibration_fun(a11,a21,N1,N2,M1,M2);
  double nu2=calibration_fun(a12,a22,N1,N2,M1,M2);
  if(!finite_d(nu1)||!finite_d(nu2))return NA_D;
  return 2.0*nuF-0.5*(nu1+nu2);
}



// Safeguarded full-sample + split-sample JK VC used in the final ABEL study.
struct SB2 { double sig2,B2,gamma0; bool valid,sigma_floor,B2_cap; };
SB2 estimate_sigma_B2_safe(const vector<double>&x,int start,int len,int L,int M,double eps=1e-8){
  if(len<=1||L<0||L>=len)return {NA_D,NA_D,NA_D,false,false,false};
  double mu=mean_segment(x,start,len), g0=0.0;
  for(int t=0;t<len;++t){double z=x[start+t]-mu;g0+=z*z;} g0/=double(len);
  double sig=g0,bb=0.0;
  for(int h=1;h<=L;++h){
    double g=0.0;for(int t=0;t<len-h;++t)g+=(x[start+t]-mu)*(x[start+t+h]-mu);g/=double(len);
    sig+=2.0*g;bb+=2.0*double(h)*g;
  }
  double floorv=eps*std::max(g0,std::numeric_limits<double>::epsilon());bool sf=false,bc=false;
  if(!finite_d(sig)||sig<floorv){sig=floorv;sf=true;}
  if(bb>0.0){double up=(1.0-eps)*double(M)*sig;if(bb>up){bb=up;bc=true;}}
  bool ok=finite_d(sig)&&finite_d(bb)&&finite_d(g0)&&g0>0.0&&sig>0.0;
  return {sig,bb,g0,ok,sf,bc};
}
struct Nu2 {double nu;bool valid;};
Nu2 nu_from_sb2(const SB2&a,const SB2&b,int M1,int M2,int N1,int N2){
  if(!a.valid||!b.valid)return {NA_D,false};
  double om1M=a.sig2-a.B2/double(M1),om2M=b.sig2-b.B2/double(M2);
  double om1N=a.sig2-a.B2/double(N1),om2N=b.sig2-b.B2/double(N2);
  double num=om1M/double(N1)+om2M/double(N2),den=om1N/double(N1)+om2N/double(N2);
  bool ok=finite_d(num)&&finite_d(den)&&num>0.0&&den>0.0;
  return {ok?num/den:NA_D,ok};
}
struct FJK2 {double full,jk;bool full_valid,jk_raw_valid,jk_fallback,any_cap;};
FJK2 estimate_full_jk_safe(const vector<double>&x1,const vector<double>&x2,
                           int N1,int N2,int M1,int M2,int L){
  SB2 f1=estimate_sigma_B2_safe(x1,0,N1,L,M1),f2=estimate_sigma_B2_safe(x2,0,N2,L,M2);
  Nu2 nf=nu_from_sb2(f1,f2,M1,M2,N1,N2);
  bool any=f1.sigma_floor||f2.sigma_floor||f1.B2_cap||f2.B2_cap;
  int n11=N1/2,n12=N1-n11,n21=N2/2,n22=N2-n21;
  int mh=std::min(std::min(n11,n12),std::min(n21,n22));
  if(!nf.valid||mh<=L+1)return {nf.nu,nf.nu,nf.valid,false,true,any};
  SB2 h11=estimate_sigma_B2_safe(x1,0,n11,L,M1),h12=estimate_sigma_B2_safe(x1,n11,n12,L,M1);
  SB2 h21=estimate_sigma_B2_safe(x2,0,n21,L,M2),h22=estimate_sigma_B2_safe(x2,n21,n22,L,M2);
  Nu2 nA=nu_from_sb2(h11,h21,M1,M2,N1,N2),nB=nu_from_sb2(h12,h22,M1,M2,N1,N2);
  any=any||h11.sigma_floor||h12.sigma_floor||h21.sigma_floor||h22.sigma_floor||h11.B2_cap||h12.B2_cap||h21.B2_cap||h22.B2_cap;
  bool raw=nf.valid&&nA.valid&&nB.valid;double jk=raw?2.0*nf.nu-0.5*(nA.nu+nB.nu):NA_D;
  bool fb=!raw||!finite_d(jk)||jk<=0.0;if(fb)jk=nf.nu;
  return {nf.nu,jk,nf.valid,raw&&!fb,fb,any};
}

double sample_mean(const vector<double>&x){ return std::accumulate(x.begin(),x.end(),0.0)/double(x.size()); }

double hac_bartlett(const vector<double>&x,int L){
  int n=int(x.size()); if(n<2)return NA_D; L=std::min(L,n-1);
  double mu=sample_mean(x);
  double g0=0.0; for(double v:x){double z=v-mu;g0+=z*z;} g0/=double(n);
  double out=g0;
  for(int h=1;h<=L;++h){
    double g=0;for(int t=0;t<n-h;++t)g+=(x[t]-mu)*(x[t+h]-mu);g/=double(n);
    double w=1.0-double(h)/double(L+1);
    out+=2.0*w*g;
  }
  return out;
}

struct TruthAll {
  double b,nu,H,G,K,C,theta1,theta2,pi1,pi2,LM,delta1,delta2,VN,VM;
};

TruthAll scenario_truth(const DGP&d1,const DGP&d2,int N1,int N2,int M1,int M2){
  int Q1=N1/M1,Q2=N2/M2; double Q=double(Q1+Q2);
  double th1=double(Q1)/Q,th2=double(Q2)/Q;
  BlockTruth bm1=block_truth(d1,M1),bm2=block_truth(d2,M2);
  double H=bm1.m2/th1+bm2.m2/th2;
  double G=bm1.m3/(th1*th1)-bm2.m3/(th2*th2);
  double K=bm1.m4/(th1*th1*th1)+bm2.m4/(th2*th2*th2);
  double C=bm1.m2*bm2.m2/(th1*th1*th2*th2);
  double b=-G*G/(3*H*H*H)+K/(2*H*H)+C/(H*H);
  BlockTruth n1=block_truth(d1,N1),n2=block_truth(d2,N2);
  double VM=bm1.omega/double(N1)+bm2.omega/double(N2);
  double VN=n1.omega/double(N1)+n2.omega/double(N2);
  double nu=VM/VN;
  double pi1=(bm1.m2/th1)/H,pi2=(bm2.m2/th2)/H;
  double LM=th2*pi1*pi1+th1*pi2*pi2;
  double sig1=long_run_var(d1),sig2=long_run_var(d2);
  double del1=B2_truth(d1)/sig1,del2=B2_truth(d2)/sig2;
  return {b,nu,H,G,K,C,th1,th2,pi1,pi2,LM,del1,del2,VN,VM};
}

} // anonymous namespace

// [[Rcpp::export]]
Rcpp::List abel_profile_cpp(Rcpp::NumericVector u1r,Rcpp::NumericVector u2r,
                            double delta0,double alpha){
  vector<double>u1(u1r.begin(),u1r.end()),u2(u2r.begin(),u2r.end());
  AelResult a=two_sample_abel(u1,u2,delta0,alpha);
  BelResult b=two_sample_bel(u1,u2,delta0);
  return Rcpp::List::create(
    Rcpp::_ ["ABEL_ok"]=a.ok,Rcpp::_ ["ABEL_W"]=a.W,Rcpp::_ ["ABEL_m"]=a.mhat,
    Rcpp::_ ["ABEL_iters"]=a.iters,Rcpp::_ ["ABEL_fallback"]=a.fallback,Rcpp::_ ["ABEL_resid"]=a.max_resid,
    Rcpp::_ ["BEL_ok"]=b.ok,Rcpp::_ ["BEL_W"]=b.W,Rcpp::_ ["BEL_m"]=b.mhat);
}

// [[Rcpp::export]]
Rcpp::List scenario_truth_cpp(int model1,double p11,double p12,int innov1,double lrv_sd1,
                              int model2,double p21,double p22,int innov2,double lrv_sd2,
                              int N1,int N2,int M1,int M2){
  if(N1%M1!=0||N2%M2!=0)Rcpp::stop("N_a must be divisible by M_a");
  DGP d1{model1,p11,p12,innov1,lrv_sd1},d2{model2,p21,p22,innov2,lrv_sd2};
  TruthAll t=scenario_truth(d1,d2,N1,N2,M1,M2);
  return Rcpp::List::create(
    Rcpp::_["b_true"]=t.b,Rcpp::_["nu_true"]=t.nu,
    Rcpp::_["H_M"]=t.H,Rcpp::_["G_M"]=t.G,Rcpp::_["K_M"]=t.K,Rcpp::_["C_M"]=t.C,
    Rcpp::_["theta1"]=t.theta1,Rcpp::_["theta2"]=t.theta2,
    Rcpp::_["pi1"]=t.pi1,Rcpp::_["pi2"]=t.pi2,Rcpp::_["L_M"]=t.LM,
    Rcpp::_["delta1"]=t.delta1,Rcpp::_["delta2"]=t.delta2,
    Rcpp::_["V_N"]=t.VN,Rcpp::_["V_M"]=t.VM
  );
}

// [[Rcpp::export]]
Rcpp::List run_scenario_cpp(int B, double seed, int n_threads,
                            int model1,double p11,double p12,int innov1,double lrv_sd1,
                            int model2,double p21,double p22,int innov2,double lrv_sd2,
                            int N1,int N2,int M1,int M2,int L_vc,int L_hac1,int L_hac2,
                            double delta_true,double delta0=0.0,int burn=500){
  if(B<=0)Rcpp::stop("B must be positive");
  if(N1%M1!=0||N2%M2!=0)Rcpp::stop("N_a must be divisible by M_a");
  if(M1<2||M2<2)Rcpp::stop("Block lengths must be >=2");
  int Q1=N1/M1,Q2=N2/M2,Q=Q1+Q2;
  if(Q1<3||Q2<3)Rcpp::stop("Need at least 3 blocks per sample");

  DGP d1{model1,p11,p12,innov1,lrv_sd1},d2{model2,p21,p22,innov2,lrv_sd2};
  TruthAll truth=scenario_truth(d1,d2,N1,N2,M1,M2);
  if(!finite_d(truth.b)||!finite_d(truth.nu)||!(truth.VN>0))Rcpp::stop("Invalid oracle quantities");
  const double alpha_true=0.5*truth.b;

  enum {BEL=0,BCO,VCO,VCBCO,ABELO,VCABELO,BCF,VCF,VCBCF,ABELF,VCABELF,
        ABEL075,VCABEL075O,WHAC,WORACLE,BHAT,ALPHAHAT,NUHAT,NUFULL,
        BELFAIL,ABELOFAIL,ABELFFAIL,ABEL075FAIL,JKFALLBACK,VCSAFE,
        ABELOFB,ABELFFB,ABEL075FB,DIFFO,DIFFF,ALPHALT025,NCOL};
  std::vector<double> out(size_t(B)*NCOL,NA_D);
  uint64_t base=static_cast<uint64_t>(seed);

  #ifdef _OPENMP
  if(n_threads<1)n_threads=1;
  #pragma omp parallel for schedule(static) num_threads(n_threads)
  #endif
  for(int r=0;r<B;++r){
    uint64_t sr=splitmix64(base ^ (0x9e3779b97f4a7c15ULL*uint64_t(r+1)));
    std::mt19937_64 rng(sr);
    vector<double>x1=simulate_series(N1,delta_true,d1,burn,rng);
    vector<double>x2=simulate_series(N2,0.0,d2,burn,rng);
    size_t off=size_t(r)*NCOL;
    if(x1.empty()||x2.empty()){out[off+BELFAIL]=1;out[off+ABELOFAIL]=1;out[off+ABELFFAIL]=1;continue;}

    vector<double>u1=block_means(x1,M1),u2=block_means(x2,M2);

    // Common diagnostics / competitors.
    double dbar=sample_mean(x1)-sample_mean(x2)-delta0;
    double hv1=hac_bartlett(x1,L_hac1),hv2=hac_bartlett(x2,L_hac2);
    double hv=hv1/double(N1)+hv2/double(N2);
    if(finite_d(hv)&&hv>0.0)out[off+WHAC]=dbar*dbar/hv;
    out[off+WORACLE]=dbar*dbar/truth.VN;

    // Empirical Bartlett factor from divisor-Q_a central moments.
    Moments4 em1=central_moments_234(u1),em2=central_moments_234(u2);
    double bh=bartlett_factor_from_mom(em1,em2,Q1,Q2);
    double ah=finite_d(bh)?0.5*bh:NA_D;
    out[off+BHAT]=bh;out[off+ALPHAHAT]=ah;
    out[off+ALPHALT025]=(finite_d(ah)&&ah<0.25-1e-10)?1.0:0.0;

    // Safeguarded split-sample JK VC, with the full-sample target N_a held fixed.
    FJK2 fjk=estimate_full_jk_safe(x1,x2,N1,N2,M1,M2,L_vc);
    out[off+NUFULL]=fjk.full;out[off+NUHAT]=fjk.jk;
    out[off+JKFALLBACK]=fjk.jk_fallback?1.0:0.0;
    out[off+VCSAFE]=fjk.any_cap?1.0:0.0;

    // Ordinary BEL and its multiplicative corrections (may fail convex hull).
    BelResult br=two_sample_bel(u1,u2,delta0);
    out[off+BELFAIL]=br.ok?0.0:1.0;
    if(br.ok){
      double W=br.W;out[off+BEL]=W;
      if(truth.nu>0.0)out[off+VCO]=truth.nu*W;
      double deno=1.0+truth.b/double(Q);
      if(deno>0.0){out[off+BCO]=W/deno;out[off+VCBCO]=truth.nu*W/deno;}
      double denf=finite_d(bh)?1.0+bh/double(Q):NA_D;
      if(finite_d(denf)&&denf>0.0){
        out[off+BCF]=W/denf;
        if(finite_d(fjk.jk)&&fjk.jk>0.0)out[off+VCBCF]=fjk.jk*W/denf;
      }
      if(finite_d(fjk.jk)&&fjk.jk>0.0)out[off+VCF]=fjk.jk*W;
    }

    // Oracle high-precision ABEL: alpha=b_M/2.
    AelResult ao=two_sample_abel(u1,u2,delta0,alpha_true);
    out[off+ABELOFAIL]=ao.ok?0.0:1.0;out[off+ABELOFB]=ao.fallback?1.0:0.0;
    if(ao.ok){out[off+ABELO]=ao.W;if(truth.nu>0.0)out[off+VCABELO]=truth.nu*ao.W;}

    // Feasible high-precision ABEL: alpha_hat=b_hat/2.
    if(finite_d(ah)&&ah>0.0){
      AelResult af=two_sample_abel(u1,u2,delta0,ah);
      out[off+ABELFFAIL]=af.ok?0.0:1.0;out[off+ABELFFB]=af.fallback?1.0:0.0;
      if(af.ok){out[off+ABELF]=af.W;if(finite_d(fjk.jk)&&fjk.jk>0.0)out[off+VCABELF]=fjk.jk*af.W;}
    } else {out[off+ABELFFAIL]=1.0;out[off+ABELFFB]=0.0;}

    // Fixed alpha=3/4 benchmark: familiar one-sample Gaussian adjustment,
    // intentionally not generally high-precision for the two-sample profile.
    AelResult a75=two_sample_abel(u1,u2,delta0,0.75);
    out[off+ABEL075FAIL]=a75.ok?0.0:1.0;out[off+ABEL075FB]=a75.fallback?1.0:0.0;
    if(a75.ok){out[off+ABEL075]=a75.W;if(truth.nu>0.0)out[off+VCABEL075O]=truth.nu*a75.W;}

    // Direct ABEL-vs-BC equivalence diagnostics on the same sample.
    if(br.ok&&ao.ok){double den=1.0+truth.b/double(Q);if(den>0.0)out[off+DIFFO]=ao.W-br.W/den;}
    if(br.ok&&finite_d(ah)&&ah>0.0&&finite_d(out[off+ABELF])){
      double den=1.0+bh/double(Q);if(den>0.0)out[off+DIFFF]=out[off+ABELF]-br.W/den;
    }
  }

  Rcpp::NumericMatrix mat(B,NCOL);
  for(int r=0;r<B;++r)for(int c=0;c<NCOL;++c)mat(r,c)=out[size_t(r)*NCOL+c];
  Rcpp::CharacterVector cn=Rcpp::CharacterVector::create(
    "BEL","BC_oracle","VC_oracle","VCBC_oracle","ABEL_oracle","VCABEL_oracle",
    "BC_feasible","VC_feasible","VCBC_feasible","ABEL_feasible","VCABEL_feasible",
    "ABEL_fixed075","VCABEL_fixed075_oracleVC","Wald_HAC","Wald_oracle",
    "bhat","alpha_hat","nuhat_JK","nuhat_full","bel_fail","abel_oracle_fail",
    "abel_feasible_fail","abel_fixed075_fail","jk_fallback","vc_safeguard",
    "abel_oracle_solver_fallback","abel_feasible_solver_fallback","abel_fixed075_solver_fallback",
    "abel_minus_bc_oracle","abel_minus_bc_feasible","alpha_hat_lt_025");
  Rcpp::colnames(mat)=cn;

  Rcpp::List tr=Rcpp::List::create(
    Rcpp::_ ["b_true"]=truth.b,Rcpp::_ ["alpha_true"]=alpha_true,Rcpp::_ ["nu_true"]=truth.nu,
    Rcpp::_ ["H_M"]=truth.H,Rcpp::_ ["G_M"]=truth.G,Rcpp::_ ["K_M"]=truth.K,Rcpp::_ ["C_M"]=truth.C,
    Rcpp::_ ["theta1"]=truth.theta1,Rcpp::_ ["theta2"]=truth.theta2,
    Rcpp::_ ["pi1"]=truth.pi1,Rcpp::_ ["pi2"]=truth.pi2,Rcpp::_ ["L_M"]=truth.LM,
    Rcpp::_ ["delta1"]=truth.delta1,Rcpp::_ ["delta2"]=truth.delta2,
    Rcpp::_ ["V_N"]=truth.VN,Rcpp::_ ["V_M"]=truth.VM,
    Rcpp::_ ["Q1"]=Q1,Rcpp::_ ["Q2"]=Q2,Rcpp::_ ["Q"]=Q);
  return Rcpp::List::create(Rcpp::_ ["stats"]=mat,Rcpp::_ ["truth"]=tr);
}
