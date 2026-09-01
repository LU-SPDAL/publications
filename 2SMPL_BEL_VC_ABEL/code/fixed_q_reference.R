# Fixed-block-count reference law and simulation helpers.
# The functions here reuse the main BEL and ABEL engine.

if (!requireNamespace("Rcpp", quietly = TRUE)) {
  stop("Package 'Rcpp' is required. Install it with install.packages('Rcpp').")
}

source("simulation_driver.R")

.dc <- parallel::detectCores(logical = TRUE)
if (is.na(.dc)) .dc <- 1L
FIXEDQ_THREADS_DEFAULT <- max(1L, as.integer(.dc) - 1L)

FIXEDQ_ROOT <- file.path(SIM_DIR, "..", "results", "fixed_q")
dir.create(FIXEDQ_ROOT, showWarnings = FALSE, recursive = TRUE)

# Exact R version of the empirical two-sample Bartlett coefficient.
# Used only to validate the appended C++ wrapper.
central_moments_234_R <- function(x) {
  d <- x - mean(x)
  c(m2 = mean(d^2), m3 = mean(d^3), m4 = mean(d^4))
}

bhat_from_blocks_R <- function(u1, u2) {
  q1 <- length(u1)
  q2 <- length(u2)
  q <- q1 + q2
  th1 <- q1 / q
  th2 <- q2 / q
  a <- central_moments_234_R(u1)
  b <- central_moments_234_R(u2)

  H <- a[["m2"]] / th1 + b[["m2"]] / th2
  G <- a[["m3"]] / th1^2 - b[["m3"]] / th2^2
  K <- a[["m4"]] / th1^3 + b[["m4"]] / th2^3
  C <- a[["m2"]] * b[["m2"]] / (th1^2 * th2^2)

  if (!is.finite(H) || H <= 0) return(NA_real_)
  -G^2 / (3 * H^3) + K / (2 * H^2) + C / H^2
}

# C++ wrappers appended to a temporary copy of the main engine.
FIXEDQ_CPP_ADDON <- r"{

// Fixed-Q wrappers around the main BEL and ABEL engine.

// [[Rcpp::export]]
Rcpp::List fixedq_eval_cpp(Rcpp::NumericVector u1r,
                           Rcpp::NumericVector u2r) {
  vector<double> u1(u1r.begin(), u1r.end());
  vector<double> u2(u2r.begin(), u2r.end());
  int Q1 = int(u1.size()), Q2 = int(u2.size());
  if (Q1 < 3 || Q2 < 3) Rcpp::stop("Need at least 3 blocks per sample");

  Moments4 em1 = central_moments_234(u1);
  Moments4 em2 = central_moments_234(u2);
  double bh = bartlett_factor_from_mom(em1, em2, Q1, Q2);
  double ah = finite_d(bh) ? 0.5 * bh : NA_D;

  AelResult af = finite_d(ah) && ah > 0.0
    ? two_sample_abel(u1, u2, 0.0, ah)
    : AelResult{false, NA_D, NA_D, 0, false, NA_D};
  BelResult br = two_sample_bel(u1, u2, 0.0);

  return Rcpp::List::create(
    Rcpp::_ ["bhat"] = bh,
    Rcpp::_ ["alpha_hat"] = ah,
    Rcpp::_ ["ABEL_ok"] = af.ok,
    Rcpp::_ ["ABEL_W"] = af.W,
    Rcpp::_ ["ABEL_m"] = af.mhat,
    Rcpp::_ ["ABEL_fallback"] = af.fallback,
    Rcpp::_ ["BEL_ok"] = br.ok,
    Rcpp::_ ["BEL_W"] = br.W,
    Rcpp::_ ["BEL_m"] = br.mhat
  );
}

// [[Rcpp::export]]
Rcpp::NumericMatrix fixedq_reference_cpp(int B, double seed, int n_threads,
                                         int Q1, int Q2, double r_scale) {
  if (B <= 0) Rcpp::stop("B must be positive");
  if (Q1 < 3 || Q2 < 3) Rcpp::stop("Need at least 3 blocks per sample");
  if (!finite_d(r_scale) || !(r_scale > 0.0)) Rcpp::stop("r_scale must be positive");

  // tau1 is normalized to 1.  Since r = tau1/tau2, tau2 = 1/r.
  enum {WABEL=0, WBEL, BHAT, ALPHAHAT, BELFAIL, ABELFAIL, ABELFB, NCOL};
  std::vector<double> out(size_t(B) * NCOL, NA_D);
  uint64_t base = static_cast<uint64_t>(seed);

  #ifdef _OPENMP
  if (n_threads < 1) n_threads = 1;
  #pragma omp parallel for schedule(static) num_threads(n_threads)
  #endif
  for (int rr = 0; rr < B; ++rr) {
    uint64_t sr = splitmix64(base ^ (0x9e3779b97f4a7c15ULL * uint64_t(rr + 1)));
    std::mt19937_64 rng(sr);
    std::normal_distribution<double> nd(0.0, 1.0);

    vector<double> u1(Q1), u2(Q2);
    for (int j = 0; j < Q1; ++j) u1[j] = nd(rng);
    for (int j = 0; j < Q2; ++j) u2[j] = nd(rng) / r_scale;

    size_t off = size_t(rr) * NCOL;
    Moments4 em1 = central_moments_234(u1);
    Moments4 em2 = central_moments_234(u2);
    double bh = bartlett_factor_from_mom(em1, em2, Q1, Q2);
    double ah = finite_d(bh) ? 0.5 * bh : NA_D;
    out[off + BHAT] = bh;
    out[off + ALPHAHAT] = ah;

    BelResult br = two_sample_bel(u1, u2, 0.0);
    out[off + BELFAIL] = br.ok ? 0.0 : 1.0;
    if (br.ok) out[off + WBEL] = br.W;

    if (finite_d(ah) && ah > 0.0) {
      AelResult af = two_sample_abel(u1, u2, 0.0, ah);
      out[off + ABELFAIL] = af.ok ? 0.0 : 1.0;
      out[off + ABELFB] = af.fallback ? 1.0 : 0.0;
      if (af.ok) out[off + WABEL] = af.W;
    } else {
      out[off + ABELFAIL] = 1.0;
      out[off + ABELFB] = 0.0;
    }
  }

  Rcpp::NumericMatrix mat(B, NCOL);
  for (int i = 0; i < B; ++i)
    for (int j = 0; j < NCOL; ++j)
      mat(i,j) = out[size_t(i) * NCOL + j];

  Rcpp::colnames(mat) = Rcpp::CharacterVector::create(
    "W_ABEl_feasible", "W_BEL", "bhat", "alpha_hat",
    "BEL_fail", "ABEL_fail", "ABEL_fallback"
  );
  return mat;
}

// [[Rcpp::export]]
Rcpp::List run_fixedq_outer_cpp(int B, double seed, int n_threads,
                                int model1, double p11, double p12, int innov1, double lrv_sd1,
                                int model2, double p21, double p22, int innov2, double lrv_sd2,
                                int N1, int N2, int M1, int M2, int L_vc,
                                double delta_true = 0.0, double delta0 = 0.0,
                                int burn = 500) {
  if (B <= 0) Rcpp::stop("B must be positive");
  if (N1 % M1 != 0 || N2 % M2 != 0) Rcpp::stop("N_a must be divisible by M_a");
  int Q1 = N1 / M1, Q2 = N2 / M2, Q = Q1 + Q2;
  if (Q1 < 3 || Q2 < 3) Rcpp::stop("Need at least 3 blocks per sample");

  DGP d1{model1, p11, p12, innov1, lrv_sd1};
  DGP d2{model2, p21, p22, innov2, lrv_sd2};
  TruthAll truth = scenario_truth(d1, d2, N1, N2, M1, M2);
  BlockTruth bt1 = block_truth(d1, M1);
  BlockTruth bt2 = block_truth(d2, M2);

  double r_true_asym = std::sqrt(
    (long_run_var(d1) / double(M1)) /
    (long_run_var(d2) / double(M2))
  );
  double r_true_block = std::sqrt(bt1.m2 / bt2.m2);

  enum {VCABEL=0, ABEL, VCBC, VC, BEL, BHAT, ALPHAHAT, NUHAT,
        RHATASYM, RHATBLOCK, BELFAIL, ABELFAIL, ABELFB,
        JKFALLBACK, VCSAFE, ALPHALT025, NCOL};
  std::vector<double> out(size_t(B) * NCOL, NA_D);
  uint64_t base = static_cast<uint64_t>(seed);

  #ifdef _OPENMP
  if (n_threads < 1) n_threads = 1;
  #pragma omp parallel for schedule(static) num_threads(n_threads)
  #endif
  for (int rr = 0; rr < B; ++rr) {
    uint64_t sr = splitmix64(base ^ (0x9e3779b97f4a7c15ULL * uint64_t(rr + 1)));
    std::mt19937_64 rng(sr);
    size_t off = size_t(rr) * NCOL;

    vector<double> x1 = simulate_series(N1, delta_true, d1, burn, rng);
    vector<double> x2 = simulate_series(N2, 0.0, d2, burn, rng);
    if (x1.empty() || x2.empty()) {
      out[off + BELFAIL] = 1.0;
      out[off + ABELFAIL] = 1.0;
      continue;
    }

    vector<double> u1 = block_means(x1, M1);
    vector<double> u2 = block_means(x2, M2);

    Moments4 em1 = central_moments_234(u1);
    Moments4 em2 = central_moments_234(u2);
    double bh = bartlett_factor_from_mom(em1, em2, Q1, Q2);
    double ah = finite_d(bh) ? 0.5 * bh : NA_D;
    out[off + BHAT] = bh;
    out[off + ALPHAHAT] = ah;
    out[off + ALPHALT025] = (finite_d(ah) && ah < 0.25 - 1e-10) ? 1.0 : 0.0;

    FJK2 fjk = estimate_full_jk_safe(x1, x2, N1, N2, M1, M2, L_vc);
    out[off + NUHAT] = fjk.jk;
    out[off + JKFALLBACK] = fjk.jk_fallback ? 1.0 : 0.0;
    out[off + VCSAFE] = fjk.any_cap ? 1.0 : 0.0;

    // Full-sample scale estimators for the finite-Q nuisance ratio r.
    SB2 e1 = estimate_sigma_B2_safe(x1, 0, N1, L_vc, M1);
    SB2 e2 = estimate_sigma_B2_safe(x2, 0, N2, L_vc, M2);
    if (e1.valid && e2.valid) {
      double a1 = e1.sig2 / double(M1);
      double a2 = e2.sig2 / double(M2);
      if (finite_d(a1) && finite_d(a2) && a1 > 0.0 && a2 > 0.0)
        out[off + RHATASYM] = std::sqrt(a1 / a2);

      double om1M = e1.sig2 - e1.B2 / double(M1);
      double om2M = e2.sig2 - e2.B2 / double(M2);
      double b1 = om1M / double(M1);
      double b2 = om2M / double(M2);
      if (finite_d(b1) && finite_d(b2) && b1 > 0.0 && b2 > 0.0)
        out[off + RHATBLOCK] = std::sqrt(b1 / b2);
    }

    BelResult br = two_sample_bel(u1, u2, delta0);
    out[off + BELFAIL] = br.ok ? 0.0 : 1.0;
    if (br.ok) {
      out[off + BEL] = br.W;
      if (finite_d(fjk.jk) && fjk.jk > 0.0) {
        out[off + VC] = fjk.jk * br.W;
        double den = finite_d(bh) ? 1.0 + bh / double(Q) : NA_D;
        if (finite_d(den) && den > 0.0)
          out[off + VCBC] = fjk.jk * br.W / den;
      }
    }

    if (finite_d(ah) && ah > 0.0) {
      AelResult af = two_sample_abel(u1, u2, delta0, ah);
      out[off + ABELFAIL] = af.ok ? 0.0 : 1.0;
      out[off + ABELFB] = af.fallback ? 1.0 : 0.0;
      if (af.ok) {
        out[off + ABEL] = af.W;
        if (finite_d(fjk.jk) && fjk.jk > 0.0)
          out[off + VCABEL] = fjk.jk * af.W;
      }
    } else {
      out[off + ABELFAIL] = 1.0;
      out[off + ABELFB] = 0.0;
    }
  }

  Rcpp::NumericMatrix mat(B, NCOL);
  for (int i = 0; i < B; ++i)
    for (int j = 0; j < NCOL; ++j)
      mat(i,j) = out[size_t(i) * NCOL + j];

  Rcpp::colnames(mat) = Rcpp::CharacterVector::create(
    "VCABEL_feasible", "ABEL_feasible", "VCBC_feasible", "VC_feasible", "BEL",
    "bhat", "alpha_hat", "nuhat_JK", "rhat_asym", "rhat_block",
    "bel_fail", "abel_feasible_fail", "abel_feasible_solver_fallback",
    "jk_fallback", "vc_safeguard", "alpha_hat_lt_025"
  );

  Rcpp::List tr = Rcpp::List::create(
    Rcpp::_ ["b_true"] = truth.b,
    Rcpp::_ ["nu_true"] = truth.nu,
    Rcpp::_ ["V_N"] = truth.VN,
    Rcpp::_ ["V_M"] = truth.VM,
    Rcpp::_ ["r_true_asym"] = r_true_asym,
    Rcpp::_ ["r_true_block"] = r_true_block,
    Rcpp::_ ["Q1"] = Q1,
    Rcpp::_ ["Q2"] = Q2,
    Rcpp::_ ["Q"] = Q
  );

  return Rcpp::List::create(Rcpp::_ ["stats"] = mat,
                            Rcpp::_ ["truth"] = tr);
}

}"

compile_fixedq_engine <- function(rebuild = TRUE, verbose = FALSE) {
  original <- readLines(CPP_FILE, warn = FALSE)
  tmp_cpp <- file.path(FIXEDQ_ROOT, "bel_mc_fixedq_augmented.cpp")
  writeLines(c(original, FIXEDQ_CPP_ADDON), tmp_cpp)

  ok <- tryCatch({
    Rcpp::sourceCpp(tmp_cpp, rebuild = rebuild, verbose = verbose)
    TRUE
  }, error = function(e) {
    message("OpenMP build failed for the fixed-Q augmented engine. Retrying serially. Compiler message: ",
            conditionMessage(e))
    FALSE
  })

  if (!ok) {
    serial <- c(original[!grepl("Rcpp::plugins\\(openmp\\)", original)], FIXEDQ_CPP_ADDON)
    tmp_serial <- file.path(FIXEDQ_ROOT, "bel_mc_fixedq_augmented_serial.cpp")
    writeLines(serial, tmp_serial)
    Rcpp::sourceCpp(tmp_serial, rebuild = TRUE, verbose = verbose)
  }

  needed <- c("fixedq_eval_cpp", "fixedq_reference_cpp", "run_fixedq_outer_cpp", "abel_profile_cpp")
  miss <- needed[!vapply(needed, exists, logical(1), mode = "function")]
  if (length(miss)) stop("Fixed-Q engine did not export: ", paste(miss, collapse = ", "))
  invisible(TRUE)
}

# Validation of the new wrappers against the already validated ABEL profile.
validate_fixedq_engine <- function(n_tests = 200L, seed = 31082026,
                                   tol_W = 5e-12, tol_b = 5e-12) {
  if (!exists("fixedq_eval_cpp", mode = "function")) compile_fixedq_engine()
  set.seed(seed)
  errW <- numeric(n_tests)
  errb <- numeric(n_tests)
  fb <- logical(n_tests)

  for (i in seq_len(n_tests)) {
    q1 <- sample(4:16, 1L)
    q2 <- sample(4:20, 1L)
    r <- exp(runif(1, log(0.5), log(2)))
    u1 <- rnorm(q1)
    u2 <- rnorm(q2) / r

    bhR <- bhat_from_blocks_R(u1, u2)
    ev <- fixedq_eval_cpp(u1, u2)
    old <- abel_profile_cpp(u1, u2, 0, bhR / 2)

    errb[i] <- abs(ev$bhat - bhR)
    errW[i] <- abs(ev$ABEL_W - old$ABEL_W)
    fb[i] <- isTRUE(ev$ABEL_fallback)
  }

  ans <- data.frame(abs_b_error = errb, abs_W_error = errW, fallback = fb)
  write.csv(ans, file.path(FIXEDQ_ROOT, "VALIDATION_FIXEDQ_ENGINE.csv"), row.names = FALSE)

  if (max(errb) > tol_b || max(errW) > tol_W) {
    stop("Fixed-Q wrapper validation tolerance exceeded.")
  }
  invisible(ans)
}

# Exact ordinary-BEL profile-failure probability at r=1.
exact_bel_failure_equal_scale <- function(Q1, Q2) {
  2 / choose(Q1 + Q2, Q1)
}

# Outer time-series designs.
new_fixedq_row <- function(id, group, Q1, Q2, M1, M2 = M1,
                           model1 = "ar1", p11 = 0.8, p12 = 0,
                           model2 = "ar1", p21 = 0.8, p22 = 0,
                           innov1 = "normal", innov2 = innov1,
                           lrv_sd1 = 1, lrv_sd2 = 1,
                           L_rule = "loglog") {
  data.frame(
    scenario_id = id,
    group = group,
    Q1 = as.integer(Q1), Q2 = as.integer(Q2),
    M1 = as.integer(M1), M2 = as.integer(M2),
    N1 = as.integer(Q1 * M1), N2 = as.integer(Q2 * M2),
    model1 = model1, p11 = p11, p12 = p12,
    model2 = model2, p21 = p21, p22 = p22,
    innov1 = innov1, innov2 = innov2,
    lrv_sd1 = lrv_sd1, lrv_sd2 = lrv_sd2,
    L_rule = L_rule,
    stringsAsFactors = FALSE
  )
}

build_fixedq_paper_design <- function() {
  rows <- list()
  k <- 0L

  # A. Exact reconstruction of the existing 30-cell small-Q stress geometry.
  #    This lets us compare the old chi-square coverage with the new reference
  #    without changing the DGP or VC+ABEL statistic.
  M_values <- c(12L, 18L, 24L, 36L, 54L)
  size_specs <- list(
    list(label = "balanced", N1 = 216L, N2 = 216L),
    list(label = "imbalance_1to2", N1 = 216L, N2 = 432L)
  )
  dist_specs <- list(
    list(label = "normal_normal", i1 = "normal", i2 = "normal"),
    list(label = "chisq1_chisq1", i1 = "chisq1", i2 = "chisq1"),
    list(label = "normal_chisq1", i1 = "normal", i2 = "chisq1")
  )

  for (ss in size_specs) {
    for (M in M_values) {
      Q1 <- ss$N1 %/% M
      Q2 <- ss$N2 %/% M
      for (dd in dist_specs) {
        k <- k + 1L
        rows[[k]] <- new_fixedq_row(
          id = sprintf("bridge_%s_Q%d_%d_M%d_%s", ss$label, Q1, Q2, M, dd$label),
          group = "smallQ_bridge",
          Q1 = Q1, Q2 = Q2, M1 = M,
          p11 = 0.8, p21 = 0.8,
          innov1 = dd$i1, innov2 = dd$i2
        )
        rows[[k]]$size_pattern <- ss$label
        rows[[k]]$dist_pattern <- dd$label
      }
    }
  }

  # B. Direct fixed-Q convergence check. Q is held fixed while M increases.
  #    Gaussian and skewed innovations show that the Gaussian reference is a
  #    block-limit result rather than a Gaussian-data assumption.
  conv_specs <- list(
    list(Q1 = 4L, Q2 = 4L),
    list(Q1 = 6L, Q2 = 6L)
  )
  conv_M <- c(12L, 24L, 48L, 96L)
  conv_dist <- list(
    list(label = "normal_normal", i1 = "normal", i2 = "normal"),
    list(label = "chisq1_chisq1", i1 = "chisq1", i2 = "chisq1")
  )

  for (qq in conv_specs) {
    for (M in conv_M) {
      for (dd in conv_dist) {
        k <- k + 1L
        rows[[k]] <- new_fixedq_row(
          id = sprintf("conv_Q%d_%d_M%d_%s", qq$Q1, qq$Q2, M, dd$label),
          group = "fixedQ_convergence",
          Q1 = qq$Q1, Q2 = qq$Q2, M1 = M,
          p11 = 0.8, p21 = 0.8,
          innov1 = dd$i1, innov2 = dd$i2
        )
        rows[[k]]$size_pattern <- "fixedQ"
        rows[[k]]$dist_pattern <- dd$label
      }
    }
  }

  # Harmonize columns introduced after row construction.
  all_names <- unique(unlist(lapply(rows, names)))
  rows <- lapply(rows, function(z) {
    miss <- setdiff(all_names, names(z))
    for (nm in miss) z[[nm]] <- NA
    z[all_names]
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out$scenario_no <- seq_len(nrow(out))
  out
}

run_fixedq_outer_design <- function(
    design = build_fixedq_paper_design(),
    B = 10000L,
    threads = FIXEDQ_THREADS_DEFAULT,
    out_dir = file.path(FIXEDQ_ROOT, "outer_B10000"),
    base_seed = 2026083111,
    overwrite = FALSE,
    allow_small_B = FALSE) {

  if (!allow_small_B && B < 10000L) {
    stop("Publication outer cells require at least 10,000 replications.")
  }
  if (!exists("run_fixedq_outer_cpp", mode = "function")) compile_fixedq_engine()
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  write.csv(design, file.path(out_dir, "DESIGN_FIXEDQ.csv"), row.names = FALSE)

  for (i in seq_len(nrow(design))) {
    s <- design[i, , drop = FALSE]
    f <- file.path(out_dir, paste0(sprintf("%03d_", s$scenario_no), s$scenario_id, ".rds"))
    if (file.exists(f) && !overwrite) {
      message(sprintf("[%d/%d] skip %s", i, nrow(design), s$scenario_id))
      next
    }

    Lvc <- choose_L_vc(s$N1, s$N2, s$M1, s$M2, s$L_rule)
    sc_seed <- as.double(base_seed + 1000003 * s$scenario_no)
    message(sprintf("[%d/%d] run %s; B=%d; L=%d", i, nrow(design), s$scenario_id, B, Lvc))
    tm <- proc.time()[3]

    ans <- run_fixedq_outer_cpp(
      B = as.integer(B), seed = sc_seed, n_threads = as.integer(threads),
      model1 = MODEL_CODE[[s$model1]], p11 = s$p11, p12 = s$p12,
      innov1 = INNOV_CODE[[s$innov1]], lrv_sd1 = s$lrv_sd1,
      model2 = MODEL_CODE[[s$model2]], p21 = s$p21, p22 = s$p22,
      innov2 = INNOV_CODE[[s$innov2]], lrv_sd2 = s$lrv_sd2,
      N1 = s$N1, N2 = s$N2, M1 = s$M1, M2 = s$M2,
      L_vc = Lvc, delta_true = 0, delta0 = 0, burn = 500L
    )

    meta <- s
    meta$B <- B
    meta$threads <- threads
    meta$seed <- sc_seed
    meta$L_vc <- Lvc
    saveRDS(list(meta = meta, truth = ans$truth, stats = ans$stats), f, compress = FALSE)
    message(sprintf("  done %.1f s", proc.time()[3] - tm))
    gc(FALSE)
  }
  invisible(out_dir)
}

fixedq_outer_files <- function(out_dir) {
  list.files(out_dir, pattern = "^[0-9]{3}_.*[.]rds$", full.names = TRUE)
}

# Build an r-grid from the ACTUAL outer simulations.
# For min(Q1,Q2)>plugin_qmax we only retain the oracle values and r=1 because
# the main purpose there is to show approach toward chi-square, not to spend
# reference Monte Carlo on a dense plug-in grid.
build_fixedq_r_grid <- function(out_dir,
                                plugin_qmax = 12L,
                                min_grid_points = 9L,
                                max_log_step = 0.12,
                                pad_log = 0.06) {
  ff <- fixedq_outer_files(out_dir)
  if (!length(ff)) stop("No outer RDS files found in ", out_dir)

  objs <- lapply(ff, readRDS)
  keys <- unique(vapply(objs, function(a)
    paste(a$meta$Q1, a$meta$Q2, sep = "_"), character(1)))

  rows <- list(); k <- 0L
  for (key in keys) {
    oo <- objs[vapply(objs, function(a)
      paste(a$meta$Q1, a$meta$Q2, sep = "_") == key, logical(1))]
    q1 <- oo[[1]]$meta$Q1
    q2 <- oo[[1]]$meta$Q2

    rtruth <- unique(unlist(lapply(oo, function(a)
      c(a$truth$r_true_asym, a$truth$r_true_block))))
    rtruth <- rtruth[is.finite(rtruth) & rtruth > 0]

    if (min(q1, q2) <= plugin_qmax) {
      rh <- unlist(lapply(oo, function(a)
        c(a$stats[, "rhat_asym"], a$stats[, "rhat_block"])))
      rh <- rh[is.finite(rh) & rh > 0]
      allr <- c(rtruth, rh, 1)
      lr <- log(allr)
      lo <- min(lr) - pad_log
      hi <- max(lr) + pad_log

      # For balanced block counts the reference law is symmetric under r <-> 1/r.
      if (q1 == q2) {
        aa <- max(abs(c(lo, hi)))
        lo <- -aa; hi <- aa
      }

      n_by_step <- ceiling((hi - lo) / max_log_step) + 1L
      ng <- max(as.integer(min_grid_points), as.integer(n_by_step))
      rg <- exp(seq(lo, hi, length.out = ng))
      rg <- sort(unique(c(rg, rtruth, 1)))
    } else {
      rg <- sort(unique(c(rtruth, 1)))
    }

    for (r in rg) {
      k <- k + 1L
      rows[[k]] <- data.frame(Q1 = q1, Q2 = q2, r = r, stringsAsFactors = FALSE)
    }
  }

  out <- do.call(rbind, rows)
  out <- out[order(out$Q1, out$Q2, out$r), ]
  rownames(out) <- NULL
  write.csv(out, file.path(FIXEDQ_ROOT, "REFERENCE_R_GRID.csv"), row.names = FALSE)
  out
}

# Gaussian fixed-Q reference simulation.
# Same seed is deliberately used across r values within a given Q pair, giving
# common Gaussian random numbers and a smoother critical-value curve.
run_fixedq_reference_grid <- function(
    grid,
    B_ref = 200000L,
    threads = FIXEDQ_THREADS_DEFAULT,
    base_seed = 2026083117,
    out_file = file.path(FIXEDQ_ROOT, "REFERENCE_CRITICAL_VALUES.csv"),
    allow_small_B = FALSE) {

  if (!allow_small_B && B_ref < 100000L) {
    stop("Reference simulation should use at least 100,000 draws per grid point.")
  }
  if (!exists("fixedq_reference_cpp", mode = "function")) compile_fixedq_engine()

  rows <- vector("list", nrow(grid))
  qkeys <- unique(paste(grid$Q1, grid$Q2, sep = "_"))
  key_seed <- setNames(base_seed + 1000003 * seq_along(qkeys), qkeys)
  chi95 <- qchisq(0.95, 1)

  for (i in seq_len(nrow(grid))) {
    q1 <- as.integer(grid$Q1[i])
    q2 <- as.integer(grid$Q2[i])
    r <- as.numeric(grid$r[i])
    key <- paste(q1, q2, sep = "_")
    seed <- as.double(key_seed[[key]])

    message(sprintf("Reference [%d/%d]: Q=(%d,%d), r=%.6g, B=%d",
                    i, nrow(grid), q1, q2, r, B_ref))
    tm <- proc.time()[3]
    S <- fixedq_reference_cpp(
      B = as.integer(B_ref), seed = seed,
      n_threads = as.integer(threads), Q1 = q1, Q2 = q2, r_scale = r
    )

    wa <- S[, "W_ABEl_feasible"]
    ok <- is.finite(wa)
    if (!any(ok)) stop("No finite ABEL reference values for Q=", q1, ",", q2, " r=", r)
    qa <- quantile(wa[ok], probs = c(.90, .95, .99), type = 8, names = FALSE)

    bel_fail <- mean(S[, "BEL_fail"] > 0.5, na.rm = TRUE)
    abel_fail <- mean(S[, "ABEL_fail"] > 0.5, na.rm = TRUE)
    exact_fail <- if (abs(r - 1) < 1e-12) exact_bel_failure_equal_scale(q1, q2) else NA_real_

    rows[[i]] <- data.frame(
      Q1 = q1, Q2 = q2, r = r, B_ref = B_ref, seed = seed,
      crit90 = qa[1], crit95 = qa[2], crit99 = qa[3],
      chi95 = chi95,
      fixedQ_chi_coverage = mean(wa[ok] <= chi95),
      BEL_failure_reference = bel_fail,
      BEL_failure_exact_r1 = exact_fail,
      BEL_failure_z = if (is.finite(exact_fail))
        (bel_fail - exact_fail) / sqrt(exact_fail * (1 - exact_fail) / B_ref) else NA_real_,
      ABEL_failure_reference = abel_fail,
      ABEL_fallback_reference = mean(S[, "ABEL_fallback"] > 0.5, na.rm = TRUE),
      bhat_mean_reference = mean(S[, "bhat"], na.rm = TRUE),
      bhat_sd_reference = sd(S[, "bhat"], na.rm = TRUE),
      cdf_mcse_95 = sqrt(.95 * .05 / B_ref),
      seconds = proc.time()[3] - tm,
      stringsAsFactors = FALSE
    )
    gc(FALSE)
  }

  out <- do.call(rbind, rows)
  write.csv(out, out_file, row.names = FALSE)
  out
}

# Interpolation on log r.
# No extrapolation is allowed.  A grid miss is reported and, by default,
# stops the publication summary rather than silently clamping a critical value.
fixedq_interp <- function(ref, Q1, Q2, r, column = "crit95") {
  z <- ref[ref$Q1 == Q1 & ref$Q2 == Q2, , drop = FALSE]
  z <- z[order(z$r), , drop = FALSE]
  if (!nrow(z)) return(rep(NA_real_, length(r)))
  if (nrow(z) == 1L) {
    # A one-point grid is used only for large-Q oracle comparisons.
    out <- rep(NA_real_, length(r))
    ii <- is.finite(r) & abs(log(r) - log(z$r[1])) < 1e-10
    out[ii] <- z[[column]][1]
    return(out)
  }
  approx(x = log(z$r), y = z[[column]], xout = log(r), rule = 1, ties = "ordered")$y
}

# Coverage summary: SAME VC+ABEL statistic, different reference laws.
summarize_fixedq_calibration <- function(
    outer_dir,
    ref_file = file.path(FIXEDQ_ROOT, "REFERENCE_CRITICAL_VALUES.csv"),
    nominal = 0.95,
    stop_on_grid_miss = TRUE) {

  if (abs(nominal - .95) > 1e-12) stop("This summary is currently written for nominal 0.95.")
  if (!file.exists(ref_file)) stop("Reference file not found: ", ref_file)
  ref <- read.csv(ref_file)
  ff <- fixedq_outer_files(outer_dir)
  if (!length(ff)) stop("No outer RDS files in ", outer_dir)

  rows <- vector("list", length(ff))
  chi <- qchisq(.95, 1)

  for (ii in seq_along(ff)) {
    a <- readRDS(ff[ii])
    m <- a$meta
    S <- a$stats
    W <- S[, "VCABEL_feasible"]
    q1 <- m$Q1; q2 <- m$Q2

    rta <- as.numeric(a$truth$r_true_asym)
    rtb <- as.numeric(a$truth$r_true_block)
    rha <- S[, "rhat_asym"]
    rhb <- S[, "rhat_block"]

    c_ora <- fixedq_interp(ref, q1, q2, rta, "crit95")
    c_orb <- fixedq_interp(ref, q1, q2, rtb, "crit95")
    c_ha <- fixedq_interp(ref, q1, q2, rha, "crit95")
    c_hb <- fixedq_interp(ref, q1, q2, rhb, "crit95")

    miss_ha <- mean(!is.finite(c_ha))
    miss_hb <- mean(!is.finite(c_hb))
    if (stop_on_grid_miss && (miss_ha > 0 || miss_hb > 0) && min(q1, q2) <= 12) {
      stop(sprintf("Reference grid miss in %s: asym %.4f, block %.4f. Rebuild a wider r-grid.",
                   m$scenario_id, miss_ha, miss_hb))
    }

    covfun_scalar <- function(cut) {
      if (!length(cut) || !is.finite(cut[1])) return(NA_real_)
      mean(is.finite(W) & W <= cut[1])
    }
    covfun_vector <- function(cut) {
      if (!any(is.finite(cut))) return(NA_real_)
      if (any(!is.finite(cut))) return(NA_real_)
      mean(is.finite(W) & W <= cut)
    }

    # Predicted chi-square coverage under the fixed-Q Gaussian law at oracle r.
    pred <- fixedq_interp(ref, q1, q2, rtb, "fixedQ_chi_coverage")

    rows[[ii]] <- data.frame(
      scenario_id = m$scenario_id,
      group = m$group,
      size_pattern = if ("size_pattern" %in% names(m)) m$size_pattern else NA_character_,
      dist_pattern = if ("dist_pattern" %in% names(m)) m$dist_pattern else NA_character_,
      N1 = m$N1, N2 = m$N2, M1 = m$M1, M2 = m$M2,
      Q1 = q1, Q2 = q2,
      innov1 = m$innov1, innov2 = m$innov2,
      phi1 = m$p11, phi2 = m$p21,
      B = m$B,
      r_true_asym = rta,
      r_true_block = rtb,
      rhat_asym_mean = mean(rha, na.rm = TRUE),
      rhat_asym_sd = sd(rha, na.rm = TRUE),
      rhat_block_mean = mean(rhb, na.rm = TRUE),
      rhat_block_sd = sd(rhb, na.rm = TRUE),
      rhat_block_q01 = unname(quantile(rhb, .01, na.rm = TRUE, type = 8)),
      rhat_block_q99 = unname(quantile(rhb, .99, na.rm = TRUE, type = 8)),
      crit_chisq = chi,
      crit_fixedQ_oracle_asym = c_ora,
      crit_fixedQ_oracle_block = c_orb,
      coverage_chisq = covfun_scalar(chi),
      coverage_fixedQ_oracle_asym = covfun_scalar(c_ora),
      coverage_fixedQ_oracle_block = covfun_scalar(c_orb),
      coverage_fixedQ_rhat_asym = covfun_vector(c_ha),
      coverage_fixedQ_rhat_block = covfun_vector(c_hb),
      fixedQ_predicted_chisq_coverage = pred,
      BEL_failure = mean(S[, "bel_fail"] > .5, na.rm = TRUE),
      ABEL_failure = mean(S[, "abel_feasible_fail"] > .5, na.rm = TRUE),
      ABEL_fallback = mean(S[, "abel_feasible_solver_fallback"] > .5, na.rm = TRUE),
      JK_fallback = mean(S[, "jk_fallback"] > .5, na.rm = TRUE),
      VC_safeguard = mean(S[, "vc_safeguard"] > .5, na.rm = TRUE),
      alpha_hat_lt_025 = mean(S[, "alpha_hat_lt_025"] > .5, na.rm = TRUE),
      alpha_hat_mean = mean(S[, "alpha_hat"], na.rm = TRUE),
      nuhat_mean = mean(S[, "nuhat_JK"], na.rm = TRUE),
      rhat_asym_grid_miss = miss_ha,
      rhat_block_grid_miss = miss_hb,
      mcse_at_95 = sqrt(.95 * .05 / m$B),
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, rows)
  out <- out[order(out$group, out$Q1, out$Q2, out$M1, out$dist_pattern), ]
  write.csv(out, file.path(FIXEDQ_ROOT, "FIXEDQ_CALIBRATION_DETAILED.csv"), row.names = FALSE)

  # Bridge averages across the three innovation pairs, directly comparable with
  # the small-Q table reported in the paper.
  bridge <- out[out$group == "smallQ_bridge", , drop = FALSE]
  if (nrow(bridge)) {
    key <- interaction(bridge$size_pattern, bridge$Q1, bridge$Q2, drop = TRUE)
    spl <- split(bridge, key)
    mean_or_na <- function(x) if (all(!is.finite(x))) NA_real_ else mean(x[is.finite(x)])
    agg <- do.call(rbind, lapply(spl, function(z) data.frame(
      size_pattern = z$size_pattern[1], Q1 = z$Q1[1], Q2 = z$Q2[1],
      M = z$M1[1],
      mean_BEL_failure = mean_or_na(z$BEL_failure),
      mean_chisq_coverage = mean_or_na(z$coverage_chisq),
      mean_fixedQ_oracle_asym = mean_or_na(z$coverage_fixedQ_oracle_asym),
      mean_fixedQ_oracle_block = mean_or_na(z$coverage_fixedQ_oracle_block),
      mean_fixedQ_rhat_asym = mean_or_na(z$coverage_fixedQ_rhat_asym),
      mean_fixedQ_rhat_block = mean_or_na(z$coverage_fixedQ_rhat_block),
      mean_predicted_chisq_coverage = mean_or_na(z$fixedQ_predicted_chisq_coverage),
      stringsAsFactors = FALSE
    )))
    rownames(agg) <- NULL
    agg <- agg[order(agg$size_pattern, -agg$Q1), ]
    write.csv(agg, file.path(FIXEDQ_ROOT, "FIXEDQ_BRIDGE_AVERAGES.csv"), row.names = FALSE)
  }

  # Direct fixed-Q convergence table.
  conv <- out[out$group == "fixedQ_convergence", , drop = FALSE]
  if (nrow(conv)) {
    conv <- conv[order(conv$Q1, conv$dist_pattern, conv$M1), ]
    write.csv(conv, file.path(FIXEDQ_ROOT, "FIXEDQ_CONVERGENCE.csv"), row.names = FALSE)
  }

  out
}

# Quick development test.  These small-B outputs are NOT for the manuscript.
run_fixedq_quick_test <- function(threads = FIXEDQ_THREADS_DEFAULT) {
  compile_fixedq_engine(rebuild = TRUE)
  validate_fixedq_engine(n_tests = 50L)

  d <- build_fixedq_paper_design()
  # Small but discriminating subset.
  d <- d[d$scenario_id %in% c(
    "bridge_balanced_Q4_4_M54_normal_normal",
    "bridge_balanced_Q4_4_M54_chisq1_chisq1",
    "bridge_imbalance_1to2_Q4_8_M54_normal_normal",
    "conv_Q4_4_M96_normal_normal"
  ), , drop = FALSE]
  d$scenario_no <- seq_len(nrow(d))

  od <- file.path(FIXEDQ_ROOT, "QUICK_TEST_OUTER")
  run_fixedq_outer_design(d, B = 500L, threads = threads, out_dir = od,
                          overwrite = TRUE, allow_small_B = TRUE)
  grid <- build_fixedq_r_grid(od, min_grid_points = 5L, max_log_step = .25)
  rf <- file.path(FIXEDQ_ROOT, "QUICK_TEST_REFERENCE.csv")
  run_fixedq_reference_grid(grid, B_ref = 5000L, threads = threads,
                            out_file = rf, allow_small_B = TRUE)
  ss <- summarize_fixedq_calibration(od, rf, stop_on_grid_miss = TRUE)
  invisible(ss)
}

# Full production wrapper.
run_fixedq_publication <- function(
    B_outer = 10000L,
    B_ref = 200000L,
    threads = FIXEDQ_THREADS_DEFAULT,
    overwrite_outer = FALSE) {

  compile_fixedq_engine(rebuild = TRUE)
  validate_fixedq_engine(n_tests = 200L)

  outer_dir <- file.path(FIXEDQ_ROOT, paste0("outer_B", B_outer))
  d <- build_fixedq_paper_design()
  run_fixedq_outer_design(
    d, B = B_outer, threads = threads, out_dir = outer_dir,
    overwrite = overwrite_outer, allow_small_B = FALSE
  )

  grid <- build_fixedq_r_grid(outer_dir)
  ref_file <- file.path(FIXEDQ_ROOT, paste0("REFERENCE_CRITICAL_VALUES_B", B_ref, ".csv"))
  ref <- run_fixedq_reference_grid(
    grid, B_ref = B_ref, threads = threads, out_file = ref_file,
    allow_small_B = FALSE
  )

  # Sanity check of the exact r=1 ordinary-BEL failure formula.
  zz <- ref[abs(ref$r - 1) < 1e-12 & is.finite(ref$BEL_failure_exact_r1), , drop = FALSE]
  if (nrow(zz)) {
    if (any(abs(zz$BEL_failure_z) > 5, na.rm = TRUE)) {
      warning("At least one r=1 BEL failure simulation is more than 5 MCSE from the exact formula.")
    }
  }

  ans <- summarize_fixedq_calibration(outer_dir, ref_file, stop_on_grid_miss = TRUE)


  invisible(list(outer_dir = outer_dir, grid = grid, reference = ref, summary = ans))
}

#
