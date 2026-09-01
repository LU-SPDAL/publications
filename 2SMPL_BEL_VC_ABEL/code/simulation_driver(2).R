# ============================================================================
# TWO-SAMPLE HIGH-PRECISION ADJUSTED BEL: COMPREHENSIVE MONTE CARLO STUDY
# ============================================================================
# R orchestration + Rcpp/C++17/OpenMP engine in bel_mc.cpp.
# Final feasible method:
#   nu_hat_JK * W_ABEl(delta0; alpha_hat),   alpha_hat = b_hat / 2.
#
# Production runs enforce B = 10,000 replications per scenario.
# Run validate_abel_solver() before launching production.
# ============================================================================

if (!requireNamespace("Rcpp", quietly = TRUE)) {
  stop("Package 'Rcpp' is required. Install it with install.packages('Rcpp').")
}

this_dir <- function() {
  of <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(of)) return(dirname(normalizePath(of)))
  getwd()
}

SIM_DIR <- this_dir()
CPP_FILE <- file.path(SIM_DIR, "bel_mc.cpp")
if (!file.exists(CPP_FILE)) stop("Cannot find bel_mc.cpp next to simulation_driver.R")

compile_abel_engine <- function(rebuild = FALSE, verbose = FALSE) {
  ok <- tryCatch({
    Rcpp::sourceCpp(CPP_FILE, rebuild = rebuild, verbose = verbose)
    TRUE
  }, error = function(e) {
    message("OpenMP build failed; retrying serial build. Compiler message: ", conditionMessage(e))
    FALSE
  })
  if (!ok) {
    txt <- readLines(CPP_FILE, warn = FALSE)
    txt <- txt[!grepl("Rcpp::plugins\\(openmp\\)", txt)]
    serial_cpp <- file.path(tempdir(), "bel_mc_serial.cpp")
    writeLines(txt, serial_cpp)
    Rcpp::sourceCpp(serial_cpp, rebuild = TRUE, verbose = verbose)
  }
  invisible(TRUE)
}

MODEL_CODE <- c(ar1 = 0L, ma2 = 1L)
INNOV_CODE <- c(normal = 0L, laplace = 1L, chisq1 = 2L, gamma2 = 3L,
                t8 = 4L, rademacher = 5L)

# ---------------------------- block/bandwidth helpers -------------------------
nearest_common_divisor <- function(N1, N2, target, min_blocks = 8L) {
  gcd <- function(a, b) if (b == 0L) a else Recall(b, a %% b)
  G <- gcd(as.integer(N1), as.integer(N2))
  cand <- seq_len(G)
  ds <- cand[G %% cand == 0L]
  ds <- ds[N1 / ds >= min_blocks & N2 / ds >= min_blocks]
  if (!length(ds)) stop("No usable common divisor")
  ds[which.min(abs(ds - target))]
}

cube_block <- function(N1, N2) {
  nearest_common_divisor(N1, N2, round(min(N1, N2)^(1/3)))
}

choose_L_vc <- function(N1, N2, M1, M2, rule = "loglog") {
  Nstar <- min(N1, N2)
  Mstar <- min(M1, M2)
  if (rule == "loglog") {
    L <- floor(log(Nstar) * log(log(Nstar)))
  } else if (rule == "log1") {
    L <- ceiling(log(Nstar))
  } else if (rule == "log2") {
    L <- ceiling(2 * log(Nstar))
  } else if (rule == "old4log") {
    L <- min(2L * Mstar, ceiling(4 * log(Nstar)))
  } else if (rule == "M23") {
    L <- max(1L, floor(0.75 * Mstar^(2/3)))
  } else stop("Unknown VC bandwidth rule: ", rule)
  half_min <- min(floor(N1/2), N1-floor(N1/2),
                  floor(N2/2), N2-floor(N2/2))
  max(1L, min(as.integer(L), as.integer(half_min - 2L)))
}

choose_L_hac <- function(N) max(1L, floor(4 * (N/100)^(2/9)))

new_scenario <- function(id, group, N1, N2, M1, M2 = M1,
                         model1 = "ar1", p11 = 0, p12 = 0,
                         model2 = "ar1", p21 = 0, p22 = 0,
                         innov1 = "normal", innov2 = innov1,
                         lrv_sd1 = 1, lrv_sd2 = 1,
                         effect_c = 0, delta0 = 0,
                         L_rule = "loglog",
                         theory_scope = if (M1 == M2) "main_common_M" else "robustness_unequal_M") {
  stopifnot(N1 %% M1 == 0, N2 %% M2 == 0)
  data.frame(
    scenario_id=id, group=group, N1=as.integer(N1), N2=as.integer(N2),
    M1=as.integer(M1), M2=as.integer(M2), model1=model1, p11=p11, p12=p12,
    model2=model2, p21=p21, p22=p22, innov1=innov1, innov2=innov2,
    lrv_sd1=lrv_sd1, lrv_sd2=lrv_sd2, effect_c=effect_c, delta0=delta0,
    L_rule=L_rule, theory_scope=theory_scope, stringsAsFactors=FALSE
  )
}

rbind_rows <- function(xs) {
  if (!length(xs)) return(data.frame())
  z <- do.call(rbind, xs); rownames(z) <- NULL; z
}

# ----------------------------- simulation designs -----------------------------
build_main_coverage_design <- function() {
  Ns <- c(216L, 512L, 1000L, 1728L)
  deps <- list(c(0,0), c(.6,.6), c(.8,.8), c(.2,.8), c(-.3,.7))
  inno <- list(c("normal","normal"), c("laplace","laplace"),
               c("chisq1","chisq1"), c("normal","chisq1"))
  z <- list(); k <- 0L
  for (N in Ns) for (d in deps) for (iv in inno) {
    M <- cube_block(N,N); k <- k+1L
    id <- sprintf("cov_N%d_phi%+.1f_%+.1f_%s_%s",N,d[1],d[2],iv[1],iv[2])
    z[[k]] <- new_scenario(id,"coverage_main",N,N,M,p11=d[1],p21=d[2],innov1=iv[1],innov2=iv[2])
  }
  rbind_rows(z)
}

build_imbalance_design <- function() {
  Npairs <- list(c(512L,1024L), c(512L,1536L), c(1024L,512L))
  deps <- list(c(.2,.8), c(.8,.2))
  inno <- list(c("normal","normal"), c("normal","chisq1"))
  scales <- list(c(1,1), c(1,2))
  z <- list(); k <- 0L
  for(nn in Npairs) for(d in deps) for(iv in inno) for(ss in scales) {
    M <- cube_block(nn[1],nn[2]); k <- k+1L
    id <- sprintf("imb_N%d_%d_phi%+.1f_%+.1f_%s_%s_sd%.0f_%.0f",
                  nn[1],nn[2],d[1],d[2],iv[1],iv[2],ss[1],ss[2])
    z[[k]] <- new_scenario(id,"imbalance",nn[1],nn[2],M,p11=d[1],p21=d[2],
                           innov1=iv[1],innov2=iv[2],lrv_sd1=ss[1],lrv_sd2=ss[2])
  }
  rbind_rows(z)
}

build_blocklength_design <- function() {
  N <- 1000L; Ms <- c(5L,10L,20L,40L)
  deps <- list(c(.6,.6),c(.2,.8),c(-.3,.7))
  inno <- list(c("normal","normal"),c("chisq1","chisq1"))
  z <- list(); k <- 0L
  for(M in Ms) for(d in deps) for(iv in inno) {
    k <- k+1L
    z[[k]] <- new_scenario(sprintf("M_N%d_M%d_phi%+.1f_%+.1f_%s",N,M,d[1],d[2],iv[1]),
                           "blocklength_common",N,N,M,p11=d[1],p21=d[2],innov1=iv[1],innov2=iv[2])
  }
  unequal <- list(c(5L,20L),c(10L,20L),c(20L,10L),c(10L,40L))
  for(mm in unequal) {
    k <- k+1L
    z[[k]] <- new_scenario(sprintf("Munequal_%d_%d",mm[1],mm[2]),
                           "blocklength_unequal_robustness",N,N,mm[1],mm[2],
                           p11=.2,p21=.8,innov1="normal",innov2="chisq1",
                           theory_scope="robustness_unequal_M")
  }
  rbind_rows(z)
}

build_model_robustness_design <- function() {
  N <- 1000L; M <- 10L
  specs <- list(
    list(m1="ar1",a11=.6,a12=0,m2="ar1",a21=.6,a22=0,label="ar1_ar1"),
    list(m1="ma2",a11=.6,a12=.2,m2="ma2",a21=.6,a22=.2,label="ma2_ma2"),
    list(m1="ar1",a11=.6,a12=0,m2="ma2",a21=.6,a22=.2,label="ar1_ma2"))
  z <- list(); k <- 0L
  for(sp in specs) for(iv in c("normal","laplace","chisq1")) {
    k <- k+1L
    z[[k]] <- new_scenario(sprintf("model_%s_%s",sp$label,iv),"model_robustness",N,N,M,
                           model1=sp$m1,p11=sp$a11,p12=sp$a12,
                           model2=sp$m2,p21=sp$a21,p22=sp$a22,innov1=iv,innov2=iv)
  }
  rbind_rows(z)
}

build_bandwidth_sensitivity_design <- function() {
  N <- 1000L; M <- 10L
  rules <- c("M23","log1","loglog","log2","old4log")
  rbind_rows(lapply(rules,function(rr)
    new_scenario(paste0("Lrule_",rr),"bandwidth_sensitivity",N,N,M,
                 p11=.8,p21=.8,innov1="chisq1",innov2="chisq1",L_rule=rr)))
}

build_power_design <- function() {
  effects <- c(0,.5,1,1.5,2,2.5,3)
  base <- list(
    list(id="P1_bal_normal",N1=1000L,N2=1000L,M=10L,p1=.6,p2=.6,i1="normal",i2="normal",s1=1,s2=1),
    list(id="P2_bal_skew",N1=1000L,N2=1000L,M=10L,p1=.6,p2=.6,i1="chisq1",i2="chisq1",s1=1,s2=1),
    list(id="P3_unbal_mixed",N1=512L,N2=1024L,M=8L,p1=.2,p2=.8,i1="normal",i2="chisq1",s1=1,s2=1),
    list(id="P4_hetero",N1=1000L,N2=1000L,M=10L,p1=-.3,p2=.7,i1="laplace",i2="gamma2",s1=1,s2=2))
  z <- list(); k <- 0L
  for(bb in base) for(cc in effects) {
    k <- k+1L
    row <- new_scenario(sprintf("%s_c%.1f",bb$id,cc),"power",bb$N1,bb$N2,bb$M,
                        p11=bb$p1,p21=bb$p2,innov1=bb$i1,innov2=bb$i2,
                        lrv_sd1=bb$s1,lrv_sd2=bb$s2,effect_c=cc)
    row$base_id <- bb$id; z[[k]] <- row
  }
  rbind_rows(z)
}

build_full_design <- function() {
  parts <- list(build_main_coverage_design(),build_imbalance_design(),build_blocklength_design(),
                build_model_robustness_design(),build_bandwidth_sensitivity_design(),build_power_design())
  parts <- lapply(parts,function(d){if(!"base_id"%in%names(d))d$base_id<-d$scenario_id;d})
  out <- do.call(rbind,parts); rownames(out)<-NULL; out$scenario_no<-seq_len(nrow(out));out
}

# Smaller first production wave; still 10,000 replications per cell.
build_core_design <- function() {
  parts <- list(build_main_coverage_design(),build_bandwidth_sensitivity_design(),build_model_robustness_design())
  parts <- lapply(parts,function(d){if(!"base_id"%in%names(d))d$base_id<-d$scenario_id;d})
  out<-do.call(rbind,parts);rownames(out)<-NULL;out$scenario_no<-seq_len(nrow(out));out
}

build_stress_design <- function() {
  N<-1000L;M<-10L
  out<-rbind_rows(list(
    new_scenario("stress_t8","stress_outside_moment_assumption",N,N,M,p11=.7,p21=.7,innov1="t8",innov2="t8",theory_scope="stress_heavy_tail"),
    new_scenario("stress_lattice","stress_outside_cramer",N,N,M,p11=.7,p21=.7,innov1="rademacher",innov2="rademacher",theory_scope="stress_lattice")))
  out$base_id<-out$scenario_id;out$scenario_no<-seq_len(nrow(out));out
}

# ----------------------------- exact slow R ABEL reference -------------------
slow_lambda_aug <- function(y,m,alpha,theta,tol=1e-12) {
  d <- c(y-m, -alpha*theta*(mean(y)-m))
  if (abs(sum(d)) < tol) return(0)
  if (!(min(d)<0 && max(d)>0)) stop("Augmented hull failure")
  lo <- max(-1/d[d>0]); hi <- min(-1/d[d<0])
  eps <- 1e-11*(1+max(abs(c(lo,hi)))); lo<-lo+eps;hi<-hi-eps
  f <- function(lam) sum(d/(1+lam*d))
  uniroot(f,c(lo,hi),tol=tol)$root
}

slow_abel_profile <- function(u1,u2,delta0,alpha,ngrid=301L) {
  # Independent, deliberately slow GLOBAL 1-D reference.  A single call to
  # optimize() is unsafe because the adjusted profile need not be unimodal
  # far away from the null.  We scan a grid, refine every grid-local minimum,
  # and return the smallest refined value.
  y1<-u1;y2<-u2+delta0;Q1<-length(y1);Q2<-length(y2);Q<-Q1+Q2
  objective <- function(m) {
    out <- tryCatch({
      l1<-slow_lambda_aug(y1,m,alpha,Q1/Q);l2<-slow_lambda_aug(y2,m,alpha,Q2/Q)
      d1<-c(y1-m,-alpha*(Q1/Q)*(mean(y1)-m))
      d2<-c(y2-m,-alpha*(Q2/Q)*(mean(y2)-m))
      2*(sum(log1p(l1*d1))+sum(log1p(l2*d2)))
    }, error=function(e) Inf)
    if(is.finite(out)) out else Inf
  }
  yy<-c(y1,y2); rg<-max(diff(range(yy)),sd(yy),1e-5)
  lo<-min(yy)-6*rg; hi<-max(yy)+6*rg
  grid<-seq(lo,hi,length.out=ngrid)
  fg<-vapply(grid,objective,numeric(1))
  if(!any(is.finite(fg))) stop("Slow ABEL reference found no finite grid value")
  cand<-which(is.finite(fg) & fg==min(fg,na.rm=TRUE))
  if(ngrid>=3L) {
    loc<-which(is.finite(fg[2:(ngrid-1L)]) &
               fg[2:(ngrid-1L)]<=fg[1:(ngrid-2L)] &
               fg[2:(ngrid-1L)]<=fg[3:ngrid])+1L
    cand<-unique(c(cand,loc))
  }
  bestW<-min(fg,na.rm=TRUE); bestm<-grid[which.min(fg)]
  for(k in cand) {
    kl<-max(1L,k-1L); kr<-min(ngrid,k+1L)
    if(kl==kr) next
    op<-tryCatch(optimize(objective,c(grid[kl],grid[kr]),tol=1e-11),error=function(e)NULL)
    if(!is.null(op) && is.finite(op$objective) && op$objective<bestW) {
      bestW<-op$objective;bestm<-op$minimum
    }
  }
  c(W=bestW,m=bestm)
}

validate_abel_solver <- function(n_tests=500L,seed=29082026,tol=2e-6,
                                 save_worst=TRUE) {
  if(!exists("abel_profile_cpp",mode="function"))compile_abel_engine()
  set.seed(seed);err<-numeric(n_tests);fallback<-logical(n_tests)
  details<-vector("list",n_tests)
  for(i in seq_len(n_tests)) {
    q1<-sample(12:50,1);q2<-sample(12:50,1)
    # Validate in the regime used by the Monte Carlo study: null/local
    # alternatives.  Both populations are centered, with delta0 aligning
    # their population means.  This avoids testing irrelevant fixed-distance
    # alternatives for which AEL may have competing far-tail minima.
    mu1<-runif(1,-.2,.2);mu2<-runif(1,-.2,.2)
    u1<-rt(q1,8)*sqrt(6/8)+mu1
    u2<-(rchisq(q2,3)-3)/sqrt(6)+mu2
    delta0<-mu1-mu2
    # Add a local alternative in half the cases.
    if(i%%2L==0L) u2<-u2+runif(1,-2,2)*sqrt(1/q1+1/q2)
    alpha<-runif(1,.25,3)
    cpp<-abel_profile_cpp(u1,u2,delta0,alpha); rr<-slow_abel_profile(u1,u2,delta0,alpha)
    if(!isTRUE(cpp$ABEL_ok)) stop("C++ ABEL solver failed validation case ",i)
    err[i]<-abs(cpp$ABEL_W-rr[["W"]]);fallback[i]<-isTRUE(cpp$ABEL_fallback)
    details[[i]]<-list(i=i,q1=q1,q2=q2,alpha=alpha,delta0=delta0,u1=u1,u2=u2,
                       cpp=cpp,ref=rr,error=err[i])
  }
  imax<-which.max(err)
  cat(sprintf("ABEL solver validation (V3 multistart): max |W_cpp-W_R| = %.3g; median = %.3g; global fallback %.2f%%\n",
              max(err),median(err),100*mean(fallback)))
  cat(sprintf("Worst case %d: W_cpp=%.12g, W_R=%.12g, error=%.3g, fallback=%s\n",
              imax,details[[imax]]$cpp$ABEL_W,details[[imax]]$ref[["W"]],err[imax],fallback[imax]))
  if(save_worst) saveRDS(details[[imax]],file.path(SIM_DIR,"ABEL_VALIDATION_WORST.rds"))
  if(max(err)>tol) stop("Validation tolerance exceeded; do NOT run production. Upload ABEL_VALIDATION_WORST.rds.")
  invisible(data.frame(abs_error=err,fallback=fallback))
}

# ----------------------------- execution --------------------------------------
truth_for_row <- function(s) scenario_truth_cpp(
  MODEL_CODE[[s$model1]],s$p11,s$p12,INNOV_CODE[[s$innov1]],s$lrv_sd1,
  MODEL_CODE[[s$model2]],s$p21,s$p22,INNOV_CODE[[s$innov2]],s$lrv_sd2,
  s$N1,s$N2,s$M1,s$M2)

run_one_scenario <- function(s,B=10000L,threads=max(1L,parallel::detectCores()-1L),
                             base_seed=20260829,burn=500L,allow_small_B=FALSE) {
  if(!allow_small_B && B<10000L)stop("Production cells require at least 10,000 replications.")
  tr<-truth_for_row(s);delta_true<-s$effect_c*sqrt(tr$V_N)
  Lvc<-choose_L_vc(s$N1,s$N2,s$M1,s$M2,s$L_rule)
  Lh1<-choose_L_hac(s$N1);Lh2<-choose_L_hac(s$N2)
  sc_seed<-as.double(base_seed+1000003*s$scenario_no)
  ans<-run_scenario_cpp(B=as.integer(B),seed=sc_seed,n_threads=as.integer(threads),
    model1=MODEL_CODE[[s$model1]],p11=s$p11,p12=s$p12,innov1=INNOV_CODE[[s$innov1]],lrv_sd1=s$lrv_sd1,
    model2=MODEL_CODE[[s$model2]],p21=s$p21,p22=s$p22,innov2=INNOV_CODE[[s$innov2]],lrv_sd2=s$lrv_sd2,
    N1=s$N1,N2=s$N2,M1=s$M1,M2=s$M2,L_vc=Lvc,L_hac1=Lh1,L_hac2=Lh2,
    delta_true=delta_true,delta0=s$delta0,burn=burn)
  meta<-s;meta$B<-B;meta$threads<-threads;meta$seed<-sc_seed;meta$delta_true<-delta_true
  meta$L_vc<-Lvc;meta$L_hac1<-Lh1;meta$L_hac2<-Lh2;meta$Q1<-s$N1/s$M1;meta$Q2<-s$N2/s$M2;meta$Q<-meta$Q1+meta$Q2
  list(meta=meta,truth=ans$truth,stats=ans$stats)
}

run_design <- function(design=build_full_design(),out_dir=file.path(SIM_DIR,"results_ABEl_raw"),
                       B=10000L,threads=max(1L,parallel::detectCores()-1L),
                       base_seed=20260829,overwrite=FALSE,allow_small_B=FALSE) {
  if(!allow_small_B && B<10000L)stop("Production design requires at least 10,000 replications per cell.")
  if(!exists("run_scenario_cpp",mode="function"))compile_abel_engine()
  if(!"scenario_no"%in%names(design))design$scenario_no<-seq_len(nrow(design))
  if(!"base_id"%in%names(design))design$base_id<-design$scenario_id
  dir.create(out_dir,showWarnings=FALSE,recursive=TRUE);write.csv(design,file.path(out_dir,"DESIGN.csv"),row.names=FALSE)
  for(i in seq_len(nrow(design))) {
    s<-design[i,,drop=FALSE];f<-file.path(out_dir,paste0(sprintf("%03d_",s$scenario_no),s$scenario_id,".rds"))
    if(file.exists(f)&&!overwrite){message(sprintf("[%d/%d] skip %s",i,nrow(design),s$scenario_id));next}
    message(sprintf("[%d/%d] run %s; B=%d",i,nrow(design),s$scenario_id,B));tm<-proc.time()[3]
    ans<-run_one_scenario(s,B,threads,base_seed,allow_small_B=allow_small_B);saveRDS(ans,f,compress=FALSE)
    message(sprintf("  done %.1f s",proc.time()[3]-tm));gc(FALSE)
  }
  invisible(out_dir)
}

run_validation_batch <- function(B=500L,threads=max(1L,parallel::detectCores()-1L)) {
  compile_abel_engine(); validate_abel_solver(50L)
  d<-build_core_design();d<-d[seq_len(min(8L,nrow(d))),,drop=FALSE];d$scenario_no<-seq_len(nrow(d))
  run_design(d,file.path(SIM_DIR,"validation_ABEl"),B=B,threads=threads,overwrite=TRUE,allow_small_B=TRUE)
  summarize_diagnostics(file.path(SIM_DIR,"validation_ABEl"))
}

# ----------------------------- summaries --------------------------------------
METHODS <- c("BEL","BC_oracle","VC_oracle","VCBC_oracle","ABEL_oracle","VCABEL_oracle",
             "BC_feasible","VC_feasible","VCBC_feasible","ABEL_feasible","VCABEL_feasible",
             "ABEL_fixed075","VCABEL_fixed075_oracleVC","Wald_HAC","Wald_oracle")

raw_files <- function(out_dir) list.files(out_dir,pattern="^[0-9]{3}_.*[.]rds$",full.names=TRUE)

leading_error_EN <- function(level,meta,truth) {
  if(meta$M1!=meta$M2)return(NA_real_)
  x<-qchisq(level,1);g1<-dchisq(x,1);th1<-truth$theta1;th2<-truth$theta2
  -x*g1*(truth$pi1*truth$delta1/(2*th2*meta$N1)*(2*th2-truth$L_M*x)+
         truth$pi2*truth$delta2/(2*th1*meta$N2)*(2*th1-truth$L_M*x))
}

summarize_coverage <- function(out_dir=file.path(SIM_DIR,"results_ABEl_raw"),levels=c(.90,.95,.99)) {
  rows<-list();k<-0L
  for(f in raw_files(out_dir)) {
    a<-readRDS(f);m<-a$meta;if(m$group=="power"||m$effect_c!=0)next;S<-a$stats
    for(method in METHODS)for(lev in levels) {
      z<-S[,method];ok<-is.finite(z);hit<-z<=qchisq(lev,1);p<-if(any(ok))mean(hit[ok])else NA_real_
      k<-k+1L;EN<-if(method%in%c("VCBC_oracle","VCABEL_oracle"))leading_error_EN(lev,m,a$truth)else NA_real_
      rows[[k]]<-cbind(m,method=method,nominal=lev,coverage=p,
        coverage_fail_as_miss=mean(ifelse(ok,hit,FALSE)),failure_rate=mean(!ok),
        mcse=if(any(ok))sqrt(p*(1-p)/sum(ok))else NA_real_,EN_theory=EN,
        scaled_emp=(m$N1+m$N2)*(p-lev),scaled_theory=(m$N1+m$N2)*EN,stringsAsFactors=FALSE)
    }
  }
  out<-rbind_rows(rows);write.csv(out,file.path(out_dir,"SUMMARY_COVERAGE.csv"),row.names=FALSE);out
}

summarize_diagnostics <- function(out_dir=file.path(SIM_DIR,"results_ABEl_raw")) {
  rows<-list();k<-0L
  for(f in raw_files(out_dir)) {
    a<-readRDS(f);S<-a$stats;m<-a$meta;k<-k+1L
    rows[[k]]<-cbind(m,
      bel_fail=mean(S[,"bel_fail"]>.5,na.rm=TRUE),abel_oracle_fail=mean(S[,"abel_oracle_fail"]>.5,na.rm=TRUE),
      abel_feasible_fail=mean(S[,"abel_feasible_fail"]>.5,na.rm=TRUE),
      abel_oracle_solver_fallback=mean(S[,"abel_oracle_solver_fallback"]>.5,na.rm=TRUE),
      abel_feasible_solver_fallback=mean(S[,"abel_feasible_solver_fallback"]>.5,na.rm=TRUE),
      jk_fallback=mean(S[,"jk_fallback"]>.5,na.rm=TRUE),vc_safeguard=mean(S[,"vc_safeguard"]>.5,na.rm=TRUE),
      alpha_hat_lt_025=mean(S[,"alpha_hat_lt_025"]>.5,na.rm=TRUE),
      b_true=a$truth$b_true,bhat_mean=mean(S[,"bhat"],na.rm=TRUE),bhat_sd=sd(S[,"bhat"],na.rm=TRUE),
      alpha_true=a$truth$alpha_true,alpha_hat_mean=mean(S[,"alpha_hat"],na.rm=TRUE),
      nu_true=a$truth$nu_true,nuhat_mean=mean(S[,"nuhat_JK"],na.rm=TRUE),nuhat_sd=sd(S[,"nuhat_JK"],na.rm=TRUE),
      diff_oracle_mean=mean(S[,"abel_minus_bc_oracle"],na.rm=TRUE),
      diff_oracle_abs_q99=unname(quantile(abs(S[,"abel_minus_bc_oracle"]),.99,na.rm=TRUE,type=8)),
      diff_feasible_mean=mean(S[,"abel_minus_bc_feasible"],na.rm=TRUE),
      diff_feasible_abs_q99=unname(quantile(abs(S[,"abel_minus_bc_feasible"]),.99,na.rm=TRUE,type=8)),
      stringsAsFactors=FALSE)
  }
  out<-rbind_rows(rows);write.csv(out,file.path(out_dir,"SUMMARY_DIAGNOSTICS.csv"),row.names=FALSE);out
}

summarize_power <- function(out_dir=file.path(SIM_DIR,"results_ABEl_raw"),alpha=.05) {
  objs<-lapply(raw_files(out_dir),readRDS);objs<-Filter(function(a)a$meta$group=="power",objs)
  if(!length(objs))return(data.frame());bases<-unique(vapply(objs,function(a)a$meta$base_id,character(1)))
  rows<-list();k<-0L
  for(base in bases) {
    oo<-Filter(function(a)a$meta$base_id==base,objs);nulls<-Filter(function(a)a$meta$effect_c==0,oo);if(!length(nulls))next
    null<-nulls[[1]]
    for(method in METHODS) {
      z0<-null$stats[,method];z0<-z0[is.finite(z0)];if(!length(z0))next
      crit_emp<-unname(quantile(z0,1-alpha,type=8));crit_chi<-qchisq(1-alpha,1)
      for(a in oo) {
        z<-a$stats[,method];ok<-is.finite(z);k<-k+1L
        rows[[k]]<-cbind(a$meta,method=method,critical_empirical=crit_emp,
          rejection_size_adjusted=if(any(ok))mean(z[ok]>crit_emp)else NA_real_,
          rejection_chisq=if(any(ok))mean(z[ok]>crit_chi)else NA_real_,failure_rate=mean(!ok),stringsAsFactors=FALSE)
      }
    }
  }
  out<-rbind_rows(rows);write.csv(out,file.path(out_dir,"SUMMARY_POWER.csv"),row.names=FALSE);out
}

summarize_equivalence <- function(out_dir=file.path(SIM_DIR,"results_ABEl_raw")) {
  rows<-list();k<-0L
  for(f in raw_files(out_dir)) {
    a<-readRDS(f);S<-a$stats;m<-a$meta;k<-k+1L
    dd<-S[,"abel_minus_bc_oracle"]
    rows[[k]]<-cbind(m,Q=m$Q,M=min(m$M1,m$M2),
      mean_diff=mean(dd,na.rm=TRUE),sd_diff=sd(dd,na.rm=TRUE),
      mean_abs=mean(abs(dd),na.rm=TRUE),q95_abs=unname(quantile(abs(dd),.95,na.rm=TRUE,type=8)),
      q99_abs=unname(quantile(abs(dd),.99,na.rm=TRUE,type=8)),
      stringsAsFactors=FALSE)
  }
  out<-rbind_rows(rows);write.csv(out,file.path(out_dir,"SUMMARY_ABEL_BC_EQUIVALENCE.csv"),row.names=FALSE);out
}

# ----------------------------- recommended commands ---------------------------
# source("simulation_driver.R")
# compile_abel_engine()
# validate_abel_solver(100)
# run_validation_batch(B=500)
#
# Production wave 1 (94 cells at present):
# core <- build_core_design(); table(core$group); nrow(core)
# run_design(core, B=10000L, threads=max(1L,parallel::detectCores()-1L))
# summarize_coverage(); summarize_diagnostics(); summarize_equivalence()
#
# Full study (adds imbalance, block-length and power modules):
# full <- build_full_design(); table(full$group); nrow(full)
# run_design(full, B=10000L, threads=max(1L,parallel::detectCores()-1L))
# summarize_coverage(); summarize_diagnostics(); summarize_equivalence(); summarize_power()
