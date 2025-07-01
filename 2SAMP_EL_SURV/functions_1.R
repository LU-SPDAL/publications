# Libraries
# ---------------------------------------

library(pseudo)
library(survival)
library(EL)
library(emplik)
library(survRM2)
library(kit)
library(foreach)
library(pracma)
library(nleqslv)
library(surveillance)

# Functions
# ---------------------------------------
data_gen <- function(n = 100, columns = 1, x = 1, c_vect = c(0.11, 0.25, 0.429), input_seed) {
    n_c <- length(c_vect)
    array_1 <- array(NA, dim = c(columns, n, 2, n_c))
    set.seed(input_seed)
    for (j in 1:n_c) {
        for (i in 1:columns) {
            lifetimes <- rexp(n, rate = x)
            censtimes <- rexp(n, rate = c_vect[j])
            array_1[i, , 1, j] <- pmin(lifetimes, censtimes)
            array_1[i, , 2, j] <- as.numeric(lifetimes < censtimes)
        }
    }
    return(array_1)
}

uncens_times <- function(dati_z) {
    dati_z <- dati_z[order(dati_z[, 1], decreasing = FALSE), ]
    dati_z <- subset(dati_z, as.logical(dati_z[, 2]))
    return(dati_z)
}

cens_times <- function(dati_z) {
    dati_z <- dati_z[order(dati_z[, 1], decreasing = FALSE), ]
    dati_z <- subset(dati_z, !as.logical(dati_z[, 2]))
    return(dati_z)
}

order_times <- function(dati_z, column = 1) {
    dati_z <- dati_z[order(dati_z[, column], decreasing = FALSE), ]
    return(dati_z)
}

more_than <- function(x, n) {
    if ((typeof(x) == "double") & !is.na(x)) {
        x <- x > 0
        return(x)
    } else {
        return(FALSE)
    }
}
# McKeague & Zhao method code
# ---------------------------------------

r_ij <- function(dati) {
    dati_T <- uncens_times(dati)
    n <- nrow(dati_T)
    r_vect <- array(dim = n)
    for (i in 1:n) {
        r_vect[i] <- sum(dati_T[i, 1] <= dati[, 1])
    }
    return(r_vect)
}

d_ij <- function(dati) {
    dati_T <- uncens_times(dati)
    n <- nrow(dati_T)
    d_vect <- array(dim = n)
    for (i in 1:n) {
        d_vect[i] <- sum((dati_T[i, 1] == dati[, 1]) & (dati[, 2] == 1))
    }
    return(d_vect)
}

K_j <- function(dati, t) {
    dati_T <- uncens_times(dati)
    return(sum(dati_T[, 1] <= t))
}

KM_estimator <- function(dati, t) {
    n <- nrow(dati)
    dati <- dati[order(dati[, 1], decreasing = FALSE), ]
    est <- (n - (1:n)) / (n - (1:n) + 1)
    est <- est[(dati[, 1] <= t) & (dati[, 2] == 1)]
    return(prod(est))
}

KM_estimator <- Vectorize(KM_estimator, vectorize.args = "t")

D_j <- function(dati, t) {
    n <- K_j(dati, t)
    dati_T <- uncens_times(dati)
    d_vect <- d_ij(dati)
    r_vect <- r_ij(dati)
    result <- array(0, dim = n)
    for (i in 1:n) {
        km_est <- KM_estimator(dati, dati_T[i, 1])
        if (km_est > 0) {
            result[i] <- (d_vect[i] - r_vect[i]) / km_est
        }
    }
    return(max(result))
}

lambda_fun_modif <- function(dati1, dati2, t0, lambda, alpha) {
    n1 <- K_j(dati1, t0)
    n2 <- K_j(dati2, t0)
    dati1_T <- uncens_times(dati1)
    dati2_T <- uncens_times(dati2)
    d1 <- d_ij(dati1)
    d2 <- d_ij(dati2)
    r1 <- r_ij(dati1)
    r2 <- r_ij(dati2)
    value <- 0
    
    dati11_T <- dati1_T[1:n1, ]
    dati21_T <- dati2_T[1:n2, ]
    
    rep_ind_1 <- which(as.logical(rank(dati11_T[, 1], ties.method = "min") - 1:nrow(dati11_T)))
    rep_ind_2 <- which(as.logical(rank(dati21_T[, 1], ties.method = "min") - 1:nrow(dati21_T)))
    
    if (n1 > 0) {
        for (i in 1:n1) {
            if (i %in% rep_ind_1) {next}
            km_est <- KM_estimator(dati1, dati1_T[i, 1])
            value <- value + km_est * log(1 - d1[i] / (r1[i] + lambda * km_est))
        }
    }
    if (n2 > 0) {
        for (i in 1:n2) {
            if (i %in% rep_ind_2) {next}
            km_est <- KM_estimator(dati2, dati2_T[i, 1])
            value <- value - km_est * log(1 - d2[i] / (r2[i] - lambda * km_est))
        }
    }
    return(value + alpha)
}

lambda_n_modif <- function(dati_1, dati_2, t_0, alpha_1, int_red = 0) {
    LL <- D_j(dati_1, t_0) + int_red
    UL <- -D_j(dati_2, t_0) - int_red
    root <- uniroot(f = lambda_fun_modif, lower = LL, upper = UL, dati1 = dati_1, dati2 = dati_2, t0 = t_0, alpha = alpha_1)$root
    return(root)
}

lambda_n_modif <- Vectorize(lambda_n_modif, vectorize.args = 'alpha_1')

MZ_stat_modif <- function(dati_1, dati_2, t_0, alpha, int_red_ = 0) {
    n1 <- K_j(dati_1, t_0)
    n2 <- K_j(dati_2, t_0)
    dati_1_T <- uncens_times(dati_1)
    dati_2_T <- uncens_times(dati_2)
    d1 <- d_ij(dati_1)
    d2 <- d_ij(dati_2)
    r1 <- r_ij(dati_1)
    r2 <- r_ij(dati_2)
    
    lambda <- lambda_n_modif(dati_1, dati_2, t_0, alpha, int_red = int_red_)
    
    dati11_T <- dati_1_T[1:n1, ]
    dati21_T <- dati_2_T[1:n2, ]
    
    rep_ind_1 <- which(as.logical(rank(dati11_T[, 1], ties.method = "min") - 1:nrow(dati11_T)))
    rep_ind_2 <- which(as.logical(rank(dati21_T[, 1], ties.method = "min") - 1:nrow(dati21_T)))
    
    value <- 0
    for (i in 1:n1) {
        if (i %in% rep_ind_1) {next}
        lg <- lambda * KM_estimator(dati_1, dati_1_T[i, 1])
        rd1 <- r1[i] - d1[i]
        
        value <- value + rd1 * log(1 + lg / rd1)
        value <- value - r1[i] * log(1 + lg / r1[i])
    }
    for (i in 1:n2) {
        if (i %in% rep_ind_2) {next}
        lg <- lambda * KM_estimator(dati_2, dati_2_T[i, 1])
        rd2 <- r2[i] - d2[i]
        
        value <- value + rd2 * log(1 - lg / rd2)
        value <- value - r2[i] * log(1 - lg / r2[i])
    }
    return(-2 * value)
}

# Pahirko & Valeinis method code
# ---------------------------------------

posumm <- function(pofit, d = 6) {
    round(cbind(
        Est = pofit$beta,
        SD = sqrt(diag(pofit$vbeta)),
        lo.ci = pofit$beta - 1.96 * sqrt(diag(pofit$vbeta)),
        up.ci = pofit$beta + 1.96 * sqrt(diag(pofit$vbeta)),
        Wald = (pofit$beta / sqrt(diag(pofit$vbeta)))^2,
        PVal = 2 - 2 * pnorm(abs(pofit$beta / sqrt(diag(pofit$vbeta))))), d)
}

# KM estimator for censored time distribution G
g_estimator <- function(t, dati) {           # 1 - G_n
    n <- nrow(dati)
    dati <- dati[order(dati[, 1], decreasing = FALSE), ]
    vec <- (n - (1:n)) / (n - (1:n) + 1)
    v <- vec[(dati[, 1] <= t) & (dati[, 2] == 0)] 
    return(prod(v))
}
g_estimator <- Vectorize(g_estimator, "t")

# Function xi, which determines the parameter
ksi <- function(x, t_0 = 0, method = "mean") {
    if (method == "mean") return(x)
    else if (method == "surv_prob") return(as.numeric(x >= t_0))
    else if (method == "RMST") return(min(x, t_0))
    else return("NULL")
}
ksi <- Vectorize(ksi, "x")

# Transformation ksi(Z_i)*delta_i/(1-G(Z_i))
parveidojums <- function(dati, t_0 = 0, method = "mean") {
    mod <- ksi(dati[, 1], t_0, method) * dati[, 2] / g_estimator(dati[, 1], dati)
    mod[which(mod == "NaN")] <- 0
    return(mod)
}

S_n_vid <- function(dati, t_0 = 0, method = "mean") {
    dati <- dati[order(dati[, 1], decreasing = FALSE), ]
    z_sort <- dati[, 1]
    delta_sort <- dati[, 2]
    n <- nrow(dati)
    vec <- (n - (1:n)) / (n - (1:n) + 1)
    vec[delta_sort == 0] <- 1
    vec1 <- c(1, cumprod(vec))
    wn = sapply(1:n, function(i) {
        delta_sort[i] * vec1[i] / (n - i + 1) 
    })
    return(sum(wn * ksi(z_sort, t_0, method)))
}

# Jackknife dispersion estimator
D_jack <- function(dati, t_0 = 0, method = "mean") {
    dati <- dati[order(dati[, 1], decreasing = FALSE), ]
    z_sort <- dati[, 1]
    delta_sort <- dati[, 2]
    n <- nrow(dati)
    s_n <- S_n_vid(dati, t_0, method)
    s_n_k = sapply(1:n, function(i) {
        S_n_vid(dati[-i, ], t_0, method)
    })
    return((n - 1) * sum((s_n_k - s_n)^2))
}

# Constant r
r_est <- function(dati1, dati2, t_0 = 0, method = "mean") {
    n <- nrow(dati1)
    m <- nrow(dati2)
    
    V1 <- D_jack(dati1, t_0, method)
    M1 <- D_jack(dati2, t_0, method)
    V2 <- 1 / n * sum((parveidojums(dati1, t_0, method) - mean(parveidojums(dati1, t_0, method)))^2)
    M2 <- 1 / m * sum((parveidojums(dati2, t_0, method) - mean(parveidojums(dati2, t_0, method)))^2)
    
    r <- (M2 + m / n * V2) / (M1 + m / n * V1)
    return(r)
}

# Via package EL, the statistic, estimator and
# confidence intervals are calculated
EL2_est_new <- function(dati1, dati2, t_0 = 0, method = "mean", conf.level = 0.95) {
    r <- r_est(dati1, dati2, t_0, method)
    conf.level_new <- pchisq(qchisq(conf.level, 1) / r, 1)
    EL_rez <- EL::EL.means(parveidojums(dati2, t_0, method), 
                           parveidojums(dati1, t_0, method),
                           mu = 0, 
                           conf.level = conf.level_new)
    ELstat <- EL_rez$statistic
    CI_lower <- EL_rez$conf.int[1]
    CI_upper <- EL_rez$conf.int[2]
    return(list(stat = ELstat, estimate = EL_rez$estimate, CI_lower = CI_lower, CI_upper = CI_upper,
                new_conf.level = conf.level_new))
}
# Survival probability (SURV) functions
# ------------------------------------------------------------------------------
# Pseudo values approach
# ---------------------------------------
PO_SURV <- function(dati1, dati2, t_0) {
    n <- nrow(dati1)
    m <- nrow(dati2)
    dati <- rbind(dati1, dati2)
    dati$group <- factor(c(rep(0, n), rep(1, m)))
    
    dati$surv1 <- pseudosurv(dati$ztimes, dati$status, tmax = t_0)$pseudo
    dati$ID <- c(1:n, (n + 1):(n + m))
    rez4 <- posumm(geese(
        surv1 ~ group,
        data = dati,
        id = ID,
        mean.link = "identity"
    ))
    lower <- rez4[2, 3]
    upper <- rez4[2, 4]
    return(list(
        lower = lower,
        upper = upper,
        length = upper - lower,
        estimate = rez4[2, 1],
        cover = (lower < 0) & (upper > 0)
    ))
}

# Pahirko & Valeinis approach
# ---------------------------------------
EL_PV_SURV_unmodif <- function(dati1, dati2, t_0) {
    rez <- EL2_est_new(dati1, dati2, t_0, method = "surv_prob", conf.level = 0.95)
    return(
        list(
            lower = rez$CI_lower,
            upper = rez$CI_upper,
            length = rez$CI_upper - rez$CI_lower,
            estimate = as.numeric(rez$estimate),
            cover = (rez$CI_lower < 0) & (rez$CI_upper > 0)
        )
    )
}

EL_PV_SURV_stute <- function(dati1, dati2, t_0) {
    dati1 <- dati1[order(dati1[, 1], decreasing = FALSE), ]
    if (dati1[nrow(dati1) - 1, 2] == 0) {
        dati1[nrow(dati1), 2] <- 0
    }
    dati2 <- dati2[order(dati2[, 1], decreasing = FALSE), ]
    if (dati2[nrow(dati2) - 1, 2] == 0) {
        dati2[nrow(dati2), 2] <- 0
    }
    rez <- EL2_est_new(dati1, dati2, t_0, method = "surv_prob", conf.level = 0.95)
    return(
        list(
            lower = rez$CI_lower,
            upper = rez$CI_upper,
            length = rez$CI_upper - rez$CI_lower,
            estimate = as.numeric(rez$estimate),
            cover = (rez$CI_lower < 0) & (rez$CI_upper > 0)
        )
    )
}

EL_PV_SURV_11 <- function(dati1, dati2, t_0) {
    dati1[topn(dati1[, 1], 2), 2] <- c(1, 1)
    dati2[topn(dati2[, 1], 2), 2] <- c(1, 1)
    
    rez <- EL2_est_new(dati1, dati2, t_0, method = "surv_prob", conf.level = 0.95)
    return(
        list(
            lower = rez$CI_lower,
            upper = rez$CI_upper,
            length = rez$CI_upper - rez$CI_lower,
            estimate = as.numeric(rez$estimate),
            cover = (rez$CI_lower < 0) & (rez$CI_upper > 0)
        )
    )
}

# Crude estimates approach
# ---------------------------------------
crude_SURV <- function(dati1, dati2, t_0) {
    n <- nrow(dati1)
    m <- nrow(dati2)
    dati <- rbind(dati1, dati2)
    dati$group <- factor(c(rep(0, n), rep(1, m)))
    
    fit1 <- survfit(Surv(ztimes, status) ~ group, data = dati)
    s_probs <- summary(fit1, times = t_0)
    m0 <- s_probs$surv[1]
    m1 <- s_probs$surv[2]
    md <- m1 - m0
    sd <- sqrt(s_probs$std.err[1] ^ 2 + s_probs$std.err[1] ^ 2)
    return(
        list(
            lower = md - 1.96 * sd,
            upper = md + 1.96 * sd,
            length = 3.92 * sd,
            estimate = md,
            cover = (md - 1.96 * sd < 0) & (md + 1.96 * sd > 0)
        )
    )
}

# McKeague & Zhao approach
# ---------------------------------------
EL_MZ_SURV_sb <- function(dati1, dati2, t_0, q_1 = NULL, N = 1000, seed = NULL, int_red = 0) {
    approx_intervals <- crude_SURV(dati1, dati2, t_0)
    Lval <- approx_intervals$lower - approx_intervals$length * 2
    Uval <- approx_intervals$upper + approx_intervals$length * 2
    estimate <- optimize(f = MZ_stat_modif, 
                         lower = Lval, 
                         upper = Uval, 
                         dati_1 = dati1, 
                         dati_2 = dati2, 
                         t_0 = t_0,
                         int_red_ = int_red)
    na <- NA
    if (identical(q_1, NULL)) {
        q_1 <- sboot_mz(dati1, dati2, t0 = t_0, alpha = estimate$minimum, N = N, input_seed = seed)
        na <- q_1$na
        q_1 <- q_1$q
    }
    
    ci_fun <- function(alpha_) {
        return(MZ_stat_modif(dati_1 = dati1, dati_2 = dati2, t_0 = t_0, alpha = alpha_, int_red_ = int_red) - q_1)
    }
    lower <- nleqslv(Lval, ci_fun)$x
    upper <- nleqslv(Uval, ci_fun)$x
    r <- list()
    
    r <- append(r, list(lower = lower))
    r <- append(r, list(upper = upper))
    r <- append(r, list(length = upper - lower))
    r <- append(r, list(estimate = estimate$minimum))
    r <- append(r, list(cover = (lower < 0) & (upper > 0)))
    r <- append(r, list(q = q_1, na = na))
    
    return(r)
}

# Restricted mean survival time (RMST) functions
# ------------------------------------------------------------------------------
# Pseudo values approach
# ---------------------------------------
PO_RMST <- function(dati1, dati2, t_0) {
    n <- nrow(dati1)
    m <- nrow(dati2)
    dati <- rbind(dati1, dati2)
    dati$group <- factor(c(rep(0, n), rep(1, m)))
    
    dati$rmst1 <- pseudomean(dati$ztimes, dati$status, tmax = t_0)
    dati$ID <- c(1:n, (n + 1):(n + m))
    rez <- posumm(geese(
        rmst1 ~ group,
        data = dati,
        id = ID,
        mean.link = "identity"
    ))
    lower <- rez[2, 3]
    upper <- rez[2, 4]
    return(list(
        lower = lower,
        upper = upper,
        length = upper - lower,
        estimate = rez[2, 1],
        cover = (lower < 0) & (upper > 0)
    ))
}

# Pahirko & Valeinis approach
# ---------------------------------------
EL_PV_RMST_unmodif <- function(dati1, dati2, t_0) {
    rez <- EL2_est_new(dati1, dati2, t_0, method = "RMST", conf.level = 0.95)
    return(
        list(
            lower = rez$CI_lower,
            upper = rez$CI_upper,
            length = rez$CI_upper - rez$CI_lower,
            estimate = as.numeric(rez$estimate),
            cover = (rez$CI_lower < 0) & (rez$CI_upper > 0)
        )
    )
}

EL_PV_RMST_stute <- function(dati1, dati2, t_0) {
    dati1 <- dati1[order(dati1[, 1], decreasing = FALSE), ]
    if (dati1[nrow(dati1) - 1, 2] == 0) {
        dati1[nrow(dati1), 2] <- 0
    }
    dati2 <- dati2[order(dati2[, 1], decreasing = FALSE), ]
    if (dati2[nrow(dati2) - 1, 2] == 0) {
        dati2[nrow(dati2), 2] <- 0
    }
    rez <- EL2_est_new(dati1, dati2, t_0, method = "RMST", conf.level = 0.95)
    return(
        list(
            lower = rez$CI_lower,
            upper = rez$CI_upper,
            length = rez$CI_upper - rez$CI_lower,
            estimate = as.numeric(rez$estimate),
            cover = (rez$CI_lower < 0) & (rez$CI_upper > 0)
        )
    )
}

EL_PV_RMST_11 <- function(dati1, dati2, t_0) {
    dati1[topn(dati1[, 1], 2), 2] <- c(1, 1)
    dati2[topn(dati2[, 1], 2), 2] <- c(1, 1)
    
    rez <- EL2_est_new(dati1, dati2, t_0, method = "RMST", conf.level = 0.95)
    return(
        list(
            lower = rez$CI_lower,
            upper = rez$CI_upper,
            length = rez$CI_upper - rez$CI_lower,
            estimate = as.numeric(rez$estimate),
            cover = (rez$CI_lower < 0) & (rez$CI_upper > 0)
        )
    )
}
# Crude estimates approach
# ---------------------------------------
crude_RMST <- function(dati1, dati2, t_0) {
    n <- nrow(dati1)
    m <- nrow(dati2)
    dati <- rbind(dati1, dati2)
    dati$group <- factor(c(rep(0, n), rep(1, m)))
    
    rez3 <- with(dati, survRM2::rmst2(ztimes, status, group, tau = t_0))
    gar <- rez3$unadjusted.result[1, 3] - rez3$unadjusted.result[1, 2]
    return(
        list(
            lower = rez3$unadjusted.result[1, 2],
            upper = rez3$unadjusted.result[1, 3],
            length = rez3$unadjusted.result[1, 3] - rez3$unadjusted.result[1, 2],
            estimate = rez3$unadjusted.result[1, 1],
            cover = (rez3$unadjusted.result[1, 2] < 0) &
                (rez3$unadjusted.result[1, 3] > 0)
        )
    )
}
# Zhou approach
# ---------------------------------------
findUL_nleq <- function (fun, est, level = qchisq(0.95, 1), ...) {
    tempfun <- function(beta) {
        return(level - fun(beta, ...))
    }
    Lint <- nleqslv(est$lower, tempfun, method = "Broyden")
    Uint <- nleqslv(est$upper, tempfun, method = "Broyden")
    return(list(Low = Lint$x, Up = Uint$x))
}

EL_Zhou_RMST <- function(dati2, dati1, t_0, lw = -1000, up = 1000, maxit_ = 5) {
    RMSTdiff <- function(r, x1, d1, x2, d2, theta) {
        temp1 <- el.cen.EM2(
            x = x1,
            d = d1,
            fun = function(x) pmin(x, t_0),
            mu = r,
            maxit = maxit_
        )
        temp2 <- el.cen.EM2(
            x = x2,
            d = d2,
            fun = function(x) pmin(x, t_0),
            mu = r - theta,
            maxit = maxit_
        )
        return(temp1$"-2LLR" + temp2$"-2LLR")
    }
    ThetafunD <- function(theta, x1, d1, x2, d2) {
        temp <- optimize(
            f = RMSTdiff,
            lower = lw,
            upper = up,
            x1 = x1,
            d1 = d1,
            x2 = x2,
            d2 = d2,
            theta = theta
        )
        return(temp$objective)
    }
    # Use SurvRM2 estimate as initial MLE value for optimization
    rez <- findUL_nleq(
        fun = ThetafunD,
        est = crude_RMST(dati2, dati1, t_0 = t_0),
        x1 = dati1[, 1],
        d1 = dati1[, 2],
        x2 = dati2[, 1],
        d2 = dati2[, 2]
    )
    estimate <- optimize(
        ThetafunD,
        lower = rez$Low,
        upper = rez$Up,
        x1 = dati1[, 1],
        d1 = dati1[, 2],
        x2 = dati2[, 1],
        d2 = dati2[, 2]
    )
    return(
        list(
            lower = rez$Low,
            upper = rez$Up,
            length = rez$Up - rez$Low,
            estimate = estimate$minimum,
            cover = (rez$Low < 0) & (rez$Up > 0)
        )
    )
}

# Bootstrap for 
# McKeague & Zhao method
# ---------------------------------------
smooth_bootstrap <- function(dati, ties.breaker = 0.00001) {
    dati <- order_times(dati)
    dati <- rbind(c(0, 1), dati)
    n <- nrow(dati)
    
    d <- rank(dati[, 1], ties.method = "last") - 1:n
    for (i in 1:n) {
        t <- d[i]
        if (t > 0) {
            dati[i:(i + t), 1] <- rep(1, t + 1) * dati[i, 1] + ties.breaker * 0:t
        }
    }
    
    censored <- cens_times(dati)
    c_n <- nrow(censored)
    upper_int <- dati[, 1] * dati[, 2]
    upper_int[n + 1] <- 0
    
    for (i in n:1) {
        if (upper_int[i] == 0) {
            upper_int[i] <- upper_int[i + 1]
        }
    }
    
    intervals <- array(NA, dim = c(n, 4))
    intervals[, 1] <- dati[, 1]
    intervals[, 2] <- upper_int[-1]
    intervals[, 3] <- dati[, 2]
    
    for (i in 1:n) {
        if (intervals[i, 3] == 1) {
            coef_ <- 1 / n
            prod_ <- 1
            for (j in censored[censored[, 1] < intervals[i, 1], 1]) {
                n_1 <- n - sum(dati[, 1] < j)
                prod_ <- prod_ * (n_1 + 1) / n_1
            }
        } else if (intervals[i, 3] == 0) {
            coef_ <- 1 / n / (n - i + 1)
            prod_ <- 1
            for (j in censored[censored[, 1] < intervals[i, 1], 1]) {
                n_1 <- n - sum(dati[, 1] < j)
                prod_ <- prod_ * (n_1 + 1) / n_1
            }
        } else {stop("Status in column not 1 or 0")}
        intervals[i, 4] <- coef_ * prod_
    }
    
    interval_ns <- sample(x = 1:n, size = n - 1, replace = TRUE, prob = intervals[, 4])
    rez <- data.frame(ztimes = rep(NA, n - 1), status = rep(1, n - 1))
    for (i in 1:(n - 1)) {
        j <- interval_ns[i]
        if (intervals[j, 2] == 0) {
            time <- intervals[j, 1]
            rate <- -log(intervals[j, 4]) / time
            ztime <- 0
            while (ztime <= time) { #with given rate (usually very small), is fast
                ztime <- rexp(1, rate = rate)
            }
        } else {
            ztime <- runif(1, min = intervals[j, 1], max = intervals[j, 2])
        }
        rez[i, 1] <- ztime
    }
    return(order_times(rez))
}

sboot_mz <- function(dati1, dati2, t0, alpha = 0, N = 10000, input_seed = NULL) {
    results <- array(NA, dim = c(N))
    if (!identical(input_seed, NULL)) {
        set.seed(input_seed)
    }
    for (i in 1:N) {
        sampled1 <- smooth_bootstrap(dati1)
        sampled2 <- smooth_bootstrap(dati2)
        it_ <- suppressWarnings(try(MZ_stat_modif(sampled1, sampled2, t_0 = t0, alpha = alpha), silent = TRUE))
        if (inherits(it_, "try-error")) {
            next
        }
        results[i] <- it_
    }
    return(list(q = quantile(results, probs = 0.95, na.rm = TRUE), na = sum(is.na(results))))
}

# Function to transform table
#(for 'custom_cols' include error column)
# ---------------------------------------

transform_n <- function (x, offset_ = 0, custom_cols, custom_col_names) {
    x <- x[, 5 + 1:custom_cols]
    y <- array(dim = c(24, 2 * custom_cols))
    for (i in 0:1) {
        for (j in 0:2) {
            for (k in 0:7) {
                if (identical(ncol(x), NULL)) {
                    y[1 + k + j * 8, 1:custom_cols + i * custom_cols] <- unlist(x[1 + k * 6 + i * 3 + j])
                } else {
                    y[1 + k + j * 8, 1:custom_cols + i * custom_cols] <- unlist(x[1 + k * 6 + i * 3 + j, ])
                }
            }
        }
    }
    y <- data.frame(y)
    colnames(y) <- rep(custom_col_names, 2)
    n_vect <- c(20, 15, 20, 30, 50, 70, 100, 150, 200)
    n_vect_ <- rep(NA, 8)
    for (i in 1:8) {
        n_vect_[i] <- paste0("(", n_vect[1 + i - 1 * offset_], ", ", n_vect[1 + i], ")")
    }
    nm <- rep(n_vect_, 3)
    y <- cbind(nm, y)
    p <- c("p = 0.1", rep(NA, 7), "p = 0.2", rep(NA, 7), "p = 0.3", rep(NA, 7))
    y <- cbind(p, y)
    t <- c(rep(NA, 2), "t = 0.5", rep(NA, custom_cols - 1), "t = 0.7", rep(NA, custom_cols - 1))
    y <- rbind(t, y)
    return(y)
}
