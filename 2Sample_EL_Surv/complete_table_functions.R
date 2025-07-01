setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("functions_1.R")

library(doParallel)
registerDoParallel(12) # Adjust cores for computer
# ---------------------------------------
coverage_accuracies_SURV <- function(n, m, t_0, N_ = 1:1000, x = 1, y = 1, c, dati1, dati2, methods, mz_qs) {
    
    #setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
    #Make sure the function is executed in the same directory as "functions_1.R", 
    #former command doesn't work while parallelizing, because rstudio is not running
    source("functions_1.R", local = TRUE)
    
    if (c == 0.11) {c_value <- 1}
    else if (c == 0.25) {c_value <- 2}
    else if (c == 0.429) {c_value <- 3}
    else {stop("Given c value is not one of (0.11, 0.25, 0.429)")}
    
    N <- length(N_)
    
    if (sum(methods) == 0) {
        stop("Given method vector values sum to 0 (no methods selected).")
    }
    
    if (methods[1]) {
        c_PO <- array(dim = c(5, N))
    }
    if (methods[2]) {
        c_crude <- array(dim = c(5, N))
    }
    if (methods[3]) {
        c_MZ <- array(dim = c(5, N))
    }
    if (methods[4]) {
        c_PV_stute <- array(dim = c(5, N))
    }
    if (methods[5]) {
        c_PV <- array(dim = c(5, N))
    }
    
    # Quantiles for M&Z method
    if (t_0 == 0.5) {
        t_value <- 0
    }
    if (t_0 == 0.7) {
        t_value <- 1
    }
    quant_ <- mz_qs[t_value * 3 + c_value]
    
    errs <- 0
    for (i in 1:N) {
        n_dati <- data.frame(ztimes = dati1[N_[i], 1:n, 1, c_value], status = dati1[N_[i], 1:n, 2, c_value])
        m_dati <- data.frame(ztimes = dati2[N_[i], 1:m, 1, c_value], status = dati2[N_[i], 1:m, 2, c_value])
        
        err1 <- 0; err2 <- 0; err3 <- 0; err4 <- 0; err5 <- 0
        
        #Pseudo observations
        if (methods[1]) {
            t1 <- try(unlist(PO_SURV(n_dati, m_dati, t_0)), silent = TRUE)
            err1 <- inherits(t1, "try-error")
            if (!err1 & more_than(t1[3], 0)) {
                c_PO[, i] <- t1
            }
        }
        #Crude estimation
        if (methods[2]) {
            t2 <- try(unlist(crude_SURV(n_dati, m_dati, t_0)), silent = TRUE)
            err2 <- inherits(t2, "try-error")
            if (!err2 & more_than(t2[3], 0)) {
                c_crude[, i] <- t2
            }
        }
        #M&Z EL method
        if (methods[3]) {
            t3 <- try(unlist(EL_MZ_SURV_sb(n_dati, m_dati, t_0, q_1 = quant_)), silent = TRUE)
            err3 <- inherits(t3, "try-error")
            if (!err3 & more_than(t3[3], 0)) {
                c_MZ[, i] <- t3
            }
        }
        #P&V EL method with Stute correction
        if (methods[4]) {
            t4 <- try(unlist(EL_PV_SURV_stute(n_dati, m_dati, t_0)), silent = TRUE)
            err4 <- inherits(t4, "try-error")
            if (!err4 & more_than(t4[3], 0)) {
                c_PV_stute[, i] <- t4
            }
        }
        #P&V EL method
        if (methods[5]) {
            t5 <- try(unlist(EL_PV_SURV_11(n_dati, m_dati, t_0)), silent = TRUE)
            err5 <- inherits(t5, "try-error")
            if (!err5 & more_than(t5[3], 0)) {
                c_PV[, i] <- t5
            }
        }
        
        if (err1 | err2 | err3 | err4 | err5) {
            errs <- errs + 1
        }
    }
    
    if (methods[1]) {
        cov_PO <- mean(na.exclude(c_PO[5, ]))
        gar_PO <- mean(na.exclude(c_PO[3, ]))
    }
    if (methods[2]) {
        cov_crude <- mean(na.exclude(c_crude[5, ]))
        gar_crude <- mean(na.exclude(c_crude[3, ]))
    }
    if (methods[3]) {
        cov_MZ <- mean(na.exclude(c_MZ[5, ]))
        gar_MZ <- mean(na.exclude(c_MZ[3, ]))
    }
    if (methods[4]) {
        cov_PV_stute <- mean(na.exclude(c_PV_stute[5, ]))
        gar_PV_stute <- mean(na.exclude(c_PV_stute[3, ]))
    }
    if (methods[5]) {
        cov_PV <- mean(na.exclude(c_PV[5, ]))
        gar_PV <- mean(na.exclude(c_PV[3, ]))
    }
    
    r <- data.frame(n = n, m = m, par = paste0("(", x, ", ", y, ")"), t = t_0, c = c)
    if (methods[1]) {
        r <- cbind(r, `crude` = paste0(round(cov_crude, 3), " (", round(gar_crude, 3), ")"))
    }
    if (methods[2]) {
        r <- cbind(r, `PO` = paste0(round(cov_PO, 3), " (", round(gar_PO, 3), ")"))
    }
    if (methods[3]) {
        r <- cbind(r, `MZ` = paste0(round(cov_MZ, 3), " (", round(gar_MZ, 3), ")"))
    }
    if (methods[4]) {
        r <- cbind(r, `PV stute` = paste0(round(cov_PV_stute, 3), " (", round(gar_PV_stute, 3), ")"))
    }
    if (methods[5]) {
        r <- cbind(r, `PV` = paste0(round(cov_PV, 3), " (", round(gar_PV, 3), ")"))
    }
    r <- cbind(r, `Err` = errs)
    return(r)
}

coverage_accuracies_RMST <- function(n, m, t_0, N_ = 1:1000, x = 1, y = 1, c, dati1, dati2, methods) {
    
    #setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
    #Make sure the function is executed in the same directory as "functions_1.R", 
    #former command doesn't work while parallelizing, because rstudio is not running
    source("functions_1.R", local = TRUE)
    
    if (c == 0.11) {c_value <- 1}
    else if (c == 0.25) {c_value <- 2}
    else if (c == 0.429) {c_value <- 3}
    else {stop("Given c value is not one of (0.11, 0.25, 0.429)")}
    
    N <- length(N_)
    
    if (sum(methods) == 0) {
        stop("Given method vector values sum to 0 (no methods selected).")
    }
    
    if (methods[1]) {
        c_PO <- array(dim = c(5, N))
    }
    if (methods[2]) {
        c_crude <- array(dim = c(5, N))
    }
    if (methods[3]) {
        c_Zhou <- array(dim = c(5, N))
    }
    if (methods[4]) {
        c_PV_stute <- array(dim = c(5, N))
    }
    if (methods[5]) {
        c_PV <- array(dim = c(5, N))
    }
    
    errs <- 0
    for (i in 1:N) {
        n_dati <- data.frame(ztimes = dati1[N_[i], 1:n, 1, c_value], status = dati1[N_[i], 1:n, 2, c_value])
        m_dati <- data.frame(ztimes = dati2[N_[i], 1:m, 1, c_value], status = dati2[N_[i], 1:m, 2, c_value])
        
        err1 <- 0; err2 <- 0; err3 <- 0; err4 <- 0; err5 <- 0
        
        #Pseudo observations
        if (methods[1]) {
            t1 <- try(unlist(PO_RMST(n_dati, m_dati, t_0)), silent = TRUE)
            err1 <- inherits(t1, "try-error")
            if (!err1 & more_than(t1[3], 0)) {
                c_PO[, i] <- t1
            }
        }
        #Crude estimation
        if (methods[2]) {
            t2 <- try(unlist(crude_RMST(n_dati, m_dati, t_0)), silent = TRUE)
            err2 <- inherits(t2, "try-error")
            if (!err2 & more_than(t2[3], 0)) {
                c_crude[, i] <- t2
            }
        }
        #Zhou EL method
        if (methods[3]) {
            t3 <- try(unlist(EL_Zhou_RMST(n_dati, m_dati, t_0, lw = -2, up = 2)), silent = TRUE)
            err3 <- inherits(t3, "try-error")
            if (!err3 & more_than(t3[3], 0)) {
                c_Zhou[, i] <- t3
            }
        }
        #P&V EL method with Stute correction
        if (methods[4]) {
            t4 <- try(unlist(EL_PV_RMST_stute(n_dati, m_dati, t_0)), silent = TRUE)
            err4 <- inherits(t4, "try-error")
            if (!err4 & more_than(t4[3], 0)) {
                c_PV_stute[, i] <- t4
            }
        }
        #P&V EL method
        if (methods[5]) {
            t5 <- try(unlist(EL_PV_RMST_11(n_dati, m_dati, t_0)), silent = TRUE)
            err5 <- inherits(t5, "try-error")
            if (!err5 & more_than(t5[3], 0)) {
                c_PV[, i] <- t5
            }
        }
        
        if (err1 | err2 | err3 | err4 | err5) {
            errs <- errs + 1
        }
    }
    
    if (methods[1]) {
        cov_PO <- mean(na.exclude(c_PO[5, ]))
        gar_PO <- mean(na.exclude(c_PO[3, ]))
    }
    if (methods[2]) {
        cov_crude <- mean(na.exclude(c_crude[5, ]))
        gar_crude <- mean(na.exclude(c_crude[3, ]))
    }
    if (methods[3]) {
        cov_Zhou <- mean(na.exclude(c_Zhou[5, ]))
        gar_Zhou <- mean(na.exclude(c_Zhou[3, ]))
    }
    if (methods[4]) {
        cov_PV_stute <- mean(na.exclude(c_PV_stute[5, ]))
        gar_PV_stute <- mean(na.exclude(c_PV_stute[3, ]))
    }
    if (methods[5]) {
        cov_PV <- mean(na.exclude(c_PV[5, ]))
        gar_PV <- mean(na.exclude(c_PV[3, ]))
    }
    
    r <- data.frame(n = n, m = m, par = paste0("(", x, ", ", y, ")"), t = t_0, c = c)
    if (methods[1]) {
        r <- cbind(r, `crude` = paste0(round(cov_crude, 3), " (", round(gar_crude, 3), ")"))
    }
    if (methods[2]) {
        r <- cbind(r, `PO` = paste0(round(cov_PO, 3), " (", round(gar_PO, 3), ")"))
    }
    if (methods[3]) {
        r <- cbind(r, `Zhou` = paste0(round(cov_Zhou, 3), " (", round(gar_Zhou, 3), ")"))
    }
    if (methods[4]) {
        r <- cbind(r, `PV stute` = paste0(round(cov_PV_stute, 3), " (", round(gar_PV_stute, 3), ")"))
    }
    if (methods[5]) {
        r <- cbind(r, `PV` = paste0(round(cov_PV, 3), " (", round(gar_PV, 3), ")"))
    }
    r <- cbind(r, `Err` = errs)
    return(r)
}

# Data used for simulations - seeds = 401; 402
# ---------------------------------------

Dati_n_surv <- data_gen(n = 200, columns = 10000, x = 1, c_vect = c(0.11, 0.25, 0.429), input_seed = 401)
Dati_m_surv <- data_gen(n = 200, columns = 10000, x = 1, c_vect = c(0.11, 0.25, 0.429), input_seed = 402)

# Parameters for parallelization
# ---------------------------------------
nm <- c(20, 15, 20, 30, 50, 70, 100, 150, 200)
offset <- 0
# offset <- 1 
Iterations <- 1:10000

# Method selection
surv_methods = c(1, 1, 1, 1, 1)
rmst_methods = c(1, 1, 1, 1, 1)

# M&Z method quantiles - seeds = 501; 502
#from quantile_generation.R

quantiles_ <- c(2.36050828882, 2.34391267871, 2.41172277519, 1.98598420405, 1.96498376766, 1.98220195727)

# Adjust for different parallel process amount (12 = default)
# code works only for 6, 12, 24, etc., 
# have to adjust creation of n and m in table cycles accordingly
#t0_values <- c(0.5, 0.5, 0.5, 0.7, 0.7, 0.7) #6 values
#c_values <- c(0.11, 0.25, 0.429, 0.11, 0.25, 0.429) #6 values

t0_values <- c(0.5, 0.5, 0.5, 0.7, 0.7, 0.7, 0.5, 0.5, 0.5, 0.7, 0.7, 0.7) #12 values
c_values <- c(0.11, 0.25, 0.429, 0.11, 0.25, 0.429, 0.11, 0.25, 0.429, 0.11, 0.25, 0.429) #12 values

# SURV Table generator
# ---------------------------------------
SURV_Table <- c()

for (i in 0:3) {
    # if (offset) { #for 6 values
    #     n <- rep(nm[1 + 1 * i], each = 6)
    #     m <- rep(nm[2 + 1 * i], each = 6)
    # } else {
    #     n <- rep(nm[2 + 1 * i], each = 6)
    #     m <- rep(nm[2 + 1 * i], each = 6)
    # }
    if (offset) { #for 12 values
        n <- rep(nm[(1 + 2 * i):(2 + 2 * i)], each = 6)
        m <- rep(nm[(2 + 2 * i):(3 + 2 * i)], each = 6)
    } else {
        n <- rep(nm[(2 + 2 * i):(3 + 2 * i)], each = 6)
        m <- rep(nm[(2 + 2 * i):(3 + 2 * i)], each = 6)
    }
    SURV_Table_iter <- foreach(
        n_iter = n,
        m_iter = m,
        t_iter = t0_values,
        c_iter = c_values,
        .combine = 'rbind'
    ) %dopar%
        coverage_accuracies_SURV(
            n = n_iter,
            m = m_iter,
            t_0 = t_iter,
            N_ = Iterations,
            c = c_iter,
            dati1 = Dati_n_surv,
            dati2 = Dati_m_surv,
            methods = surv_methods,
            mz_qs = quantiles_
        )
    SURV_Table <- rbind(SURV_Table, SURV_Table_iter)
}
SURV_Table

# RMST Table generator
# ---------------------------------------
RMST_Table <- c()

for (i in 0:3) {
    # if (offset) { #for 6 values
    #     n <- rep(nm[1 + 1 * i], each = 6)
    #     m <- rep(nm[2 + 1 * i], each = 6)
    # } else {
    #     n <- rep(nm[2 + 1 * i], each = 6)
    #     m <- rep(nm[2 + 1 * i], each = 6)
    # }
    if (offset) { #for 12 values
        n <- rep(nm[(1 + 2 * i):(2 + 2 * i)], each = 6)
        m <- rep(nm[(2 + 2 * i):(3 + 2 * i)], each = 6)
    } else {
        n <- rep(nm[(2 + 2 * i):(3 + 2 * i)], each = 6)
        m <- rep(nm[(2 + 2 * i):(3 + 2 * i)], each = 6)
    }
    RMST_Table_iter <- foreach(n_iter = n, m_iter = m, t_iter = t0_values, c_iter = c_values, .combine = 'rbind') %dopar%
        coverage_accuracies_RMST(
            n = n_iter,
            m = m_iter,
            t_0 = t_iter,
            N_ = Iterations,
            c = c_iter,
            dati1 = Dati_n_surv,
            dati2 = Dati_m_surv,
            methods = rmst_methods
        )
    RMST_Table <- rbind(RMST_Table, RMST_Table_iter)
}
RMST_Table

#Using the transformation functions, to get better looking tables
writexl::write_xlsx(
    transform_n(SURV_Table, offset_ = 0, custom_cols = 6, custom_col_names = c("Norm. Approx.", 
                                                                               "PO", 
                                                                               "EL (MZ)", 
                                                                               "EL (PV Stute)", 
                                                                               "EL (PV)", 
                                                                               "Err")),
    "SURV_Table.xlsx")
writexl::write_xlsx(
    transform_n(RMST_Table, offset_ = 0, custom_cols = 6, custom_col_names = c("Norm. Approx.", 
                                                                               "PO", 
                                                                               "EL (Zhou)",  
                                                                               "EL (PV Stute)", 
                                                                               "EL (PV)", 
                                                                               "Err")),
    "RMST_Table.xlsx")