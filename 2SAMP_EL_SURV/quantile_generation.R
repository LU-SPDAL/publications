setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("functions_1.R")

library(doParallel)
registerDoParallel(6)

# M&Z method quantiles - seeds = 501; 502
#1000 by 1000 sample test, for asymptotic properties
# !!! Very large data frames
qdati_n <- data_gen(n = 1000, columns = 10000, input_seed = 501)
qdati_m <- data_gen(n = 1000, columns = 10000, input_seed = 502)

array_1 <- c()

t0_values <- c(0.5, 0.5, 0.5, 0.7, 0.7, 0.7) #6 values
c_values <- c(0.11, 0.25, 0.429, 0.11, 0.25, 0.429) #6 values
n <- rep(1000, 6)
m <- rep(1000, 6)
N <- 10000
array_1 <- foreach(
    n_iter = n,
    m_iter = m,
    t_iter = t0_values,
    c_iter = c_values,
    .combine = 'rbind',
    .packages = c('nleqslv')
) %dopar% {
    
    #setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
    #Make sure the function is executed in the same directory as "functions_1.R", 
    #former command doesn't work while parallelizing, because rstudio is not running
    source("functions_1.R", local = TRUE)
    
    iter_array <- array(NA, dim = c(2, N))
    iter_array[1, ] <- c(n_iter, m_iter, t_iter, c_iter, rep(NA, N - 4))
    occurences <- 0
    occurences_2 <- 0
    for (i in 1:N) {
        
        if (c_iter == 0.11) {c_value <- 1}
        else if (c_iter == 0.25) {c_value <- 2}
        else if (c_iter == 0.429) {c_value <- 3}
        else {stop("Given c value is not one of (0.11, 0.25, 0.429)")}
        
        n_dati <- data.frame(ztimes = qdati_n[i, 1:n_iter, 1, c_value],
                             status = qdati_n[i, 1:n_iter, 2, c_value])
        m_dati <- data.frame(ztimes = qdati_m[i, 1:m_iter, 1, c_value],
                             status = qdati_m[i, 1:m_iter, 2, c_value])
        
        if ((K_j(n_dati, t_iter) == 0) | (K_j(m_dati, t_iter) == 0)) {
            occurences_2 <- occurences_2 + 1
            next
        }
        
        n_dati_check <- uncens_times(n_dati)
        m_dati_check <- uncens_times(m_dati)
        
        if (!((sum(n_dati_check[, 1] > t_iter) > 0) & (sum(m_dati_check[, 1] > t_iter) > 0))) { 
            #checks if both have at least one *uncensored* time larger than the given t_0
            #if no, skips the calculations and counts how many times this has happened
            occurences <- occurences + 1
            next
        }
        iter_array[2, i] <- MZ_stat_modif(n_dati, m_dati, t_iter, 0, int_red_ = 1e-6)
    }
    iter_array[1, 5:6] <- c(occurences, occurences_2)
    return(iter_array)
}
data.frame(
    p = rep(c("10%", "20%", "30%"), 2),
    t = t0_values,
    q = c(
        quantile(x = as.numeric(array_1[2, ]), 0.95),
        quantile(x = as.numeric(array_1[4, ]), 0.95),
        quantile(x = as.numeric(array_1[6, ]), 0.95),
        quantile(x = as.numeric(array_1[8, ]), 0.95),
        quantile(x = as.numeric(array_1[10, ]), 0.95),
        quantile(x = as.numeric(array_1[12, ]), 0.95)
    )
)