source('functions.R')
library(tidyverse)
library(parallel)
library(doParallel)
# install.packages('emplik')

coverage <- function(ci, mu = 0){
  
  if(is.infinite(ci[1]) || is.infinite(ci[2])){
    return(list('cov' = NA,
                'len' = NA))
  }else{
    return(list('cov' = ifelse(ci[1] < mu & ci[2] > mu, 1, 0),
                'len' = ci[2] - ci[1]))
  }
}


n <- c(50, 100, 200, 500)
iter <- 10000

cores <- detectCores() - 1
clust <- makeCluster(cores)
clusterEvalQ(clust, set.seed(1))
clusterExport(clust, varlist = c('iter', 'n', 'gen.N', 'F.cn', 'gen.N2', 'F.cn2'))

F1 <- parSapply(clust, n, function(nn) lapply(1:iter, function(x) replicate(nn, gen.N())))
F2 <- parSapply(clust, n, function(nn) lapply(1:iter, function(x) replicate(nn, gen.N2())))

stopCluster(clust)



alph <- c(0.05, 0.1, 0.15, 0.2)
gamm <- c(0.1, 0.2, 0.3, 0.4)
alph_gam <- data.frame(alpha = c(rep(alph[1],4),rep(alph[2],3), rep(alph[3],3), rep(alph[4],2)),
                                gamma = c(gamm, gamm[-1], gamm[-1], gamm[-(1:2)]))

F1.cov <- matrix(nrow = nrow(alph_gam), ncol = length(n))
F1.len <- matrix(nrow = nrow(alph_gam), ncol = length(n))
F1.cov.norm <- matrix(nrow = nrow(alph_gam), ncol = length(n))
F1.len.norm <- matrix(nrow = nrow(alph_gam), ncol = length(n))

F2.cov <- matrix(nrow = nrow(alph_gam), ncol = length(n))
F2.len <- matrix(nrow = nrow(alph_gam), ncol = length(n))
F2.cov.norm <- matrix(nrow = nrow(alph_gam), ncol = length(n))
F2.len.norm <- matrix(nrow = nrow(alph_gam), ncol = length(n))

system.time({
  clust <- makeCluster(cores)
  clusterExport(clust, varlist = c('iter', 
                                 'empirical.stm', 
                                 'coverage', 
                                 'ST_mean',
                                 'F1',
                                 'F2',
                                 'J_fun',
                                 'emp.conf.intervals.stmean',
                                 'emp.lik.stmean',
                                 'stmeanvar.asym',
                                 'normal.stm',
                                 'alph_gam'))
  
  ## F1
  # n = 50
  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:nrow(alph_gam), function(j) {
      coverage(empirical.stm(F1[i,1][[1]], alph_gam$alpha[j], alph_gam$gamma[j])$conf.int)
    })
  })

  F1.cov[,1] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F1.len[,1] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)

  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:nrow(alph_gam), function(j) {
      coverage(normal.stm(F1[i,1][[1]], alph_gam$alpha[j], alph_gam$gamma[j])$conf.int)
    })
  })

  F1.cov.norm[,1] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F1.len.norm[,1] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)

  ## n = 100
  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:nrow(alph_gam), function(j) {
      coverage(empirical.stm(F1[i,2][[1]], alph_gam$alpha[j], alph_gam$gamma[j])$conf.int)
    })
  })

  F1.cov[,2] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F1.len[,2] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)

  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:nrow(alph_gam), function(j) {
      coverage(normal.stm(F1[i,2][[1]], alph_gam$alpha[j], alph_gam$gamma[j])$conf.int)
    })
  })

  F1.cov.norm[,2] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F1.len.norm[,2] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)

  ## n = 200
  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:nrow(alph_gam), function(j) {
      coverage(empirical.stm(F1[i,3][[1]], alph_gam$alpha[j], alph_gam$gamma[j])$conf.int)
    })
  })

  F1.cov[,3] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F1.len[,3] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)

  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:nrow(alph_gam), function(j) {
      coverage(normal.stm(F1[i,3][[1]], alph_gam$alpha[j], alph_gam$gamma[j])$conf.int)
    })
  })

  F1.cov.norm[,3] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F1.len.norm[,3] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)

  ## n = 500
  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:nrow(alph_gam), function(j) {
      coverage(empirical.stm(F1[i,4][[1]], alph_gam$alpha[j], alph_gam$gamma[j])$conf.int)
    })
  })

  F1.cov[,4] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F1.len[,4] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)

  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:nrow(alph_gam), function(j) {
      coverage(normal.stm(F1[i,4][[1]], alph_gam$alpha[j], alph_gam$gamma[j])$conf.int)
    })
  })

  F1.cov.norm[,4] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F1.len.norm[,4] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)

  rm(temp)
  
  ## F2 ##
  # n = 50
  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:nrow(alph_gam), function(j) {
      coverage(empirical.stm(F2[i,1][[1]], alph_gam$alpha[j], alph_gam$gamma[j])$conf.int)
    })
  })
  
  F2.cov[,1] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F2.len[,1] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)
  
  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:nrow(alph_gam), function(j) {
      coverage(normal.stm(F2[i,1][[1]], alph_gam$alpha[j], alph_gam$gamma[j])$conf.int)
    })
  })
  
  F2.cov.norm[,1] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F2.len.norm[,1] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)
  
  ## n = 100
  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:nrow(alph_gam), function(j) {
      coverage(empirical.stm(F2[i,2][[1]], alph_gam$alpha[j], alph_gam$gamma[j])$conf.int)
    })
  })
  
  F2.cov[,2] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F2.len[,2] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)
  
  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:nrow(alph_gam), function(j) {
      coverage(normal.stm(F2[i,2][[1]], alph_gam$alpha[j], alph_gam$gamma[j])$conf.int)
    })
  })
  
  F2.cov.norm[,2] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F2.len.norm[,2] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)
  
  ## n = 200
  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:nrow(alph_gam), function(j) {
      coverage(empirical.stm(F2[i,3][[1]], alph_gam$alpha[j], alph_gam$gamma[j])$conf.int)
    })
  })
  
  F2.cov[,3] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F2.len[,3] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)
  
  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:nrow(alph_gam), function(j) {
      coverage(normal.stm(F2[i,3][[1]], alph_gam$alpha[j], alph_gam$gamma[j])$conf.int)
    })
  })
  
  F2.cov.norm[,3] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F2.len.norm[,3] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)
  
  ## n = 500
  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:nrow(alph_gam), function(j) {
      coverage(empirical.stm(F2[i,4][[1]], alph_gam$alpha[j], alph_gam$gamma[j])$conf.int)
    })
  })
  
  F2.cov[,4] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F2.len[,4] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)
  
  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:nrow(alph_gam), function(j) {
      coverage(normal.stm(F2[i,4][[1]], alph_gam$alpha[j], alph_gam$gamma[j])$conf.int)
    })
  })
  
  F2.cov.norm[,4] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F2.len.norm[,4] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)
  
  rm(temp)
  
  
  stopCluster(clust)
})

F1.cov <- cbind(F1.cov, alph_gam)
F1.len <- cbind(F1.len, alph_gam)
F1.cov.norm <- cbind(F1.cov.norm, alph_gam)
F1.len.norm <- cbind(F1.len.norm, alph_gam)

F2.cov <- cbind(F2.cov, alph_gam)
F2.len <- cbind(F2.len, alph_gam)
F2.cov.norm <- cbind(F2.cov.norm, alph_gam)
F2.len.norm <- cbind(F2.len.norm, alph_gam)


## TRIMMED MEAN ##
F1.cov.tm <- matrix(nrow = length(alph), ncol = length(n))
F1.len.tm <- matrix(nrow = length(alph), ncol = length(n))
F1.cov.tm.norm <- matrix(nrow = length(alph), ncol = length(n))
F1.len.tm.norm <- matrix(nrow = length(alph), ncol = length(n))

F2.cov.tm <- matrix(nrow = length(alph), ncol = length(n))
F2.len.tm <- matrix(nrow = length(alph), ncol = length(n))
F2.cov.tm.norm <- matrix(nrow = length(alph), ncol = length(n))
F2.len.tm.norm <- matrix(nrow = length(alph), ncol = length(n))

system.time({
  clust <- makeCluster(4)
  clusterExport(clust, varlist = c('iter', 
                                   'empirical.tm', 
                                   'coverage', 
                                   'F1',
                                   'F2',
                                   'normal.tm',
                                   'alph',
                                   "trim.var",
                                   "winsor.var",
                                   "emp.conf.intervals.tmean",
                                   "emp.lik.tmean"))
  ## F1 ##
  ## n = 50
  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:length(alph), function(j) {
      coverage(empirical.tm(F1[i,1][[1]], alph[j])$conf.int)
    })
  })

  F1.cov.tm[,1] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F1.len.tm[,1] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)

  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:length(alph), function(j) {
      coverage(normal.tm(F1[i,1][[1]], alph[j])$conf.int)
    })
  })

  F1.cov.tm.norm[,1] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F1.len.tm.norm[,1] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)

  # n = 100
  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:length(alph), function(j) {
      coverage(empirical.tm(F1[i,2][[1]], alph[j])$conf.int)
    })
  })

  F1.cov.tm[,2] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F1.len.tm[,2] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)

  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:length(alph), function(j) {
      coverage(normal.tm(F1[i,2][[1]], alph[j])$conf.int)
    })
  })

  F1.cov.tm.norm[,2] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F1.len.tm.norm[,2] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)

  ## n = 200
  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:length(alph), function(j) {
      coverage(empirical.tm(F1[i,3][[1]], alph[j])$conf.int)
    })
  })

  F1.cov.tm[,3] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F1.len.tm[,3] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)

  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:length(alph), function(j) {
      coverage(normal.tm(F1[i,4][[1]], alph[j])$conf.int)
    })
  })

  F1.cov.tm.norm[,3] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F1.len.tm.norm[,3] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)

  ## n = 500
  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:length(alph), function(j) {
      coverage(empirical.tm(F1[i,4][[1]], alph[j])$conf.int)
    })
  })

  F1.cov.tm[,4] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F1.len.tm[,4] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)

  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:length(alph), function(j) {
      coverage(normal.tm(F1[i,4][[1]], alph[j])$conf.int)
    })
  })

  F1.cov.tm.norm[,4] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F1.len.tm.norm[,4] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)

  rm(temp)
  
  ## F2 ##
  ## n = 50
  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:length(alph), function(j) {
      coverage(empirical.tm(F2[i,1][[1]], alph[j])$conf.int)
    })
  })
  
  F2.cov.tm[,1] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F2.len.tm[,1] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)

  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:length(alph), function(j) {
      coverage(normal.tm(F2[i,1][[1]], alph[j])$conf.int)
    })
  })

  F2.cov.tm.norm[,1] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F2.len.tm.norm[,1] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)
  
  # n = 100
  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:length(alph), function(j) {
      coverage(empirical.tm(F2[i,2][[1]], alph[j])$conf.int)
    })
  })
  
  F2.cov.tm[,2] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F2.len.tm[,2] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)

  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:length(alph), function(j) {
      coverage(normal.tm(F2[i,2][[1]], alph[j])$conf.int)
    })
  })

  F2.cov.tm.norm[,2] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F2.len.tm.norm[,2] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)

  ## n = 200
  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:length(alph), function(j) {
      coverage(empirical.tm(F2[i,3][[1]], alph[j])$conf.int)
    })
  })
  
  F2.cov.tm[,3] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F2.len.tm[,3] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)
  
  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:length(alph), function(j) {
      coverage(normal.tm(F2[i,4][[1]], alph[j])$conf.int)
    })
  })

  F2.cov.tm.norm[,3] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F2.len.tm.norm[,3] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)
  # 
  ## n = 500
  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:length(alph), function(j) {
      coverage(empirical.tm(F2[i,4][[1]], alph[j])$conf.int)
    })
  })
  
  F2.cov.tm[,4] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F2.len.tm[,4] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)

  temp <- parSapply(clust, 1:iter, function(i){
    lapply(1:length(alph), function(j) {
      coverage(normal.tm(F2[i,4][[1]], alph[j])$conf.int)
    })
  })

  F2.cov.tm.norm[,4] <- colMeans(t(matrix(sapply(temp, `[[`, 1), nrow = nrow(temp))), na.rm = T)
  F2.len.tm.norm[,4] <- colMeans(t(matrix(sapply(temp, `[[`, 2), nrow = nrow(temp))), na.rm = T)

  rm(temp)
  
  
  stopCluster(clust)
})

F1.cov.tm <- cbind(F1.cov.tm, alph)
F1.len.tm <- cbind(F1.len.tm, alph)
F1.cov.tm.norm <- cbind(F1.cov.tm.norm, alph)
F1.len.tm.norm <- cbind(F1.len.tm.norm, alph)

F2.cov.tm <- cbind(F2.cov.tm, alph)
F2.len.tm <- cbind(F2.len.tm, alph)
F2.cov.tm.norm <- cbind(F2.cov.tm.norm, alph)
F2.len.tm.norm <- cbind(F2.len.tm.norm, alph)




## HUBER ##
F1.cov.hb <- matrix(nrow = 1, ncol = length(n))
F1.len.hb <- matrix(nrow = 1, ncol = length(n))
F1.cov.hb.norm <- matrix(nrow = 1, ncol = length(n))
F1.len.hb.norm <- matrix(nrow = 1, ncol = length(n))

F2.cov.hb <- matrix(nrow = 1, ncol = length(n))
F2.len.hb <- matrix(nrow = 1, ncol = length(n))
F2.cov.hb.norm <- matrix(nrow = 1, ncol = length(n))
F2.len.hb.norm <- matrix(nrow = 1, ncol = length(n))

system.time({
  clust <- makeCluster(4)
  clusterExport(clust, varlist = c('iter', 
                                   'empirical.huber', 
                                   'var.h',
                                   'normal.huber',
                                   'huber',
                                   'target',
                                   'coverage', 
                                   'F1',
                                   'F2',
                                   'el.test',
                                   'confint'))
  ## F1 ##
  print('F1')
  print('n = 50')
  temp <- parSapply(clust, 1:iter, function(i){
      coverage(empirical.huber(F1[i,1][[1]])$conf.int)
  })
  
  
  F1.cov.hb[,1] <-  mean(unlist(temp[1,]), na.rm = T)
  F1.len.hb[,1] <-  mean(unlist(temp[2,]), na.rm = T)

  temp <- parSapply(clust, 1:iter, function(i){
    coverage(normal.huber(F1[i,1][[1]], B = 2000)$conf.int)
  })

  F1.cov.hb.norm[,1] <-  mean(unlist(temp[1,]), na.rm = T)
  F1.len.hb.norm[,1] <-  mean(unlist(temp[2,]), na.rm = T)

  print('n = 100')
  temp <- parSapply(clust, 1:iter, function(i){
    coverage(empirical.huber(F1[i,2][[1]])$conf.int)
  })
  
  F1.cov.hb[,2] <-  mean(unlist(temp[1,]), na.rm = T)
  F1.len.hb[,2] <-  mean(unlist(temp[2,]), na.rm = T)

  temp <- parSapply(clust, 1:iter, function(i){
    coverage(normal.huber(F1[i,2][[1]], B = 2000)$conf.int)
  })

  F1.cov.hb.norm[,2] <-  mean(unlist(temp[1,]), na.rm = T)
  F1.len.hb.norm[,2] <-  mean(unlist(temp[2,]), na.rm = T)



  print('n = 200')
  temp <- parSapply(clust, 1:iter, function(i){
    coverage(empirical.huber(F1[i,3][[1]])$conf.int)
  })
  
  F1.cov.hb[,3] <-  mean(unlist(temp[1,]), na.rm = T)
  F1.len.hb[,3] <-  mean(unlist(temp[2,]), na.rm = T)

  temp <- parSapply(clust, 1:iter, function(i){
    coverage(normal.huber(F1[i,3][[1]], B = 2000)$conf.int)
  })

  F1.cov.hb.norm[,3] <-  mean(unlist(temp[1,]), na.rm = T)
  F1.len.hb.norm[,3] <-  mean(unlist(temp[2,]), na.rm = T)


  print('n = 500')
  temp <- parSapply(clust, 1:iter, function(i){
    coverage(empirical.huber(F1[i,4][[1]])$conf.int)
  })

  F1.cov.hb[,4] <-  mean(unlist(temp[1,]), na.rm = T)
  F1.len.hb[,4] <-  mean(unlist(temp[2,]), na.rm = T)

  temp <- parSapply(clust, 1:iter, function(i){
    coverage(normal.huber(F1[i,4][[1]], B = 2000)$conf.int)
  })

  F1.cov.hb.norm[,4] <-  mean(unlist(temp[1,]), na.rm = T)
  F1.len.hb.norm[,4] <-  mean(unlist(temp[2,]), na.rm = T)

  rm(temp)

  ## F2 ##
  print('F2')
  print('n = 50')
  temp <- parSapply(clust, 1:iter, function(i){
    coverage(empirical.huber(F2[i,1][[1]])$conf.int)
  })


  F2.cov.hb[,1] <-  mean(unlist(temp[1,]), na.rm = T)
  F2.len.hb[,1] <-  mean(unlist(temp[2,]), na.rm = T)

  temp <- parSapply(clust, 1:iter, function(i){
    coverage(normal.huber(F2[i,1][[1]], B = 2000)$conf.int)
  })

  F2.cov.hb.norm[,1] <-  mean(unlist(temp[1,]), na.rm = T)
  F2.len.hb.norm[,1] <-  mean(unlist(temp[2,]), na.rm = T)
  
  print('n = 100')
  temp <- parSapply(clust, 1:iter, function(i){
    coverage(empirical.huber(F2[i,2][[1]])$conf.int)
  })

  F2.cov.hb[,2] <-  mean(unlist(temp[1,]), na.rm = T)
  F2.len.hb[,2] <-  mean(unlist(temp[2,]), na.rm = T)

  temp <- parSapply(clust, 1:iter, function(i){
    coverage(normal.huber(F2[i,2][[1]], B = 2000)$conf.int)
  })

  F2.cov.hb.norm[,2] <-  mean(unlist(temp[1,]), na.rm = T)
  F2.len.hb.norm[,2] <-  mean(unlist(temp[2,]), na.rm = T)
  
  
  
  print('n = 200')
  temp <- parSapply(clust, 1:iter, function(i){
    coverage(empirical.huber(F2[i,3][[1]])$conf.int)
  })
  
  F2.cov.hb[,3] <-  mean(unlist(temp[1,]), na.rm = T)
  F2.len.hb[,3] <-  mean(unlist(temp[2,]), na.rm = T)

  temp <- parSapply(clust, 1:iter, function(i){
    coverage(normal.huber(F2[i,3][[1]], B = 2000)$conf.int)
  })

  F2.cov.hb.norm[,3] <-  mean(unlist(temp[1,]), na.rm = T)
  F2.len.hb.norm[,3] <-  mean(unlist(temp[2,]), na.rm = T)
  
  
  print('n = 500')
  temp <- parSapply(clust, 1:iter, function(i){
    coverage(empirical.huber(F2[i,4][[1]])$conf.int)
  })

  F2.cov.hb[,4] <-  mean(unlist(temp[1,]), na.rm = T)
  F2.len.hb[,4] <-  mean(unlist(temp[2,]), na.rm = T)
  
  temp <- parSapply(clust, 1:iter, function(i){
    coverage(normal.huber(F2[i,4][[1]], B = 2000)$conf.int)
  })
  
  F2.cov.hb.norm[,4] <-  mean(unlist(temp[1,]), na.rm = T)
  F2.len.hb.norm[,4] <-  mean(unlist(temp[2,]), na.rm = T)
  
  rm(temp)
  
  stopCluster(clust)
})


rbind(F1.cov %>%
        mutate(method = 'STM.EL') %>%
        select(n1 = 1, n2 = 2, n3 = 3, n4 = 4, alpha, gamma, method),
F1.cov.norm %>%
        mutate(method = 'STM.norm') %>%
        select(n1 = 1, n2 = 2, n3 = 3, n4 = 4, alpha, gamma, method),
as.data.frame(F1.cov.hb) %>%
  mutate(alpha = NA,
         gamma = NA,
         method = 'huber.EL') %>%
  select(n1 = V1, n2 = V2, n3 = V3, n4 = V4, alpha, gamma, method),
as.data.frame(F1.cov.hb.norm) %>%
  mutate(alpha = NA,
         gamma = NA,
         method = 'huber.norm') %>%
  select(n1 = V1, n2 = V2, n3 = V3, n4 = V4, alpha, gamma, method),
as.data.frame(F1.cov.tm) %>%
  mutate(gamma = NA,
         method = 'TM.EL') %>%
  select(n1 = V1, n2 = V2, n3 = V3, n4 = V4, alpha = alph, gamma, method),
as.data.frame(F1.cov.tm.norm) %>%
  mutate(gamma = NA,
         method = 'TM.norm') %>%
  select(n1 = V1, n2 = V2, n3 = V3, n4 = V4, alpha = alph, gamma, method)) %>%
  openxlsx::write.xlsx('tab2.xlsx')

rbind(F2.cov %>%
        mutate(method = 'STM.EL') %>%
        select(n1 = 1, n2 = 2, n3 = 3, n4 = 4, alpha, gamma, method),
      F2.cov.norm %>%
        mutate(method = 'STM.norm') %>%
        select(n1 = 1, n2 = 2, n3 = 3, n4 = 4, alpha, gamma, method),
      as.data.frame(F2.cov.hb) %>%
        mutate(alpha = NA,
               gamma = NA,
               method = 'huber.EL') %>%
        select(n1 = V1, n2 = V2, n3 = V3, n4 = V4, alpha, gamma, method),
      as.data.frame(F2.cov.hb.norm) %>%
        mutate(alpha = NA,
               gamma = NA,
               method = 'huber.norm') %>%
        select(n1 = V1, n2 = V2, n3 = V3, n4 = V4, alpha, gamma, method),
      as.data.frame(F2.cov.tm) %>%
        mutate(gamma = NA,
               method = 'TM.EL') %>%
        select(n1 = V1, n2 = V2, n3 = V3, n4 = V4, alpha = alph, gamma, method),
      as.data.frame(F2.cov.tm.norm) %>%
        mutate(gamma = NA,
               method = 'TM.norm') %>%
        select(n1 = V1, n2 = V2, n3 = V3, n4 = V4, alpha = alph, gamma, method)) %>%
  openxlsx::write.xlsx('tab3.xlsx')



rbind(F1.len %>%
        mutate(method = 'STM.EL') %>%
        select(n1 = 1, n2 = 2, n3 = 3, n4 = 4, alpha, gamma, method),
      F1.len.norm %>%
        mutate(method = 'STM.norm') %>%
        select(n1 = 1, n2 = 2, n3 = 3, n4 = 4, alpha, gamma, method),
      as.data.frame(F1.len.hb) %>%
        mutate(alpha = NA,
               gamma = NA,
               method = 'huber.EL') %>%
        select(n1 = V1, n2 = V2, n3 = V3, n4 = V4, alpha, gamma, method),
      as.data.frame(F1.len.hb.norm) %>%
        mutate(alpha = NA,
               gamma = NA,
               method = 'huber.norm') %>%
        select(n1 = V1, n2 = V2, n3 = V3, n4 = V4, alpha, gamma, method),
      as.data.frame(F1.len.tm) %>%
        mutate(gamma = NA,
               method = 'TM.EL') %>%
        select(n1 = V1, n2 = V2, n3 = V3, n4 = V4, alpha = alph, gamma, method),
      as.data.frame(F1.len.tm.norm) %>%
        mutate(gamma = NA,
               method = 'TM.norm') %>%
        select(n1 = V1, n2 = V2, n3 = V3, n4 = V4, alpha = alph, gamma, method)) %>%
  openxlsx::write.xlsx('tab2len.xlsx')

rbind(F2.len %>%
        mutate(method = 'STM.EL') %>%
        select(n1 = 1, n2 = 2, n3 = 3, n4 = 4, alpha, gamma, method),
      F2.len.norm %>%
        mutate(method = 'STM.norm') %>%
        select(n1 = 1, n2 = 2, n3 = 3, n4 = 4, alpha, gamma, method),
      as.data.frame(F2.len.hb) %>%
        mutate(alpha = NA,
               gamma = NA,
               method = 'huber.EL') %>%
        select(n1 = V1, n2 = V2, n3 = V3, n4 = V4, alpha, gamma, method),
      as.data.frame(F2.len.hb.norm) %>%
        mutate(alpha = NA,
               gamma = NA,
               method = 'huber.norm') %>%
        select(n1 = V1, n2 = V2, n3 = V3, n4 = V4, alpha, gamma, method),
      as.data.frame(F2.len.tm) %>%
        mutate(gamma = NA,
               method = 'TM.EL') %>%
        select(n1 = V1, n2 = V2, n3 = V3, n4 = V4, alpha = alph, gamma, method),
      as.data.frame(F2.len.tm.norm) %>%
        mutate(gamma = NA,
               method = 'TM.norm') %>%
        select(n1 = V1, n2 = V2, n3 = V3, n4 = V4, alpha = alph, gamma, method)) %>%
  openxlsx::write.xlsx('tab3len.xlsx')


