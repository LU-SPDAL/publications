library(tidyr)
library(MASS)
library(psych)
library(melt)
library(emplik)

F.cn <- function(x){
  0.8 * pnorm(x)+
    0.2 * pnorm(x, 0, 25)
}
F.cn2 <- function(x){
  0.1 * pnorm(x + 10) + 
    0.8 * pnorm(x) + 
    0.1 * pnorm(x - 10)
}


gen.N <- function(){
  y <- runif(1)
  return(uniroot(function(x) {F.cn(x) - y}, c(-1000,1000))$root)
}
gen.N2 <- function(){
  y <- runif(1)
  return(uniroot(function(x) {F.cn2(x) - y}, c(-1000,1000))$root)
}


### ONE SAMPLE ----

J_fun      <- function(u, alpha, gamma) {
  w <- 0
  if (u < alpha) {w <- 0}
  else if (u <= gamma) {w <- (u-alpha)/(gamma-alpha)}
  else if (u <= 1 - gamma) {w <- 1}
  else if (u <= 1 - alpha) {w <- (1-alpha-u)/(gamma-alpha)}
  else {w <- 0}
  w
}

ST_mean    <- function(data, alpha, gamma) {
  n <- length(data)
  weights <- sapply(seq(1:n)/(n+1), J_fun, alpha, gamma)
  m <- sum(weights!=0)
  w.norm <- weights*m/sum(weights)
  stmean <- sum(sort(data)*w.norm)/m
  stmean
}

stmeanvar.asym  <- function(data, alpha, gamma) {
  
  # data <- x
  data_sort <- sort(data)
  n <- length(data)
  r  <- floor(alpha*n)
  m  <- floor(gamma*n) + 1
  
  
  weights <- sapply(seq(1:n)/(n+1), J_fun, alpha, gamma)
  const <- n/sum(weights)
  
  if (alpha >= gamma) NA
  else {
    
    ## safe index sets
    idx_rm <- if (m == 0) integer(0) else if (r == 0) 1:m else (r+1):m
    idx_nm_nr <- if (m == 0) integer(0) else if (r == 0) (n-m+1):n else (n-m+1):(n-r)
    
    ## safe order stats
    Xm1 <- if (m == 0) data_sort[1] else data_sort[m+1]
    Xnm <- if (m == 0) data_sort[n] else data_sort[n-m]
    
    c <- if (m == 0) {
      0
    } else {
      1/(m-r)*(
        (m + m*r/n - r - m^2/n)*Xm1 -
          (1 + r/n)*sum(data_sort[idx_rm]) +
          2/n*sum(data_sort[idx_rm]*idx_rm) +
          (2-r/n)*sum(data_sort[idx_nm_nr]) +
          (r*m - m^2)/n*Xnm -
          2/n*sum(data_sort[idx_nm_nr]*idx_nm_nr)
      ) +
        1/n*(sum(data_sort[(m+1):(n-m)]) + m*Xnm + (m-n)*Xm1)
    }
    
    infl <- numeric(n)
    
    for (i in 1:n){
      
      x <- data_sort[i]
      
      left_r <- if (r == 0) -Inf else data_sort[r]
      mid_m  <- if (m == 0) -Inf else data_sort[m]
      right_m <- if (m == 0) Inf else data_sort[n-m]
      right_r <- if (r == 0) Inf else data_sort[n-r]
      
      if (x <= left_r){
        
        infl[i] <- -c
        
      } else if (x > left_r && x <= mid_m){
        
        infl[i] <- if (m == 0) {
          x - c
        } else {
          1/(m-r)*((i-r)*x - sum(data_sort[if (r==0) 1:i else (r+1):i])) - c
        }
        
      } else if (x > mid_m && x <= right_m) {
        
        infl[i] <- if (m == 0) {
          x - c
        } else {
          1/(m-r)*((m-r)*Xm1 - sum(data_sort[idx_rm])) +
            x - Xm1 - c
        }
        
      } else if (x > right_m && x <= right_r) {
        
        infl[i] <- if (m == 0) {
          x - c
        } else {
          1/(m-r)*((m-r)*Xm1 - sum(data_sort[idx_rm])) +
            Xnm - Xm1 +
            1/(m-r)*((n-r)*x + (r-m)*Xnm -
                       i*x + sum(data_sort[(n-m+1):i])) - c
        }
        
      } else {
        
        infl[i] <- if (m == 0) {
          x - c
        } else {
          1/(m-r)*((m-r)*Xm1 - sum(data_sort[idx_rm])) +
            Xnm - Xm1 +
            1/(m-r)*((r-m)*Xnm + sum(data_sort[idx_nm_nr])) - c
        }
      }
    }
    
    sum(infl^2)*const^2/n^2
  }
}

normal.stm <- function(x, alpha, gamma){
  st <- ST_mean(x, alpha, gamma)
  var <- stmeanvar.asym(x, alpha, gamma)
  
  list('conf.int' = c(st - qnorm(0.975)*sqrt(var),
          st + qnorm(0.975)*sqrt(var)))
}

emp.lik.stmean <- function(mu, data, alpha, gamma) {
  
  # data <- F1[26,4][[1]]
  # alpha <- 0.05
  # gamma <- 0.4
  # mu <- 0
  
  data  <- sort(data)
  
  n <- length(data)
  r <- floor(n*alpha)
  
  weights <- sapply(seq(1:n)/(n+1), J_fun, alpha, gamma)
  weights <- weights/sum(weights)
  
  m   <- n-2*r
  Wni <- (data-mu)[(r+1):(n-r)]
  
  
  lam.fun <- function(lambda)
  {
    sum(weights[(r+1):(n-r)]*Wni/(1+lambda*Wni))
  }
  
  # lam.fun <- Vectorize(lam.fun)
  
  
  gal1 <- 1/(-max(Wni)) + 0.0001
  gal2 <- 1/(-min(Wni)) - 0.0001
  
  root <- uniroot(function(lam) lam.fun(lam), c(gal1, gal2))$root
  
  if (gal1 >= gal2) {
    stat <- 100
  } else {
    
    root <- uniroot(function(lam) lam.fun(lam), c(gal1, gal2))$root
    
    sigma1sq <- sum((weights*(data - ST_mean(data, alpha, gamma))^2))
    sigma2sq <- stmeanvar.asym(data, alpha, gamma)*n
    a <- sigma1sq/sigma2sq/(1-2*alpha)
    
    stat <- 2*a*sum(weights[(r+1):(n-r)]*m*log(1+root*Wni))
  }
  stat
}

emp.conf.intervals.stmean <- function (step = 0.01, initStep = 0, level, data, alpha, gamma) {
  
  mu <- ST_mean(data, alpha, gamma)
  value <- 0
  step1 <- step
  Lbeta <- mu - initStep
  while (value < level) {
    Lbeta <- Lbeta - step1
    value <- emp.lik.stmean(Lbeta, data, alpha, gamma)
  }
  Lbeta0 <- Lbeta
  Lbeta1 <- Lbeta + step1
  
  tempfun <- function(beta1) {
    return(level - emp.lik.stmean(beta1, data, alpha, gamma))
  }
  
  if (round(abs(Lbeta0-mu),2)<=0.01) {
    Lbeta <- mu
  } else {
    temp1 <- uniroot(tempfun, lower = Lbeta0, upper = Lbeta1)
    Lbeta <- temp1$root
  }
  
  value <- 0
  Ubeta <- mu + initStep
  while (value < level) {
    Ubeta <- Ubeta + step
    value <- emp.lik.stmean(Ubeta, data, alpha, gamma)
  }
  Ubeta0 <- Ubeta
  Ubeta1 <- Ubeta - step
  
  if (round(abs(Ubeta0-mu),2)<=0.01) {
    Ubeta <- mu
  } else {
    temp2 <- uniroot(tempfun, lower = Ubeta1, upper = Ubeta0)
    Ubeta <- temp2$root
  }
  c(Lbeta, Ubeta)
}

empirical.stm <- function(data, alpha, gamma, conf.level = 0.95) {
  
  level <- qchisq(conf.level, df=1)
  estimate   <- ST_mean(data = data, alpha = alpha, gamma = gamma)
  conf.int   <- tryCatch(
    expr = {emp.conf.intervals.stmean(data = data, alpha = alpha, gamma = gamma, level = level)
    }
    ,
    error = function(e)
      c(NA, NA)
  )
  
  list(estimate = estimate, conf.int = conf.int)
}




trim.var <- function(x, alpha){
  n <- length(x)
  winsor.var(x, trim = alpha, na.rm = TRUE)  / ((1 - 2*alpha)^2 * n)
}

normal.tm <- function(x, alpha){
  tm <- mean(x, trim = alpha)
  var <- trim.var(x, alpha)
  
  list('conf.int' = c(tm - qnorm(0.975)*sqrt(var),
    tm + qnorm(0.975)*sqrt(var)))
}

emp.lik.tmean<-function(mu,dati,alpha,var.tmean){
  
  izlase<-sort(dati)
  n<-length(dati)
  r<-floor(n*alpha)
  Wni<-(izlase-mu)[(r+1):(n-r)]
  m<-n-2*r
  
  lam.fun<-function(lambda){
    m*sum(Wni/(1+lambda*Wni))}
  
  lam.fun<-Vectorize(lam.fun)
  
  gal1<-1/(-max(Wni))+0.0001
  gal2<-1/(-min(Wni))-0.0001
  
  if(gal1>=gal2){
    stat<-100
  }else{
    sigma1sq<-1/m*sum((izlase[(r+1):(n-r)]-mean(dati,alpha))^2)
    #sigma2sq<-trimvar(izlase,alpha)*n
    sigma2sq<-var.tmean*n
    a<-sigma1sq/sigma2sq/(1-2*alpha)
    
    sakne<-uniroot(function(lam)lam.fun(lam),c(gal1,gal2))$root
    stat<-2*a*sum(log(1+sakne*Wni))
  }
  stat
}


emp.conf.intervals.tmean<-function(step=0.01,initStep=0,level= 3.84,dati,alpha){
  mu <- mean(dati, alpha)
  var.tmean <- trim.var(dati, alpha)
  value<-0
  step1<-step
  Lbeta<-mu-initStep
  while(value<level){
    Lbeta<-Lbeta-step1
    value<-emp.lik.tmean(Lbeta,dati,alpha,var.tmean)
  }
  Lbeta0<-Lbeta
  Lbeta1<-Lbeta+step1
  
  tempfun<-function(beta1){
    return(level-emp.lik.tmean(beta1,dati,alpha,var.tmean))}
  
  if(round(abs(Lbeta0-mu),2)<=0.01){
    Lbeta<-mu
  }else{
    temp1<-uniroot(tempfun,lower=Lbeta0,upper=Lbeta1)
    Lbeta<-temp1$root
  }
  
  value<-0
  Ubeta<-mu+initStep
  while(value<level){
    Ubeta<-Ubeta+step
    value<-emp.lik.tmean(Ubeta,dati,alpha,var.tmean)
  }
  Ubeta0<-Ubeta
  Ubeta1<-Ubeta-step
  
  if(round(abs(Ubeta0-mu),2)<=0.01){
    Ubeta<-mu
  }else{
    temp2<-uniroot(tempfun,lower=Ubeta1,upper=Ubeta0)
    Ubeta<-temp2$root
  }
  
  int_length<-Ubeta-Lbeta
  
  indic<-prod(c(Ubeta,Lbeta)-0)
  c(Lbeta,Ubeta)
}

empirical.tm <- function(data, alpha,  conf.level = 0.95) {
  
  level <- qchisq(conf.level, df=1)
  estimate   <- mean(data, alpha)
  conf.int   <- emp.conf.intervals.tmean(dati = data, alpha = alpha, level = level)
  
  list(estimate = estimate, conf.int = conf.int)
}

#### HUBER

var.h <- function(x, k = 1.35, B){
  var(vapply(1:B, function(i) huber(sample(x, replace = TRUE), k = k)$mu, numeric(1)))
}

normal.huber <- function(x, k = 1.35, mu = 0, B = 500){
  h <- huber(x, k)$mu
  v <- var.h(x, k, B)
  z <- qnorm(0.975)
  ci <- c(h - mu - sqrt(v) * z, h - mu + sqrt(v) * z)
  return(list('conf.int' = ci))
}

target <- function(x, mu, k, crit) {
  # mu <- 0
  t <- x - mu
  psi <- ifelse(abs(t) <= k, t, k * sign(t))
  return(el.test(psi, mu = 0)$`-2LLR` - crit)
}

empirical.huber <- function(x, k = 1.35, conf.level = 0.95) {
  crit <- qchisq(conf.level, df = 1)
  

  lower <- uniroot(function(m) target(x, m, k, crit), interval = c(min(x), huber(x, k)$mu))$root
  upper <- uniroot(function(m) target(x, m, k, crit), interval = c(huber(x,k)$mu, max(x)))$root
  
  return(list('conf.int' = c(lower, upper)))
}
