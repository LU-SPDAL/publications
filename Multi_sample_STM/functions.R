library(tidyr)
library(WRS2)
library(nleqslv)

coverage <- function(mu, lb, ub){
  if(is.na(lb)|is.na(ub)){
    NA
  }else if(mu > lb & mu < ub){
    1
  }else{
    0
  }
}

### contaminated cdf ----
F.cn <- function(x){
  0.8 * pnorm(x)+
    0.2 * pnorm(x, 0, 25)
}
F.cn2 <- function(x){
  0.1 * pnorm(x + 10) + 
    0.8 * pnorm(x) + 
    0.1 * pnorm(x - 10)
}
F.cnu <- function(x){
  0.9 * pnorm(x)+
    0.1 * punif(x, -5, 10)
}
F.cnu2 <- function(x){
  0.8 * pnorm(x)+
    0.1 * punif(x, 3, 10)+
    0.1 * punif(x, -10, -3)
}
F.cnu3 <- function(x){
  0.8 * pnorm(x)+
    0.1 * punif(x, 2, 15)+
    0.1 * punif(x, -15, -2)
}

gen.N <- function(){
  y <- runif(1)
  return(uniroot(function(x) {F.cn(x) - y}, c(-1000,1000))$root)
}
gen.N2 <- function(){
  y <- runif(1)
  return(uniroot(function(x) {F.cn2(x) - y}, c(-1000,1000))$root)
}
gen.NU <- function(){
  y <- runif(1)
  return(uniroot(function(x) {F.cnu(x) - y}, c(-1000,1000))$root)
}
gen.NU2 <- function(){
  y <- runif(1)
  return(uniroot(function(x) {F.cnu2(x) - y}, c(-1000,1000))$root)
}
gen.NU3 <- function(){
  y <- runif(1)
  return(uniroot(function(x) {F.cnu3(x) - y}, c(-1000,1000))$root)
}

### ONE SAMPLE ----
win_data <- function(data, alpha) {
  n <- length(data)
  r <- floor(n*alpha) + 1
  s <- n-floor(n*alpha)
  
  data.ord <- sort(data)
  data_win    <- c(rep(data.ord[r], r-1), data.ord[r:s], rep(data.ord[s], r-1))
  data_win
}

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
  
  data_sort <- sort(data)
  n <- length(data)
  r  <- floor(alpha*n)
  m  <- floor(gamma*n)
  
  weights <- sapply(seq(1:n)/(n+1), J_fun, alpha, gamma)
  const <- n/sum(weights)
  
  if (alpha >= gamma) NA
  else {
    
    c <- 1/(m-r)*((m + m*r/n - r - m^2/n)*data_sort[m+1] - (1 + r/n)*sum(data_sort[(r+1):m]) + 
                    2/n*sum(data_sort[(r+1):m]*((r+1):m)) + (2-r/n)*sum(data_sort[(n-m+1):(n-r)]) + 
                    (r*m - m^2)/n*data_sort[n-m] - 2/n*sum(data_sort[(n-m+1):(n-r)]*((n-m+1):(n-r)))) +
      1/n*(sum(data_sort[(m+1):(n-m)]) + m*data_sort[n-m] + (m-n)*data_sort[m+1])
    
    infl <- c()
    
    for (i in 1:length(data)){
      
      x <- data_sort[i]
      
      if (data_sort[i] <= data_sort[r]){
        
        infl[i] <- -c
        
      } else if ((data_sort[i] > data_sort[r]) & (data_sort[i] <= data_sort[m])){
        
        infl[i] <- 1/(m-r)*((i-r)*x - sum(data_sort[(r+1):i])) - c
        
      } else if ((data_sort[i] > data_sort[m]) & (data_sort[i] <= data_sort[n-m])) {
        
        infl[i] <- 1/(m-r)*((m-r)*data_sort[m+1] - sum(data_sort[(r+1):m])) + x - data_sort[m+1] -c
        
      } else if ((data_sort[i] > data_sort[n-m]) & (data_sort[i] <= data_sort[n-r])) {
        
        infl[i] <- 1/(m-r)*((m-r)*data_sort[m+1] - sum(data_sort[(r+1):m])) + data_sort[n-m] - data_sort[m+1] +
          1/(m-r)*((n-r)*x + (r-m)*data_sort[n-m] - i*x + sum(data_sort[(n-m+1):i])) - c
        
      } else if(data_sort[i] > data_sort[n-r]){
        
        infl[i] <- 1/(m-r)*((m-r)*data_sort[m+1] - sum(data_sort[(r+1):m])) + data_sort[n-m] - data_sort[m+1] +
          1/(m-r)*((r-m)*data_sort[n-m] + sum(data_sort[(n-m+1):(n-r)])) - c
      }
    }
    
    sum(infl^2)*const^2/n^2
  }
}
# ST_mean(rnorm(100), 0.05, 0.1)
emp.lik.stmean <- function(mu, data, alpha, gamma) {
  
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
  
  lam.fun <- Vectorize(lam.fun)
  
  
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
  conf.int   <- emp.conf.intervals.stmean(data = data, alpha = alpha, gamma = gamma, level = level)
  
  list(estimate = estimate, conf.int = conf.int)
}



win_izlase<-function(izlase,alpha){
  n<-length(izlase)
  r<-floor(n*alpha)+1
  s<-n-floor(n*alpha)
  
  izlase.ord<-sort(izlase)
  izl_win<-c(rep(izlase.ord[r],r-1),izlase.ord[r:s],rep(izlase.ord[s],r-1))
  izl_win
}

trim.var <- function(izlase, alpha){
  n <- length(izlase)
  win.izlase <- win_izlase(izlase, alpha)
  trim.sigma <- 1/(n-1)/n/(1-2*alpha)^2 * sum((win.izlase - mean(win.izlase))^2)
  trim.sigma
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
  var.tmean <- length(dati)*trim.var(dati, alpha)
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
  c(indic,int_length)
}

empirical.tm <- function(data, alpha,  conf.level = 0.95) {
  
  level <- qchisq(conf.level, df=1)
  estimate   <- mean(data, alpha)
  conf.int   <- emp.conf.intervals.tmean(dati = data, alpha = alpha, level = level)
  
  list(estimate = estimate, conf.int = conf.int)
}

#### TWO SAMPLE ----

yuen.tm <- function (formula, data, tr = 0.2, ...) 
{
  if (missing(data)) {
    mf <- model.frame(formula)
  }
  else {
    mf <- model.frame(formula, data)
  }
  cl <- match.call()
  xy <- split(model.extract(mf, "response"), mf[, 2])
  faclevels <- names(xy)
  x <- xy[[1]]
  y <- xy[[2]]
  if (tr == 0.5) 
    warning("Comparing medians should not be done with this function!")
  alpha <- 0.05
  if (is.null(y)) {
    if (is.matrix(x) || is.data.frame(x)) {
      y = x[, 2]
      x = x[, 1]
    }
    if (is.list(x)) {
      y = x[[2]]
      x = x[[1]]
    }
  }
  if (tr > 0.25) 
    print("Warning: with tr>.25 type I error control might be poor")
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  h1 <- length(x) - 2 * floor(tr * length(x))
  h2 <- length(y) - 2 * floor(tr * length(y))
  q1 <- (length(x) - 1) * winvar(x, tr)/(h1 * (h1 - 1))
  q2 <- (length(y) - 1) * winvar(y, tr)/(h2 * (h2 - 1))
  df <- (q1 + q2)^2/((q1^2/(h1 - 1)) + (q2^2/(h2 - 1)))
  crit <- qt(1 - alpha/2, df)
  dif <- mean(x, tr) - mean(y, tr)
  low <- dif - crit * sqrt(q1 + q2)
  up <- dif + crit * sqrt(q1 + q2)
  test <- dif/sqrt(q1 + q2)
  yuen <- 2 * (1 - pt(test, df))
  result <- list(test = test, conf.int = c(low, up), p.value = yuen, 
                 df = df, diff = dif, call = cl)
  class(result) <- "yuen"
  result
}

yuen.stm <- function(x, y, alpha = 0.1, gamma = 0.1){
  
  n.x <- length(x)
  n.y <- length(y)
  
  stm.x <- ST_mean(x, alpha, gamma)
  stm.y <- ST_mean(y, alpha, gamma)
  
  var.x <- stmeanvar.asym(x, alpha, gamma)
  var.y <- stmeanvar.asym(y, alpha, gamma)
  
  
  df1 <- sum(sapply(seq(1:n.x)/(n.x+1), J_fun, alpha, gamma))
  df2 <- sum(sapply(seq(1:n.y)/(n.y+1), J_fun, alpha, gamma))
  
  delta <- stm.x - stm.y
  se <- sqrt(var.x + var.y)
  
  t <- (delta)/se
  df <- (var.x+var.y)^2/
    (var.x^2/df1+
       var.y^2/df2)
  
  
  ci <- c(delta - qt(p = 0.975, df = df)*se, delta + qt(p = 0.975, df = df)*se)
  return(list('st' = t,
              'df' = df,
              'conf.int' = ci,
              'estimate' = -1*delta))
}


f.w <- function(x, mu, delta){
  x - delta - mu
}

f.lam <- function(x, w,  mu, delta, lam){
  sum(w * f.w(x, mu, delta) / (1 + lam * f.w(x, mu, delta)))
}


f.pq <- function(x, xx, yy, delta, w1, w2, m1, m2, alpha, gamma){
  
  init <- c(0, 0, ST_mean(x, alpha, gamma)-delta) # starting guess
  
  get <- nleqslv(init, get.est,
                 xsamp = xx, 
                 ysamp = yy,
                 w1 = w1,
                 w2 = w2, 
                 m1 = m1,
                 m2 = m2, 
                 delta = delta,
                 control = list('stepmax' = 0.1))$x
  
  p <- sum(w1/ (1 + get[1] * f.w(xx, get[3], 0)))
  q <- sum(w2/ (1 + get[2] * f.w(yy, get[3], delta)))
  return(c(p, q))
}



get.est <- function(xsamp, ysamp, w1, w2, delta, m1, m2, l){
  ll <- numeric(3)
  ll[1] <- f.lam(xsamp, w1, l[3], 0, l[1])
  ll[2] <- f.lam(ysamp, w2, l[3], delta, l[2])
  
  ll[3] <- m1 * l[1] * sum(w1 /(1 + l[1] * f.w(xsamp, l[3], 0))) +  
    m2 * l[2] * sum(w2 /(1 + l[2] * f.w(ysamp, l[3], delta)))
  ll
}

f.mu <- function(xx, yy, w1, w2, m1, m2, delta, l1, l2, mu){
  m1 * l1 * sum(w1 /(1 + l1 * f.w(xx, mu, 0))) +  
    m2 * l2 * sum(w2 /(1 + l2 * f.w(yy, mu, delta)))
}


f.ell <- function(x, xx, yy, w1, w2, m1, m2, delta, alpha, gamma){
  
  # delta <- 0
  init <- c(-0.01, 0.01, ST_mean(x, alpha, gamma) - delta) # starting guess
  
  get <- nleqslv(init, get.est,
                 xsamp = xx, 
                 ysamp = yy,
                 w1 = w1,
                 w2 = w2, 
                 m1 = m1,
                 m2 = m2, 
                 delta = delta,
                 control = list('stepmax' = 0.01))$x
  
  mu.t <- get[3]
  l1 <- get[1]
  l2 <- get[2]
  
  
  return(m1 * sum(w1 * log(1 + l1 * f.w(xx, mu.t, 0)), na.rm = TRUE) + 
           m2 * sum(w2 * log(1 + l2 * f.w(yy, mu.t, delta)), na.rm = TRUE)) # dažkārt neaprēķina
}

eq.draw <- function(x, y, alpha, gamma, delta){
  xx <- sort(x)
  yy <- sort(y)
  n1 <- length(x)
  r1 <- floor(n1*alpha)
  n2 <- length(y)
  r2 <- floor(n2*alpha)
  
  w1 <- sapply(seq(1:n1)/(n1+1), J_fun, alpha, gamma)
  m1 <- sum(w1)
  w1 <- w1/m1
  w2 <- sapply(seq(1:n2)/(n2+1), J_fun, alpha, gamma)
  m2 <- sum(w2)
  w2 <- w2/m2
  
  xx <- xx[(r1+1):(n1-r1)]
  yy <- yy[(r2+1):(n2-r2)]
  
  w1 <- w1[(r1+1):(n1-r1)]
  w2 <- w2[(r2+1):(n2-r2)]
  
  
  
  init <- c(0, 0, ST_mean(x, alpha, gamma) - delta) # starting guess
  
  get <- nleqslv(init, get.est,
                 xsamp = xx, 
                 ysamp = yy,
                 w1 = w1,
                 w2 = w2, 
                 m1 = m1,
                 m2 = m2, 
                 delta = delta,
                 control = list('stepmax' = 0.01, 'maxit' = 150/0.01))$x
  
  mu.t <- get[3]
  l1 <- get[1]
  l2 <- get[2]
  
  ll11 <- seq(l1-0.2, l1+0.2, by = 0.01)
  
  ll1 <- ggplot()+
    geom_point(aes(x = ll11, y = sapply(ll11, function(ll)f.lam(xx, w1, delta = 0, mu = mu.t, ll))))+
    geom_vline(xintercept = l1, col = 'red', lty = 2, linewidth = 1.2)+
    geom_hline(yintercept = 0)+
    xlab(expression(lambda[1]))+
    ylab(expression(f(lambda[1])))+
    annotate("text", label = round(l1,4), x = l1+0.03, y = 0.03)+
    theme_classic()
  
  ll22 <- seq(l2-0.2, l2+0.2, by = 0.01)
  
  ll2 <- ggplot()+
    geom_point(aes(x = ll22, y = sapply(ll22, function(ll)f.lam(yy, w2, delta = delta, mu = mu.t, ll))))+
    geom_vline(xintercept = l2, col = 'red', lty = 2, linewidth = 1.2)+
    geom_hline(yintercept = 0)+
    xlab(expression(lambda[2]))+
    ylab(expression(f(lambda[2])))+
    annotate("text", label = round(l2,4), x = l2+0.03, y = 0.03)+
    theme_classic()
  
  mm <- seq(mu.t - 0.2, mu.t + 0.2, by = 0.01)
  
  mmu <- ggplot()+
    geom_point(aes(x = mm, y = sapply(mm, function(mm) f.mu(xx, yy, w1, w2, m1, m2, delta, l1, l2, mm))))+
    geom_vline(xintercept = mu.t, col = 'red', lty = 2, linewidth = 1.2)+
    geom_hline(yintercept = 0)+
    xlab(expression(mu))+
    ylab(expression(f(mu)))+
    annotate("text", label = round(mu.t,4), x = mu.t+0.03, y = 0.03)+
    theme_classic()
  
  ggarrange(ll1, ll2, mmu, ncol = 2, nrow = 2, labels = c('l1', 'l2', 'mu'))
  
  
}


EL.stm <- function(x, y, alpha, gamma, level = 0.95, step = 0.1, delta0 = 0, plot = FALSE){
  xx <- sort(x)
  yy <- sort(y)
  n1 <- length(x)
  r1 <- floor(n1*alpha)
  n2 <- length(y)
  r2 <- floor(n2*alpha)
  crit.val <- qchisq(level, df = 1)
  
  x.stm <- ST_mean(x, alpha, gamma)
  y.stm <- ST_mean(y, alpha, gamma)
  
  w1 <- sapply(seq(1:n1)/(n1+1), J_fun, alpha, gamma)
  m1 <- sum(w1)
  w1 <- w1/m1
  w2 <- sapply(seq(1:n2)/(n2+1), J_fun, alpha, gamma)
  m2 <- sum(w2)
  w2 <- w2/m2
  
  a <- ((m2*sum((w1*(xx - x.stm)^2)) + 
           m1*sum((w2*(yy - y.stm)^2)))) /
    (m1*m2*(stmeanvar.asym(x, alpha, gamma) + 
              stmeanvar.asym(y, alpha, gamma)))
  
  xx <- xx[(r1+1):(n1-r1)]
  yy <- yy[(r2+1):(n2-r2)]
  
  w1 <- w1[(r1+1):(n1-r1)]
  w2 <- w2[(r2+1):(n2-r2)]
  
  delta.return0 <- y.stm - x.stm
  delta.return <- delta.return0
  
  
  if(2*a*f.ell(x, xx, yy, w1, w2, m1, m2, delta.return0, alpha, gamma) > crit.val & 
     2*a*f.ell(x, xx, yy, w1, w2, m1, m2, delta.return0-step, alpha, gamma) > 2*a*f.ell(x, xx, yy, w1, w2, m1, m2, delta.return0, alpha, gamma)){
    while (2*a*f.ell(x, xx, yy, w1, w2, m1, m2, delta.return, alpha, gamma) > crit.val & 
           2*a*f.ell(x, xx, yy, w1, w2, m1, m2, delta.return, alpha, gamma) < 2*a*f.ell(x, xx, yy, w1, w2, m1, m2, delta.return0, alpha, gamma)) {
      delta.return <- delta.return + step
    }
  }else if(2*a*f.ell(x, xx, yy, w1, w2, m1, m2, delta.return0, alpha, gamma) > crit.val & 
           2*a*f.ell(x, xx, yy, w1, w2, m1, m2, delta.return0-step, alpha, gamma) < 2*a*f.ell(x, xx, yy, w1, w2, m1, m2, delta.return0, alpha, gamma)){
    while (2*a*f.ell(x, xx, yy, w1, w2, m1, m2, delta.return, alpha, gamma) > crit.val& 
           2*a*f.ell(x, xx, yy, w1, w2, m1, m2, delta.return, alpha, gamma) < 2*a*f.ell(x, xx, yy, w1, w2, m1, m2, delta.return0, alpha, gamma)) {
      delta.return <- delta.return - step
    }
  }
  
  if(2*a*f.ell(x, xx, yy, w1, w2, m1, m2, delta.return, alpha, gamma) < crit.val){
    
    lo <- delta.return - step
    hi <- delta.return + step
    
    while(2*a*f.ell(x, xx, yy, w1, w2, m1, m2, delta = lo, alpha, gamma) < crit.val ){
      lo <- lo - step
    }
    while(2*a*f.ell(x, xx, yy, w1, w2, m1, m2, delta = hi, alpha, gamma) < crit.val ){
      hi <- hi + step
    }
    
    delta.return <- optimise(function(d)f.ell(x, xx, yy, w1, w2, m1, m2, d, alpha, gamma), interval = c(lo, hi))$minimum
    ci <- c(uniroot(function(d) 2*a*f.ell(x, xx, yy, w1, w2, m1, m2, d, alpha, gamma)  - crit.val, c(lo, delta.return))$root,
            uniroot(function(d) 2*a*f.ell(x, xx, yy, w1, w2, m1, m2, d, alpha, gamma)  - crit.val, c(delta.return, hi))$root)
    
    # st <- 2*a*f.ell(x, xx, yy, w1, w2, m1, m2, delta0)
    
    pq <- c(f.pq(x, xx, yy, ci[1], w1, w2, m1, m2, alpha, gamma), f.pq(x, xx, yy, ci[2], w1, w2, m1, m2, alpha, gamma))
    names(pq) <- c('p.ci.lb', 'q.ci.lb','p.ci.ub', 'q.ci.ub')
    
    if(sum(round(pq) == 1) == 4){
      ci <- ci
    }else{
      ci <- c(NA, NA)
    }
    
  }else{
    ci <- c(NA, NA)
    pq <- c(NA, NA, NA, NA)
    names(pq) <- c('p.ci.lb', 'q.ci.lb','p.ci.ub', 'q.ci.ub')
  }
  
  if(plot == TRUE){
    dd <- seq(ci[1] - 0.1, ci[2] + 0.1, by = 0.01)
    
    el <- ggplot()+
      geom_line(aes(x = dd, y = 2*a*sapply(dd, function(dd) f.ell(x, xx, yy, w1, w2, m1, m2, dd, alpha, gamma))), linewidth = 1.2)+
      geom_hline(yintercept = qchisq(0.95, df = 1), col = 'red', lty = 2, linewidth = 1.5)+
      xlab(expression(Delta))+
      ylab(expression(-2*a*log(mu,Delta)))+
      theme_classic()
    
  }else{
    el = 'EL plot not returned'
  }
  
  # eq.draw(x, y, alpha, gamma, lo)
  # f.ell(x, xx, yy, w1, w2, m1, m2, -4)
  # return(list('conf.int' = ci
  #             ,'par_lb' = f.ell2(x, xx, yy, w1, w2, m1, m2, ci[1])$par
  #             ,'par_ub' = f.ell2(x, xx, yy, w1, w2, m1, m2, ci[2])$par
  # ))
  return(list('conf.int' = ci
              # ,'st' = st
              ,'est' = delta.return
              # ,'p.value' = 1 - pchisq(st, df = 1)
              ,'pq' = pq
              ,'ell.plot' = el
  ))
  
}

trim.smpl<-function(Y,a,b)  
{
  #Returns trimmed sample and quantiles 
  #trimming constants: a, b < 0.5
  n<-length(Y)
  r<- trunc(n*a) + 1
  s<- n-trunc(n * b)
  m<- s - r +1
  ys<-sort(Y)
  x=ys[r:s]
  list(x=x, quan=c(ys[r],ys[s]))
}

a_abhat3<-function(Y,alpha,beta) {
  ## Function finds trimmed sample, and constants sigma, tau, a
  n<-length(Y)
  r<- trunc(n*alpha) + 1
  s<- n-trunc(n * beta)
  m<- s - r +1
  Y_tr<-sort(Y)[r:s]
  mu_ab <- mean(Y_tr)
  xi1 <-  Y_tr[1]
  xi2 <-  tail(Y_tr,1)
  sigma2 <- sum((Y_tr - mu_ab)^2)/n/(1-alpha-beta)
  tau2<-sigma2/(1-beta-alpha) + (beta*(1-beta)*(xi2-mu_ab)^2 - 2*alpha*beta*(xi1-mu_ab)*(xi2-mu_ab)               +alpha*(1-alpha)*(xi1-mu_ab)^2)/(1-beta-alpha)/(1-beta-alpha)
  a <- sigma2/(1-beta-alpha)/tau2
  return(list("mu_ab" = mu_ab, "sigma2" = sigma2, "tau2" =tau2, "aconst" = a, "m" = m ))
}

EL.tm <- function(x, y, mu.t=0, alpha=0.05, beta=0.05, conf.level = 0.95){
  n1 <- length(x)
  n2 <- length(y)
  scaling_1 <- a_abhat3(x, alpha, beta)
  scaling_2 <- a_abhat3(y, alpha, beta)
  m1 <- scaling_1$m
  m2 <- scaling_2$m
  a_scaling <- (n1/m1)* (n2/m2) * (m2 * scaling_1$sigma2 + m1 * scaling_2$sigma2)/
    (n2 * scaling_1$tau2 + n1*scaling_2$tau2)
  #Setting confidence level for TM finding
  conf.level.tm <- pchisq(qchisq(conf.level, 1)/a_scaling, 1)
  elmeans<- EL::EL.means(trim.smpl(x, alpha, beta)$x, trim.smpl(y, alpha, beta)$x, mu=mu.t, conf.level = conf.level.tm)
  logratio <- as.numeric(elmeans$statistic*a_scaling)
  names(logratio) = "-2 * LogLikelihood * a" 
  pval <- 1 - pchisq(logratio, 1)
  conf.int <- elmeans$conf.int
  attr(conf.int, "conf.level") <-  conf.level
  call <- match.call()
  estimate = elmeans$estimate; names(estimate) <- "Trimmed means difference"
  res <- list(estimate = elmeans$estimate, conf.int = conf.int, p.value = pval, 
              statistic = logratio, method = paste0("Empirical likelihood ", alpha, "-", beta, "-trimmed mean difference test"), 
              null.value = mu.t, data.name = paste(deparse(call$x), 
                                                   " and ", deparse(call$y)))
  class(res) <- "htest"
  res
}

### N SAMPLE ----

a.ANOVA <- function(x, alpha, gamma){
  x <- sort(x)
  n <- length(x)
  w <- sapply(seq(1:n)/(n+1), J_fun, alpha, gamma)
  m <- sum(w)
  w_norm <- w/m
  x.stm <- ST_mean(x, alpha, gamma)
  sigma <- sum(w_norm*(x - x.stm)^2)
  d.x <- stmeanvar.asym(x, alpha, gamma)
  
  return(list('a' = sigma/(m * d.x),
              'sigma' = sigma,
              'stm' = x.stm,
              'm' = m))
}

EL.anova.stm<-function(w, alpha, gamma)
{
  
  k<-length(w)
  a <-unlist(lapply(w, function(x) a.ANOVA(x, alpha, gamma)$a))
  stm <-unlist(lapply(w, function(x) a.ANOVA(x, alpha, gamma)$stm))
  
  sigma <-unlist(lapply(w, function(x) a.ANOVA(x, alpha, gamma)$sigma))
  m <- unlist(lapply(w, function(x) a.ANOVA(x, alpha, gamma)$m))
  
  mu_hat <- sum(stm * m / sigma) / sum(m / sigma)
  r <- sum(a*m*(stm - mu_hat)^2/sigma)
  
  pval<-1-pchisq(r,k-1)
  
  rez<-list(mu0=mu_hat,statistic=r,p.value=pval)#,ci=ci) #new
  return(rez)
}

ANOVA.stm <- function(w, alpha, gamma){
  k<-length(w)
  stm <-unlist(lapply(w, function(x) a.ANOVA(x, alpha, gamma)$stm))
  m <- unlist(lapply(w, function(x) a.ANOVA(x, alpha, gamma)$m))
  d <- unlist(lapply(w, function(x) stmeanvar.asym(x, alpha, gamma)))
  
  wj.1 <- 1/d
  w.1 <- sum(wj.1)
  x.1 <- sum(wj.1 * stm)/w.1
  F.st1 <- 1/(k-1) * sum(wj.1 * (stm - x.1)^2)/ (1 + 2*(k-2)/(k^2 - 1) * sum(1/(m-1) * (1-wj.1/w.1)^2))
  df1 <- (k^2 - 1)/(3*sum(1/(m-1) * (1-wj.1/w.1)^2))  
  
  return(list('p.value' = 1-pf(F.st1, k-1, df1),
              'F' = F.st1,
              'df' = c(k-1, df1)))
  
}


EL.anova.tm<-function(w, alpha, beta)
{
  ## Compute the EL-trimmed ANOVA
  ## w is the original sample LIST
  k<-length(w)
  aa<-unlist(lapply(w, function(x) a_abhat3(x, alpha, beta)$aconst))
  mu_ab<-unlist(lapply(w, function(x) a_abhat3(x, alpha, beta)$mu_ab))
  #yhat<-out$yhat
  #s2<-out$s2
  sigma2<-unlist(lapply(w, function(x) a_abhat3(x, alpha, beta)$sigma2))
  m <- unlist(lapply(w, function(x) a_abhat3(x, alpha, beta)$m))
  weights<-m/sigma2
  muhat<-sum(weights*mu_ab)/sum(weights)
  r<-sum(aa*m*(mu_ab - muhat)^2/sigma2)
  pval<-1-pchisq(r,k-1)
  #ci<-conf.int.anova(w,n,muhat,yhat,s,a,alfa) #new
  rez<-list(mu0=muhat,statistic=r,p.value=pval)#,ci=ci) #new
  return(rez)
}

EL.anova<-function(w)
  #w - list of samples! There is no free variable "t". Ie, the better trimming guess, 
  #biger the chance that  mu[a,b] is close to real mu and falls in the CI.
{
  k<-length(w)
  n<-sapply(w,length)
  s<-sapply(w,function(i) var(i)* (length(i)-1)/length(i))
  yhat<-sapply(w,mean)
  muhat<-sum(n*yhat/s)/sum(n/s)
  r<-sum(n*(yhat-muhat)^2/s)
  pval<-1-pchisq(r,k-1)
  rez<-list(mu0=muhat,statistic=r,pval=pval)
  return(rez)
}






