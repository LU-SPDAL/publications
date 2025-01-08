source('functions.R')
library(WRS2)
library(tidyverse)
library(rstatix)

# Parameters ----

NN <- c(50,100,200,500)
alph <- c(0.05,0.1,0.15,0.2)
gamm <- c(0.1,0.2,0.3,0.4)
alph_gam <- as.data.frame(cbind(c(rep(alph[1],4),rep(alph[2],3), rep(alph[3],3), rep(alph[4],2)),
                                c(gamm, gamm[-1], gamm[-1], gamm[-(1:2)])))
names(alph_gam) <- c('alpha', 'gamma')
iter <- 10000

# 2 sample data sim ---- 

tm.norm.el <- as.data.frame(matrix(nrow = length(NN), ncol = 4))
tm.t.el <- as.data.frame(matrix(nrow = length(NN), ncol = 4))
tm.chi.el <- as.data.frame(matrix(nrow = length(NN), ncol = 4))
tm.N.el <- as.data.frame(matrix(nrow = length(NN), ncol = 4))
tm.N2.el <- as.data.frame(matrix(nrow = length(NN), ncol = 4))
tm.NU.el <- as.data.frame(matrix(nrow = length(NN), ncol = 4))

tm.norm.y <- as.data.frame(matrix(nrow = length(NN), ncol = 4))
tm.t.y <- as.data.frame(matrix(nrow = length(NN), ncol = 4))
tm.chi.y <- as.data.frame(matrix(nrow = length(NN), ncol = 4))
tm.N.y <- as.data.frame(matrix(nrow = length(NN), ncol = 4))
tm.N2.y <- as.data.frame(matrix(nrow = length(NN), ncol = 4))
tm.NU.y <- as.data.frame(matrix(nrow = length(NN), ncol = 4))

stm.norm.y <- as.data.frame(matrix(nrow = length(NN), ncol = 12))
stm.t.y <- as.data.frame(matrix(nrow = length(NN), ncol = 12))
stm.chi.y <- as.data.frame(matrix(nrow = length(NN), ncol = 12))
stm.N.y <- as.data.frame(matrix(nrow = length(NN), ncol = 12))
stm.N2.y <- as.data.frame(matrix(nrow = length(NN), ncol = 12))
stm.NU.y <- as.data.frame(matrix(nrow = length(NN), ncol = 12))

stm.norm.el <- as.data.frame(matrix(nrow = length(NN), ncol = 12))
stm.t.el <- as.data.frame(matrix(nrow = length(NN), ncol = 12))
stm.chi.el <- as.data.frame(matrix(nrow = length(NN), ncol = 12))
stm.N.el <- as.data.frame(matrix(nrow = length(NN), ncol = 12))
stm.N2.el <- as.data.frame(matrix(nrow = length(NN), ncol = 12))
stm.NU.el <- as.data.frame(matrix(nrow = length(NN), ncol = 12))

t.norm <- as.data.frame(matrix(nrow = length(NN), ncol = 1))
t.t <- as.data.frame(matrix(nrow = length(NN), ncol = 1))
t.chi <- as.data.frame(matrix(nrow = length(NN), ncol = 1))
t.N <- as.data.frame(matrix(nrow = length(NN), ncol = 1))
t.N2 <- as.data.frame(matrix(nrow = length(NN), ncol = 1))
t.NU <- as.data.frame(matrix(nrow = length(NN), ncol = 1))

set.seed(1)

system.time({
  for(n in 1:length(NN)){ ### LOOP FOR SAMPLE SIZE n
  # n <- 1
  
  t.temp.norm <- matrix(ncol = 1, nrow = iter)
  t.temp.t <- matrix(ncol = 1, nrow = iter)
  t.temp.chi <- matrix(ncol = 1, nrow = iter)
  t.temp.N <- matrix(ncol = 1, nrow = iter)
  t.temp.N2 <- matrix(ncol = 1, nrow = iter)
  t.temp.NU <- matrix(ncol = 1, nrow = iter)

  tm.temp.norm.el <- matrix(ncol = 4, nrow = iter)
  tm.temp.t.el <- matrix(ncol = 4, nrow = iter)
  tm.temp.chi.el <- matrix(ncol = 4, nrow = iter)
  tm.temp.N.el <- matrix(ncol = 4, nrow = iter)
  tm.temp.N2.el <- matrix(ncol = 4, nrow = iter)
  tm.temp.NU.el <- matrix(ncol = 4, nrow = iter)

  tm.temp.norm.y <- matrix(ncol = 4, nrow = iter)
  tm.temp.t.y <- matrix(ncol = 4, nrow = iter)
  tm.temp.chi.y <- matrix(ncol = 4, nrow = iter)
  tm.temp.N.y <- matrix(ncol = 4, nrow = iter)
  tm.temp.N2.y <- matrix(ncol = 4, nrow = iter)
  tm.temp.NU.y <- matrix(ncol = 4, nrow = iter)

  stm.temp.N.y <- matrix(ncol = 12, nrow = iter)
  stm.temp.N2.y <- matrix(ncol = 12, nrow = iter)
  stm.temp.NU.y <- matrix(ncol = 12, nrow = iter)
  stm.temp.norm.y <- matrix(ncol = 12, nrow = iter)
  stm.temp.t.y <- matrix(ncol = 12, nrow = iter)
  stm.temp.chi.y <- matrix(ncol = 12, nrow = iter)

  stm.temp.N.el <- matrix(ncol = 12, nrow = iter)
  stm.temp.N2.el <- matrix(ncol = 12, nrow = iter)
  stm.temp.NU.el <- matrix(ncol = 12, nrow = iter)
  stm.temp.norm.el <- matrix(ncol = 12, nrow = iter)
  stm.temp.t.el <- matrix(ncol = 12, nrow = iter)
  stm.temp.chi.el <- matrix(ncol = 12, nrow = iter)

  
  
  for(i in 1:iter){ ### LOOP FOR ITERATIONS
    # i <- 1
    
    ### GENERATING DATA
    x.N <- replicate(NN[n], gen.N())
    y.N <- replicate(NN[n], gen.N())
    
    x.N2 <- replicate(NN[n], gen.N2())
    y.N2 <- replicate(NN[n], gen.N2())
    
    x.NU <- replicate(NN[n], gen.NU())
    y.NU <- replicate(NN[n], gen.NU())
    
    x.norm <- rnorm(NN[n])
    y.norm <- rnorm(NN[n])
    
    x.t <- rt(NN[n], df = 2)
    y.t <- rt(NN[n], df = 2)
    
    x.chi <- rchisq(NN[n], df = 3)
    y.chi <- rchisq(NN[n], df = 3)
    ### END
    
    ci.N.t <- t.test(x.N, y.N)$conf.int
    ci.N2.t <- t.test(x.N2, y.N2)$conf.int
    ci.NU.t <- t.test(x.NU, y.NU)$conf.int
    ci.norm.t <- t.test(x.norm, y.norm)$conf.int
    ci.t.t <- t.test(x.t, y.t)$conf.int
    ci.chi.t <- t.test(x.chi, y.chi)$conf.int

    t.temp.N[i,1] <- coverage(0, ci.N.t[1], ci.N.t[2])
    t.temp.N2[i,1] <- coverage(0, ci.N2.t[1], ci.N2.t[2])
    t.temp.NU[i,1] <- coverage(0, ci.NU.t[1], ci.NU.t[2])
    t.temp.norm[i,1] <- coverage(0, ci.norm.t[1], ci.norm.t[2])
    t.temp.t[i,1] <- coverage(0, ci.t.t[1], ci.t.t[2])
    t.temp.chi[i,1] <- coverage(0, ci.chi.t[1], ci.chi.t[2])
    
    ### TRANSOFRMING DATA FOR yuen.tm() FUNNCTION
    gr <- c(rep(1, NN[n]), rep(2, NN[n]))

    data.N <- data.frame(data = c(x.N,y.N), gr = gr)
    data.N2 <- data.frame(data = c(x.N2,y.N2), gr = gr)
    data.NU <- data.frame(data = c(x.NU,y.NU), gr = gr)

    data.norm <- data.frame(data = c(x.norm,y.norm), gr = gr)
    data.t <- data.frame(data = c(x.t,y.t), gr = gr)
    data.chi <- data.frame(data = c(x.chi,y.chi), gr = gr)
    ### END

    ## FOR SMOOTHLY TRIMMED MEAN
    temp.alph.N.y <- matrix(ncol = 12, nrow = 1)
    temp.alph.N2.y <- matrix(ncol = 12, nrow = 1)
    temp.alph.NU.y <- matrix(ncol = 12, nrow = 1)
    temp.alph.norm.y <- matrix(ncol = 12, nrow = 1)
    temp.alph.t.y <- matrix(ncol = 12, nrow = 1)
    temp.alph.chi.y <- matrix(ncol = 12, nrow = 1)

    temp.alph.N.el <- matrix(ncol = 12, nrow = 1)
    temp.alph.N2.el <- matrix(ncol = 12, nrow = 1)
    temp.alph.NU.el <- matrix(ncol = 12, nrow = 1)
    temp.alph.norm.el <- matrix(ncol = 12, nrow = 1)
    temp.alph.t.el <- matrix(ncol = 12, nrow = 1)
    temp.alph.chi.el <- matrix(ncol = 12, nrow = 1)

    ### END

    for(a in 1:length(alph)){ ### LOOP FOR ALPHAS
      # a <- 1
      ci.N.y <- yuen.tm(data ~ gr, data = data.N, tr = alph[a])$conf.int
      ci.N2.y <- yuen.tm(data ~ gr, data = data.N2, tr = alph[a])$conf.int
      ci.NU.y <- yuen.tm(data ~ gr, data = data.NU, tr = alph[a])$conf.int
      ci.norm.y <- yuen.tm(data ~ gr, data = data.norm, tr = alph[a])$conf.int
      ci.t.y <- yuen.tm(data ~ gr, data = data.t, tr = alph[a])$conf.int
      ci.chi.y <- yuen.tm(data ~ gr, data = data.chi, tr = alph[a])$conf.int

      ci.N.el <- EL.tm(x.N, y.N, alph[a], beta = alph[a])$conf.int
      ci.N2.el <- EL.tm(x.N2, y.N2, alph[a], beta = alph[a])$conf.int
      ci.NU.el <- EL.tm(x.NU, y.NU, alph[a], beta = alph[a])$conf.int
      ci.norm.el <- EL.tm(x.norm, y.norm, alph[a], beta = alph[a])$conf.int
      ci.t.el <- EL.tm(x.t, y.t, alph[a], beta = alph[a])$conf.int
      ci.chi.el <- EL.tm(x.chi, y.chi, alph[a], beta = alph[a])$conf.int

      tm.temp.N.y[i,a] <- coverage(0, ci.N.y[1], ci.N.y[2])
      tm.temp.N2.y[i,a] <- coverage(0, ci.N2.y[1], ci.N2.y[2])
      tm.temp.NU.y[i,a] <- coverage(0, ci.NU.y[1], ci.NU.y[2])
      tm.temp.norm.y[i,a] <- coverage(0, ci.norm.y[1], ci.norm.y[2])
      tm.temp.t.y[i,a] <- coverage(0, ci.t.y[1], ci.t.y[2])
      tm.temp.chi.y[i,a] <- coverage(0, ci.chi.y[1], ci.chi.y[2])

      tm.temp.N.el[i,a] <- coverage(0, ci.N.el[1], ci.N.el[2])
      tm.temp.N2.el[i,a] <- coverage(0, ci.N2.el[1], ci.N2.el[2])
      tm.temp.NU.el[i,a] <- coverage(0, ci.NU.el[1], ci.NU.el[2])
      tm.temp.norm.el[i,a] <- coverage(0, ci.norm.el[1], ci.norm.el[2])
      tm.temp.t.el[i,a] <- coverage(0, ci.t.el[1], ci.t.el[2])
      tm.temp.chi.el[i,a] <- coverage(0, ci.chi.el[1], ci.chi.el[2])


      ### FIND VALID GAMMAS FOR GIVEN ALPHA
      gam <- alph_gam[alph_gam$alpha == alph[a],]$gamma

      if(a == 1){
        indx <- 1
      }else if(a == 2){
        indx <- 5
      }else if(a == 3){
        indx <- 8
      }else{
        indx <- 11
      }
      ### END

      temp.gam.N.y <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.N2.y <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.NU.y <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.norm.y <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.t.y <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.chi.y <- matrix(ncol = length(gam), nrow = 1)

      temp.gam.N.el <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.N2.el <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.NU.el <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.norm.el <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.t.el <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.chi.el <- matrix(ncol = length(gam), nrow = 1)


      for(g in 1:length(gam)){ ### LOOP FOR GAMMAS
        # g <- 1
        ci.N.y <- yuen.stm(x.N,y.N, alph[a], gam[g])$conf.int
        ci.N2.y <- yuen.stm(x.N2,y.N2, alph[a], gam[g])$conf.int
        ci.NU.y <- yuen.stm(x.NU,y.NU, alph[a], gam[g])$conf.int
        ci.norm.y <- yuen.stm(x.norm,y.norm, alph[a], gam[g])$conf.int
        ci.t.y <- yuen.stm(x.t,y.t, alph[a], gam[g])$conf.int
        ci.chi.y <- yuen.stm(x.chi,y.chi, alph[a], gam[g])$conf.int


        temp.gam.N.y[1,g] <- coverage(0, ci.N.y[1], ci.N.y[2])
        temp.gam.N2.y[1,g] <- coverage(0, ci.N2.y[1], ci.N2.y[2])
        temp.gam.NU.y[1,g] <- coverage(0, ci.NU.y[1], ci.NU.y[2])
        temp.gam.norm.y[1,g] <- coverage(0, ci.norm.y[1], ci.norm.y[2])
        temp.gam.t.y[1,g] <- coverage(0, ci.t.y[1], ci.t.y[2])
        temp.gam.chi.y[1,g] <- coverage(0, ci.chi.y[1], ci.chi.y[2])

        ci.N.el <- EL.stm(x.N, y.N, alpha = alph[a], gamma = gam[g])$conf.int 
        ci.N2.el <- EL.stm(x.N2,y.N2, alph[a], gamma = gam[g])$conf.int
        ci.NU.el <- EL.stm(x.NU,y.NU, alph[a], gamma = gam[g])$conf.int
        ci.norm.el <- EL.stm(x.norm,y.norm, alph[a], gamma = gam[g])$conf.int
        ci.t.el <- EL.stm(x.t,y.t, alph[a], gamma = gam[g])$conf.int
        ci.chi.el <- EL.stm(x.chi,y.chi, alph[a], gamma = gam[g])$conf.int


        temp.gam.N.el[1,g] <- coverage(0, ci.N.el[1], ci.N.el[2])
        temp.gam.N2.el[1,g] <- coverage(0, ci.N2.el[1], ci.N2.el[2])
        temp.gam.NU.el[1,g] <- coverage(0, ci.NU.el[1], ci.NU.el[2])
        temp.gam.norm.el[1,g] <- coverage(0, ci.norm.el[1], ci.norm.el[2])
        temp.gam.t.el[1,g] <- coverage(0, ci.t.el[1], ci.t.el[2])
        temp.gam.chi.el[1,g] <- coverage(0, ci.chi.el[1], ci.chi.el[2])
        # print(g)

      } ### LOOP FOR GAMMAS ENDS


      temp.alph.N.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.N.y
      temp.alph.N2.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.N2.y
      temp.alph.NU.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.NU.y
      temp.alph.norm.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.norm.y
      temp.alph.t.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.t.y
      temp.alph.chi.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.chi.y

      temp.alph.N.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.N.el
      temp.alph.N2.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.N2.el
      temp.alph.NU.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.NU.el
      temp.alph.norm.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.norm.el
      temp.alph.t.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.t.el
      temp.alph.chi.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.chi.el

      # print(a)

    } ### LOOP FOR ALPHAS ENDS

    stm.temp.N.y[i,] <- temp.alph.N.y
    stm.temp.N2.y[i,] <- temp.alph.N2.y
    stm.temp.NU.y[i,] <- temp.alph.NU.y
    stm.temp.norm.y[i,] <- temp.alph.norm.y
    stm.temp.t.y[i,] <- temp.alph.t.y
    stm.temp.chi.y[i,] <- temp.alph.chi.y


    stm.temp.N.el[i,] <- temp.alph.N.el
    stm.temp.N2.el[i,] <- temp.alph.N2.el
    stm.temp.NU.el[i,] <- temp.alph.NU.el
    stm.temp.norm.el[i,] <- temp.alph.norm.el
    stm.temp.t.el[i,] <- temp.alph.t.el
    stm.temp.chi.el[i,] <- temp.alph.chi.el

    print(paste('i: ', i, '; n: ', n, sep = ''))
  } ### LOOP FOR ITERATIONS ENDS


  t.N[n,] <- colMeans(t.temp.N, na.rm = TRUE)
  t.N2[n,] <- colMeans(t.temp.N2, na.rm = TRUE)
  t.NU[n,] <- colMeans(t.temp.NU, na.rm = TRUE)
  t.norm[n,] <- colMeans(t.temp.norm, na.rm = TRUE)
  t.t[n,] <- colMeans(t.temp.t, na.rm = TRUE)
  t.chi[n,] <- colMeans(t.temp.chi, na.rm = TRUE)

  tm.N.y[n,] <- colMeans(tm.temp.N.y, na.rm = TRUE)
  tm.N2.y[n,] <- colMeans(tm.temp.N2.y, na.rm = TRUE)
  tm.NU.y[n,] <- colMeans(tm.temp.NU.y, na.rm = TRUE)
  tm.norm.y[n,] <- colMeans(tm.temp.norm.y, na.rm = TRUE)
  tm.t.y[n,] <- colMeans(tm.temp.t.y, na.rm = TRUE)
  tm.chi.y[n,] <- colMeans(tm.temp.chi.y, na.rm = TRUE)

  tm.N.el[n,] <- colMeans(tm.temp.N.el, na.rm = TRUE)
  tm.N2.el[n,] <- colMeans(tm.temp.N2.el, na.rm = TRUE)
  tm.NU.el[n,] <- colMeans(tm.temp.NU.el, na.rm = TRUE)
  tm.norm.el[n,] <- colMeans(tm.temp.norm.el, na.rm = TRUE)
  tm.t.el[n,] <- colMeans(tm.temp.t.el, na.rm = TRUE)
  tm.chi.el[n,] <- colMeans(tm.temp.chi.el, na.rm = TRUE)

  stm.N.y[n,] <- colMeans(stm.temp.N.y, na.rm = TRUE)
  stm.N2.y[n,] <- colMeans(stm.temp.N2.y, na.rm = TRUE)
  stm.NU.y[n,] <- colMeans(stm.temp.NU.y, na.rm = TRUE)
  stm.norm.y[n,] <- colMeans(stm.temp.norm.y, na.rm = TRUE)
  stm.t.y[n,] <- colMeans(stm.temp.t.y, na.rm = TRUE)
  stm.chi.y[n,] <- colMeans(stm.temp.chi.y, na.rm = TRUE)

  stm.N.el[n,] <- colMeans(stm.temp.N.el, na.rm = TRUE)
  stm.N2.el[n,] <- colMeans(stm.temp.N2.el, na.rm = TRUE)
  stm.NU.el[n,] <- colMeans(stm.temp.NU.el, na.rm = TRUE)
  stm.norm.el[n,] <- colMeans(stm.temp.norm.el, na.rm = TRUE)
  stm.t.el[n,] <- colMeans(stm.temp.t.el, na.rm = TRUE)
  stm.chi.el[n,] <- colMeans(stm.temp.chi.el, na.rm = TRUE)
  
  
} ### LOOP FOR SAMPLE SIZE n ENDS 
  })


# 3 sample data sim ----

p.aov.norm <- matrix(ncol = 2, nrow = length(NN))
p.aov.lnorm <- matrix(ncol = 2, nrow = length(NN))
p.aov.chi <- matrix(ncol = 2, nrow = length(NN))
p.aov.N <- matrix(ncol = 2, nrow = length(NN))
p.aov.N2 <- matrix(ncol = 2, nrow = length(NN))
p.aov.NU <- matrix(ncol = 2, nrow = length(NN))
p.aov.N3 <- matrix(ncol = 2, nrow = length(NN))

p.y.norm.tm <- matrix(ncol = 4, nrow = length(NN))
p.y.lnorm.tm <- matrix(ncol = 4, nrow = length(NN))
p.y.chi.tm <- matrix(ncol = 4, nrow = length(NN))
p.y.N.tm <- matrix(ncol = 4, nrow = length(NN))
p.y.N2.tm <- matrix(ncol = 4, nrow = length(NN))
p.y.NU.tm <- matrix(ncol = 4, nrow = length(NN))
p.y.N3.tm <- matrix(ncol = 4, nrow = length(NN))

p.el.norm.tm <- matrix(ncol = 4, nrow = length(NN))
p.el.lnorm.tm <- matrix(ncol = 4, nrow = length(NN))
p.el.chi.tm <- matrix(ncol = 4, nrow = length(NN))
p.el.N.tm <- matrix(ncol = 4, nrow = length(NN))
p.el.N2.tm <- matrix(ncol = 4, nrow = length(NN))
p.el.NU.tm <- matrix(ncol = 4, nrow = length(NN))
p.el.N3.tm <- matrix(ncol = 4, nrow = length(NN))

p.el.norm.stm <- matrix(ncol = 12, nrow = length(NN))
p.el.lnorm.stm <- matrix(ncol = 12, nrow = length(NN))
p.el.chi.stm <- matrix(ncol = 12, nrow = length(NN))
p.el.N.stm <- matrix(ncol = 12, nrow = length(NN))
p.el.N2.stm <- matrix(ncol = 12, nrow = length(NN))
p.el.NU.stm <- matrix(ncol = 12, nrow = length(NN))
p.el.N3.stm <- matrix(ncol = 12, nrow = length(NN))

p.y.norm.stm <- matrix(ncol = 12, nrow = length(NN))
p.y.lnorm.stm <- matrix(ncol = 12, nrow = length(NN))
p.y.chi.stm <- matrix(ncol = 12, nrow = length(NN))
p.y.N.stm <- matrix(ncol = 12, nrow = length(NN))
p.y.N2.stm <- matrix(ncol = 12, nrow = length(NN))
p.y.NU.stm <- matrix(ncol = 12, nrow = length(NN))
p.y.N3.stm <- matrix(ncol = 12, nrow = length(NN))
# n <- 1

set.seed(1)
# iter <- 1000
system.time(for(n in 1:length(NN)){ ### LOOP FOR SAMPLE SIZE
  temp.aov.norm <- matrix(ncol = 2, nrow = iter)
  temp.aov.lnorm <- matrix(ncol = 2, nrow = iter)
  temp.aov.chi <- matrix(ncol = 2, nrow = iter)
  temp.aov.N <- matrix(ncol = 2, nrow = iter)
  temp.aov.N2 <- matrix(ncol = 2, nrow = iter)
  temp.aov.NU <- matrix(ncol = 2, nrow = iter)

  temp.y.norm.tm <- matrix(ncol = 4, nrow = iter)
  temp.y.lnorm.tm <- matrix(ncol = 4, nrow = iter)
  temp.y.chi.tm <- matrix(ncol = 4, nrow = iter)
  temp.y.N.tm <- matrix(ncol = 4, nrow = iter)
  temp.y.N2.tm <- matrix(ncol = 4, nrow = iter)
  temp.y.NU.tm <- matrix(ncol = 4, nrow = iter)

  temp.el.norm.tm <- matrix(ncol = 4, nrow = iter)
  temp.el.lnorm.tm <- matrix(ncol = 4, nrow = iter)
  temp.el.chi.tm <- matrix(ncol = 4, nrow = iter)
  temp.el.N.tm <- matrix(ncol = 4, nrow = iter)
  temp.el.N2.tm <- matrix(ncol = 4, nrow = iter)
  temp.el.NU.tm <- matrix(ncol = 4, nrow = iter)
  
  temp.el.norm.stm <- matrix(ncol = 12, nrow = iter)
  temp.el.lnorm.stm <- matrix(ncol = 12, nrow = iter)
  temp.el.chi.stm <- matrix(ncol = 12, nrow = iter)
  temp.el.N.stm <- matrix(ncol = 12, nrow = iter)
  temp.el.N2.stm <- matrix(ncol = 12, nrow = iter)
  temp.el.NU.stm <- matrix(ncol = 12, nrow = iter)
  
  temp.y.norm.stm <- matrix(ncol = 12, nrow = iter)
  temp.y.lnorm.stm <- matrix(ncol = 12, nrow = iter)
  temp.y.chi.stm <- matrix(ncol = 12, nrow = iter)
  temp.y.N.stm <- matrix(ncol = 12, nrow = iter)
  temp.y.N2.stm <- matrix(ncol = 12, nrow = iter)
  temp.y.NU.stm <- matrix(ncol = 12, nrow = iter)
  
  for(i in 1:iter){ ### LOOP FOR ITERATIONS
    ww.norm <- list(rnorm(NN[n]), rnorm(NN[n]), rnorm(NN[n]))
    ww.lnorm <- list(rlnorm(NN[n], sdlog = 5), rlnorm(NN[n], sdlog = 5), rlnorm(NN[n], sdlog = 5))
    ww.chi <- list(rchisq(NN[n], df = 3), rchisq(NN[n], df = 3), rchisq(NN[n], df = 3))
    ww.N <- list(replicate(NN[n], gen.N()), replicate(NN[n], gen.N()), replicate(NN[n], gen.N()))
    ww.N2 <- list(replicate(NN[n], gen.N2()), replicate(NN[n], gen.N2()), replicate(NN[n], gen.N2()))
    ww.NU <- list(replicate(NN[n], gen.NU()), replicate(NN[n], gen.NU()), replicate(NN[n], gen.NU()))
    
    dati.norm <- data.frame(data = c(ww.norm[[1]], ww.norm[[2]], ww.norm[[3]]),
                            gr = c(rep(1, NN[n]), rep(2,NN[n]), rep(3,NN[n])))
    dati.lnorm <- data.frame(data = c(ww.lnorm[[1]], ww.lnorm[[2]], ww.lnorm[[3]]),
                             gr = c(rep(1, NN[n]), rep(2,NN[n]), rep(3,NN[n])))
    dati.chi <- data.frame(data = c(ww.chi[[1]], ww.chi[[2]], ww.chi[[3]]),
                           gr = c(rep(1, NN[n]), rep(2,NN[n]), rep(3,NN[n])))
    dati.N <- data.frame(data = c(ww.N[[1]], ww.N[[2]], ww.N[[3]]),
                         gr = c(rep(1, NN[n]), rep(2,NN[n]), rep(3,NN[n])))
    dati.N2 <- data.frame(data = c(ww.N2[[1]], ww.N2[[2]], ww.N2[[3]]),
                          gr = c(rep(1, NN[n]), rep(2,NN[n]), rep(3,NN[n])))
    dati.NU <- data.frame(data = c(ww.NU[[1]], ww.NU[[2]], ww.NU[[3]]),
                          gr = c(rep(1, NN[n]), rep(2,NN[n]), rep(3,NN[n])))

    temp.aov.norm[i,1] <- summary(aov(data ~ gr, data = dati.norm))[[1]]$`Pr(>F)`[1]
    temp.aov.lnorm[i,1] <- summary(aov(data ~ gr, data = dati.lnorm))[[1]]$`Pr(>F)`[1]
    temp.aov.chi[i,1] <- summary(aov(data ~ gr, data = dati.chi))[[1]]$`Pr(>F)`[1]
    temp.aov.N[i,1] <- summary(aov(data ~ gr, data = dati.N))[[1]]$`Pr(>F)`[1]
    temp.aov.N2[i,1] <- summary(aov(data ~ gr, data = dati.N2))[[1]]$`Pr(>F)`[1]
    temp.aov.NU[i,1] <- summary(aov(data ~ gr, data = dati.NU))[[1]]$`Pr(>F)`[1]

    temp.aov.norm[i,2] <- welch_anova_test(dati.norm, data ~ gr)$p
    temp.aov.lnorm[i,2] <- welch_anova_test(dati.lnorm, data ~ gr)$p
    temp.aov.chi[i,2] <- welch_anova_test(dati.chi, data ~ gr)$p
    temp.aov.N[i,2] <- welch_anova_test(dati.N, data ~ gr)$p
    temp.aov.N2[i,2] <- welch_anova_test(dati.N2, data ~ gr)$p
    temp.aov.NU[i,2] <- welch_anova_test(dati.NU, data ~ gr)$p
    
    temp.alph.N.el <- matrix(ncol = 12, nrow = 1)
    temp.alph.N2.el <- matrix(ncol = 12, nrow = 1)
    temp.alph.NU.el <- matrix(ncol = 12, nrow = 1)
    temp.alph.norm.el <- matrix(ncol = 12, nrow = 1)
    temp.alph.lnorm.el <- matrix(ncol = 12, nrow = 1)
    temp.alph.chi.el <- matrix(ncol = 12, nrow = 1)
    
    temp.alph.N.y <- matrix(ncol = 12, nrow = 1)
    temp.alph.N2.y <- matrix(ncol = 12, nrow = 1)
    temp.alph.NU.y <- matrix(ncol = 12, nrow = 1)
    temp.alph.norm.y <- matrix(ncol = 12, nrow = 1)
    temp.alph.lnorm.y <- matrix(ncol = 12, nrow = 1)
    temp.alph.chi.y <- matrix(ncol = 12, nrow = 1)
    
    for(a in 1:length(alph)){ ### LOOP FOR ALPHAS
      # a <- 1
      temp.y.norm.tm[i,a] <- t1way(data ~ gr, data = dati.norm, nboot = 1, tr = alph[a])$p.value
      temp.y.lnorm.tm[i,a] <- t1way(data ~ gr, data = dati.lnorm, nboot = 1, tr = alph[a])$p.value
      temp.y.chi.tm[i,a] <- t1way(data ~ gr, data = dati.chi, nboot = 1, tr = alph[a])$p.value
      temp.y.N.tm[i,a] <- t1way(data ~ gr, data = dati.N, nboot = 1, tr = alph[a])$p.value
      temp.y.N2.tm[i,a] <- t1way(data ~ gr, data = dati.N2, nboot = 1, tr = alph[a])$p.value
      temp.y.NU.tm[i,a] <- t1way(data ~ gr, data = dati.NU, nboot = 1, tr = alph[a])$p.value

      temp.el.norm.tm[i,a] <- EL.anova.tm(ww.norm, alph[a], alph[a])$p.value
      temp.el.lnorm.tm[i,a] <- EL.anova.tm(ww.lnorm, alph[a], alph[a])$p.value
      temp.el.chi.tm[i,a] <- EL.anova.tm(ww.chi, alph[a], alph[a])$p.value
      temp.el.N.tm[i,a] <- EL.anova.tm(ww.N, alph[a], alph[a])$p.value
      temp.el.N2.tm[i,a] <- EL.anova.tm(ww.N2, alph[a], alph[a])$p.value
      temp.el.NU.tm[i,a] <- EL.anova.tm(ww.NU, alph[a], alph[a])$p.value
      
      
      gam <- alph_gam[alph_gam$alph == alph[a],]$gamma
      
      if(a == 1){
        indx <- 1
      }else if(a == 2){
        indx <- 5
      }else if(a == 3){
        indx <- 8
      }else{
        indx <- 11
      }
      
      temp.gam.N.el <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.N2.el <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.NU.el <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.norm.el <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.lnorm.el <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.chi.el <- matrix(ncol = length(gam), nrow = 1)
      
      temp.gam.N.y <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.N2.y <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.NU.y <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.norm.y <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.lnorm.y <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.chi.y <- matrix(ncol = length(gam), nrow = 1)
      
      for(g in 1:length(gam)){ ### LOOP FOR GAMMAS
        temp.gam.norm.el[1,g] <- EL.anova.stm(ww.norm, alph[a], gam[g])$p.value
        temp.gam.lnorm.el[1,g] <- EL.anova.stm(ww.lnorm, alph[a], gam[g])$p.value
        temp.gam.chi.el[1,g] <- EL.anova.stm(ww.chi, alph[a], gam[g])$p.value
        temp.gam.N.el[1,g] <- EL.anova.stm(ww.N, alph[a], gam[g])$p.value
        temp.gam.N2.el[1,g] <- EL.anova.stm(ww.N2, alph[a], gam[g])$p.value
        temp.gam.NU.el[1,g] <- EL.anova.stm(ww.NU, alph[a], gam[g])$p.value
        
        temp.gam.norm.y[1,g] <- ANOVA.stm(ww.norm, alph[a], gam[g])$p.value
        temp.gam.lnorm.y[1,g] <- ANOVA.stm(ww.lnorm, alph[a], gam[g])$p.value
        temp.gam.chi.y[1,g] <- ANOVA.stm(ww.chi, alph[a], gam[g])$p.value
        temp.gam.N.y[1,g] <- ANOVA.stm(ww.N, alph[a], gam[g])$p.value
        temp.gam.N2.y[1,g] <- ANOVA.stm(ww.N2, alph[a], gam[g])$p.value
        temp.gam.NU.y[1,g] <- ANOVA.stm(ww.NU, alph[a], gam[g])$p.value
        
      } ### LOOP FOR GAMMAS ENDS
      
      temp.alph.N.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.N.el
      temp.alph.N2.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.N2.el
      temp.alph.NU.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.NU.el
      temp.alph.norm.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.norm.el
      temp.alph.lnorm.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.lnorm.el
      temp.alph.chi.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.chi.el
      
      temp.alph.N.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.N.y
      temp.alph.N2.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.N2.y
      temp.alph.NU.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.NU.y
      temp.alph.norm.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.norm.y
      temp.alph.lnorm.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.lnorm.y
      temp.alph.chi.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.chi.y
      
      
    } ### LOOP FOR ALPHAS ENDS
    
    temp.el.N.stm[i,] <- temp.alph.N.el
    temp.el.N2.stm[i,] <- temp.alph.N2.el
    temp.el.NU.stm[i,] <- temp.alph.NU.el
    temp.el.norm.stm[i,] <- temp.alph.norm.el
    temp.el.lnorm.stm[i,] <- temp.alph.lnorm.el
    temp.el.chi.stm[i,] <- temp.alph.chi.el
    
    temp.y.N.stm[i,] <- temp.alph.N.y
    temp.y.N2.stm[i,] <- temp.alph.N2.y
    temp.y.NU.stm[i,] <- temp.alph.NU.y
    temp.y.norm.stm[i,] <- temp.alph.norm.y
    temp.y.lnorm.stm[i,] <- temp.alph.lnorm.y
    temp.y.chi.stm[i,] <- temp.alph.chi.y
    
    print(paste('i: ', i, '; ', 'n: ', n, sep = ''))
  } ### LOOP FOR ITERATIONS ENDS
  
  p.aov.norm[n,1] <- mean(temp.aov.norm[,1] < 0.05)
  p.aov.lnorm[n,1] <- mean(temp.aov.lnorm[,1] < 0.05)
  p.aov.chi[n,1] <- mean(temp.aov.chi[,1] < 0.05)
  p.aov.N[n,1] <- mean(temp.aov.N[,1] < 0.05)
  p.aov.N2[n,1] <- mean(temp.aov.N2[,1] < 0.05)
  p.aov.NU[n,1] <- mean(temp.aov.NU[,1] < 0.05)

  p.aov.norm[n,2] <- mean(temp.aov.norm[,2] < 0.05)
  p.aov.lnorm[n,2] <- mean(temp.aov.lnorm[,2] < 0.05)
  p.aov.chi[n,2] <- mean(temp.aov.chi[,2] < 0.05)
  p.aov.N[n,2] <- mean(temp.aov.N[,2] < 0.05)
  p.aov.N2[n,2] <- mean(temp.aov.N2[,2] < 0.05)
  p.aov.NU[n,2] <- mean(temp.aov.NU[,2] < 0.05)

  p.el.norm.tm[n,] <- colMeans(temp.el.norm.tm < 0.05, na.rm = TRUE)
  p.el.lnorm.tm[n,] <- colMeans(temp.el.lnorm.tm< 0.05, na.rm = TRUE)
  p.el.chi.tm[n,] <- colMeans(temp.el.chi.tm< 0.05, na.rm = TRUE)
  p.el.N.tm[n,] <- colMeans(temp.el.N.tm< 0.05, na.rm = TRUE)
  p.el.N2.tm[n,] <- colMeans(temp.el.N2.tm< 0.05, na.rm = TRUE)
  p.el.NU.tm[n,] <- colMeans(temp.el.NU.tm< 0.05, na.rm = TRUE)

  p.y.norm.tm[n,] <- colMeans(temp.y.norm.tm < 0.05, na.rm = TRUE)
  p.y.lnorm.tm[n,] <- colMeans(temp.y.lnorm.tm< 0.05, na.rm = TRUE)
  p.y.chi.tm[n,] <- colMeans(temp.y.chi.tm< 0.05, na.rm = TRUE)
  p.y.N.tm[n,] <- colMeans(temp.y.N.tm< 0.05, na.rm = TRUE)
  p.y.N2.tm[n,] <- colMeans(temp.y.N2.tm< 0.05, na.rm = TRUE)
  p.y.NU.tm[n,] <- colMeans(temp.y.NU.tm< 0.05, na.rm = TRUE)
  
  p.el.norm.stm[n,] <- colMeans(temp.el.norm.stm < 0.05, na.rm = TRUE)
  p.el.lnorm.stm[n,] <- colMeans(temp.el.lnorm.stm< 0.05, na.rm = TRUE)
  p.el.chi.stm[n,] <- colMeans(temp.el.chi.stm< 0.05, na.rm = TRUE)
  p.el.N.stm[n,] <- colMeans(temp.el.N.stm< 0.05, na.rm = TRUE)
  p.el.N2.stm[n,] <- colMeans(temp.el.N2.stm< 0.05, na.rm = TRUE)
  p.el.NU.stm[n,] <- colMeans(temp.el.NU.stm< 0.05, na.rm = TRUE)
  
  p.y.norm.stm[n,] <- colMeans(temp.y.norm.stm < 0.05, na.rm = TRUE)
  p.y.lnorm.stm[n,] <- colMeans(temp.y.lnorm.stm < 0.05, na.rm = TRUE)
  p.y.chi.stm[n,] <- colMeans(temp.y.chi.stm < 0.05, na.rm = TRUE)
  p.y.N.stm[n,] <- colMeans(temp.y.N.stm < 0.05, na.rm = TRUE)
  p.y.N2.stm[n,] <- colMeans(temp.y.N2.stm < 0.05, na.rm = TRUE)
  p.y.NU.stm[n,] <- colMeans(temp.y.NU.stm < 0.05, na.rm = TRUE)
  
}) ### LOOP FOR SAMPLE SIZE ENDS

# Data preperation ----

prep.tm <- function(data, method){
  
  data <- data %>% 
    t() %>% 
    as.data.frame() %>% 
    mutate(alpha = alph,
           gamma = NA,
           method = rep(method, ncol(data)))
  
  names(data) <- c(NN, 'alpha' ,'gamma', 'method')
  
  data <- data %>% 
    pivot_longer(cols = 1:4) %>% 
    mutate(name = as.numeric(name)) %>% 
    arrange(name, alpha, gamma) %>% 
    pivot_wider(names_from = alpha) %>% 
    select(name, gamma, `0.05`, `0.1`,`0.15`,`0.2`, method)
  
  return(data)
} 
prep.stm <- function(data, method){
  
  data <- data %>% 
    t() %>% 
    as.data.frame() %>% 
    mutate(alpha = alph_gam$alpha,
           gamma = alph_gam$gamma,
           method = rep(method, ncol(data)))
  
  names(data) <- c(NN, 'alpha' ,'gamma', 'method')
  
  data <- data %>% 
    pivot_longer(cols = 1:4) %>% 
    mutate(name = as.numeric(name)) %>% 
    arrange(name, alpha, gamma) %>% 
    pivot_wider(names_from = alpha) %>% 
    select(name, gamma, `0.05`, `0.1`,`0.15`,`0.2`, method)
  
  return(data)
} 
prep.f <- function(data, method){
  
  # method <- c('F-test', 'Welch')
  data <- data %>% 
    t() %>% 
    as.data.frame() %>% 
    mutate(alpha = NA,
           gamma = NA,
           method = rep(method, ncol(data)))
  names(data) <- c(NN, 'alpha' ,'gamma', 'method')
  
  data <- data %>% 
    pivot_longer(cols = 1:4) %>% 
    mutate(name = as.numeric(name)) %>% 
    arrange(method, name, alpha, gamma) %>% 
    pivot_wider(names_from = alpha) %>% 
    select(name, gamma, `NA`, method)
  
  return(data)
} 

## Two sample ----

### Table 1 ----

norm <- full_join(rbind(prep.tm(tm.norm.y, 'Yuen TM'),
                  prep.tm(tm.norm.el, 'EL TM'),
                  prep.stm(stm.norm.y, 'Yuen STM'),
                  prep.stm(stm.norm.el, 'EL STM')), prep.f(t.norm, 't-test'))


t <- full_join(rbind(prep.tm(tm.t.y, 'Yuen TM'),
              prep.tm(tm.t.el, 'EL TM'),
              prep.stm(stm.t.y, 'Yuen STM'),
              prep.stm(stm.t.el, 'EL STM')), prep.f(t.t, 't-test'))

chi <- full_join(rbind(prep.tm(tm.chi.y, 'Yuen TM'),
              prep.tm(tm.chi.el, 'EL TM'),
              prep.stm(stm.chi.y, 'Yuen STM'),
              prep.stm(stm.chi.el, 'EL STM')), prep.f(t.chi, 't-test'))

N <- full_join(rbind(prep.tm(tm.N.y, 'Yuen TM'),
              prep.tm(tm.N.el, 'EL TM'),
              prep.stm(stm.N.y, 'Yuen STM'),
              prep.stm(stm.N.el, 'EL STM')), prep.f(t.N, 't-test'))

N2 <- full_join(rbind(prep.tm(tm.N2.y, 'Yuen TM'),
              prep.tm(tm.N2.el, 'EL TM'),
              prep.stm(stm.N2.y, 'Yuen STM'),
              prep.stm(stm.N2.el, 'EL STM')), prep.f(t.N2, 't-test'))

NU <- full_join(rbind(prep.tm(tm.NU.y, 'Yuen TM'),
              prep.tm(tm.NU.el, 'EL TM'),
              prep.stm(stm.NU.y, 'Yuen STM'),
              prep.stm(stm.NU.el, 'EL STM')), prep.f(t.NU, 't-test'))


# openxlsx::write.xlsx(norm, 'norm.xlsx', rowNames = F)
# openxlsx::write.xlsx(t, 't.xlsx', rowNames = F)
# openxlsx::write.xlsx(chi, 'chi.xlsx', rowNames = F)
# openxlsx::write.xlsx(N, 'N.xlsx', rowNames = F)
# openxlsx::write.xlsx(N2, 'N2.xlsx', rowNames = F)
# openxlsx::write.xlsx(NU, 'NU.xlsx', rowNames = F)

## Three sample ----
### Table 2 ----
norm.anova <- full_join(rbind(prep.tm(p.el.norm.tm, 'EL TM'),
                        prep.tm(p.y.norm.tm, 'Yuen TM'),
                        prep.stm(p.y.norm.stm, 'Yuen STM'),
                        prep.stm(p.el.norm.stm, 'EL STM')), 
                        rbind(prep.f(as.data.frame(p.aov.norm[,1]), 'F-test'),
                              prep.f(as.data.frame(p.aov.norm[,2]), 'Welch')))

lnorm.anova <- full_join(rbind(prep.tm(p.el.lnorm.tm, 'EL TM'),
                              prep.tm(p.y.lnorm.tm, 'Yuen TM'),
                              prep.stm(p.y.lnorm.stm, 'Yuen STM'),
                              prep.stm(p.el.lnorm.stm, 'EL STM')), 
                        rbind(prep.f(as.data.frame(p.aov.lnorm[,1]), 'F-test'),
                              prep.f(as.data.frame(p.aov.lnorm[,2]), 'Welch')))

chi.anova <- full_join(rbind(prep.tm(p.el.chi.tm, 'EL TM'),
                              prep.tm(p.y.chi.tm, 'Yuen TM'),
                              prep.stm(p.y.chi.stm, 'Yuen STM'),
                              prep.stm(p.el.chi.stm, 'EL STM')), 
                        rbind(prep.f(as.data.frame(p.aov.chi[,1]), 'F-test'),
                              prep.f(as.data.frame(p.aov.chi[,2]), 'Welch')))

N.anova <- full_join(rbind(prep.tm(p.el.N.tm, 'EL TM'),
                              prep.tm(p.y.N.tm, 'Yuen TM'),
                              prep.stm(p.y.N.stm, 'Yuen STM'),
                              prep.stm(p.el.N.stm, 'EL STM')), 
                        rbind(prep.f(as.data.frame(p.aov.N[,1]), 'F-test'),
                              prep.f(as.data.frame(p.aov.N[,2]), 'Welch')))

N2.anova <- full_join(rbind(prep.tm(p.el.N2.tm, 'EL TM'),
                              prep.tm(p.y.N2.tm, 'Yuen TM'),
                              prep.stm(p.y.N2.stm, 'Yuen STM'),
                              prep.stm(p.el.N2.stm, 'EL STM')), 
                        rbind(prep.f(as.data.frame(p.aov.N2[,1]), 'F-test'),
                              prep.f(as.data.frame(p.aov.N2[,2]), 'Welch')))

NU.anova <- full_join(rbind(prep.tm(p.el.NU.tm, 'EL TM'),
                              prep.tm(p.y.NU.tm, 'Yuen TM'),
                              prep.stm(p.y.NU.stm, 'Yuen STM'),
                              prep.stm(p.el.NU.stm, 'EL STM')), 
                        rbind(prep.f(as.data.frame(p.aov.NU[,1]), 'F-test'),
                              prep.f(as.data.frame(p.aov.NU[,2]), 'Welch')))


# openxlsx::write.xlsx(norm.anova, 'norm_anova.xlsx', rowNames = F)
# openxlsx::write.xlsx(lnorm.anova, 'lnorm_anova.xlsx', rowNames = F)
# openxlsx::write.xlsx(chi.anova, 'chi_anova.xlsx', rowNames = F)
# openxlsx::write.xlsx(N.anova, 'N_anova.xlsx', rowNames = F)
# openxlsx::write.xlsx(N2.anova, 'N2_anova.xlsx', rowNames = F)
# openxlsx::write.xlsx(NU.anova, 'NU_anova.xlsx', rowNames = F)


# t and F statistics ----

# norm
# lognorm


df.t.norm.stm <- matrix(ncol = 12, nrow = length(NN))
df.a.norm.stm <- matrix(ncol = 12, nrow = length(NN))
df.t.lnorm.stm <- matrix(ncol = 12, nrow = length(NN))
df.a.lnorm.stm <- matrix(ncol = 12, nrow = length(NN))

st.t.norm.stm <- matrix(ncol = 12, nrow = length(NN))
st.a.norm.stm <- matrix(ncol = 12, nrow = length(NN))
st.t.lnorm.stm <- matrix(ncol = 12, nrow = length(NN))
st.a.lnorm.stm <- matrix(ncol = 12, nrow = length(NN))

set.seed(1)
# iter <- 1000
system.time(for(n in 1:length(NN)){ ### LOOP FOR SAMPLE SIZE

  temp.t.df.norm <- matrix(ncol = 12, nrow = iter)
  temp.a.df.norm <- matrix(ncol = 12, nrow = iter)
  temp.t.st.norm <- matrix(ncol = 12, nrow = iter)
  temp.a.st.norm <- matrix(ncol = 12, nrow = iter)
  
  temp.t.df.lnorm <- matrix(ncol = 12, nrow = iter)
  temp.a.df.lnorm <- matrix(ncol = 12, nrow = iter)
  temp.t.st.lnorm <- matrix(ncol = 12, nrow = iter)
  temp.a.st.lnorm <- matrix(ncol = 12, nrow = iter)
  
  for(i in 1:iter){ ### LOOP FOR ITERATIONS
    
    x.norm <- rnorm(NN[n])
    y.norm <- rnorm(NN[n])
    ww.norm <- list(rnorm(NN[n]), rnorm(NN[n]), rnorm(NN[n]))
    
    x.lnorm <- rlnorm(NN[n])
    y.lnorm <- rlnorm(NN[n])
    ww.lnorm <- list(rlnorm(NN[n]), rlnorm(NN[n]), rlnorm(NN[n]))
    
    temp.alpha.t.df.norm <- matrix(ncol = 12, nrow = 1)
    temp.alpha.t.df.lnorm <- matrix(ncol = 12, nrow = 1)
    temp.alpha.a.df.norm <- matrix(ncol = 12, nrow = 1)
    temp.alpha.a.df.lnorm <- matrix(ncol = 12, nrow = 1)
    
    temp.alpha.t.st.norm <- matrix(ncol = 12, nrow = 1)
    temp.alpha.t.st.lnorm <- matrix(ncol = 12, nrow = 1)
    temp.alpha.a.st.norm <- matrix(ncol = 12, nrow = 1)
    temp.alpha.a.st.lnorm <- matrix(ncol = 12, nrow = 1)
    
    for(a in 1:length(alph)){ ### LOOP FOR ALPHAS
      
      gam <- alph_gam[alph_gam$alph == alph[a],]$gamma
      
      if(a == 1){
        indx <- 1
      }else if(a == 2){
        indx <- 5
      }else if(a == 3){
        indx <- 8
      }else{
        indx <- 11
      }
      
      temp.gam.t.df.norm <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.t.df.lnorm <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.a.df.norm <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.a.df.lnorm <- matrix(ncol = length(gam), nrow = 1)
      
      temp.gam.t.st.norm <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.t.st.lnorm <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.a.st.norm <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.a.st.lnorm <- matrix(ncol = length(gam), nrow = 1)
      
      for(g in 1:length(gam)){ ### LOOP FOR GAMMAS

        t.norm <- yuen.stm(x.norm, y.norm, alph[a], gam[g])
        a.norm <- ANOVA.stm(ww.norm, alph[a], gam[g])
        
        t.lnorm <- yuen.stm(x.lnorm, y.lnorm, alph[a], gam[g])
        a.lnorm <- ANOVA.stm(ww.lnorm, alph[a], gam[g])
        
        temp.gam.t.df.norm[1,g] <- t.norm$df
        temp.gam.t.st.norm[1,g] <- t.norm$st
        temp.gam.a.df.norm[1,g] <- a.norm$df[2]
        temp.gam.a.st.norm[1,g] <- a.norm$F
        
        temp.gam.t.df.lnorm[1,g] <- t.lnorm$df
        temp.gam.t.st.lnorm[1,g] <- t.lnorm$st
        temp.gam.a.df.lnorm[1,g] <- a.lnorm$df[2]
        temp.gam.a.st.lnorm[1,g] <- a.lnorm$F
        
      } ### LOOP FOR GAMMAS ENDS
      
      temp.alpha.t.df.norm[1,indx:(length(gam)+indx - 1)] <- temp.gam.t.df.norm
      temp.alpha.t.st.norm[1,indx:(length(gam)+indx - 1)] <- temp.gam.t.st.norm
      temp.alpha.a.df.norm[1,indx:(length(gam)+indx - 1)] <- temp.gam.a.df.norm
      temp.alpha.a.st.norm[1,indx:(length(gam)+indx - 1)] <- temp.gam.a.st.norm
      
      temp.alpha.t.df.lnorm[1,indx:(length(gam)+indx - 1)] <- temp.gam.t.df.lnorm
      temp.alpha.t.st.lnorm[1,indx:(length(gam)+indx - 1)] <- temp.gam.t.st.lnorm
      temp.alpha.a.df.lnorm[1,indx:(length(gam)+indx - 1)] <- temp.gam.a.df.lnorm
      temp.alpha.a.st.lnorm[1,indx:(length(gam)+indx - 1)] <- temp.gam.a.st.lnorm
      
    } ### LOOP FOR ALPHAS ENDS
    
    temp.t.df.norm[i,] <- temp.alpha.t.df.norm
    temp.a.df.norm[i,] <- temp.alpha.a.df.norm
    temp.t.st.norm[i,] <- temp.alpha.t.st.norm
    temp.a.st.norm[i,] <- temp.alpha.a.st.norm
    
    temp.t.df.lnorm[i,] <- temp.alpha.t.df.lnorm
    temp.a.df.lnorm[i,] <- temp.alpha.a.df.lnorm
    temp.t.st.lnorm[i,] <- temp.alpha.t.st.lnorm
    temp.a.st.lnorm[i,] <- temp.alpha.a.st.lnorm

    print(paste('i: ', i, '; ', 'n: ', n, sep = ''))
  } ### LOOP FOR ITERATIONS ENDS
  
  df.t.norm.stm[n,] <- colMeans(temp.t.df.norm, na.rm = TRUE)
  df.a.norm.stm[n,] <- colMeans(temp.a.df.norm, na.rm = TRUE)
  st.t.norm.stm[n,] <- as.vector(sapply(as.data.frame(temp.t.st.norm), function(x) quantile(x, probs = 0.975, na.rm = TRUE)))
  st.a.norm.stm[n,] <- as.vector(sapply(as.data.frame(temp.a.st.norm), function(x) quantile(x, probs = 0.975, na.rm = TRUE)))
  
  df.t.lnorm.stm[n,] <- colMeans(temp.t.df.lnorm, na.rm = TRUE)
  df.a.lnorm.stm[n,] <- colMeans(temp.a.df.lnorm, na.rm = TRUE)
  st.t.lnorm.stm[n,] <- as.vector(sapply(as.data.frame(temp.t.st.lnorm), function(x) quantile(x, probs = 0.975, na.rm = TRUE)))
  st.a.lnorm.stm[n,] <- as.vector(sapply(as.data.frame(temp.a.st.lnorm), function(x) quantile(x, probs = 0.975, na.rm = TRUE)))
  
})


### Table 3 ----
st_norm <- as.data.frame(rbind(prep.stm(st.t.norm.stm, 't_stm'),
                    prep.stm(st.a.norm.stm, 'a_stm'),
                    prep.stm(apply(df.t.norm.stm, c(1, 2), function(x) qt(0.975, x)),'t'),
                    prep.stm(apply(df.a.norm.stm, c(1, 2), function(x) qf(0.975, 2, x)),'F')))

st_lnorm <- as.data.frame(rbind(prep.stm(st.t.lnorm.stm, 't_stm'),
                    prep.stm(st.a.lnorm.stm, 'a_stm'),
                    prep.stm(apply(df.t.lnorm.stm, c(1, 2), function(x) qt(0.975, x)), 't'),
                    prep.stm(apply(df.a.lnorm.stm, c(1, 2), function(x) qf(0.975, 2, x)), 'F')))


# openxlsx::write.xlsx(st_norm, 'st_norm.xlsx')
# openxlsx::write.xlsx(st_lnorm, 'st_lnorm.xlsx')

# Confidence interval example ----


set.seed(1)
x <- replicate(100, gen.N())
y <- replicate(100, gen.N())

dati <- data.frame(x = c(x, y), gr = c(rep(0, 100), rep(1, 100)))

alpha <- c(0.05, 0.1, 0.15, 0.2)
gamma <- c(0.1, 0.2, 0.3, 0.4)

ci.stm.el<- data.frame(ub = c(
  -1*EL.stm(x, y, alpha[2], gamma[2])$conf.int[1],
  -1*EL.stm(x, y, alpha[2], gamma[3])$conf.int[1],
  -1*EL.stm(x, y, alpha[2], gamma[4])$conf.int[1],
  -1*EL.stm(x, y, alpha[3], gamma[2])$conf.int[1],
  -1*EL.stm(x, y, alpha[3], gamma[3])$conf.int[1],
  -1*EL.stm(x, y, alpha[3], gamma[4])$conf.int[1],
  -1*EL.stm(x, y, alpha[4], gamma[3])$conf.int[1],
  -1*EL.stm(x, y, alpha[4], gamma[4])$conf.int[1]),
  
  lb = c(
    -1*EL.stm(x, y, alpha[2], gamma[2])$conf.int[2],
    -1*     EL.stm(x, y, alpha[2], gamma[3])$conf.int[2],
    -1*     EL.stm(x, y, alpha[2], gamma[4])$conf.int[2],
    -1*     EL.stm(x, y, alpha[3], gamma[2])$conf.int[2],
    -1*     EL.stm(x, y, alpha[3], gamma[3])$conf.int[2],
    -1*     EL.stm(x, y, alpha[3], gamma[4])$conf.int[2],
    -1*     EL.stm(x, y, alpha[4], gamma[3])$conf.int[2],
    -1*     EL.stm(x, y, alpha[4], gamma[4])$conf.int[2]),
  
  est = c(
    -1* (ST_mean(y,alpha[2], gamma[2]) - ST_mean(x,alpha[2], gamma[2])),
    -1*   (ST_mean(y,alpha[2], gamma[2]) - ST_mean(x,alpha[2], gamma[3])),
    -1*     (ST_mean(y,alpha[2], gamma[2]) - ST_mean(x,alpha[2], gamma[4])),
    -1*     (ST_mean(y,alpha[2], gamma[2]) - ST_mean(x,alpha[3], gamma[2])),
    -1*     (ST_mean(y,alpha[2], gamma[2]) - ST_mean(x,alpha[3], gamma[3])),
    -1*     (ST_mean(y,alpha[2], gamma[2]) - ST_mean(x,alpha[3], gamma[4])),
    -1*     (ST_mean(y,alpha[2], gamma[2]) - ST_mean(x,alpha[4], gamma[3])),
    -1*     (ST_mean(y,alpha[2], gamma[2]) - ST_mean(x,alpha[4], gamma[4]))),
  alpha = c(alpha[2],alpha[2],alpha[2],
            alpha[3],alpha[3],alpha[3],
            alpha[4],alpha[4]),
  mean = 'STM',
  method = 'el',
  gamma = c(gamma[2],gamma[3],gamma[4],
            gamma[2],gamma[3],gamma[4],
            gamma[3],gamma[4]))


ci.stm.y<- data.frame(lb = c(
  yuen.stm(x, y, alpha[2], gamma[2])$conf.int[1],
  yuen.stm(x, y, alpha[2], gamma[3])$conf.int[1],
  yuen.stm(x, y, alpha[2], gamma[4])$conf.int[1],
  yuen.stm(x, y, alpha[3], gamma[2])$conf.int[1],
  yuen.stm(x, y, alpha[3], gamma[3])$conf.int[1],
  yuen.stm(x, y, alpha[3], gamma[4])$conf.int[1],
  yuen.stm(x, y, alpha[4], gamma[3])$conf.int[1],
  yuen.stm(x, y, alpha[4], gamma[4])$conf.int[1]),
  
  ub = c(
    yuen.stm(x, y, alpha[2], gamma[2])$conf.int[2],
    yuen.stm(x, y, alpha[2], gamma[3])$conf.int[2],
    yuen.stm(x, y, alpha[2], gamma[4])$conf.int[2],
    yuen.stm(x, y, alpha[3], gamma[2])$conf.int[2],
    yuen.stm(x, y, alpha[3], gamma[3])$conf.int[2],
    yuen.stm(x, y, alpha[3], gamma[4])$conf.int[2],
    yuen.stm(x, y, alpha[4], gamma[3])$conf.int[2],
    yuen.stm(x, y, alpha[4], gamma[4])$conf.int[2]),
  
  est = c(
    -1* (ST_mean(y,alpha[2], gamma[2]) - ST_mean(x,alpha[2], gamma[2])),
    -1*   (ST_mean(y,alpha[2], gamma[2]) - ST_mean(x,alpha[2], gamma[3])),
    -1*     (ST_mean(y,alpha[2], gamma[2]) - ST_mean(x,alpha[2], gamma[4])),
    -1*     (ST_mean(y,alpha[2], gamma[2]) - ST_mean(x,alpha[3], gamma[2])),
    -1*     (ST_mean(y,alpha[2], gamma[2]) - ST_mean(x,alpha[3], gamma[3])),
    -1*     (ST_mean(y,alpha[2], gamma[2]) - ST_mean(x,alpha[3], gamma[4])),
    -1*     (ST_mean(y,alpha[2], gamma[2]) - ST_mean(x,alpha[4], gamma[3])),
    -1*     (ST_mean(y,alpha[2], gamma[2]) - ST_mean(x,alpha[4], gamma[4]))),
  alpha = c(alpha[2],alpha[2],alpha[2],
            alpha[3],alpha[3],alpha[3],
            alpha[4],alpha[4]),
  mean = 'STM',
  method = 't',
  gamma = c(gamma[2],gamma[3],gamma[4],
            gamma[2],gamma[3],gamma[4],
            gamma[3],gamma[4]))

ci.tm.y<- data.frame(lb = c(
  yuen.tm(x~gr, data = dati, alpha[2])$conf.int[1],
  yuen.tm(x~gr, data = dati, alpha[3])$conf.int[1],
  yuen.tm(x~gr, data = dati, alpha[4])$conf.int[1]),
  
  ub = c(
    yuen.tm(x~gr, data = dati, alpha[2])$conf.int[2],
    yuen.tm(x~gr, data = dati, alpha[3])$conf.int[2],
    yuen.tm(x~gr, data = dati, alpha[4])$conf.int[2]),
  
  est = c(
    yuen.tm(x~gr, data = dati, alpha[2])$diff,
    yuen.tm(x~gr, data = dati, alpha[3])$diff,
    yuen.tm(x~gr, data = dati, alpha[4])$diff),
  alpha = c(alpha[2],alpha[3],alpha[4]),
  mean = 'TM',
  method = 't',
  gamma = NA)

ci.tm.el <- data.frame(lb = c(
  EL.tm(x, y, alpha = alpha[2],beta = alpha[2])$conf.int[1],
  EL.tm(x, y, alpha = alpha[3],beta= alpha[3])$conf.int[1],
  EL.tm(x, y, alpha = alpha[4],beta = alpha[4])$conf.int[1]),
  ub = c(
    EL.tm(x, y, alpha = alpha[2],beta = alpha[2])$conf.int[2],
    EL.tm(x, y, alpha = alpha[3],beta = alpha[3])$conf.int[2],
    EL.tm(x, y, alpha = alpha[4],beta = alpha[4])$conf.int[2]),
  
  est = c(
    EL.tm(x, y, alpha = alpha[2],beta = alpha[2])$estimate,
    EL.tm(x, y, alpha = alpha[3],beta = alpha[3])$estimate,
    EL.tm(x, y, alpha = alpha[4],beta = alpha[4])$estimate),
  alpha = c(alpha[2],alpha[3],alpha[4]),  
  mean = 'TM',
  method = 'el',
  gamma = NA)

ci.t <- data.frame(lb= t.test(x, y)$conf.int[1],
                   ub = t.test(x, y)$conf.int[2],
                   est = mean(y) - mean(x))



ci.los <- rbind(ci.stm.el,
                ci.stm.y,
                ci.tm.el,
                ci.tm.y)


ci.los <- ci.los %>% 
  arrange(alpha, method) %>% 
  mutate(ci_method = ifelse(mean == 'STM', 
                            paste0(method, "~", mean, "~alpha==", alpha, "*','~gamma==", gamma),
                            paste0(method, "~", mean, "~alpha==", alpha)))


ci.los %>% 
  rbind(data.frame(lb = t.test(x, y)$conf.int[1],
                   ub = t.test(x, y)$conf.int[2],
                   est = mean(x) - mean(y),
                   alpha =NA,
                   gamma =NA,
                   method = NA,
                   mean = NA,
                   ci_method = 't-test')) %>% 
  ggplot() +
  geom_point(aes(x = ci_method, y = est)) +
  geom_errorbar(aes(x = ci_method, ymin = lb, ymax = ub, col = as.character(alpha))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "none", panel.background = element_rect(fill = "white"),panel.grid.major = element_line(colour = "gray90")) +
  ylab('Confidence intervals') +
  xlab('Method')+
  scale_x_discrete(labels = function(x) parse(text = x))+
  geom_hline(yintercept = 0, lty = 2, size = 1)

ggsave('ci.png', bg = 'white')
