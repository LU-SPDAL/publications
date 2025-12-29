source('functions.R')
library(WRS2)
library(tidyverse)
library(rstatix)
library(EL)


# Figure 1 ----

x1 <- seq(-50,50,0.01)

ggplot(aes(x = x1, y = f.cn(x1)), data = NULL)+
  geom_line(linewidth = 1.2)+
  labs(x = 'x',
       y = expression(f[1]))+
  theme_bw()+
  theme(axis.text=element_text(size=12),
                   axis.title=element_text(size=20))
ggsave('F1.png')

x2 <- seq(-15,15,0.01)

ggplot(aes(x = x2, y = f.cn2(x2)), data = NULL)+
  geom_line(linewidth = 1.2)+
  labs(x = 'x',
       y = expression(f[2]))+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=20))
ggsave('F2.png')

set.seed(1)
x1 <- replicate(1000, gen.N())
x2 <- replicate(1000, gen.N2())

ggplot(aes(x = x1), data = NULL)+
  geom_histogram(aes(y = ..density..), bins = 25, col = 'black', fill = 'white')+
  xlab('F1')+
  theme_bw()
ggsave('F1.png')

ggplot(aes(x = x2), data = NULL)+
  geom_histogram(aes(y = ..density..), bins = 25, col = 'black', fill = 'white')+
  xlab('F2')+
  theme_bw()
ggsave('F2.png')
# Simulation study ----

## Parameters ----

NN <- c(50,100,200,500)
alph <- c(0.05,0.1,0.2)
gamm <- c(0.1,0.2,0.3,0.4)
alph_gam <- as.data.frame(cbind(c(rep(alph[1],4),rep(alph[2],3), rep(alph[3],2)),
                                c(gamm, gamm[-1], gamm[-(1:2)])))
names(alph_gam) <- c('alpha', 'gamma')
iter <- 10000

## 2 sample data sim ---- 

tm.norm.el <- as.data.frame(matrix(nrow = length(NN), ncol = 3))
tm.t.el <- as.data.frame(matrix(nrow = length(NN), ncol = 3))
tm.chi.el <- as.data.frame(matrix(nrow = length(NN), ncol = 3))
tm.N.el <- as.data.frame(matrix(nrow = length(NN), ncol = 3))
tm.N2.el <- as.data.frame(matrix(nrow = length(NN), ncol = 3))
tm.NU.el <- as.data.frame(matrix(nrow = length(NN), ncol = 3))

tm.norm.y <- as.data.frame(matrix(nrow = length(NN), ncol = 3))
tm.t.y <- as.data.frame(matrix(nrow = length(NN), ncol = 3))
tm.chi.y <- as.data.frame(matrix(nrow = length(NN), ncol = 3))
tm.N.y <- as.data.frame(matrix(nrow = length(NN), ncol = 3))
tm.N2.y <- as.data.frame(matrix(nrow = length(NN), ncol = 3))
tm.NU.y <- as.data.frame(matrix(nrow = length(NN), ncol = 3))

stm.norm.y <- as.data.frame(matrix(nrow = length(NN), ncol = 9))
stm.t.y <- as.data.frame(matrix(nrow = length(NN), ncol = 9))
stm.chi.y <- as.data.frame(matrix(nrow = length(NN), ncol = 9))
stm.N.y <- as.data.frame(matrix(nrow = length(NN), ncol = 9))
stm.N2.y <- as.data.frame(matrix(nrow = length(NN), ncol = 9))
stm.NU.y <- as.data.frame(matrix(nrow = length(NN), ncol = 9))

stm.norm.el <- as.data.frame(matrix(nrow = length(NN), ncol = 9))
stm.t.el <- as.data.frame(matrix(nrow = length(NN), ncol = 9))
stm.chi.el <- as.data.frame(matrix(nrow = length(NN), ncol = 9))
stm.N.el <- as.data.frame(matrix(nrow = length(NN), ncol = 9))
stm.N2.el <- as.data.frame(matrix(nrow = length(NN), ncol = 9))
stm.NU.el <- as.data.frame(matrix(nrow = length(NN), ncol = 9))

t.norm <- as.data.frame(matrix(nrow = length(NN), ncol = 1))
t.t <- as.data.frame(matrix(nrow = length(NN), ncol = 1))
t.chi <- as.data.frame(matrix(nrow = length(NN), ncol = 1))
t.N <- as.data.frame(matrix(nrow = length(NN), ncol = 1))
t.N2 <- as.data.frame(matrix(nrow = length(NN), ncol = 1))
t.NU <- as.data.frame(matrix(nrow = length(NN), ncol = 1))

t.norm.EL <- as.data.frame(matrix(nrow = length(NN), ncol = 1))
t.t.EL <- as.data.frame(matrix(nrow = length(NN), ncol = 1))
t.chi.EL <- as.data.frame(matrix(nrow = length(NN), ncol = 1))
t.N.EL <- as.data.frame(matrix(nrow = length(NN), ncol = 1))
t.N2.EL <- as.data.frame(matrix(nrow = length(NN), ncol = 1))
t.NU.EL <- as.data.frame(matrix(nrow = length(NN), ncol = 1))

set.seed(1)


system.time({
  for(n in 1:length(NN)){ ### LOOP FOR SAMPLE SIZE n
  # n <- 1
  # 
  t.temp.norm <- matrix(ncol = 1, nrow = iter)
  t.temp.t <- matrix(ncol = 1, nrow = iter)
  t.temp.chi <- matrix(ncol = 1, nrow = iter)
  t.temp.N <- matrix(ncol = 1, nrow = iter)
  t.temp.N2 <- matrix(ncol = 1, nrow = iter)
  t.temp.NU <- matrix(ncol = 1, nrow = iter)

  t.temp.norm.el <- matrix(ncol = 1, nrow = iter)
  t.temp.t.el <- matrix(ncol = 1, nrow = iter)
  t.temp.chi.el <- matrix(ncol = 1, nrow = iter)
  t.temp.N.el <- matrix(ncol = 1, nrow = iter)
  t.temp.N2.el <- matrix(ncol = 1, nrow = iter)
  t.temp.NU.el <- matrix(ncol = 1, nrow = iter)

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

    ci.N.el <- EL.means(x.N, y.N)$conf.int
    ci.N2.el <- EL.means(x.N2, y.N2)$conf.int
    ci.NU.el <- EL.means(x.NU, y.NU)$conf.int
    ci.norm.el <- EL.means(x.norm, y.norm)$conf.int
    ci.t.el <- EL.means(x.t, y.t)$conf.int
    ci.chi.el <- EL.means(x.chi, y.chi)$conf.int

    t.temp.N[i,1] <- coverage(0, ci.N.t[1], ci.N.t[2])
    t.temp.N2[i,1] <- coverage(0, ci.N2.t[1], ci.N2.t[2])
    t.temp.NU[i,1] <- coverage(0, ci.NU.t[1], ci.NU.t[2])
    t.temp.norm[i,1] <- coverage(0, ci.norm.t[1], ci.norm.t[2])
    t.temp.t[i,1] <- coverage(0, ci.t.t[1], ci.t.t[2])
    t.temp.chi[i,1] <- coverage(0, ci.chi.t[1], ci.chi.t[2])
    
    t.temp.N.el[i,1] <- coverage(0, ci.N.el[1], ci.N.el[2])
    t.temp.N2.el[i,1] <- coverage(0, ci.N2.el[1], ci.N2.el[2])
    t.temp.NU.el[i,1] <- coverage(0, ci.NU.el[1], ci.NU.el[2])
    t.temp.norm.el[i,1] <- coverage(0, ci.norm.el[1], ci.norm.el[2])
    t.temp.t.el[i,1] <- coverage(0, ci.t.el[1], ci.t.el[2])
    t.temp.chi.el[i,1] <- coverage(0, ci.chi.el[1], ci.chi.el[2])
    
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

      ci.N.el <- EL.tm(x.N, y.N, alpha = alph[a], beta = alph[a])$conf.int
      ci.N2.el <- EL.tm(x.N2, y.N2, alpha = alph[a], beta = alph[a])$conf.int
      ci.NU.el <- EL.tm(x.NU, y.NU, alpha = alph[a], beta = alph[a])$conf.int
      ci.norm.el <- EL.tm(x.norm, y.norm, alpha = alph[a], beta = alph[a])$conf.int
      ci.t.el <- EL.tm(x.t, y.t, alpha = alph[a], beta = alph[a])$conf.int
      ci.chi.el <- EL.tm(x.chi, y.chi, alpha = alph[a], beta = alph[a])$conf.int

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

  t.N.EL[n,] <- colMeans(t.temp.N.el, na.rm = TRUE)
  t.N2.EL[n,] <- colMeans(t.temp.N2.el, na.rm = TRUE)
  t.NU.EL[n,] <- colMeans(t.temp.NU.el, na.rm = TRUE)
  t.norm.EL[n,] <- colMeans(t.temp.norm.el, na.rm = TRUE)
  t.t.EL[n,] <- colMeans(t.temp.t.el, na.rm = TRUE)
  t.chi.EL[n,] <- colMeans(t.temp.chi.el, na.rm = TRUE)

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


## 3 sample data sim equal variance ----

p.aov.lnorm <- matrix(ncol = 2, nrow = length(NN))
p.aov.chi <- matrix(ncol = 2, nrow = length(NN))
p.aov.N <- matrix(ncol = 2, nrow = length(NN))

p.aov.lnorm.el <- matrix(ncol = 1, nrow = length(NN))
p.aov.chi.el <- matrix(ncol = 1, nrow = length(NN))
p.aov.N.el <- matrix(ncol = 1, nrow = length(NN))

p.y.lnorm.tm <- matrix(ncol = 3, nrow = length(NN))
p.y.chi.tm <- matrix(ncol = 3, nrow = length(NN))
p.y.N.tm <- matrix(ncol = 3, nrow = length(NN))

p.el.lnorm.tm <- matrix(ncol = 3, nrow = length(NN))
p.el.chi.tm <- matrix(ncol = 3, nrow = length(NN))
p.el.N.tm <- matrix(ncol = 3, nrow = length(NN))

p.el.lnorm.stm <- matrix(ncol = 9, nrow = length(NN))
p.el.chi.stm <- matrix(ncol = 9, nrow = length(NN))
p.el.N.stm <- matrix(ncol = 9, nrow = length(NN))

p.y.lnorm.stm <- matrix(ncol = 9, nrow = length(NN))
p.y.chi.stm <- matrix(ncol = 9, nrow = length(NN))
p.y.N.stm <- matrix(ncol = 9, nrow = length(NN))

# n <- 1

set.seed(1)
# iter <- 10000
system.time(for(n in 1:length(NN)){ ### LOOP FOR SAMPLE SIZE
  
  temp.aov.lnorm <- matrix(ncol = 2, nrow = iter)
  temp.aov.chi <- matrix(ncol = 2, nrow = iter)
  temp.aov.N <- matrix(ncol = 2, nrow = iter)
  
  temp.aov.lnorm.el <- matrix(ncol = 1, nrow = iter)
  temp.aov.chi.el <- matrix(ncol = 1, nrow = iter)
  temp.aov.N.el <- matrix(ncol = 1, nrow = iter)
  
  temp.y.lnorm.tm <- matrix(ncol = 4, nrow = iter)
  temp.y.chi.tm <- matrix(ncol = 4, nrow = iter)
  temp.y.N.tm <- matrix(ncol = 4, nrow = iter)

  temp.el.lnorm.tm <- matrix(ncol = 4, nrow = iter)
  temp.el.chi.tm <- matrix(ncol = 4, nrow = iter)
  temp.el.N.tm <- matrix(ncol = 4, nrow = iter)

  temp.el.lnorm.stm <- matrix(ncol = 12, nrow = iter)
  temp.el.chi.stm <- matrix(ncol = 12, nrow = iter)
  temp.el.N.stm <- matrix(ncol = 12, nrow = iter)

  temp.y.lnorm.stm <- matrix(ncol = 12, nrow = iter)
  temp.y.chi.stm <- matrix(ncol = 12, nrow = iter)
  temp.y.N.stm <- matrix(ncol = 12, nrow = iter)
  
  for(i in 1:iter){ ### LOOP FOR ITERATIONS
    
    ww.lnorm <- list(rlnorm(NN[n], sdlog = 2), rlnorm(NN[n], sdlog = 2), rlnorm(NN[n], sdlog = 2))
    ww.chi <- list(rchisq(NN[n], df = 3), rchisq(NN[n], df = 3), rchisq(NN[n], df = 3))
    ww.N <- list(replicate(NN[n], gen.N()), replicate(NN[n], gen.N()), replicate(NN[n], gen.N()))

    dati.lnorm <- data.frame(data = c(ww.lnorm[[1]], ww.lnorm[[2]], ww.lnorm[[3]]),
                             gr = c(rep(1, NN[n]), rep(2,NN[n]), rep(3,NN[n])))
    dati.chi <- data.frame(data = c(ww.chi[[1]], ww.chi[[2]], ww.chi[[3]]),
                           gr = c(rep(1, NN[n]), rep(2,NN[n]), rep(3,NN[n])))
    dati.N <- data.frame(data = c(ww.N[[1]], ww.N[[2]], ww.N[[3]]),
                         gr = c(rep(1, NN[n]), rep(2,NN[n]), rep(3,NN[n])))

    temp.aov.lnorm[i,1] <- summary(aov(data ~ gr, data = dati.lnorm))[[1]]$`Pr(>F)`[1]
    temp.aov.chi[i,1] <- summary(aov(data ~ gr, data = dati.chi))[[1]]$`Pr(>F)`[1]
    temp.aov.N[i,1] <- summary(aov(data ~ gr, data = dati.N))[[1]]$`Pr(>F)`[1]

    temp.aov.lnorm[i,2] <- welch_anova_test(dati.lnorm, data ~ gr)$p
    temp.aov.chi[i,2] <- welch_anova_test(dati.chi, data ~ gr)$p
    temp.aov.N[i,2] <- welch_anova_test(dati.N, data ~ gr)$p
    
    temp.aov.lnorm.el[i,] <- EL.anova(ww.lnorm)$pval
    temp.aov.chi.el[i,] <- EL.anova(ww.chi)$pval
    temp.aov.N.el[i,] <- EL.anova(ww.N)$pval
    

    temp.alph.N.el <- matrix(ncol = 12, nrow = 1)
    temp.alph.lnorm.el <- matrix(ncol = 12, nrow = 1)
    temp.alph.chi.el <- matrix(ncol = 12, nrow = 1)

    temp.alph.N.y <- matrix(ncol = 12, nrow = 1)
    temp.alph.lnorm.y <- matrix(ncol = 12, nrow = 1)
    temp.alph.chi.y <- matrix(ncol = 12, nrow = 1)

    for(a in 1:length(alph)){ ### LOOP FOR ALPHAS
      # a <- 1
      temp.y.lnorm.tm[i,a] <- t1way(data ~ gr, data = dati.lnorm, nboot = 1, tr = alph[a])$p.value
      temp.y.chi.tm[i,a] <- t1way(data ~ gr, data = dati.chi, nboot = 1, tr = alph[a])$p.value
      temp.y.N.tm[i,a] <- t1way(data ~ gr, data = dati.N, nboot = 1, tr = alph[a])$p.value

      temp.el.lnorm.tm[i,a] <- EL.anova.tm(ww.lnorm, alph[a], alph[a])$p.value
      temp.el.chi.tm[i,a] <- EL.anova.tm(ww.chi, alph[a], alph[a])$p.value
      temp.el.N.tm[i,a] <- EL.anova.tm(ww.N, alph[a], alph[a])$p.value

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

      temp.gam.N.el <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.lnorm.el <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.chi.el <- matrix(ncol = length(gam), nrow = 1)

      temp.gam.N.y <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.lnorm.y <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.chi.y <- matrix(ncol = length(gam), nrow = 1)

      for(g in 1:length(gam)){ ### LOOP FOR GAMMAS

        temp.gam.lnorm.el[1,g] <- EL.anova.stm(ww.lnorm, alph[a], gam[g])$p.value
        temp.gam.chi.el[1,g] <- EL.anova.stm(ww.chi, alph[a], gam[g])$p.value
        temp.gam.N.el[1,g] <- EL.anova.stm(ww.N, alph[a], gam[g])$p.value

        temp.gam.lnorm.y[1,g] <- ANOVA.stm(ww.lnorm, alph[a], gam[g])$p.value
        temp.gam.chi.y[1,g] <- ANOVA.stm(ww.chi, alph[a], gam[g])$p.value
        temp.gam.N.y[1,g] <- ANOVA.stm(ww.N, alph[a], gam[g])$p.value

      } ### LOOP FOR GAMMAS ENDS

      temp.alph.N.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.N.el
      temp.alph.lnorm.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.lnorm.el
      temp.alph.chi.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.chi.el

      temp.alph.N.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.N.y
      temp.alph.lnorm.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.lnorm.y
      temp.alph.chi.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.chi.y


    } ### LOOP FOR ALPHAS ENDS
    
    temp.el.N.stm[i,] <- temp.alph.N.el
    temp.el.lnorm.stm[i,] <- temp.alph.lnorm.el
    temp.el.chi.stm[i,] <- temp.alph.chi.el

    temp.y.N.stm[i,] <- temp.alph.N.y
    temp.y.lnorm.stm[i,] <- temp.alph.lnorm.y
    temp.y.chi.stm[i,] <- temp.alph.chi.y
    
    print(paste('i: ', i, '; ', 'n: ', n, sep = ''))
  } ### LOOP FOR ITERATIONS ENDS
  
  
  p.aov.lnorm[n,1] <- mean(temp.aov.lnorm[,1] < 0.05)
  p.aov.chi[n,1] <- mean(temp.aov.chi[,1] < 0.05)
  p.aov.N[n,1] <- mean(temp.aov.N[,1] < 0.05)

  p.aov.lnorm[n,2] <- mean(temp.aov.lnorm[,2] < 0.05)
  p.aov.chi[n,2] <- mean(temp.aov.chi[,2] < 0.05)
  p.aov.N[n,2] <- mean(temp.aov.N[,2] < 0.05)
  
  p.aov.lnorm.el[n,] <- mean(temp.aov.lnorm.el < 0.05)
  p.aov.chi.el[n,] <- mean(temp.aov.chi.el < 0.05)
  p.aov.N.el[n,] <- mean(temp.aov.N.el < 0.05)
  
  p.el.lnorm.tm[n,] <- colMeans(temp.el.lnorm.tm< 0.05, na.rm = TRUE)
  p.el.chi.tm[n,] <- colMeans(temp.el.chi.tm< 0.05, na.rm = TRUE)
  p.el.N.tm[n,] <- colMeans(temp.el.N.tm< 0.05, na.rm = TRUE)

  p.y.lnorm.tm[n,] <- colMeans(temp.y.lnorm.tm< 0.05, na.rm = TRUE)
  p.y.chi.tm[n,] <- colMeans(temp.y.chi.tm< 0.05, na.rm = TRUE)
  p.y.N.tm[n,] <- colMeans(temp.y.N.tm< 0.05, na.rm = TRUE)

  p.el.lnorm.stm[n,] <- colMeans(temp.el.lnorm.stm< 0.05, na.rm = TRUE)
  p.el.chi.stm[n,] <- colMeans(temp.el.chi.stm< 0.05, na.rm = TRUE)
  p.el.N.stm[n,] <- colMeans(temp.el.N.stm< 0.05, na.rm = TRUE)

  p.y.lnorm.stm[n,] <- colMeans(temp.y.lnorm.stm < 0.05, na.rm = TRUE)
  p.y.chi.stm[n,] <- colMeans(temp.y.chi.stm < 0.05, na.rm = TRUE)
  p.y.N.stm[n,] <- colMeans(temp.y.N.stm < 0.05, na.rm = TRUE)
  
}) ### LOOP FOR SAMPLE SIZE ENDS



## 3 sample data sim 1:4:9 variance ----

### Estimate population smoothly trimmed means ----

chi.stm <- matrix(ncol = 9, nrow = 10000)
ln.stm <- matrix(ncol = 9, nrow = 10000)
set.seed(1)
for(i in 1:10000){

  x1 <- rchisq(100000, df = 3)
  x2 <- rlnorm(100000, sdlog = 2)

  for(aa in 1:length(alph)){
    if(aa == 1){
      indx <- 0
    }else if(aa == 2){
      indx <- 4
    }else if(aa == 3){
      indx <- 7
    }else{
      indx <- 10
    }
    gam <- alph_gam[alph_gam$alpha == alph[aa],]$gamma
    for(g in 1:length(gam)){
      chi.stm[i,indx + g] <- ST_mean(x1, alph[aa], gam[g])
      ln.stm[i,indx + g] <- ST_mean(x2, alph[aa], gam[g])


    }
  }
  print(i)
}

stm.chi <- colMeans(chi.stm[,1:9])
stm.ln <- colMeans(ln.stm[,1:9])
tm.chi <- c(integrate(function(x) x*dchisq(x,3), lower=qchisq(alph[1],3), upper=qchisq(1-alph[1],3))$val/.9,
            integrate(function(x) x*dchisq(x,3), lower=qchisq(alph[2],3), upper=qchisq(1-alph[2],3))$val/.8,
            integrate(function(x) x*dchisq(x,3), lower=qchisq(alph[3],3), upper=qchisq(1-alph[3],3))$val/.6)
tm.ln <- c(integrate(function(x) x*dlnorm(x,sdlog = 2), lower = qlnorm(alph[1],sdlog = 2), upper=qlnorm(1-alph[1],sdlog = 2))$val/.9,
           integrate(function(x) x*dlnorm(x,sdlog = 2), lower = qlnorm(alph[2],sdlog = 2), upper=qlnorm(1-alph[2],sdlog = 2))$val/.8,
           integrate(function(x) x*dlnorm(x,sdlog = 2), lower = qlnorm(alph[3],sdlog = 2), upper=qlnorm(1-alph[3],sdlog = 2))$val/.6)


p.aov.lnorm.v2 <- matrix(ncol = 2, nrow = length(NN))
p.aov.chi.v2 <- matrix(ncol = 2, nrow = length(NN))
p.aov.N.v2 <- matrix(ncol = 2, nrow = length(NN))

p.aov.lnorm.v2.el <- matrix(ncol = 1, nrow = length(NN))
p.aov.chi.v2.el <- matrix(ncol = 1, nrow = length(NN))
p.aov.N.v2.el <- matrix(ncol = 1, nrow = length(NN))

p.y.lnorm.tm.v2 <- matrix(ncol = 3, nrow = length(NN))
p.y.chi.tm.v2 <- matrix(ncol = 3, nrow = length(NN))
p.y.N.tm.v2 <- matrix(ncol = 3, nrow = length(NN))

p.el.lnorm.tm.v2 <- matrix(ncol = 3, nrow = length(NN))
p.el.chi.tm.v2 <- matrix(ncol = 3, nrow = length(NN))
p.el.N.tm.v2 <- matrix(ncol = 3, nrow = length(NN))

p.el.lnorm.stm.v2 <- matrix(ncol = 9, nrow = length(NN))
p.el.chi.stm.v2 <- matrix(ncol = 9, nrow = length(NN))
p.el.N.stm.v2 <- matrix(ncol = 9, nrow = length(NN))

p.y.lnorm.stm.v2 <- matrix(ncol = 9, nrow = length(NN))
p.y.chi.stm.v2 <- matrix(ncol = 9, nrow = length(NN))
p.y.N.stm.v2 <- matrix(ncol = 9, nrow = length(NN))
# n <- 1
### Unequal variance samples ----

mu.chi <- 3
mu.ln <- exp(2)

sig0.chi <- 1/(sqrt(2*3))
sig0.ln <- 1/sqrt((exp(4) - 1) * exp(4))
sig1 <- 1
sig2 <- 2
sig3 <- 3

set.seed(1)
# iter <- 10000
system.time(for(n in 1:length(NN)){ ### LOOP FOR SAMPLE SIZE
  
  temp.aov.lnorm <- matrix(ncol = 2, nrow = iter)
  temp.aov.chi <- matrix(ncol = 2, nrow = iter)
  temp.aov.N <- matrix(ncol = 2, nrow = iter)
  
  temp.aov.lnorm.el <- matrix(ncol = 1, nrow = iter)
  temp.aov.chi.el <- matrix(ncol = 1, nrow = iter)
  temp.aov.N.el <- matrix(ncol = 1, nrow = iter)
  
  temp.y.lnorm.tm <- matrix(ncol = 3, nrow = iter)
  temp.y.chi.tm <- matrix(ncol = 3, nrow = iter)
  temp.y.N.tm <- matrix(ncol = 3, nrow = iter)

  temp.el.lnorm.tm <- matrix(ncol = 3, nrow = iter)
  temp.el.chi.tm <- matrix(ncol = 3, nrow = iter)
  temp.el.N.tm <- matrix(ncol = 3, nrow = iter)

  temp.el.lnorm.stm <- matrix(ncol = 9, nrow = iter)
  temp.el.chi.stm <- matrix(ncol = 9, nrow = iter)
  temp.el.N.stm <- matrix(ncol = 9, nrow = iter)

  temp.y.lnorm.stm <- matrix(ncol = 9, nrow = iter)
  temp.y.chi.stm <- matrix(ncol = 9, nrow = iter)
  temp.y.N.stm <- matrix(ncol = 9, nrow = iter)
  
  for(i in 1:iter){ ### LOOP FOR ITERATIONS
    
    ww.lnorm <- list(rlnorm(NN[n], sdlog = 2), rlnorm(NN[n], sdlog = 2), rlnorm(NN[n], sdlog = 2))
    ww.chi <- list(rchisq(NN[n], df = 3), rchisq(NN[n], df = 3), rchisq(NN[n], df = 3))
    ww.N <- list(replicate(NN[n], gen.N()), 2*replicate(NN[n], gen.N()), 3*replicate(NN[n], gen.N()))
    
    ww.ln.aov <- list(sig1*(ww.lnorm[[1]] - mu.ln)*sig0.ln, 
                      sig2*(ww.lnorm[[2]] - mu.ln)*sig0.ln,
                      sig3*(ww.lnorm[[3]] - mu.ln)*sig0.ln)
    
    ww.chi.aov <- list(sig1*(ww.chi[[1]] - mu.chi)*sig0.chi, 
                      sig2*(ww.chi[[2]] - mu.chi)*sig0.chi,
                      sig3*(ww.chi[[3]] - mu.chi)*sig0.chi)
    
    
    
    dati.lnorm.aov <- data.frame(data = c(ww.ln.aov[[1]], ww.ln.aov[[2]], ww.ln.aov[[3]]),
                             gr = c(rep(1, NN[n]), rep(2,NN[n]), rep(3,NN[n])))
    dati.chi.aov <- data.frame(data = c(ww.chi.aov[[1]], ww.chi.aov[[2]], ww.chi.aov[[3]]),
                           gr = c(rep(1, NN[n]), rep(2,NN[n]), rep(3,NN[n])))
    dati.N <- data.frame(data = c(ww.N[[1]], ww.N[[2]], ww.N[[3]]),
                         gr = c(rep(1, NN[n]), rep(2,NN[n]), rep(3,NN[n])))

    temp.aov.lnorm[i,1] <- summary(aov(data ~ gr, data = dati.lnorm.aov))[[1]]$`Pr(>F)`[1]
    temp.aov.chi[i,1] <- summary(aov(data ~ gr, data = dati.chi.aov))[[1]]$`Pr(>F)`[1]
    temp.aov.N[i,1] <- summary(aov(data ~ gr, data = dati.N))[[1]]$`Pr(>F)`[1]
    
    temp.aov.lnorm.el[i,1] <- EL.anova(ww.ln.aov)$pval
    temp.aov.chi.el[i,1] <- EL.anova(ww.chi.aov)$pval
    temp.aov.N.el[i,1] <- EL.anova(ww.N)$pval
    
    temp.alph.N.el <- matrix(ncol = 9, nrow = 1)
    temp.alph.lnorm.el <- matrix(ncol = 9, nrow = 1)
    temp.alph.chi.el <- matrix(ncol = 9, nrow = 1)

    temp.alph.N.y <- matrix(ncol = 9, nrow = 1)
    temp.alph.lnorm.y <- matrix(ncol = 9, nrow = 1)
    temp.alph.chi.y <- matrix(ncol = 9, nrow = 1)

    for(a in 1:length(alph)){ ### LOOP FOR ALPHAS
      # a <- 1
      ww.lnorm.tm <- list(sig1 * (ww.lnorm[[1]] - tm.ln[a]) * sig0.ln,
                          sig2 * (ww.lnorm[[2]] - tm.ln[a]) * sig0.ln,
                          sig3 * (ww.lnorm[[3]] - tm.ln[a]) * sig0.ln)

      dati.lnorm.tm <- data.frame(data = c(ww.lnorm.tm[[1]], ww.lnorm.tm[[2]], ww.lnorm.tm[[3]]),
                                   gr = c(rep(1, NN[n]), rep(2,NN[n]), rep(3,NN[n])))

      ww.chi.tm <- list(sig1 * (ww.chi[[1]] - tm.chi[a]) * sig0.chi,
                          sig2 * (ww.chi[[2]] - tm.chi[a]) * sig0.chi,
                          sig3 * (ww.chi[[3]] - tm.chi[a]) * sig0.chi)

      dati.lnorm.tm <- data.frame(data = c(ww.lnorm.tm[[1]], ww.lnorm.tm[[2]], ww.lnorm.tm[[3]]),
                                  gr = c(rep(1, NN[n]), rep(2,NN[n]), rep(3,NN[n])))
      dati.chi.tm <- data.frame(data = c(ww.chi.tm[[1]], ww.chi.tm[[2]], ww.chi.tm[[3]]),
                                  gr = c(rep(1, NN[n]), rep(2,NN[n]), rep(3,NN[n])))

      temp.y.lnorm.tm[i,a] <- t1way(data ~ gr, data = dati.lnorm.tm, nboot = 1, tr = alph[a])$p.value
      temp.y.chi.tm[i,a] <- t1way(data ~ gr, data = dati.chi.tm, nboot = 1, tr = alph[a])$p.value
      temp.y.N.tm[i,a] <- t1way(data ~ gr, data = dati.N, nboot = 1, tr = alph[a])$p.value

      temp.el.lnorm.tm[i,a] <- EL.anova.tm(ww.lnorm.tm, alph[a], alph[a])$p.value
      temp.el.chi.tm[i,a] <- EL.anova.tm(ww.chi.tm, alph[a], alph[a])$p.value
      temp.el.N.tm[i,a] <- EL.anova.tm(ww.N, alph[a], alph[a])$p.value


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

      temp.gam.N.el <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.lnorm.el <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.chi.el <- matrix(ncol = length(gam), nrow = 1)

      temp.gam.N.y <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.lnorm.y <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.chi.y <- matrix(ncol = length(gam), nrow = 1)

      for(g in 1:length(gam)){ ### LOOP FOR GAMMAS

        ww.lnorm.stm <- list(sig1 * (ww.lnorm[[1]] - stm.ln[indx-1+g]) * sig0.ln,
                            sig2 * (ww.lnorm[[2]] - stm.ln[indx-1+g]) * sig0.ln,
                            sig3 * (ww.lnorm[[3]] - stm.ln[indx-1+g]) * sig0.ln)

        ww.chi.stm <- list(sig1 * (ww.chi[[1]] - stm.chi[indx-1+g]) * sig0.chi,
                             sig2 * (ww.chi[[2]] - stm.chi[indx-1+g]) * sig0.chi,
                             sig3 * (ww.chi[[3]] - stm.chi[indx-1+g]) * sig0.chi)

        temp.gam.lnorm.el[1,g] <- EL.anova.stm(ww.lnorm.stm, alph[a], gam[g])$p.value
        temp.gam.chi.el[1,g] <- EL.anova.stm(ww.chi.stm, alph[a], gam[g])$p.value
        temp.gam.N.el[1,g] <- EL.anova.stm(ww.N, alph[a], gam[g])$p.value

        temp.gam.lnorm.y[1,g] <- ANOVA.stm(ww.lnorm.stm, alph[a], gam[g])$p.value
        temp.gam.chi.y[1,g] <- ANOVA.stm(ww.chi.stm, alph[a], gam[g])$p.value
        temp.gam.N.y[1,g] <- ANOVA.stm(ww.N, alph[a], gam[g])$p.value

      } ### LOOP FOR GAMMAS ENDS

      temp.alph.N.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.N.el
      temp.alph.lnorm.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.lnorm.el
      temp.alph.chi.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.chi.el

      temp.alph.N.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.N.y
      temp.alph.lnorm.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.lnorm.y
      temp.alph.chi.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.chi.y


    } ### LOOP FOR ALPHAS ENDS

    temp.el.N.stm[i,] <- temp.alph.N.el
    temp.el.lnorm.stm[i,] <- temp.alph.lnorm.el
    temp.el.chi.stm[i,] <- temp.alph.chi.el

    temp.y.N.stm[i,] <- temp.alph.N.y
    temp.y.lnorm.stm[i,] <- temp.alph.lnorm.y
    temp.y.chi.stm[i,] <- temp.alph.chi.y
  #   
    print(paste('i: ', i, '; ', 'n: ', n, sep = ''))
  } ### LOOP FOR ITERATIONS ENDS

  
  p.aov.lnorm.v2[n,1] <- mean(temp.aov.lnorm[,1] < 0.05)
  p.aov.chi.v2[n,1] <- mean(temp.aov.chi[,1] < 0.05)
  p.aov.N.v2[n,1] <- mean(temp.aov.N[,1] < 0.05)

  p.aov.lnorm.v2[n,2] <- mean(temp.aov.lnorm[,2] < 0.05)
  p.aov.chi.v2[n,2] <- mean(temp.aov.chi[,2] < 0.05)
  p.aov.N.v2[n,2] <- mean(temp.aov.N[,2] < 0.05)
  
  p.aov.lnorm.v2.el[n,] <- mean(temp.aov.lnorm.el < 0.05)
  p.aov.chi.v2.el[n,] <- mean(temp.aov.chi.el < 0.05)
  p.aov.N.v2.el[n,] <- mean(temp.aov.N.el < 0.05)
  
  p.el.lnorm.tm.v2[n,] <- colMeans(temp.el.lnorm.tm< 0.05, na.rm = TRUE)
  p.el.chi.tm.v2[n,] <- colMeans(temp.el.chi.tm< 0.05, na.rm = TRUE)
  p.el.N.tm.v2[n,] <- colMeans(temp.el.N.tm< 0.05, na.rm = TRUE)

  p.y.lnorm.tm.v2[n,] <- colMeans(temp.y.lnorm.tm< 0.05, na.rm = TRUE)
  p.y.chi.tm.v2[n,] <- colMeans(temp.y.chi.tm< 0.05, na.rm = TRUE)
  p.y.N.tm.v2[n,] <- colMeans(temp.y.N.tm< 0.05, na.rm = TRUE)

  p.el.lnorm.stm.v2[n,] <- colMeans(temp.el.lnorm.stm< 0.05, na.rm = TRUE)
  p.el.chi.stm.v2[n,] <- colMeans(temp.el.chi.stm< 0.05, na.rm = TRUE)
  p.el.N.stm.v2[n,] <- colMeans(temp.el.N.stm< 0.05, na.rm = TRUE)

  p.y.lnorm.stm.v2[n,] <- colMeans(temp.y.lnorm.stm < 0.05, na.rm = TRUE)
  p.y.chi.stm.v2[n,] <- colMeans(temp.y.chi.stm < 0.05, na.rm = TRUE)
  p.y.N.stm.v2[n,] <- colMeans(temp.y.N.stm < 0.05, na.rm = TRUE)
  
}) ### LOOP FOR SAMPLE SIZE ENDS

## Data preperation ----

prep.tm <- function(data, method){
  # data <- st.a.lnorm.stm
  
  data <- data %>% 
    t() %>% 
    as.data.frame() %>% 
    mutate(alpha = alph,
           gamma = NA,
           method = rep(method, ncol(data)))
  
  names(data) <- c(NN, 'alpha' ,'gamma', 'method')
  
  data <- data %>% 
    pivot_longer(cols = 1:length(NN)) %>% 
    mutate(name = as.numeric(name)) %>% 
    arrange(name, alpha, gamma) %>% 
    pivot_wider(names_from = alpha) %>% 
    select(name, gamma, `0.05`, `0.1`,`0.2`, method)
  
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
    pivot_longer(cols = 1:length(NN)) %>% 
    mutate(name = as.numeric(name)) %>% 
    arrange(name, alpha, gamma) %>% 
    pivot_wider(names_from = alpha) %>% 
    select(name, gamma, `0.05`, `0.1`,`0.2`, method)
  
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
    pivot_longer(cols = 1:length(NN)) %>% 
    mutate(name = as.numeric(name)) %>% 
    arrange(method, name, alpha, gamma) %>% 
    pivot_wider(names_from = alpha) %>% 
    select(name, gamma, `NA`, method)
  
  return(data)
} 

### Two sample ----

#### Table 2 ----

norm <- full_join(rbind(prep.tm(tm.norm.y, 'Yuen TM'),
                  prep.tm(tm.norm.el, 'EL TM'),
                  prep.stm(stm.norm.y, 'Yuen STM'),
                  prep.stm(stm.norm.el, 'EL STM')), prep.f(t.norm, 't-test'), prep(t.norm.EL, 'EL'))


t <- full_join(rbind(prep.tm(tm.t.y, 'Yuen TM'),
              prep.tm(tm.t.el, 'EL TM'),
              prep.stm(stm.t.y, 'Yuen STM'),
              prep.stm(stm.t.el, 'EL STM')), prep.f(t.t, 't-test'), prep(t.t.EL, 'EL'))

chi <- full_join(rbind(prep.tm(tm.chi.y, 'Yuen TM'),
              prep.tm(tm.chi.el, 'EL TM'),
              prep.stm(stm.chi.y, 'Yuen STM'),
              prep.stm(stm.chi.el, 'EL STM')), prep.f(t.chi, 't-test'), prep(t.chi.EL, 'EL'))

N <- full_join(rbind(prep.tm(tm.N.y, 'Yuen TM'),
              prep.tm(tm.N.el, 'EL TM'),
              prep.stm(stm.N.y, 'Yuen STM'),
              prep.stm(stm.N.el, 'EL STM')), prep.f(t.N, 't-test'), prep(t.N.EL, 'EL'))

N2 <- full_join(rbind(prep.tm(tm.N2.y, 'Yuen TM'),
              prep.tm(tm.N2.el, 'EL TM'),
              prep.stm(stm.N2.y, 'Yuen STM'),
              prep.stm(stm.N2.el, 'EL STM')), prep.f(t.N2, 't-test'), prep(t.N2.EL, 'EL'))

NU <- full_join(rbind(prep.tm(tm.NU.y, 'Yuen TM'),
              prep.tm(tm.NU.el, 'EL TM'),
              prep.stm(stm.NU.y, 'Yuen STM'),
              prep.stm(stm.NU.el, 'EL STM')), prep.f(t.NU, 't-test'), prep(t.NU.EL, 'EL'))


# openxlsx::write.xlsx(norm, 'norm.xlsx', rowNames = F)
# openxlsx::write.xlsx(t, 't.xlsx', rowNames = F)
# openxlsx::write.xlsx(chi, 'chi.xlsx', rowNames = F)
# openxlsx::write.xlsx(N, 'N.xlsx', rowNames = F)
# openxlsx::write.xlsx(N2, 'N2.xlsx', rowNames = F)
# openxlsx::write.xlsx(NU, 'NU.xlsx', rowNames = F)

### Three sample ----
#### Table 4 ----


lnorm.anova <- full_join(rbind(prep.tm(p.el.lnorm.tm, 'EL TM'),
                              prep.tm(p.y.lnorm.tm, 'Yuen TM'),
                              prep.stm(p.y.lnorm.stm, 'Yuen STM'),
                              prep.stm(p.el.lnorm.stm, 'EL STM')), 
                        rbind(prep.f(as.data.frame(p.aov.lnorm[,1]), 'F-test'),
                              prep.f(as.data.frame(p.aov.lnorm[,2]), 'Welch'),
                              prep.f(as.data.frame(p.aov.lnorm.el[,2]), 'EL')))

chi.anova <- full_join(rbind(prep.tm(p.el.chi.tm, 'EL TM'),
                              prep.tm(p.y.chi.tm, 'Yuen TM'),
                              prep.stm(p.y.chi.stm, 'Yuen STM'),
                              prep.stm(p.el.chi.stm, 'EL STM')), 
                        rbind(prep.f(as.data.frame(p.aov.chi[,1]), 'F-test'),
                              prep.f(as.data.frame(p.aov.chi[,2]), 'Welch'),
                              prep.f(as.data.frame(p.aov.chi.el[,2]), 'EL')))

N.anova <- full_join(rbind(prep.tm(p.el.N.tm, 'EL TM'),
                              prep.tm(p.y.N.tm, 'Yuen TM'),
                              prep.stm(p.y.N.stm, 'Yuen STM'),
                              prep.stm(p.el.N.stm, 'EL STM')), 
                        rbind(prep.f(as.data.frame(p.aov.N[,1]), 'F-test'),
                              prep.f(as.data.frame(p.aov.N[,2]), 'Welch'),
                              prep.f(as.data.frame(p.aov.N.el[,2]), 'EL')))


# openxlsx::write.xlsx(lnorm.anova, 'lnorm_anova.xlsx', rowNames = F)
# openxlsx::write.xlsx(chi.anova, 'chi_anova.xlsx', rowNames = F)
# openxlsx::write.xlsx(N.anova, 'N_anova.xlsx', rowNames = F)

#### Table 5 ----


lnorm.anova.v2 <- full_join(rbind(prep.tm(p.el.lnorm.tm.v2, 'EL TM'),
                               prep.tm(p.y.lnorm.tm.v2, 'Yuen TM'),
                               prep.stm(p.y.lnorm.stm.v2, 'Yuen STM'),
                               prep.stm(p.el.lnorm.stm.v2, 'EL STM')), 
                         rbind(prep.f(as.data.frame(p.aov.lnorm.v2[,1]), 'F-test'),
                               prep.f(as.data.frame(p.aov.lnorm.v2[,2]), 'Welch'),
                               prep.f(as.data.frame(p.aov.lnorm.v2.el[,2]), 'EL')))

chi.anova.v2 <- full_join(rbind(prep.tm(p.el.chi.tm.v2, 'EL TM'),
                             prep.tm(p.y.chi.tm.v2, 'Yuen TM'),
                             prep.stm(p.y.chi.stm.v2, 'Yuen STM'),
                             prep.stm(p.el.chi.stm.v2, 'EL STM')), 
                       rbind(prep.f(as.data.frame(p.aov.chi.v2[,1]), 'F-test'),
                             prep.f(as.data.frame(p.aov.chi.v2[,2]), 'Welch'),
                             prep.f(as.data.frame(p.aov.chi.v2.el[,2]), 'EL')))

N.anova.v2 <- full_join(rbind(prep.tm(p.el.N.tm.v2, 'EL TM'),
                           prep.tm(p.y.N.tm.v2, 'Yuen TM'),
                           prep.stm(p.y.N.stm.v2, 'Yuen STM'),
                           prep.stm(p.el.N.stm.v2, 'EL STM')), 
                     rbind(prep.f(as.data.frame(p.aov.N.v2[,1]), 'F-test'),
                           prep.f(as.data.frame(p.aov.N.v2[,2]), 'Welch'),
                           prep.f(as.data.frame(p.aov.N.v2.el[,2]), 'EL')))


# openxlsx::write.xlsx(lnorm.anova.v2, 'lnorm_anova_v2.xlsx', rowNames = F)
# openxlsx::write.xlsx(chi.anova.v2, 'chi_anova_v2.xlsx', rowNames = F)
# openxlsx::write.xlsx(N.anova.v2, 'N_anova_v2.xlsx', rowNames = F)

## t and F statistics ----

# norm
# lognorm




NN2 <- c(50,100,1000)
alph2 <- c(0.05,0.2)
gamm2 <- c(0.1,0.4)
alph_gam2 <- data.frame(alpha = c(0.05, 0.05, 0.2),
                        gamma = c(0.1, 0.4, 0.4))
names(alph_gam2) <- c('alpha', 'gamma')
iter <- 10000
set.seed(56464186)

st.t.norm.stm.90 <- matrix(ncol = nrow(alph_gam2), nrow = length(NN2))
st.a.norm.stm.90 <- matrix(ncol = nrow(alph_gam2), nrow = length(NN2))

st.t.norm.stm.95 <- matrix(ncol = nrow(alph_gam2), nrow = length(NN2))
st.a.norm.stm.95 <- matrix(ncol = nrow(alph_gam2), nrow = length(NN2))

st.t.norm.stm.975 <- matrix(ncol = nrow(alph_gam2), nrow = length(NN2))
st.a.norm.stm.975 <- matrix(ncol = nrow(alph_gam2), nrow = length(NN2))


# iter <- 1000
### Loop for statistics ----
system.time(for(n in 1:length(NN2)){ ### LOOP FOR SAMPLE SIZE
 
  temp.t.st.norm <- matrix(ncol = nrow(alph_gam2), nrow = iter)
  temp.a.st.norm <- matrix(ncol = nrow(alph_gam2), nrow = iter)
  
  for(i in 1:iter){ ### LOOP FOR ITERATIONS
 
    x.norm <- rnorm(NN2[n])
    y.norm <- rnorm(NN2[n])
    
    ww.norm <- list(rnorm(NN2[n]), rnorm(NN2[n]), rnorm(NN2[n]))
    
    
    for(j in 1:nrow(alph_gam2)){ ### LOOP FOR ALPHAS
        
        t.norm <- yuen.stm(x.norm, y.norm, alph_gam2$alpha[j], alph_gam2$gamma[j])$st
        a.norm <- ANOVA.stm(ww.norm, alph_gam2$alpha[j], alph_gam2$gamma[j])$F
        
        temp.t.st.norm[i,j] <- t.norm
        temp.a.st.norm[i,j] <- a.norm
  
      
    } ### LOOP FOR ALPHAS ENDS
    
    
    
    print(paste('i: ', i, '; ', 'n: ', n, sep = ''))
  } ### LOOP FOR ITERATIONS ENDS
  
  st.t.norm.stm.90[n,] <- as.vector(sapply(as.data.frame(temp.t.st.norm), function(x) quantile(x, probs = 0.90, na.rm = TRUE)))
  st.a.norm.stm.90[n,] <- as.vector(sapply(as.data.frame(temp.a.st.norm), function(x) quantile(x, probs = 0.90, na.rm = TRUE)))
  
  st.t.norm.stm.95[n,] <- as.vector(sapply(as.data.frame(temp.t.st.norm), function(x) quantile(x, probs = 0.95, na.rm = TRUE)))
  st.a.norm.stm.95[n,] <- as.vector(sapply(as.data.frame(temp.a.st.norm), function(x) quantile(x, probs = 0.95, na.rm = TRUE)))
  
  st.t.norm.stm.975[n,] <- as.vector(sapply(as.data.frame(temp.t.st.norm), function(x) quantile(x, probs = 0.975, na.rm = TRUE)))
  st.a.norm.stm.975[n,] <- as.vector(sapply(as.data.frame(temp.a.st.norm), function(x) quantile(x, probs = 0.975, na.rm = TRUE)))
})

df.t.norm.stm <- matrix(ncol = nrow(alph_gam2), nrow = length(NN))
df.a.norm.stm <- matrix(ncol = nrow(alph_gam2), nrow = length(NN))


sapply(as.data.frame(matrix(c(1,1,0,1,2,0,1,3,0), byrow = T, nrow = 3, ncol = 3)), function(x) mean(x))

### Loop for degrees of freedom ----
set.seed(2)
iter <- 100000

system.time(for(n in 1:length(NN2)){ ### LOOP FOR SAMPLE SIZE
  
  temp.t.df.norm <- matrix(ncol = nrow(alph_gam2), nrow = iter)
  temp.a.df.norm <- matrix(ncol = nrow(alph_gam2), nrow = iter)
  
  temp.t.df.lnorm <- matrix(ncol = nrow(alph_gam2), nrow = iter)
  temp.a.df.lnorm <- matrix(ncol = nrow(alph_gam2), nrow = iter)
  
  for(i in 1:iter){ ### LOOP FOR ITERATIONS
    
    x.norm <- rnorm(NN2[n])
    y.norm <- rnorm(NN2[n])
    ww.norm <- list(rnorm(NN2[n]), rnorm(NN2[n]), rnorm(NN2[n]))
    
    x.lnorm <- rlnorm(NN2[n])
    y.lnorm <- rlnorm(NN2[n])
    ww.lnorm <- list(rlnorm(NN2[n]), rlnorm(NN2[n]), rlnorm(NN2[n]))
    
    temp.alpha.t.df.norm <- matrix(ncol = nrow(alph_gam2), nrow = 1)
    temp.alpha.t.df.lnorm <- matrix(ncol = nrow(alph_gam2), nrow = 1)
    temp.alpha.a.df.norm <- matrix(ncol = nrow(alph_gam2), nrow = 1)
    temp.alpha.a.df.lnorm <- matrix(ncol = nrow(alph_gam2), nrow = 1)
    
    for(a in 1:length(alph2)){ ### LOOP FOR ALPHAS
      
      gam <- alph_gam2[alph_gam2$alpha == alph[a],]$gamma
      
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
      
      for(g in 1:length(gam)){ ### LOOP FOR GAMMAS
        
        t.norm <- yuen.stm(x.norm, y.norm, alph2[a], gam[g])
        a.norm <- ANOVA.stm(ww.norm, alph2[a], gam[g])
        
        t.lnorm <- yuen.stm(x.lnorm, y.lnorm, alph2[a], gam[g])
        a.lnorm <- ANOVA.stm(ww.lnorm, alph2[a], gam[g])
        
        temp.gam.t.df.norm[1,g] <- t.norm$df
        temp.gam.a.df.norm[1,g] <- a.norm$df[2]
        
        temp.gam.t.df.lnorm[1,g] <- t.lnorm$df
        temp.gam.a.df.lnorm[1,g] <- a.lnorm$df[2]
        
      } ### LOOP FOR GAMMAS ENDS
      
      temp.alpha.t.df.norm[1,indx:(length(gam)+indx - 1)] <- temp.gam.t.df.norm
      temp.alpha.a.df.norm[1,indx:(length(gam)+indx - 1)] <- temp.gam.a.df.norm
      
      temp.alpha.t.df.lnorm[1,indx:(length(gam)+indx - 1)] <- temp.gam.t.df.lnorm
      temp.alpha.a.df.lnorm[1,indx:(length(gam)+indx - 1)] <- temp.gam.a.df.lnorm
      
    } ### LOOP FOR ALPHAS ENDS
    
    temp.t.df.norm[i,] <- temp.alpha.t.df.norm
    temp.a.df.norm[i,] <- temp.alpha.a.df.norm
    
    temp.t.df.lnorm[i,] <- temp.alpha.t.df.lnorm
    temp.a.df.lnorm[i,] <- temp.alpha.a.df.lnorm
    
    print(paste('i: ', i, '; ', 'n: ', n, sep = ''))
  } ### LOOP FOR ITERATIONS ENDS
  
  
  
  df.t.norm.stm[n,] <- colMeans(temp.t.df.norm, na.rm = TRUE)
  df.a.norm.stm[n,] <- colMeans(temp.a.df.norm, na.rm = TRUE)
  
  df.t.lnorm.stm[n,] <- colMeans(temp.t.df.lnorm, na.rm = TRUE)
  df.a.lnorm.stm[n,] <- colMeans(temp.a.df.lnorm, na.rm = TRUE)
  
})


#### Table 1 ----
st_norm.90 <- as.data.frame(rbind(prep.stm(st.t.norm.stm.90, 't_stm'),
                    prep.stm(st.a.norm.stm.90, 'a_stm'),
                    prep.stm(apply(df.t.norm.stm, c(1, 2), function(x) qt(0.90, x)),'t'),
                    prep.stm(apply(df.a.norm.stm, c(1, 2), function(x) qf(0.90, 2, x)),'F')))

st_lnorm.90 <- as.data.frame(rbind(prep.stm(st.t.lnorm.stm.90, 't_stm'),
                    prep.stm(st.a.lnorm.stm.90, 'a_stm'),
                    prep.stm(apply(df.t.lnorm.stm, c(1, 2), function(x) qt(0.90, x)), 't'),
                    prep.stm(apply(df.a.lnorm.stm, c(1, 2), function(x) qf(0.90, 2, x)), 'F')))


st_norm.95 <- as.data.frame(rbind(prep.stm(st.t.norm.stm.95, 't_stm'),
                                  prep.stm(st.a.norm.stm.95, 'a_stm'),
                                  prep.stm(apply(df.t.norm.stm, c(1, 2), function(x) qt(0.95, x)),'t'),
                                  prep.stm(apply(df.a.norm.stm, c(1, 2), function(x) qf(0.95, 2, x)),'F')))

st_lnorm.95 <- as.data.frame(rbind(prep.stm(st.t.lnorm.stm.95, 't_stm'),
                                   prep.stm(st.a.lnorm.stm.95, 'a_stm'),
                                   prep.stm(apply(df.t.lnorm.stm, c(1, 2), function(x) qt(0.95, x)), 't'),
                                   prep.stm(apply(df.a.lnorm.stm, c(1, 2), function(x) qf(0.95, 2, x)), 'F')))

st_norm.975 <- as.data.frame(rbind(prep.stm(st.t.norm.stm.975, 't_stm'),
                                  prep.stm(st.a.norm.stm.975, 'a_stm'),
                                  prep.stm(apply(df.t.norm.stm, c(1, 2), function(x) qt(0.975, x)),'t'),
                                  prep.stm(apply(df.a.norm.stm, c(1, 2), function(x) qf(0.975, 2, x)),'F')))

st_lnorm.975 <- as.data.frame(rbind(prep.stm(st.t.lnorm.stm.975, 't_stm'),
                                   prep.stm(st.a.lnorm.stm.975, 'a_stm'),
                                   prep.stm(apply(df.t.lnorm.stm, c(1, 2), function(x) qt(0.975, x)), 't'),
                                   prep.stm(apply(df.a.lnorm.stm, c(1, 2), function(x) qf(0.975, 2, x)), 'F')))


openxlsx::write.xlsx(st_norm.90, 'st_norm_90.xlsx')
openxlsx::write.xlsx(st_lnorm.90, 'st_lnorm_90.xlsx')

openxlsx::write.xlsx(st_norm.95, 'st_norm_95.xlsx')
openxlsx::write.xlsx(st_lnorm.95, 'st_lnorm_95.xlsx')

openxlsx::write.xlsx(st_norm.975, 'st_norm_975.xlsx')
openxlsx::write.xlsx(st_lnorm.975, 'st_lnorm_975.xlsx')

## Confidence interval example ----
library(PairedData)


ci.len.t.N <- numeric(iter)
ci.len.el.N <- numeric(iter)
ci.len.tm.y.N <- numeric(iter)
ci.len.tm.el.N <- numeric(iter)
ci.len.stm.y.N <- numeric(iter)
ci.len.stm.el.N <- numeric(iter)

ci.len.t.N2 <- numeric(iter)
ci.len.el.N2 <- numeric(iter)
ci.len.tm.y.N2 <- numeric(iter)
ci.len.tm.el.N2 <- numeric(iter)
ci.len.stm.y.N2 <- numeric(iter)
ci.len.stm.el.N2 <- numeric(iter)

n <- 50
set.seed(1)
for(i in 1:iter){
  x.N <- replicate(n, gen.N())
  y.N <- replicate(n, gen.N())
  
  x.N2 <- replicate(n, gen.N2())
  y.N2 <- replicate(n, gen.N2())
  
  ci.t.N <- t.test(x.N, y.N)$conf.int
  ci.el.N <- EL.means(x.N, y.N)$conf.int
  ci.tm.y.N <- yuen.t.test(x.N, y.N, tr = 0.05)$conf.int
  ci.tm.el.N <- EL.tm(x.N, y.N, alpha = 0.05, beta = 0.05)$conf.int
  ci.stm.y.N <- yuen.stm(x.N, y.N, alpha = 0.05, gamma = 0.1)$conf.int
  ci.stm.el.N <- EL.stm(x.N, y.N, alpha = 0.05, gamma = 0.1)$conf.int
  
  ci.t.N2 <- t.test(x.N2, y.N2)$conf.int
  ci.el.N2 <- EL.means(x.N2, y.N2)$conf.int
  ci.tm.y.N2 <- yuen.t.test(x.N2, y.N2, tr = 0.05)$conf.int
  ci.tm.el.N2 <- EL.tm(x.N2, y.N2, alpha = 0.05, beta = 0.05)$conf.int
  ci.stm.y.N2 <- yuen.stm(x.N2, y.N2, alpha = 0.05, gamma = 0.1)$conf.int
  ci.stm.el.N2 <- EL.stm(x.N2, y.N2, alpha = 0.05, gamma = 0.1)$conf.int
  
  ci.len.t.N[i] <- ci.t.N[2] - ci.t.N[1]
  ci.len.el.N[i] <- ci.el.N[2] - ci.el.N[1]
  ci.len.tm.y.N[i] <- ci.tm.y.N[2] - ci.tm.y.N[1]
  ci.len.tm.el.N[i] <- ci.tm.el.N[2] - ci.tm.el.N[1]
  ci.len.stm.y.N[i] <- ci.stm.y.N[2] - ci.stm.y.N[1]
  ci.len.stm.el.N[i] <- ci.stm.el.N[2] - ci.stm.el.N[1]
  
  ci.len.t.N2[i] <- ci.t.N2[2] - ci.t.N2[1]
  ci.len.el.N2[i] <- ci.el.N2[2] - ci.el.N2[1]
  ci.len.tm.y.N2[i] <- ci.tm.y.N2[2] - ci.tm.y.N2[1]
  ci.len.tm.el.N2[i] <- ci.tm.el.N2[2] - ci.tm.el.N2[1]
  ci.len.stm.y.N2[i] <- ci.stm.y.N2[2] - ci.stm.y.N2[1]
  ci.len.stm.el.N2[i] <- ci.stm.el.N2[2] - ci.stm.el.N2[1]
  
  print(i)
  
}

#### Table 3 ----
data.frame(method = c('t-test', 'EL', 't_TM', 'EL_TM', 't_ST', 'EL_STM'),
  ci.len = c(mean(ci.len.t.N), mean(ci.len.el.N),
                      mean(ci.len.tm.y.N), mean(ci.len.tm.el.N),
                      mean(ci.len.stm.y.N), mean(ci.len.stm.el.N, na.rm = TRUE)))

data.frame(method = c('t-test', 'EL', 't_TM', 'EL_TM', 't_ST', 'EL_STM'),
           ci.len = c(mean(ci.len.t.N2), mean(ci.len.el.N2),
                      mean(ci.len.tm.y.N2), mean(ci.len.tm.el.N2),
                      mean(ci.len.stm.y.N2), mean(ci.len.stm.el.N2, na.rm = TRUE)))

## Sensitivity analysis ----

set.seed(123)
X <- rlnorm(200)
Y <- rlnorm(200)

alpha = seq(0.01, 0.45, by = 0.01)
### Figure 2 ----
CV.opt(list(X, Y), ST.diff, alpha, 10, plot = TRUE)



set.seed(123)
X1 <- rlnorm(200)
Y1 <- rlnorm(200)
Z1 <- rlnorm(200)

Y = list(X1, Y1, Z1)
### Figure 3 ----
alpha = seq(0.01, 0.45, by = 0.01)
CV.opt(Y, common.stm, alpha, 10, plot = TRUE)


