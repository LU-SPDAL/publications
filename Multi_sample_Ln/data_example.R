library(tidyverse)
library(rstatix)
library(PairedData)
library(EL)
library(ggpubr)
library(ggtext)
source('functions.R')

data0 <- openxlsx::read.xlsx('datu_kopas.xlsx', sheet = 'cloth')
x.2 <- data0$Mill_a
y.2 <- data0$Mill_b


data1 <- data.frame('x' = c(x.2, y.2), 
                    'gr' = c(rep('Mill_A', length(x.2)), rep('Mill_B', length(y.2))))

st <- data1 %>% 
  group_by(gr) %>% 
  summarise(mean = mean(x),
            tm_005 = mean(x, trim = 0.05),
            tm_01 = mean(x, trim = 0.1),
            tm_02 = mean(x, trim = 0.2),
            stm_005 = ST_mean(x, alpha = 0.05, gamma = 0.2),
            stm_01 = ST_mean(x, alpha = 0.1, gamma = 0.2),
            stm_02 = ST_mean(x, alpha = 0.15, gamma = 0.2),
            n = n()) %>% 
  ungroup() %>% 
  mutate(gr = paste(gr, ' (n = ', n, ')', sep = '')) %>% 
  dplyr::select(-n) %>% 
  ungroup() %>%  
  t() 
  
nam <- st[1,]
st <- as.data.frame(st[-1,])
names(st) <- nam

st <- st %>% 
  mutate(est = row.names(st)) %>% 
  dplyr::select(est, `Mill_A (n = 22)`, `Mill_B (n = 22)`)

x <- data1 %>% 
  filter(gr == 'Mill_A') %>% 
  dplyr::select(x) %>% 
  unlist()
y <- data1 %>% 
  filter(gr == 'Mill_B') %>% 
  dplyr::select(x)%>% 
  unlist()

gamma <- 0.2
alpha <- c(0.05, 0.1, 0.15)

p.t <- t.test(x, y)$p.value
p.el <- EL.means(x, y)$p.value
p.tm.y <- c(yuen.t.test(x, y, tr = alpha[1])$p.value, yuen.t.test(x, y, tr = alpha[2])$p.value, yuen.t.test(x, y, tr = alpha[3])$p.value)

p.tm.el <- c(EL.tm(x, y, alpha = alpha[1], beta = alpha[1])$p.value, EL.tm(x, y, alpha = alpha[2], beta = alpha[2])$p.value, EL.tm(x, y, alpha = alpha[2], beta = alpha[3])$p.value)

p.stm.y <- c(yuen.stm(x, y, alpha = alpha[1], gamma = gamma)$p.value, yuen.stm(x, y, alpha = alpha[2], gamma = gamma)$p.value, yuen.stm(x, y, alpha = alpha[3], gamma = gamma)$p.value)
p.stm.el <- c(EL.stm(x, y, alpha = alpha[1], gamma = gamma)$p.value, EL.stm(x, y, alpha = alpha[2], gamma = gamma)$p.value, EL.stm(x, y, alpha = alpha[3], gamma = gamma)$p.value)

p.tab <- data.frame(est = st$est, p.t = c(p.t, p.tm.y, p.stm.y), p.el = c(p.el, p.tm.el, p.stm.el))

rez <- left_join(st, p.tab)



# Y <- list(data1$x[data1$gr == 'Mill_A'], data1$x[data1$gr != 'Mill_A'])
# 
# alpha = seq(0.01, 0.45, 0.01)     
# CV.opt(Y, ST.diff, alpha = alpha, 5, plot = TRUE) # alpha = 0.34; gamma = 0.35

      
# openxlsx::write.xlsx(rez, 'p_tab.xlsx', row.names = FALSE)


ci.t <- t.test(x.2, y.2)$conf.int
ci.el <- EL.means(x.2, y.2)$conf.int

ci.t.tm <- matrix(ncol = 2, nrow = length(alpha))
ci.el.tm <- matrix(ncol = 2, nrow = length(alpha))
ci.t.stm <- matrix(ncol = 2, nrow = length(alpha))
ci.el.stm <- matrix(ncol = 2, nrow = length(alpha))

for(a in 1:length(alpha)){
  ci.t.tm[a, ] <- yuen.tm(x ~ gr, data1, tr = alpha[a])$conf.int
  ci.el.tm[a, ] <- EL.tm(x.2, y.2, alpha = alpha[a], beta = alpha[a])$conf.int
  ci.t.stm[a, ] <- yuen.stm(x.2, y.2, alpha = alpha[a], gamma = 0.2)$conf.int
  ci.el.stm[a, ] <- c(-1*EL.stm(x.2, y.2, alpha = alpha[a], gamma = 0.2)$conf.int[2], -1*EL.stm(x.2, y.2, alpha = alpha[a], gamma = 0.2)$conf.int[1])
}

d.t.tm <- numeric(length(alpha))
d.el.tm <- numeric(length(alpha))
d.t.stm <- numeric(length(alpha))
d.el.stm <- numeric(length(alpha))

for(a in 1:length(alpha)){
  d.t.tm[a] <- yuen.tm(x ~ gr, data1, tr = alpha[a])$diff
  d.el.tm[a] <- EL.tm(x.2, y.2, alpha = alpha[a], beta = alpha[a])$estimate
  d.t.stm[a] <- -1*yuen.stm(x.2, y.2, alpha = alpha[a], gamma = 0.2)$estimate
  d.el.stm[a] <- -1*EL.stm(x.2, y.2, alpha = alpha[a], gamma = 0.2)$est
}


ci.t.tm <- as.data.frame(cbind(ci.t.tm, alpha))
ci.el.tm <- as.data.frame(cbind(ci.el.tm, alpha))
ci.t.stm <- as.data.frame(cbind(ci.t.stm, alpha))
ci.el.stm <- as.data.frame(cbind(ci.el.stm, alpha))

names(ci.t.tm) <- c('lb', 'ub', 'alpha')
names(ci.el.tm) <- c('lb', 'ub', 'alpha')
names(ci.t.stm) <- c('lb', 'ub', 'alpha')
names(ci.el.stm) <- c('lb', 'ub', 'alpha')

ci.t.tm$method <- rep('T_TM', length(alpha))
ci.el.tm$method <- rep('EL_TM', length(alpha))
ci.t.stm$method <- rep('T_ST', length(alpha))
ci.el.stm$method <- rep('EL_ST', length(alpha))

d.t.tm <- as.data.frame(cbind(d.t.tm, alpha))
d.el.tm <- as.data.frame(cbind(d.el.tm, alpha))
d.t.stm <- as.data.frame(cbind(d.t.stm, alpha))
d.el.stm <- as.data.frame(cbind(d.el.stm, alpha))

names(d.t.tm) <- c('est', 'alpha')
names(d.el.tm) <- c('est', 'alpha')
names(d.t.stm) <- c('est', 'alpha')
names(d.el.stm) <- c('est', 'alpha')


d.t.tm$method <- rep('T_TM', length(alpha))
d.el.tm$method <- rep('EL_TM', length(alpha))
d.t.stm$method <- rep('T_ST', length(alpha))
d.el.stm$method <- rep('EL_ST', length(alpha))



ci.tab <- rbind(data.frame('lb' = ci.t[1], 'ub' = ci.t[2], est = mean(x.2) - mean(y.2), alpha = 0, method = 'T'),
                data.frame('lb' = ci.el[1], 'ub' = ci.el[2], est = EL.means(x.2, y.2)$est, alpha = 0, method = 'EL'),
                left_join(ci.t.tm, d.t.tm),
                left_join(ci.el.tm, d.el.tm),
                left_join(ci.t.stm, d.t.stm),
                left_join(ci.el.stm, d.el.stm))



ci.tab %>%
  mutate(alpha = as.character(alpha)) %>% 
  ggplot(aes(x = method, y = est))+
  geom_point(aes(shape = alpha), size = 3.75, position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin = lb, ymax = ub, lty = alpha), width=.1, size = 1.2, position = position_dodge(width = 0.5))+
  geom_hline(yintercept = 0, col = 'black', size = 1, lty = 2)+
  theme_bw()+
  ylab(expression(Delta))+
  scale_x_discrete(limits = c('EL_ST', 'EL_TM', 'EL', 'T', 'T_ST', 'T_TM'))


# ggsave('ci_exmp.png', bg = 'white')

data0 %>% 
  pivot_longer(cols = 1:2) %>% 
  ggplot(aes(x = name, y = value))+
  geom_boxplot()+
  theme_bw()+
  xlab('group')+
  ylab('Cloth')

ggsave('bp2.png')



# 3 samp ----

library(rrcov)
data(OsloTransect)


OsloTransect <- OsloTransect %>% 
  filter(X.FLITHO %in% c('CAMSED', 'GNEIS_O', 'GNEIS_R', 'MAGM'))
OsloTransect$X.FLITHO <- as.character(OsloTransect$X.FLITHO)

OsloTransect <- OsloTransect %>% 
  dplyr::select( FILTHO = X.FLITHO , Ag_ppb,  B,    Ba,    Ca,   Cd,   Co,  Cr,   Cu,  Fe, Hg_ppb,     
          K   ,La  ,Mg   ,Mn   ,Mo   ,Ni    ,P ,Pb    ,S   ,Sb    ,Sr ,Ti    ,Zn) %>% 
  na.omit()



Ag <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Ag_ppb))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Ag_ppb')

B <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = B))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('B')

Ba <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Ba))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('BA')

Ca <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Ca))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Ca')

Cd <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = B))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Cd')

Co <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Co))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Co')

Cr <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Cr))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Cr')

Cu <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Cu))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Cu')

Fe <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Fe))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Fe')

Hg <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Hg_ppb))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Hg_ppb')

K <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = K))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('K')

La <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = La))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('La')

Mg <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Mg))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Mg')

Mn <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Mn))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Mn')

Mo <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Mo))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Mo')

Ni <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Ni))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Ni')

P <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = P))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('P')

Pb <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Pb))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Pb')

S <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = S))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('S')

Sb <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Sb))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Sb')

Sr <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Sr))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Sr')

Ti <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Ti))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Ti')

Zn <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Zn))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Zn')


# names(OsloTransect)
ggpubr::ggarrange(Ag, B, Ba, Ca, Cd, Co, Cu, Fe, Hg, K, La,  nrow = 4, ncol = 3)
# ggsave('bp_anova1.png', width = 10, bg = 'white')

ggpubr::ggarrange(Mg, Mn, Mo, Ni, P, Pb, S, Sb, Sr, Ti, Zn, nrow = 4, ncol = 3)
# ggsave('bp_anova2.png', width = 10, bg = 'white')

rez1 <- OsloTransect %>% 
  pivot_longer(cols = 2:24) %>% 
  group_by(FILTHO, name) %>% 
  summarise(mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(mean = paste(round(mean,3), ' (', round(sd,3), ')', sep = '')) %>% 
  dplyr::select(-sd) %>% 
  pivot_wider(names_from = 1, values_from = 3)

rez2 <- OsloTransect %>% 
  dplyr::select(-FILTHO) %>% 
  pivot_longer(cols = 1:23) %>% 
  group_by(name) %>% 
  summarise(`Whole cohort` = paste(round(mean(value, na.rm = TRUE),3), ' (', round(sd(value, na.rm = TRUE),3), ')', sep = ''))

rez <- left_join(rez1, rez2)


names(rez) <- c('Element', 'Camsed (n = 99)', 'Geneis_0 (n = 90)', 'Geneis_R (n = 36)', 'Magm (n = 117)', 'Whole data set (n = 342)')

OsloTransect %>% 
  group_by(FILTHO) %>% 
  summarise(n = n()) %>% 
  xtable::xtable()

alpha <- c(0.05, 0.1, 0.2)
gamma <- c(0.3, 0.4)


welch_p <- data.frame(p_w = c(welch_anova_test(data = OsloTransect, Ag_ppb ~ FILTHO)$p,
                              welch_anova_test(data = OsloTransect, B ~ FILTHO)$p,
                              welch_anova_test(data = OsloTransect, Ba ~ FILTHO)$p,
                              welch_anova_test(data = OsloTransect, Ca ~ FILTHO)$p,
                              welch_anova_test(data = OsloTransect, Cd ~ FILTHO)$p,
                              welch_anova_test(data = OsloTransect, Co ~ FILTHO)$p,
                              welch_anova_test(data = OsloTransect, Cr ~ FILTHO)$p,
                              welch_anova_test(data = OsloTransect, Cu ~ FILTHO)$p,
                              welch_anova_test(data = OsloTransect, Fe ~ FILTHO)$p,
                              welch_anova_test(data = OsloTransect, Hg_ppb ~ FILTHO)$p,
                              welch_anova_test(data = OsloTransect, K ~ FILTHO)$p,
                              welch_anova_test(data = OsloTransect, La ~ FILTHO)$p,
                              welch_anova_test(data = OsloTransect, Mg ~ FILTHO)$p,
                              welch_anova_test(data = OsloTransect, Mn ~ FILTHO)$p,
                              welch_anova_test(data = OsloTransect, Mo ~ FILTHO)$p,
                              welch_anova_test(data = OsloTransect, Ni ~ FILTHO)$p,
                              welch_anova_test(data = OsloTransect, P ~ FILTHO)$p,
                              welch_anova_test(data = OsloTransect, Pb ~ FILTHO)$p,
                              welch_anova_test(data = OsloTransect, S ~ FILTHO)$p,
                              welch_anova_test(data = OsloTransect, Sb ~ FILTHO)$p,
                              welch_anova_test(data = OsloTransect, Sr ~ FILTHO)$p,
                              welch_anova_test(data = OsloTransect, Ti ~ FILTHO)$p,
                              welch_anova_test(data = OsloTransect, Zn ~ FILTHO)$p), 
                      Element = names(OsloTransect)[2:length(names(OsloTransect))])

f_p <- data.frame(p_f = c(summary(aov( Ag_ppb ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                          summary(aov( B ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                          summary(aov( Ba ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                          summary(aov( Ca ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                          summary(aov( Cd ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                          summary(aov( Co ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                          summary(aov( Cr ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                          summary(aov( Cu ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                          summary(aov( Fe ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                          summary(aov( Hg_ppb ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                          summary(aov( K ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                          summary(aov( La ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                          summary(aov( Mg ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                          summary(aov( Mn ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                          summary(aov( Mo ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                          summary(aov( Ni ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                          summary(aov( P ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                          summary(aov( Pb ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                          summary(aov( S ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                          summary(aov( Sb ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                          summary(aov( Sr ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                          summary(aov( Ti ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                          summary(aov( Zn ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1]),
                  Element = names(OsloTransect)[2:length(names(OsloTransect))])

y_p <- data.frame(py_005 = c(t1way( Ag_ppb ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                             t1way( B ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                             t1way( Ba ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                             t1way( Ca ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                             t1way( Cd ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                             t1way( Co ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                             t1way( Cr ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                             t1way( Cu ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                             t1way( Fe ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                             t1way( Hg_ppb ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                             t1way( K ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                             t1way( La ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                             t1way( Mg ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                             t1way( Mn ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                             t1way( Mo ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                             t1way( Ni ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                             t1way( P ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                             t1way( Pb ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                             t1way( S ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                             t1way( Sb ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                             t1way( Sr ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                             t1way( Ti ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                             t1way( Zn ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value),
                  py_01 = c(t1way( Ag_ppb ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( B ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Ba ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Ca ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Cd ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Co ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Cr ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Cu ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Fe ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Hg_ppb ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( K ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( La ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Mg ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Mn ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Mo ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Ni ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( P ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Pb ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( S ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Sb ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Sr ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Ti ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Zn ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value),
                  py_02 = c(t1way( Ag_ppb ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( B ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Ba ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Ca ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Cd ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Co ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Cr ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Cu ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Fe ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Hg_ppb ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( K ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( La ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Mg ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Mn ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Mo ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Ni ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( P ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Pb ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( S ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Sb ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Sr ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Ti ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Zn ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value),
                  Element = names(OsloTransect)[2:length(names(OsloTransect))])




ely_p <- data.frame(pel_005 = c(EL.anova.tm(split(OsloTransect$Ag_ppb, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                                EL.anova.tm(split(OsloTransect$B, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                                EL.anova.tm(split(OsloTransect$Ba, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                                EL.anova.tm(split(OsloTransect$Ca, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                                EL.anova.tm(split(OsloTransect$Cd, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                                EL.anova.tm(split(OsloTransect$Co, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                                EL.anova.tm(split(OsloTransect$Cr, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                                EL.anova.tm(split(OsloTransect$Cu, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                                EL.anova.tm(split(OsloTransect$Fe, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                                EL.anova.tm(split(OsloTransect$Hg_ppb, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                                EL.anova.tm(split(OsloTransect$K, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                                EL.anova.tm(split(OsloTransect$La, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                                EL.anova.tm(split(OsloTransect$Mg, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                                EL.anova.tm(split(OsloTransect$Mn, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                                EL.anova.tm(split(OsloTransect$Mo, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                                EL.anova.tm(split(OsloTransect$Ni, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                                EL.anova.tm(split(OsloTransect$P, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                                EL.anova.tm(split(OsloTransect$Pb, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                                EL.anova.tm(split(OsloTransect$S, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                                EL.anova.tm(split(OsloTransect$Sb, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                                EL.anova.tm(split(OsloTransect$Sr, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                                EL.anova.tm(split(OsloTransect$Ti, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                                EL.anova.tm(split(OsloTransect$Zn, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value),
                    
                    pel_01 = c(EL.anova.tm(split(OsloTransect$Ag_ppb, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                               EL.anova.tm(split(OsloTransect$B, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                               EL.anova.tm(split(OsloTransect$Ba, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                               EL.anova.tm(split(OsloTransect$Ca, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                               EL.anova.tm(split(OsloTransect$Cd, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                               EL.anova.tm(split(OsloTransect$Co, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                               EL.anova.tm(split(OsloTransect$Cr, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                               EL.anova.tm(split(OsloTransect$Cu, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                               EL.anova.tm(split(OsloTransect$Fe, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                               EL.anova.tm(split(OsloTransect$Hg_ppb, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                               EL.anova.tm(split(OsloTransect$K, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                               EL.anova.tm(split(OsloTransect$La, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                               EL.anova.tm(split(OsloTransect$Mg, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                               EL.anova.tm(split(OsloTransect$Mn, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                               EL.anova.tm(split(OsloTransect$Mo, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                               EL.anova.tm(split(OsloTransect$Ni, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                               EL.anova.tm(split(OsloTransect$P, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                               EL.anova.tm(split(OsloTransect$Pb, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                               EL.anova.tm(split(OsloTransect$S, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                               EL.anova.tm(split(OsloTransect$Sb, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                               EL.anova.tm(split(OsloTransect$Sr, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                               EL.anova.tm(split(OsloTransect$Ti, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                               EL.anova.tm(split(OsloTransect$Zn, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value),
                    pel_02 = c(EL.anova.tm(split(OsloTransect$Ag_ppb, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                               EL.anova.tm(split(OsloTransect$B, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                               EL.anova.tm(split(OsloTransect$Ba, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                               EL.anova.tm(split(OsloTransect$Ca, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                               EL.anova.tm(split(OsloTransect$Cd, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                               EL.anova.tm(split(OsloTransect$Co, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                               EL.anova.tm(split(OsloTransect$Cr, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                               EL.anova.tm(split(OsloTransect$Cu, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                               EL.anova.tm(split(OsloTransect$Fe, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                               EL.anova.tm(split(OsloTransect$Hg_ppb, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                               EL.anova.tm(split(OsloTransect$K, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                               EL.anova.tm(split(OsloTransect$La, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                               EL.anova.tm(split(OsloTransect$Mg, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                               EL.anova.tm(split(OsloTransect$Mn, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                               EL.anova.tm(split(OsloTransect$Mo, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                               EL.anova.tm(split(OsloTransect$Ni, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                               EL.anova.tm(split(OsloTransect$P, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                               EL.anova.tm(split(OsloTransect$Pb, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                               EL.anova.tm(split(OsloTransect$S, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                               EL.anova.tm(split(OsloTransect$Sb, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                               EL.anova.tm(split(OsloTransect$Sr, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                               EL.anova.tm(split(OsloTransect$Ti, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                               EL.anova.tm(split(OsloTransect$Zn, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value),
                    Element = names(OsloTransect)[2:length(names(OsloTransect))])


stm_p <- data.frame(pel_005_g1 = c(EL.anova.stm(split(OsloTransect$Ag_ppb, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   EL.anova.stm(split(OsloTransect$B, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   EL.anova.stm(split(OsloTransect$Ba, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   EL.anova.stm(split(OsloTransect$Ca, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   EL.anova.stm(split(OsloTransect$Cd, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   EL.anova.stm(split(OsloTransect$Co, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   EL.anova.stm(split(OsloTransect$Cr, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   EL.anova.stm(split(OsloTransect$Cu, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   EL.anova.stm(split(OsloTransect$Fe, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   EL.anova.stm(split(OsloTransect$Hg_ppb, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   EL.anova.stm(split(OsloTransect$K, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   EL.anova.stm(split(OsloTransect$La, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   EL.anova.stm(split(OsloTransect$Mg, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   EL.anova.stm(split(OsloTransect$Mn, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   EL.anova.stm(split(OsloTransect$Mo, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   EL.anova.stm(split(OsloTransect$Ni, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   EL.anova.stm(split(OsloTransect$P, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   EL.anova.stm(split(OsloTransect$Pb, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   EL.anova.stm(split(OsloTransect$S, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   EL.anova.stm(split(OsloTransect$Sb, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   EL.anova.stm(split(OsloTransect$Sr, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   EL.anova.stm(split(OsloTransect$Ti, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   EL.anova.stm(split(OsloTransect$Zn, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value),
                    pel_005_g2 = c(EL.anova.stm(split(OsloTransect$Ag_ppb, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   EL.anova.stm(split(OsloTransect$B, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   EL.anova.stm(split(OsloTransect$Ba, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   EL.anova.stm(split(OsloTransect$Ca, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   EL.anova.stm(split(OsloTransect$Cd, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   EL.anova.stm(split(OsloTransect$Co, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   EL.anova.stm(split(OsloTransect$Cr, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   EL.anova.stm(split(OsloTransect$Cu, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   EL.anova.stm(split(OsloTransect$Fe, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   EL.anova.stm(split(OsloTransect$Hg_ppb, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   EL.anova.stm(split(OsloTransect$K, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   EL.anova.stm(split(OsloTransect$La, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   EL.anova.stm(split(OsloTransect$Mg, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   EL.anova.stm(split(OsloTransect$Mn, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   EL.anova.stm(split(OsloTransect$Mo, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   EL.anova.stm(split(OsloTransect$Ni, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   EL.anova.stm(split(OsloTransect$P, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   EL.anova.stm(split(OsloTransect$Pb, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   EL.anova.stm(split(OsloTransect$S, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   EL.anova.stm(split(OsloTransect$Sb, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   EL.anova.stm(split(OsloTransect$Sr, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   EL.anova.stm(split(OsloTransect$Ti, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   EL.anova.stm(split(OsloTransect$Zn, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value),
                    pel_01_g1 = c(EL.anova.stm(split(OsloTransect$Ag_ppb, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$B, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Ba, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Ca, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Cd, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Co, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Cr, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Cu, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Fe, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Hg_ppb, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$K, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$La, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Mg, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Mn, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Mo, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Ni, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$P, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Pb, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$S, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Sb, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Sr, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Ti, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Zn, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value),
                    pel_01_g2 = c(EL.anova.stm(split(OsloTransect$Ag_ppb, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$B, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Ba, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Ca, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Cd, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Co, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Cr, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Cu, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Fe, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Hg_ppb, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$K, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$La, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Mg, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Mn, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Mo, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Ni, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$P, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Pb, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$S, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Sb, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Sr, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Ti, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Zn, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value),
                    pel_02_g1 = c(EL.anova.stm(split(OsloTransect$Ag_ppb, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$B, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Ba, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Ca, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Cd, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Co, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Cr, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Cu, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Fe, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Hg_ppb, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$K, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$La, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Mg, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Mn, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Mo, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Ni, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$P, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Pb, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$S, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Sb, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Sr, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Ti, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  EL.anova.stm(split(OsloTransect$Zn, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value),
                    pel_02_g2 = c(EL.anova.stm(split(OsloTransect$Ag_ppb, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$B, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Ba, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Ca, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Cd, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Co, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Cr, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Cu, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Fe, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Hg_ppb, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$K, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$La, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Mg, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Mn, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Mo, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Ni, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$P, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Pb, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$S, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Sb, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Sr, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Ti, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  EL.anova.stm(split(OsloTransect$Zn, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value),
                    Element = names(OsloTransect)[2:length(names(OsloTransect))])


stm_y_p <- data.frame(py_005_g1 = c(ANOVA.stm(split(OsloTransect$Ag_ppb, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   ANOVA.stm(split(OsloTransect$B, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   ANOVA.stm(split(OsloTransect$Ba, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   ANOVA.stm(split(OsloTransect$Ca, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   ANOVA.stm(split(OsloTransect$Cd, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   ANOVA.stm(split(OsloTransect$Co, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   ANOVA.stm(split(OsloTransect$Cr, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   ANOVA.stm(split(OsloTransect$Cu, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   ANOVA.stm(split(OsloTransect$Fe, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   ANOVA.stm(split(OsloTransect$Hg_ppb, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   ANOVA.stm(split(OsloTransect$K, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   ANOVA.stm(split(OsloTransect$La, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   ANOVA.stm(split(OsloTransect$Mg, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   ANOVA.stm(split(OsloTransect$Mn, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   ANOVA.stm(split(OsloTransect$Mo, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   ANOVA.stm(split(OsloTransect$Ni, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   ANOVA.stm(split(OsloTransect$P, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   ANOVA.stm(split(OsloTransect$Pb, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   ANOVA.stm(split(OsloTransect$S, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   ANOVA.stm(split(OsloTransect$Sb, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   ANOVA.stm(split(OsloTransect$Sr, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   ANOVA.stm(split(OsloTransect$Ti, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                                   ANOVA.stm(split(OsloTransect$Zn, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value),
                    py_005_g2 = c(ANOVA.stm(split(OsloTransect$Ag_ppb, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   ANOVA.stm(split(OsloTransect$B, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   ANOVA.stm(split(OsloTransect$Ba, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   ANOVA.stm(split(OsloTransect$Ca, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   ANOVA.stm(split(OsloTransect$Cd, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   ANOVA.stm(split(OsloTransect$Co, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   ANOVA.stm(split(OsloTransect$Cr, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   ANOVA.stm(split(OsloTransect$Cu, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   ANOVA.stm(split(OsloTransect$Fe, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   ANOVA.stm(split(OsloTransect$Hg_ppb, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   ANOVA.stm(split(OsloTransect$K, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   ANOVA.stm(split(OsloTransect$La, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   ANOVA.stm(split(OsloTransect$Mg, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   ANOVA.stm(split(OsloTransect$Mn, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   ANOVA.stm(split(OsloTransect$Mo, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   ANOVA.stm(split(OsloTransect$Ni, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   ANOVA.stm(split(OsloTransect$P, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   ANOVA.stm(split(OsloTransect$Pb, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   ANOVA.stm(split(OsloTransect$S, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   ANOVA.stm(split(OsloTransect$Sb, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   ANOVA.stm(split(OsloTransect$Sr, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   ANOVA.stm(split(OsloTransect$Ti, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                   ANOVA.stm(split(OsloTransect$Zn, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value),
                    py_01_g1 = c(ANOVA.stm(split(OsloTransect$Ag_ppb, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$B, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Ba, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Ca, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Cd, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Co, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Cr, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Cu, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Fe, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Hg_ppb, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$K, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$La, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Mg, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Mn, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Mo, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Ni, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$P, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Pb, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$S, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Sb, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Sr, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Ti, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Zn, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value),
                    py_01_g2 = c(ANOVA.stm(split(OsloTransect$Ag_ppb, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$B, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Ba, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Ca, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Cd, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Co, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Cr, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Cu, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Fe, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Hg_ppb, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$K, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$La, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Mg, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Mn, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Mo, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Ni, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$P, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Pb, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$S, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Sb, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Sr, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Ti, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Zn, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value),
                    py_02_g1 = c(ANOVA.stm(split(OsloTransect$Ag_ppb, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$B, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Ba, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Ca, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Cd, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Co, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Cr, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Cu, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Fe, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Hg_ppb, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$K, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$La, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Mg, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Mn, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Mo, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Ni, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$P, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Pb, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$S, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Sb, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Sr, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Ti, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                  ANOVA.stm(split(OsloTransect$Zn, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value),
                    py_02_g2 = c(ANOVA.stm(split(OsloTransect$Ag_ppb, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$B, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Ba, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Ca, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Cd, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Co, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Cr, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Cu, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Fe, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Hg_ppb, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$K, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$La, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Mg, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Mn, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Mo, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Ni, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$P, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Pb, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$S, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Sb, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Sr, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Ti, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                  ANOVA.stm(split(OsloTransect$Zn, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value),
                    Element = names(OsloTransect)[2:length(names(OsloTransect))])



tab.anova <- left_join(welch_p, 
                       left_join(f_p, 
                                 left_join(y_p, 
                                           left_join(ely_p, 
                                                     left_join(stm_p, stm_y_p)))))

f.temp <- function(elem, alpha = seq(0.01, 0.45, 0.01), K){
  # elem <- nam[1]
  Y <- OsloTransect %>%
    dplyr::select(FILTHO, x = elem) %>% 
    group_by(FILTHO) %>%
    summarise(B = list(x), .groups = "drop") %>%
    tibble::deframe() 
    
  CV.opt(Y, common.stm, alpha, K)$optimal.values[2:3]
  # CV.opt(Y, common.stm, alpha, K)
   
}

nam <- names(OsloTransect)[-1]
rez <- t(sapply(nam, function(x) f.temp(x, alpha = seq(0.01, 0.3, 0.01), 10)))

rez2 <- as.data.frame(rez) %>% 
  mutate(Element = row.names(rez),
         alpha = as.numeric(alpha),
         gamma = as.numeric(gamma))

rez2 %>% 
  dplyr::select(Element, alpha, gamma) %>% 
  openxlsx::write.xlsx('opt_ag.xlsx')

f.temp2 <- function(elem, alpha, gamma, method){
  # elem <- "Ag_ppb"
  Y <- OsloTransect %>%
    dplyr::select(FILTHO, x = elem) %>% 
    group_by(FILTHO) %>%
    summarise(B = list(x), .groups = "drop") %>%
    tibble::deframe() 
  
  if(method == 'EL'){
    EL.anova.stm(Y, alpha, gamma)$p.value
  }else if (method == 'F'){
    ANOVA.stm(Y, alpha, gamma)$p.value
  }
  
}


p.anova.stm.opt <- data.frame(p_y = sapply(1:nrow(rez2), function(i) f.temp2(rez2$Element[i], rez2$alpha[i], rez2$gamma[i], 'F')),
           p_el = sapply(1:nrow(rez2), function(i) f.temp2(rez2$Element[i], rez2$alpha[i], rez2$gamma[i], 'EL')))
openxlsx::write.xlsx(p.anova.stm.opt, 'opt_p.xlsx', row.names = F)
cbind(tab.anova, p.anova.stm.opt)
# openxlsx::write.xlsx(tab.anova, 'elem.xlsx')


oslo.camsed <- OsloTransect %>% 
  filter(FILTHO == 'CAMSED') %>% 
  dplyr::select(La)


tm005.1 <- quantile(oslo.camsed$La, probs = 0.95)
tm01.1 <- quantile(oslo.camsed$La, probs = 0.90)
tm02.1 <- quantile(oslo.camsed$La, probs = 0.8)
bw.1 <- round(1/bw.nrd(oslo.camsed$La))


camsed <- oslo.camsed %>% 
  ggplot(aes(x = La))+
  geom_histogram(aes(y = ..density..), bins = bw.1, col = 'black', fill = 'white')+
  geom_vline(aes(xintercept = tm005.1, linetype = '0.05'), size = 1.3)+
  geom_vline(aes(xintercept = tm01.1, linetype = '0.1'), size = 1.3)+
  geom_vline(aes(xintercept = tm02.1, linetype = '0.2'), size = 1.3)+
  theme_bw()+
  labs(linetype = expression(alpha),
       x = '',
       y = '')+
  scale_color_brewer(palette = "Dark2")+
  ggtitle("CAMSED") +
  theme(
    plot.title = element_textbox(
      size = 9,
      face = "bold",
      color = "black",
      fill = "grey90",# similar to default facet strip
      box.color = "grey70",
      # box.size = 0.5,
      padding = margin(4, 4, 4, 4),
      margin = margin(6, 0, 6, 0)
    )
  )




oslo.geneis_o <- OsloTransect %>% 
  filter(FILTHO == 'GNEIS_O') %>% 
  dplyr::select(La)


tm005.2 <- quantile(oslo.geneis_o$La, probs = 0.95)
tm01.2 <- quantile(oslo.geneis_o$La, probs = 0.90)
tm02.2 <- quantile(oslo.geneis_o$La, probs = 0.8)
bw.2 <- round(1/bw.nrd(oslo.geneis_o$La))


gneis_o <- oslo.geneis_o %>% 
  ggplot(aes(x = La))+
  geom_histogram(aes(y = ..density..), bins = bw.2, col = 'black', fill = 'white')+
  geom_vline(aes(xintercept = tm005.2, linetype = '0.05'), size = 1.3)+
  geom_vline(aes(xintercept = tm01.2, linetype = '0.1'), size = 1.3)+
  geom_vline(aes(xintercept = tm02.2, linetype = '0.2'), size = 1.3)+
  theme_bw()+
  labs(linetype = expression(alpha),
       x = '',
       y = '')+
  scale_color_brewer(palette = "Dark2")+
  ggtitle("GNEIS_O") +
  theme(
    plot.title = element_textbox(
      size = 9,
      face = "bold",
      color = "black",
      fill = "grey90",# similar to default facet strip
      box.color = "grey70",
      # box.size = 0.5,
      padding = margin(4, 4, 4, 4),
      margin = margin(6, 0, 6, 0)
    )
  )

oslo.geneis_r <- OsloTransect %>% 
  filter(FILTHO == 'GNEIS_R') %>% 
  dplyr::select(La)


tm005.3 <- quantile(oslo.geneis_r$La, probs = 0.95)
tm01.3 <- quantile(oslo.geneis_r$La, probs = 0.90)
tm02.3 <- quantile(oslo.geneis_r$La, probs = 0.8)
bw.3 <- round(1/bw.nrd(oslo.geneis_o$La))


gneis_r <- oslo.geneis_r %>% 
  ggplot(aes(x = La))+
  geom_histogram(aes(y = ..density..), bins = bw.3, col = 'black', fill = 'white')+
  geom_vline(aes(xintercept = tm005.3, linetype = '0.05'), size = 1.3)+
  geom_vline(aes(xintercept = tm01.3, linetype = '0.1'), size = 1.3)+
  geom_vline(aes(xintercept = tm02.3, linetype = '0.2'), size = 1.3)+
  theme_bw()+
  labs(linetype = expression(alpha),
       x = '',
       y = '')+
  scale_color_brewer(palette = "Dark2")+
  ggtitle("GNEIS_R") +
  theme(
    plot.title = element_textbox(
      size = 9,
      face = "bold",
      color = "black",
      fill = "grey90",# similar to default facet strip
      box.color = "grey70",
      # box.size = 0.5,
      padding = margin(4, 4, 4, 4),
      margin = margin(6, 0, 6, 0)
    )
  )

oslo.magm <- OsloTransect %>% 
  filter(FILTHO == 'MAGM') %>% 
  dplyr::select(La)


tm005.4 <- quantile(oslo.magm$La, probs = 0.95)
tm01.4 <- quantile(oslo.magm$La, probs = 0.90)
tm02.4 <- quantile(oslo.magm$La, probs = 0.8)
bw.4 <- round(1/bw.nrd(oslo.magm$La))


magm <- oslo.magm %>% 
  ggplot(aes(x = La))+
  geom_histogram(aes(y = ..density..), bins = bw.4, col = 'black', fill = 'white')+
  geom_vline(aes(xintercept = tm005.4, linetype = '0.05'), size = 1.3)+
  geom_vline(aes(xintercept = tm01.4, linetype = '0.1'), size = 1.3)+
  geom_vline(aes(xintercept = tm02.4, linetype = '0.2'), size = 1.3)+
  theme_bw()+
  labs(linetype = expression(alpha),
      x = '',
       y = '')+
  scale_color_brewer(palette = "Dark2")+
  ggtitle("MAGM") +
  theme(
    plot.title = element_textbox(
      size = 9,
      face = "bold",
      color = "black",
      fill = "grey90",# similar to default facet strip
      box.color = "grey70",
      # box.size = 0.5,
      padding = margin(4, 4, 4, 4),
      margin = margin(6, 0, 6, 0)
    )
  )



ggarrange(camsed, gneis_o, gneis_r, magm, common.legend = TRUE, legend = 'bottom', ncol = 2, nrow = 2) %>%
  annotate_figure(
    left = text_grob("Density", rot = 90, vjust = 1, size = 12),
    bottom = text_grob("La", size = 12)
  )

# ggsave('la_hist.png', bg = 'white', width = 10)
# 




