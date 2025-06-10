library(tidyverse)
library(rstatix)
library(PairedData)
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
            stm_005 = ST_mean(x, alpha = 0.05, gamma = 0.2),
            stm_01 = ST_mean(x, alpha = 0.1, gamma = 0.2),
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
alpha <- c(0.05, 0.1)

p.t <- t.test(x, y)$p.value
p.tm.y <- c(yuen.t.test(x, y, tr = alpha[1])$p.value, yuen.t.test(x, y, tr = alpha[2])$p.value)

p.tm.el <- c(EL.tm(x, y, alpha = alpha[1], beta = alpha[1])$p.value, EL.tm(x, y, alpha = alpha[2], beta = alpha[2])$p.value)

p.stm.y <- c(yuen.stm(x, y, alpha = alpha[1], gamma = gamma)$p.value, yuen.stm(x, y, alpha = alpha[2], gamma = gamma)$p.value)
p.stm.el <- c(EL.stm(x, y, alpha = alpha[1], gamma = gamma)$p.value, EL.stm(x, y, alpha = alpha[2], gamma = gamma)$p.value)

p.tab <- data.frame(est = st$est, p.t = c(p.t, p.tm.y, p.stm.y), p.el = c(NA, p.tm.el, p.stm.el))

rez <- left_join(st, p.tab)
openxlsx::write.xlsx(rez, 'p_tab.xlsx', row.names = FALSE)

data1 %>% 
  ggplot(aes(x = gr, y = x))+
  geom_boxplot()+
  theme_minimal()+
  xlab('')+
  ylab('Cotton')

ggsave('bp2.png', bg = 'white')


ci.t <- t.test(x.2, y.2)$conf.int

ci.t.tm <- matrix(ncol = 2, nrow = length(alpha))
ci.el.tm <- matrix(ncol = 2, nrow = length(alpha))
ci.t.stm <- matrix(ncol = 2, nrow = length(alpha))
ci.el.stm <- matrix(ncol = 2, nrow = length(alpha))

for(a in 1:length(alpha)){
  ci.t.tm[a, ] <- yuen.tm(x ~ gr, data1, tr = alpha[a])$conf.int
  ci.el.tm[a, ] <- EL.tm(x.2, y.2, alpha = alpha[a], beta = alpha[a])$conf.int
  ci.t.stm[a, ] <- yuen.stm(x.2, y.2, alpha = alpha[a], gamma = 0.4)$conf.int
  ci.el.stm[a, ] <- c(-1*EL.stm(x.2, y.2, alpha = alpha[a], gamma = 0.4)$conf.int[2], -1*EL.stm(x.2, y.2, alpha = alpha[a], gamma = 0.4)$conf.int[1])
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

ci.t.tm$method <- c('T_TM', 'T_TM')
ci.el.tm$method <- c('EL_TM', 'EL_TM')
ci.t.stm$method <- c('T_ST', 'T_ST')
ci.el.stm$method <- c('EL_ST', 'EL_ST')

d.t.tm <- as.data.frame(cbind(d.t.tm, alpha))
d.el.tm <- as.data.frame(cbind(d.el.tm, alpha))
d.t.stm <- as.data.frame(cbind(d.t.stm, alpha))
d.el.stm <- as.data.frame(cbind(d.el.stm, alpha))

names(d.t.tm) <- c('est', 'alpha')
names(d.el.tm) <- c('est', 'alpha')
names(d.t.stm) <- c('est', 'alpha')
names(d.el.stm) <- c('est', 'alpha')

d.t.tm$method <- c('T_TM', 'T_TM')
d.el.tm$method <- c('EL_TM', 'EL_TM')
d.t.stm$method <- c('T_ST', 'T_ST')
d.el.stm$method <- c('EL_ST', 'EL_ST')


ci.tab <- rbind(data.frame('lb' = ci.t[1], 'ub' = ci.t[2], est = mean(x.2) - mean(y.2), alpha = NA, method = 'T'),
                left_join(ci.t.tm, d.t.tm),
                left_join(ci.el.tm, d.el.tm),
                left_join(ci.t.stm, d.t.stm),
                left_join(ci.el.stm, d.el.stm))



ci.tab %>%
  mutate(alpha = as.character(alpha)) %>% 
  ggplot(aes(x = method, y = est))+
  geom_point(aes(col = alpha), size = 3.75, position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin = lb, ymax = ub, col = alpha), width=.1, size = 1.2, position = position_dodge(width = 0.5))+
  geom_hline(yintercept = 0, col = 'red', size = 1, lty = 2)+
  theme_minimal()+
  ylab(expression(Delta))

ggsave('ci_exmp.png', bg = 'white')

data0 %>% 
  pivot_longer(cols = 1:2) %>% 
  ggplot(aes(x = name, y = value))+
  geom_boxplot()+
  theme_minimal()+
  xlab('group')+
  ylab('Cloth')



# ggsave('hist.png', bg = 'white')


# 3 samp ----

library(rrcov)
data(OsloTransect)


OsloTransect <- OsloTransect %>% 
  filter(X.FLITHO %in% c('CAMSED', 'GNEIS_O', 'GNEIS_R', 'MAGM'))
OsloTransect$X.FLITHO <- as.character(OsloTransect$X.FLITHO)

OsloTransect <- OsloTransect %>% 
  select( FILTHO = X.FLITHO , Ag_ppb,  B,    Ba,    Ca,   Cd,   Co,  Cr,   Cu,  Fe, Hg_ppb,     
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
ggpubr::ggarrange(Ag, B, Ba, Ca, Cd, Co, Cu, Fe, Hg, K, La, nrow = 4, ncol = 3)
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

# openxlsx::write.xlsx(tab.anova, 'elem.xlsx')
