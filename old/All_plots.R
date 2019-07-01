source("library/hpModel.run.R")
source("library/hpModel.exploration.R")
source("library/hpModel.plot.R")
require(reshape2)
require(ggplot2)
require(scales)

# ======================================================================================
### (1) Um - max
# ======================================================================================

# Um.ph, Um.hp
SEQ1 <- seq(0.5, 1.5, length.out=5)
# max.h, max.p
SEQ2 <- seq(50, 300, length.out=5)

exp4 <- hpModel.exploration(
  # growth rate 
  r.h = 0.15, 
  r.p = 0.15, 
  # basic resources 
  max.h = SEQ2, 
  max.p = SEQ2, 
  # utility
  Um.ph = SEQ1,
  Um.hp = SEQ1,
  # number of types
  n.h = 10,         
  n.p = 10,        
  # undirected variation 
  v.h = 0.15,
  v.p = 0.15,
  # initial conditions
  iniH = 10,
  iniP = 10,
  # proportion of mean utility
  Ump.ph = 10, 
  Ump.hp = 10, 
  # proportion of mean utility
  Kmp.h = 10, 
  Kmp.p = 10, 
  MaxArea = 200)

g1 <- ggplot(exp4, aes(x = max.h, y = max.p)) + 
  geom_raster(aes(fill = LM.h)) + 
  geom_point(aes(color = LM.p), shape = 15, size = 2.5) + # 1.75
  #geom_point(aes(size = time), shape = 't') +
  scale_fill_gradientn(
     colors=c('red','white','blue'),
     values=rescale(c(min(exp4$LM.h), 0, max(exp4$LM.h))),
     limits=c(min(exp4$LM.h), max(exp4$LM.h))) + 
  scale_color_gradientn(
     colors=c('red','white','blue', 'darkblue'),
     values=rescale(c(min(exp4$LM.p), 0, 1, max(exp4$LM.p))),
     limits=c(min(exp4$LM.p), max(exp4$LM.p))) + 
  theme_bw() + # + theme(legend.position="none")
  theme(strip.background = element_rect(fill = NA, colour = NA), panel.grid.minor =  element_blank()) +
  facet_wrap(vars(paste('Um.ph = ', Um.ph), paste('Um.hp = ', Um.hp)), 
             as.table = FALSE,
             scales='free', ncol=5, 
             labeller = label_wrap_gen(multi_line=FALSE))

svg('plot.svg', width=10, height=10); g1; dev.off()
png('plot.png', width=1000, height=1000); g1; dev.off()

# ======================================================================================
### (2) Um - Ump
# ======================================================================================

# Um.ph, Um.hp
SEQ1 <- seq(0.5, 1.5, length.out=5)
# Ump.ph, Ump.hp
SEQ2 <- seq(1, 10, length.out=10)

exp4 <- hpModel.exploration(
  # growth rate 
  r.h = 0.15, 
  r.p = 0.15, 
  # basic resources 
  max.h = 100, 
  max.p = 100, 
  # utility
  Um.ph = SEQ1,
  Um.hp = SEQ1,
  # number of types
  n.h = 10,         
  n.p = 10,        
  # undirected variation 
  v.h = 0.15,
  v.p = 0.15,
  # initial conditions
  iniH = 10,
  iniP = 10,
  # proportion of mean utility
  Ump.ph = SEQ2, 
  Ump.hp = SEQ2, 
  # proportion of mean utility
  Kmp.h = 10, 
  Kmp.p = 10, 
  MaxArea = 200)

# delete non-finished runs
EXP <- exp4
EXP[which(EXP$time == 19999),22] <- NA
EXP[which(EXP$time == 19999),23] <- NA


g1 <- ggplot(EXP, aes(x = Ump.ph, y = Ump.hp)) + 
  geom_raster(aes(fill = LM.h)) + 
  geom_point(aes(color = LM.p), shape = 15, size = 2.5) + # 1.75
  #geom_point(aes(size = time), shape = 't') +
  scale_fill_gradientn(
     colors=c('red','white','blue'),
     values=rescale(c(min(exp4$LM.h), 0, max(exp4$LM.h))),
     limits=c(min(exp4$LM.h), max(exp4$LM.h))) + 
  scale_color_gradientn(
     colors=c('red','white','blue', 'darkblue'),
     values=rescale(c(min(exp4$LM.p), 0, 1, max(exp4$LM.p))),
     limits=c(min(exp4$LM.p), max(exp4$LM.p))) + 
  theme_bw() + # + theme(legend.position="none")
  theme(strip.background = element_rect(fill = NA, colour = NA), panel.grid.minor =  element_blank()) +
  facet_wrap(vars(paste('Um.ph = ', Um.ph), paste('Um.hp = ', Um.hp)),
             as.table = FALSE,
             scales='free', ncol=5, 
             labeller = label_wrap_gen(multi_line=FALSE))

svg('plot.svg', width=10, height=10); g1; dev.off()
png('plot.png', width=1000, height=1000); g1; dev.off()

# ======================================================================================
### (3) Ump - max
# ======================================================================================

SEQ1 <- seq(1, 10, length.out=5)
SEQ2 <- seq(50, 300, length.out=10)

exp4 <- hpModel.exploration(
  # growth rate 
  r.h = 0.15, 
  r.p = 0.15, 
  # basic resources 
  max.h = SEQ2, 
  max.p = SEQ2, 
  # utility
  Um.ph = 1,
  Um.hp = 1,
  # number of types
  n.h = 10,         
  n.p = 10,        
  # undirected variation 
  v.h = 0.15,
  v.p = 0.15,
  # initial conditions
  iniH = 10,
  iniP = 10,
  # proportion of mean utility
  Ump.ph = SEQ1, 
  Ump.hp = SEQ1, 
  # proportion of mean utility
  Kmp.h = 10, 
  Kmp.p = 10, 
  MaxArea = 200)

g1 <- ggplot(exp4, aes(x = max.h, y = max.p)) + 
  geom_raster(aes(fill = LM.h)) + 
  geom_point(aes(color = LM.p), shape = 15, size = 2.5) + # 1.75
  #geom_point(aes(size = time), shape = 't') +
  scale_fill_gradientn(
     colors=c('red','white','blue'),
     values=rescale(c(min(exp4$LM.h), 0, max(exp4$LM.h))),
     limits=c(min(exp4$LM.h), max(exp4$LM.h))) + 
  scale_color_gradientn(
     colors=c('red','white','blue', 'darkblue'),
     values=rescale(c(min(exp4$LM.p), 0, 1, max(exp4$LM.p))),
     limits=c(min(exp4$LM.p), max(exp4$LM.p))) + 
  theme_bw() + # + theme(legend.position="none")
  theme(strip.background = element_rect(fill = NA, colour = NA), panel.grid.minor =  element_blank()) +
  facet_wrap(vars(paste('Ump.ph = ', ifelse(Ump.ph == 10, 'z10', Ump.ph)), 
                  paste('Ump.hp = ', ifelse(Ump.hp == 10, 'z10', Ump.hp))), 
             as.table = FALSE,
             scales='free', ncol=5, 
             labeller = label_wrap_gen(multi_line=FALSE), drop = F)

svg('plot.svg', width=10, height=10); g1; dev.off()
png('plot.png', width=1000, height=1000); g1; dev.off()

# ======================================================================================
### (4) Kmp - max
# ======================================================================================

SEQ1 <- seq(1, 10, length.out=5)
SEQ2 <- seq(50, 300, length.out=10)

exp4 <- hpModel.exploration(
  # growth rate 
  r.h = 0.15, 
  r.p = 0.15, 
  # basic resources 
  max.h = SEQ2, 
  max.p = SEQ2, 
  # utility
  Um.ph = 1,
  Um.hp = 1,
  # number of types
  n.h = 10,         
  n.p = 10,        
  # undirected variation 
  v.h = 0.15,
  v.p = 0.15,
  # initial conditions
  iniH = 10,
  iniP = 10,
  # proportion of mean utility
  Ump.ph = 10, 
  Ump.hp = 10, 
  # proportion of mean utility
  Kmp.h = SEQ1, 
  Kmp.p = SEQ1, 
  MaxArea = 200)

g1 <- ggplot(exp4, aes(x = max.h, y = max.p)) + 
  geom_raster(aes(fill = LM.h)) + 
  geom_point(aes(color = LM.p), shape = 15, size = 2.5) + # 1.75
  #geom_point(aes(size = time), shape = 't') +
  scale_fill_gradientn(
     colors=c('red','white','blue'),
     values=rescale(c(min(exp4$LM.h), 0, max(exp4$LM.h))),
     limits=c(min(exp4$LM.h), max(exp4$LM.h))) + 
  scale_color_gradientn(
     colors=c('red','white','blue', 'darkblue'),
     values=rescale(c(min(exp4$LM.p), 0, 1, max(exp4$LM.p))),
     limits=c(min(exp4$LM.p), max(exp4$LM.p))) + 
  theme_bw() + # + theme(legend.position="none")
  theme(strip.background = element_rect(fill = NA, colour = NA), panel.grid.minor =  element_blank()) +
  facet_wrap(vars(paste('Kmp.h = ', ifelse(Kmp.h == 10, 'z10', Kmp.h)), 
                  paste('Kmp.p = ', ifelse(Kmp.p == 10, 'z10', Kmp.p))), 
             as.table = FALSE,
             scales='free', ncol=5, 
             labeller = label_wrap_gen(multi_line=FALSE))

svg('plot.svg', width=10, height=10); g1; dev.off()
png('plot.png', width=1000, height=1000); g1; dev.off()

