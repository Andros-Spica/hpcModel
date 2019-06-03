
source("library/hpcModel.run.R")
source("library/hpcModel.exploration.R")
source("library/hpcModel.plot.R")
require(reshape2)
require(ggplot2)
require(scales)

########################################################################################
### exploration of four parameters

# ======================================================================================
### (1) Um - max
# ======================================================================================

# Um.ph, Um.hp
SEQ1 <- seq(0.5, 1.5, length.out = 5)
# max.h, max.p
SEQ2 <- seq(50, 300, length.out = 5)

exp1 <- hpcModel.exploration(
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
  # maximum local area to be used by populations (multiplier or scaling effect)
  MaxArea = 200,
  # settings 
  # simulation flow & data
  maxIt = 20000,
  tol = 6,
  saveTrajectories = FALSE,
  messages = TRUE
  
)

g1 <- ggplot(exp1, aes(x = max.h, y = max.p)) +
  geom_raster(aes(fill = LM.h)) +
  geom_point(aes(color = LM.p), shape = 15, size = 2.5) + # 1.75
  #geom_point(aes(size = time), shape = 't') +
  scale_fill_gradientn(
    colors = c('red', 'white', 'blue'),
    values = rescale(c(min(exp1$LM.h), 0, max(exp1$LM.h))),
    limits = c(min(exp1$LM.h), max(exp1$LM.h))
  ) +
  scale_color_gradientn(
    colors = c('red', 'white', 'blue', 'darkblue'),
    values = rescale(c(min(exp1$LM.p), 0, 1, max(exp1$LM.p))),
    limits = c(min(exp1$LM.p), max(exp1$LM.p))
  ) +
  theme_bw() + # + theme(legend.position="none")
  theme(strip.background = element_rect(fill = NA, colour = NA),
        panel.grid.minor =  element_blank()) +
  facet_wrap(
    vars(paste('Um.ph = ', Um.ph), paste('Um.hp = ', Um.hp)),
    as.table = FALSE,
    scales = 'free',
    ncol = 5,
    labeller = label_wrap_gen(multi_line = FALSE)
  )

svg('plots/4_Um-max-plot.svg',
    width = 10,
    height = 10)
g1
dev.off()
png('plots/4_Um-max-plot.png',
    width = 1000,
    height = 1000)
g1
dev.off()


# ======================================================================================
### (2) Um - Ump
# ======================================================================================

# Um.ph, Um.hp
SEQ3 <- seq(0.5, 1.5, length.out = 5)
# max.h, max.p
SEQ4 <- seq(1, 10, length.out = 10)

exp2 <- hpcModel.exploration(
  # growth rate
  r.h = 0.15,
  r.p = 0.15,
  # basic resources:
  # population of type 1 that can be sustained by resources independent of HP relationship
  max.h = 100,
  max.p = 100,
  # Utility of individuals of type N
  Um.ph = SEQ3,
  Um.hp = SEQ3,
  # number of types
  n.h = 10,
  n.p = 10,
  # undirected variation
  v.h = 0.15,
  v.p = 0.15,
  # initial populations
  iniH = 10,
  iniP = 10,
  # proportion of mean utility:
  # How less utility has type 1 individuals in relation to type N
  Ump.ph = SEQ4,
  Ump.hp = SEQ4,
  # How less population of type N can be sustained by resources
  # # independent of HP relationship in relation to type 1
  Kmp.h = 10,
  Kmp.p = 10,
  # maximum local area to be used by populations (multiplier or scaling effect)
  MaxArea = 200,
  # settings 
  # simulation flow & data
  maxIt = 20000,
  tol = 6,
  saveTrajectories = FALSE,
  messages = TRUE
)

g2 <- ggplot(exp2, aes(x = Ump.ph, y = Ump.hp)) +
  geom_raster(aes(fill = LM.h)) +
  geom_point(aes(color = LM.p), shape = 15, size = 2.5) + # 1.75
  #geom_point(aes(size = time), shape = 't') +
  scale_fill_gradientn(
    colors = c('red', 'white', 'blue'),
    values = rescale(c(min(exp2$LM.h), 0, max(exp2$LM.h))),
    limits = c(min(exp2$LM.h), max(exp2$LM.h))
  ) +
  scale_color_gradientn(
    colors = c('red', 'white', 'blue', 'darkblue'),
    values = rescale(c(min(exp2$LM.p), 0, 1, max(exp2$LM.p))),
    limits = c(min(exp2$LM.p), max(exp2$LM.p))
  ) +
  theme_bw() + # + theme(legend.position="none")
  theme(strip.background = element_rect(fill = NA, colour = NA),
        panel.grid.minor =  element_blank()) +
  facet_wrap(
    vars(paste('Um.ph = ', Um.ph), paste('Um.hp = ', Um.hp)),
    as.table = FALSE,
    scales = 'free',
    ncol = 5,
    labeller = label_wrap_gen(multi_line = FALSE)
  )

png('plots/4_Um-Ump-plot.png',
    width = 1000,
    height = 1000)
g2
dev.off()


# ======================================================================================
### (3) Ump - max
# ======================================================================================

# Um.ph, Um.hp
SEQ5 <- seq(1, 10, length.out = 5)
# max.h, max.p
SEQ6 <- seq(50, 300, length.out = 10)

exp3 <- hpcModel.exploration(
  # growth rate
  r.h = 0.15,
  r.p = 0.15,
  # basic resources:
  # population of type 1 that can be sustained by resources independent of HP relationship
  max.h = SEQ6,
  max.p = SEQ6,
  # Utility of individuals of type N
  Um.ph = 1,
  Um.hp = 1,
  # number of types
  n.h = 10,
  n.p = 10,
  # undirected variation
  v.h = 0.15,
  v.p = 0.15,
  # initial populations
  iniH = 10,
  iniP = 10,
  # proportion of mean utility:
  # How less utility has type 1 individuals in relation to type N
  Ump.ph = SEQ5,
  Ump.hp = SEQ5,
  # How less population of type N can be sustained by resources
  # # independent of HP relationship in relation to type 1
  Kmp.h = 10,
  Kmp.p = 10,
  # maximum local area to be used by populations (multiplier or scaling effect)
  MaxArea = 200,
  # settings 
  # simulation flow & data
  maxIt = 20000,
  tol = 6,
  saveTrajectories = FALSE,
  messages = TRUE
)

g3 <- ggplot(exp3, aes(x = max.h, y = max.p)) +
  geom_raster(aes(fill = LM.h)) +
  geom_point(aes(color = LM.p), shape = 15, size = 2.5) + # 1.75
  #geom_point(aes(size = time), shape = 't') +
  scale_fill_gradientn(
    colors = c('red', 'white', 'blue'),
    values = rescale(c(min(exp3$LM.h), 0, max(exp3$LM.h))),
    limits = c(min(exp3$LM.h), max(exp3$LM.h))
  ) +
  scale_color_gradientn(
    colors = c('red', 'white', 'blue', 'darkblue'),
    values = rescale(c(min(exp3$LM.p), 0, 1, max(exp3$LM.p))),
    limits = c(min(exp3$LM.p), max(exp3$LM.p))
  ) +
  theme_bw() + # + theme(legend.position="none")
  theme(strip.background = element_rect(fill = NA, colour = NA),
        panel.grid.minor =  element_blank()) +
  facet_wrap(
    vars(paste(
      'Ump.ph = ', ifelse(Ump.ph == 10, 'z10', Ump.ph)
    ),
    paste(
      'Ump.hp = ', ifelse(Ump.hp == 10, 'z10', Ump.hp)
    )),
    as.table = FALSE,
    scales = 'free',
    ncol = 5,
    labeller = label_wrap_gen(multi_line = FALSE),
    drop = F
  )

png('plots/4_Ump-max-plot.png',
    width = 1000,
    height = 1000)
g3
dev.off()


# ======================================================================================
### (4) Kmp - max
# ======================================================================================

# Um.ph, Um.hp
SEQ7 <- seq(1, 10, length.out = 5)
# max.h, max.p
SEQ8 <- seq(50, 300, length.out = 10)

exp4 <- hpcModel.exploration(
  # growth rate
  r.h = 0.15,
  r.p = 0.15,
  # basic resources
  max.h = SEQ8,
  max.p = SEQ8,
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
  Kmp.h = SEQ7,
  Kmp.p = SEQ7,
  # maximum local area to be used by populations (multiplier or scaling effect)
  MaxArea = 200,
  # settings 
  # simulation flow & data
  maxIt = 20000,
  tol = 6,
  saveTrajectories = FALSE,
  messages = TRUE
)

g4 <- ggplot(exp4, aes(x = max.h, y = max.p)) +
  geom_raster(aes(fill = LM.h)) +
  geom_point(aes(color = LM.p), shape = 15, size = 2.5) + # 1.75
  #geom_point(aes(size = time), shape = 't') +
  scale_fill_gradientn(
    colors = c('red', 'white', 'blue'),
    values = rescale(c(min(exp4$LM.h), 0, max(exp4$LM.h))),
    limits = c(min(exp4$LM.h), max(exp4$LM.h))
  ) +
  scale_color_gradientn(
    colors = c('red', 'white', 'blue', 'darkblue'),
    values = rescale(c(min(exp4$LM.p), 0, 1, max(exp4$LM.p))),
    limits = c(min(exp4$LM.p), max(exp4$LM.p))
  ) +
  theme_bw() + # + theme(legend.position="none")
  theme(strip.background = element_rect(fill = NA, colour = NA),
        panel.grid.minor =  element_blank()) +
  facet_wrap(
    vars(paste('Kmp.h = ', ifelse(
      Kmp.h == 10, 'z10', Kmp.h
    )),
    paste('Kmp.p = ', ifelse(
      Kmp.p == 10, 'z10', Kmp.p
    ))),
    as.table = FALSE,
    scales = 'free',
    ncol = 5,
    labeller = label_wrap_gen(multi_line = FALSE)
  )

png('plots/4_Kmp-max-plot.png',
    width = 1000,
    height = 1000)
g4
dev.off()
