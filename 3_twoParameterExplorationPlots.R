
source("hpModel.run.R")
source("hpModel.exploration.R")
source("hpModel.plot.R")
require(reshape2)
require(ggplot2)
require(scales)

########################################################################################
### exploration of two parameters
# Um.ph, Um.hp

SEQ <- seq(0.1, 2, length.out = 14)

exp3 <- hpModel.exploration(
  # growth rate
  r.h = 0.15,
  r.p = 0.15,
  # basic resources
  max.h = 80,
  max.p = 100,
  # utility
  Um.ph = SEQ,
  Um.hp = SEQ,
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
  MaxArea = 200,
  maxIt = 20000,
  tol = 6
)

#svg('plots/3_Um-Um.svg', width=10, height=10)
png("plots/3_Um-Um.png")
ggplot(exp3,
       aes(x = Um.ph, y = Um.hp)) +
  geom_raster(aes(fill = LM.h)) +
  geom_point(aes(color = LM.p),
             shape = 15, size = 10) +
  geom_point(aes(size = time),
             shape = 't') +
  scale_fill_gradientn(
    colors = c('red', 'white', 'blue'),
    values = rescale(c(min(exp3$LM.h), 0, max(exp3$LM.h))),
    limits = c(min(exp3$LM.h), max(exp3$LM.h))
  ) +
  scale_color_gradientn(
    colors = c('red', 'white', 'blue'),
    values = rescale(c(min(exp3$LM.p), 0, max(exp3$LM.p))),
    limits = c(min(exp3$LM.p), max(exp3$LM.p))
  ) +
  theme_bw() + # + theme(legend.position="none")
  theme(strip.background = element_rect(fill = NA, colour = NA),
        panel.grid.minor =  element_blank())
dev.off()
