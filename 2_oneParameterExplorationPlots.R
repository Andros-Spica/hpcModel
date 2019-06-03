
source("hpModel.run.R")
source("hpModel.exploration.R")
source("hpModel.plot.R")
require(reshape2)
require(ggplot2)
require(scales)

########################################################################################
### exploration of one parameter
### max.h

SEQ <- seq(50, 300, length.out = 30)

exp1 <- hpModel.exploration(
  # growth rate
  r.h = 0.15,
  r.p = 0.15,
  # basic resources
  max.h = SEQ,
  max.p = 100,
  # utility
  Um.ph = 1.1,
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
  # proportion of mean  basic resources
  Kmp.h = 10,
  Kmp.p = 10,
  # settings
  MaxArea = 200,
  maxIt = 20000,
  tol = 6
)

# plot
#svg('plots/2_bifurcation1.svg', width=10, height=10)
png("plots/2_bifurcation1.png")
plot(SEQ,
     exp1$LM.h,
     type = 'l',
     col = 'blue',
     ylim = c(-1, 2),
     ylab = "LM",
     xlab = "max.h")
points(SEQ, exp1$LM.p, type = 'l', col = 'red')
legend(100, 1, legend = c("LM.h", "LM.p"), col = c('blue', 'red'), lty = 1)
dev.off()

### bifurcation plot

result1 <- data.frame()
result2 <- data.frame()

for (i in seq(2, 4, by = 0.01))
{
  exp2 <- hpModel.run(
    # growth rate
    r.h = 0.15,
    r.p = 0.15,
    # basic resources
    max.h = 100,
    max.p = 100,
    # utility
    Um.ph = 0.5,
    Um.hp = 0.75,
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
    Ump.ph = 1,
    Ump.hp = i,
    # proportion of mean  basic resources
    Kmp.h = 10,
    Kmp.p = 10,
    # settings
    MaxArea = 200,
    maxIt = 600,
    tol = Inf,
    saveTrajectories = TRUE
  )
  
  # store the result
  result1 <-
    rbind(result1, data.frame(i, val = exp2$TRAJECTORIES$LM.h[400:599]))
  result2 <-
    rbind(result2, data.frame(i, val = exp2$TRAJECTORIES$LM.p[400:599]))
  
}

#svg('plots/2_bifurcation1.svg', width=10, height=10)
png("plots/2_bifurcation2.png")
par(mfrow = c(2, 1))
plot(result1,
     #pch = '.',
     xlab = 'Ump.hp value',
     ylab = 'LM.h')
plot(result2,
     pch = '.',
     xlab = 'Ump.hp value',
     ylab = 'LM.p')
dev.off()
