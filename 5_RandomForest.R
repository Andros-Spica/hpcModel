
source("hpModel.run.R")
source("hpModel.exploration.R")
source("hpModel.plot.R")
require(reshape2)
require(ggplot2)
require(scales)

########################################################################################
### exploring paramenter space with **random forest** (NEW VERSION WITHOUT TESTING!!!)

require(DoE.wrapper)    # LHC
require(randomForest)   # RF
require(party)          # classification / regression tree
require(parallel)       # use parallelization, it takes 8 min aprox

### generate LHS design (strauss method, with 0.2 radious interaction)
# it takes 45 min aprox
if (FALSE)
{
  LHS <- lhs.design(
    10000,
    8,
    'strauss',
    seed = 777,
    factor.names = list(
      max.h = c(50, 300),
      max.p = c(50, 300),
      Um.ph = c(0.5, 1.5),
      Um.hp = c(0.5, 1.5),
      Ump.ph = c(1, 10),
      Ump.hp = c(1, 10),
      Kmp.h = c(1, 10),
      Kmp.p = c(1, 10)
    ),
    digits = 2,
    RND = 0.2
  )
}

# load
load('LHS.RData')

# build PARS
PARS <- as.data.frame(LHS)

# build cluster
cl <- makeCluster(6)
clusterExport(cl, list('hpModel.run', 'Fitness', 'PARS'), envir = environment())

# loop
ret <- parLapply(cl, 1:nrow(PARS),
                 function(p) {
                   RESULTS <- hpModel.run(
                     r.h = 0.15,
                     r.p = 0.15,
                     max.h = PARS[p, 1],
                     max.p = PARS[p, 2],
                     Um.ph = PARS[p, 3],
                     Um.hp = PARS[p, 4],
                     n.h = 10,
                     n.p = 10,
                     v.h = 0.15,
                     v.p = 0.15,
                     iniH = 10,
                     iniP = 10,
                     Ump.ph = PARS[p, 5],
                     Ump.hp = PARS[p, 6],
                     Kmp.h = PARS[p, 7],
                     Kmp.p = PARS[p, 8],
                     MaxArea = 200,
                     maxIt = 20000,
                     tol = 6,
                     saveTrajectories = FALSE,
                     messages = FALSE
                   )
                   return(as.data.frame(RESULTS$END))
                 })

# stop cluster and clean
stopCluster(cl)
rm(cl)
gc()

# bind result
ret <- do.call('rbind', ret)

# build object
RES <- cbind(PARS, ret)

# eliminate the non-finished runs
RES <- RES[RES$time != 19999, ]

# load
load('RES.RData')

# colapsed plots
g5.1 <-
  ggplot(RES, aes(x = max.h)) +
  stat_smooth(aes(y = LM.h),
              alpha = 0.5,
              level = 0.9999999,
              colour = 'blue') +
  geom_point(aes(y = LM.h), size = 0.05, colour = 'blue') +
  stat_smooth(aes(y = LM.p),
              alpha = 0.5,
              level = 0.9999999,
              colour = 'red') +
  geom_point(aes(y = LM.p), size = 0.05, colour = 'red')

#svg('plots/5_collapsed.svg', width=10, height=10)
png("plots/5_collapsed.png")
g5.1
dev.off()

# Random Forest
RF.1 <-
  randomForest(
    LM.p ~ max.h + max.p + Um.ph + Um.hp + Ump.ph + Ump.hp + Kmp.h + Kmp.p,
    data = RES,
    mtry = 5,
    ntree = 1000,
    importance = T
  )
acc <- mean((predict(RF.1, RES[, 1:8]) - RES$LM.h) ^ 2)

# plot

#svg('plots/5_randomForest1.svg', width=10, height=10)
png("plots/5_randomForest1.png")
layout(matrix(1:2, ncol = 2))
importance(RF.1)
varImpPlot(RF.1)
dev.off()

# --------------------------------------------------------------------------------------
### classification Random Forest

# aux vector
# check evolution and put set 0 = no co-evolution, 1 = only cultivation, 2 = only domestication, 3 = both
aux <- apply(RES, 1, function(x) {
  if (x[9] != 0 & x[10] != 0) {
    return(3)
  } else
    if (x[9] != 0 & x[10] == 0) {
      return(1)
    } else
      if (x[9] == 0 & x[10] != 0) {
        return(2)
      } else
        if (x[9] == 0 & x[10] == 0) {
          return(0)
        }
})
aux <- factor(aux)

### random forest
RF.2 <- randomForest(RES[, 1:8],
                   aux,
                   mtry = 3,
                   ntree = 500,
                   importance = T)

#svg('plots/5_randomForest2.svg', width=10, height=10)
png("plots/5_randomForest2.png")
layout(matrix(1:2, ncol = 2))
importance(RF.2)
varImpPlot(RF.2)
dev.off()

# stats
table(predict(RF.2, RES[, 1:8]), aux) # perfect fit


### manual tuning ----------------------------------------------------------------------

# prepare data

set.seed(777)

RES.r <- RES[sample(1:nrow(RES), nrow(RES)), ]

RES.spl  <-
  split(as.data.frame(RES.r), f = factor(ceiling(seq_along(1:nrow(
    RES.r
  )) / 1000)))

Grid <- expand.grid(c(2:6), c(500, 750, 1000))

# cluster and loop

cl <- makeCluster(6)

clusterExport(cl, list('randomForest', 'Grid', 'RES.spl'), envir = environment())

AUX <- parSapply(cl, 1:nrow(Grid), function(p) {
  aux <- c()
  for (i in 1:10) {
    X <- do.call(rbind, RES.spl[-i])
    RF.h <-
      randomForest(
        LM.h ~ max.h + max.p + Um.ph + Um.hp + Ump.ph + Ump.hp + Kmp.h + Kmp.p,
        data = X,
        mtry = Grid[p, 1],
        ntree = Grid[p, 2]
      )
    RF.p <-
      randomForest(
        LM.p ~ max.h + max.p + Um.ph + Um.hp + Ump.ph + Ump.hp + Kmp.h + Kmp.p,
        data = X,
        mtry = Grid[p, 1],
        ntree = Grid[p, 2]
      )
    acc.h <-
      mean((predict(RF.h, RES.spl[[i]][, 1:8]) - RES.spl[[i]]$LM.h) ^ 2)
    acc.p <-
      mean((predict(RF.p, RES.spl[[i]][, 1:8]) - RES.spl[[i]]$LM.p) ^ 2)
    aux[i] <- acc.h + acc.p
  }
  return(mean(aux))
})

stopCluster(cl)

rm(cl)

gc()

# --------------------------------------------------------------------------------------

### timing plots

p1 <- ggplot(RES, aes(x = max.h)) +
  stat_smooth(aes(y = time),
              alpha = 0.5,
              level = 0.95,
              colour = 'blue')
p2 <- ggplot(RES, aes(x = max.p)) +
  stat_smooth(aes(y = time),
              alpha = 0.5,
              level = 0.95,
              colour = 'blue')
p3 <- ggplot(RES, aes(x = Um.ph)) +
  stat_smooth(aes(y = time),
              alpha = 0.5,
              level = 0.95,
              colour = 'blue')
p4 <- ggplot(RES, aes(x = Um.hp)) +
  stat_smooth(aes(y = time),
              alpha = 0.5,
              level = 0.95,
              colour = 'blue')
p5 <- ggplot(RES, aes(x = Ump.ph)) +
  stat_smooth(aes(y = time),
              alpha = 0.5,
              level = 0.95,
              colour = 'blue')
p6 <- ggplot(RES, aes(x = Ump.hp)) +
  stat_smooth(aes(y = time),
              alpha = 0.5,
              level = 0.95,
              colour = 'blue')
p7 <- ggplot(RES, aes(x = Kmp.h)) +
  stat_smooth(aes(y = time),
              alpha = 0.5,
              level = 0.95,
              colour = 'blue')
p8 <- ggplot(RES, aes(x = Kmp.p)) +
  stat_smooth(aes(y = time),
              alpha = 0.5,
              level = 0.95,
              colour = 'blue')

#svg('plots/5_randomForest3.svg', width=10, height=10)
png("plots/5_randomForest3.png")
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol = 2)
dev.off()

# only coevolution (time difference)
RES.aux <- RES[which(RES$evol.h != 0 & RES$evol.p != 0), ]

p1 <- ggplot(RES.aux, aes(x = max.h)) +
  stat_smooth(
    aes(y = evol.h - evol.p),
    alpha = 0.5,
    level = 0.95,
    colour = 'blue'
  )
p2 <- ggplot(RES.aux, aes(x = max.p)) +
  stat_smooth(
    aes(y = evol.h - evol.p),
    alpha = 0.5,
    level = 0.95,
    colour = 'blue'
  )
p3 <- ggplot(RES.aux, aes(x = Um.ph)) +
  stat_smooth(
    aes(y = evol.h - evol.p),
    alpha = 0.5,
    level = 0.95,
    colour = 'blue'
  )
p4 <- ggplot(RES.aux, aes(x = Um.hp)) +
  stat_smooth(
    aes(y = evol.h - evol.p),
    alpha = 0.5,
    level = 0.95,
    colour = 'blue'
  )
p5 <- ggplot(RES.aux, aes(x = Ump.ph)) +
  stat_smooth(
    aes(y = evol.h - evol.p),
    alpha = 0.5,
    level = 0.95,
    colour = 'blue'
  )
p6 <- ggplot(RES.aux, aes(x = Ump.hp)) +
  stat_smooth(
    aes(y = evol.h - evol.p),
    alpha = 0.5,
    level = 0.95,
    colour = 'blue'
  )
p7 <- ggplot(RES.aux, aes(x = Kmp.h)) +
  stat_smooth(
    aes(y = evol.h - evol.p),
    alpha = 0.5,
    level = 0.95,
    colour = 'blue'
  )
p8 <- ggplot(RES.aux, aes(x = Kmp.p)) +
  stat_smooth(
    aes(y = evol.h - evol.p),
    alpha = 0.5,
    level = 0.95,
    colour = 'blue'
  )

#svg('plots/5_randomForest4.svg', width=10, height=10)
png("plots/5_randomForest4.png")
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol = 2)
dev.off()
