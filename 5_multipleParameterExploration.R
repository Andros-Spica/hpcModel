
source("library/hpcModel.run.R")
source("library/hpcModel.exploration.R")
source("library/hpcModel.plot.R")
require(reshape2)
require(ggplot2)
require(scales)

########################################################################################
### exploring paramenter space with **random forest** (NEW VERSION WITHOUT TESTING!!!)

require(DoE.wrapper)    # LHC
require(randomForest)   # RF
require(party)          # classification / regression tree
require(parallel)       # use parallelization, it takes 8 min aprox


run.LHC.experiment <- function(experimentSettings, cluster)
{
  parLapply(cluster, 1:nrow(experimentSettings),
            function(experimentIndex) {
              RESULTS <- hpcModel.run(
                iniH = 10,
                iniP = 10,
                n.H = experimentSettings[experimentIndex, 1],         
                n.P = experimentSettings[experimentIndex, 2],        
                v.H = experimentSettings[experimentIndex, 3],
                v.P = experimentSettings[experimentIndex, 4],
                r.H = 0.04, 
                r.P = 0.1, 
                mU.PnH = experimentSettings[experimentIndex, 5],
                mU.HnP = experimentSettings[experimentIndex, 6],
                mU.P1H = experimentSettings[experimentIndex, 7],                                  
                mU.H1P = experimentSettings[experimentIndex, 8],                                   
                U.bHn = experimentSettings[experimentIndex, 9],                                
                U.bPn = experimentSettings[experimentIndex, 10], 
                U.bH1 = experimentSettings[experimentIndex, 11],                               
                U.bP1 = experimentSettings[experimentIndex, 12],
                MaxArea = 200,
                maxIt = 5000,
                tol = 6,
                saveTrajectories = FALSE,
                messages = FALSE
              )
              return(as.data.frame(RESULTS$END))
            })
}

collapsed.ggplot <- function(results, parameterName, variableName1, variableName2)
{
  ggplot(results, aes_string(x = parameterName)) +
    stat_smooth(aes_string(y = variableName1),
                alpha = 0.5,
                level = 0.9999999,
                colour = 'blue') +
    geom_point(aes_string(y = variableName1), size = 0.05, colour = 'blue') +
    stat_smooth(aes_string(y = variableName2),
                alpha = 0.5,
                level = 0.9999999,
                colour = 'red') +
    geom_point(aes_string(y = variableName2), size = 0.05, colour = 'red')
}


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
      n.H = c(3, 50),         
      n.P = c(3, 50),        
      v.H = c(0.1, 0.3),
      v.P = c(0.1, 0.3),
      mU.PnH = c(0, 3),
      mU.HnP = c(0, 3),
      mU.P1H = c(0, 3),                                  
      mU.H1P = c(0, 3),                                   
      U.bHn = c(0, 300),                                
      U.bPn = c(0, 300), 
      U.bH1 = c(0, 300),                               
      U.bP1 = c(0, 300)
    ),
    digits = 2,
    RND = 0.2
  )
}
save(LHS, file = "LHS.RData")

#---

# load
load('LHS.RData')

# build PARS
PARS <- as.data.frame(LHS)

# Create versions of PARS depending on assumptions
# # mU.PnH > mU.P1H (mutualistic plant types give more utility)

# # mU.HnP > mU.H1P (mutualistic human types give more utility)

# # mU.PnH > mU.P1H AND mU.HnP > mU.H1P (mutualistic types give more utility)

# # U.bP1 > U.bPn (non-mutualistic plant types obtain more utility from other resources)

# # U.bH1 > U.bHn (non-mutualistic human types obtain more utility from other resources)

# # mU.PnH > mU.P1H AND mU.HnP > mU.H1P (non-mutualistic types obtain more utility from other resources)

#---

# build cluster
cl <- makeCluster(6)
clusterExport(cl, list('hpcModel.run', 'Fitness', 'PARS'), envir = environment())

# loop
ret <- run.LHC.experiment(PARS, cl)

# stop cluster and clean
stopCluster(cl)
rm(cl)
gc()

# bind result
ret <- do.call('rbind', ret)

# build object
RES <- cbind(PARS, ret)

# eliminate the non-finished runs
#RES <- RES[RES$time != 19999, ]

# load
load('RES.RData')

# colapsed plots

#svg('plots/5_collapsed.svg', width=10, height=10)
png("plots/5_collapsed-ggplot.png")
collapsed.ggplot(RES, 'mU.HnP', 'coevo.H', 'coevo.P')
dev.off()

# Random Forest
RF.1 <-
  randomForest(
    coevo.H ~ n.H + n.P + v.H + v.P + mU.HnP + mU.H1P + mU.PnH + mU.P1H + U.bP1 + U.bPn + U.bH1 + U.bHn,
    data = RES,
    mtry = 5,
    ntree = 1000,
    importance = T
  )
acc <- mean((predict(RF.1, RES[, 1:12]) - RES$coevo.H) ^ 2)

# plot

#svg('plots/5_randomForest1.svg', width=10, height=10)
png("plots/5_randomForest-coevo.H.png")
layout(matrix(1:2, ncol = 2))
importance(RF.1)
varImpPlot(RF.1)
dev.off()

# --------------------------------------------------------------------------------------
### classification Random Forest

# aux vector
# check evolution and put set 0 = no co-evolution, 1 = only cultivation, 2 = only domestication, 3 = both
# ******* ADAPT !!!!!! ********
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
RF.2 <- randomForest(RES[, 1:12],
                   aux,
                   mtry = 3,
                   ntree = 500,
                   importance = T)

#svg('plots/5_randomForest2.svg', width=10, height=10)
png("plots/5_randomForest-split-end-states.png")
layout(matrix(1:2, ncol = 2))
importance(RF.2)
varImpPlot(RF.2)
dev.off()

# stats
table(predict(RF.2, RES[, 1:12]), aux) # perfect fit


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
        coevo.H ~ n.H + n.P + v.H + v.P + mU.HnP + mU.H1P + mU.PnH + mU.P1H + U.bP1 + U.bPn + U.bH1 + U.bHn,
        data = X,
        mtry = Grid[p, 1],
        ntree = Grid[p, 2]
      )
    RF.p <-
      randomForest(
        coevo.H ~ n.H + n.P + v.H + v.P + mU.HnP + mU.H1P + mU.PnH + mU.P1H + U.bP1 + U.bPn + U.bH1 + U.bHn,
        data = X,
        mtry = Grid[p, 1],
        ntree = Grid[p, 2]
      )
    acc.h <-
      mean((predict(RF.h, RES.spl[[i]][, 1:12]) - RES.spl[[i]]$coevo.h) ^ 2)
    acc.p <-
      mean((predict(RF.p, RES.spl[[i]][, 1:12]) - RES.spl[[i]]$coevo.p) ^ 2)
    aux[i] <- acc.h + acc.p
  }
  return(mean(aux))
})

stopCluster(cl)

rm(cl)

gc()

# --------------------------------------------------------------------------------------

### timing plots
### ****** NEEDS TO ADAPT !!!!! ********

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
png("plots/5_timingPlots.png")
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
