
source("hpModel.run.R")
source("hpModel.exploration.R")
source("hpModel.plot.R")
require(reshape2)
require(ggplot2)
require(scales)

########################################################################################
### single run (pretty visual simulation)

exp1 <- hpModel.run(
  # growth rate 
  r.h = 0.05,         # 0.05
  r.p = 0.1,          # 0.1
  # basic resources 
  max.h = 80,         # 80
  max.p = 100,        # 100
  # utility
  Um.ph = 1.7,        # 1.7
  Um.hp = 1,          # 1
  # number of types
  n.h = 10,           # 10
  n.p = 10,           # 10
  # undirected variation 
  v.h = 0.15,         # 0.15
  v.p = 0.15,         # 0.15
  # initial conditions
  iniH = 10,          # 10
  iniP = 10,          # 10
  # proportion of mean utility
  Ump.ph = 10,        # 10
  Ump.hp = 100,       # 100
  # proportion of mean basic resources
  Kmp.h = 100,        # 100
  Kmp.p = 100,        # 100
  # settings 
  MaxArea = 200,
  maxIt = 1000,
  tol = 4,
  saveTrajectories = TRUE, 
  PLOT = F, SLEEP=0.1)

hpModel.plot(exp1); exp1$END

g.data <- data.frame(H = exp1$TRAJECTORIES$H, P = exp1$TRAJECTORIES$P)[!is.na(exp1$TRAJECTORIES$H),]
g.data$time <- 1:nrow(g.data)
g.data <- melt(g.data, measure.vars = c('H', 'P'))
p <- ggplot(g.data, aes(x = time, y = value, colour=variable)) + geom_line() + 
    scale_color_manual(values=c('blue', 'red'))


exp1 <- hpModel.run(
  # growth rate 
  r.h = 0.15,         
  r.p = 0.15,         
  # basic resources 
  max.h = 140,     
  max.p = 145,       
  # utility
  Um.ph = 0.73,     
  Um.hp = 0.69,        
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
  Ump.ph = 2.11,     
  Ump.hp = 3.32,      
  # proportion of mean  basic resources
  Kmp.h = 2.87,     
  Kmp.p = 1.15, 
  # settings 
  MaxArea = 200,
  maxIt = 20000,
  tol = 6, PLOT=T)


### bifurcation plot

result1 <- data.frame()
result2 <- data.frame()

for (i in seq(2, 4, by=0.01)) 
{
  exp1 <- hpModel.run(
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
  tol = Inf, saveTrajectories = TRUE)

  # store the result 
  result1 <- rbind(result1, data.frame(i, val=exp1$TRAJECTORIES$LM.h[400:599])  )
  result2 <- rbind(result2, data.frame(i, val=exp1$TRAJECTORIES$LM.p[400:599])  )

}

par(mfrow=c(2,1))
plot(result1, pch='.', xlab='Ump.hp value', ylab='LM.h')
plot(result2, pch='.', xlab='Ump.hp value', ylab='LM.p')



########################################################################################
### exploration of one parameter

SEQ <- seq(50, 300, length.out=30)

exp2 <- hpModel.exploration(
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
  tol = 6)

# plot 
plot(SEQ, exp2$LM.h, type='l', col='blue', ylim=c(-1, 2))
points(SEQ, exp2$LM.p, type='l', col='red')

########################################################################################
### exploration of two parameters
# Um.ph, Um.hp

SEQ <- seq(0.1, 2, length.out=14)

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
  tol = 6)


ggplot(exp3, aes(x = Um.ph, y = Um.hp)) + 
  geom_raster(aes(fill=LM.h)) + 
  geom_point(aes(color = LM.p), shape = 15, size = 10) +
  geom_point(aes(size = time), shape = 't') +
  scale_fill_gradientn(
   colors=c('red','white','blue'),
   values=rescale(c(min(exp3$LM.h), 0, max(exp3$LM.h))),
   limits=c(min(exp3$LM.h), max(exp3$LM.h))) + 
  scale_color_gradientn(
   colors=c('red','white','blue'),
   values=rescale(c(min(exp3$LM.p), 0, max(exp3$LM.p))),
   limits=c(min(exp3$LM.p), max(exp3$LM.p))) + 
  theme_bw() + # + theme(legend.position="none")
  theme(strip.background = element_rect(fill = NA, colour = NA), panel.grid.minor =  element_blank())


########################################################################################
### exploration of four parameters

# Um.ph, Um.hp
SEQ1 <- seq(0.5, 1.5, length.out=5)
# max.h, max.p
SEQ2 <- seq(50, 300, length.out=10)

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
  geom_point(aes(color = LM.p), shape = 15, size = 1.75) +
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

########################################################################################
### exploring paramenter space with **random forest** (NEW VERSION WITHOUT TESTING!!!)

require(DoE.wrapper)    # LHC
require(randomForest)   # RF
require(party)          # classification / regression tree
require(parallel)       # use parallelization, it takes 8 min aprox 

### generate LHS design (strauss method, with 0.2 radious interaction)
# it takes 45 min aprox 
LHS <- lhs.design(
  10000, 8, 'strauss',
  seed = 777,
  factor.names = list(
    max.h = c(50, 300),
    max.p = c(50, 300),
    Um.ph = c(0.5, 1.5),
    Um.hp = c(0.5, 1.5),
    Ump.ph = c(1, 10),
    Ump.hp = c(1, 10),
    Kmp.h = c(1, 10),
    Kmp.p = c(1, 10)),
  digits = 2, RND = 0.2)

# load 
load('LHS.RData')

# build PARS
PARS <- as.data.frame(LHS)

# build cluster 
cl <- makeCluster(6)
clusterExport(cl, list('hpModel.run', 'Fitness', 'PARS'), envir=environment())
# loop
ret <- parLapply(cl, 1:nrow(PARS), 
                 function(p) {
                   RESULTS <- hpModel.run(
                     r.h = 0.15, 
                     r.p = 0.15, 
                     max.h = PARS[p,1],                               
                     max.p = PARS[p,2],                                
                     Um.ph = PARS[p,3],
                     Um.hp = PARS[p,4],
                     n.h = 10,         
                     n.p = 10,        
                     v.h = 0.15,
                     v.p = 0.15,
                     iniH = 10,
                     iniP = 10,
                     Ump.ph = PARS[p,5],                                  
                     Ump.hp = PARS[p,6],                                   
                     Kmp.h = PARS[p,7],                                
                     Kmp.p = PARS[p,8], 
                     MaxArea = 200,
                     maxIt = 20000,
                     tol = 6,
                     saveTrajectories = FALSE,
                     messages = FALSE)
                   return(as.data.frame(RESULTS$END))
                   })

# stop cluster and clean
stopCluster(cl); rm(cl); gc()
# bind result 
ret <- do.call('rbind', ret)

# build object
RES <- cbind(PARS, ret)

# eliminate the non-finished runs 
RES <- RES[RES$time != 19999,]

# load
load('RES.RData')

# colapsed plots 
p <- ggplot(RES, aes(x = max.h)) + 
    stat_smooth(aes(y = LM.h), alpha = 0.5, level = 0.9999999, colour = 'blue') + 
    geom_point(aes(y = LM.h), size = 0.05, colour = 'blue') + 
    stat_smooth(aes(y = LM.p), alpha = 0.5, level = 0.9999999, colour = 'red') + 
    geom_point(aes(y = LM.p), size = 0.05, colour = 'red') 

# Random Forest 
RF <- randomForest(LM.p ~ max.h+max.p+Um.ph+Um.hp+Ump.ph+Ump.hp+Kmp.h+Kmp.p, data=RES, 
        mtry=5, ntree=1000, importance=T)
acc <- mean((predict(RF, RES[,1:8]) - RES$LM.h)^2)

# plot 
importance(RF); varImpPlot(RF)



# --------------------------------------------------------------------------------------
### classification Random Forest

# aux vector
# check evolution and put set 0 = no co-evolution, 1 = only cultivation, 2 = only domestication, 3 = both
aux <- apply(RES, 1, function(x) {
        if (x[9] != 0 & x[10] != 0) {return(3)} else 
        if (x[9] != 0 & x[10] == 0) {return(1)} else
        if (x[9] == 0 & x[10] != 0) {return(2)} else 
        if (x[9] == 0 & x[10] == 0) {return(0)}
        })
aux <- factor(aux)

### random forest 
RF <-randomForest(RES[,1:8], aux, mtry=3, ntree=500, importance=T)
importance(RF); varImpPlot(RF)

# stats 
table(predict(RF, RES[,1:8]), aux) # perfect fit 


### manual tuning ----------------------------------------------------------------------
# prepare data
set.seed(777)
RES.r <- RES[sample(1:nrow(RES), nrow(RES)),]
RES.spl  <- split(as.data.frame(RES.r), f=factor(ceiling(seq_along(1:nrow(RES.r))/1000)))
Grid <- expand.grid(c(2:6), c(500, 750, 1000))
# cluster and loop 
cl <- makeCluster(6)
clusterExport(cl, list('randomForest', 'Grid', 'RES.spl'), envir=environment())
AUX <- parSapply(cl, 1:nrow(Grid), function(p) {
    aux <- c()
    for (i in 1:10) {
        X <- do.call(rbind, RES.spl[-i])
        RF.h <- randomForest(LM.h ~ max.h+max.p+Um.ph+Um.hp+Ump.ph+Ump.hp+Kmp.h+Kmp.p, data=X, 
              mtry=Grid[p,1], ntree=Grid[p,2])
        RF.p <- randomForest(LM.p ~ max.h+max.p+Um.ph+Um.hp+Ump.ph+Ump.hp+Kmp.h+Kmp.p, data=X, 
              mtry=Grid[p,1], ntree=Grid[p,2])
        acc.h <- mean((predict(RF.h, RES.spl[[i]][,1:8]) - RES.spl[[i]]$LM.h)^2)
        acc.p <- mean((predict(RF.p, RES.spl[[i]][,1:8]) - RES.spl[[i]]$LM.p)^2)
        aux[i] <- acc.h + acc.p
    }
    return(mean(aux))
})
stopCluster(cl); rm(cl); gc()
# --------------------------------------------------------------------------------------

### timing plots 

p1 <- ggplot(RES, aes(x = max.h)) + 
    stat_smooth(aes(y = time), alpha = 0.5, level = 0.95, colour = 'blue') 
p2 <- ggplot(RES, aes(x = max.p)) + 
    stat_smooth(aes(y = time), alpha = 0.5, level = 0.95, colour = 'blue') 
p3 <- ggplot(RES, aes(x = Um.ph)) + 
    stat_smooth(aes(y = time), alpha = 0.5, level = 0.95, colour = 'blue')
p4 <- ggplot(RES, aes(x = Um.hp)) + 
    stat_smooth(aes(y = time), alpha = 0.5, level = 0.95, colour = 'blue')
p5 <- ggplot(RES, aes(x = Ump.ph)) + 
    stat_smooth(aes(y = time), alpha = 0.5, level = 0.95, colour = 'blue')
p6 <- ggplot(RES, aes(x = Ump.hp)) + 
    stat_smooth(aes(y = time), alpha = 0.5, level = 0.95, colour = 'blue')
p7 <- ggplot(RES, aes(x = Kmp.h)) + 
    stat_smooth(aes(y = time), alpha = 0.5, level = 0.95, colour = 'blue')
p8 <- ggplot(RES, aes(x = Kmp.p)) + 
    stat_smooth(aes(y = time), alpha = 0.5, level = 0.95, colour = 'blue')

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol=2)

# only coevolution (time difference)
RES.aux <- RES[which(RES$evol.h != 0 & RES$evol.p != 0),] 

p1 <- ggplot(RES.aux, aes(x = max.h)) + 
    stat_smooth(aes(y = evol.h - evol.p), alpha = 0.5, level = 0.95, colour = 'blue') 
p2 <- ggplot(RES.aux, aes(x = max.p)) + 
    stat_smooth(aes(y = evol.h - evol.p), alpha = 0.5, level = 0.95, colour = 'blue') 
p3 <- ggplot(RES.aux, aes(x = Um.ph)) + 
    stat_smooth(aes(y = evol.h - evol.p), alpha = 0.5, level = 0.95, colour = 'blue')
p4 <- ggplot(RES.aux, aes(x = Um.hp)) + 
    stat_smooth(aes(y = evol.h - evol.p), alpha = 0.5, level = 0.95, colour = 'blue')
p5 <- ggplot(RES.aux, aes(x = Ump.ph)) + 
    stat_smooth(aes(y = evol.h - evol.p), alpha = 0.5, level = 0.95, colour = 'blue')
p6 <- ggplot(RES.aux, aes(x = Ump.hp)) + 
    stat_smooth(aes(y = evol.h - evol.p), alpha = 0.5, level = 0.95, colour = 'blue')
p7 <- ggplot(RES.aux, aes(x = Kmp.h)) + 
    stat_smooth(aes(y = evol.h - evol.p), alpha = 0.5, level = 0.95, colour = 'blue')
p8 <- ggplot(RES.aux, aes(x = Kmp.p)) + 
    stat_smooth(aes(y = evol.h - evol.p), alpha = 0.5, level = 0.95, colour = 'blue') 

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol=2)



