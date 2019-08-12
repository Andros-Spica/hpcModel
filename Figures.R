
# Figures

########################
# set up

## HPC model functions
##--------------------
source("library/hpcModel.run.R")
source("library/hpcModel.exploration.R")
source("library/hpcModel.plot.R")

## load packages
##-------------
require(ggplot2)
require(scales)
require(ggpubr)

## custom plot functions
##----------------------

source("library/plotting/varImpPlot2.R")
source("library/plotting/trajectories.ggplot.R")
source("library/plotting/fourPar.ggplot.R")

## this is the 'default' parameter setting (end state is 'fast coevolution')
## ----------------------
### initial populations
iniH.default = 10
iniP.default = 10
### number of discrete types
n.H.default = 30
n.P.default = 30    
### undirected variation 
v.H.default = 0.15
v.P.default = 0.15
### intrinsic growth rate 
r.H.default = 0.04
r.P.default = 0.1
### Utility per capita of individuals of type N
mU.PnH.default = 1.5
mU.HnP.default = 1
### Utility per capita of individuals of type 1
mU.P1H.default = 0.15                           
mU.H1P.default = 0                               
### basic resources:
### population of type N that can be sustained by resources independent of HP relationship
U.bHn.default = 10                               
U.bPn.default = 20
### population of type 1 that can be sustained by resources independent of HP relationship
U.bH1.default = 80                               
U.bP1.default = 100                                 
### maximum local area to be used by populations (multiplier or scaling effect)
MaxArea.default = 200
### settings 
### simulation flow & data
maxIt.default = 5000
tol.default = 6
timing.threshold.default = 0.5
saveTrajectories.default = TRUE
messages.default = TRUE

#==================================================================
#########################
# Figure 2 - Trajectories
# -----------------------

load('data/exp.LHS.trajectories.RData')

plotScale = 10

png("plots/figures/Fig1-trajectories.png",
    width = 100 * plotScale, height = 50 * plotScale)

trajectories.ggplot(exp.LHS.trajectories, 
                    c('H', 'P'), c('coevo.H', 'coevo.P'),
                    'Populations', 'Coevolution coefficients',
                    cex = plotScale)

dev.off()

#########################
# Figure 3 - Run scenario: Coevolution occurs
# -----------------------

run.coevo.coeta.name <- "coevo.coeta"

run.coevo.coeta <- hpcModel.run(
  iniH = iniH.default,
  iniP = iniP.default,
  n.H = n.H.default,         
  n.P = n.P.default,        
  v.H = v.H.default,
  v.P = v.P.default,
  r.H = r.H.default, 
  r.P = r.P.default, 
  mU.PnH = mU.PnH.default,
  mU.HnP = mU.HnP.default,
  mU.P1H = mU.P1H.default,                                  
  mU.H1P = mU.H1P.default,                                   
  U.bHn = U.bHn.default,                                
  U.bPn = U.bPn.default, 
  U.bH1 = U.bH1.default,                               
  U.bP1 = U.bP1.default,                                 
  MaxArea = MaxArea.default,
  maxIt = maxIt.default,
  tol = tol.default,
  timing.threshold = timing.threshold.default,
  saveTrajectories = saveTrajectories.default,
  messages = messages.default, 
  # plotting
  plot.preview = FALSE, 
  plot.save = FALSE
)

png(paste0("plots/figures/Fig2-", run.coevo.coeta.name, ".png"))
hpcModel.plot(run.coevo.coeta)
dev.off()

#########################
# Figure 4 - Run scenario: Coevolution do not occur
# -----------------------

run.no.coevo.name <- "no.coevo"

run.no.coevo <- hpcModel.run(
  iniH = iniH.default,
  iniP = iniP.default,
  n.H = n.H.default,         
  n.P = n.P.default,        
  v.H = v.H.default,
  v.P = v.P.default,
  r.H = r.H.default, 
  r.P = r.P.default, 
  mU.PnH = mU.PnH.default,
  mU.HnP = mU.HnP.default,
  mU.P1H = 0, # utility of type 1 plants has utility zero                      
  mU.H1P = mU.H1P.default,                                   
  U.bHn = U.bHn.default,                                
  U.bPn = U.bPn.default, 
  U.bH1 = U.bH1.default,                               
  U.bP1 = U.bP1.default,                                 
  MaxArea = MaxArea.default,
  maxIt = maxIt.default,
  tol = tol.default,
  timing.threshold = timing.threshold.default,
  saveTrajectories = saveTrajectories.default,
  messages = messages.default, 
  # plotting
  plot.preview = FALSE, 
  plot.sleep = 0.05,
  plot.save = TRUE,
  plot.saveEvery = 5,
  plot.directory = runPlot.dir,
  plot.fileName = paste0(run.no.coevo.name, "-runPlot")
)

png(paste0("plots/figures/Fig3-", run.no.coevo.name, ".png"))
hpcModel.plot(run.no.coevo)
dev.off()

#########################
# Figure 5 - Run scenario: Coevolution occurs partially
# -----------------------

run.semicoevo.name <- "semicoevo"

run.semicoevo <- hpcModel.run(
  iniH = iniH.default,
  iniP = iniP.default,
  n.H = n.H.default,         
  n.P = n.P.default,        
  v.H = v.H.default,
  v.P = v.P.default,
  r.H = r.H.default, 
  r.P = r.P.default, 
  mU.PnH = 0.5, # utility type n plant is the same than type 1 plants and lower than default
  mU.HnP = mU.HnP.default,
  mU.P1H = 0.5, # utility type 1 plant is the same than type n plants and higher than default
  mU.H1P = mU.H1P.default,                                   
  U.bHn = U.bHn.default,                                
  U.bPn = U.bPn.default, 
  U.bH1 = U.bH1.default,                               
  U.bP1 = U.bP1.default,                                 
  MaxArea = MaxArea.default,
  maxIt = maxIt.default,
  tol = tol.default,
  timing.threshold = timing.threshold.default,
  saveTrajectories = saveTrajectories.default,
  messages = messages.default, 
  # plotting
  plot.preview = FALSE, 
  plot.sleep = 0.05,
  plot.save = TRUE,
  plot.saveEvery = 5,
  plot.directory = runPlot.dir,
  plot.fileName = paste0(run.semicoevo.name, "-runPlot")
)

png(paste0("plots/figures/Fig4-", run.semicoevo.name, ".png"))
hpcModel.plot(run.semicoevo)
dev.off()

#########################
# Figure 6
# -----------------------

run.semicoevo.osc2.name <- "semicoevo.osc2"

run.semicoevo.osc2 <- hpcModel.run(
  iniH = iniH.default,
  iniP = iniP.default,
  n.H = 10,         
  n.P = 10,        
  v.H = v.H.default,
  v.P = v.P.default, 
  r.H = 0.15,
  r.P = 0.15, 
  mU.PnH = 1,
  mU.HnP = 0.5, # utility type n humans is the same than type 1
  mU.P1H = 0.2,
  mU.H1P = 0.5,
  U.bHn = 10,
  U.bPn = 10, 
  U.bH1 = 100,
  U.bP1 = 100,
  MaxArea = MaxArea.default, 
  maxIt = 2000, # limit iterations
  tol = tol.default,
  timing.threshold = timing.threshold.default,
  saveTrajectories = saveTrajectories.default,
  messages = messages.default, 
  # plotting
  plot.preview = FALSE, 
  plot.sleep = 0.05,
  plot.save = TRUE,
  plot.saveEvery = 5,
  plot.directory = runPlot.dir,
  plot.fileName = paste0(run.semicoevo.osc2.name, "-runPlot")
)

png(paste0("plots/figures/Fig5-", run.semicoevo.osc2.name, ".png"))
hpcModel.plot(run.semicoevo.osc2)
dev.off()

#########################
# Figure 7
# -----------------------

# load
load('data/RF.coevo.H.RData')
load('data/RF.coevo.P.RData')

png("plots/figures/Fig6-RF-coevo.png", width = 640 * plotScale, height = 360 * plotScale)
varImpPlot2Pair(RF.coevo.H, RF.coevo.P, cex = plotScale)
dev.off()

#########################
# Figure 8
# -----------------------

# load
load('data/exp.n_v.RData')

plotScale = 10

png(paste0("plots/figures/Fig7-nxv.png"),
    width = 100 * plotScale, height = 100 * plotScale)
fourPar.ggplot(exp.n_v, 'v.H', 'v.P', 'n.H', 'n.P', 'coevo.H', 'coevo.P', 
               xlab = expression(v['H']),
               ylab = expression(v['P']),
               var1lab = expression('coevo'[H]),
               var2lab = expression('coevo'[P]),
               plotScale = plotScale)
dev.off()

#########################
# Figure 9
# -----------------------

# load
load('data/exp.U.bH_U.PH.RData')

plotScale = 10

png(paste0("plots/figures/Fig7-K.H.png"),
    width = 100 * plotScale, height = 100 * plotScale)
fourPar.ggplot(exp.U.bH_U.PH, 'U.bH1', 'U.bHn', 'mU.P1H', 'mU.PnH', 'coevo.H', 'coevo.P', 
               xlab = expression(U['bH1']),
               ylab = expression(U['bHn']),
               var1lab = expression('coevo'[H]),
               var2lab = expression('coevo'[P]),
               plotScale = plotScale)
dev.off()
