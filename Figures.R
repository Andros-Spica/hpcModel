
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
require(randomForest)

## custom plot functions
##----------------------

source("library/plotting/varImpPlot2.R")
source("library/plotting/trajectories.ggplot.R")
source("library/plotting/fourPar.ggplot.R")

## Select output type (png or eps)
## ----------------------
outputType = "png"
#outputType = "eps"

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
# Figure - Trajectories
# -----------------------

load('data/5_exp.LHS.trajectories.RData')

plotName = "Fig3"

if (outputType == "png")
{
  plotScale = 10
  png(paste0("plots/figures/", plotName, ".png"),
      width = 100 * plotScale, height = 50 * plotScale)
}
if (outputType == "eps")
{
  plotScale = 8
  extrafont::loadfonts(device = "postscript")
  grDevices::postscript(file = paste0("plots/figures/", plotName, ".eps"),
                        pointsize = 10,
                        width = 10,
                        height = 10,
                        horizontal = FALSE,
                        paper = "special",
                        onefile = FALSE,
                        family = "sans",
                        colormodel = "cmyk")
}

trajectories.ggplot(exp.LHS.trajectories, 
                    c('H', 'P'), c('coevo.H', 'coevo.P'),
                    'Populations', 'Coevolution coefficients',
                    cex = plotScale)

dev.off()

#########################
# Figure - Run scenario: Coevolution occurs
# -----------------------

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

plotName = "Fig5"

if (outputType == "png")
{
  png(paste0("plots/figures/", plotName, ".png"),
      width = 1000, height = 1000)
}
if (outputType == "eps")
{
  extrafont::loadfonts(device = "postscript")
  grDevices::postscript(file = paste0("plots/figures/", plotName, ".eps"),
                        pointsize = 10,
                        width = 10,
                        height = 10,
                        horizontal = FALSE,
                        paper = "special",
                        onefile = FALSE,
                        family = "sans",
                        colormodel = "cmyk")
}

hpcModel.plot(run.coevo.coeta)
dev.off()

#########################
# Figure - Run scenario: Coevolution do not occur
# -----------------------

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
  plot.save = FALSE
)

plotName = "Fig4"

if (outputType == "png")
{
  png(paste0("plots/figures/", plotName, ".png"),
      width = 1000, height = 1000)
}
if (outputType == "eps")
{
  extrafont::loadfonts(device = "postscript")
  grDevices::postscript(file = paste0("plots/figures/", plotName, ".eps"),
                        pointsize = 10,
                        width = 10,
                        height = 10,
                        horizontal = FALSE,
                        paper = "special",
                        onefile = FALSE,
                        family = "sans",
                        colormodel = "cmyk")
}

hpcModel.plot(run.no.coevo)
dev.off()

#########################
# Figure - Run scenario: Coevolution occurs partially
# -----------------------

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
  plot.save = FALSE
)

plotName = "Fig-partialCoevo"

if (outputType == "png")
{
  png(paste0("plots/figures/", plotName, ".png"),
      width = 1000, height = 1000)
}
if (outputType == "eps")
{
  extrafont::loadfonts(device = "postscript")
  grDevices::postscript(file = paste0("plots/figures/", plotName, ".eps"),
                        pointsize = 10,
                        width = 10,
                        height = 10,
                        horizontal = FALSE,
                        paper = "special",
                        onefile = FALSE,
                        family = "sans",
                        colormodel = "cmyk")
}

hpcModel.plot(run.semicoevo)
dev.off()

#########################

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
  maxIt = 600, # limit iterations
  tol = tol.default,
  timing.threshold = timing.threshold.default,
  saveTrajectories = saveTrajectories.default,
  messages = messages.default, 
  # plotting
  plot.preview = FALSE, 
  plot.save = FALSE
)

plotName = "Fig6"#"Fig-partialCoevo-osc"

if (outputType == "png")
{
  png(paste0("plots/figures/", plotName, ".png"),
      width = 1000, height = 1000)
}
if (outputType == "eps")
{
  extrafont::loadfonts(device = "postscript")
  grDevices::postscript(file = paste0("plots/figures/", plotName, ".eps"),
                        pointsize = 10,
                        width = 10,
                        height = 10,
                        horizontal = FALSE,
                        paper = "special",
                        onefile = FALSE,
                        family = "sans",
                        colormodel = "cmyk")
}

hpcModel.plot(run.semicoevo.osc2)
dev.off()

#########################
# Figure - importance plot (random forest)
# -----------------------

# load
load('data/5_RF.coevo.H.RData')
load('data/5_RF.coevo.P.RData')

plotName = "Fig7"

if (outputType == "png")
{
  plotScale = 4
  png(paste0("plots/figures/", plotName, ".png"),
      width = 350 * plotScale, height = 350 * plotScale)
}
if (outputType == "eps")
{
  plotScale = 1.2
  extrafont::loadfonts(device = "postscript")
  grDevices::postscript(file = paste0("plots/figures/", plotName, ".eps"),
                        pointsize = 10,
                        width = 7 * plotScale,
                        height = 7 * plotScale,
                        horizontal = FALSE,
                        paper = "special",
                        onefile = FALSE,
                        family = "sans",
                        colormodel = "cmyk")
}

varImpPlot2Pair(RF.coevo.H, RF.coevo.P, cex = plotScale)
dev.off()

#########################
# Figure - mutual utility
# -----------------------

# load
load('data/4_exp.U.bH_U.PH.RData')

plotName = "Fig8"

if (outputType == "png")
{
  plotScale = 10
  png(paste0("plots/figures/", plotName, ".png"),
      width = 100 * plotScale, height = 100 * plotScale)
}
if (outputType == "eps")
{
  plotScale = 7
  extrafont::loadfonts(device = "postscript")
  grDevices::postscript(file = paste0("plots/figures/", plotName, ".eps"),
                        pointsize = 10,
                        width = 10,
                        height = 10,
                        horizontal = FALSE,
                        paper = "special",
                        onefile = FALSE,
                        family = "sans",
                        colormodel = "cmyk")
}

fourPar.ggplot(exp.U.bH_U.PH, 'U.bH1', 'U.bHn', 'mU.P1H', 'mU.PnH', 'coevo.H', 'coevo.P', 
               xlab = expression(U['bH1']),
               ylab = expression(U['bHn']),
               var1lab = expression('coevo'[H]),
               var2lab = expression('coevo'[P]),
               plotScale = plotScale)
dev.off()

#########################
# Figure - n x v
# -----------------------

# load
load('data/4_exp.n_v.RData')

plotName = "Fig-nxv"

if (outputType == "png")
{
  plotScale = 10
  png(paste0("plots/figures/", plotName, ".png"),
      width = 100 * plotScale, height = 100 * plotScale)
}
if (outputType == "eps")
{
  plotScale = 6
  extrafont::loadfonts(device = "postscript")
  grDevices::postscript(file = paste0("plots/figures/", plotName, ".eps"),
                        pointsize = 10,
                        width = 10,
                        height = 10,
                        horizontal = FALSE,
                        paper = "special",
                        onefile = FALSE,
                        family = "sans",
                        colormodel = "cmyk")
}

fourPar.ggplot(exp.n_v, 'v.H', 'v.P', 'n.H', 'n.P', 'coevo.H', 'coevo.P', 
               xlab = expression(v['H']),
               ylab = expression(v['P']),
               var1lab = expression('coevo'[H]),
               var2lab = expression('coevo'[P]),
               plotScale = plotScale)
dev.off()
