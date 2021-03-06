# Single runs

source("library/hpcModel.run.R")
source("library/hpcModel.exploration.R")
source("library/hpcModel.plot.R")
# for ggplot:
require(reshape2)
require(ggplot2)
require(scales)
# for creating animates GIF
require(purrr)
require(magick)

runPlot.dir <- "plots/runPlot/" # directory holding animation files

plot.trajectory <- function(meltedRunData, plotScale = 1)
{
  ggplot(meltedRunData,
         aes(x = time, y = value, colour = variable), size = 2 * plotScale) +
    geom_line(size = 0.2 * plotScale) +
    ylab("population") + 
    scale_color_manual(labels = c('Humans', ' Plants'), values = c('blue', 'red')) +
    
    theme(axis.title = element_text(size = 3 * plotScale),
          axis.text = element_text(size = 2 * plotScale),
          legend.title = element_blank(),
          legend.text = element_text(size = 3 * plotScale),
          legend.key.size = unit(0.2 * plotScale, 'lines'))
}

animate.trajectory <- function(filePath, fileName, delete.frames = TRUE)
{
  # files <- dir(filePath)
  # for (i in 1:length(files))
  # {
  #   files[i] <- paste0(filePath, files[i])
  # }
  files <- paste0(filePath, dir(path = filePath, pattern = paste0("^", fileName, ".*\\.png$")))
  images <- map(files, image_read)
  images <- image_join(images)
  animation <- image_animate(images, fps = 10)
  image_write(animation, paste0(filePath, fileName, "-anim.gif"))
  if (delete.frames) { unlink(files); }
}

# this is the 'default' parameter setting (end state is 'fast coevolution'):
# initial populations
iniH.default = 10
iniP.default = 10
# number of discrete types
n.H.default = 30
n.P.default = 30    
# undirected variation 
v.H.default = 0.15
v.P.default = 0.15
# intrinsic growth rate 
r.H.default = 0.04
r.P.default = 0.1
# Utility per capita of individuals of type N
mU.PnH.default = 1.5
mU.HnP.default = 1
# Utility per capita of individuals of type 1
mU.P1H.default = 0.15                           
mU.H1P.default = 0                               
# basic resources:
# population of type N that can be sustained by resources independent of HP relationship
U.bHn.default = 10                               
U.bPn.default = 20
# population of type 1 that can be sustained by resources independent of HP relationship
U.bH1.default = 80                               
U.bP1.default = 100                                 
# maximum local area to be used by populations (multiplier or scaling effect)
MaxArea.default = 200
# settings 
# simulation flow & data
maxIt.default = 5000
tol.default = 6
timing.threshold.default = 0.5
saveTrajectories.default = TRUE
messages.default = TRUE

## Fast coevolution (default)

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

run.coevo.coeta$END

# Plotting the end state, i.e. both populations become stationary'}

hpcModel.plot(run.coevo.coeta, scale.override = 1)

run.coevo.coeta.melted <-
  data.frame(H = run.coevo.coeta$TRAJECTORIES$H, 
             P = run.coevo.coeta$TRAJECTORIES$P)[!is.na(run.coevo.coeta$TRAJECTORIES$H), ]

run.coevo.coeta.melted$time <- 1:nrow(run.coevo.coeta.melted)

run.coevo.coeta.melted <- melt(run.coevo.coeta.melted, measure.vars = c('H', 'P'))

plotScale = 10
#svg('plots/1_singleRun-ggplot.svg', width=plotScale, height=plotScale)
png(paste0("plots/1_singleRun-ggplot-", run.coevo.coeta.name, ".png"), 
    width = 200 * plotScale, height = 100 * plotScale)
plot.trajectory(run.coevo.coeta.melted, plotScale = plotScale)
invisible(dev.off())

# run saving plot frames for animation

run.coevo.coeta2 <- hpcModel.run(
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
  plot.sleep = 0.05,
  plot.save = TRUE,
  plot.saveEvery = 5,
  plot.directory = runPlot.dir,
  plot.fileName = paste0(run.coevo.coeta.name, "-runPlot")
)

# Generate an animated GIF using the images in plot.directory folder
animate.trajectory(runPlot.dir, run.coevo.coeta.name)

## No coevolution

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

# Generate an animated GIF using the images in plot.directory folder
animate.trajectory(runPlot.dir, run.no.coevo.name)

hpcModel.plot(run.no.coevo, scale.override = 1)

## Coevolution with early cultivation

run.coevo.early.cult.name <- "coevo.early.cult"

run.coevo.early.cult <- hpcModel.run(
  iniH = iniH.default,
  iniP = iniP.default,
  n.H = n.H.default,         
  n.P = n.P.default,        
  v.H = v.H.default,
  v.P = v.P.default,
  r.H = r.H.default, 
  r.P = r.P.default, 
  mU.PnH = 2, # utility type n plant is higher than default
  mU.HnP = mU.HnP.default,
  mU.P1H = 1, # lutility type 1 plant is higher than default
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
  plot.fileName = paste0(run.coevo.early.cult.name, "-runPlot")
)

# Generate an animated GIF using the images in plot.directory folder
animate.trajectory(runPlot.dir, run.coevo.early.cult.name)

hpcModel.plot(run.coevo.early.cult, scale.override = 1)

## Coevolution with early domestication

run.coevo.early.dom.name <- "coevo.early.dom"

run.coevo.early.dom <- hpcModel.run(
  iniH = iniH.default,
  iniP = iniP.default,
  n.H = n.H.default,         
  n.P = n.P.default,        
  v.H = v.H.default,
  v.P = v.P.default,
  r.H = r.H.default, 
  r.P = r.P.default, 
  mU.PnH = 1, # utility type n plant is the same than type 1 plants
  mU.HnP = mU.HnP.default,
  mU.P1H = 1, # utility type 1 plant is the same than type n plants
  mU.H1P = mU.H1P.default,                                   
  U.bHn = U.bHn.default,                                
  U.bPn = U.bPn.default, 
  U.bH1 = U.bH1.default,                               
  U.bP1 = 20, # baseline carrying capacity of type 1 plant is the same than type n plants
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
  plot.fileName = paste0(run.coevo.early.dom.name, "-runPlot")
)

# Generate an animated GIF using the images in plot.directory folder
animate.trajectory(runPlot.dir, run.coevo.early.dom.name)

hpcModel.plot(run.coevo.early.dom, scale.override = 1)

## Cultivation without domestication

run.cult.without.dom.name <- "cult.without.dom"

run.cult.without.dom <- hpcModel.run(
  iniH = iniH.default,
  iniP = iniP.default,
  n.H = n.H.default,         
  n.P = n.P.default,        
  v.H = v.H.default,
  v.P = v.P.default,
  r.H = r.H.default, 
  r.P = r.P.default, 
  mU.PnH = 2.5, # utility type n plant is higher than default
  mU.HnP = 0.45, # utility type n humans is lower than default
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
  plot.sleep = 0.05,
  plot.save = TRUE,
  plot.saveEvery = 5,
  plot.directory = runPlot.dir,
  plot.fileName = paste0(run.cult.without.dom.name, "-runPlot")
)

# Generate an animated GIF using the images in plot.directory folder
animate.trajectory(runPlot.dir, run.cult.without.dom.name)

hpcModel.plot(run.cult.without.dom, scale.override = 1)

## Coevolution with population "bleep"

run.coevo.bleep.name <- "coevo.bleep"

run.coevo.bleep <- hpcModel.run(
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
  MaxArea = 110, # MaxArea is lower than default
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
  plot.fileName = paste0(run.coevo.bleep.name, "-runPlot")
)

# Generate an animated GIF using the images in plot.directory folder
animate.trajectory(runPlot.dir, run.coevo.bleep.name)

hpcModel.plot(run.coevo.bleep, scale.override = 1)

## Coevolution with population "boom"

run.coevo.boom.name <- "coevo.boom"

run.coevo.boom <- hpcModel.run(
  iniH = iniH.default,
  iniP = iniP.default,
  n.H = n.H.default,         
  n.P = n.P.default,        
  v.H = v.H.default,
  v.P = v.P.default,
  r.H = r.H.default, 
  r.P = r.P.default, 
  mU.PnH = 2, # utility type n plant is higher than default
  mU.HnP = mU.HnP.default,
  mU.P1H = mU.P1H.default,                                  
  mU.H1P = mU.H1P.default,                                   
  U.bHn = U.bHn.default,                                
  U.bPn = U.bPn.default, 
  U.bH1 = U.bH1.default,                               
  U.bP1 = U.bP1.default,                                 
  MaxArea = 500, # MaxArea is higher than default
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
  plot.fileName = paste0(run.coevo.boom.name, "-runPlot")
)

# Generate an animated GIF using the images in plot.directory folder
animate.trajectory(runPlot.dir, run.coevo.boom.name)

hpcModel.plot(run.coevo.boom, scale.override = 1)

## Coevolution with long population "boom"

run.coevo.long.boom.name <- "coevo.long.boom"

run.coevo.long.boom <- hpcModel.run(
  iniH = iniH.default,
  iniP = iniP.default,
  n.H = n.H.default,         
  n.P = n.P.default,        
  v.H = v.H.default,
  v.P = v.P.default,
  r.H = r.H.default, 
  r.P = r.P.default, 
  mU.PnH = 1, # utility type n plant is the same than type 1 plants
  mU.HnP = mU.HnP.default,
  mU.P1H = 1, # utility type 1 plant is the same than type n plants
  mU.H1P = mU.H1P.default,                                   
  U.bHn = U.bHn.default,                                
  U.bPn = U.bPn.default, 
  U.bH1 = U.bH1.default,                               
  U.bP1 = U.bP1.default,                                 
  MaxArea = 1000, # MaxArea is much higher than default
  maxIt = 2000, # this is a dangerously long simulation!!!
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
  plot.fileName = paste0(run.coevo.long.boom.name, "-runPlot")
)

# Generate an animated GIF using the images in plot.directory folder
animate.trajectory(runPlot.dir, run.coevo.long.boom.name)

hpcModel.plot(run.coevo.long.boom, scale.override = 1)

## Semi-coevolution (stationary point)

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

# Generate an animated GIF using the images in plot.directory folder
animate.trajectory(runPlot.dir, run.semicoevo.name)

hpcModel.plot(run.semicoevo, scale.override = 1)

## Semi-coevolution (oscillations)

run.semicoevo.osc1.name <- "semicoevo.osc1"

run.semicoevo.osc1 <- hpcModel.run(
  iniH = iniH.default,
  iniP = iniP.default,
  n.H = n.H.default,#10,         
  n.P = n.P.default,#10,        
  v.H = v.H.default,
  v.P = v.P.default, 
  r.H = r.H.default,
  r.P = r.P.default, 
  mU.PnH = 0.5, # utility type n plant is the same than type 1 plants and lower than default
  mU.HnP = 0.9, # utility type n humans is lower than default
  mU.P1H = 0.5, # utility type 1 plant is the same than type n plants and higher than default
  mU.H1P = mU.H1P.default,                                   
  U.bHn = 20, # baseline carrying capacity for type n humans is higher than default
  U.bPn = U.bPn.default, 
  U.bH1 = 100, # baseline carrying capacity for type 1 humans is higher than default
  U.bP1 = U.bP1.default,                                 
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
  plot.fileName = paste0(run.semicoevo.osc1.name, "-runPlot")
)

# Generate an animated GIF using the images in plot.directory folder
animate.trajectory(runPlot.dir, run.semicoevo.osc1.name)

hpcModel.plot(run.semicoevo.osc1, scale.override = 1)

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

# Generate an animated GIF using the images in plot.directory folder
animate.trajectory(runPlot.dir, run.semicoevo.osc2.name)

hpcModel.plot(run.semicoevo.osc2, scale.override = 1)

