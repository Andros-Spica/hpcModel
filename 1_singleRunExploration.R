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

## Fast coevolution (default)

run.coevo.coeta.name <- "coevo.coeta"

run.coevo.coeta <- hpcModel.run(
  # initial populations
  iniH = 10,
  iniP = 10,
  # number of discrete types
  n.H = 30,         
  n.P = 30,        
  # undirected variation 
  v.H = 0.15,
  v.P = 0.15,
  # intrinsic growth rate 
  r.H = 0.04, 
  r.P = 0.1, 
  # Utility per capita of individuals of type N
  mU.PnH = 1.5,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = 0.15,                                  
  mU.H1P = 0,                                   
  # basic resources:
  # population of type N that can be sustained by resources independent of HP relationship
  U.bHn = 10,                                
  U.bPn = 20, 
  # population of type 1 that can be sustained by resources independent of HP relationship
  U.bH1 = 80,                               
  U.bP1 = 100,                                 
  # maximum local area to be used by populations (multiplier or scaling effect)
  MaxArea = 200,
  # settings 
  # simulation flow & data
  maxIt = 20000,
  tol = 6,
  saveTrajectories = TRUE,
  messages = TRUE, 
  # plotting
  plot.preview = FALSE, 
  plot.save = FALSE
)

# Plotting the *end state*, i.e. both populations become stationary:
  
hpcModel.plot(run.coevo.coeta, scale.override = 1)

# Plotting population trajectories with *ggplot*: 
  
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
dev.off()

# Animated GIF showing the *sequence of states* throughout the simulation:
  
run2 <- hpcModel.run(
  # initial populations
  iniH = 10,
  iniP = 10,
  # number of discrete types
  n.H = 30,         
  n.P = 30,        
  # undirected variation 
  v.H = 0.15,
  v.P = 0.15,
  # intrinsic growth rate 
  r.H = 0.04, 
  r.P = 0.1, 
  # Utility per capita of individuals of type N
  mU.PnH = 1.5,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = 0.15,                                  
  mU.H1P = 0,                                   
  # basic resources:
  # population of type N that can be sustained by resources independent of HP relationship
  U.bHn = 10,                                
  U.bPn = 20, 
  # population of type 1 that can be sustained by resources independent of HP relationship
  U.bH1 = 80,                               
  U.bP1 = 100,                                 
  # maximum local area to be used by populations (multiplier or scaling effect)
  MaxArea = 200, 
  # settings 
  # simulation flow & data
  maxIt = 20000,
  tol = 6,
  saveTrajectories = TRUE,
  messages = TRUE,  
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
  # initial populations
  iniH = 10,
  iniP = 10,
  # number of discrete types
  n.H = 30,         
  n.P = 30,        
  # undirected variation 
  v.H = 0.15,
  v.P = 0.15,
  # intrinsic growth rate 
  r.H = 0.04, 
  r.P = 0.1, 
  # Utility per capita of individuals of type N
  mU.PnH = 1.5,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = 0,                                  
  mU.H1P = 0,                                   
  # basic resources:
  # population of type N that can be sustained by resources independent of HP relationship
  U.bHn = 10,                                
  U.bPn = 20, 
  # population of type 1 that can be sustained by resources independent of HP relationship
  U.bH1 = 80,                               
  U.bP1 = 100,                                 
  # maximum local area to be used by populations (multiplier or scaling effect)
  MaxArea = 200,
  # settings 
  # simulation flow & data
  maxIt = 20000,
  tol = 6,
  saveTrajectories = TRUE,
  messages = TRUE, 
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

## Coevolution with early cultivation

run.coevo.early.cult.name <- "coevo.early.cult"

run.coevo.early.cult <- hpcModel.run(
  # initial populations
  iniH = 10,
  iniP = 10,
  # number of discrete types
  n.H = 30,         
  n.P = 30,        
  # undirected variation 
  v.H = 0.15,
  v.P = 0.15,
  # intrinsic growth rate 
  r.H = 0.04, 
  r.P = 0.1, 
  # Utility per capita of individuals of type N
  mU.PnH = 2,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = 1,                                  
  mU.H1P = 0,                                   
  # basic resources:
  # population of type N that can be sustained by resources independent of HP relationship
  U.bHn = 10,                                
  U.bPn = 20, 
  # population of type 1 that can be sustained by resources independent of HP relationship
  U.bH1 = 80,                               
  U.bP1 = 100,                                 
  # maximum local area to be used by populations (multiplier or scaling effect)
  MaxArea = 200,
  # settings 
  # simulation flow & data
  maxIt = 20000,
  tol = 6,
  saveTrajectories = TRUE,
  messages = TRUE, 
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

## Coevolution with early domestication

run.coevo.early.dom.name <- "coevo.early.dom"

run.coevo.early.dom <- hpcModel.run(
  # initial populations
  iniH = 10,
  iniP = 10,
  # number of discrete types
  n.H = 30,         
  n.P = 30,        
  # undirected variation 
  v.H = 0.15,
  v.P = 0.15,
  # intrinsic growth rate 
  r.H = 0.04, 
  r.P = 0.1, 
  # Utility per capita of individuals of type N
  mU.PnH = 1,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = 1,                                  
  mU.H1P = 0,                                   
  # basic resources:
  # population of type N that can be sustained by resources independent of HP relationship
  U.bHn = 10,                                
  U.bPn = 20, 
  # population of type 1 that can be sustained by resources independent of HP relationship
  U.bH1 = 80,                               
  U.bP1 = 20,                                 
  # maximum local area to be used by populations (multiplier or scaling effect)
  MaxArea = 200,
  # settings 
  # simulation flow & data
  maxIt = 20000,
  tol = 6,
  saveTrajectories = TRUE,
  messages = TRUE, 
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

## Cultivation without domestication

run.cult.without.dom.name <- "cult.without.dom"

run.cult.without.dom <- hpcModel.run(
  # initial populations
  iniH = 10,
  iniP = 10,
  # number of discrete types
  n.H = 30,         
  n.P = 30,        
  # undirected variation 
  v.H = 0.15,
  v.P = 0.15,
  # intrinsic growth rate 
  r.H = 0.04, 
  r.P = 0.1, 
  # Utility per capita of individuals of type N
  mU.PnH = 2.5,
  mU.HnP = 0.45,
  # Utility per capita of individuals of type 1
  mU.P1H = 0.15,                                  
  mU.H1P = 0,                                   
  # basic resources:
  # population of type N that can be sustained by resources independent of HP relationship
  U.bHn = 10,                                
  U.bPn = 20, 
  # population of type 1 that can be sustained by resources independent of HP relationship
  U.bH1 = 80,                               
  U.bP1 = 100,                                 
  # maximum local area to be used by populations (multiplier or scaling effect)
  MaxArea = 200,
  # settings 
  # simulation flow & data
  maxIt = 20000,
  tol = 6,
  saveTrajectories = TRUE,
  messages = TRUE, 
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

## Coevolution with population "bleep"

run.coevo.bleep.name <- "coevo.bleep"

run.coevo.bleep <- hpcModel.run(
  # initial populations
  iniH = 10,
  iniP = 10,
  # number of discrete types
  n.H = 30,         
  n.P = 30,        
  # undirected variation 
  v.H = 0.15,
  v.P = 0.15,
  # intrinsic growth rate 
  r.H = 0.04, 
  r.P = 0.1, 
  # Utility per capita of individuals of type N
  mU.PnH = 1.5,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = 0.15,                                  
  mU.H1P = 0,                                   
  # basic resources:
  # population of type N that can be sustained by resources independent of HP relationship
  U.bHn = 10,                                
  U.bPn = 20, 
  # population of type 1 that can be sustained by resources independent of HP relationship
  U.bH1 = 80,                               
  U.bP1 = 100,                                 
  # maximum local area to be used by populations (multiplier or scaling effect)
  MaxArea = 110,
  # settings 
  # simulation flow & data
  maxIt = 20000,
  tol = 6,
  saveTrajectories = TRUE,
  messages = TRUE, 
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

## Coevolution with population "boom"

run.coevo.boom.name <- "coevo.boom"

run.coevo.boom <- hpcModel.run(
  # initial populations
  iniH = 10,
  iniP = 10,
  # number of discrete types
  n.H = 30,         
  n.P = 30,        
  # undirected variation 
  v.H = 0.15,
  v.P = 0.15,
  # intrinsic growth rate 
  r.H = 0.04, 
  r.P = 0.1, 
  # Utility per capita of individuals of type N
  mU.PnH = 2,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = 0.15,                                  
  mU.H1P = 0,                                   
  # basic resources:
  # population of type N that can be sustained by resources independent of HP relationship
  U.bHn = 10,                                
  U.bPn = 20, 
  # population of type 1 that can be sustained by resources independent of HP relationship
  U.bH1 = 80,                               
  U.bP1 = 100,                                 
  # maximum local area to be used by populations (multiplier or scaling effect)
  MaxArea = 500,
  # settings 
  # simulation flow & data
  maxIt = 20000,
  tol = 6,
  saveTrajectories = TRUE,
  messages = TRUE, 
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

## Coevolution with long population "boom"

run.coevo.long.boom.name <- "coevo.long.boom"

run.coevo.long.boom <- hpcModel.run(
  # initial populations
  iniH = 10,
  iniP = 10,
  # number of discrete types
  n.H = 30,         
  n.P = 30,        
  # undirected variation 
  v.H = 0.15,
  v.P = 0.15,
  # intrinsic growth rate 
  r.H = 0.04, 
  r.P = 0.1, 
  # Utility per capita of individuals of type N
  mU.PnH = 1,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = 1,                                  
  mU.H1P = 0,                                   
  # basic resources:
  # population of type N that can be sustained by resources independent of HP relationship
  U.bHn = 10,                                
  U.bPn = 20, 
  # population of type 1 that can be sustained by resources independent of HP relationship
  U.bH1 = 80,                               
  U.bP1 = 100,                                 
  # maximum local area to be used by populations (multiplier or scaling effect)
  MaxArea = 1000,
  # settings 
  # simulation flow & data
  maxIt = 2000, # this is a dangerously long simulation!!!
  tol = 6,
  saveTrajectories = TRUE,
  messages = TRUE, 
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
