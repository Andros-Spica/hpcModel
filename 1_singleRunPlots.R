
source("library/hpcModel.run.R")
source("library/hpcModel.exploration.R")
source("library/hpcModel.plot.R")
require(reshape2)
require(ggplot2)
require(scales)

########################################################################################
### single run

run1 <- hpcModel.run(
  # initial populations
  iniH = 10,
  iniP = 10,
  # number of discrete types
  n.H = 10,         
  n.P = 10,        
  # undirected variation 
  v.H = 0.15,
  v.P = 0.15,
  # intrinsic growth rate 
  r.H = 0.05, 
  r.P = 0.1, 
  # proportion of mean utility per capita:
  # Utility of individuals of type N
  mU.PnH = 1.7,
  mU.HnP = 1,
  # Utility of individuals of type 1
  mU.P1H = 0.17,                                  
  mU.H1P = 0.01,                                   
  # basic resources:
  # population of type N that can be sustained by resources independent of HP relationship
  mU.bHn = 0.08,                                
  mU.bPn = 1, 
  # population of type 1 that can be sustained by resources independent of HP relationship
  mU.bH1 = 80,                               
  mU.bP1 = 100,                                
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

#svg('plots/1_singleRun.svg', width=10, height=10)
png("plots/1_singleRun.png", width = 1000, height = 1000)
hpcModel.plot(run1)
dev.off()

run1$END

# trajectories in ggplot

g.data <-
  data.frame(H = run1$TRAJECTORIES$H, 
             P = run1$TRAJECTORIES$P)[!is.na(run1$TRAJECTORIES$H), ]

g.data$time <- 1:nrow(g.data)

g.data <- melt(g.data, measure.vars = c('H', 'P'))

plotScale = 100

g1 <- 
  ggplot(g.data,
         aes(x = time, y = value, colour = variable), size = 2 * plotScale) +
  geom_line(size = 0.2 * plotScale) +
  ylab("population") + 
  scale_color_manual(labels = c('Humans', ' Plants'), values = c('blue', 'red')) +
  
  theme(axis.title = element_text(size = 3 * plotScale),
        axis.text = element_text(size = 2 * plotScale),
        legend.title = element_blank(),
        legend.text = element_text(size = 3 * plotScale),
        legend.key.size = unit(0.2 * plotScale, 'lines'))

#svg('plots/1_singleRun-ggplot.svg', width=10, height=10)
png("plots/1_singleRun-ggplot.png", width = 100 * plotScale, height = 100 * plotScale)
g1
dev.off()


########################################################################################
### single run (generate images every step for animation) 

run2 <- hpcModel.run(
  # initial populations
  iniH = 10,
  iniP = 10,
  # number of discrete types
  n.H = 10,         
  n.P = 10,        
  # undirected variation 
  v.H = 0.15,
  v.P = 0.15,
  # intrinsic growth rate 
  r.H = 0.05, 
  r.P = 0.1, 
  # proportion of mean utility per capita:
  # Utility of individuals of type N
  mU.PnH = 1.7,
  mU.HnP = 1,
  # Utility of individuals of type 1
  mU.P1H = 0.17,                                  
  mU.H1P = 0.01,                                   
  # basic resources:
  # population of type N that can be sustained by resources independent of HP relationship
  mU.bHn = 0.08,                                
  mU.bPn = 1, 
  # population of type 1 that can be sustained by resources independent of HP relationship
  mU.bH1 = 80,                               
  mU.bP1 = 100,                                
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
  plot.directory = "plots/runPlot/",
  plot.fileName = "runPlot"
)

# Generate an animated GIF using the images in plot.directory folder

require(purrr)
require(magick)

files <- dir("plots/runPlot/")
for (i in 1:length(files))
{
  files[i] <- paste("plots/runPlot/", files[i], sep = "")
}
images <- map(files, image_read)
images <- image_join(images)
animation <- image_animate(images, fps = 10)
image_write(animation, "plots/runPlot/runPlot.gif")
