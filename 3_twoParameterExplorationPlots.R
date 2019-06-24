# Two parameter exploration

source("library/hpcModel.run.R")
source("library/hpcModel.exploration.R")
source("library/hpcModel.plot.R")
require(reshape2)
require(ggplot2)
require(scales)

twoPar.ggplot <- function(twoPar.exp, par1, par2, var1, var2, 
                          xlab = NULL, ylab = NULL, var1lab = NULL, var2lab = NULL,
                          plotScale = 1)
{
  if (is.null(xlab)) {
    xlab = par1
  }
  if (is.null(ylab)) {
    ylab = par2
  }
  if (is.null(var1lab)) {
    var1lab = var1
  }
  if (is.null(var2lab)) {
    var2lab = var2
  }
  
  var1min = -1#min(twoPar.exp[,var1])
  var1max = 1#max(twoPar.exp[,var1])
  var2min = -1#min(twoPar.exp[,var2])
  var2max = 1#max(twoPar.exp[,var2])
  
  ggplot(twoPar.exp, aes_string(x = par1, y = par2)) +
    geom_raster(aes_string(fill = var1)) +
    geom_point(aes_string(color = var2),
               shape = 15,
               size = plotScale * 1.5) +
    geom_point(aes(size = time), shape = 't', color = 'grey') +
    scale_fill_gradientn(
      colors = c('white', 'darkblue'),
      values = rescale(c(var1min, var1max)),
      limits = c(var1min, var1max)
    ) +
    scale_color_gradientn(
      colors = c('white', 'darkred'),
      values = rescale(c(var2min, var2max)),
      limits = c(var2min, var2max)
    ) +
    scale_size(range = c(plotScale * 0.5, plotScale * 2)) +
    labs(x = xlab,
         y = ylab,
         fill = var1lab,
         color = var2lab) +
    theme_bw() +
    theme(
      panel.background = element_blank(),
      axis.title = element_text(size = plotScale * 3.5),
      axis.text = element_text(size = plotScale * 2),
      legend.title = element_text(size = plotScale * 3.5),
      legend.text = element_text(size = plotScale * 2)
    )
}

## Full example

### Utility per capita from type n humans and plants (mU.HnP x mU.PnH):

exp.mU.HnP_mU.PnH.name <- "mU.HnP-mU.PnH"
SEQ.mU.HnP <- seq(0, 2.5, length.out = 15)
SEQ.mU.PnH <- seq(0, 2.5, length.out = 15)

exp.mU.HnP_mU.PnH <- hpcModel.exploration(
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
  # Utility per capita of individuals of type n
  mU.PnH = SEQ.mU.PnH,
  mU.HnP = SEQ.mU.HnP,
  # Utility per capita of individuals of type 1
  mU.P1H = 0.15,                                  
  mU.H1P = 0,                                   
  # basic resources:
  # population of type n that can be sustained by resources independent of HP relationship
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
  messages = FALSE
)

plotScale = 100 / length(SEQ.mU.HnP)

png(paste0("plots/3_twoPar-", exp.mU.HnP_mU.PnH.name, "_plot.png"),
    width = 100 * plotScale, height = 100 * plotScale)
twoPar.ggplot(exp.mU.HnP_mU.PnH, 'mU.HnP', 'mU.PnH', 'coevo.H', 'coevo.P', 
              xlab = expression(bar(U)['HnP']),
              ylab = expression(bar(U)['PnH']),
              var1lab = expression('coevo'[H]),
              var2lab = expression('coevo'[P]),
              plotScale = plotScale)
dev.off()

#---
  
## Exploration on 'default' setting for (directly related) parameter pairs:
  
### Number of types of humans and plants (n.H x n.P):
  
exp.n.H_n.P.name <- "n.H-n.P"
SEQ.n.H <- seq(3, 42, by = 3)
SEQ.n.P <- seq(3, 42, by = 3)

exp.n.H_n.P <- hpcModel.exploration(
  # initial populations
  iniH = 10,
  iniP = 10,
  # number of discrete types
  n.H = SEQ.n.H,         
  n.P = SEQ.n.P,        
  # undirected variation 
  v.H = 0.15,
  v.P = 0.15,
  # intrinsic growth rate 
  r.H = 0.04, 
  r.P = 0.1, 
  # Utility per capita of individuals of type n
  mU.PnH = 1.5,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = 0.15,                                  
  mU.H1P = 0,                                   
  # basic resources:
  # population of type n that can be sustained by resources independent of HP relationship
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
  messages = FALSE
)

plotScale = 100 / length(SEQ.n.H)

png(paste0("plots/3_twoPar-", exp.n.H_n.P.name, "_plot.png"),
    width = 100 * plotScale, height = 100 * plotScale)
twoPar.ggplot(exp.n.H_n.P, 'n.H', 'n.P', 'coevo.H', 'coevo.P', 
              xlab = expression(n['H']),
              ylab = expression(n['P']),
              var1lab = expression('coevo'[H]),
              var2lab = expression('coevo'[P]),
              plotScale = plotScale)
dev.off()

### Undirected variation in humans and plants (v.H x v.P):

exp.v.H_v.P.name <- "v.H-v.P"
SEQ.v.H <- seq(0.05, 0.25, length.out = 15)
SEQ.v.P <- seq(0.05, 0.25, length.out = 15)

exp.v.H_v.P <- hpcModel.exploration(
  # initial populations
  iniH = 10,
  iniP = 10,
  # number of discrete types
  n.H = 30,         
  n.P = 30,        
  # undirected variation 
  v.H = SEQ.v.H,
  v.P = SEQ.v.P,
  # intrinsic growth rate 
  r.H = 0.04, 
  r.P = 0.1, 
  # Utility per capita of individuals of type n
  mU.PnH = 1.5,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = 0.15,                                  
  mU.H1P = 0,                                   
  # basic resources:
  # population of type n that can be sustained by resources independent of HP relationship
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
  messages = FALSE
)

plotScale = 100 / length(SEQ.v.H)

png(paste0("plots/3_twoPar-", exp.v.H_v.P.name, "_plot.png"),
    width = 100 * plotScale, height = 100 * plotScale)
twoPar.ggplot(exp.v.H_v.P, 'v.H', 'v.P', 'coevo.H', 'coevo.P', 
              xlab = expression(v['H']),
              ylab = expression(v['P']),
              var1lab = expression('coevo'[H]),
              var2lab = expression('coevo'[P]),
              plotScale = plotScale)
dev.off()

### Utility per capita from type 1 humans and plants (mU.H1P x mU.P1H):

exp.mU.H1P_mU.P1H.name <- "mU.H1P-mU.P1H"
SEQ.mU.H1P <- seq(0, 2.5, length.out = 15)
SEQ.mU.P1H <- seq(0, 2.5, length.out = 15)

exp.mU.H1P_mU.P1H <- hpcModel.exploration(
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
  # Utility per capita of individuals of type n
  mU.PnH = 1.5,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = SEQ.mU.P1H,                                  
  mU.H1P = SEQ.mU.H1P,                                   
  # basic resources:
  # population of type n that can be sustained by resources independent of HP relationship
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
  messages = FALSE
)

plotScale = 100 / length(SEQ.mU.H1P)

png(paste0("plots/3_twoPar-", exp.mU.H1P_mU.P1H.name, "_plot.png"),
    width = 100 * plotScale, height = 100 * plotScale)
twoPar.ggplot(exp.mU.H1P_mU.P1H, 'mU.H1P', 'mU.P1H', 'coevo.H', 'coevo.P', 
              xlab = expression(bar(U)['H1P']),
              ylab = expression(bar(U)['P1H']),
              var1lab = expression('coevo'[H]),
              var2lab = expression('coevo'[P]),
              plotScale = plotScale)
dev.off()

### Utility per capita from type n humans and plants (mU.HnP x mU.PnH):

# see above (full example)

### Utility per capita from humans to plants (mU.H1P x mU.HnP):

exp.mU.H1P_mU.HnP.name <- "mU.H1P-mU.HnP"
SEQ.mU.H1P <- seq(0, 2.5, length.out = 15)
SEQ.mU.HnP <- seq(0, 2.5, length.out = 15)

exp.mU.H1P_mU.HnP <- hpcModel.exploration(
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
  # Utility per capita of individuals of type n
  mU.PnH = 1.5,
  mU.HnP = SEQ.mU.HnP,
  # Utility per capita of individuals of type 1
  mU.P1H = 0.15,                                  
  mU.H1P = SEQ.mU.H1P,                                   
  # basic resources:
  # population of type n that can be sustained by resources independent of HP relationship
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
  messages = FALSE
)

plotScale = 100 / length(SEQ.mU.H1P)

png(paste0("plots/3_twoPar-", exp.mU.H1P_mU.HnP.name, "_plot.png"),
    width = 100 * plotScale, height = 100 * plotScale)
twoPar.ggplot(exp.mU.H1P_mU.HnP, 'mU.H1P', 'mU.HnP', 'coevo.H', 'coevo.P', 
              xlab = expression(bar(U)['H1P']),
              ylab = expression(bar(U)['HnP']),
              var1lab = expression('coevo'[H]),
              var2lab = expression('coevo'[P]),
              plotScale = plotScale)
dev.off()

### Utility per capita from plants to humans (mU.P1H x mU.PnH):

exp.mU.P1H_mU.PnH.name <- "mU.P1H-mU.PnH"
SEQ.mU.P1H <- seq(0, 2.5, length.out = 15)
SEQ.mU.PnH <- seq(0, 2.5, length.out = 15)

exp.mU.P1H_mU.PnH <- hpcModel.exploration(
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
  # Utility per capita of individuals of type n
  mU.PnH = SEQ.mU.PnH,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = SEQ.mU.P1H,                                  
  mU.H1P = 0,                                   
  # basic resources:
  # population of type n that can be sustained by resources independent of HP relationship
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
  messages = FALSE
)

plotScale = 100 / length(SEQ.mU.P1H)

png(paste0("plots/3_twoPar-", exp.mU.P1H_mU.PnH.name, "_plot.png"),
    width = 100 * plotScale, height = 100 * plotScale)
twoPar.ggplot(exp.mU.P1H_mU.PnH, 'mU.P1H', 'mU.PnH', 'coevo.H', 'coevo.P', 
              xlab = expression(bar(U)['P1H']),
              ylab = expression(bar(U)['PnH']),
              var1lab = expression('coevo'[H]),
              var2lab = expression('coevo'[P]),
              plotScale = plotScale)
dev.off()

### Utility of other resources to type 1 humans and plants (U.bH1 x U.bP1):

exp.U.bH1_U.bP1.name <- "U.bH1-U.bP1"
SEQ.U.bH1 <- seq(10, 150, length.out = 15)
SEQ.U.bP1 <- seq(10, 150, length.out = 15)

exp.U.bH1_U.bP1 <- hpcModel.exploration(
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
  # Utility per capita of individuals of type n
  mU.PnH = 1.5,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = 0.15,                                  
  mU.H1P = 0,                                   
  # basic resources:
  # population of type n that can be sustained by resources independent of HP relationship
  U.bHn = 10,                                
  U.bPn = 20, 
  # population of type 1 that can be sustained by resources independent of HP relationship
  U.bH1 = SEQ.U.bH1,                               
  U.bP1 = SEQ.U.bP1,                                
  # maximum local area to be used by populations (multiplier or scaling effect)
  MaxArea = 200, 
  # settings 
  # simulation flow & data
  maxIt = 20000,
  tol = 6,
  messages = FALSE
)

plotScale = 100 / length(SEQ.U.bH1)

png(paste0("plots/3_twoPar-", exp.U.bH1_U.bP1.name, "_plot.png"),
    width = 100 * plotScale, height = 100 * plotScale)
twoPar.ggplot(exp.U.bH1_U.bP1, 'U.bH1', 'U.bP1', 'coevo.H', 'coevo.P', 
              xlab = expression(U['bH1']),
              ylab = expression(U['bP1']),
              var1lab = expression('coevo'[H]),
              var2lab = expression('coevo'[P]),
              plotScale = plotScale)
dev.off()

### Utility of other resources to type n humans and plants (U.bHn x U.bPn):

exp.U.bHn_U.bPn.name <- "U.bHn-U.bPn"
SEQ.U.bHn <- seq(10, 150, length.out = 15)
SEQ.U.bPn <- seq(10, 150, length.out = 15)

exp.U.bHn_U.bPn <- hpcModel.exploration(
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
  # Utility per capita of individuals of type n
  mU.PnH = 1.5,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = 0.15,                                  
  mU.H1P = 0,                                   
  # basic resources:
  # population of type n that can be sustained by resources independent of HP relationship
  U.bHn = SEQ.U.bHn,                                
  U.bPn = SEQ.U.bPn, 
  # population of type 1 that can be sustained by resources independent of HP relationship
  U.bH1 = 80,                               
  U.bP1 = 100,                                
  # maximum local area to be used by populations (multiplier or scaling effect)
  MaxArea = 200, 
  # settings 
  # simulation flow & data
  maxIt = 20000,
  tol = 6,
  messages = FALSE
)

plotScale = 100 / length(SEQ.U.bHn)

png(paste0("plots/3_twoPar-", exp.U.bHn_U.bPn.name, "_plot.png"),
    width = 100 * plotScale, height = 100 * plotScale)
twoPar.ggplot(exp.U.bHn_U.bPn, 'U.bHn', 'U.bPn', 'coevo.H', 'coevo.P', 
              xlab = expression(U['bHn']),
              ylab = expression(U['bPn']),
              var1lab = expression('coevo'[H]),
              var2lab = expression('coevo'[P]),
              plotScale = plotScale)
dev.off()

### Utility of other resources to humans (U.bH1 x U.bHn):

exp.U.bH1_U.bHn.name <- "U.bH1-U.bHn"
SEQ.U.bH1 <- seq(10, 150, length.out = 15)
SEQ.U.bHn <- seq(10, 150, length.out = 15)

exp.U.bH1_U.bHn <- hpcModel.exploration(
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
  # Utility per capita of individuals of type n
  mU.PnH = 1.5,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = 0.15,                                  
  mU.H1P = 0,                                   
  # basic resources:
  # population of type n that can be sustained by resources independent of HP relationship
  U.bHn = SEQ.U.bHn,                                
  U.bPn = 20, 
  # population of type 1 that can be sustained by resources independent of HP relationship
  U.bH1 = SEQ.U.bH1,                               
  U.bP1 = 100,                                
  # maximum local area to be used by populations (multiplier or scaling effect)
  MaxArea = 200, 
  # settings 
  # simulation flow & data
  maxIt = 20000,
  tol = 6,
  messages = FALSE
)

plotScale = 100 / length(SEQ.U.bHn)

png(paste0("plots/3_twoPar-", exp.U.bH1_U.bHn.name, "_plot.png"),
    width = 100 * plotScale, height = 100 * plotScale)
twoPar.ggplot(exp.U.bH1_U.bHn, 'U.bH1', 'U.bHn', 'coevo.H', 'coevo.P', 
              xlab = expression(U['bH1']),
              ylab = expression(U['bHn']),
              var1lab = expression('coevo'[H]),
              var2lab = expression('coevo'[P]),
              plotScale = plotScale)
dev.off()

### Utility of other resources to plants (U.bP1 x U.bPn):

exp.U.bP1_U.bPn.name <- "U.bP1-U.bPn"
SEQ.U.bP1 <- seq(10, 150, length.out = 15)
SEQ.U.bPn <- seq(10, 150, length.out = 15)

exp.U.bP1_U.bPn <- hpcModel.exploration(
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
  # Utility per capita of individuals of type n
  mU.PnH = 1.5,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = 0.15,                                  
  mU.H1P = 0,                                   
  # basic resources:
  # population of type n that can be sustained by resources independent of HP relationship
  U.bHn = 10,                                
  U.bPn = SEQ.U.bPn, 
  # population of type 1 that can be sustained by resources independent of HP relationship
  U.bH1 = 80,                               
  U.bP1 = SEQ.U.bP1,                                
  # maximum local area to be used by populations (multiplier or scaling effect)
  MaxArea = 200, 
  # settings 
  # simulation flow & data
  maxIt = 20000,
  tol = 6,
  messages = FALSE
)

plotScale = 100 / length(SEQ.U.bPn)

png(paste0("plots/3_twoPar-", exp.U.bP1_U.bPn.name, "_plot.png"),
    width = 100 * plotScale, height = 100 * plotScale)
twoPar.ggplot(exp.U.bP1_U.bPn, 'U.bP1', 'U.bPn', 'coevo.H', 'coevo.P', 
              xlab = expression(U['bP1']),
              ylab = expression(U['bPn']),
              var1lab = expression('coevo'[H]),
              var2lab = expression('coevo'[P]),
              plotScale = plotScale)
dev.off()
