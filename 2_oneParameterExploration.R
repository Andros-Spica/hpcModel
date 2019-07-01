# One parameter exploration

source("library/hpcModel.run.R")
source("library/hpcModel.exploration.R")
source("library/hpcModel.plot.R")
require(reshape2)
require(ggplot2)
require(scales)

bifurcation.plot.simple <- function(parameterValues, 
                                    observedVariables, 
                                    xlab = "parameter", 
                                    ylab = "observed variable",
                                    legendPos = c(100, 1),
                                    variableNames = paste("variable ", seq(0, ncol(observedVariables))),
                                    col = rainbow(ncol(observedVariables), v=.8),
                                    cex = 1
)
{
  plot(parameterValues,
       observedVariables[,1],
       type = 'l',
       col = col[1],
       #ylim = c(-1, 2),
       ylab = ylab,
       xlab = xlab,
       cex.axis = cex * 0.2,
       cex.lab = cex * 0.2,
       lwd = cex)
  
  for (v in 1:ncol(observedVariables))
  {
    points(parameterValues, observedVariables[,v], type = 'l', col = col[v], lwd = cex)
  }
  
  legend(legendPos[1], legendPos[2], legend = variableNames, col = col, lty = 1, lwd = cex, cex = cex * 0.2)
}

bifurcation.plot.pair <- function(result.H, result.P, 
                                  parameterLabel, variableLabelH, variableLabelP, 
                                  plotScale = 1)
{
  ylim = c(
    -1,#min(c(min(result.H[,2]), min(result.P[,2]))), 
    1#max(c(max(result.H[,2]), max(result.P[,2]))))
  )
  
  layout(matrix(c(1, 2), ncol = 2))
  
  par(mar = plotScale * c(1.2, 1.2, 0.1, 0.1),
      mgp = plotScale * c(0.8, 0.3, 0),
      cex.lab = plotScale * 0.5,
      cex.axis = plotScale * 0.5,
      lwd = plotScale
  )
  
  plot(result.H,
       #pch = 1,
       xlab = parameterLabel,
       ylab = variableLabelH,
       cex = plotScale,
       type = 'c',
       ylim = ylim
  )
  
  plot(result.P,
       #pch = 1,
       xlab = parameterLabel,
       ylab = variableLabelP,
       cex = plotScale,
       type = 'c',
       ylim = ylim
  )
}

## Full example (table+plot alternatives)

### utility per capita **of** type n plants **to** humans (mU.PnH):

exp.mU.PnH.name <- "mU.PnH"
SEQ.mU.PnH <- seq(0.5, 2.5, length.out = 100)

exp.mU.PnH <- hpcModel.exploration(
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
  # Utility per capita of individuals of type N
  mU.PnH = SEQ.mU.PnH,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = 0.15,                                  
  mU.H1P = 0.1,                                   
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
  messages = TRUE
)

plotScale = 10
#svg('paste0("plots/2_onePar-", exp1.name, "_ggbifplot.png"), width=plotScale, height=plotScale)
png(paste0("plots/2_onePar-", exp.mU.PnH.name, "_ggbifplot.png"),
    width = 200 * plotScale, height = 100 * plotScale)
bifurcation.plot.simple(SEQ.mU.PnH, 
                        cbind(exp.mU.PnH$coevo.H, exp.mU.PnH$coevo.P),
                        legendPos = c(360, 0),
                        xlab = expression(paste('utility per capita of type n plants to humans (', bar('U')['PnH'], ')')),
                        ylab = "coevolution coefficient",
                        variableNames = c("Humans", "Plants"),
                        cex = plotScale
)
dev.off()

plotScale = 10

#svg(paste0("plots/2_onePar-", exp.mU.PnH.name, "_bifplot-pair.svg"), width=plotScale, height=plotScale)
png(paste0("plots/2_onePar-", exp.mU.PnH.name, "_bifplot-pair.png"),
    width = 400 * plotScale, height = 100 * plotScale)
bifurcation.plot.pair(cbind(SEQ.mU.PnH, exp.mU.PnH$coevo.H), 
                      cbind(SEQ.mU.PnH, exp.mU.PnH$coevo.P), 
                      parameterLabel = expression(paste('utility per capita of type n plants to humans (', bar('U')['PnH'], ')')),
                      variableLabelH = expression('coevo'['H']),
                      variableLabelP = expression('coevo'['P']),
                      plotScale = plotScale)
dev.off()

#---
  
## Exploration on 'default' setting for each parameter:
  
### Initial populations of humans and plants (init.H x init.P):
  
exp.init.H.name <- "init.H"
exp.init.P.name <- "init.P"
SEQ.init.H <- 5:100
SEQ.init.P <- 5:100

exp.init.H <- hpcModel.exploration(
  # initial populations
  iniH = SEQ.init.H,
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
  # Utility per capita of individuals of type N
  mU.PnH = 1.5,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = 0.15,                                  
  mU.H1P = 0.1,                                   
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
  messages = TRUE
)

exp.init.P <- hpcModel.exploration(
  # initial populations
  iniH = 10,
  iniP = SEQ.init.P,
  # number of discrete types
  n.H = 10,         
  n.P = 10,        
  # undirected variation 
  v.H = 0.15,
  v.P = 0.15,
  # intrinsic growth rate 
  r.H = 0.05, 
  r.P = 0.1, 
  # Utility per capita of individuals of type N
  mU.PnH = 1.5,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = 0.15,                                  
  mU.H1P = 0.1,                                   
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
  messages = TRUE
)

plotScale = 10

#svg(paste0("plots/2_onePar-", exp.init.H.name, "_bifplot-pair.svg"), width=plotScale, height=plotScale)
png(paste0("plots/2_onePar-", exp.init.H.name, "_bifplot-pair.png"),
    width = 400 * plotScale, height = 100 * plotScale)
bifurcation.plot.pair(cbind(SEQ.init.H, exp.init.H$coevo.H), 
                      cbind(SEQ.init.H, exp.init.H$coevo.P), 
                      parameterLabel = expression(paste('Initial human population (',
                                                        'ini'['H'], ')')),
                      variableLabelH = expression('coevo'['H']),
                      variableLabelP = expression('coevo'['P']),
                      plotScale = plotScale)
dev.off()

plotScale = 10

#svg(paste0("plots/2_onePar-", exp.init.P.name, "_bifplot-pair.svg"), width=plotScale, height=plotScale)
png(paste0("plots/2_onePar-", exp.init.P.name, "_bifplot-pair.png"),
    width = 400 * plotScale, height = 100 * plotScale)
bifurcation.plot.pair(cbind(SEQ.init.P, exp.init.P$coevo.H), 
                      cbind(SEQ.init.P, exp.init.P$coevo.P), 
                      parameterLabel = expression(paste('Initial plant population (',
                                                        'ini'['P'], ')')),
                      variableLabelH = expression('coevo'['H']),
                      variableLabelP = expression('coevo'['P']),
                      plotScale = plotScale)
dev.off()

### Number of types of humans and plants (n.H x n.P):

exp.n.H.name <- "n.H"
exp.n.P.name <- "n.P"
SEQ.n.H <- 5:40
SEQ.n.P <- 5:40

exp.n.H <- hpcModel.exploration(
  # initial populations
  iniH = 10,
  iniP = 10,
  # number of discrete types
  n.H = SEQ.n.H,         
  n.P = 10,        
  # undirected variation 
  v.H = 0.15,
  v.P = 0.15,
  # intrinsic growth rate 
  r.H = 0.05, 
  r.P = 0.1, 
  # Utility per capita of individuals of type N
  mU.PnH = 1.5,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = 0.15,                                  
  mU.H1P = 0.1,                                   
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
  messages = TRUE
)

exp.n.P <- hpcModel.exploration(
  # initial populations
  iniH = 10,
  iniP = 10,
  # number of discrete types
  n.H = 10,         
  n.P = SEQ.n.P,        
  # undirected variation 
  v.H = 0.15,
  v.P = 0.15,
  # intrinsic growth rate 
  r.H = 0.05, 
  r.P = 0.1, 
  # Utility per capita of individuals of type N
  mU.PnH = 1.5,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = 0.15,                                  
  mU.H1P = 0.1,                                   
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
  messages = TRUE
)

plotScale = 10

#svg(paste0("plots/2_onePar-", exp1.name, "_bifplot-pair.svg"), width=plotScale, height=plotScale)
png(paste0("plots/2_onePar-", exp.n.H.name, "_bifplot-pair.png"),
    width = 400 * plotScale, height = 100 * plotScale)
bifurcation.plot.pair(cbind(SEQ.n.H, exp.n.H$coevo.H), 
                      cbind(SEQ.n.H, exp.n.H$coevo.P), 
                      parameterLabel = expression(paste('number of human types (',
                                                        'n'['H'], ')')),
                      variableLabelH = expression('coevo'['H']),
                      variableLabelP = expression('coevo'['P']),
                      plotScale = plotScale)
dev.off()

plotScale = 10

#svg(paste0("plots/2_onePar-", exp.n.P.name, "_bifplot-pair.svg"), width=plotScale, height=plotScale)
png(paste0("plots/2_onePar-", exp.n.P.name, "_bifplot-pair.png"),
    width = 400 * plotScale, height = 100 * plotScale)
bifurcation.plot.pair(cbind(SEQ.n.P, exp.n.P$coevo.H), 
                      cbind(SEQ.n.P, exp.n.P$coevo.P), 
                      parameterLabel = expression(paste('number of plant types (',
                                                        'n'['P'], ')')),
                      variableLabelH = expression('coevo'['H']),
                      variableLabelP = expression('coevo'['P']),
                      plotScale = plotScale)
dev.off()

### level of undirected variation in humans and plants (v.H x v.P):

exp.v.H.name <- "v.H"
exp.v.P.name <- "v.P"
SEQ.v.H <- seq(0.05, 0.25, length.out = 100)
SEQ.v.P <- seq(0.05, 0.25, length.out = 100)

exp.v.H <- hpcModel.exploration(
  # initial populations
  iniH = 10,
  iniP = 10,
  # number of discrete types
  n.H = 10,         
  n.P = 10,        
  # undirected variation 
  v.H = SEQ.v.H,
  v.P = 0.15,
  # intrinsic growth rate 
  r.H = 0.05, 
  r.P = 0.1, 
  # Utility per capita of individuals of type N
  mU.PnH = 1.5,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = 0.15,                                  
  mU.H1P = 0.1,                                   
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
  messages = TRUE
)

exp.v.P <- hpcModel.exploration(
  # initial populations
  iniH = 10,
  iniP = 10,
  # number of discrete types
  n.H = 10,         
  n.P = 10,        
  # undirected variation 
  v.H = 0.15,
  v.P = SEQ.v.P,
  # intrinsic growth rate 
  r.H = 0.05, 
  r.P = 0.1, 
  # Utility per capita of individuals of type N
  mU.PnH = 1.5,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = 0.15,                                  
  mU.H1P = 0.1,                                   
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
  messages = TRUE
)

plotScale = 10

#svg(paste0("plots/2_onePar-", exp.v.H.name, "_bifplot-pair.svg"), width=plotScale, height=plotScale)
png(paste0("plots/2_onePar-", exp.v.H.name, "_bifplot-pair.png"),
    width = 400 * plotScale, height = 100 * plotScale)
bifurcation.plot.pair(cbind(SEQ.v.H, exp.v.H$coevo.H), 
                      cbind(SEQ.v.H, exp.v.H$coevo.P), 
                      parameterLabel = expression(paste('undirected variation in humans (',
                                                        'v'['H'], ')')),
                      variableLabelH = expression('coevo'['H']),
                      variableLabelP = expression('coevo'['P']),
                      plotScale = plotScale)
dev.off()

plotScale = 10

#svg(paste0("plots/2_onePar-", exp.v.P.name, "_bifplot-pair.svg"), width=plotScale, height=plotScale)
png(paste0("plots/2_onePar-", exp.v.P.name, "_bifplot-pair.png"),
    width = 400 * plotScale, height = 100 * plotScale)
bifurcation.plot.pair(cbind(SEQ.v.P, exp.v.P$coevo.H), 
                      cbind(SEQ.v.P, exp.v.P$coevo.P), 
                      parameterLabel = expression(paste('undirected variation in plants (',
                                                        'v'['P'], ')')),
                      variableLabelH = expression('coevo'['H']),
                      variableLabelP = expression('coevo'['P']),
                      plotScale = plotScale)
dev.off()

### intrinsic growth rates for human and plant populations (r.H x r.P):

exp.r.H.name <- "r.H"
exp.r.P.name <- "r.P"
SEQ.r.H <- seq(0.05, 0.25, length.out = 100)
SEQ.r.P <- seq(0.01, 0.25, length.out = 100)

exp.r.H <- hpcModel.exploration(
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
  r.H = SEQ.r.H, 
  r.P = 0.1, 
  # Utility per capita of individuals of type N
  mU.PnH = 1.5,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = 0.15,                                  
  mU.H1P = 0.1,                                   
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
  messages = TRUE
)

exp.r.P <- hpcModel.exploration(
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
  r.P = SEQ.r.P, 
  # Utility per capita of individuals of type N
  mU.PnH = 1.5,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = 0.15,                                  
  mU.H1P = 0.1,                                   
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
  messages = TRUE
)

plotScale = 10

#svg(paste0("plots/2_onePar-", exp.r.H.name, "_bifplot-pair.svg"), width=plotScale, height=plotScale)
png(paste0("plots/2_onePar-", exp.r.H.name, "_bifplot-pair.png"),
    width = 400 * plotScale, height = 100 * plotScale)
bifurcation.plot.pair(cbind(SEQ.r.H, exp.r.H$coevo.H), 
                      cbind(SEQ.r.H, exp.r.H$coevo.P), 
                      parameterLabel = expression(paste('human instrinsic growth rate (',
                                                        'r'['H'], ')')),
                      variableLabelH = expression('coevo'['H']),
                      variableLabelP = expression('coevo'['P']),
                      plotScale = plotScale)
dev.off()

plotScale = 10

#svg(paste0("plots/2_onePar-", exp.r.P.name, "_bifplot-pair.svg"), width=plotScale, height=plotScale)
png(paste0("plots/2_onePar-", exp.r.P.name, "_bifplot-pair.png"),
    width = 400 * plotScale, height = 100 * plotScale)
bifurcation.plot.pair(cbind(SEQ.r.P, exp.r.P$coevo.H), 
                      cbind(SEQ.r.P, exp.r.P$coevo.P), 
                      parameterLabel = expression(paste('plant instrinsic growth rate (',
                                                        'r'['P'], ')')),
                      variableLabelH = expression('coevo'['H']),
                      variableLabelP = expression('coevo'['P']),
                      plotScale = plotScale)
dev.off()

### utility per capita **of** type n plants **to** humans (mU.PnH):

# see above (full example)

### utility per capita **of** type n human **to** plants (mU.HnP):

exp.mU.HnP.name <- "mU.HnP"
SEQ.mU.HnP <- seq(0, 1.5, length.out = 100)

exp.mU.HnP <- hpcModel.exploration(
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
  # Utility per capita of individuals of type N
  mU.PnH = 1.5,
  mU.HnP = SEQ.mU.HnP,
  # Utility per capita of individuals of type 1
  mU.P1H = 0.15,                                  
  mU.H1P = 0.1,                                   
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
  messages = TRUE
)

plotScale = 10

#svg(paste0("plots/2_onePar-", exp.mU.HnP.name, "_bifplot-pair.svg"), width=plotScale, height=plotScale)
png(paste0("plots/2_onePar-", exp.mU.HnP.name, "_bifplot-pair.png"),
    width = 400 * plotScale, height = 100 * plotScale)
bifurcation.plot.pair(cbind(SEQ.mU.HnP, exp.mU.HnP$coevo.H), 
                      cbind(SEQ.mU.HnP, exp.mU.HnP$coevo.P), 
                      parameterLabel = expression(paste('utility per capita of type n human to plants (', bar('U')['HnP'], ')')),
                      variableLabelH = expression('coevo'['H']),
                      variableLabelP = expression('coevo'['P']),
                      plotScale = plotScale)
dev.off()

### utility per capita **of** type 1 plants **to** humans (mU.P1H):

exp.mU.P1H.name <- "mU.P1H"
SEQ.mU.P1H <- seq(0, 0.5, length.out = 100)

exp.mU.P1H <- hpcModel.exploration(
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
  # Utility per capita of individuals of type N
  mU.PnH = 1.5,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = SEQ.mU.P1H,                                  
  mU.H1P = 0.1,                                   
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
  messages = TRUE
)

plotScale = 10

#svg(paste0("plots/2_onePar-", exp.mU.P1H.name, "_bifplot-pair.svg"), width=plotScale, height=plotScale)
png(paste0("plots/2_onePar-", exp.mU.P1H.name, "_bifplot-pair.png"),
    width = 400 * plotScale, height = 100 * plotScale)
bifurcation.plot.pair(cbind(SEQ.mU.P1H, exp.mU.P1H$coevo.H), 
                      cbind(SEQ.mU.P1H, exp.mU.P1H$coevo.P), 
                      parameterLabel = expression(paste('utility per capita of type 1 plants to humans (', bar('U')['P1H'], ')')),
                      variableLabelH = expression('coevo'['H']),
                      variableLabelP = expression('coevo'['P']),
                      plotScale = plotScale)
dev.off()

### utility per capita **of** type 1 humans **to** plants (mU.H1P):

exp.mU.H1P.name <- "mU.H1P"
SEQ.mU.H1P <- seq(0, 2, length.out = 100)

exp.mU.H1P <- hpcModel.exploration(
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
  # Utility per capita of individuals of type N
  mU.PnH = 1.5,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = 0.15,                                  
  mU.H1P = SEQ.mU.H1P,                                   
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
  messages = TRUE
)

plotScale = 10

#svg(paste0("plots/2_onePar-", exp.mU.H1P.name, "_bifplot-pair.svg"), width=plotScale, height=plotScale)
png(paste0("plots/2_onePar-", exp.mU.H1P.name, "_bifplot-pair.png"),
    width = 400 * plotScale, height = 100 * plotScale)
bifurcation.plot.pair(cbind(SEQ.mU.H1P, exp.mU.H1P$coevo.H), 
                      cbind(SEQ.mU.H1P, exp.mU.H1P$coevo.P), 
                      parameterLabel = expression(paste('utility per capita of type 1 humans to plants (', bar('U')['H1P'], ')')),
                      variableLabelH = expression('coevo'['H']),
                      variableLabelP = expression('coevo'['P']),
                      plotScale = plotScale)
dev.off()

### utility **of** other resources **to** humans of type 1 ($U_{bH_{1}}$):

exp.U.bH1.name <- "U.bH1"
SEQ.U.bH1 <- seq(5, 400, length.out = 100)

exp.U.bH1 <- hpcModel.exploration(
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
  # Utility per capita of individuals of type N
  mU.PnH = 1.5,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = 0.15,                                  
  mU.H1P = 0.1,                                   
  # basic resources:
  # population of type N that can be sustained by resources independent of HP relationship
  U.bHn = 10,                                
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
  messages = TRUE
)

plotScale = 10

#svg(paste0("plots/2_onePar-", exp1.name, "_bifplot-pair.svg"), width=plotScale, height=plotScale)
png(paste0("plots/2_onePar-", exp.U.bH1.name, "_bifplot-pair.png"),
    width = 400 * plotScale, height = 100 * plotScale)
bifurcation.plot.pair(cbind(SEQ.U.bH1, exp.U.bH1$coevo.H), 
                      cbind(SEQ.U.bH1, exp.U.bH1$coevo.P), 
                      parameterLabel = expression(paste('Baseline carrying capacity for type 1 humans (', bar(U)['bH'[1]], ')')),
                      variableLabelH = expression('coevo'['H']),
                      variableLabelP = expression('coevo'['P']),
                      plotScale = plotScale)
dev.off()

### utility **of** non-anthropic space **to** type 1 plants (U.bP1):

exp.U.bP1.name <- "U.bP1"
SEQ.U.bP1 <- seq(5, 400, length.out = 100)

exp.U.bP1 <- hpcModel.exploration(
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
  # Utility per capita of individuals of type N
  mU.PnH = 1.5,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = 0.15,                                  
  mU.H1P = 0.1,                                   
  # basic resources:
  # population of type N that can be sustained by resources independent of HP relationship
  U.bHn = 10,                                
  U.bPn = 20, 
  # population of type 1 that can be sustained by resources independent of HP relationship
  U.bH1 = 80,                               
  U.bP1 = SEQ.U.bP1,                                
  # maximum local area to be used by populations (multiplier or scaling effect)
  MaxArea = 200, 
  # settings 
  # simulation flow & data
  maxIt = 20000,
  tol = 6,
  messages = TRUE
)

plotScale = 10

#svg(paste0("plots/2_onePar-", exp.U.bP1.name, "_bifplot-pair.svg"), width=plotScale, height=plotScale)
png(paste0("plots/2_onePar-", exp.U.bP1.name, "_bifplot-pair.png"),
    width = 400 * plotScale, height = 100 * plotScale)
bifurcation.plot.pair(cbind(SEQ.U.bP1, exp.U.bP1$coevo.H), 
                      cbind(SEQ.U.bP1, exp.U.bP1$coevo.P), 
                      parameterLabel = expression(paste('utility of non-anthropic space to type 1 plants (', bar('U')['bP1'], ')')),
                      variableLabelH = expression('coevo'['H']),
                      variableLabelP = expression('coevo'['P']),
                      plotScale = plotScale)
dev.off()

### utility **of** other resources **to** type n humans (U.bHn):

exp.U.bHn.name <- "U.bHn"
SEQ.U.bHn <- seq(5, 400, length.out = 100)

exp.U.bHn <- hpcModel.exploration(
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
  # Utility per capita of individuals of type N
  mU.PnH = 1.5,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = 0.15,                                  
  mU.H1P = 0.1,                                   
  # basic resources:
  # population of type N that can be sustained by resources independent of HP relationship
  U.bHn = SEQ.U.bHn,                                
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
  messages = TRUE
)

plotScale = 10

#svg(paste0("plots/2_onePar-", exp.U.bHn.name, "_bifplot-pair.svg"), width=plotScale, height=plotScale)
png(paste0("plots/2_onePar-", exp.U.bHn.name, "_bifplot-pair.png"),
    width = 400 * plotScale, height = 100 * plotScale)
bifurcation.plot.pair(cbind(SEQ.U.bHn, exp.U.bHn$coevo.H), 
                      cbind(SEQ.U.bHn, exp.U.bHn$coevo.P), 
                      parameterLabel = expression(paste('utility of other resources to type n humans (', bar('U')['bHn'], ')')),
                      variableLabelH = expression('coevo'['H']),
                      variableLabelP = expression('coevo'['P']),
                      plotScale = plotScale)
dev.off()

### utility **of** non-anthropic space **to** type n plants (U.bPn):

exp.U.bPn.name <- "U.bPn"
SEQ.U.bPn <- seq(5, 400, length.out = 100)

exp.U.bPn <- hpcModel.exploration(
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
  # Utility per capita of individuals of type N
  mU.PnH = 1.5,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = 0.15,                                  
  mU.H1P = 0.1,                                   
  # basic resources:
  # population of type N that can be sustained by resources independent of HP relationship
  U.bHn = 10,                                
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
  messages = TRUE
)

plotScale = 10

#svg(paste0("plots/2_onePar-", exp.U.bPn.name, "_bifplot-pair.svg"), width=plotScale, height=plotScale)
png(paste0("plots/2_onePar-", exp.U.bPn.name, "_bifplot-pair.png"),
    width = 400 * plotScale, height = 100 * plotScale)
bifurcation.plot.pair(cbind(SEQ.U.bPn, exp.U.bPn$coevo.H), 
                      cbind(SEQ.U.bPn, exp.U.bPn$coevo.P), 
                      parameterLabel = expression(paste('utility of non-anthropic space to type n plants (', bar('U')['bPn'], ')')),
                      variableLabelH = expression('coevo'['H']),
                      variableLabelP = expression('coevo'['P']),
                      plotScale = plotScale)
dev.off()

### maximum contiguous area to be used by plants (MaxArea):

exp.MaxArea.name <- "MaxArea"
SEQ.MaxArea <- seq(5, 400, length.out = 100)

exp.MaxArea <- hpcModel.exploration(
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
  # Utility per capita of individuals of type N
  mU.PnH = 1.5,
  mU.HnP = 1,
  # Utility per capita of individuals of type 1
  mU.P1H = 0.15,                                  
  mU.H1P = 0.1,                                   
  # basic resources:
  # population of type N that can be sustained by resources independent of HP relationship
  U.bHn = 10,                                
  U.bPn = 20, 
  # population of type 1 that can be sustained by resources independent of HP relationship
  U.bH1 = 80,                               
  U.bP1 = 100,                                
  # maximum local area to be used by populations (multiplier or scaling effect)
  MaxArea = SEQ.MaxArea, 
  # settings 
  # simulation flow & data
  maxIt = 20000,
  tol = 6,
  messages = TRUE
)

plotScale = 10

#svg(paste0("plots/2_onePar-", exp.MaxArea.name, "_bifplot-pair.svg"), width=plotScale, height=plotScale)
png(paste0("plots/2_onePar-", exp.MaxArea.name, "_bifplot-pair.png"),
    width = 400 * plotScale, height = 100 * plotScale)
bifurcation.plot.pair(cbind(SEQ.MaxArea, exp.MaxArea$coevo.H), 
                      cbind(SEQ.MaxArea, exp.MaxArea$coevo.P), 
                      parameterLabel = expression('MaxArea'),
                      variableLabelH = expression('coevo'['H']),
                      variableLabelP = expression('coevo'['P']),
                      plotScale = plotScale)
dev.off()

#---
  
## Bifurcation plot with last 100 time steps
  
# **to capture oscillations or 'slow' asymptotic stability**
  
# **INCOMPLETE**
  
# exp.osc.name <- "osc"
# SEQ.osc <- seq(20, 400, length.out = 100)
# 
# result.H <- data.frame()
# result.P <- data.frame()
# 
# for (i in SEQ.osc)
# {
#   exp.osc <- hpcModel.run(
#     # initial populations
#     iniH = 10,
#     iniP = 10,
#     # number of discrete types
#     n.H = 10,         
#     n.P = 10,        
#     # undirected variation 
#     v.H = 0.15,
#     v.P = 0.15,
#     # intrinsic growth rate 
#     r.H = 0.05, 
#     r.P = 0.1, 
#     # Utility per capita of individuals of type N
#     mU.PnH = 1.5,
#     mU.HnP = 1,
#     # Utility per capita of individuals of type 1
#     mU.P1H = 0.15,                                  
#     mU.H1P = 0.1,                                   
#     # basic resources:
#     # population of type N that can be sustained by resources independent of HP relationship
#     U.bHn = 10,                                
#     U.bPn = 20, 
#     # population of type 1 that can be sustained by resources independent of HP relationship
#     U.bH1 = i,                               
#     U.bP1 = 100, 
#     # maximum local area to be used by populations (multiplier or scaling effect)
#     MaxArea = 200, 
#     # settings 
#     # simulation flow & data
#     maxIt = 600,
#     tol = Inf,
#     saveTrajectories = TRUE,
#     messages = TRUE
#   )
#   
#   # store the result
#   result.H <-
#     rbind(result.H, data.frame(i, exp.osc$TRAJECTORIES$coevo.H[400:599]))
#   result.P <-
#     rbind(result.P, data.frame(i, exp.osc$TRAJECTORIES$coevo.P[400:599]))
#   
# }
# 
# plotScale = 10
# 
# #svg(paste0("plots/2_onePar-", exp.osc.name, "_bifplot-pair.svg"), width=plotScale, height=plotScale)
# png(paste0("plots/2_onePar-", exp.osc.name, "_bifplot-pair.png"),
#     width = 400 * plotScale, height = 100 * plotScale)
# bifurcation.plot.pair(result.H, result.P, 
#                       parameterLabel = expression(paste('parameter (',
#                                                         'ini'['H'], ')')),
#                       variableLabelH = expression('coevo'['H']),
#                       variableLabelP = expression('coevo'['P']),
#                       plotScale = plotScale)
# dev.off()
