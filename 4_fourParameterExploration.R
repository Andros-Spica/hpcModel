# Four parameter exploration

source("library/hpcModel.run.R")
source("library/hpcModel.exploration.R")
source("library/hpcModel.plot.R")
require(reshape2)
require(ggplot2)
require(scales)

fourPar.ggplot <- function(twoPar.exp, par1, par2, par3, par4, var1, var2, 
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
               size = plotScale * 0.8) +
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
    facet_grid(
      reformulate(par3, par4), 
      scales='free', 
      as.table = FALSE,
      labeller = label_both#label_wrap_gen(multi_line=FALSE)
    ) +
    theme_bw() +
    theme(
      strip.background = element_rect(fill = NA, colour = NA), 
      strip.text = element_text(size = plotScale * 1.5),
      panel.grid.minor =  element_blank(),
      panel.background = element_blank(),
      axis.title = element_text(size = plotScale * 3),
      axis.text = element_text(size = plotScale * 1.5),
      legend.title = element_text(size = plotScale * 3.5),
      legend.text = element_text(size = plotScale * 2)
    )
}

## Full example

### Utility per capita between humans and plants ($\bar{U}_{H_{1}P}$ x $\bar{U}_{P_{1}H}$ x $\bar{U}_{H_{n}P}$ x $\bar{U}_{P_{n}H}$):

exp.mU.HP_mU.PH.name <- "mU.HP-mU.PH"
SEQ.mU.HnP <- seq(0, 2.5, length.out = 5)
SEQ.mU.PnH <- seq(0, 2.5, length.out = 5)
SEQ.mU.H1P <- seq(0, 2.5, length.out = 5)
SEQ.mU.P1H <- seq(0, 2.5, length.out = 5)

exp.mU.HP_mU.PH <- hpcModel.exploration(
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
  mU.PnH = SEQ.mU.PnH,
  mU.HnP = SEQ.mU.HnP,
  # Utility per capita of individuals of type 1
  mU.P1H = SEQ.mU.P1H,                                  
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
  messages = FALSE
)

plotScale = 10#100 / length(SEQ.U.bPn)

png(paste0("plots/4_fourPar-", exp.mU.HP_mU.PH.name, "_plot.png"),
    width = 100 * plotScale, height = 100 * plotScale)
fourPar.ggplot(exp.mU.HP_mU.PH, 'mU.P1H', 'mU.H1P', 'mU.PnH', 'mU.HnP', 'coevo.H', 'coevo.P', 
               xlab = expression(bar(U)['P1H']),
               ylab = expression(bar(U)['H1P']),
               var1lab = expression('coevo'[H]),
               var2lab = expression('coevo'[P]),
               plotScale = plotScale)
dev.off()

# **_Interpretation_**:  
#   - Higher values of all four parameters facilitate coevolution; under the 'default' setting, a value around 1 is enough for all four parameters (intermediate values in this exploration).  
# - Coevolution is still possible if any single one of these parameters equal zero (bottom-left corners). Under this type of conditions, agriculture (blue) appears more probable than domestication (red), and the latter is strongly dependent on a non-null $\bar{U}_{H_{n}P}$.  
# - As a summary of possible end-states:  
#   - *'Fast' coevolution* (red square in blue tile, small *t*): most cases when values are greater than 0.625.  
# - *Domestication without cultivation* (red square in whitish tile): most cases when $\bar{U}_{H_{n}P} > 0.625$, $\bar{U}_{H_{1}P} => 0.625$, $\bar{U}_{P_{n}H} = 0$, and $\bar{U}_{P_{1}H} < 2.5$.  
# - *Cultivation without domestication* (whitish square in blue tile): most cases when $\bar{U}_{H_{n}P} = 0$.

### Utility from other resources to humans and plants ($U_{bH_{1}}$ x $U_{bP_{1}}$ x $U_{bH_{n}}$ x $U_{bP_{n}}$):

# For this experiment, consider that the default setting includes $MaxArea=200$ (i.e. the maximum for the plant population).

exp.U.bH_U.bP.name <- "U.bH-U.bP"
SEQ.U.bHn <- seq(5, 300, length.out = 5)
SEQ.U.bPn <- seq(5, 300, length.out = 5)
SEQ.U.bH1 <- seq(5, 300, length.out = 5)
SEQ.U.bP1 <- seq(5, 300, length.out = 5)

exp.U.bH_U.bP <- hpcModel.exploration(
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
  U.bHn = SEQ.U.bHn,                                
  U.bPn = SEQ.U.bPn, 
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

plotScale = 10#100 / length(SEQ.U.bPn)

png(paste0("plots/4_fourPar-", exp.U.bH_U.bP.name, "_plot.png"),
    width = 100 * plotScale, height = 100 * plotScale)
fourPar.ggplot(exp.U.bH_U.bP, 'U.bP1', 'U.bH1', 'U.bPn', 'U.bHn', 'coevo.H', 'coevo.P', 
               xlab = expression(U['bP1']),
               ylab = expression(U['bH1']),
               var1lab = expression('coevo'[H]),
               var2lab = expression('coevo'[P]),
               plotScale = plotScale)
dev.off()

# **_Interpretation_**:  
#   - Lower values of all four parameters facilitate coevolution; under the 'default' setting and for all four parameters, values higher than $MaxArea$ impede coevolution. The human parameters ($U_{bH_{1}}$, $U_{bH_{n}}$), together regulating the scale of the subsistence alternatives for humans, are significantly more important; their relationship (if one is greater than the other) seems to be less important as long as their combined sum is small enough.  
# - Coevolution is likely to occur when $U_{bH_{1}} = 0$, unless $U_{bP_{1}}$ is too small.  
# - As a summary of possible end-states:  
#   - *'Fast' coevolution* (red square in blue tile, small *t*): most cases when $U_{bH_{1}}$ and $U_{bH_{n}}$ are less than 100 (half of $MaxArea$).  
# - *Domestication without cultivation* (red square in whitish tile): most cases when $U_{bP_{n}} = 0$, $U_{bP_{1}} = 0$ (i.e. there is no carrying capacity for plants beyond the anthropic space) and $U_{bH_{1}} > 100$ (i.e. humans get plenty other resources when -still- not engaged in agriculture).  
# - *Cultivation without domestication* (whitish square in blue tile): *no cases visible*.


### Number of types and undirected variation of humans and plants ($n_{H}$ x $n_{P}$ x $v_{H}$ x $v_{P}$):

exp.n_v.name <- "n-v"
SEQ.n.H <- seq(5, 45, by = 10)
SEQ.n.P <- seq(5, 45, by = 10)
SEQ.v.H <- seq(0.05, 0.25, length.out = 5)
SEQ.v.P <- seq(0.05, 0.25, length.out = 5)

exp.n_v <- hpcModel.exploration(
  # initial populations
  iniH = 10,
  iniP = 10,
  # number of discrete types
  n.H = SEQ.n.H,         
  n.P = SEQ.n.P,        
  # undirected variation 
  v.H = SEQ.v.H,
  v.P = SEQ.v.P,
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
  messages = FALSE
)

plotScale = 10#100 / length(SEQ.U.bPn)

png(paste0("plots/4_fourPar-", exp.n_v.name, "_plot.png"),
    width = 100 * plotScale, height = 100 * plotScale)
fourPar.ggplot(exp.n_v, 'v.H', 'v.P', 'n.H', 'n.P', 'coevo.H', 'coevo.P', 
               xlab = expression(v['H']),
               ylab = expression(v['P']),
               var1lab = expression('coevo'[H]),
               var2lab = expression('coevo'[P]),
               plotScale = plotScale)
dev.off()

# **_Interpretation_**:  
#   - Higher values of all four parameters facilitate coevolution.  
# - As a summary of possible end-states:  
#   - *'Fast' coevolution* (red square in blue tile, small *t*): most cases when the numbers of types ($n_{H}$, $n_{P}$) are greater than **15** and values of undirected variation ($v_{H}$, $v_{P}$) higher than **0.15**.  
# - *'Semi-coevolution'* (redish square in blueish tile): cases when $n_{H}>=15$, $n_{P}>=15$, $v_{H}<=0.1$ and $v_{P}<=0.1$.  
# - *'Semi-domestication' without cultivation* (redish square in whitish tile): cases when $n_{H}=5$, $n_{P}>=15$ and $v_{P}<=0.1$.  
# - *'Semi-cultivation' without domestication* (whitish square in blue tile): cases when $n_{H}>=15$, $n_{P}=5$ and $v_{H}<=0.1$.

