hpcModel.exploration <- function(
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
  # maximum local area to be used by plant population
  MaxArea = 200, 
  # settings 
  # simulation flow & data
  maxIt = 5000,
  tol = 6,
  timing.threshold = 0.5,
  messages = TRUE
)
{
  if (messages) { cat('preparing experiments...') }
  #source('hpModel.run.R')
  
  # acumulabe 
  DF <- data.frame()
  
  if (messages) { 
    i = 1 # experiment index for console messages
    N = (length(r.H) * length(r.P) * length(U.bH1) * length(U.bP1) * length(mU.PnH) * length(mU.HnP) *
           length(n.H) * length(n.P) * length(v.H) * length(v.P) * length(iniH) * length(iniP) *
           length(mU.P1H) * length(mU.H1P) * length(U.bHn) * length(U.bPn) * length(MaxArea))
    cat(paste('done\niterating parameters values (total number of combinations = ', N, '\n'))
  }
  
  # iterate for all values of every parameter
  for (r.H.i in r.H) {
    for (r.P.i in r.P) {
      for (U.bH1.i in U.bH1) {
        for (U.bP1.i in U.bP1) {
          for (mU.PnH.i in mU.PnH) {
            for (mU.HnP.i in mU.HnP) {
              for (n.H.i in n.H) {
                for (n.P.i in n.P) {
                  for (v.H.i in v.H) {
                    for (v.P.i in v.P) {
                      for (iniH.i in iniH) {
                        for (iniP.i in iniP) {
                          for (mU.P1H.i in mU.P1H) {
                            for (mU.H1P.i in mU.H1P) {
                              for (U.bHn.i in U.bHn) {
                                for (U.bPn.i in U.bPn) {
                                  for (MaxArea.i in MaxArea) {
                                    
                                    if (messages) { 
                                      cat(paste('===========================================\nexperiment ', i, ' of ', N, '\n'))
                                      i = i + 1
                                    }
                                    
                                    RUN <- hpcModel.run(
                                      iniH = iniH.i, 
                                      iniP = iniP.i,
                                      r.H = r.H.i, 
                                      r.P = r.P.i,
                                      n.H = n.H.i, 
                                      n.P = n.P.i,
                                      v.H = v.H.i, 
                                      v.P = v.P.i,
                                      mU.PnH = mU.PnH.i,
                                      mU.HnP = mU.HnP.i,
                                      mU.P1H = mU.P1H.i, 
                                      mU.H1P = mU.H1P.i,
                                      U.bH1 = U.bH1.i, 
                                      U.bP1 = U.bP1.i,
                                      U.bHn = U.bHn.i, 
                                      U.bPn = U.bPn.i,
                                      MaxArea = MaxArea.i,
                                      # fixed settings
                                      maxIt = maxIt,
                                      tol = tol,
                                      timing.threshold = timing.threshold,
                                      saveTrajectories = FALSE,
                                      messages = messages,
                                      plot.preview = FALSE, 
                                      plot.save = FALSE
                                    )
                                    
                                    DF <- rbind(DF, c(RUN$PARS, RUN$END))
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  if (messages) { cat('experiments finished.') }
  
  return(DF)
}
