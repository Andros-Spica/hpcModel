hpcModel.exploration <- function(
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
  r.H = 0.15, 
  r.P = 0.15, 
  # proportion of mean utility per capita:
  # Utility of individuals of type N
  mU.PnH = 1.7,
  mU.HnP = 1,
  # Utility of individuals of type 1
  mU.P1H = 10,                                  
  mU.H1P = 10,                                   
  # basic resources:
  # population of type N that can be sustained by resources independent of HP relationship
  mU.bHn = 10,                                
  mU.bPn = 10, 
  # population of type 1 that can be sustained by resources independent of HP relationship
  mU.bH1 = 80,                               
  mU.bP1 = 100,                                
  # maximum local area to be used by populations (multiplier or scaling effect)
  MaxArea = 200, 
  # settings 
  # simulation flow & data
  maxIt = 20000,
  tol = 6,
  saveTrajectories = FALSE,
  messages = TRUE
)
{
  if (messages) { cat('preparing experiments...') }
  #source('hpModel.run.R')
  
  # acumulabe 
  DF <- data.frame()
  
  if (messages) { 
    i = 1 # experiment index for console messages
    N = (length(r.H) * length(r.P) * length(mU.bH1) * length(mU.bP1) * length(mU.PnH) * length(mU.HnP) *
           length(n.H) * length(n.P) * length(v.H) * length(v.P) * length(iniH) * length(iniP) *
           length(mU.P1H) * length(mU.H1P) * length(mU.bHn) * length(mU.bPn))
    cat(paste('done\niterating parameters values (total number of combinations = ', N, '\n'))
  }
  
  # iterate for all values of every parameter
  for (r.H.i in r.H) {
    for (r.P.i in r.P) {
      for (mU.bH1.i in mU.bH1) {
        for (mU.bP1.i in mU.bP1) {
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
                              for (mU.bHn.i in mU.bHn) {
                                for (mU.bPn.i in mU.bPn) {
                                  
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
                                    mU.bH1 = mU.bH1.i, 
                                    mU.bP1 = mU.bP1.i,
                                    mU.bHn = mU.bHn.i, 
                                    mU.bPn = mU.bPn.i,
                                    MaxArea = MaxArea,
                                    maxIt=maxIt,
                                    tol=tol,
                                    saveTrajectories = saveTrajectories,
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
  
  if (messages) { cat('experiments finished.') }
  
  return(DF)
}
