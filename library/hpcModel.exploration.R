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
  # Utility of individuals of type N
  mU.PnH = 1.7,
  mU.HnP = 1,
  # proportion of mean utility:
  # How less utility has type 1 individuals in relation to type N
  pmU.P1H = 10,                                  
  pmU.H1P = 10,                                   
  # basic resources:
  # population of type 1 that can be sustained by resources independent of HP relationship
  mU.bH1 = 80,                               
  mU.bP1 = 100,                                
  # How less population of type N can be sustained by resources 
  # # independent of HP relationship in relation to type 1
  pmU.bHn = 10,                                
  pmU.bPn = 10, 
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
           length(pmU.P1H) * length(pmU.H1P) * length(pmU.bHn) * length(pmU.bPn))
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
                          for (pmU.P1H.i in pmU.P1H) {
                            for (pmU.H1P.i in pmU.H1P) {
                              for (pmU.bHn.i in pmU.bHn) {
                                for (pmU.bPn.i in pmU.bPn) {
                                  
                                  if (messages) { 
                                    cat(paste('===========================================\nexperiment ', i, ' of ', N, '\n'))
                                    i = i + 1
                                  }
                                  
                                  RUN <- hpcModel.run(
                                    r.H = r.H.i, 
                                    r.P = r.P.i,
                                    mU.bH1 = mU.bH1.i, 
                                    mU.bP1 = mU.bP1.i,
                                    mU.PnH = mU.PnH.i,
                                    mU.HnP = mU.HnP.i,
                                    n.H = n.H.i, 
                                    n.P = n.P.i,
                                    v.H = v.H.i, 
                                    v.P = v.P.i,
                                    iniH = iniH.i, 
                                    iniP = iniP.i,
                                    pmU.P1H = pmU.P1H.i, 
                                    pmU.H1P = pmU.H1P.i,
                                    pmU.bHn = pmU.bHn.i, 
                                    pmU.bPn = pmU.bPn.i,
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
