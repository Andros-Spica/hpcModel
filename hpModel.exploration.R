hpModel.exploration <- function(
  # growth rate 
  r.h = 0.15, 
  r.p = 0.15, 
  # basic resources:
  # population of type 1 that can be sustained by resources independent of HP relationship
  max.h = 80,                               
  max.p = 100,                                
  # utility:
  # Utility of individuals of type N
  Um.ph = 1.7,
  Um.hp = 1,
  # number of types:
  # number of discrete types
  n.h = 10,         
  n.p = 10,        
  # undirected variation 
  v.h = 0.15,
  v.p = 0.15,
  # initial populations
  iniH = 10,
  iniP = 10,
  # proportion of mean utility:
  # How less utility has type 1 individuals in relation to type N
  Ump.ph = 10,                                  
  Ump.hp = 10,                                   
  # How less population of type N can be sustained by resources 
  # # independent of HP relationship in relation to type 1
  Kmp.h = 10,                                
  Kmp.p = 10, 
  # maximum local area to be used by populations (multiplier or scaling effect)
  MaxArea = 200, 
  # settings 
  maxIt=20000,
  tol=6,
  messages = TRUE
)
{
  if (messages) { cat('preparing experiments...') }
  source('hpModel.run.R')
  
  # acumulabe 
  DF <- data.frame()
  
  if (messages) { 
    i = 1 # experiment index for console messages
    N = (length(r.h) * length(r.p) * length(max.h) * length(max.p) * length(Um.ph) * length(Um.hp) *
           length(n.h) * length(n.p) * length(v.h) * length(v.p) * length(iniH) * length(iniP) *
           length(Ump.ph) * length(Ump.hp) * length(Kmp.h) * length(Kmp.p))
    cat(paste('done\niterating parameters values (total number of combinations = ', N, '\n'))
  }
  
  # iterate for all values of every parameter
  for (r.h.i in r.h) {
    for (r.p.i in r.p) {
      for (max.h.i in max.h) {
        for (max.p.i in max.p) {
          for (Um.ph.i in Um.ph) {
            for (Um.hp.i in Um.hp) {
              for (n.h.i in n.h) {
                for (n.p.i in n.p) {
                  for (v.h.i in v.h) {
                    for (v.p.i in v.p) {
                      for (iniH.i in iniH) {
                        for (iniP.i in iniP) {
                          for (Ump.ph.i in Ump.ph) {
                            for (Ump.hp.i in Ump.hp) {
                              for (Kmp.h.i in Kmp.h) {
                                for (Kmp.p.i in Kmp.p) {
                                  
                                  if (messages) { 
                                    cat(paste('===========================================\nexperiment ', i, ' of ', N, '\n'))
                                    i = i + 1
                                  }
                                  
                                  RUN <- hpModel.run(
                                    r.h = r.h.i, 
                                    r.p = r.p.i,
                                    max.h = max.h.i, 
                                    max.p = max.p.i,
                                    Um.ph = Um.ph.i,
                                    Um.hp = Um.hp.i,
                                    n.h = n.h.i, 
                                    n.p = n.p.i,
                                    v.h = v.h.i, 
                                    v.p = v.p.i,
                                    iniH = iniH.i, 
                                    iniP = iniP.i,
                                    Ump.ph = Ump.ph.i, 
                                    Ump.hp = Ump.hp.i,
                                    Kmp.h = Kmp.h.i, 
                                    Kmp.p = Kmp.p.i,
                                    MaxArea = MaxArea,
                                    maxIt=maxIt,
                                    tol=tol
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
