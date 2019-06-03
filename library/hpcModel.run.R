
# fitness function
Fitness <- function(i, U.BA, K.A)
{
  ((length(i) - i)*(K.A - U.BA) + i*U.BA) / K.A
}


hpcModel.run <- function(
  # intrinsic growth rate 
  r.h = 0.15, 
  r.p = 0.15, 
  # basic resources:
  # population of type 1 that can be sustained by resources independent of HP relationship
  max.h = 80,                               
  max.p = 100,                                
  # Utility of individuals of type N
  Um.ph = 1.7,
  Um.hp = 1,
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
  # simulation flow & data
  maxIt = 20000,
  tol = 6,
  saveTrajectories = FALSE,
  messages = TRUE, 
  # plotting
  plot.preview = FALSE, 
  plot.sleep = 0.05,
  plot.save = FALSE,
  plot.saveEvery = 5,
  plot.directory = "plots/runPlot/",
  plot.fileName = "runPlot")
{
  #-----------SETUP-------------------------------------------------------------------
  
  RESULTS <- list()
  RESULTS$PARS <- list(
    r.h = r.h, 
    r.p = r.p, 
    max.h = max.h,                               
    max.p = max.p,                                
    Um.ph = Um.ph,
    Um.hp = Um.hp,
    n.h = n.h,         
    n.p = n.p,        
    v.h = v.h,
    v.p = v.p,
    iniH = iniH,
    iniP = iniP,
    Ump.ph = Ump.ph,                                  
    Ump.hp = Ump.hp,                                   
    Kmp.h = Kmp.h,                                
    Kmp.p = Kmp.p, 
    MaxArea = MaxArea,
    maxIt = maxIt, 
    tol = tol)
  
  ### declare fixed vectors of utility anb basic resources ===========================
  # utility
  Um.ph.per.type <- seq(Um.ph/Ump.ph, Um.ph, length.out=n.p)
  Um.hp.per.type <- seq(Um.hp/Ump.hp, Um.hp, length.out=n.h)
  # basic resources
  Km.h.per.type <- seq(max.h, max.h/Kmp.h, length.out=n.h)
  Km.p.per.type <- seq(max.p, max.p/Kmp.p, length.out=n.p)
  
  ### declare cumulative vectors =====================================================
  ### declare states (cumulative)
  # population
  H <- rep(NA, maxIt)
  P <- rep(NA, maxIt)
  # set initial conditions 
  H[1] <- iniH    
  P[1] <- iniP
  # total carrying capacity
  K.h <- rep(NA, maxIt)
  K.p <- rep(NA, maxIt)
  # basic resources 
  Kb.h <- rep(NA, maxIt)
  Kb.p <- rep(NA, maxIt)
  # Utility
  U.ph <- rep(NA, maxIt)
  U.hp <- rep(NA, maxIt)
  # fitness of every type
  fit.h <- matrix(NA, maxIt, n.h)
  fit.p <- matrix(NA, maxIt, n.p)
  # population proportions of every type
  H.n <- matrix(NA, maxIt, n.h)
  P.n <- matrix(NA, maxIt, n.p)
  # set initial conditions
  H.n[1,] <- c(1, rep(0, n.h-1))
  P.n[1,] <- c(1, rep(0, n.p-1))
  ### stats (cumulative) -------------------------------------------------------------
  # increment
  IN.h <- rep(NA, maxIt)
  IN.p <- rep(NA, maxIt)
  # fitness slope
  aux.h <- 1:n.h
  aux.p <- 1:n.p
  LM.h <- rep(NA, maxIt)
  LM.p <- rep(NA, maxIt)
  # evolution time 
  evol.h <- 0
  evol.p <- 0

  #-----------ITERATIONS-------------------------------------------------------------------
  if (messages) { cat('running simulation...') }
  #### main loop =====================================================================
  for (t in 1:(maxIt - 1)) 
  {
    ### carrying capacity ----------------------------------------------------------
    # set utilities
    U.ph[t] <- sum(P[t] * P.n[t,] * Um.ph.per.type)
    U.hp[t] <- sum(H[t] * H.n[t,] * Um.hp.per.type)
    #U.hp[t] <- min(sum(H[t] * H.n[t,] * Um.hp.per.type), MaxArea)
    # set basic resources 
    Kb.h[t] <- sum(H.n[t,] * Km.h.per.type)
    Kb.p[t] <- sum(P.n[t,] * Km.p.per.type)
    # update carrying capacity
    K.h[t] <- sum(U.ph[t], Kb.h[t])
    K.p[t] <- min(sum(U.hp[t], Kb.p[t]), MaxArea)
    ### update population types ----------------------------------------------------
    # set fitness
    fit.h[t,] <- Fitness(1:n.h, U.ph[t], K.h[t])
    fit.p[t,] <- Fitness(1:n.p, U.hp[t], K.p[t])
    # set undirected variation 
    H.n[t,] <- H.n[t,] + v.h * ((1/n.h) - H.n[t,])
    P.n[t,] <- P.n[t,] + v.p * ((1/n.p) - P.n[t,])
    # replicator dynamics
    H.n[t+1,] <- fit.h[t,] * H.n[t,] / sum(fit.h[t,] * H.n[t,])
    P.n[t+1,] <- fit.p[t,] * P.n[t,] / sum(fit.p[t,] * P.n[t,])
    # alternative replicator dynamics [generates chaotic regime]
    # H.n[t+1,] <- H.n[t,] + 0.1 * (H.n[t,] * (fit.h - sum(fit.h * H.n[t,])))
    # P.n[t+1,] <- P.n[t,] + 0.1 * (P.n[t,] * (fit.p - sum(fit.p * P.n[t,])))
    ### update populations --------------------------------------------------------- 
    H[t+1] <- (1 + r.h) * H[t] - r.h * (H[t]^2 / K.h[t])
    P[t+1] <- (1 + r.p) * P[t] - r.p * (P[t]^2 / K.p[t])
    ### update stats ---------------------------------------------------------------
    # increments (delta)
    IN.h[t] <- H[t+1] - H[t]
    IN.p[t] <- P[t+1] - P[t]
    # slope of the fitness function
    try(LM.h[t] <- lm(fit.h[t,] ~ aux.h)$coefficients[2])
    try(LM.p[t] <- lm(fit.p[t,] ~ aux.p)$coefficients[2])

    ### running plot --------------------------------------------------------------------
    if(plot.preview || plot.save) 
    {
        RESULTS$END <- list(time = t)
        RESULTS$TRAJECTORIES <- data.frame(
            H = H, P = P, 
            U.ph = U.ph,
            U.hp = U.hp,
            Kb.h = Kb.h,
            Kb.p = Kb.p,
            K.h = K.h,
            K.p = K.p,
            IN.h = IN.h,
            IN.p = IN.p,
            LM.h = LM.h,
            LM.p = LM.p)
        RESULTS$TYPES$H.n <- H.n
        RESULTS$TYPES$P.n <- P.n 
        RESULTS$TYPES$fit.h <- fit.h
        RESULTS$TYPES$fit.p <- fit.p
        
        if (plot.preview)
        {
          hpcModel.plot(RESULTS, device.sleep = plot.sleep)
        }
        
        if (plot.save && t %% plot.saveEvery == 1)
        {
          dir.create(file.path(plot.directory))
          
          tWithZeros <- paste0(paste(rep('0', nchar(maxIt) - nchar(t)), collapse = ''), t)
          
          png(paste(plot.directory, plot.fileName, '_', tWithZeros, '.png', sep = ""), 
              width = 1000, height = 1000)
          hpcModel.plot(RESULTS, device.sleep = plot.sleep)
          dev.off()
        }
    }
    
    # break loop & check evolution -------------------------------------------------
    if (t > 2) 
    {
      # evolution (store the time step of change) 
      if (evol.h == 0) {evol.h <- ifelse(H.n[t-1,1] > H.n[t-1,n.h], 0, t)}
      if (evol.p == 0) {evol.p <- ifelse(P.n[t-1,1] > P.n[t-1,n.p], 0, t)}
      # break loop (reltol method: from optim)
      if(abs(LM.h[t-2] - LM.h[t-1]) < 10^-tol & abs(LM.p[t-2] - LM.p[t-1]) < 10^-tol) {break}
    }
  }
  
  if (messages) { cat('done.\nloading results...') }
  
  # build results list -----------------------------------------------------------------
  
  RESULTS$END <- list(
    evol.h = evol.h, 
    evol.p = evol.p, 
    LM.h = LM.h[t], 
    LM.p = LM.p[t], 
    time = t)
  
  if (saveTrajectories)
  {
    RESULTS$TRAJECTORIES <- data.frame(
      H = H, 
      P = P, 
      U.ph = U.ph,
      U.hp = U.hp,
      Kb.h = Kb.h,
      Kb.p = Kb.p,
      K.h = K.h,
      K.p = K.p,
      IN.h = IN.h,
      IN.p = IN.p,
      LM.h = LM.h, 
      LM.p = LM.p)
    RESULTS$TYPES$H.n <- H.n
    RESULTS$TYPES$P.n <- P.n 
    RESULTS$TYPES$fit.h <- fit.h
    RESULTS$TYPES$fit.p <- fit.p
  }
  
  if (messages) { cat('done.\n') }
  
  return(RESULTS)
}
