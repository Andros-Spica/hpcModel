
# fitness function
Fitness <- function(i, U.BA, K.A)
{
  #if (K.A < U.BA) {K.A = U.BA}
  return(((length(i) - i)*(K.A - U.BA) + i*U.BA) / K.A)
}

# coevolution coefficients:
# between -1, i.e. all pop in type 1, 
# and 1, i.e. all pop in type n
coevo.coef <- function(populationProportions, typesIndexes)
{
  sum(populationProportions * (typesIndexes - 1)) / max(typesIndexes - 1) * 2 - 1
}

hpcModel.run <- function(
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
    iniH = iniH,
    iniP = iniP,
    n.H = n.H,         
    n.P = n.P,        
    v.H = v.H,
    v.P = v.P,
    r.H = r.H, 
    r.P = r.P, 
    mU.PnH = mU.PnH,
    mU.HnP = mU.HnP,
    mU.P1H = mU.P1H,                                  
    mU.H1P = mU.H1P,                                   
    U.bHn = U.bHn,                                
    U.bPn = U.bPn, 
    U.bH1 = U.bH1,                               
    U.bP1 = U.bP1,                                
    MaxArea = MaxArea,
    maxIt = maxIt, 
    tol = tol,
    timing.threshold = timing.threshold
    )
  
  ### declare fixed vectors of utility anb basic resources ===========================
  # utility
  mU.PH.per.type <- seq(mU.P1H, mU.PnH, length.out = n.P)
  mU.HP.per.type <- seq(mU.H1P, mU.HnP, length.out = n.H)
  # basic resources
  U.bH.per.type <- seq(U.bH1, U.bHn, length.out = n.H)
  U.bP.per.type <- seq(U.bP1, U.bPn, length.out = n.P)
  
  ### declare cumulative vectors =====================================================
  ### declare states (cumulative)
  # population
  H <- rep(NA, maxIt)
  P <- rep(NA, maxIt)
  # set initial conditions 
  H[1] <- iniH    
  P[1] <- iniP
  # total carrying capacity
  K.H <- rep(NA, maxIt)
  K.P <- rep(NA, maxIt)
  # basic resources 
  U.bH <- rep(NA, maxIt)
  U.bP <- rep(NA, maxIt)
  # Utility
  U.PH <- rep(NA, maxIt)
  U.HP <- rep(NA, maxIt)
  # fitness of every type
  fitness.H <- matrix(NA, maxIt, n.H)
  fitness.P <- matrix(NA, maxIt, n.P)
  # population proportions of every type
  pop.H <- matrix(NA, maxIt, n.H)
  pop.P <- matrix(NA, maxIt, n.P)
  # set initial conditions
  pop.H[1,] <- c(1, rep(0, n.H-1))
  pop.P[1,] <- c(1, rep(0, n.P-1))
  ### stats (cumulative) -------------------------------------------------------------
  # increment
  d.H <- rep(NA, maxIt)
  d.P <- rep(NA, maxIt)
  # fitness slope
  types.H <- 1:n.H
  types.P <- 1:n.P
  coevo.H <- rep(NA, maxIt)
  coevo.P <- rep(NA, maxIt)
  depend.H <- rep(NA, maxIt)
  depend.P <- rep(NA, maxIt)
  # evolution time 
  timing.H <- 0
  timing.P <- 0

  #-----------ITERATIONS-------------------------------------------------------------------
  if (messages) { cat('running simulation...') }
  #### main loop =====================================================================
  for (t in 1:(maxIt - 1)) 
  {
    ### carrying capacity ----------------------------------------------------------
    # set utilities
    U.PH[t] <- sum(P[t] * pop.P[t,] * mU.PH.per.type)
    #U.HP[t] <- sum(H[t] * pop.H[t,] * mU.HP.per.type)
    U.HP[t] <- min(sum(H[t] * pop.H[t,] * mU.HP.per.type), MaxArea)
    # set basic resources 
    U.bH[t] <- sum(pop.H[t,] * U.bH.per.type)
    U.bP[t] <- sum(pop.P[t,] * U.bP.per.type)
    # update carrying capacity
    K.H[t] <- sum(U.PH[t], U.bH[t])
    K.P[t] <- min(sum(U.HP[t], U.bP[t]), MaxArea)
    ### update population types ----------------------------------------------------
    # set fitness
    fitness.H[t,] <- Fitness(1:n.H, U.PH[t], K.H[t])
    fitness.P[t,] <- Fitness(1:n.P, U.HP[t], K.P[t])
    # set undirected variation 
    pop.H[t,] <- pop.H[t,] + v.H * ((1/n.H) - pop.H[t,])
    pop.P[t,] <- pop.P[t,] + v.P * ((1/n.P) - pop.P[t,])
    # replicator dynamics
    pop.H[t+1,] <- fitness.H[t,] * pop.H[t,] / sum(fitness.H[t,] * pop.H[t,])
    pop.P[t+1,] <- fitness.P[t,] * pop.P[t,] / sum(fitness.P[t,] * pop.P[t,])
    # alternative replicator dynamics [generates chaotic regime]
    # pop.H[t+1,] <- pop.H[t,] + 0.1 * (pop.H[t,] * (fitness.H - sum(fitness.H * pop.H[t,])))
    # pop.P[t+1,] <- pop.P[t,] + 0.1 * (pop.P[t,] * (fitness.P - sum(fitness.P * pop.P[t,])))
    ### update populations --------------------------------------------------------- 
    H[t+1] <- (1 + r.H) * H[t] - r.H * (H[t]^2 / K.H[t])
    P[t+1] <- (1 + r.P) * P[t] - r.P * (P[t]^2 / K.P[t])
    ### update stats ---------------------------------------------------------------
    # increments (delta)
    d.H[t] <- H[t+1] - H[t]
    d.P[t] <- P[t+1] - P[t]
    # slope of the population distribution among types (degree of coevolution)
    coevo.H[t] <- coevo.coef(pop.H[t,], types.H)
    coevo.P[t] <- coevo.coef(pop.P[t,], types.P)
    # slope of the fitness function (degree of dependency)
    # if (all(fitness.H[t,] == NA))
    # {
    #   depend.H[t] <- NA
    # }
    # else
    # {
      try(depend.H[t] <- lm(fitness.H[t,] ~ types.H)$coefficients[2])
    # }
    # if (all(fitness.P[t,] == NA))
    # {
    #   depend.P[t] <- NA
    # }
    # else
    # {
      try(depend.P[t] <- lm(fitness.P[t,] ~ types.P)$coefficients[2])
    #}
    
    ### running plot --------------------------------------------------------------------
    if(plot.preview || plot.save) 
    {
        RESULTS$END <- list(
          time = t, 
          timing.H = timing.H, 
          timing.P = timing.P
          )
        
        RESULTS$TRAJECTORIES <- data.frame(
            H = H, P = P, 
            U.PH = U.PH,
            U.HP = U.HP,
            U.bH = U.bH,
            U.bP = U.bP,
            K.H = K.H,
            K.P = K.P,
            d.H = d.H,
            d.P = d.P,
            coevo.H = coevo.H,
            coevo.P = coevo.P,
            depend.H = depend.H,
            depend.P = depend.P
            )
        
        RESULTS$TYPES$pop.H <- pop.H
        RESULTS$TYPES$pop.P <- pop.P 
        RESULTS$TYPES$fitness.H <- fitness.H
        RESULTS$TYPES$fitness.P <- fitness.P
        
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
      # store the time step of change
      if (timing.H == 0 && !is.na(coevo.H[t]) && coevo.H[t] > timing.threshold) 
      {timing.H <- t}
      
      if (timing.P == 0 && !is.na(coevo.P[t]) &&  coevo.P[t] > timing.threshold)
      {timing.P <- t}
      
      # break loop (reltol method: from optim)
      evolution.stopped.H <- is.na(coevo.H[t]) || abs(coevo.H[t-2] - coevo.H[t-1]) < 10^-tol
      evolution.stopped.P <- is.na(coevo.P[t]) || abs(coevo.P[t-2] - coevo.P[t-1]) < 10^-tol
      evolution.stopped <- (evolution.stopped.H & evolution.stopped.P)
      
      populations.stable <- is.na(H[t]) & is.na(P[t]) || (abs(H[t-2] - H[t-1]) < 10^-tol & abs(P[t-2] - P[t-1]) < 10^-tol)
      
      if (evolution.stopped && populations.stable) {break}
    }
  }
  
  if (messages) { cat('done.\nloading results...') }
  
  # build results list -----------------------------------------------------------------
  
  RESULTS$END <- list(
    H = H[t],
    P = P[t],
    coevo.H = coevo.H[t], 
    coevo.P = coevo.P[t], 
    depend.H = depend.H[t], 
    depend.P = depend.P[t],
    timing.H = timing.H, 
    timing.P = timing.P, 
    time = t,
    adaptativeCost.H = sum(d.H[d.H < 0], na.rm = TRUE),
    adaptativeCost.P = sum(d.P[d.P < 0], na.rm = TRUE)
    )
  
  if (saveTrajectories)
  {
    RESULTS$TRAJECTORIES <- data.frame(
      H = H, 
      P = P, 
      U.PH = U.PH,
      U.HP = U.HP,
      U.bH = U.bH,
      U.bP = U.bP,
      K.H = K.H,
      K.P = K.P,
      d.H = d.H,
      d.P = d.P,
      coevo.H = coevo.H, 
      coevo.P = coevo.P,
      depend.H = depend.H,
      depend.P = depend.P)
    RESULTS$TYPES$pop.H <- pop.H
    RESULTS$TYPES$pop.P <- pop.P 
    RESULTS$TYPES$fitness.H <- fitness.H
    RESULTS$TYPES$fitness.P <- fitness.P
  }
  
  if (messages) { cat('done.\n') }
  
  return(RESULTS)
}
