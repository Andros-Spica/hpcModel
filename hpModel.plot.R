hpModel.plot <- function(RESULTS, SLEEP=0.05) {
  
  #PARS <- RESULTS$PARS
  TRAJ <- RESULTS$TRAJECTORIES
  TYPES <- RESULTS$TYPES
  t <- RESULTS$END$time
  
  ### set plot positions
  layout(matrix(
    c(1, 1, 2, 2, 1, 1, 3, 3, 4, 5, 6, 6, 7, 8, 9, 9),
    nrow = 4,
    ncol = 4,
    byrow = TRUE
  ))
  
  ### population
  plot(
    TRAJ$H,
    type = 'l',
    col = 'blue',
    main = 'Population',
    ylim = c(0, max(c(TRAJ$H, TRAJ$P), na.rm = T)),
    xlim = c(0, t)
  )
  points(TRAJ$P, type = 'l', col = 'red')
  
  ### Utility
  plot(
    TRAJ$U.ph,
    type = 'l',
    col = 'blue',
    xlim = c(0, t),
    main = 'Utility',
    ylim = c(0, max(c(TRAJ$U.ph, TRAJ$U.hp, TRAJ$Kb.h, TRAJ$Kb.p), na.rm = T))
  )
  points(TRAJ$U.hp, type = 'l', col = 'red')
  points(TRAJ$Kb.h,
         type = 'l',
         col = 'blue',
         lty = 2)
  points(TRAJ$Kb.p,
         type = 'l',
         col = 'red',
         lty = 2)
  
  ### carrying capacity
  plot(
    TRAJ$K.h,
    type = 'l',
    col = 'blue',
    xlim = c(0, t),
    main = 'Carrying capacity',
    ylim = c(0, max(c(TRAJ$K.h, TRAJ$K.p), na.rm = T))
  )
  points(TRAJ$K.p, type = 'l', col = 'red')
  
  ### population types
  barplot(TYPES$H.n[t, ],
          main = 'Humans',
          width = 0.86,
          ylim = c(0, 1))
  barplot(TYPES$P.n[t, ],
          main = 'Plants',
          width = 0.86,
          ylim = c(0, 1))
  
  ### Increments (delta)
  plot(
    TRAJ$IN.h,
    type = 'l',
    col = 'blue',
    xlim = c(0, t),
    main = 'Increments',
    ylim = c(min(c(TRAJ$IN.h, TRAJ$IN.p), na.rm = T), max(c(TRAJ$IN.h, TRAJ$IN.p), na.rm = T))
  )
  points(TRAJ$IN.p, type = 'l', col = 'red')
  abline(h = 0, lty = 2)
  
  # fitness
  plot(TYPES$fit.h[t, ],
       col = 'blue',
       type = 'b',
       main = 'fitness Humans')
  plot(TYPES$fit.p[t, ],
       col = 'red',
       type = 'b',
       main = 'fitness Plants')
  
  # slope in fitness
  plot(
    TRAJ$LM.h,
    type = 'l',
    col = 'blue',
    xlim = c(0, t),
    main = 'fitness slope',
    ylim = c(min(c(TRAJ$LM.h, TRAJ$LM.p), na.rm = T), max(c(TRAJ$LM.h, TRAJ$LM.p), na.rm = T))
  )
  points(TRAJ$LM.p, type = 'l', col = 'red')
  abline(h = 0, lty = 2)
  
  # sleep
  Sys.sleep(SLEEP)
}
