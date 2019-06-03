hpModel.plot <- function(RESULTS, SLEEP=0.05) {
  
  PARS <- RESULTS$PARS
  TRAJ <- RESULTS$TRAJECTORIES
  TYPES <- RESULTS$TYPES
  t <- RESULTS$END$time
  
  ### set plot positions
  layout(matrix(
    c(1, 1, 1, 9, 1, 1, 1, 5, 2, 2, 2, 6, 3, 3, 3, 7, 4, 4, 4, 8),
    nrow = 5,
    ncol = 4,
    byrow = TRUE
  ))
  
  par(mar = c(3, 4, 0.3, 0.3), lab.cex = 1.2)
  
  ### 1. population
  plot(
    TRAJ$H,
    type = 'l',
    col = 'blue',
    ylim = c(0, max(c(TRAJ$H, TRAJ$P, TRAJ$K.h, TRAJ$K.p), na.rm = T)),
    xlim = c(0, t),
    xlab = '',
    ylab = 'populations'
  )
  points(TRAJ$P, type = 'l', col = 'red')
  
  points(TRAJ$K.h, type = 'l', col = 'darkblue', lty = 2)
  points(TRAJ$K.p, type = 'l', col = 'darkred', lty = 2)
  
  # legend
  legend('bottomright', legend=c("Plant", "Human", "Plant (K)", "Human (K)"),
         col=c('red', 'blue', 'darkred', 'darkblue'), 
         lty=c(1, 1, 2, 2), cex=0.8)
  
  ### 2. Increments (delta)
  plot(
    TRAJ$IN.h,
    type = 'l',
    col = 'blue',
    xlim = c(0, t),
    xlab = '',
    ylab = 'growth',
    ylim = c(min(c(TRAJ$IN.h, TRAJ$IN.p), na.rm = T), max(c(TRAJ$IN.h, TRAJ$IN.p), na.rm = T))
  )
  points(TRAJ$IN.p, type = 'l', col = 'red')
  abline(h = 0, lty = 2)
  
  ### 3. Utility
  plot(
    TRAJ$U.ph,
    type = 'l',
    col = 'red',
    xlim = c(0, t),
    ylim = c(0, max(c(TRAJ$U.ph, TRAJ$U.hp, TRAJ$Kb.h, TRAJ$Kb.p), na.rm = T)),
    xlab = '',
    ylab = 'utility'
  )
  points(TRAJ$U.hp, type = 'l', col = 'blue')
  points(TRAJ$Kb.h,
         type = 'l',
         col = 'blue',
         lty = 2)
  points(TRAJ$Kb.p,
         type = 'l',
         col = 'red',
         lty = 2)
  
  # legend
  legend('bottomright', legend=c("Plant -> Humans (U.ph)", "Humans -> Plant (Um.hp))", "Plant (K)", "Human (K)"),
         col=c('red', 'blue', 'darkred', 'darkblue'), 
         lty=c(1, 1, 2, 2), cex=0.8)
  
  # 4. slope in fitness
  plot(
    TRAJ$LM.h,
    type = 'l',
    col = 'blue',
    xlim = c(0, t),
    xlab = 't',
    ylab = 'fitness slope',
    ylim = c(min(c(TRAJ$LM.h, TRAJ$LM.p), na.rm = T), max(c(TRAJ$LM.h, TRAJ$LM.p), na.rm = T))
  )
  points(TRAJ$LM.p, type = 'l', col = 'red')
  abline(h = 0, lty = 2)
  
  # add legend
  
  ### 6. & 7. population types
  par(mar = c(1, 3, 1, 0.3))
  
  barplot(TYPES$H.n[t, ],
          main = 'Human types',
          width = 0.86,
          ylim = c(0, 1)
          #,names.arg = 1:PARS$n.h
          )
  barplot(TYPES$P.n[t, ],
          main = 'plant types',
          width = 0.86,
          ylim = c(0, 1)
          #,names.arg = character(1:PARS$n.p)
          )
  
  # 8. & 9. fitness
  par(mar = c(2, 3, 1.5, 0.3))
  
  plot(TYPES$fit.h[t, ],
       col = 'blue',
       type = 'b',
       main = 'fitness humans')
  plot(TYPES$fit.p[t, ],
       col = 'red',
       type = 'b',
       main = 'fitness Plants')
  
  par(mar = c(0, 0, 0, 0))
  
  ### add title plot
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(0.5, 0.55, labels = 'Human-Plant\nCoevolution\nmodel', cex = 2, font = 2)

  # sleep
  Sys.sleep(SLEEP)
}
