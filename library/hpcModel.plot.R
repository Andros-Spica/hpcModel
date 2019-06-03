hpcModel.plot <-
  function(RESULTS,
           device.sleep = 0.05) {
    
    PARS <- RESULTS$PARS
    TRAJ <- RESULTS$TRAJECTORIES
    TYPES <- RESULTS$TYPES
    t <- RESULTS$END$time
    
    scaleMultiplier = (dev.size('px')[1] / 480)
    
    ### set plot positions
    layout(matrix(
      c(
        1,   1,  1,  9,
        1,   1,  1,  5,
        2,   2,  2,  6,
        3,   3,  3,  7,
        4,   4,  4,  8,
        10, 10, 10, 11,
        12, 12, 12, 12
      ),
      # last row with x axis title
      nrow = 7,
      ncol = 4,
      byrow = TRUE
    ),
    heights = c(1, 1, 1, 1, 1, 0.1, 0.7))
    
    par(mar = c(3, 5, 0.3, 0.3), 
        cex.axis = scaleMultiplier * 1,
        cex.lab = scaleMultiplier * 1.2)
    
    ### 1. population
    plot(
      TRAJ$H,
      type = 'l',
      col = 'blue',
      ylim = c(0, max(
        c(TRAJ$H, TRAJ$P, TRAJ$K.h, TRAJ$K.p), na.rm = T
      )),
      xlim = c(0, t + ceiling(t * 0.35)),
      xlab = '',
      ylab = 'populations'
    )
    points(TRAJ$P, type = 'l', col = 'red')
    
    points(TRAJ$K.h,
           type = 'l',
           col = 'darkblue',
           lty = 2)
    points(TRAJ$K.p,
           type = 'l',
           col = 'darkred',
           lty = 2)
    
    # legend
    legend(
      'right',
      legend = c("Plant (P)",
                 "Human (H)",
                 "Plant (K.p)",
                 "Human (K.h)"),
      col = c('red', 'blue', 'darkred', 'darkblue'),
      lty = c(1, 1, 2, 2),
      cex = scaleMultiplier * 0.8
    )
    
    ### 2. Increments (delta)
    plot(
      TRAJ$IN.h,
      type = 'l',
      col = 'blue',
      xlim = c(0, t + ceiling(t * 0.35)),
      xlab = '',
      ylab = 'growth',
      ylim = c(min(c(
        TRAJ$IN.h, TRAJ$IN.p
      ), na.rm = T), max(c(
        TRAJ$IN.h, TRAJ$IN.p
      ), na.rm = T))
    )
    points(TRAJ$IN.p, type = 'l', col = 'red')
    abline(h = 0, lty = 2)
    
    # legend
    legend(
      'right',
      legend = c("Plant (IN.p)",
                 "Humans (IN.h))"),
      lty = c(1, 1),
      col = c('red', 'blue'),
      cex = scaleMultiplier * 0.8
    )
    
    ### 3. Utility
    plot(
      TRAJ$U.ph,
      type = 'l',
      col = 'red',
      xlim = c(0, t + ceiling(t * 0.35)),
      ylim = c(0, max(
        c(TRAJ$U.ph, TRAJ$U.hp, TRAJ$Kb.h, TRAJ$Kb.p), na.rm = T
      )),
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
    legend(
      'right',
      legend = c(
        "Plant to Humans\n(U.ph)",
        "Humans to Plant\n(Um.hp))",
        "Other to Humans\n(Kb.h)",
        "Other to Plants\n(Kb.p)"
      ),
      col = c('red', 'blue', 'darkred', 'darkblue'),
      lty = c(1, 1, 2, 2),
      cex = scaleMultiplier * 0.8
    )
    
    # 4. slope in fitness
    plot(
      TRAJ$LM.h,
      type = 'l',
      col = 'blue',
      xlim = c(0, t + ceiling(t * 0.35)),
      xlab = 't',
      ylab = 'fitness slope',
      ylim = c(min(c(
        TRAJ$LM.h, TRAJ$LM.p
      ), na.rm = T), max(c(
        TRAJ$LM.h, TRAJ$LM.p
      ), na.rm = T))
    )
    points(TRAJ$LM.p, type = 'l', col = 'red')
    abline(h = 0, lty = 2)
    
    # legend
    legend(
      'right',
      legend = c("Plant (LM.p)",
                 "Humans (LM.h))"),
      lty = c(1, 1),
      col = c('red', 'blue'),
      cex = scaleMultiplier * 0.8
    )
    
    ### 6. & 7. population types
    par(mar = c(1, 3, 1, 0.3), cex.main = scaleMultiplier)
    
    barplot(TYPES$H.n[t,],
            main = 'Human types',
            width = 0.86,
            ylim = c(0, 1))
    barplot(TYPES$P.n[t,],
            main = 'plant types',
            width = 0.86,
            ylim = c(0, 1))
    
    # 8. & 9. fitness
    par(mar = c(2, 3, 1.5, 0.3))
    
    plot(
      TYPES$fit.h[t,],
      col = 'blue',
      type = 'b',
      main = 'fitness humans',
      cex = scaleMultiplier
    )
    plot(
      TYPES$fit.p[t,],
      col = 'red',
      type = 'b',
      main = 'fitness Plants',
      cex = scaleMultiplier
    )
    
    par(mar = c(0, 0, 0, 0))
    
    ### add title plot
    plot(
      c(0, 1),
      c(0, 1),
      ann = F,
      bty = 'n',
      type = 'n',
      xaxt = 'n',
      yaxt = 'n'
    )
    text(
      0.5,
      0.55,
      labels = 'Human-Plant\nCoevolution\nmodel',
      cex = 2 * scaleMultiplier,
      font = 2
    )
    
    ### add t axis title plot
    plot(
      c(0, 1),
      c(0, 1),
      ann = F,
      bty = 'n',
      type = 'n',
      xaxt = 'n',
      yaxt = 'n'
    )
    text(0.6, 0.8, labels = 't', cex = scaleMultiplier)
    
    ### add types axis title plot
    plot(
      c(0, 1),
      c(0, 1),
      ann = F,
      bty = 'n',
      type = 'n',
      xaxt = 'n',
      yaxt = 'n'
    )
    text(0.62, 0.65, labels = '"wild" <---> "domesticated"', cex = scaleMultiplier * 0.8)
    
    ### add parameters values
    plot(
      c(0, 1),
      c(0, 1),
      ann = F,
      bty = 'n',
      type = 'n',
      xaxt = 'n',
      yaxt = 'n'
    )
    
    text(0.5,
         0.75,
         labels = 'Parameter setting:',
         cex = scaleMultiplier * 1.2,
         font = 2)
    
    parNameAndValue1 <- ''
    parNameAndValue2 <- ''
    for (i in 1:length(PARS))
    {
      if (i < 11)
      {
        parNameAndValue1 <-
          paste(parNameAndValue1, names(PARS)[i], ' = ', PARS[[i]], sep = '')
        parNameAndValue1 <- paste0(parNameAndValue1, ', ')
      }
      else
      {
        parNameAndValue2 <-
          paste(parNameAndValue2, names(PARS)[i], ' = ', PARS[[i]], sep = '')
        if (i < length(PARS))
        {
          parNameAndValue2 <- paste0(parNameAndValue2, ', ')
        }
      }
    }
    
    text(0.5, 0.5,
         labels = parNameAndValue1,
         cex = scaleMultiplier)
    text(0.5, 0.25,
         labels = parNameAndValue2,
         cex = scaleMultiplier)
    
    # sleep
    Sys.sleep(device.sleep)
  }
