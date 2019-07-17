hpcModel.plot <-
  function(RESULTS,
           device.sleep = 0.05,
           scale.override = NULL) {
    
    PARS <- RESULTS$PARS
    TRAJ <- RESULTS$TRAJECTORIES
    TYPES <- RESULTS$TYPES
    t <- RESULTS$END$time
    timing.H <- RESULTS$END$timing.H
    timing.P <- RESULTS$END$timing.P
    
    scaleMultiplier = (dev.size('px')[1] / 480)
    if (!is.null(scale.override)) { scaleMultiplier = scale.override }
    
    time.axis.limit <- c(0, t + ceiling(t * 0.4))
    
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
    
    par(mar = c(3, 5, 0.4, 0.3), 
        cex.axis = scaleMultiplier * 1,
        cex.lab = scaleMultiplier * 1.2)
    
    ### 1. population
    plot(
      TRAJ$H,
      type = 'l',
      col = 'blue',
      ylim = c(0, max(
        c(TRAJ$H, TRAJ$P, TRAJ$K.H, TRAJ$K.P), na.rm = T
      )),
      xlim = time.axis.limit,
      xlab = '',
      ylab = 'populations'
    )
    points(TRAJ$P, type = 'l', col = 'red')
    
    points(TRAJ$K.H,
           type = 'l',
           col = 'darkblue',
           lty = 2)
    points(TRAJ$K.P,
           type = 'l',
           col = 'darkred',
           lty = 2)
    
    if (timing.H != 0)
    { abline(v = timing.H, col = 'cyan', lty = 2) }
    if (timing.P != 0)
    { abline(v = timing.P, col = 'pink', lty = 2) }
    
    # legend
    legend(
      'right',
      legend = c("Plants (P)",
                 "Humans (H)",
                 "Plants (K.P)",
                 "Humans (K.H)"),
      col = c('red', 'blue', 'darkred', 'darkblue'),
      lty = c(1, 1, 2, 2),
      cex = scaleMultiplier * 0.8
    )
    
    ### 2. Increments (delta)
    plot(
      TRAJ$d.H,
      type = 'l',
      col = 'blue',
      xlim = time.axis.limit,
      xlab = '',
      ylab = 'growth',
      ylim = c(min(c(
        TRAJ$d.H[!is.infinite(TRAJ$d.H)], TRAJ$d.P[!is.infinite(TRAJ$d.P)], -1E-6
      ), na.rm = T), max(c(
        TRAJ$d.H[!is.infinite(TRAJ$d.H)], TRAJ$d.P[!is.infinite(TRAJ$d.P)], 1E-6
      ), na.rm = T))
    )
    points(TRAJ$d.P, type = 'l', col = 'red')
    
    abline(h = 0, lty = 2)
    
    if (timing.H != 0)
    { abline(v = timing.H, col = 'cyan', lty = 2) }
    if (timing.P != 0)
    { abline(v = timing.P, col = 'pink', lty = 2) }
    
    # legend
    legend(
      'right',
      legend = c("Plants (d.P)",
                 "Humans (d.H))"),
      lty = c(1, 1),
      col = c('red', 'blue'),
      cex = scaleMultiplier * 0.8
    )
    
    ### 3. Utility
    plot(
      TRAJ$U.PH,
      type = 'l',
      col = 'blue',
      xlim = time.axis.limit,
      ylim = c(0, max(
        c(TRAJ$U.PH, TRAJ$U.HP, TRAJ$U.bH, TRAJ$U.bP, 1E-6), na.rm = T
      )),
      xlab = '',
      ylab = 'utility'
    )
    points(TRAJ$U.HP, type = 'l', col = 'red')
    points(TRAJ$U.bH,
           type = 'l',
           col = 'darkblue',
           lty = 2)
    points(TRAJ$U.bP,
           type = 'l',
           col = 'darkred',
           lty = 2)
    
    if (timing.H != 0)
    { abline(v = timing.H, col = 'cyan', lty = 2) }
    if (timing.P != 0)
    { abline(v = timing.P, col = 'pink', lty = 2) }
    
    # legend
    legend(
      'right',
      legend = c(
        "Plants to Humans (U.PH)",
        "Humans to Plants (U.HP))",
        "Other to Humans (U.bH)",
        "Other to Plants (U.bP)"
      ),
      col = c('blue', 'red', 'darkblue', 'darkred'),
      lty = c(1, 1, 2, 2),
      cex = scaleMultiplier * 0.8
    )
    
    # 4. Coevolution and dependency coefficients
    plot(
      TRAJ$coevo.H,
      type = 'l',
      col = 'blue',
      xlim = time.axis.limit,
      xlab = 't',
      ylab = 'coevolution\nand dependency',
      ylim = c(min(c(
        TRAJ$coevo.H, TRAJ$coevo.P, TRAJ$depend.H, TRAJ$depend.P, -1E-6
      ), na.rm = T), max(c(
        TRAJ$coevo.H, TRAJ$coevo.P, TRAJ$depend.H, TRAJ$depend.P, 1E-6
      ), na.rm = T))
    )
    points(TRAJ$coevo.P, type = 'l', col = 'red')
    points(TRAJ$depend.H, type = 'l', lty = 2, col = 'darkblue')
    points(TRAJ$depend.P, type = 'l', lty = 2, col = 'darkred')
    
    abline(h = 0, lty = 2)
    
    if (timing.H != 0)
    { abline(v = timing.H, col = 'cyan', lty = 2) }
    if (timing.P != 0)
    { abline(v = timing.P, col = 'pink', lty = 2) }
    
    # legend
    legend(
      'right',
      legend = c("Human change (coevo.H))",
                 "Plant change (coevo.P)",
                 "Human dependency (depend.H))",
                 "Plant dependency (depend.P)"),
      lty = c(1, 1, 2, 2),
      col = c('blue', 'red', 'darkblue', 'darkred'),
      cex = scaleMultiplier * 0.8
    )
    
    ### 6. & 7. population types
    par(mar = c(1, 3, 1, 0.3), cex.main = scaleMultiplier)
    
    barplot(TYPES$pop.H[t,],
            main = 'human types',
            width = 0.86,
            ylim = c(0, 1))
    barplot(TYPES$pop.P[t,],
            main = 'plant types',
            width = 0.86,
            ylim = c(0, 1))
    
    # 8. & 9. fitness
    par(mar = c(2, 3, 1.5, 0.3))
    
    plot(
      TYPES$fitness.H[t,],
      col = 'blue',
      type = 'b',
      main = 'fitness humans',
      ylim = c(0, max(1E-6, TYPES$fitness.H[t,], na.rm = T)),
      cex = scaleMultiplier
    )
    plot(
      TYPES$fitness.P[t,],
      col = 'red',
      type = 'b',
      main = 'fitness plants',
      ylim = c(0, max(1E-6, TYPES$fitness.P[t,], na.rm = T)),
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
