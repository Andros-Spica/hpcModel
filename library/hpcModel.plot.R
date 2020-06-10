hpcModel.plot <-
  function(RESULTS,
           device.sleep = 0.05,
           layout = "A",
           legend.verbose = FALSE,
           scale.override = NULL,
           lwd = 2,
           cex.axis = 1.3,
           cex.lab = 2,
           cex.legend = 1.5,
           cex.rightPlots.titles = 1.2,
           cex.rightPlots.subtitles = 1.2,
           cex.title = 2,
           cex.pars = 1.5) {
    
    PARS <- RESULTS$PARS
    TRAJ <- RESULTS$TRAJECTORIES
    TYPES <- RESULTS$TYPES
    t <- RESULTS$END$time
    timing.H <- RESULTS$END$timing.H
    timing.P <- RESULTS$END$timing.P
    
    scaleMultiplier = (dev.size('px')[1] / 480)
    if (!is.null(scale.override)) { scaleMultiplier = scale.override }
    
    legend.labels <- list(
      pop = c("P", "H", "K.P", "K.H"),
      pop.delta = c("d.P", "d.H"),
      util = c("U.PH", "U.HP", "U.bH", "U.bP"),
      coevo = c("coevo.H", "coevo.P", "depend.H", "depend.P"),
      timing = c("timing.H", "timing.P")
    )
    xaxis.overhead = 0.27 * cex.legend
    legend.timing.horiz = TRUE
    
    if (legend.verbose)
    {
      legend.labels <- list(
        pop = c("Plants (P)",
                "Humans (H)",
                "Plants (K.P)",
                "Humans (K.H)"),
        pop.delta = c("Plants (d.P)",
                      "Humans (d.H)"),
        util = c("Plants to Humans (U.PH)",
                 "Humans to Plants (U.HP))",
                 "Other to Humans (U.bH)",
                 "Other to Plants (U.bP)"),
        coevo = c("Human change (coevo.H)",
                  "Plant change (coevo.P)",
                  "Human dependency (depend.H)",
                  "Plant dependency (depend.P)"),
        timing = c("Time of human change (timing.H)",
                   "Time of plant change (timing.P)")
      )
      xaxis.overhead = 0.9 * cex.legend
      legend.timing.horiz = FALSE
    }
    
    time.axis.limit <- c(0, t + ceiling(t * xaxis.overhead))
    
    ### set plot positions
    if (layout == "A")
    {
      layout(matrix(
        c(
          1,   1,  1,  5,
          2,   2,  2,  6,
          3,   3,  3,  7,
          4,   4,  4,  8,
          9,   9,  9, 10,
          11, 11, 11, 11
        ),
        # last row with x axis title
        nrow = 6,
        ncol = 4,
        byrow = TRUE
      ),
      heights = c(1, 1, 1, 1, (0.1 + 0.07 * legend.verbose) * cex.legend, (0.7 - 0.05 * legend.verbose)))
    }
    else if (layout == "B")
    {
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
      heights = c(1, 1, 1, 1, 1, (0.1 + 0.07 * legend.verbose) * cex.legend, (0.7 - 0.05 * legend.verbose)))
    }
    else { errorCondition('Select a valid layout: "A" or "B"') }
    
    par(mar = c(5, 7, 2, 0.3), 
        lwd = lwd,
        cex.axis = scaleMultiplier * cex.axis,
        cex.lab = scaleMultiplier * cex.lab)
    
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
    { abline(v = timing.H, col = 'dodgerblue', lty = 2) }
    if (timing.P != 0)
    { abline(v = timing.P, col = 'deeppink', lty = 2) }
    
    # legend
    legend(
      'right',
      legend = legend.labels$pop,
      col = c('red', 'blue', 'darkred', 'darkblue'),
      lty = c(1, 1, 2, 2),
      cex = scaleMultiplier * cex.legend
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
    
    lines(c(0, t), c(0, 0), lty = 2)
    
    if (timing.H != 0)
    { abline(v = timing.H, col = 'dodgerblue', lty = 2) }
    if (timing.P != 0)
    { abline(v = timing.P, col = 'deeppink', lty = 2) }
    
    # legend
    legend(
      'right',
      legend = legend.labels$pop.delta,
      lty = c(1, 1),
      col = c('red', 'blue'),
      cex = scaleMultiplier * cex.legend
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
    { abline(v = timing.H, col = 'dodgerblue', lty = 2) }
    if (timing.P != 0)
    { abline(v = timing.P, col = 'deeppink', lty = 2) }
    
    # legend
    legend(
      'right',
      legend = legend.labels$util,
      col = c('blue', 'red', 'darkblue', 'darkred'),
      lty = c(1, 1, 2, 2),
      cex = scaleMultiplier * cex.legend
    )
    
    # 4. Coevolution and dependency coefficients
    plot(
      TRAJ$coevo.H,
      type = 'l',
      col = 'blue',
      xlab = '',
      xlim = time.axis.limit,
      ylab = 'coevolution',
      ylim = c(min(c(
        TRAJ$coevo.H, TRAJ$coevo.P, TRAJ$depend.H, TRAJ$depend.P, -1E-6
      ), na.rm = T), max(c(
        TRAJ$coevo.H, TRAJ$coevo.P, TRAJ$depend.H, TRAJ$depend.P, 1E-6
      ), na.rm = T))
    )
    points(TRAJ$coevo.P, type = 'l', col = 'red')
    points(TRAJ$depend.H, type = 'l', lty = 2, col = 'darkblue')
    points(TRAJ$depend.P, type = 'l', lty = 2, col = 'darkred')
    
    lines(c(0, t), c(0, 0), lty = 2)
    
    if (timing.H != 0)
    { abline(v = timing.H, col = 'dodgerblue', lty = 2) }
    if (timing.P != 0)
    { abline(v = timing.P, col = 'deeppink', lty = 2) }
    
    # legend
    legend(
      'right',
      legend = legend.labels$coevo,
      lty = c(1, 1, 2, 2),
      col = c('blue', 'red', 'darkblue', 'darkred'),
      cex = scaleMultiplier * cex.legend
    )
    
    ### 6. & 7. population types
    par(mar = c(2, 3, 2, 0.3), cex.main = scaleMultiplier * cex.rightPlots.titles)
    
    barplot(TYPES$pop.H[t,],
            main = 'population per type',
            names.arg = as.character(1:PARS$n.H),
            width = 0.86,
            ylim = c(0, 1))
    text(PARS$n.H/2, 0.9, labels = 'humans', 
         font = 4, cex = scaleMultiplier * cex.rightPlots.subtitles)
    
    par(mar = c(3, 3, 1, 0.3))
    barplot(TYPES$pop.P[t,],
            names.arg = as.character(1:PARS$n.P),
            width = 0.86,
            ylim = c(0, 1),
            xlab = '')
    text(PARS$n.P/2, 0.9, labels = 'plants', 
         font = 4, cex = scaleMultiplier * cex.rightPlots.subtitles)
    
    # 8. & 9. fitness
    par(mar = c(2, 3, 2, 0.3))
    
    plot(
      TYPES$fitness.H[t,],
      col = 'blue',
      type = 'b',
      bty= 'n',
      main = 'fitness per type',
      ylim = c(0, max(1E-6, TYPES$fitness.H[t,], na.rm = T)),
      cex = scaleMultiplier,
      xlab = ''
    )
    text(PARS$n.H/2, max(1E-6, TYPES$fitness.H[t,], na.rm = T), labels = 'humans', 
         font = 4, cex = scaleMultiplier * cex.rightPlots.subtitles)
    
    par(mar = c(3, 3, 1, 0.3))
    plot(
      TYPES$fitness.P[t,],
      col = 'red',
      type = 'b',
      bty= 'n',
      ylim = c(0, max(1E-6, TYPES$fitness.P[t,], na.rm = T)),
      cex = scaleMultiplier,
      xlab = ''
    )
    text(PARS$n.P/2, max(1E-6, TYPES$fitness.P[t,], na.rm = T), labels = 'plants', 
         font = 4, cex = scaleMultiplier * cex.rightPlots.subtitles)
    
    par(mar = c(0, 0, 0, 0))
    
    if (layout == "B")
    {
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
        cex = scaleMultiplier * cex.title,
        font = 2
      )
    }
    
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
    text(0.6, 0.8, labels = 'time', cex = scaleMultiplier * cex.lab)
    
    legend(
      -0.02, 1.5,
      legend = legend.labels$timing,
      lty = c(2, 2),
      col = c('dodgerblue', 'deeppink'),
      cex = scaleMultiplier * cex.legend,
      horiz = legend.timing.horiz,
      bty = "n"
    )
    
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
    text(0.6, 0.65, labels = 'types', cex = scaleMultiplier * cex.lab)
    
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
         cex = scaleMultiplier * cex.pars,
         font = 2)
    
    parNameAndValue1 <- ''
    parNameAndValue2 <- ''
    parNameAndValue3 <- ''
    for (i in 1:length(PARS))
    {
      if (i < 9)
      {
        parNameAndValue1 <-
          paste(parNameAndValue1, names(PARS)[i], ' = ', PARS[[i]], sep = '')
        parNameAndValue1 <- paste0(parNameAndValue1, ', ')
      }
      else
      {
        if (i < 15)
        {
          parNameAndValue2 <-
            paste(parNameAndValue2, names(PARS)[i], ' = ', PARS[[i]], sep = '')
          if (i < length(PARS))
          {
            parNameAndValue2 <- paste0(parNameAndValue2, ', ')
          }
        }
        else
        {
          parNameAndValue3 <-
            paste(parNameAndValue3, names(PARS)[i], ' = ', PARS[[i]], sep = '')
          if (i < length(PARS))
          {
            parNameAndValue3 <- paste0(parNameAndValue3, ', ')
          }
        }
      }
    }
    
    text(0.5, 0.5,
         labels = parNameAndValue1,
         cex = scaleMultiplier * cex.pars)
    text(0.5, 0.3,
         labels = parNameAndValue2,
         cex = scaleMultiplier * cex.pars)
    text(0.5, 0.1,
         labels = parNameAndValue3,
         cex = scaleMultiplier * cex.pars)
    
    # sleep
    Sys.sleep(device.sleep)
  }
