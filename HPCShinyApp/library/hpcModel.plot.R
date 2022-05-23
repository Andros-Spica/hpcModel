#### CODE STYLE NOTES: ####
# - All functions names with verbs
# - Main (public) functions inside the main pseudo class "hpcModel" using dot (.) as separator
# - higher-level composite objects in uppercase
# - function arguments using underscore (_) as separator
# - local variables and functions using camel case
# - some variables here use conventional names in R graphics, like "cex" for font size in the "par" function

hpcModel.plot <-
  function(RESULTS,
           device_sleep = 0.05,
           layout = "A",
           legend_verbose = TRUE,
           scale_override = NULL,
           lwd = 2,
           x_axis_overhead = 0.25,
           # font sizes
           cex_axis = 0.8,
           cex_lab = 1.5,
           cex_legend = 0.8,
           cex_rightPlots_titles = 1,
           cex_rightPlots_subtitles = 0.8,
           cex_title = 1.5,
           cex_parameters_title = 0.8,
           cex_parameters = 0.8) {
    
    PARAMETERS <- RESULTS$PARAMETERS
    TRAJECTORIES <- RESULTS$TRAJECTORIES
    TYPES <- RESULTS$TYPES
    time_end <- RESULTS$END$time_end
    timing_humans <- RESULTS$END$timing_humans
    timing_plants <- RESULTS$END$timing_plants
    
    scaleMultiplier = (dev.size('px')[1] / 480)
    if (!is.null(scale_override)) { scaleMultiplier = scale_override }
    
    # legend_verbose = FALSE use math notation in legends
    legendLabels <- list(
      population = c("P", "H", "K_P", "K_H"),
      populationDelta = c("d_P", "d_H"),
      utilitities = c("U_PH", "U_HP", "U_bH", "U_bP"),
      coefficients = c("coevo_H", "coevo_P", "depend_H", "depend_P"),
      timing = c("timing_H", "timing_P")
    )
    
    legendTimingHorizontal = TRUE
    
    # legend_verbose = TRUE use code variable names in legends
    if (legend_verbose)
    {
      legendLabels <- list(
        population = c("Plants",
                       "Humans",
                       "Plants (K)",
                       "Humans (K)"),
        populationDelta = c("Plants",
                            "Humans"),
        utilitities = c("Plants to Humans",
                        "Humans to Plants",
                        "Other to Humans",
                        "Other to Plants"),
        coefficients = c("Humans (coevo)",
                         "Plants (coevo)",
                         "Humans (depend)",
                         "Plants (depend)"),
        timing = c("Humans (timing)",
                   "Plants (timing)")
      )
      
      legendTimingHorizontal = TRUE
    }
    
    timeAxisLimit <- c(0, time_end + ceiling(time_end * x_axis_overhead))
    
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
      heights = c(1, 1, 1, 1, (0.1 + 0.07 * legend_verbose) * cex_legend, (0.7 - 0.05 * legend_verbose)))
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
      heights = c(1, 1, 1, 1, 1, (0.1 + 0.07 * legend_verbose) * cex_legend, (0.7 - 0.05 * legend_verbose)))
    }
    else { errorCondition('Select a valid layout: "A" or "B"') }
    
    par(mar = c(1, 6, 2, 0.3), 
        lwd = lwd,
        cex.axis = scaleMultiplier * cex_axis,
        cex.lab = scaleMultiplier * cex_lab)
    
    ### 1. population
    plot(
      TRAJECTORIES$humans,
      type = 'l',
      col = 'blue',
      ylim = c(0, max(
        c(TRAJECTORIES$humans, TRAJECTORIES$plants, TRAJECTORIES$carrying_capacity_humans, TRAJECTORIES$carrying_capacity_plants), na.rm = T
      )),
      xlim = timeAxisLimit,
      xlab = '',
      xaxt = 'n',
      ylab = 'populations'
    )
    points(TRAJECTORIES$plants, type = 'l', col = 'red')
    
    points(TRAJECTORIES$carrying_capacity_humans,
           type = 'l',
           col = 'darkblue',
           lty = 2)
    points(TRAJECTORIES$carrying_capacity_plants,
           type = 'l',
           col = 'darkred',
           lty = 2)
    
    if (timing_humans != 0)
    { abline(v = timing_humans, col = 'dodgerblue', lty = 2) }
    if (timing_plants != 0)
    { abline(v = timing_plants, col = 'deeppink', lty = 2) }
    
    # legend
    legend(
      'right',
      legend = legendLabels$population,
      col = c('red', 'blue', 'darkred', 'darkblue'),
      lty = c(1, 1, 2, 2),
      cex = scaleMultiplier * cex_legend
    )
    
    ### 2. Increments (delta)
    par(mar = c(1, 6, 1, 0.3))
    
    plot(
      TRAJECTORIES$population_change_humans,
      type = 'l',
      col = 'blue',
      xlim = timeAxisLimit,
      xlab = '',
      xaxt = 'n',
      ylab = 'growth',
      ylim = c(min(c(
        TRAJECTORIES$population_change_humans[!is.infinite(TRAJECTORIES$population_change_humans)], TRAJECTORIES$population_change_plants[!is.infinite(TRAJECTORIES$population_change_plants)], -1E-6
      ), na.rm = T), max(c(
        TRAJECTORIES$population_change_humans[!is.infinite(TRAJECTORIES$population_change_humans)], TRAJECTORIES$population_change_plants[!is.infinite(TRAJECTORIES$population_change_plants)], 1E-6
      ), na.rm = T))
    )
    points(TRAJECTORIES$population_change_plants, type = 'l', col = 'red')
    
    lines(c(0, time_end), c(0, 0), lty = 2)
    
    if (timing_humans != 0)
    { abline(v = timing_humans, col = 'dodgerblue', lty = 2) }
    if (timing_plants != 0)
    { abline(v = timing_plants, col = 'deeppink', lty = 2) }
    
    # legend
    legend(
      'right',
      legend = legendLabels$populationDelta,
      lty = c(1, 1),
      col = c('red', 'blue'),
      cex = scaleMultiplier * cex_legend
    )
    
    ### 3. Utility
    
    plot(
      TRAJECTORIES$utility_plants_to_humans,
      type = 'l',
      col = 'blue',
      xlim = timeAxisLimit,
      ylim = c(0, max(
        c(TRAJECTORIES$utility_plants_to_humans, TRAJECTORIES$utility_humans_to_plants, TRAJECTORIES$utility_other_to_humans, TRAJECTORIES$utility_other_to_plants, 1E-6), na.rm = T
      )),
      xlab = '',
      xaxt = 'n',
      ylab = 'utility'
    )
    points(TRAJECTORIES$utility_humans_to_plants, type = 'l', col = 'red')
    points(TRAJECTORIES$utility_other_to_humans,
           type = 'l',
           col = 'darkblue',
           lty = 2)
    points(TRAJECTORIES$utility_other_to_plants,
           type = 'l',
           col = 'darkred',
           lty = 2)
    
    if (timing_humans != 0)
    { abline(v = timing_humans, col = 'dodgerblue', lty = 2) }
    if (timing_plants != 0)
    { abline(v = timing_plants, col = 'deeppink', lty = 2) }
    
    # legend
    legend(
      'right',
      legend = legendLabels$utilitities,
      col = c('blue', 'red', 'darkblue', 'darkred'),
      lty = c(1, 1, 2, 2),
      cex = scaleMultiplier * cex_legend
    )
    
    # 4. Coevolution and dependency coefficients
    par(mar = c(2, 6, 1, 0.3))
    
    plot(
      TRAJECTORIES$coevolution_coefficient_humans,
      type = 'l',
      col = 'blue',
      xlab = '',
      xlim = timeAxisLimit,
      ylab = 'coevolution',
      ylim = c(min(c(
        TRAJECTORIES$coevolution_coefficient_humans, TRAJECTORIES$coevolution_coefficient_plants, TRAJECTORIES$dependency_coefficient_humans, TRAJECTORIES$dependency_coefficient_plants, -1E-6
      ), na.rm = T), max(c(
        TRAJECTORIES$coevolution_coefficient_humans, TRAJECTORIES$coevolution_coefficient_plants, TRAJECTORIES$dependency_coefficient_humans, TRAJECTORIES$dependency_coefficient_plants, 1E-6
      ), na.rm = T))
    )
    points(TRAJECTORIES$coevolution_coefficient_plants, type = 'l', col = 'red')
    points(TRAJECTORIES$dependency_coefficient_humans, type = 'l', lty = 2, col = 'darkblue')
    points(TRAJECTORIES$dependency_coefficient_plants, type = 'l', lty = 2, col = 'darkred')
    
    lines(c(0, time_end), c(0, 0), lty = 3)
    
    if (timing_humans != 0)
    { abline(v = timing_humans, col = 'dodgerblue', lty = 2) }
    if (timing_plants != 0)
    { abline(v = timing_plants, col = 'deeppink', lty = 2) }
    
    # legend
    legend(
      'right',
      legend = legendLabels$coefficients,
      lty = c(1, 1, 2, 2),
      col = c('blue', 'red', 'darkblue', 'darkred'),
      cex = scaleMultiplier * cex_legend
    )
    
    ### 6. & 7. population types
    par(mar = c(2, 3, 2, 0.3), cex.main = scaleMultiplier * cex_rightPlots_titles)
    
    barplot(RESULTS$TYPES$type_proportions_humans[time_end,],
            main = 'proportion per type',
            names.arg = as.character(1:PARAMETERS$number_types_humans),
            width = 0.86,
            ylim = c(0, 1))
    text(PARAMETERS$number_types_humans/2, 0.9, labels = 'humans', 
         font = 4, cex = scaleMultiplier * cex_rightPlots_subtitles)
    
    par(mar = c(3, 3, 1, 0.3))
    barplot(RESULTS$TYPES$type_proportions_plants[time_end,],
            names.arg = as.character(1:PARAMETERS$number_types_plants),
            width = 0.86,
            ylim = c(0, 1),
            xlab = '')
    text(PARAMETERS$number_types_plants/2, 0.9, labels = 'plants', 
         font = 4, cex = scaleMultiplier * cex_rightPlots_subtitles)
    
    # 8. & 9. fitness
    par(mar = c(2, 3, 2, 0.3))
    
    plot(
      TYPES$fitness_humans[time_end,],
      col = 'blue',
      type = 'b',
      bty= 'n',
      main = 'fitness per type',
      ylim = c(0, max(1E-6, TYPES$fitness_humans[time_end,], na.rm = T)),
      cex = scaleMultiplier,
      xlab = '',
      ylab = ''
    )
    text(PARAMETERS$number_types_humans/2, max(1E-6, TYPES$fitness_humans[time_end,], na.rm = T), labels = 'humans', 
         font = 4, cex = scaleMultiplier * cex_rightPlots_subtitles)
    
    par(mar = c(3, 3, 1, 0.3))
    plot(
      TYPES$fitness_plants[time_end,],
      col = 'red',
      type = 'b',
      bty= 'n',
      ylim = c(0, max(1E-6, TYPES$fitness_plants[time_end,], na.rm = T)),
      cex = scaleMultiplier,
      xlab = '',
      ylab = ''
    )
    text(PARAMETERS$number_types_plants/2, max(1E-6, TYPES$fitness_plants[time_end,], na.rm = T), labels = 'plants', 
         font = 4, cex = scaleMultiplier * cex_rightPlots_subtitles)
    
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
        cex = scaleMultiplier * cex_title,
        font = 2
      )
    }
    
    ### add x axis (time)
    
    plot(
      c(0, 1),
      c(0, 1),
      ann = F,
      bty = 'n',
      type = 'n',
      xaxt = 'n',
      yaxt = 'n'
    )
    text(0.6, 0.8, labels = 'time', cex = scaleMultiplier * cex_lab)

    legend(
      -0.02, 1.2,
      legend = legendLabels$timing,
      lty = c(2, 2),
      col = c('dodgerblue', 'deeppink'),
      cex = scaleMultiplier * cex_legend,
      horiz = legendTimingHorizontal,
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
    text(0.6, 0.65, labels = 'types', cex = scaleMultiplier * cex_lab)
    
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
         0.99,
         labels = 'Parameter setting:',
         cex = scaleMultiplier * cex_parameters_title,
         font = 2)
    
    parNameAndValue1 <- ''
    parNameAndValue2 <- ''
    parNameAndValue3 <- ''
    parNameAndValue4 <- ''
    parNameAndValue5 <- ''
    parNameAndValue6 <- ''
    
    for (i in 1:length(PARAMETERS))
    {
      if (i < 6)
      {
        parNameAndValue1 <-
          paste(parNameAndValue1, names(PARAMETERS)[i], ' = ', PARAMETERS[[i]], sep = '')
        parNameAndValue1 <- paste0(parNameAndValue1, ', ')
      }
      else
      {
        if (i < 9)
        {
          parNameAndValue2 <-
            paste(parNameAndValue2, names(PARAMETERS)[i], ' = ', PARAMETERS[[i]], sep = '')
          if (i < length(PARAMETERS))
          {
            parNameAndValue2 <- paste0(parNameAndValue2, ', ')
          }
        }
        else
        {
          if (i < 11)
          {
            parNameAndValue3 <-
              paste(parNameAndValue3, names(PARAMETERS)[i], ' = ', PARAMETERS[[i]], sep = '')
            if (i < length(PARAMETERS))
            {
              parNameAndValue3 <- paste0(parNameAndValue3, ', ')
            }
          }
          else
          {
            if (i < 13)
            {
              parNameAndValue4 <-
                paste(parNameAndValue4, names(PARAMETERS)[i], ' = ', PARAMETERS[[i]], sep = '')
              if (i < length(PARAMETERS))
              {
                parNameAndValue4 <- paste0(parNameAndValue4, ', ')
              }
            }
            else
            {
              if (i < 17)
              {
                parNameAndValue5 <-
                  paste(parNameAndValue5, names(PARAMETERS)[i], ' = ', PARAMETERS[[i]], sep = '')
                if (i < length(PARAMETERS))
                {
                  parNameAndValue5 <- paste0(parNameAndValue5, ', ')
                }
              }
              else
              {
                parNameAndValue6 <-
                  paste(parNameAndValue6, names(PARAMETERS)[i], ' = ', PARAMETERS[[i]], sep = '')
                if (i < length(PARAMETERS))
                {
                  parNameAndValue6 <- paste0(parNameAndValue6, ', ')
                }
              }
            }
          }
        }
      }
    }
    
    text(0.5, 0.85,
         labels = parNameAndValue1,
         cex = scaleMultiplier * cex_parameters)
    text(0.5, 0.7,
         labels = parNameAndValue2,
         cex = scaleMultiplier * cex_parameters)
    text(0.5, 0.55,
         labels = parNameAndValue3,
         cex = scaleMultiplier * cex_parameters)
    text(0.5, 0.4,
         labels = parNameAndValue4,
         cex = scaleMultiplier * cex_parameters)
    text(0.5, 0.25,
         labels = parNameAndValue5,
         cex = scaleMultiplier * cex_parameters)
    text(0.5, 0.1,
         labels = parNameAndValue6,
         cex = scaleMultiplier * cex_parameters)
    
    # sleep
    Sys.sleep(device_sleep)
  }
