parameter.x.scenario.ggplot <- function(results,
                                        parameterNames,
                                        variableName1,
                                        variableName2,
                                        scenarioSplitter,
                                        ylab = 'observation variable',
                                        plotScale = 1,
                                        y.log = FALSE)
{
  temp <- cbind(results, scenarioSplitter)
  temp <- melt(temp[, c(variableName1, 
                        variableName2, 
                        names(temp)[ncol(temp)], 
                        parameterNames)], 
               id.vars = 1:3)
  names(temp) <-
    c("obsVar1", "obsVar2", "scenario", "parameter", "parValue")
  
  p <- ggplot(temp) +
    geom_point(
      data = temp,
      aes(x = parValue, y = obsVar1, color = 'Humans'),
      alpha = 0.2, size = 1
    ) +
    geom_smooth(
      data = temp,
      aes(x = parValue, y = obsVar1), color = 'darkblue'
    ) +
    geom_point(
      data = temp,
      aes(x = parValue, y = obsVar2, color = 'Plants'),
      alpha = 0.2, size = 1
    ) +
    geom_smooth(
      data = temp,
      aes(x = parValue, y = obsVar2), color = 'darkred'
    ) +  
    facet_grid(
      scenario ~ parameter,
      scales = "free_x",
      labeller = label_parsed
    ) +
    scale_color_manual(name = '', values = c('Humans' = 'blue', 'Plants' = 'red')) +
    guides(color = guide_legend(override.aes = list(size = plotScale))) +
    labs(x = 'parameters values',
         y = ylab) +
    theme_bw() +
    theme(
      strip.background = element_rect(fill = NA, colour = NA), 
      strip.text.x = element_text(size = plotScale * 3),
      strip.text.y = element_text(size = plotScale * 2),
      panel.grid.minor =  element_blank(),
      panel.background = element_blank(),
      axis.title = element_text(size = plotScale * 3),
      axis.text = element_text(size = plotScale * 1.5),
      legend.title = element_text(size = plotScale * 3.5),
      legend.text = element_text(size = plotScale * 2)
    )
  if (y.log) p <- p + scale_y_log10()
  p
}
