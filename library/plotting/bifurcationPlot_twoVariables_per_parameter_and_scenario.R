bifurcationPlot_twoVariables_per_parameter_and_scenario <- function(results_per_parameter_and_scenario,
                                                                    parameter_names,
                                                                    variable_1,
                                                                    variable_2,
                                                                    scenario_splitter,
                                                                    axis_x_label = 'parameter values',
                                                                    axis_y_label = 'observation variable',
                                                                    variable_1_label = 'Variable 1', variable_1_color = c('blue', 'darkblue'),
                                                                    variable_2_label = 'Variable 2', variable_2_color = c('red', 'darkred'),
                                                                    axis_y_log = FALSE,
                                                                    plot_scale = 1,
                                                                    strip_text_x_size = 1.5, strip_text_y_size = 3,
                                                                    axis_title_size = 3, axis_text_size = 1.5,
                                                                    legend_title_size = 3.5, legend_text_size = 2)
{
  preparationDataframe <- cbind(results_per_parameter_and_scenario, scenario_splitter)
  preparationDataframe <- melt(preparationDataframe[, c(variable_1, variable_2, 
                                                        names(preparationDataframe)[ncol(preparationDataframe)], 
                                                        parameter_names)], 
                               id.vars = 1:3)
  names(preparationDataframe) <-
    c("observationVariable1", "observationVariable2", "scenario", "parameter", "parameterValue")
  
  variableLabelColorMap <- list(variable_1_color[1], variable_2_color[1])
  names(variableLabelColorMap) <- c(variable_1_label, variable_2_label)
  
  thePlot <- ggplot(preparationDataframe) +
    geom_point(
      data = preparationDataframe,
      aes(x = parameterValue, y = observationVariable1, color = variable_1_label),
      alpha = 0.2, size = 1
    ) +
    geom_smooth(
      data = preparationDataframe,
      aes(x = parameterValue, y = observationVariable1), color = variable_1_color[2]
    ) +
    geom_point(
      data = preparationDataframe,
      aes(x = parameterValue, y = observationVariable2, color = variable_2_label),
      alpha = 0.2, size = 1
    ) +
    geom_smooth(
      data = preparationDataframe,
      aes(x = parameterValue, y = observationVariable2), color = variable_2_color[2]
    ) +  
    facet_grid(
      scenario ~ parameter,
      scales = "free_x",
      labeller = label_parsed,
      
    ) +
    scale_color_manual(name = '', values = variableLabelColorMap) +
    guides(color = guide_legend(override.aes = list(size = plot_scale))) +
    labs(x = axis_x_label,
         y = axis_y_label) +
    theme_bw() +
    theme(
      strip.background = element_rect(fill = NA, colour = NA), 
      strip.text.x = element_text(size = plot_scale * strip_text_x_size),
      strip.text.y = element_text(size = plot_scale * strip_text_y_size),
      panel.grid.minor =  element_blank(),
      panel.background = element_blank(),
      axis.title = element_text(size = plot_scale * axis_title_size),
      axis.text = element_text(size = plot_scale * axis_text_size),
      legend.title = element_text(size = plot_scale * legend_title_size),
      legend.text = element_text(size = plot_scale * legend_text_size)
    )
  if (axis_y_log) thePlot <- thePlot + scale_y_log10()
  thePlot
}
