bifurcationPlot_twoVariables_per_parameter <- function(results_per_parameter,
                                                       parameter_names,
                                                       variable_1,
                                                       variable_2,
                                                       axis_x_label = 'parameter values',
                                                       axis_y_label = 'observation variable',
                                                       variable_1_label = 'Variable 1', variable_1_color = c('blue', 'darkblue'),
                                                       variable_2_label = 'Variable 2', variable_2_color = c('red', 'darkred'),
                                                       axis_y_log = FALSE,
                                                       facet_wrap_number_of_rows = 4,
                                                       plot_scale = 1,
                                                       strip_text_x_size = 2,
                                                       axis_title_size = 3, axis_text_size = 1.5,
                                                       legend_title_size = 3.5, legend_text_size = 2)
{
  temporaryData <- melt(results_per_parameter[, c(variable_1, variable_2, parameter_names)], 
               id.vars = 1:2)
  names(temporaryData) <-
    c("observationVariable1", "observationVariable2", "parameter", "parameterValue")
  
  variableLabelColorMap <- c(variable_1_color[1], variable_2_color[1])
  names(variableLabelColorMap) <- c(variable_1_label, variable_2_label)
  
  thePlot <- ggplot(temporaryData) +
    geom_point(
      data = temporaryData,
      aes(x = parameterValue, y = observationVariable1, color = variable_1_label),
      alpha = 0.2, size = 1
    ) +
    geom_smooth(
      data = temporaryData,
      aes(x = parameterValue, y = observationVariable1), color = variable_1_color[2]
    ) +
    geom_point(
      data = temporaryData,
      aes(x = parameterValue, y = observationVariable2, color = variable_2_label),
      alpha = 0.2, size = 1
    ) +
    geom_smooth(
      data = temporaryData,
      aes(x = parameterValue, y = observationVariable2), color = variable_2_color[2]
    ) +  
    facet_wrap(
      vars(parameter), 
      ncol = ceiling(length(parameter_names) / facet_wrap_number_of_rows),
      scales = "free_x",
      labeller = label_parsed
    ) +
    scale_color_manual(name = '', values = variableLabelColorMap) +
    guides(color = guide_legend(override.aes = list(size = plot_scale))) +
    labs(x = axis_x_label,
         y = axis_y_label) +
    theme_bw() +
    theme(
      strip.background = element_rect(fill = NA, colour = NA), 
      strip.text.x = element_text(size = plot_scale * strip_text_x_size),
      panel.grid.minor =  element_blank(),
      panel.background = element_blank(),
      axis.title = element_text(size = plot_scale * axis_title_size),
      axis.text = element_text(size = plot_scale * axis_text_size),
      legend.title = element_text(size = plot_scale * axis_title_size),
      legend.text = element_text(size = plot_scale * legend_text_size)
    )
  if (axis_y_log) thePlot <- thePlot + scale_y_log10()
  thePlot
}
