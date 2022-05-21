bifurcationPlot_twoVariables <- function(results_per_parameter,
                                         parameter_x,
                                         variable_1,
                                         variable_2,
                                         axis_x_label = 'parameter values',
                                         axis_y_label = 'observation variable',
                                         variable_1_label = 'Variable 1', variable_1_color = 'blue',
                                         variable_2_label = 'Variable 2', variable_2_color = 'red',
                                         plot_scale = 1)
{
  variableLabelColorMap <- c(variable_1_color, variable_2_color)
  names(variableLabelColorMap) <- c(variable_1_label, variable_2_label)
  
  variable_1_label_reString = paste0('"', variable_1_label, '"')
  variable_2_label_reString = paste0('"', variable_2_label, '"')
  
  ggplot(results_per_parameter, aes_string(x = parameter_x)) +
    geom_point(aes_string(y = variable_1, colour = variable_1_label_reString), size = 0.05) +
    stat_smooth(
      aes_string(y = variable_1, colour = variable_1_label_reString),
      alpha = 0.5,
      level = 0.9999999
    ) +
    geom_point(aes_string(y = variable_2, colour = variable_2_label_reString), size = 0.05) +
    stat_smooth(
      aes_string(y = variable_2, colour = variable_2_label_reString),
      alpha = 0.5,
      level = 0.9999999
    ) +
    scale_colour_manual(name = '', values = variableLabelColorMap) +
    labs(x = axis_x_label,
         y = axis_y_label) +
    theme_bw() +
    theme(
      axis.title = element_text(size = plot_scale * 3),
      axis.text = element_text(size = plot_scale * 1.5),
      legend.title = element_text(size = plot_scale * 3.5),
      legend.text = element_text(size = plot_scale * 2)
    )
}
