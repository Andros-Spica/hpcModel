variableImportancePlot <- function(random_forest_results, plot_scale = 1, color_variable_map = NULL)
{
  variableImportance <- randomForest::importance(random_forest_results)
  variableImportance <- variableImportance[nrow(variableImportance):1, ] # invert row order for plotting top-downwards
  
  # set negative values as the minimum positive values
  variableImportance[variableImportance < 0] <- min(variableImportance[variableImportance > 0]) 
  
  # log 10 scale
  variableImportance <- log(variableImportance, 10)
  
  if (is.null(color_variable_map))
  {
    color_variable_map <- rep('black', length(variableImportance))
  }
  
  dotchart(variableImportance, 
           color = color_variable_map,
           cex = plot_scale,
           main = random_forest_results$terms[[2]],
           pch = 19)
}

variableImportancePlotPair <- function(random_forest_results_1, 
                                       random_forest_results_2, 
                                       plot_scale = 1,
                                       color_variable_map = NULL,
                                       layout_type = 'vertical',
                                       x_axis_label_x_position = 0.8 # c(0.3, 0.83)
                                       )
{
  par(mar = c(3,1,3,1), cex.lab = plot_scale * 0.5)
  
  if (layout_type == 'horizontal')
  {
    layout(matrix(c(1,2,3,3), ncol = 2, byrow = T),
           heights = c(12,1))
  }
  if (layout_type == 'vertical')
  {
    layout(matrix(c(1,2,3), nrow = 3, byrow = T),
           heights = c(12,12,1))
  }
  
  variableImportancePlot(random_forest_results_1, plot_scale = plot_scale * 0.8, color_variable_map = color_variable_map)
  
  variableImportancePlot(random_forest_results_2, plot_scale = plot_scale * 0.8, color_variable_map = color_variable_map)
  
  par(mar = c(0, 0, 0, 0))
  
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  
  if (layout_type == 'horizontal')
  {
    text(x = x_axis_label_x_position[1], y = 0.8, 
         expression(paste(log[10], x)), 
         cex = 1.6 * plot_scale, col = "black", font = 4)
    text(x = x_axis_label_x_position[2], y = 0.8, 
         expression(paste(log[10], x)), 
         cex = 1.6 * plot_scale, col = "black", font = 4)
  }
  if (layout_type == 'vertical')
  {
    text(x = x_axis_label_x_position, y = 0.8, 
         expression(paste(log[10], x)), 
         cex = 1.6 * plot_scale, col = "black", font = 4)
  }
  
}
