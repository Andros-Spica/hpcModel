plotTrajectoriesPerVariableGroup <- function(runs,
                                             plot1_variables,
                                             plot2_variables,
                                             plot1_colors = c('blue', 'red'),
                                             plot2_colors = c('blue', 'red'),
                                             plot1_title = 'variables group 1',
                                             plot2_title = 'variables group 2',
                                             plot_legend_labels = c('Variable 1', 'Variable 2', 'Variable 3', 'Variable 4'),
                                             plot_legend_line_x_positions = c(35, 39, 57, 61),
                                             plot_legend_label_x_positions = c(40, 40, 62, 62),
                                             plot1_mark_y_zero = FALSE,
                                             plot2_mark_y_zero = TRUE,
                                             plot_scale = 1)
{
  run_labeller <- function(runIndex){
    return(LETTERS[runIndex])
  }
  
  maxTime = max(runs$time)
  
  runIndexes <- unique(runs$run)
  
  titlesOfRunsData <- data.frame(
    label = LETTERS[runIndexes],
    positionX = 4 + 100 * (1:length(runIndexes) - 0.5) / length(runIndexes)
  )
  
  titlesOfRuns <- 
    ggplot(data = titlesOfRunsData) +
    geom_text(data = titlesOfRunsData,
              aes(x = positionX, label = label), 
              y = 5, size = plot_scale, fontface = "bold") +
    xlim(0, 100) + ylim(0, 10) +
    theme_void()
  
  plot1_data <- reshape::melt(runs[, c('run', 'time', plot1_variables)], 
                              id.vars = c('run', 'time'))
  plot1 <- 
    ggplot(data = plot1_data, aes(x = time)) +
    geom_line(data = plot1_data, aes(x = time, y = value, colour = variable), show.legend = FALSE) +
    #geom_text(data = plot1_data, aes(y = max(value)*0.9, label = run_labeller(run)), x = maxTime*0.9, fontface = "bold", size = plot_scale) +
    facet_wrap(.~factor(run), ncol = 5) +
    scale_color_manual(values = plot1_colors) +
    theme_bw() +
    ylab(plot1_title) +
    theme(strip.background = element_blank(), 
          strip.text.x = element_blank(),
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = plot_scale * 2, margin = ggplot2::margin(l = -10, r = 8.5)),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = plot_scale * 2),
          plot.margin = unit(c(10, 5.5, 2, 17.5), units = "pt"))
  
  if (plot1_mark_y_zero)
  {
    plot1 <- plot1 + geom_hline(aes(yintercept = 0), data = plot1_data, linetype = "dashed")
  }
  
  plot2_data <- reshape::melt(runs[, c('run', 'time', plot2_variables)], 
                              id.vars = c('run', 'time'))
  plot2 <- 
    ggplot(data = plot2_data, aes(x = time)) +
    geom_line(data = plot2_data, aes(x = time, y = value, colour = variable), show.legend = FALSE) +
    #geom_hline(aes(yintercept = 0), data = plot2_data, linetype = "dashed") +
    ylim(-1, 1) +
    #geom_text(data = plot2_data, aes(y = 0.8, label = run_labeller(run)), x = maxTime*0.9, fontface = "bold", size = plot_scale) +
    facet_wrap(.~factor(run), ncol = 5) +
    scale_color_manual(values = plot2_colors) +
    theme_bw() +
    theme(strip.background = element_blank(), 
          strip.text.x = element_blank(),
          legend.title = element_blank(),
          axis.title = element_text(size = plot_scale * 2),
          axis.text = element_text(size = plot_scale * 2)) +
    ylab(plot2_title)
  
  if (plot2_mark_y_zero)
  {
    plot2 <- plot2 + geom_hline(aes(yintercept = 0), data = plot2_data, linetype = "dashed")
  }
  
  bottomLegendData <- data.frame(
    label = plot_legend_labels,
    positionX = plot_legend_line_x_positions,
    labelX = plot_legend_label_x_positions
  )
  
  bottomLegend <- 
    ggplot(data = bottomLegendData) +
    geom_path(data = bottomLegendData,
              aes(x = positionX, group = label, color = label), 
              y = 5, show.legend = FALSE, size = plot_scale * 0.2) +
    geom_text(data = bottomLegendData,
              aes(x = labelX, label = label), 
              y = 5, size = plot_scale, hjust = 0) +
    scale_color_manual(values = c('blue', 'red')) +
    xlim(0, 100) + ylim(0, 10) +
    theme_void()
  
  ggpubr::ggarrange(titlesOfRuns, plot1, plot2, bottomLegend, nrow = 4, heights = c(1, 5.5, 6, 1))#, legend = "bottom", common.legend = TRUE)
}
