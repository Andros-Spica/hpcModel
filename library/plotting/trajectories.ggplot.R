trajectories.ggplot <- function(runs,
                                variables.plot.1,
                                variables.plot.2,
                                variables.plot.1.title,
                                variables.plot.2.title,
                                cex = 1)
{
  run_labeller <- function(runIndex){
    return(LETTERS[runIndex])
  }
  
  maxTime = max(runs$time)
  
  runIndexes <- unique(runs$run)
  
  runs.titles.data <- data.frame(
    label = LETTERS[runIndexes],
    pos.x = 4 + 100 * (1:length(runIndexes) - 0.5) / length(runIndexes)
  )
  
  runs.titles <- 
    ggplot(data = runs.titles.data) +
    geom_text(data = runs.titles.data,
              aes(x = pos.x, label = label), 
              y = 5, size = cex, fontface = "bold") +
    xlim(0, 100) + ylim(0, 10) +
    theme_void()
    
  g1.data <- reshape::melt(runs[, c('run', 'time', variables.plot.1)], 
                           id.vars = c('run', 'time'))
  g1 <- 
    ggplot(data = g1.data, aes(x = time)) +
    geom_line(data = g1.data, aes(x = time, y = value, colour = variable), show.legend = FALSE) +
    #geom_text(data = g1.data, aes(y = max(value)*0.9, label = run_labeller(run)), x = maxTime*0.9, fontface = "bold", size = cex) +
    facet_wrap(.~factor(run), ncol = 5) +
    scale_color_manual(values = c('blue', 'red')) +
    theme_bw() +
    ylab(variables.plot.1.title) +
    theme(strip.background = element_blank(), 
          strip.text.x = element_blank(),
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = cex*2, margin = ggplot2::margin(l = -10, r = 8.5)),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = cex*2),
          plot.margin = unit(c(10, 5.5, 2, 17.5), units = "pt"))
  
  g2.data <- reshape::melt(exp.LHS.trajectories[, c('run', 'time', variables.plot.2)], 
                           id.vars = c('run', 'time'))
  g2 <- 
    ggplot(data = g2.data, aes(x = time)) +
    geom_line(data = g2.data, aes(x = time, y = value, colour = variable), show.legend = FALSE) +
    geom_hline(aes(yintercept = 0), data = g2.data, linetype = "dashed") +
    ylim(-1, 1) +
    #geom_text(data = g2.data, aes(y = 0.8, label = run_labeller(run)), x = maxTime*0.9, fontface = "bold", size = cex) +
    facet_wrap(.~factor(run), ncol = 5) +
    scale_color_manual(values = c('blue', 'red')) +
    theme_bw() +
    theme(strip.background = element_blank(), 
          strip.text.x = element_blank(),
          legend.title = element_blank(),
          axis.title = element_text(size = cex*2),
          axis.text = element_text(size = cex*2)) +
    ylab(variables.plot.2.title)
  
  bottom.legend.data <- data.frame(
    label = c('Humans', 'Humans', 'Plants', 'Plants'),
    pos.x = c(35, 39, 57, 61),
    label.x = c(40, 40, 62, 62)
  )
  
  bottom.legend <- 
    ggplot(data = bottom.legend.data) +
    geom_path(data = bottom.legend.data,
              aes(x = pos.x, group = label, color = label), 
              y = 5, show.legend = FALSE, size = cex*0.2) +
    geom_text(data = bottom.legend.data,
              aes(x = label.x, label = label), 
              y = 5, size = cex, hjust = 0) +
    scale_color_manual(values = c('blue', 'red')) +
    xlim(0, 100) + ylim(0, 10) +
    theme_void()
  
  ggpubr::ggarrange(runs.titles, g1, g2, bottom.legend, nrow = 4, heights = c(1, 5.5, 6, 1))#, legend = "bottom", common.legend = TRUE)
}
