trajectories.ggplot <- function(runs,
                                variables.plot.1,
                                variables.plot.2,
                                variables.plot.1.title,
                                variables.plot.2.title,
                                cex = 1)
{
  g1.data <- reshape::melt(exp.LHS.trajectories[, c('run', 'time', variables.plot.1)], 
                           id.vars = c('run', 'time'))
  g1 <- 
    ggplot(data = g1.data, aes(x = time)) +
    geom_line(data = g1.data, aes(x = time, y = value, colour = variable)) +
    facet_wrap(.~factor(run), ncol = 5) +
    scale_color_manual(values = c('blue', 'red')) +
    theme_bw() +
    theme(strip.background = element_blank(), strip.text.x = element_blank(),
          legend.title = element_blank()) +
    ggtitle(variables.plot.1.title) +
    ylab('')
  
  g2.data <- reshape::melt(exp.LHS.trajectories[, c('run', 'time', variables.plot.2)], 
                           id.vars = c('run', 'time'))
  g2 <- 
    ggplot(data = g2.data, aes(x = time)) +
    geom_line(data = g2.data, aes(x = time, y = value, colour = variable)) +
    geom_hline(aes(yintercept = 0), data = g2.data, linetype = "dashed") +
    ylim(-1, 1) +
    facet_wrap(.~factor(run), ncol = 5) +
    scale_color_manual(values = c('blue', 'red')) +
    theme_bw() +
    theme(strip.background = element_blank(), strip.text.x = element_blank(),
          legend.title = element_blank()) +
    ggtitle(variables.plot.2.title) +
    ylab('')
  
  ggpubr::ggarrange(g1, g2, nrow = 2, legend = "bottom", common.legend = TRUE)
}
