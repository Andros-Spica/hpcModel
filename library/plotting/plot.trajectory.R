plot.trajectory <- function(meltedRunData, plotScale = 1)
{
  ggplot(meltedRunData,
         aes(x = time, y = value, colour = variable), size = 2 * plotScale) +
    geom_line(size = 0.2 * plotScale) +
    ylab("population") + 
    scale_color_manual(labels = c('Humans', ' Plants'), values = c('blue', 'red')) +
    
    theme(axis.title = element_text(size = 3 * plotScale),
          axis.text = element_text(size = 2 * plotScale),
          legend.title = element_blank(),
          legend.text = element_text(size = 3 * plotScale),
          legend.key.size = unit(0.2 * plotScale, 'lines'))
}
