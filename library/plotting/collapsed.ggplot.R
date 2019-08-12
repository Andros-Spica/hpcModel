collapsed.ggplot <- function(results,
                             parameterName,
                             variableName1,
                             variableName2,
                             ylab = 'observation variable',
                             plotScale = 1)
{
  ggplot(results, aes_string(x = parameterName)) +
    stat_smooth(
      aes_string(y = variableName1, colour = '"Humans"'),
      alpha = 0.5,
      level = 0.9999999
    ) +
    geom_point(aes_string(y = variableName1, colour = '"Humans"'), size = 0.05) +
    stat_smooth(
      aes_string(y = variableName2, colour = '"Plants"'),
      alpha = 0.5,
      level = 0.9999999
    ) +
    geom_point(aes_string(y = variableName2, colour = '"Plants"'), size = 0.05) +
    scale_colour_manual(name = '', values = c('Humans' = 'blue', 'Plants' = 'red')) +
    labs(x = parameterName,
         y = ylab) +
    theme_bw() +
    theme(
      axis.title = element_text(size = plotScale * 3),
      axis.text = element_text(size = plotScale * 1.5),
      legend.title = element_text(size = plotScale * 3.5),
      legend.text = element_text(size = plotScale * 2)
    )
}
