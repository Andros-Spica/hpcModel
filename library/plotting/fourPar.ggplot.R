fourPar.ggplot <- function(twoPar.exp, par1, par2, par3, par4, var1, var2, 
                           xlab = NULL, ylab = NULL, var1lab = NULL, var2lab = NULL,
                           plotScale = 1)
{
  if (is.null(xlab)) {
    xlab = par1
  }
  if (is.null(ylab)) {
    ylab = par2
  }
  if (is.null(var1lab)) {
    var1lab = var1
  }
  if (is.null(var2lab)) {
    var2lab = var2
  }
  
  var1min = -1#min(twoPar.exp[,var1])
  var1max = 1#max(twoPar.exp[,var1])
  var2min = -1#min(twoPar.exp[,var2])
  var2max = 1#max(twoPar.exp[,var2])
  
  ggplot(twoPar.exp, aes_string(x = par1, y = par2)) +
    geom_raster(aes_string(fill = var1)) +
    geom_point(aes_string(color = var2),
               shape = 15,
               size = plotScale * 0.8) +
    geom_point(aes(size = time), shape = 't', color = 'grey') +
    scale_fill_gradientn(
      colors = c('white', 'darkblue'),
      values = rescale(c(var1min, var1max)),
      limits = c(var1min, var1max)
    ) +
    scale_color_gradientn(
      colors = c('white', 'darkred'),
      values = rescale(c(var2min, var2max)),
      limits = c(var2min, var2max)
    ) +
    scale_size(range = c(plotScale * 0.5, plotScale * 2)) +
    labs(x = xlab,
         y = ylab,
         fill = var1lab,
         color = var2lab,
         size = expression(t['end'])) +
    guides(
      color = guide_colorbar(order = 0),
      fill = guide_colorbar(order = 1),
      size = guide_legend(order = 2)
    ) +
    facet_grid(
      reformulate(par3, par4), 
      scales='free', 
      as.table = FALSE,
      labeller = label_both#label_wrap_gen(multi_line=FALSE)
    ) +
    theme_bw() +
    theme(
      strip.background = element_rect(fill = NA, colour = NA), 
      strip.text = element_text(size = plotScale * 1.5),
      panel.grid.minor =  element_blank(),
      panel.background = element_blank(),
      axis.title = element_text(size = plotScale * 3),
      axis.text = element_text(size = plotScale * 1.5),
      legend.title = element_text(size = plotScale * 3.5),
      legend.text = element_text(size = plotScale * 2)
    )
}
