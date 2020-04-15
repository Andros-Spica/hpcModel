varImpPlot2 <- function(RF, cex = 1)
{
  imp <- randomForest::importance(RF)
  imp <- imp[nrow(imp):1, ] # invert row order for plotting top-downwards
  
  # set negative values as the minimum positive values
  imp[imp < 0] <- min(imp[imp > 0]) 
  # log 10 scale
  imp <- log(imp, 10)
  
  # assuming order to be: n, v, r, mU, U.b and MaxArea
  parColors <- c("black", rep("blue", 4), rep("green", 4), rep("red", 6))
  dotchart(imp, 
           color = parColors,
           cex = cex,
           main = RF$terms[[2]],
           pch = 19)
}

varImpPlot2Pair <- function(RF1, RF2, cex = 1)
{
  par(mar = c(3,1,3,1), cex.lab = cex * 0.5)
  layout(matrix(c(1,2,3,3), ncol = 2, byrow = T),
         heights = c(12,1))
  
  varImpPlot2(RF1, cex = cex * 0.8)
  varImpPlot2(RF2, cex = cex * 0.8)
  
  par(mar=c(0, 0, 0, 0))
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.3, y = 0.8, expression(paste(log[10], x)), 
       cex = 1.6, col = "black", font = 4)
  text(x = 0.83, y = 0.8, expression(paste(log[10], x)), 
       cex = 1.6, col = "black", font = 4)
}
