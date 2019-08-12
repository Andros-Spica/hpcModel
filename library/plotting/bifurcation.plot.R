bifurcation.plot.simple <- function(parameterValues, 
                                    observedVariables, 
                                    xlab = "parameter", 
                                    ylab = "observed variable",
                                    legendPos = c(0, 0),
                                    variableNames = paste("variable ", seq(0, ncol(observedVariables))),
                                    col = rainbow(ncol(observedVariables), v=.8),
                                    cex = 1
)
{
  par(mar = c(5,5,1,1))
  plot(parameterValues,
       observedVariables[,1],
       type = 'l',
       col = col[1],
       ylim = c(min(observedVariables), max(observedVariables)) * 1.01,
       ylab = ylab,
       xlab = xlab,
       cex.axis = cex * 0.3,
       cex.lab = cex * 0.3,
       lwd = cex)
  
  for (v in 1:ncol(observedVariables))
  {
    points(parameterValues, observedVariables[,v], type = 'l', col = col[v], lwd = cex)
  }
  
  legend(legendPos[1],legendPos[2], legend = variableNames, col = col, lty = 1, lwd = cex, cex = cex * 0.3)
}

bifurcation.plot.pair <- function(result.H, result.P, 
                                  parameterLabel, variableLabelH, variableLabelP, 
                                  plotScale = 1, type = 'c')
{
  ylim = c(
    -1,#min(c(min(result.H[,2]), min(result.P[,2]))), 
    1#max(c(max(result.H[,2]), max(result.P[,2]))))
  )
  
  layout(matrix(c(1, 2), ncol = 2))
  
  par(mar = plotScale * c(1.2, 1.2, 0.1, 0.1),
      mgp = plotScale * c(0.8, 0.3, 0),
      cex.lab = plotScale * 0.5,
      cex.axis = plotScale * 0.5,
      lwd = plotScale
  )
  
  plot(result.H,
       pch = '·',
       xlab = parameterLabel,
       ylab = variableLabelH,
       cex = plotScale,
       type = type,
       ylim = ylim
  )
  
  plot(result.P,
       pch = '·',
       xlab = parameterLabel,
       ylab = variableLabelP,
       cex = plotScale,
       type = type,
       ylim = ylim
  )
}
