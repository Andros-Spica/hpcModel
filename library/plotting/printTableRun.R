printTableRun <- function(run_result, caption = NULL, output = "html")
{
  runParameterNames <- names(run_result$PARAMETERS)
  runParameterValues <- c()
  for (i in 1:length(run_result$PARAMETERS))
  {
    runParameterValues <- c(runParameterValues, run_result$PARAMETERS[[i]])
  }
  knitr::kable(cbind(runParameterNames, runParameterValues), output,
               col.names = c("parameter", "values"),
               caption = caption) %>%
    kable_styling(latex_options = "hold_position")
}
