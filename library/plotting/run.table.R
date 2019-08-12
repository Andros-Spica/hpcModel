run.table <- function(run.result, caption = NULL, output = "html")
{
  run.parnames <- names(run.result$PARS)
  run.parvalues <- c()
  for (i in 1:length(run.result$PARS))
  {
    run.parvalues <- c(run.parvalues, run.result$PARS[[i]])
  }
  knitr::kable(cbind(run.parnames, run.parvalues), output,
               col.names = c("parameter", "values"),
               caption = caption) %>%
    kable_styling(latex_options = "hold_position")
}
