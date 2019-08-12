exp.table <- function(exp.results, caption = NULL, numberOfParameters = 17, output = "html")
{
  exp.parnames <- c()
  exp.values <- c()
  for (i in 1:numberOfParameters)
  {
    exp.parnames <- c(exp.parnames, names(exp.results)[i])
    value <- levels(factor(exp.results[[i]]))
    if (length(value) > 1) 
    {
      value <- as.numeric(as.character(value))
      value <- paste(min(value), "-", max(value), "(sample =", length(value), ")")
    }
    exp.values <- c(exp.values, value)
  }
  kable(cbind(exp.parnames, exp.values), output,
        col.names = c("parameter", "value"),
        caption = caption) %>%
    kable_styling(latex_options = "hold_position")
}
