printTableExperiment <- function(exp_parameter_values, caption = NULL, output = "html")
{
  number_of_parameters = length(exp_parameter_values)
  
  table_parameter_names <- c()
  table_parameter_values <- c()
  
  for (i in 1:number_of_parameters)
  {
    table_parameter_names <- c(table_parameter_names, names(exp_parameter_values)[i])
    value <- levels(factor(exp_parameter_values[,i]))
    if (length(value) > 1) 
    {
      value <- as.numeric(as.character(value))
      value <- paste(min(value), "-", max(value), "(sample =", length(value), ")")
    }
    table_parameter_values <- c(table_parameter_values, value)
  }
  
  knitr::kable(cbind(table_parameter_names, table_parameter_values), 
               output,
               col.names = c("parameter", "value"),
               caption = caption) %>%
    kable_styling(latex_options = "hold_position")
}
