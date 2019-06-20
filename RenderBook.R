# create the nojeckyll file (once)
#file.create('docs/.nojekyll')

bookdown::render_book("index.Rmd", 
                      output_format = bookdown::gitbook(),
                      clean = TRUE, 
                      envir = parent.frame(), clean_envir = !interactive(), 
                      output_dir = "docs", 
                      new_session = NA, preview = FALSE, 
                      encoding = "UTF-8", config_file = "_bookdown.yml")
