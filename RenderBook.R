# visit https://bookdown.org/yihui/bookdown for more options

# create the nojeckyll file (once, if docs/ is empty)
#file.create('docs/.nojekyll')

# Go to 'index.Rmd' and make sure that 'params: ispdf: FALSE' 
# at the end of the YAML header

bookdown::render_book("index.Rmd", 
                      output_format = bookdown::gitbook(),
                      clean = TRUE, 
                      envir = parent.frame(), clean_envir = !interactive(), 
                      output_dir = "docs", 
                      new_session = NA, preview = FALSE, 
                      encoding = "UTF-8", config_file = "_bookdown.yml")

# It is posible to obtain the pdf version using the following code OR 
# by accessing the save>pdf option in the html gitbook version generated above.

# if MikTex is installed: 
# devtools::install_github('yihui/tinytex')

# Go to 'index.Rmd' and make sure that 'params: ispdf: TRUE' 
# at the end of the YAML header

bookdown::render_book("index.Rmd",
                      output_format = bookdown::pdf_book(),
                      clean = TRUE,
                      envir = parent.frame(), clean_envir = !interactive(),
                      output_dir = "pdf",
                      new_session = NA, preview = FALSE,
                      encoding = "UTF-8", config_file = "_bookdown.yml")

# to preview a single chapter

bookdown::preview_chapter("5_randomForest.Rmd")
