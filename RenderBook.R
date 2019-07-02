# visit https://bookdown.org/yihui/bookdown for more options

# create the nojeckyll file (once, if docs/ is empty)
#file.create('docs/.nojekyll')

bookdown::render_book("index.Rmd", 
                      output_format = 'bookdown::gitbook',
                      output_dir = "docs", 
                      params = list(ispdf = FALSE))

# It is posible to obtain the pdf version using the following code OR 
# by accessing the save>pdf option in the html gitbook version generated above.

# if MikTex is installed: 
# devtools::install_github('yihui/tinytex')

bookdown::render_book("index.Rmd",
                      output_format = 'bookdown::pdf_book',
                      output_dir = "pdf",
                      params = list(ispdf = TRUE))

# to preview a single chapter

bookdown::preview_chapter("1_singleRunExploration.Rmd")
