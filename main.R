
## Generate Clean Bibliographies

library(stringr)

source('aux_funs/function_cleanbib.R')

clean_bib(input_file = 'paper/paper.Rmd',input_bib = 'references/references.bib',
          output_bib = 'references/references_sec')

## Render Markdown

rmarkdown::render('paper/paper.Rmd')
