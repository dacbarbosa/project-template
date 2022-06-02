
## Load Necessary Packages

library(stringr) # Edit Strings for Processing Bibliographic Files
library(fixest) # Fast Fixed-Effects Estimations
library(modelsummary) # Customize Outputs
library(pander) # Process Tables with Pandoc
library(kableExtra)

## Generate Clean Bibliographies

source('aux_funs/function_cleanbib.R')

clean_bib(input_file = 'paper/paper.Rmd',input_bib = 'references/references.bib',
          output_bib = 'references/references_sec')

## Render Markdown

rmarkdown::render('paper/paper.Rmd')
  