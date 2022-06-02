clean_bib <- function(input_file, input_bib, output_bib){

  lines <- paste(readLines(input_file), collapse = "")
  lines <- stringr::str_split(lines,'(APPENDIX)')
  main = lines[[1]][1]
  appendix = lines[[1]][2]

  entries_main <- unique(str_match_all(main, "@([a-zA-Z0-9_-]+)[,\\. \\?\\!\\]\\;]")[[1]][, 2]) 
  entries_appendix <- unique(str_match_all(appendix, "@([a-zA-Z0-9_-]+)[,\\. \\?\\!\\]\\;]")[[1]][, 2]) 

  bib <- paste(readLines(input_bib), collapse = "\n")
  bib <- unlist(strsplit(bib, "\n@"))

  output_main <- sapply(entries_main, grep, bib, value = T)
  output_main <- paste("@", output_main, sep = "")

  output_appendix <- sapply(entries_appendix, grep, bib, value = T)
  output_appendix <- paste("@", output_appendix, sep = "")

  writeLines(unlist(output_main), paste0(output_bib,'_main.bib'))
  writeLines(unlist(output_appendix), paste0(output_bib,'_appendix.bib'))

}