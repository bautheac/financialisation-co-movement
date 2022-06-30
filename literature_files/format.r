caption_latex <- function(x){
  x <- gsub("(\\d)\\%", "\\1\\\\%", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("&", "\\\\&", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("\\\\\\*", "*", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("\\\\@ref\\((.+?)\\)", "\\\\ref{\\1}", x, ignore.case = TRUE, perl = TRUE)
  x
}

show_table <- function(type, table){
  if (type == 'html') { 
    table <- kable_classic(table, full_width = TRUE, html_font = html_font) 
  } 
  else if (type == 'latex') {
    table <- kable_classic(
      table, font_size = tables_font_size, 
      latex_options = latex_options, repeat_header_text = latex_repeat_header_text, repeat_header_method = latex_repeat_header_method
    )
  }

  table
}

#' Extract result table from result tables data frame provided
#'
#' @param tables A data frame with result tables. Columns are:
#'  - analysis: name of the analysis.
#'  - result: nested result table.
#' @param analysis A string with the analysis name
#' @return A data frame with the result table
get_table <- function(tables, analysis){
  results <- tables[tables$analysis  == analysis, "results"]
  if (NCOL(results) > 1L) { 
    message <- paste0("Multiple tables found for analysis: ", analysis)
    message(message) 
  } else {
    results[[1]]
  }
}