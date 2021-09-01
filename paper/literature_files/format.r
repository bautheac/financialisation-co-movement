caption_latex <- function(x){
  x <- gsub("(\\d)\\%", "\\1\\\\%", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("&", "\\\\&", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("\\\\\\*", "*", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("\\\\@ref\\((.+?)\\)", "\\\\ref{\\1}", x, ignore.case = TRUE, perl = TRUE)
  x
}
