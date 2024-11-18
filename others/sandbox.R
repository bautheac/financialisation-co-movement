
pacman::p_load(kableExtra, magrittr, slituR)

urls <- c(
  "https://www.dropbox.com/scl/fi/n7t8g3hn32d8e5gf2rcti/references.bib?rlkey=xy251n44q3y0t3omxpqk0dvyf&dl=1",
  "https://www.dropbox.com/scl/fi/c46nb5xpxwgvjdspla9p5/style.css?rlkey=d666hnaxod2stvkxel33or0cg&dl=1"
)
file_names <- c("bibliography.bib", "style.css")
purrr::walk2(urls, file_names, function(url, file_name) download.file(url, file_name))


tables_directory_path <- here::here("explore", "tables", "revision-jfm")


tables_collapse_rows_align <- "top"
tables_cut <- TRUE
tables_hline <- "major"

tables <- readr::read_rds(paste_forward_slash(tables_directory_path, "tables-formatted.rds"))
sectors <- c("country", "sector", "sub-sector")
periods <- c("past", "financialisation", "crisis", "post-crisis")

make_table <- function(table_name, columns_to_collapse_rows_for, colnames, align, bold_columns, coloured_columns){
  get_results(tables, table_name) %>% collapse_rows(columns_to_collapse_rows_for) %>%
  kbl(booktabs = T, longtable = T, col.names = colnames, align = align) %>%
  kable_styling(latex_options = "striped", font_size = 7L) %>%
  row_spec(0L, bold = TRUE, color = "black") %>%
  column_spec(bold_columns, bold = TRUE) %>% column_spec(coloured_columns, color = "#4285f4")
}

colnames <- c("asset", "regime", "estimate", periods)
align <- make_series_of_repeated_chars(list(l = 3L, r = 4L))

test <- make_table("stats - combined", c("asset", "regime"), colnames, align, 1L:7L, 4L:7L)


remotes::install_github("bautheac/slituR")
