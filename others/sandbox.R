library(magrittr)
library(slituR)
library(kableExtra)


tables_directory_path <- here::here("explore", "tables", "revision-jfm")
tables <- readr::read_rds(paste_forward_slash(tables_directory_path, "tables-formatted.rds"))
sectors <- c("country", "sector", "sub-sector")
periods <- c("past", "financialisation", "crisis", "post-crisis")



collapse_rows <- function(df, columns){
  dplyr::mutate(df, row = dplyr::row_number(), .by = columns) %>%
    dplyr::mutate(dplyr::across(columns, ~ifelse(row == 1L, ., "")))
}

make_table <- function(table_name, columns_to_collapse_rows_for, colnames, align, bold_columns, coloured_columns){
  get_results(tables, table_name) %>% collapse_rows(columns_to_collapse_rows_for) %>%
    kbl(booktabs = T, longtable = T, col.names = colnames, align = align) %>%
    kable_styling(latex_options = "striped", font_size = 7L) %>%
    row_spec(0L, bold = TRUE, color = "black") %>%
    column_spec(bold_columns, bold = TRUE) %>% column_spec(coloured_columns, color = "#4285f4")
}

colnames <- c("asset", "regime", "estimate", periods)
align <- make_series_of_repeated_chars(list(l = 3L, r = 4L))

make_table("stats - combined", c("asset", "regime"), colnames, align, 1L:7L, 4L:7L)



