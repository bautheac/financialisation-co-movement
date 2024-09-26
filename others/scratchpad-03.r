library(magrittr)

# Globals ####

## variables ####
results_directory_path <- here::here("explore", "results", "revision-jfm")
tables_directory_path <- here::here("explore", "tables", "revision-jfm")
factors <- c("market", "CHP", "open interest nearby", "open interest aggregate", "term structure")
sector_levels <- c("all", "agriculturals", "energy", "metals")
subsector_levels <- c("all", "grains", "livestock", "softs", "gas", "petroleum", "base", "precious")
sort_levels <- c(
  "all-all-all", "US-all-all", "US-agriculturals-all", "US-agriculturals-grains",
  "US-agriculturals-livestock", "US-agriculturals-softs", "US-energy-all", 
  "US-energy-gas", "US-energy-petroleum", "US-metals-all", "US-metals-base", 
  "US-metals-precious", "GB-all-all"
)

## functions ####
paste_forward_slash <- function(...) paste(..., sep = "/")
significance <- function(p.value, estimate){
  if(p.value <= 0.01) paste0("***", estimate)
  else if (p.value > 0.01 && p.value <= 0.05) paste0("**", estimate)
  else if (p.value > 0.05 && p.value <= 0.10) paste0("*", estimate)
  else estimate
}
percentize <- function(value) paste0(round(value, digits = 4L) * 100, "%")
sort_table_by_country_sector_subsector <- function(tb, sort_levels){
  dplyr::mutate(
    tb,
    sort = paste(country, sector, subsector, sep = "-"),
    sort = factor(sort, levels = sort_levels)
  ) %>% dplyr::arrange(sort) %>% dplyr::select(-sort)
}

# regressions ####
## all commodity returns ~ factors ####
regressions_factors <- readr::read_rds(
  paste_forward_slash(results_directory_path, "regressions-factors.rds")
)

`all commodity returns ~ factors` <- dplyr::filter(
  regressions_factors, field == "close price", type == "return", frequency == "day", 
  timespan == "period", factor != "open interest aggregate"
) %>%
  dplyr::mutate(
    factor = factor %>% stringr::str_replace_all("open interest nearby", "open interest")
    ) %>%
  dplyr::select(country, sector, subsector, period, factor, leg, regime, average) %>%
  dplyr::mutate(average = percentize(average)) %>%
  tidyr::pivot_wider(names_from = "period", values_from = "average") %>%
  sort_table_by_country_sector_subsector(sort_levels)
