library(magrittr)

significance <- function(p.value, estimate){
  if(p.value <= 0.01) paste0("***", estimate)
  else if (p.value > 0.01 && p.value <= 0.05) paste0("**", estimate)
  else if (p.value > 0.05 && p.value <= 0.10) paste0("*", estimate)
  else estimate
}

percentize <- function(value) paste0(round(value, digits = 4L) * 100, "%")

factors <- c("market", "CHP", "open interest nearby", "open interest aggregate", "term structure")
sector_levels <- c("all", "agriculturals", "energy", "metals")
subsector_levels <- c("all", "grains", "livestock", "softs", "gas", "petroleum", "base", "precious")


# descriptive stats ####
stats <- readr::read_rds(
  here::here("explore", "results", "revision-jfm", "descriptive-statistics-clean.rds")
)

commodities_market_individuals <- dplyr::filter(stats, analysis == "commodity futures") %>%
  dplyr::select(results) %>% tidyr::unnest(results) %>%
  dplyr::filter(analysis == "market variables") %>%
  dplyr::select(results) %>% tidyr::unnest(results) %>%
  dplyr::filter(assets == "individual commodities") %>%
  dplyr::select(results) %>% tidyr::unnest(results) %>%
  dplyr::filter(
    type == "returns", period != "year", frequency == "daily", 
    field == "close price"
  ) %>% dplyr::select(-c(ticker, type, frequency, field, year)) %>%
  dplyr::mutate_if(.predicate = is.list, .funs = unlist)

commodities_market_countries <- dplyr::filter(stats, analysis == "commodity futures") %>%
  dplyr::select(results) %>% tidyr::unnest(results) %>%
  dplyr::filter(analysis == "market variables") %>%
  dplyr::select(results) %>% tidyr::unnest(results) %>%
  dplyr::filter(assets == "countries - sectors - subsectors") %>%
  dplyr::select(results) %>% tidyr::unnest(results) %>%
  dplyr::filter(
    sector == "all", subsector == "all", field == "close price",
    type == "returns", period != "year", frequency == "daily"
  ) %>% dplyr::select(-c(type, frequency, field, year)) %>%
  dplyr::mutate_if(.predicate = is.list, .funs = unlist)

## whole ####
commodities <- dplyr::filter(commodities_market_individuals, regime == "all") %>%
  dplyr::select(-c(regime, min, max)) %>% 
  dplyr::mutate(
    mean = percentize(mean), sd = percentize(sd),
    mean = purrr::map2_chr(p.value, mean, ~ significance(.x, .y))
  ) %>% dplyr::select(-p.value) %>%
  tidyr::pivot_longer(
    cols = c("mean", "sd"), names_to = "estimate", values_to = "value"
  ) %>% tidyr::pivot_wider(names_from = "period", values_from = "value") %>%
  dplyr::arrange(dplyr::desc(country), sector, subsector, commodity) %>%
  dplyr::select(-c(country, sector, subsector)) %>%
  dplyr::rename(asset = commodity)

countries <- dplyr::filter(commodities_market_countries, regime == "all") %>%
  dplyr::select(-c(regime, min, max, sector, subsector)) %>% 
  dplyr::mutate(asset = paste(country, "commodities", sep = " ")) %>%
  dplyr::select(-country) %>% 
  dplyr::mutate(
    mean = percentize(mean), sd = percentize(sd),
    mean = purrr::map2_chr(p.value, mean, ~ significance(.x, .y))
  ) %>% dplyr::select(-p.value) %>%
  tidyr::pivot_longer(
    cols = c("mean", "sd"), names_to = "estimate", values_to = "value"
  ) %>% tidyr::pivot_wider(names_from = "period", values_from = "value")

descriptive_stats_whole <- dplyr::bind_rows(commodities, countries)


## regimes ####
commodities <- dplyr::filter(commodities_market_individuals, regime != "all") %>%
  dplyr::select(-c(min, max)) %>% 
  dplyr::mutate(
    mean = percentize(mean), sd = percentize(sd),
    mean = purrr::map2_chr(p.value, mean, ~ significance(.x, .y))
  ) %>% dplyr::select(-p.value) %>%
  tidyr::pivot_longer(
    cols = c("mean", "sd"), names_to = "estimate", values_to = "value"
  ) %>% tidyr::pivot_wider(names_from = "period", values_from = "value") %>%
  dplyr::arrange(dplyr::desc(country), sector, subsector, commodity) %>%
  dplyr::select(-c(country, sector, subsector)) %>%
  dplyr::rename(asset = commodity)

countries <- dplyr::filter(commodities_market_countries, regime != "all") %>%
  dplyr::select(-c(min, max, sector, subsector)) %>% 
  dplyr::mutate(asset = paste(country, "commodities", sep = " ")) %>%
  dplyr::select(-country) %>% 
  dplyr::mutate(
    mean = percentize(mean), sd = percentize(sd),
    mean = purrr::map2_chr(p.value, mean, ~ significance(.x, .y))
  ) %>% dplyr::select(-p.value) %>%
  tidyr::pivot_longer(
    cols = c("mean", "sd"), names_to = "estimate", values_to = "value"
  ) %>% tidyr::pivot_wider(names_from = "period", values_from = "value")

descriptive_stats_regimes <- dplyr::bind_rows(commodities, countries)



path <- here::here("explore", "tables", "revision-jfm", "regressions-factors.csv")
readr::write_csv(formatted, path)

