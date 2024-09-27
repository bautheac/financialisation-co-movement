library(magrittr)

# Globals ####
## datasets ####
data("tickers_futures", "tickers_cftc", package = "BBGsymbols")
data("exchanges", package = "fewISOs")

## variables ####
factors <- c("market", "CHP", "open interest nearby", "open interest aggregate", "term structure")
results_directory_path <- here::here("explore", "results", "revision-jfm")
sector_levels <- c("all", "agriculturals", "energy", "metals")
sort_levels <- c(
  "all-all-all", "US-all-all", "US-agriculturals-all", "US-agriculturals-grains",
  "US-agriculturals-livestock", "US-agriculturals-softs", "US-energy-all", 
  "US-energy-gas", "US-energy-petroleum", "US-metals-all", "US-metals-base", 
  "US-metals-precious", "GB-all-all"
)
subsector_levels <- c("all", "grains", "livestock", "softs", "gas", "petroleum", "base", "precious")
tables_directory_path <- here::here("explore", "tables", "revision-jfm")
commodity_futures_tickers <- c(
  "BOA Comdty", "C A Comdty", "CCA Comdty", "CLA Comdty", "CTA Comdty", 
  "FCA Comdty", "GCA Comdty", "HGA Comdty", "HOA Comdty", "JOA Comdty", 
  "KCA Comdty", "LAA Comdty", "LBA Comdty", "LCA Comdty", "LHA Comdty", 
  "LLA Comdty", "LNA Comdty", "LPA Comdty", "LTA Comdty", "LXA Comdty", 
  "NGA Comdty", "O A Comdty", "PAA Comdty", "PLA Comdty", "S A Comdty", 
  "SBA Comdty", "SIA Comdty", "SMA Comdty", "W A Comdty", "XBWA Comdty",
  "LAA Comdty", "LPA Comdty", "LLA Comdty", "LNA Comdty", "LTA Comdty",
  "LXA Comdty"
)

## functions ####
make_asset_name_country_sector_subsector_dataframe <- function(tickers){
  
  dplyr::left_join(
    dplyr::filter(tickers_futures, ticker %in% tickers) %>% 
      dplyr::select(ticker, name, sector, subsector, MIC), 
    dplyr::select(exchanges, MIC, country), 
    by = "MIC"
  ) %>% dplyr::select(-ticker)
}
paste_forward_slash <- function(...) paste(..., sep = "/")
percentize <- function(value) paste0(round(value, digits = 4L) * 100, "%")
significance <- function(p.value, estimate){
  if(p.value <= 0.01) paste0("***", estimate)
  else if (p.value > 0.01 && p.value <= 0.05) paste0("**", estimate)
  else if (p.value > 0.05 && p.value <= 0.10) paste0("*", estimate)
  else estimate
}
sort_table_by_country_sector_subsector <- function(tb, sort_levels){
  dplyr::mutate(
    tb,
    sort = paste(country, sector, subsector, sep = "-"),
    sort = factor(sort, levels = sort_levels)
  ) %>% dplyr::arrange(sort) %>% dplyr::select(-sort)
}

# asset taxonomy ####
`asset taxonomy` <- 
  make_asset_name_country_sector_subsector_dataframe(commodity_futures_tickers) %>%
  dplyr::mutate(
    asset = paste0(name, " (", MIC, ")"),
    country = factor(country, levels = c("US", "GB"))
    ) %>% dplyr::select(-c(name, MIC)) %>%
  dplyr::relocate(asset, .before = dplyr::everything()) %>%
  dplyr::relocate(country, .after = asset) %>%
  dplyr::arrange(country, sector, subsector) %>%
  dplyr::mutate(asset = dplyr::case_when(
    asset == "Orange juice-frozen concentrated (IFUS)" ~ "Orange juice (IFUS)",
    asset == "Lumber-random length (XCME)" ~ "Lumber (XCME)",
    asset == "Aluminium-primary (XLME)" ~ "Aluminium (XLME)",
    asset == "Lead-refined pig (XLME)" ~ "Lead (XLME)",
    TRUE ~ asset
  ))

# descriptive stats ####
stats <- readr::read_rds(
  paste_forward_slash(results_directory_path, "descriptive-statistics-clean.rds")
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

## combined ####
commodities <- dplyr::select(commodities_market_individuals, -c(min, max)) %>% 
  dplyr::mutate(
    mean = percentize(mean), sd = percentize(sd),
    mean = purrr::map2_chr(p.value, mean, ~ significance(.x, .y))
  ) %>% dplyr::select(-p.value) %>%
  tidyr::pivot_longer(
    cols = c("mean", "sd"), names_to = "estimate", values_to = "value"
  ) %>% tidyr::pivot_wider(names_from = "period", values_from = "value") %>%
  dplyr::arrange(dplyr::desc(country), sector, subsector, commodity, regime) %>%
  dplyr::select(-c(country, sector, subsector)) %>%
  dplyr::rename(asset = commodity)

countries <- dplyr::select(commodities_market_countries, -c(min, max, sector, subsector)) %>% 
  dplyr::mutate(asset = paste(country, "commodities", sep = " ")) %>%
  dplyr::select(-country) %>% 
  dplyr::mutate(
    mean = percentize(mean), sd = percentize(sd),
    mean = purrr::map2_chr(p.value, mean, ~ significance(.x, .y))
  ) %>% dplyr::select(-p.value) %>%
  tidyr::pivot_longer(
    cols = c("mean", "sd"), names_to = "estimate", values_to = "value"
  ) %>% tidyr::pivot_wider(names_from = "period", values_from = "value")

# q: this only works in the html output, μ and σ appear as blank in the pdf output; how to fix this?

descriptive_stats_combined <- dplyr::bind_rows(commodities, countries) %>%
  dplyr::mutate(
    regime = ifelse(regime == "all", "whole period", regime),
    estimate = ifelse(estimate == "mean", estimate, "volatility")
    )


# correlations ####
correlations <- readr::read_rds(
  paste_forward_slash(results_directory_path, "correlations.rds")
)

## By period ####
correlations_periods <- dplyr::filter(
  correlations, field == "close price", type == "return", frequency == "day", timespan == "period"
) %>%
  dplyr::select(country, sector, subsector, period, regime, average) %>%
  dplyr::group_by(country, sector, subsector, period, regime) %>%
  dplyr::slice_tail(n = 1L) %>% dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = "period", values_from = "average") %>%
  sort_table_by_country_sector_subsector(sort_levels)

## By year ####
correlations_years <- dplyr::filter(
  correlations, field == "close price", type == "return", frequency == "day", 
  timespan == "year", regime == "whole period"
) %>%
  dplyr::select(country, sector, subsector, year, average) %>%
  dplyr::group_by(country, sector, subsector, year) %>%
  dplyr::slice_tail(n = 1L) %>% dplyr::ungroup() %>%
  dplyr::mutate(
    decade = slituR::floor_year_to_nearest_decade(year),
    year = slituR::remove_decade_from_year(year)
    ) %>%
  sort_table_by_country_sector_subsector(sort_levels) %>%
  tidyr::pivot_wider(names_from = "year", values_from = "average") %>%
  dplyr::relocate(`0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, .after = dplyr::last_col()) %>%
  dplyr::mutate(
    dplyr::across(c(`0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`), ~ format(.x, scientific = FALSE)),
    dplyr::across(c(`0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`), ~ stringr::str_replace_all(.x, "    NA", ""))
    )


# regressions ####

## US commodity returns ~ CHP ####
regressions_CHP <- readr::read_rds(
  paste_forward_slash(results_directory_path, "regressions-time-series-clean.rds")
)

### US commodity returns ~ US commodity individual CHP ####
subsectors <- dplyr::filter(
  regressions_CHP, analysis == "US commodity returns ~ US commodity individual CHP"
) %>% dplyr::select(results) %>% tidyr::unnest(results) %>% 
  dplyr::filter(regressor == "pressure change contemporaneous") %>%
  dplyr::group_by(country, sector, subsector, period, regressor, regime.CHP.type, CHP.regime) %>%
  dplyr::summarise(`average rsquared` = mean(rsquared, na.rm = TRUE)) %>%
  dplyr::ungroup()
sectors <- dplyr::filter(
  regressions_CHP, analysis == "US commodity returns ~ US commodity individual CHP"
) %>% dplyr::select(results) %>% tidyr::unnest(results) %>% 
  dplyr::filter(regressor == "pressure change contemporaneous") %>%
  dplyr::group_by(country, sector, period, regressor, regime.CHP.type, CHP.regime) %>%
  dplyr::summarise(`average rsquared` = mean(rsquared, na.rm = TRUE)) %>%
  dplyr::mutate(subsector = "all") %>% dplyr::ungroup()
countries <- dplyr::filter(
  regressions_CHP, analysis == "US commodity returns ~ US commodity individual CHP"
) %>% dplyr::select(results) %>% tidyr::unnest(results) %>% 
  dplyr::filter(regressor == "pressure change contemporaneous") %>%
  dplyr::group_by(country, period, regressor, regime.CHP.type, CHP.regime) %>%
  dplyr::summarise(`average rsquared` = mean(rsquared, na.rm = TRUE)) %>%
  dplyr::mutate(sector = "all", subsector = "all") %>% dplyr::ungroup()

`US commodity returns ~ US commodity individual CHP` <- dplyr::bind_rows(
  countries, sectors, subsectors 
) %>%
  dplyr::mutate(
    regressor = "Δ% commodity CHP", 
    `average rsquared` = percentize(`average rsquared`),
    sector = factor(sector, levels = sector_levels),
    subsector = factor(subsector, levels = subsector_levels)
  ) %>% 
  dplyr::filter(regime.CHP.type == "none") %>%
  dplyr::select(regressor, sector, subsector, period, `average rsquared`) %>%
  dplyr::arrange(sector, subsector, period) %>%
  tidyr::pivot_wider(names_from = period, values_from = `average rsquared`)

### US commodity returns ~ US commodity aggregate CHP ####
subsectors <- dplyr::filter(
  regressions_CHP, analysis == "US commodity returns ~ US commodity aggregate CHP"
  ) %>% dplyr::select(results) %>% tidyr::unnest(results) %>% 
  dplyr::filter(regressor == "pressure change contemporaneous") %>%
  dplyr::group_by(
    country, sector, subsector, period, regressor, regime.CHP.type, CHP.regime
  ) %>% dplyr::summarise(`average rsquared` = mean(rsquared, na.rm = TRUE)) %>%
  dplyr::ungroup()
sectors <- dplyr::filter(
  regressions_CHP, analysis == "US commodity returns ~ US commodity aggregate CHP"
) %>% dplyr::select(results) %>% tidyr::unnest(results) %>% 
  dplyr::filter(regressor == "pressure change contemporaneous") %>%
  dplyr::group_by(country, sector, period, regressor, regime.CHP.type, CHP.regime) %>%
  dplyr::summarise(`average rsquared` = mean(rsquared, na.rm = TRUE)) %>%
  dplyr::mutate(subsector = "all") %>% dplyr::ungroup()
countries <- dplyr::filter(
  regressions_CHP, analysis == "US commodity returns ~ US commodity aggregate CHP"
) %>% dplyr::select(results) %>% tidyr::unnest(results) %>% 
  dplyr::filter(regressor == "pressure change contemporaneous") %>%
  dplyr::group_by(country, period, regressor, regime.CHP.type, CHP.regime) %>%
  dplyr::summarise(`average rsquared` = mean(rsquared, na.rm = TRUE)) %>%
  dplyr::mutate(sector = "all", subsector = "all") %>% dplyr::ungroup()

`US commodity returns ~ US commodity aggregate CHP` <- dplyr::bind_rows(
  countries, sectors, subsectors 
) %>%
  dplyr::mutate(
    regressor = "Δ% aggregate CHP",
    `average rsquared` = percentize(`average rsquared`),
    sector = factor(sector, levels = sector_levels),
    subsector = factor(subsector, levels = subsector_levels)
  ) %>% 
  dplyr::filter(regime.CHP.type == "none") %>%
  dplyr::select(regressor, sector, subsector, period, `average rsquared`) %>%
  dplyr::arrange(sector, subsector, period) %>%
  tidyr::pivot_wider(names_from = period, values_from = `average rsquared`)

`US commodity returns ~ CHP` <- dplyr::bind_rows(
  `US commodity returns ~ US commodity individual CHP`, 
  `US commodity returns ~ US commodity aggregate CHP`
  )

## all commodity returns ~ market index ####
regressions_index <- readr::read_rds(
  paste_forward_slash(results_directory_path, "regressions-index.rds")
)

`all commodity returns ~ market index` <- dplyr::filter(
  regressions_index, field == "close price", type == "return", frequency == "day", 
  timespan == "period", regime == "whole period",
  !(country == "US" & sector == "all" & subsector == "all")
) %>%
  dplyr::select(country, sector, subsector, period, average) %>%
  dplyr::group_by(country, sector, subsector, period) %>%
  dplyr::slice_tail(n = 1L) %>% dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = "period", values_from = "average") %>%
  sort_table_by_country_sector_subsector(sort_levels)

## all commodity returns ~ factors ####
regressions_factors <- readr::read_rds(
  paste_forward_slash(results_directory_path, "regressions-factors.rds")
)

`all commodity returns ~ factors` <- dplyr::filter(
  regressions_factors, field == "close price", type == "return", frequency == "day", 
  timespan == "period", factor != "open interest aggregate", leg == "factor"
) %>%
  dplyr::mutate(
    factor = factor %>% stringr::str_replace_all("open interest nearby", "open interest")
    ) %>%
  dplyr::select(country, sector, subsector, period, factor, regime, average) %>%
  dplyr::mutate(average = percentize(average)) %>%
  tidyr::pivot_wider(names_from = "period", values_from = "average") %>%
  sort_table_by_country_sector_subsector(sort_levels)


# export ####
tables <- tibble::tribble(
    ~analysis,                                  ~results,
    "stats - whole",                            descriptive_stats_whole,
    "stats - regimes",                          descriptive_stats_regimes,
    "stats - combined",                         descriptive_stats_combined,
    "regressions - US returns ~ US CHP",        `US commodity returns ~ CHP`,
    "correlations - periods",                   correlations_periods,
    "correlations - years",                     correlations_years,
    "regressions - all returns ~ market index", `all commodity returns ~ market index`,
    "regressions - all returns ~ factors",      `all commodity returns ~ factors`,
    "asset taxonomy",                           `asset taxonomy`
  )

readr::write_rds(
  tables, paste_forward_slash(tables_directory_path, "tables-formatted.rds")
  )

rm(list = ls())