library(magrittr)

significance <- function(p.value, estimate){
  if(p.value <= 0.01) paste0("***", estimate)
  else if (p.value > 0.01 && p.value <= 0.05) paste0("**", estimate)
  else if (p.value > 0.05 && p.value <= 0.10) paste0("*", estimate)
  else estimate
}

percentize <- function(value) paste0(round(value, digits = 4L) * 100, "%")

factors <- c("market", "CHP", "OI nearby", "OI aggregate", "term structure")
sector_levels <- c("all", "agriculturals", "energy", "metals")
subsector_levels <- c("all", "grains", "livestock", "softs", "gas", "petroleum", "base", "precious")

# descriptive stats ####
stats <- readr::read_rds(
  here::here("explore", "results", "descriptive-statistics-clean.rds")
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

# factors <- dplyr::filter(stats, analysis == "factors") %>%
#   dplyr::select(results) %>% tidyr::unnest(results) %>%
#   dplyr::filter(`asset pool` == "US commodities") %>%
#   dplyr::select(results) %>% tidyr::unnest(results) %>%
#   dplyr::filter(leg == "factor", !is.na(period)) %>% dplyr::select(
#     -c(
#       `asset pool`, `update frequency`, `return frequency`, `ranking period`, 
#        `long threshold`, `short threshold`, year
#     )
#   ) %>% dplyr::mutate_if(.predicate = is.list, .funs = unlist)

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

# factors <- dplyr::filter(factors, regime == "all") %>%
#   dplyr::select(-c(regime, min, max)) %>% 
#   dplyr::mutate(asset = paste0(name, " factor")) %>%
#   dplyr::select(-name) %>% 
#   dplyr::mutate(
#     mean = percentize(mean), sd = percentize(sd),
#     mean = purrr::map2_chr(p.value, mean, ~ significance(.x, .y))
#   ) %>% dplyr::select(-p.value) %>%
#   tidyr::pivot_longer(
#     cols = c("mean", "sd"), names_to = "estimate", values_to = "value"
#   ) %>% tidyr::pivot_wider(names_from = "period", values_from = "value")

whole <- dplyr::bind_rows(commodities, countries)


tables <- tibble::tibble(
  analysis = "descriptive statistics (no regimes)", results = list(whole)
)


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

# factors <- dplyr::filter(factors, regime != "all") %>%
#   dplyr::select(-c(min, max)) %>% 
#   dplyr::mutate(asset = paste0(name, " factor")) %>%
#   dplyr::select(-name) %>% 
#   dplyr::mutate(
#     mean = percentize(mean), sd = percentize(sd),
#     mean = purrr::map2_chr(p.value, mean, ~ significance(.x, .y))
#   ) %>% dplyr::select(-p.value) %>%
#   tidyr::pivot_longer(
#     cols = c("mean", "sd"), names_to = "estimate", values_to = "value"
#   ) %>% tidyr::pivot_wider(names_from = "period", values_from = "value")

regimes <- dplyr::bind_rows(commodities, countries)


tables <- dplyr::bind_rows(
  tables,
  tibble::tibble(
    analysis = "descriptive statistics (regimes)", results = list(regimes)
  )
)
  
readr::write_rds(tables, here::here("explore", "tables", "tables.rds"))


## averages ####
statistics_commodity_futures_market_individuals_periods_returns <-
  dplyr::filter(stats, analysis == "commodity futures") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(analysis == "market variables") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(assets == "individual commodities") %>%
  dplyr::select(-assets) %>%
  tidyr::unnest(results) %>% 
  dplyr::filter(type == "returns", period != "year") %>%
  dplyr::select(-c(ticker, type, year)) %>%
  dplyr::select(
    country, sector, subsector, commodity, field, dplyr::everything()
  ) %>%
  dplyr::mutate_at(.vars = c("mean", "p.value"), .funs = unlist) %>%
  dplyr::arrange(dplyr::desc(country), sector, subsector, commodity)

averages <- dplyr::filter(
  statistics_commodity_futures_market_individuals_periods_returns,
  field == "close price", frequency == "daily"
) %>% 
  dplyr::mutate(
    country = factor(country, levels = c("US", "GB")),
    period = factor(period, levels = c("past", "financialization", "crisis", "present"))
  ) %>%
  dplyr::group_by(country, period, regime) %>% 
  dplyr::select(-c(min, max, p.value)) %>%
  dplyr::summarise_if(.predicate = is.numeric, .funs = mean, na.rm = TRUE) %>%
  dplyr::ungroup() %>%
  dplyr::mutate_if(.predicate = is.numeric, .funs = percentize)







# proportions ####

proportions <- readr::read_rds(
  here::here("explore", "results", "proportions-clean.rds")
) %>% dplyr::select(factor, period, sector, subsector, pick, long, short) %>%
  dplyr::mutate_at(.vars = c("long", "short"), .funs = percentize) %>% 
  dplyr::mutate(
    period = factor(period, levels = c("past", "financialization", "crisis", "present")),
    factor = factor(factor, levels = c("market", factors[ factors != "market"]))
  ) %>% dplyr::arrange(factor, period, sector, subsector, pick) %>%
  dplyr::select(-c(sector, subsector)) %>% 
  dplyr::filter(factor != "OI aggregate")

tables <- dplyr::bind_rows(
  tables,
  tibble::tibble(
    analysis = "proportions", results = list(proportions)
  )
)

# tables$results[[3L]] <- proportions


# regressions ####
regressions <- readr::read_rds(
  here::here("explore", "results", "regressions-time-series-clean.rds")
)

## US commodities ~ factors from US commodities ####
`US commodities ~ factors from US commodities` <- dplyr::filter(
  regressions, analysis == "US commodities ~ factors from US commodities"
) %>% dplyr::select(results) %>% tidyr::unnest(results) %>% 
  dplyr::filter(regressors %in% factors) %>% 
  dplyr::group_by(regressors, leg, period, regime) %>%
  dplyr::summarise(`average rsquared` = mean(rsquared, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    period = factor(period, levels = c("past", "financialization", "crisis", "present")),
    regressors = factor(regressors, levels = c("market", factors[ factors != "market"])),
    `average rsquared` = percentize(`average rsquared`)
  ) %>% dplyr::arrange(regressors, leg, period, regime) %>% 
  tidyr::pivot_wider(names_from = "period", values_from = "average rsquared") %>%
  dplyr::rename(factor = regressors) %>% dplyr::filter(factor != "OI aggregate")
  
# tables <- readr::read_rds(here::here("explore", "tables", "tables.rds"))
# tables$results[[4L]] <- `US commodities ~ factors from US commodities`

# tables <- dplyr::bind_rows(
#   tables,
#   tibble::tibble(
#     analysis = "regressions - time series - US commodities ~ factors from US commodities", 
#     results = list(`US commodities ~ factors from US commodities`)
#   )
# )

# readr::write_rds(tables, here::here("explore", "tables", "tables.rds"))

## factor picks ~ factors from picks ####
`factor picks ~ factors from picks` <- dplyr::filter(
  regressions, analysis == "factor picks ~ factors from picks"
) %>% dplyr::select(results) %>% tidyr::unnest(results) %>% 
  dplyr::filter(
    # regressors %in% factors, `factor leg` == "factor", regime == "all"
    # regressors %in% factors, regime == "all"
    regressors %in% factors
  ) %>% 
  # dplyr::group_by(`picking factor name`, period, regressors) %>%
  # dplyr::group_by(`picking factor name`, period, regressors, leg = `factor leg`) %>%
  dplyr::group_by(`picking factor name`, period, regressors, leg = `factor leg`, regime) %>%
  dplyr::summarise(`average rsquared` = mean(rsquared, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::rename(`picking factor` = `picking factor name`, factor = regressors) %>%
  dplyr::mutate_at(
    .vars = c("picking factor", "factor"), 
    .funs = ~ factor(.x, levels = c("market", factors[ factors != "market"]))
  ) %>% dplyr::mutate(`average rsquared` = percentize(`average rsquared`)) %>%
  dplyr::arrange(`picking factor`, period, factor) %>%
  tidyr::pivot_wider(names_from = "period", values_from = "average rsquared") %>%
  dplyr::filter(`picking factor` != "OI aggregate", factor != "OI aggregate")

# tables <- readr::read_rds(here::here("explore", "tables", "tables.rds"))
# tables1 <- dplyr::slice(tables, 1L:4L)
# tables2 <- dplyr::slice(tables, 5L)
# tables <- dplyr::bind_rows(
#   tables1,
#   tibble::tibble(
#     analysis = "regressions - time series - factor picks ~ factors from picks",
#     results = list(`factor picks ~ factors from picks`)
#   ),
#   tables2
# )
# readr::write_rds(tables, here::here("explore", "tables", "tables.rds"))

# tables <- readr::read_rds(here::here("explore", "tables", "tables.rds"))
# tables$results[[5L]] <- `factor picks ~ factors from picks`
# readr::write_rds(tables, here::here("explore", "tables", "tables.rds"))


## US commodities ~ factors from picks ####
`US commodities ~ factors from picks` <- dplyr::filter(
  regressions, analysis == "US commodities ~ factors from picks"
) %>% dplyr::select(results) %>% tidyr::unnest(results) %>% 
  dplyr::filter(
    # regressors %in% factors, `factor leg` == "factor", regime == "all"
    # regressors %in% factors, regime == "all"
    regressors %in% factors
  ) %>% 
  # dplyr::group_by(`picking factor name`, period, regressors) %>%
  # dplyr::group_by(`picking factor name`, period, regressors, leg = `factor leg`) %>%
  dplyr::group_by(`picking factor name`, period, regressors, leg = `factor leg`, regime) %>%
  dplyr::summarise(`average rsquared` = mean(rsquared, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::rename(`picking factor` = `picking factor name`, factor = regressors) %>%
  dplyr::mutate_at(
    .vars = c("picking factor", "factor"), 
    .funs = ~ factor(.x, levels = c("market", factors[ factors != "market"]))
  ) %>% dplyr::mutate(`average rsquared` = percentize(`average rsquared`)) %>%
  dplyr::arrange(`picking factor`, period, factor) %>%
  tidyr::pivot_wider(names_from = "period", values_from = "average rsquared") %>%
  dplyr::filter(`picking factor` != "OI aggregate", factor != "OI aggregate")

# tables <- readr::read_rds(here::here("explore", "tables", "tables.rds"))
# tables1 <- dplyr::slice(tables, 1L:5L)
# tables2 <- dplyr::slice(tables, 6L)
# tables <- dplyr::bind_rows(
#   tables1,
#   tibble::tibble(
#     analysis = "regressions - time series - US commodities ~ factors from picks",
#     results = list(`US commodities ~ factors from picks`)
#   ),
#   tables2
# )
# readr::write_rds(tables, here::here("explore", "tables", "tables.rds"))

# tables <- readr::read_rds(here::here("explore", "tables", "tables.rds"))
# tables$results[[6L]] <- `US commodities ~ factors from picks`
# readr::write_rds(tables, here::here("explore", "tables", "tables.rds"))


## UK metals ~ factors from picks ####
`UK metals ~ factors from picks` <- dplyr::filter(
  regressions, analysis == "UK metals ~ factors from picks"
) %>% dplyr::select(results) %>% tidyr::unnest(results) %>% 
  dplyr::filter(
    # regressors %in% factors, `factor leg` == "factor", regime == "all"
    # regressors %in% factors, regime == "all"
    regressors %in% factors
  ) %>% 
  # dplyr::group_by(`picking factor name`, period, regressors) %>%
  # dplyr::group_by(`picking factor name`, period, regressors, leg = `factor leg`) %>%
  dplyr::group_by(`picking factor name`, period, regressors, leg = `factor leg`, regime) %>%
  dplyr::summarise(`average rsquared` = mean(rsquared, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::rename(`picking factor` = `picking factor name`, factor = regressors) %>%
  dplyr::mutate_at(
    .vars = c("picking factor", "factor"), 
    .funs = ~ factor(.x, levels = c("market", factors[ factors != "market"]))
  ) %>% dplyr::mutate(`average rsquared` = percentize(`average rsquared`)) %>%
  dplyr::arrange(`picking factor`, period, factor) %>%
  tidyr::pivot_wider(names_from = "period", values_from = "average rsquared") %>%
  dplyr::filter(`picking factor` != "OI aggregate", factor != "OI aggregate")

# tables <- readr::read_rds(here::here("explore", "tables", "tables.rds"))
# tables1 <- dplyr::slice(tables, 1L:6L)
# tables2 <- dplyr::slice(tables, 7L)
# tables <- dplyr::bind_rows(
#   tables1,
#   tibble::tibble(
#     analysis = "regressions - time series - UK metals ~ factors from picks",
#     results = list(`UK metals ~ factors from picks`)
#   ),
#   tables2
# )
# readr::write_rds(tables, here::here("explore", "tables", "tables.rds"))


## Commodity returns ~ CHP ####

### US commodity returns ~ US commodity individual CHP ####
subsectors <- dplyr::filter(
  regressions, analysis == "US commodity returns ~ US commodity individual CHP"
) %>% dplyr::select(results) %>% tidyr::unnest(results) %>% 
  dplyr::filter(
    regressor == "pressure change contemporaneous"
  ) %>%
  dplyr::group_by(
    country, sector, subsector, period, regressor, regime.CHP.type, CHP.regime
  ) %>%
  dplyr::summarise(`average rsquared` = mean(rsquared, na.rm = TRUE))
sectors <- dplyr::filter(
  regressions, analysis == "US commodity returns ~ US commodity individual CHP"
) %>% dplyr::select(results) %>% tidyr::unnest(results) %>% 
  dplyr::filter(
    regressor == "pressure change contemporaneous"
  ) %>%
  dplyr::group_by(
    country, sector, period, regressor, regime.CHP.type, CHP.regime
  ) %>%
  dplyr::summarise(`average rsquared` = mean(rsquared, na.rm = TRUE)) %>%
  dplyr::mutate(subsector = "all")
countries <- dplyr::filter(
  regressions, analysis == "US commodity returns ~ US commodity individual CHP"
) %>% dplyr::select(results) %>% tidyr::unnest(results) %>% 
  dplyr::filter(
    regressor == "pressure change contemporaneous"
  ) %>%
  dplyr::group_by(
    country, period, regressor, regime.CHP.type, CHP.regime
  ) %>%
  dplyr::summarise(`average rsquared` = mean(rsquared, na.rm = TRUE)) %>%
  dplyr::mutate(sector = "all", subsector = "all")

`US commodity returns ~ US commodity individual CHP` <- dplyr::bind_rows(
  countries, sectors, subsectors 
) %>%
  dplyr::mutate(
    # regressor = "Δ commodity CHP",
    `average rsquared` = percentize(`average rsquared`),
    sector = factor(sector, levels = sector_levels),
    subsector = factor(subsector, levels = subsector_levels)
  ) %>% 
  dplyr::ungroup() %>%
  dplyr::filter(regime.CHP.type %in% c("individual", "none")) %>%
  dplyr::select(sector, subsector, period, regime = CHP.regime, `average rsquared`) %>%
  dplyr::arrange(sector, subsector, period, regime) %>%
  tidyr::pivot_wider(names_from = period, values_from = `average rsquared`)

tables <- readr::read_rds(here::here("explore", "tables", "tables.rds"))
tables1 <- dplyr::slice(tables, 1L:7L)
tables2 <- dplyr::slice(tables, 8L)
tables <- dplyr::bind_rows(
  tables1,
  tibble::tibble(
    analysis = "regressions - time series - US commodity returns ~ US commodity individual CHP",
    results = list(`US commodity returns ~ US commodity individual CHP`)
  ),
  tables2
)
readr::write_rds(tables, here::here("explore", "tables", "tables.rds"))


### US commodity returns ~ US commodity aggregate CHP ####
subsectors <- dplyr::filter(
  regressions, analysis == "US commodity returns ~ US commodity aggregate CHP"
) %>% dplyr::select(results) %>% tidyr::unnest(results) %>% 
  dplyr::filter(
    regressor == "pressure change contemporaneous"
  ) %>%
  dplyr::group_by(
    country, sector, subsector, period, regressor, regime.CHP.type, CHP.regime
  ) %>%
  dplyr::summarise(`average rsquared` = mean(rsquared, na.rm = TRUE))
sectors <- dplyr::filter(
  regressions, analysis == "US commodity returns ~ US commodity aggregate CHP"
) %>% dplyr::select(results) %>% tidyr::unnest(results) %>% 
  dplyr::filter(
    regressor == "pressure change contemporaneous"
  ) %>%
  dplyr::group_by(
    country, sector, period, regressor, regime.CHP.type, CHP.regime
  ) %>%
  dplyr::summarise(`average rsquared` = mean(rsquared, na.rm = TRUE)) %>%
  dplyr::mutate(subsector = "all")
countries <- dplyr::filter(
  regressions, analysis == "US commodity returns ~ US commodity aggregate CHP"
) %>% dplyr::select(results) %>% tidyr::unnest(results) %>% 
  dplyr::filter(
    regressor == "pressure change contemporaneous"
  ) %>%
  dplyr::group_by(
    country, period, regressor, regime.CHP.type, CHP.regime
  ) %>%
  dplyr::summarise(`average rsquared` = mean(rsquared, na.rm = TRUE)) %>%
  dplyr::mutate(sector = "all", subsector = "all")

`US commodity returns ~ US commodity aggregate CHP` <- dplyr::bind_rows(
  countries, sectors, subsectors 
) %>%
  dplyr::mutate(
    # regressor = "Δ aggregate CHP", 
    `average rsquared` = percentize(`average rsquared`),
    sector = factor(sector, levels = sector_levels),
    subsector = factor(subsector, levels = subsector_levels)
  ) %>% 
  dplyr::ungroup() %>%
  dplyr::filter(regime.CHP.type %in% c("aggregate", "none")) %>%
  dplyr::select(sector, subsector, period, regime = CHP.regime, `average rsquared`) %>%
  dplyr::arrange(sector, subsector, period, regime) %>%
  tidyr::pivot_wider(names_from = period, values_from = `average rsquared`)

tables <- readr::read_rds(here::here("explore", "tables", "tables.rds"))
tables1 <- dplyr::slice(tables, 1L:8L)
tables2 <- dplyr::slice(tables, 9L)
tables <- dplyr::bind_rows(
  tables1,
  tibble::tibble(
    analysis = "regressions - time series - US commodity returns ~ US commodity aggregate CHP",
    results = list(`US commodity returns ~ US commodity aggregate CHP`)
  ),
  tables2
)
readr::write_rds(tables, here::here("explore", "tables", "tables.rds"))


## US commodity returns ~ US commodity CHP ####
`US commodity returns ~ CHP` <- dplyr::bind_rows(
  dplyr::mutate(
    `US commodity returns ~ US commodity individual CHP`,
    regressor = "Δ% commodity CHP"
  ),
  dplyr::mutate(
    `US commodity returns ~ US commodity aggregate CHP`,
    regressor = "Δ% aggregate CHP"
  )
) %>% 
  dplyr::filter(regime == "all") %>%
  dplyr::select(-regime)

tables <- readr::read_rds(here::here("explore", "tables", "tables.rds"))
tables1 <- dplyr::slice(tables, 1L:9L)
tables2 <- dplyr::slice(tables, 10L)
tables <- dplyr::bind_rows(
  tables1,
  tibble::tibble(
    analysis = "regressions - time series - US commodity returns ~ US commodity CHP",
    results = list(`US commodity returns ~ CHP`)
  ),
  tables2
)
readr::write_rds(tables, here::here("explore", "tables", "tables.rds"))

## UK commodity returns ~ US commodity aggregate CHP ####
# `UK commodity returns ~ US commodity aggregate CHP` <- dplyr::filter(
#   regressions, analysis == "UK commodity returns ~ US commodity aggregate CHP"
# ) %>% dplyr::select(results) %>% tidyr::unnest(results) %>% 
#   # dplyr::filter(
#   #   regressor == "pressure level contemporaneous"
#   # ) %>%
#   dplyr::group_by(period, regressor, regime.CHP.type, CHP.regime) %>%
#   dplyr::summarise(`average rsquared` = mean(rsquared, na.rm = TRUE))  %>%
#   dplyr::mutate(
#     `asset pool` = "GB commodities",
#     # regressor = "Δ aggregate CHP",
#     `average rsquared` = percentize(`average rsquared`)
#   ) %>%
#   dplyr::select(`asset pool`, dplyr::everything())
# 
# tables <- readr::read_rds(here::here("explore", "tables", "tables.rds"))
# tables1 <- dplyr::slice(tables, 1L:9L)
# tables2 <- dplyr::slice(tables, 10L)
# tables <- dplyr::bind_rows(
#   tables1,
#   tibble::tibble(
#     analysis = "regressions - time series - US commodity returns ~ US commodity CHP",
#     results = list(`US commodity returns ~ CHP`)
#   ),
#   tables2
# )
# readr::write_rds(tables, here::here("explore", "tables", "tables.rds"))


readr::write_rds(
  dplyr::slice(tables, c(1:7, 11)),
  here::here("explore", "tables", "tables.rds")
)







# correlations ####
correlations <- readr::read_rds(
  here::here("explore", "results", "correlations-clean.rds")
)

## amongst factor picks ####
picks <- dplyr::filter(correlations, `asset pool` == "factor picks") %>% 
  dplyr::select(results) %>% tidyr::unnest(results) %>%
  dplyr::filter(
    type == "returns", frequency == "day", field == "close price", period != "year"
  ) %>%
  dplyr::select(factor, period, regime, average_correlation) %>%
  dplyr::mutate(
    average_correlation = percentize(average_correlation),
    factor = factor(factor, levels = c("market", factors[ factors != "market"])),
  ) %>%
  tidyr::pivot_wider(names_from = period, values_from = average_correlation) %>%
  dplyr::arrange(factor, regime) %>% dplyr::filter(factor != "OI aggregate")
  
# tables$results[[5L]] <- picks
tables <- dplyr::bind_rows(
  tables,
  tibble::tibble(
    analysis = "average correlations (amongst factor picks)", results = list(picks)
  )
)

readr::write_rds(tables, here::here("explore", "tables", "tables.rds"))




tables <- readr::read_rds(here::here("explore", "tables", "tables.rds"))
tidyr::unnest(tables[1, 2])






