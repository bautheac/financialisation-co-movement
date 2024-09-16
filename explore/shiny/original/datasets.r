library(magrittr)

# data(tickers_futures, package = "BBGsymbols")

factor_parameters <- tibble::tibble(
  `update frequency` = "week",
  `return frequency` = "day",
  `ranking period` = 26L,
  `long threshold` = 0.67,
  `short threshold` = 0.33,
  weighted = TRUE
)

periods <- tibble::tibble(
  period = c(
    rep("past", 2L), rep("financialization", 2L), 
    rep("crisis", 2L), rep("present", 2L)
  ), 
  bound = rep(c("start", "end"), 4L),
  date = c(
    "1997-07-01", "2003-12-31", "2004-01-01", "2008-09-14", "2008-09-15", 
    "2013-06-19", "2013-06-20", "2018-12-31"
  )
)

# Descriptive statistics ####
statistics <- readr::read_rds("results/descriptive-statistics-clean.rds")

## Commodity futures 

### market variables 

#### individuals

##### by year 

###### levels 
# Commodity futures - market variables - individuals - by year - levels
statistics_commodity_futures_market_individuals_years_levels <-
  dplyr::filter(statistics, analysis == "commodity futures") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(analysis == "market variables") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(assets == "individual commodities") %>%
  dplyr::select(-assets) %>%
  tidyr::unnest(results) %>% 
  dplyr::filter(type == "levels", period == "year") %>%
  dplyr::select(-c(ticker, type, frequency, period)) %>%
  dplyr::select(
    country, sector, subsector, commodity, field, dplyr::everything()
  ) %>%
  dplyr::mutate(year = as.integer(year)) %>%
  dplyr::mutate_at(.vars = c("mean", "p.value"), .funs = unlist) %>%
  dplyr::arrange(dplyr::desc(country), sector, subsector, commodity)

###### returns 
# Commodity futures - market variables - individuals - by year - returns
statistics_commodity_futures_market_individuals_years_returns <-
  dplyr::filter(statistics, analysis == "commodity futures") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(analysis == "market variables") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(assets == "individual commodities") %>%
  dplyr::select(-assets) %>%
  tidyr::unnest(results) %>% 
  dplyr::filter(type == "returns", period == "year") %>%
  dplyr::select(-c(ticker, type, period)) %>%
  dplyr::select(
    country, sector, subsector, commodity, field, dplyr::everything()
  ) %>%
  dplyr::mutate(year = as.integer(year)) %>%
  dplyr::mutate_at(.vars = c("mean", "p.value"), .funs = unlist) %>%
  dplyr::arrange(dplyr::desc(country), sector, subsector, commodity)

##### by period 

###### levels 
# Commodity futures - market variables - individuals - by period - levels
statistics_commodity_futures_market_individuals_periods_levels <-
  dplyr::filter(statistics, analysis == "commodity futures") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(analysis == "market variables") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(assets == "individual commodities") %>%
  dplyr::select(-assets) %>%
  tidyr::unnest(results) %>% 
  dplyr::filter(type == "levels", period != "year") %>%
  dplyr::select(-c(ticker, type, frequency, year)) %>%
  dplyr::select(
    country, sector, subsector, commodity, field, dplyr::everything()
  ) %>%
  dplyr::mutate_at(.vars = c("mean", "p.value"), .funs = unlist) %>%
  dplyr::arrange(dplyr::desc(country), sector, subsector, commodity)

###### Returns 
# Commodity futures - market variables - individuals - by period - returns
statistics_commodity_futures_market_individuals_periods_returns <-
  dplyr::filter(statistics, analysis == "commodity futures") %>%
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


#### groups 

##### by year 

###### levels 
# Commodity futures - market variables - individuals - by year - levels
statistics_commodity_futures_market_groups_years_levels <-
  dplyr::filter(statistics, analysis == "commodity futures") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(analysis == "market variables") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(assets == "countries - sectors - subsectors") %>%
  dplyr::select(-assets) %>%
  tidyr::unnest(results) %>% 
  dplyr::filter(type == "levels", period == "year") %>%
  dplyr::select(-c(type, frequency, period)) %>%
  dplyr::mutate(year = as.integer(year)) %>%
  dplyr::mutate_at(.vars = c("mean", "p.value"), .funs = unlist)

###### returns 
# Commodity futures - market variables - individuals - by year - returns
statistics_commodity_futures_market_groups_years_returns <-
  dplyr::filter(statistics, analysis == "commodity futures") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(analysis == "market variables") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(assets == "countries - sectors - subsectors") %>%
  dplyr::select(-assets) %>%
  tidyr::unnest(results) %>% 
  dplyr::filter(type == "returns", period == "year") %>%
  dplyr::select(-c(type, period)) %>%
  dplyr::select(field, dplyr::everything()) %>%
  dplyr::mutate(year = as.integer(year)) %>%
  dplyr::mutate_at(.vars = c("mean", "p.value"), .funs = unlist)

##### by period 

###### levels 
# Commodity futures - market variables - individuals - by period - levels
statistics_commodity_futures_market_groups_periods_levels <-
  dplyr::filter(statistics, analysis == "commodity futures") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(analysis == "market variables") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(assets == "countries - sectors - subsectors") %>%
  dplyr::select(-assets) %>%
  tidyr::unnest(results) %>% 
  dplyr::filter(type == "levels", period != "year") %>%
  dplyr::select(-c(type, frequency, year)) %>%
  dplyr::select(field, dplyr::everything()) %>%
  dplyr::mutate_at(.vars = c("mean", "p.value"), .funs = unlist)

###### Returns 
# Commodity futures - market variables - individuals - by period - returns
statistics_commodity_futures_market_groups_periods_returns <-
  dplyr::filter(statistics, analysis == "commodity futures") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(analysis == "market variables") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(assets == "countries - sectors - subsectors") %>%
  dplyr::select(-assets) %>%
  tidyr::unnest(results) %>% 
  dplyr::filter(type == "returns", period != "year") %>%
  dplyr::select(-c(type, year)) %>%
  dplyr::select(field, dplyr::everything()) %>%
  dplyr::mutate_at(.vars = c("mean", "p.value"), .funs = unlist)




### position variables 

#### individuals 

##### by year 

###### levels 
# Commodity futures - position variables - individuals - by year - levels
statistics_commodity_futures_cftc_individuals_years_levels <-
  dplyr::filter(statistics, analysis == "commodity futures") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(analysis == "CFTC") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(assets == "individual commodities") %>%
  dplyr::select(-assets) %>%
  tidyr::unnest(results) %>% 
  dplyr::filter(type == "levels", period == "year") %>%
  dplyr::select(-c(ticker, type, frequency, period)) %>%
  dplyr::select(
    country, sector, subsector, commodity, participant, dplyr::everything()
  ) %>%
  dplyr::mutate(year = as.integer(year)) %>%
  dplyr::mutate_at(.vars = c("mean", "p.value"), .funs = unlist) %>%
  dplyr::arrange(dplyr::desc(country), sector, subsector, commodity, participant)

###### returns 
# Commodity futures - position variables - individuals - by year - returns
statistics_commodity_futures_cftc_individuals_years_returns <-
  dplyr::filter(statistics, analysis == "commodity futures") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(analysis == "CFTC") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(assets == "individual commodities") %>%
  dplyr::select(-assets) %>%
  tidyr::unnest(results) %>% 
  dplyr::filter(type == "returns", period == "year") %>%
  dplyr::select(-c(ticker, type, period)) %>%
  dplyr::select(
    country, sector, subsector, commodity, participant, dplyr::everything()
  ) %>%
  dplyr::mutate(year = as.integer(year)) %>%
  dplyr::mutate_at(.vars = c("mean", "p.value"), .funs = unlist) %>%
  dplyr::arrange(dplyr::desc(country), sector, subsector, commodity, participant)


##### by period 

###### levels 
# Commodity futures - market variables - individuals - by period - levels
statistics_commodity_futures_cftc_individuals_periods_levels <-
  dplyr::filter(statistics, analysis == "commodity futures") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(analysis == "CFTC") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(assets == "individual commodities") %>%
  dplyr::select(-assets) %>%
  tidyr::unnest(results) %>% 
  dplyr::filter(type == "levels", period != "year") %>%
  dplyr::select(-c(ticker, type, frequency, year)) %>%
  dplyr::select(
    country, sector, subsector, commodity, participant, dplyr::everything()
  ) %>%
  dplyr::mutate_at(.vars = c("mean", "p.value"), .funs = unlist) %>%
  dplyr::arrange(dplyr::desc(country), sector, subsector, commodity, participant)

###### Returns 
# Commodity futures - position variables - individuals - by period - returns
statistics_commodity_futures_cftc_individuals_periods_returns <-
  dplyr::filter(statistics, analysis == "commodity futures") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(analysis == "CFTC") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(assets == "individual commodities") %>%
  dplyr::select(-assets) %>%
  tidyr::unnest(results) %>% 
  dplyr::filter(type == "returns", period != "year") %>%
  dplyr::select(-c(ticker, type, year)) %>%
  dplyr::select(
    country, sector, subsector, commodity, participant, dplyr::everything()
  ) %>%
  dplyr::mutate_at(.vars = c("mean", "p.value"), .funs = unlist) %>%
  dplyr::arrange(dplyr::desc(country), sector, subsector, commodity, participant)

#### groups 

##### by year 

###### levels 
# Commodity futures - market variables - individuals - by year - levels
statistics_commodity_futures_cftc_groups_years_levels <-
  dplyr::filter(statistics, analysis == "commodity futures") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(analysis == "CFTC") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(assets == "countries - sectors - subsectors") %>%
  dplyr::select(-assets) %>%
  tidyr::unnest(results) %>% 
  dplyr::filter(type == "levels", period == "year") %>%
  dplyr::select(-c(type, frequency, period)) %>%
  dplyr::mutate(year = as.integer(year)) %>%
  dplyr::mutate_at(.vars = c("mean", "p.value"), .funs = unlist)

###### returns 
# Commodity futures - position variables - individuals - by year - returns
statistics_commodity_futures_cftc_groups_years_returns <-
  dplyr::filter(statistics, analysis == "commodity futures") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(analysis == "CFTC") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(assets == "countries - sectors - subsectors") %>%
  dplyr::select(-assets) %>%
  tidyr::unnest(results) %>% 
  dplyr::filter(type == "returns", period == "year") %>%
  dplyr::select(-c(type, period)) %>%
  dplyr::select(participant, dplyr::everything()) %>%
  dplyr::mutate(year = as.integer(year)) %>%
  dplyr::mutate_at(.vars = c("mean", "p.value"), .funs = unlist)

##### by period 

###### levels 
# Commodity futures - position variables - individuals - by period - levels
statistics_commodity_futures_cftc_groups_periods_levels <-
  dplyr::filter(statistics, analysis == "commodity futures") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(analysis == "CFTC") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(assets == "countries - sectors - subsectors") %>%
  dplyr::select(-assets) %>%
  tidyr::unnest(results) %>% 
  dplyr::filter(type == "levels", period != "year") %>%
  dplyr::select(-c(type, frequency, year)) %>%
  dplyr::select(participant, dplyr::everything()) %>%
  dplyr::mutate_at(.vars = c("mean", "p.value"), .funs = unlist)

###### Returns 
# Commodity futures - position variables - individuals - by period - returns
statistics_commodity_futures_cftc_groups_periods_returns <-
  dplyr::filter(statistics, analysis == "commodity futures") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(analysis == "CFTC") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(assets == "countries - sectors - subsectors") %>%
  dplyr::select(-assets) %>%
  tidyr::unnest(results) %>% 
  dplyr::filter(type == "returns", period != "year") %>%
  dplyr::select(-c(type, year)) %>%
  dplyr::select(participant, dplyr::everything()) %>%
  dplyr::mutate_at(.vars = c("mean", "p.value"), .funs = unlist)


## factors 
statistics_factors <-
  dplyr::filter(statistics, analysis == "factors") %>%
  dplyr::select(-analysis) %>% tidyr::unnest(results)

### asset pool: US commodities 
statistics_factors_US_commos <- dplyr::filter(
  statistics_factors,
  `asset pool` == "US commodities"
) %>%
  dplyr::select(-`asset pool`) %>%
  tidyr::unnest(results) %>%
  dplyr::select(-`asset pool`) %>%
  dplyr::mutate_at(
    .vars = c("long threshold", "short threshold"), .funs = round, digits = 2L
  ) %>%
  dplyr::mutate_if(.predicate = is.list, .fun = unlist) %>%
  dplyr::filter(name != "market")

#### by year 
statistics_factors_US_commos_years <- dplyr::filter(
  statistics_factors_US_commos, is.na(period)
) %>% dplyr::select(-period) %>% dplyr::mutate(year = as.integer(year))

#### by period 
statistics_factors_US_commos_periods <- dplyr::filter(
  statistics_factors_US_commos, ! is.na(period)
) %>% dplyr::select(-year)

### asset pool: factor picks 
statistics_factors_factor_picks <- dplyr::filter(
  statistics_factors,
  `asset pool` == "factor picks"
) %>%
  dplyr::select(-`asset pool`) %>%
  tidyr::unnest(results) %>%
  dplyr::select(-`asset pool`) %>%
  dplyr::mutate_at(
    .vars = c("long threshold", "short threshold"), .funs = round, digits = 2L
  ) %>%
  dplyr::mutate_if(.predicate = is.list, .fun = unlist)

#### by year 
statistics_factors_factor_picks_years <- dplyr::filter(
  statistics_factors_factor_picks, is.na(period)
) %>%
  dplyr::select(-period) %>%
  dplyr::mutate(year = as.integer(year))

#### by period 
statistics_factors_factor_picks_periods <- dplyr::filter(
  statistics_factors_factor_picks, ! is.na(period)
) %>%
  dplyr::select(-year) %>%
  dplyr::mutate(period = as.character(period))





# tests of differences ####
differences <- readr::read_rds("results/tests-differences-clean.rds")

## periods 
`between periods` <- dplyr::filter(differences, analysis == "periods") %>%
  dplyr::select(results) %>% tidyr::unnest(results)

### Commodity futures 

#### market variables 

##### individuals

###### levels
# Commodity futures - market variables - individuals - by year - levels
differences_periods_commodity_futures_market_individuals_levels <-
  dplyr::filter(`between periods`, analysis == "commodity futures") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(analysis == "market variables") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(assets == "individual commodities") %>%
  dplyr::select(-assets) %>%
  tidyr::unnest(results) %>% 
  dplyr::filter(type == "levels") %>%
  dplyr::select(-c(ticker, type, frequency)) %>%
  dplyr::select(
    country, sector, subsector, commodity, field, dplyr::everything()
  ) %>% dplyr::arrange(dplyr::desc(country), sector, subsector, commodity)

###### returns
# Commodity futures - market variables - individuals - by year - returns
differences_periods_commodity_futures_market_individuals_returns <-
  dplyr::filter(`between periods`, analysis == "commodity futures") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(analysis == "market variables") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(assets == "individual commodities") %>%
  dplyr::select(-assets) %>%
  tidyr::unnest(results) %>% 
  dplyr::filter(type == "returns") %>%
  dplyr::select(-c(ticker, type)) %>%
  dplyr::select(
    country, sector, subsector, commodity, field, dplyr::everything()
  ) %>% dplyr::arrange(dplyr::desc(country), sector, subsector, commodity)

##### groups

###### levels
# Commodity futures - market variables - individuals - by year - levels
differences_periods_commodity_futures_market_groups_levels <-
  dplyr::filter(`between periods`, analysis == "commodity futures") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(analysis == "market variables") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(assets == "countries - sectors - subsectors") %>%
  dplyr::select(-assets) %>%
  tidyr::unnest(results) %>% 
  dplyr::filter(type == "levels") %>%
  dplyr::select(-c(type, frequency)) %>%
  dplyr::select(
    country, sector, subsector, field, dplyr::everything()
  ) %>% dplyr::arrange(dplyr::desc(country), sector, subsector)

###### returns
# Commodity futures - market variables - individuals - by year - returns
differences_periods_commodity_futures_market_groups_returns <-
  dplyr::filter(`between periods`, analysis == "commodity futures") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(analysis == "market variables") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(assets == "countries - sectors - subsectors") %>%
  dplyr::select(-assets) %>%
  tidyr::unnest(results) %>% 
  dplyr::filter(type == "returns") %>%
  dplyr::select(-c(type)) %>%
  dplyr::select(
    country, sector, subsector, field, dplyr::everything()
  ) %>% dplyr::arrange(dplyr::desc(country), sector, subsector)

#### positions

##### individuals

###### levels
# Commodity futures - positions - individuals - by year - levels
differences_periods_commodity_futures_positions_individuals_levels <-
  dplyr::filter(`between periods`, analysis == "commodity futures") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(analysis == "position variables") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(assets == "individual commodities") %>%
  dplyr::select(-assets) %>%
  tidyr::unnest(results) %>% 
  dplyr::filter(type == "levels") %>%
  dplyr::select(-c(ticker, type, frequency)) %>%
  dplyr::select(
    country, sector, subsector, commodity, participant, dplyr::everything()
  ) %>% dplyr::arrange(dplyr::desc(country), sector, subsector, commodity)

###### returns
# Commodity futures - positions - individuals - by year - returns
differences_periods_commodity_futures_positions_individuals_returns <-
  dplyr::filter(`between periods`, analysis == "commodity futures") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(analysis == "position variables") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(assets == "individual commodities") %>%
  dplyr::select(-assets) %>%
  tidyr::unnest(results) %>% 
  dplyr::filter(type == "returns") %>%
  dplyr::select(-c(ticker, type)) %>%
  dplyr::select(
    country, sector, subsector, commodity, participant, dplyr::everything()
  ) %>% dplyr::arrange(dplyr::desc(country), sector, subsector, commodity)

##### groups

###### levels
# Commodity futures - positions - individuals - by year - levels
differences_periods_commodity_futures_positions_groups_levels <-
  dplyr::filter(`between periods`, analysis == "commodity futures") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(analysis == "position variables") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(assets == "countries - sectors - subsectors") %>%
  dplyr::select(-assets) %>%
  tidyr::unnest(results) %>% 
  dplyr::filter(type == "levels") %>%
  dplyr::select(-c(type, frequency)) %>%
  dplyr::select(
    country, sector, subsector, participant, dplyr::everything()
  ) %>% dplyr::arrange(dplyr::desc(country), sector, subsector)

###### returns
# Commodity futures - positions - individuals - by year - returns
differences_periods_commodity_futures_positions_groups_returns <-
  dplyr::filter(`between periods`, analysis == "commodity futures") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(analysis == "position variables") %>%
  dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::filter(assets == "countries - sectors - subsectors") %>%
  dplyr::select(-assets) %>%
  tidyr::unnest(results) %>% 
  dplyr::filter(type == "returns") %>%
  dplyr::select(-c(type)) %>%
  dplyr::select(
    country, sector, subsector, participant, dplyr::everything()
  ) %>% dplyr::arrange(dplyr::desc(country), sector, subsector)


### factors
differences_periods_factors <- dplyr::filter(
  `between periods`, analysis == "factors"
) %>% dplyr::select(-analysis) %>% tidyr::unnest(results)

#### asset pool: US commodities
differences_periods_factors_US_commos <- dplyr::filter(
  differences_periods_factors, `asset pool` == "US commodities"
) %>% dplyr::select(-`asset pool`) %>% tidyr::unnest(results) %>%
  dplyr::select(-`asset pool`) %>%
  dplyr::mutate_at(
    .vars = c("long threshold", "short threshold"), .funs = round, digits = 2L
  ) %>% dplyr::filter(name != "market")


## regimes
`between regimes` <- dplyr::filter(differences, analysis == "regimes") %>%
  dplyr::select(results) %>% tidyr::unnest(results)

### Commodity futures

#### market variables

##### individuals

###### levels
# Commodity futures - market variables - individuals - by year - levels
differences_regimes_commodity_futures_market_individuals_levels <-
  dplyr::filter(`between regimes`, analysis == "commodity futures") %>%
  dplyr::select(-analysis) %>% tidyr::unnest(results) %>%
  dplyr::filter(analysis == "market variables") %>%
  dplyr::select(-analysis) %>% tidyr::unnest(results) %>%
  dplyr::filter(assets == "individual commodities") %>%
  dplyr::select(-assets) %>% tidyr::unnest(results) %>% 
  dplyr::filter(type == "levels") %>%
  dplyr::select(-c(ticker, type, frequency)) %>%
  dplyr::select(
    country, sector, subsector, commodity, field, dplyr::everything()
  ) %>% dplyr::arrange(dplyr::desc(country), sector, subsector, commodity) %>%
  dplyr::mutate_if(.predicate = is.factor, .funs = as.character)

###### returns
# Commodity futures - market variables - individuals - by year - returns
differences_regimes_commodity_futures_market_individuals_returns <-
  dplyr::filter(`between regimes`, analysis == "commodity futures") %>%
  dplyr::select(-analysis) %>% tidyr::unnest(results) %>%
  dplyr::filter(analysis == "market variables") %>%
  dplyr::select(-analysis) %>% tidyr::unnest(results) %>%
  dplyr::filter(assets == "individual commodities") %>%
  dplyr::select(-assets) %>% tidyr::unnest(results) %>% 
  dplyr::filter(type == "returns") %>% dplyr::select(-c(ticker, type)) %>%
  dplyr::select(
    country, sector, subsector, commodity, field, dplyr::everything()
  ) %>% dplyr::arrange(dplyr::desc(country), sector, subsector, commodity) %>%
  dplyr::mutate_if(.predicate = is.factor, .funs = as.character)

##### groups

###### levels
# Commodity futures - market variables - individuals - by year - levels
differences_regimes_commodity_futures_market_groups_levels <-
  dplyr::filter(`between regimes`, analysis == "commodity futures") %>%
  dplyr::select(-analysis) %>% tidyr::unnest(results) %>%
  dplyr::filter(analysis == "market variables") %>%
  dplyr::select(-analysis) %>% tidyr::unnest(results) %>%
  dplyr::filter(assets == "countries - sectors - subsectors") %>%
  dplyr::select(-assets) %>% tidyr::unnest(results) %>% 
  dplyr::filter(type == "levels") %>%
  dplyr::select(-c(type, frequency)) %>%
  dplyr::select(
    country, sector, subsector, field, dplyr::everything()
  ) %>% dplyr::arrange(dplyr::desc(country), sector, subsector) %>%
  dplyr::mutate_if(.predicate = is.factor, .funs = as.character)

###### returns
# Commodity futures - market variables - individuals - by year - returns
differences_regimes_commodity_futures_market_groups_returns <-
  dplyr::filter(`between regimes`, analysis == "commodity futures") %>%
  dplyr::select(-analysis) %>% tidyr::unnest(results) %>%
  dplyr::filter(analysis == "market variables") %>%
  dplyr::select(-analysis) %>% tidyr::unnest(results) %>%
  dplyr::filter(assets == "countries - sectors - subsectors") %>%
  dplyr::select(-assets) %>% tidyr::unnest(results) %>% 
  dplyr::filter(type == "returns") %>% dplyr::select(-c(type)) %>%
  dplyr::select(
    country, sector, subsector, field, dplyr::everything()
  ) %>% dplyr::arrange(dplyr::desc(country), sector, subsector) %>%
  dplyr::mutate_if(.predicate = is.factor, .funs = as.character)

### factors
differences_regimes_factors <- dplyr::filter(
  `between regimes`, analysis == "factors"
) %>% dplyr::select(-analysis) %>% tidyr::unnest(results)

#### asset pool: US commodities
differences_regimes_factors_US_commos <- dplyr::filter(
  differences_regimes_factors, `asset pool` == "US commodities"
) %>% dplyr::select(-`asset pool`) %>% tidyr::unnest(results) %>%
  dplyr::select(-`asset pool`) %>%
  dplyr::mutate_at(
    .vars = c("long threshold", "short threshold"), .funs = round, digits = 2L
  ) %>% dplyr::mutate_if(.predicate = is.factor, .funs = as.character) %>% 
  dplyr::filter(name != "market")

#### asset pool: factor picks
differences_regimes_factors_from_picks_periods <- dplyr::filter(
  differences_regimes_factors, `asset pool` == "factor picks"
) %>% dplyr::select(-`asset pool`) %>% tidyr::unnest(results) %>%
  dplyr::select(-`asset pool`) %>%
  dplyr::mutate_at(
    .vars = c("long threshold", "short threshold"), .funs = round, digits = 2L
  ) %>% dplyr::mutate_if(.predicate = is.factor, .funs = as.character)









# Factor picks ####
picks <- readr::read_rds("results/factor-picks-clean.rds") %>%
  dplyr::mutate_at(
    .vars = c("long threshold", "short threshold"), .funs = round, digits = 2L
  ) 









# # Regressions - simple
# simple_regressions <- readr::read_rds("results/regressions-simple-clean.rds")
# 
# 
# ## US commodities ~ factors from US commodities
# US_commos_vs_factors_from_US_commos <- dplyr::filter(
#   simple_regressions,
#   analysis == "US commodities ~ factors from US commodities"
# ) %>% dplyr::select(-analysis) %>%
#   tidyr::unnest(results) %>%
#   dplyr::mutate_at(
#     .vars = c("long threshold", "short threshold"),
#     .funs = round,
#     digits = 2L
#   ) %>%
#   dplyr::select(country, sector, subsector, commodity, dplyr::everything()) %>%
#   dplyr::arrange(country, sector, subsector, commodity)
# 
# 
# ## factor picks ~ factors from picks
# factor_picks_vs_factors_from_picks <- dplyr::filter(
#   simple_regressions,
#   analysis == "factor picks ~ factors from picks"
# ) %>% dplyr::select(-analysis) %>%
#   tidyr::unnest(results) %>%
#   dplyr::mutate_at(
#     .vars = c("long threshold", "short threshold"),
#     .funs = round,
#     digits = 2L
#   ) %>%
#   dplyr::mutate_if(.predicate = is.factor, .funs = as.character)
# 
# ## US commodities ~ factors from picks
# US_commodities_vs_factors_from_picks <- dplyr::filter(
#   simple_regressions,
#   analysis == "US commodities ~ factors from picks"
# ) %>% dplyr::select(-analysis) %>%
#   tidyr::unnest(results) %>%
#   dplyr::mutate_at(
#     .vars = c("long threshold", "short threshold"),
#     .funs = round,
#     digits = 2L
#   ) %>%
#   dplyr::mutate_if(.predicate = is.factor, .funs = as.character)
# 
# ## UK metals ~ factors from picks (regressor: factor)
# UK_metals_vs_factors_from_picks <- dplyr::filter(
#   simple_regressions,
#   analysis == "UK metals ~ factors from picks (regressor: factor)"
# ) %>% dplyr::select(-analysis) %>%
#   tidyr::unnest(results) %>%
#   dplyr::mutate_at(
#     .vars = c("long threshold", "short threshold"),
#     .funs = round,
#     digits = 2L
#   ) %>%
#   dplyr::mutate_if(.predicate = is.factor, .funs = as.character)
# 
# ## UK metals ~ factors from picks (regressor: factor leg)
# UK_metals_vs_factors_from_picks_legs <- dplyr::filter(
#   simple_regressions,
#   analysis == "UK metals ~ factors from picks (regressor: factor leg)"
# ) %>% dplyr::select(-analysis) %>%
#   tidyr::unnest(results) %>%
#   dplyr::mutate_at(
#     .vars = c("long threshold", "short threshold"),
#     .funs = round,
#     digits = 2L
#   ) %>%
#   dplyr::mutate_if(.predicate = is.factor, .funs = as.character)














# Regressions ####

## Time series

time_series_regressions <- readr::read_rds(
  "results/regressions-time-series-clean.rds"
)


## US commodities ~ factors from US commodities
time_series_US_commos_vs_factors_from_US_commos <- dplyr::filter(
  time_series_regressions,
  analysis == "US commodities ~ factors from US commodities"
) %>% dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::mutate_at(
    .vars = c("long threshold", "short threshold"), .funs = round, digits = 2L
  ) %>%
  dplyr::mutate(
    regressors = stringr::str_replace_all(regressors, "`", "")
  ) %>%
  dplyr::select(country, sector, subsector, commodity, dplyr::everything()) %>%
  dplyr::arrange(country, sector, subsector, commodity)



## factor picks ~ factors from picks
time_series_factor_picks_vs_factors_from_picks <- dplyr::filter(
  time_series_regressions,
  analysis == "factor picks ~ factors from picks"
) %>% dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::mutate_at(
    .vars = c("long threshold", "short threshold"), .funs = round, digits = 2L
  ) %>%
  dplyr::mutate(
    regressors = stringr::str_replace_all(regressors, "`", "")
  ) %>%
  dplyr::mutate_if(.predicate = is.factor, .funs = as.character) %>%
  dplyr::select(country, sector, subsector, pick, dplyr::everything()) %>%
  dplyr::arrange(country, sector, subsector, pick)


## US commodities ~ factors from picks
time_series_US_commodities_vs_factors_from_picks <- dplyr::filter(
  time_series_regressions,
  analysis == "US commodities ~ factors from picks"
) %>% dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::mutate_at(
    .vars = c("long threshold", "short threshold"), .funs = round, digits = 2L
  ) %>%
  dplyr::mutate(
    regressors = stringr::str_replace_all(regressors, "`", "")
  ) %>%
  dplyr::mutate_if(.predicate = is.factor, .funs = as.character) %>%
  dplyr::select(country, sector, subsector, commodity, dplyr::everything()) %>%
  dplyr::arrange(country, sector, subsector, commodity)

## UK metals ~ factors from picks (regressor: factor)
time_series_UK_metals_vs_factors_from_picks <- dplyr::filter(
  time_series_regressions,
  analysis == "UK metals ~ factors from picks"
) %>% dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::mutate_at(
    .vars = c("long threshold", "short threshold"), .funs = round, digits = 2L
  ) %>%
  dplyr::mutate(
    regressors = stringr::str_replace_all(regressors, "`", "")
  ) %>%
  dplyr::mutate_if(.predicate = is.factor, .funs = as.character) %>%
  dplyr::select(country, sector, subsector, commodity, dplyr::everything()) %>%
  dplyr::arrange(country, sector, subsector, commodity)

# ## UK metals ~ factors from picks (regressor: factor leg)
# time_series_UK_metals_vs_factors_from_picks_legs <- dplyr::filter(
#   time_series_regressions,
#   analysis == "UK metals ~ factors from picks (regressor: factor leg)"
# ) %>% dplyr::select(-analysis) %>%
#   tidyr::unnest(results) %>%
#   dplyr::mutate_at(
#     .vars = c("long threshold", "short threshold"), .funs = round, digits = 2L
#   ) %>%
#   dplyr::mutate(
#     regressors = stringr::str_replace_all(regressors, "`", "")
#   ) %>%
#   dplyr::mutate_if(.predicate = is.factor, .funs = as.character) %>%
#   dplyr::select(country, sector, subsector, commodity, dplyr::everything()) %>%
#   dplyr::arrange(country, sector, subsector, commodity)

## US commodity returns ~ US commodity individual CHP
time_series_US_commos_vs_US_individual_CHP <- dplyr::filter(
  time_series_regressions,
  analysis == "US commodity returns ~ US commodity individual CHP"
) %>% dplyr::select(-analysis) %>%
  tidyr::unnest(results)

## US commodity returns ~ US commodity aggregate CHP
time_series_US_commos_vs_US_aggregate_CHP <- dplyr::filter(
  time_series_regressions,
  analysis == "US commodity returns ~ US commodity aggregate CHP"
) %>% dplyr::select(-analysis) %>%
  tidyr::unnest(results)

## UK commodity returns ~ US commodity aggregate CHP
time_series_UK_commos_vs_US_aggregate_CHP <- dplyr::filter(
  time_series_regressions,
  analysis == "UK commodity returns ~ US commodity aggregate CHP"
) %>% dplyr::select(-analysis) %>%
  tidyr::unnest(results)



## Cross-section

cross_section_regressions <- readr::read_rds(
  "results/regressions-cross-section-clean.rds"
)

## US commodities ~ factors from US commodities
cross_section_US_commos_vs_factors_from_US_commos <- dplyr::filter(
  cross_section_regressions,
  analysis == "US commodities ~ factors from US commodities"
) %>% dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::mutate_at(
    .vars = c("long threshold", "short threshold"), .funs = round, digits = 2L
  ) 


## factor picks ~ factors from picks
cross_section_factor_picks_vs_factors_from_picks <- dplyr::filter(
  cross_section_regressions,
  analysis == "factor picks ~ factors from picks"
) %>% dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::mutate_at(
    .vars = c("long threshold", "short threshold"), .funs = round, digits = 2L
  ) %>%
  dplyr::mutate_if(.predicate = is.factor, .funs = as.character)


## US commodities ~ factors from picks
cross_section_US_commodities_vs_factors_from_picks <- dplyr::filter(
  cross_section_regressions,
  analysis == "US commodities ~ factors from picks"
) %>% dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::mutate_at(
    .vars = c("long threshold", "short threshold"), .funs = round, digits = 2L
  ) %>%
  dplyr::mutate_if(.predicate = is.factor, .funs = as.character)


## UK metals ~ factors from picks
cross_section_UK_metals_vs_factors_from_picks <- dplyr::filter(
  cross_section_regressions,
  analysis == "UK metals ~ factors from picks"
) %>% dplyr::select(-analysis) %>%
  tidyr::unnest(results) %>%
  dplyr::mutate_at(
    .vars = c("long threshold", "short threshold"), .funs = round, digits = 2L
  ) %>% dplyr::mutate_if(.predicate = is.factor, .funs = as.character)


## UK metals ~ factors from picks (regressor: factor leg)
# cross_section_UK_metals_vs_factors_from_picks_legs <- dplyr::filter(
#   cross_section_regressions,
#   analysis == "UK metals ~ factors from picks (regressor: factor leg)"
# ) %>% dplyr::select(-analysis) %>%
#   tidyr::unnest(results) %>%
#   dplyr::mutate_at(
#     .vars = c("long threshold", "short threshold"), .funs = round, digits = 2L
#   ) %>%
#   dplyr::mutate_if(.predicate = is.factor, .funs = as.character)




regression_factors <- stringr::str_split(
  unique(time_series_US_commos_vs_factors_from_US_commos$regressors),
  " \\+ "
) %>% do.call(c, args = .) %>% unique()
















# Proportions ####

proportions <- readr::read_rds("results/proportions-clean.rds") %>%
  dplyr::mutate_at(
    .vars = c("long threshold", "short threshold"), .funs = round, digits = 2L
  ) %>%
  dplyr::mutate_if(.predicate = is.factor, .funs = as.character)







# Correlations ####

correlations <- readr::read_rds("results/correlations-clean.rds")

## Among factor picks
correlations_factor_picks <- dplyr::filter(
  correlations, `asset pool` == "factor picks"
) %>%
  dplyr::select(-`asset pool`) %>% tidyr::unnest(results)  %>%
  dplyr::mutate_at(
    .vars = c("long threshold", "short threshold"), .funs = round, digits = 2L
  )

### by year

#### levels
correlations_factor_picks_years_levels <- dplyr::filter(
  correlations_factor_picks, type == "levels", period == "year"
) %>% dplyr::select(-c(type, period))

#### returns
correlations_factor_picks_years_returns <- dplyr::filter(
  correlations_factor_picks, type == "returns", period == "year"
) %>% dplyr::select(-c(type, period))

### by period

#### levels
correlations_factor_picks_periods_levels <- dplyr::filter(
  correlations_factor_picks, type == "levels", period != "year"
) %>% dplyr::select(-c(type, year))

#### returns
correlations_factor_picks_periods_returns <- dplyr::filter(
  correlations_factor_picks, type == "returns", period != "year"
) %>% dplyr::select(-c(type, year))


## Among US commodities
correlations_US_commodities <- dplyr::filter(
  correlations, `asset pool` == "US commodities"
) %>% dplyr::select(-`asset pool`) %>% tidyr::unnest(results)

### by year

#### levels
correlations_US_commodities_years_levels <- dplyr::filter(
  correlations_US_commodities, type == "levels", period == "year"
) %>% dplyr::select(-c(type, period))

#### returns
correlations_US_commodities_years_returns <- dplyr::filter(
  correlations_US_commodities, type == "returns", period == "year"
) %>% dplyr::select(-c(type, period))

### by period

#### levels
correlations_US_commodities_periods_levels <- dplyr::filter(
  correlations_US_commodities, type == "levels", period != "year"
) %>% dplyr::select(-c(type, year))

#### returns
correlations_US_commodities_periods_returns <- dplyr::filter(
  correlations_US_commodities, type == "returns", period != "year"
) %>% dplyr::select(-c(type, year))




## Among US commodities (excluding metals)
correlations_US_commodities_no_metals <- dplyr::filter(
  correlations, `asset pool` == "US commodities (excluding metals)"
) %>%
  dplyr::select(-`asset pool`) %>% 
  tidyr::unnest(results)

### by year

#### levels
correlations_US_commodities_no_metals_years_levels <- dplyr::filter(
  correlations_US_commodities_no_metals, type == "levels", period == "year"
) %>%
  dplyr::select(-c(type, period))

#### returns
correlations_US_commodities_no_metals_years_returns <- dplyr::filter(
  correlations_US_commodities_no_metals, type == "returns", period == "year"
) %>%
  dplyr::select(-c(type, period))

### by period

#### levels
correlations_US_commodities_no_metals_periods_levels <- dplyr::filter(
  correlations_US_commodities_no_metals, type == "levels", period != "year"
) %>%
  dplyr::select(-c(type, year))

#### returns
correlations_US_commodities_no_metals_periods_returns <- dplyr::filter(
  correlations_US_commodities_no_metals, type == "returns", period != "year"
) %>%
  dplyr::select(-c(type, year))


## Among US metals
correlations_US_metals <- dplyr::filter(
  correlations, `asset pool` == "US metals"
) %>%
  dplyr::select(-`asset pool`) %>% 
  tidyr::unnest(results)

### by year

#### levels
correlations_US_metals_years_levels <- dplyr::filter(
  correlations_US_metals, type == "levels", period == "year"
) %>%
  dplyr::select(-c(type, period))

#### returns
correlations_US_metals_years_returns <- dplyr::filter(
  correlations_US_metals, type == "returns", period == "year"
) %>%
  dplyr::select(-c(type, period))

### by period

#### levels
correlations_US_metals_periods_levels <- dplyr::filter(
  correlations_US_metals, type == "levels", period != "year"
) %>%
  dplyr::select(-c(type, year))

#### returns
correlations_US_metals_periods_returns <- dplyr::filter(
  correlations_US_metals, type == "returns", period != "year"
) %>%
  dplyr::select(-c(type, year))





## Among UK metals
correlations_UK_metals <- dplyr::filter(
  correlations, `asset pool` == "UK metals"
) %>%
  dplyr::select(-`asset pool`) %>% tidyr::unnest(results)

### by year

#### levels
correlations_UK_metals_years_levels <- dplyr::filter(
  correlations_UK_metals, type == "levels", period == "year"
) %>%
  dplyr::select(-c(type, period))

#### returns
correlations_UK_metals_years_returns <- dplyr::filter(
  correlations_UK_metals, type == "returns", period == "year"
) %>%
  dplyr::select(-c(type, period))

### by period

#### levels
correlations_UK_metals_periods_returns <- dplyr::filter(
  correlations_UK_metals, type == "levels", period != "year"
) %>%
  dplyr::select(-c(type, year))

#### returns
correlations_UK_metals_periods_levels <- dplyr::filter(
  correlations_UK_metals, type == "returns", period != "year"
) %>%
  dplyr::select(-c(type, year))


## Among US energy
# correlations_US_energy <- dplyr::filter(
#   correlations, `asset pool` == "US energy"
# ) %>% dplyr::select(-`asset pool`) %>% tidyr::unnest(results)
# 
# ### by year
# 
# #### levels 
# correlations_US_energy_years_levels <- dplyr::filter(
#   correlations_US_energy, type == "levels", period == "year"
# ) %>% dplyr::select(-c(type, period))
# 
# #### returns
# correlations_US_energy_years_returns <- dplyr::filter(
#   correlations_US_energy, type == "returns", period == "year"
# ) %>% dplyr::select(-c(type, period))
# 
# ### by period
# 
# #### levels
# correlations_US_energy_periods_returns <- dplyr::filter(
#   correlations_US_energy, type == "levels", period != "year"
# ) %>% dplyr::select(-c(type, year))
# 
# #### returns
# correlations_US_energy_periods_levels <- dplyr::filter(
#   correlations_US_energy, type == "returns", period != "year"
# ) %>% dplyr::select(-c(type, year))
