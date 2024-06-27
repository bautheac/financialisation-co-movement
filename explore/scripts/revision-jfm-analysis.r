library(finRes); library(magrittr)

# source functions ####
source(paste0(here::here(), "/explore/scripts/revision-jfm-functions.r"))

# globals ####
## datasets ####
data("tickers_futures", "tickers_cftc", package = "BBGsymbols")
data("exchanges", package = "fewISOs")

## variables ####
storethat_db_path <- make_path_to_storethat_database()

period_bounds <- list(
  past = list(start = "1997-07-01", end = "2003-12-31"),
  financialization = list(start = "2004-01-01", end = "2008-09-14"),
  crisis = list(start = "2008-09-15", end = "2013-06-19"),
  present = list(start = "2013-06-20", end = "2018-12-31")
)
periods <- make_periods_data_frame(period_bounds)

rf <- readr::read_rds(here::here("data", "risk-free-rate.rds"))

# load data ####
## time boundaries ####
start <- "1996-01-01"; end <- "2018-12-14"

## individual commodity futures tickers ####
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

## commodity futures levels data ####
### individual data ####
commodity_futures_individual_data_levels <- get_commodity_futures_individual_data_levels(
  commodity_futures_tickers, start, end, storethat_db_path
    )

### aggregate data ####
commodity_futures_aggregate_data_levels <- get_commodity_futures_aggregate_data_levels(
  commodity_futures_tickers, start, end, storethat_db_path
)

### both ####
commodity_futures_data_levels <- bind_individual_and_aggregate_commodity_futures_dataframes(
  commodity_futures_individual_data_levels, commodity_futures_aggregate_data_levels
)

## commodity futures relative changes data ####
commodity_futures_relative_changes <- 
  make_commodity_futures_relative_changes_dataframe(commodity_futures_data_levels)

## bind levels and relative changes ####
commodity_futures_data <- make_commodity_futures_dataframe(
  commodity_futures_data_levels, commodity_futures_relative_changes
)

## period dates ####
period_dates <- make_period_dates_timeseries(commodity_futures_data, periods)

## commodity CFTC tickers ####
commodity_CFTC_tickers <- commodity_futures_tickers[
  commodity_futures_tickers %in% tickers_cftc$`active contract ticker`
]

## commodity CFTC data ####
commodity_CFTC_data <- pullit::pull_futures_CFTC(
  source = "storethat", active_contract_tickers = commodity_CFTC_tickers, 
  start = start, end = end, file = storethat_db_path
)

## aggregate CHP regimes ####
aggregate_CHP_regimes <- make_aggregate_CHP_regimes_dataframe()

## construct commodity index returns ####
US_commodity_futures_tickers <- filter_commodity_futures_tickers(
  commodity_futures_tickers, filter_country = "US"
)

first_period_boundaries <- get_period_boundaries(periods, "past")
last_period_boundaries <- get_period_boundaries(periods, "present")

commodity_futures_index_returns <- make_commodity_futures_index_returns_dataframe(
  commodity_futures_individual_data_levels, US_commodity_futures_tickers, 
  first_period_boundaries$start, last_period_boundaries$end
)

# analysis ####
library(furrr)
plan(multisession, workers = parallel::detectCores())

commodity_pool_tickers <- make_commodity_pool_tickers_dataframe(commodity_futures_tickers)

## correlations ####
### compute raw results ####
correlations_raw <- make_pairwise_correlations_for_ticker_combinations_dataframe(
  commodity_pool_tickers, commodity_futures_data, aggregate_CHP_regimes, period_dates
)

### save raw results ####
path_correlations_raw_file <- paste0(
  here::here(), "/explore/results/revision-jfm/correlations-raw.rds"
)
saveRDS(correlations_raw, path_correlations_raw_file)

### summarise results ####
#### load raw dataset (optional) ####
correlations_raw <- readr::read_rds(path_correlations_raw_file)

#### summarise ####
correlations_summary <- 
  add_top_3_and_average_to_pairwise_correlations_for_ticker_combinations_dataframe(correlations_raw)

#### save summary results ####
path_correlations_summary_file <- paste0(
  here::here(), "/explore/results/revision-jfm/correlations-summary.rds"
)
saveRDS(correlations_summary, path_correlations_summary_file)

### format results ####
#### load summary dataset (optional) ####
correlations_summary <- readr::read_rds(path_correlations_summary_file)

#### format ####
correlations_formatted <- format_correlation_summary_statistics_into_table(correlations_summary)

#### save formatted results ####
path_correlations_formatted_file <- paste0(
  here::here(), "/explore/tables/revision-jfm/correlations.rds"
)

saveRDS(correlations_formatted, path_correlations_formatted_file)

## regressions ####
### index ####
#### raw ####
regressions_index_raw <- make_regressions_index_for_ticker_combinations_dataframe(
  commodity_pool_tickers, commodity_futures_data, commodity_futures_index_returns, 
  aggregate_CHP_regimes, period_dates
)

#### save raw results ####
path_regressions_index_raw_file <- paste0(
  here::here(), "/explore/results/revision-jfm/regressions-index-raw.rds"
  )
saveRDS(regressions_index_raw, path_regressions_index_raw_file)

#### summarise results ####
##### load raw dataset (optional) ####
# regressions_index_raw <- readr::read_rds(path_regressions_index_raw_file)

##### summarise ####  
regressions_index_summary <- 
  add_top_3_and_average_to_regressions_index_for_ticker_combinations_dataframe(regressions_index_raw)

##### save summary results ####
path_regressions_index_summary_file <- paste0(
  here::here(), "/explore/results/revision-jfm/regressions-index-summary.rds"
)
saveRDS(regressions_index_summary, path_regressions_index_summary_file)

#### format ####
##### load summary dataset (optional) ####
# regressions_index_summary <- readr::read_rds(path_regressions_index_summary_file)

##### format ####  
regressions_index_formatted <- format_regression_index_summary_statistics_into_table(regressions_index_summary)

##### save formatted results ####
path_regressions_index_formatted_file <- paste0(
  here::here(), "/explore/tables/revision-jfm/regressions-index.rds"
)
saveRDS(regressions_index_formatted, path_regressions_index_formatted_file)




################################################################################
# Rerun regressions with equally weighted factors
################################################################################


# factor data - asset pool: US commodities ####
## dependent variables: US commodities ####
`US commodity futures tickers` <- dplyr::left_join(
  dplyr::select(tickers_futures, ticker, MIC), 
  dplyr::select(exchanges, MIC, country), by = "MIC"
) %>% 
  dplyr::filter(ticker %in% `commodity futures tickers`, country == "US") %>% 
  dplyr::select(ticker) %>% purrr::flatten_chr()

`commodity futures data` <- pullit::pull_futures_market(
  source = "storethat", type = "term structure", 
  active_contract_tickers = `US commodity futures tickers`,
  start = start, end = end, TS_positions = 1L:2L, 
  roll_type = "A", roll_days = 0L, roll_months = 0L, roll_adjustment = "N", 
  file = storethat
)

`commodity aggregate data` <- pullit::pull_futures_market(
  source = "storethat", type = "aggregate", 
  active_contract_tickers = `US commodity futures tickers`,
  start = start, end = end, file = storethat
)

`commodity CFTC tickers` <- `US commodity futures tickers`[
  `US commodity futures tickers` %in% tickers_cftc$`active contract ticker`
]

`commodity CFTC data` <- pullit::pull_futures_CFTC(
  source = "storethat", active_contract_tickers = `commodity CFTC tickers`, 
  start = start, end = end, file = storethat
)

## construct factors ####
update_frequency = "week"
return_frequency <- "day"
ranking_period <- 26L
long_threshold <- 2/3
short_threshold <- 1/3
weighted = FALSE

market <- factorem::market_factor(
  data = `commodity futures data`, return_frequency = return_frequency, long = T
)


CHP <- factorem::CHP_factor(
  price_data = `commodity futures data`, CHP_data = `commodity CFTC data`, 
  update_frequency = update_frequency, return_frequency = return_frequency,
  ranking_period = ranking_period, 
  long_threshold = long_threshold, short_threshold = short_threshold, 
  weighted = weighted
)

`open interest nearby` <- factorem::OI_nearby_factor(
  data = `commodity futures data`, update_frequency = update_frequency, 
  return_frequency = return_frequency, ranking_period = ranking_period, 
  long_threshold = long_threshold, short_threshold = short_threshold, 
  weighted = weighted
)

`open interest aggregate` <- factorem::OI_aggregate_factor(
  price_data = `commodity futures data`, 
  aggregate_data = `commodity aggregate data`, 
  update_frequency = update_frequency, return_frequency = return_frequency, 
  ranking_period = ranking_period, 
  long_threshold = long_threshold, short_threshold = short_threshold, 
  weighted = weighted
)

`term structure` <- factorem::TS_factor(
  data = `commodity futures data`, update_frequency = update_frequency, 
  return_frequency = return_frequency, front = 1L, back = 2L,
  ranking_period = ranking_period, long_threshold = long_threshold, 
  short_threshold = short_threshold, weighted = weighted
)

factors <- tibble::tibble(
  `asset pool` = rep("US commodities", 5L), 
  name = c("market", "CHP", "OI nearby", "OI aggregate", "term structure"),
  factor = list(
    market, CHP, `open interest nearby`, `open interest aggregate`, `term structure`
  )
)

`factor returns` <- foreach(
  y = 1L:nrow(factors), 
  .combine = dplyr::bind_rows
) %dopar% { 
  library(data.table)
  
  factors$factor[[y]]@returns[
    , `asset pool` := factors$`asset pool`[y]
  ][, name := factors$name[y]] 
  
} %>% tidyr::gather(leg, return, -c(date, name, `asset pool`)) %>% 
  dplyr::filter(! is.na(return)) %>% 
  dplyr::mutate(
    `update frequency` = update_frequency, `return frequency` = return_frequency, 
    `ranking period` = ranking_period, `long threshold` = long_threshold, 
    `short threshold` = short_threshold
  ) %>%
  dplyr::select(
    `asset pool`, `update frequency`, `return frequency`, `ranking period`, 
    `long threshold`, `short threshold`, name, leg, date, return
  )
`futures prices` <- dplyr::left_join(
  `commodity futures data`@data, 
  dplyr::select(
    `commodity futures data`@term_structure_tickers, 
    `active contract ticker`, ticker, `TS position`),
  by = "ticker"
) %>% 
  dplyr::filter(`TS position` == 1L, field == "PX_LAST") %>% 
  dplyr::select(`active contract ticker`, date, value)


## US commodities ~ factors (asset pool: US commodities) ####
factor_names <- ifelse(
  stringr::str_detect(unique(`factor returns`$name), pattern = " ", negate = FALSE),
  paste0("`", unique(`factor returns`$name), "`"), unique(`factor returns`$name)
)

regressors <- lapply(
  seq_along(factor_names),
  function(x){
    combn(factor_names, x) %>% apply(MARGIN = 2L, paste, collapse = " + ")
  }) %>% do.call(c, args = .)

combinations <- expand.grid(
  unique(`factor returns`$`asset pool`), 
  unique(`factor returns`$`update frequency`), 
  unique(`factor returns`$`return frequency`),
  unique(`factor returns`$`ranking period`), 
  unique(`factor returns`$`long threshold`),
  unique(`factor returns`$`short threshold`),
  unique(`factor returns`$leg),
  regressors,
  unique(`futures prices`$`active contract ticker`), 
  unique(periods$period),
  stringsAsFactors = F
) %>% 
  setNames(
    c(
      "asset pool", "update frequency", "return frequency", "ranking period", 
      "long threshold", "short threshold", "leg", "regressors", "commodity", 
      "period"
    )
  ) %>%
  dplyr::filter(
    ! ((stringr::str_detect(regressors, "market")) & (leg %in% c("long", "short")))
  )


### whole periods ####
whole <- foreach(y = 1L:nrow(combinations), .combine = rbind) %dopar% {
  library(magrittr); library(lubridate)
  message(paste(y, nrow(combinations), sep = "/"))
  
  `asset pool` <- combinations[y, "asset pool"]
  `update frequency` <- combinations[y, "update frequency"]
  `return frequency` <- combinations[y, "return frequency"]
  `ranking period` <- combinations[y, "ranking period"]
  `long threshold` <- combinations[y, "long threshold"]
  `short threshold` <- combinations[y, "short threshold"]
  leg <- combinations[y, "leg"]
  regressors <- combinations[y, "regressors"]
  `commodity name` <- combinations[y, "commodity"]
  period <- combinations[y, "period"]
  
  start <- dplyr::filter(periods, period == !! period, bound == "start") %>%
    dplyr::select(date) %>% purrr::flatten_chr()
  
  end <- dplyr::filter(periods, period == !! period, bound == "end") %>%
    dplyr::select(date) %>% purrr::flatten_chr()
  
  futures <- dplyr::filter(
    `futures prices`,
    date >= as.Date(start),
    date <= as.Date(end),
    `active contract ticker` == !! `commodity name`
  ) %>%
    dplyr::select(date, commodity = value)
  
  futures <- if (`return frequency` == "day") {
    dplyr::mutate(
      futures,
      commodity = (commodity / dplyr::lag(commodity, 1L)) - 1L
    )
  } else {
    dplyr::mutate(
      futures,
      year = lubridate::year(date),
      unit = do.call(
        what = eval(parse(text = paste0("lubridate::", `return frequency`))),
        args = list(date)
      )
    ) %>% dplyr::group_by(year, unit) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c(year, unit)) %>%
      dplyr::mutate(commodity = (commodity / dplyr::lag(commodity, 1L)) - 1L)
  }
  
  factor_names <- stringr::str_split(regressors, " \\+ ") %>% unlist() %>%
    stringr::str_replace_all("`", "")
  
  factors <- dplyr::filter(
    `factor returns`,
    `asset pool` == !! `asset pool`,
    `update frequency` == !! `update frequency`,
    `return frequency` == !! `return frequency`,
    `ranking period` == !! `ranking period`,
    `long threshold` == !! `long threshold`,
    `short threshold` == !! `short threshold`,
    name %in% factor_names,
    leg == !! leg,
    date >= as.Date(start), date <= as.Date(end)
  ) %>%
    dplyr::select(date, name, return) %>%
    tidyr::pivot_wider(names_from = name, values_from = return)
  
  # browser()
  `risk free` <- dplyr::filter(
    rf, frequency == `return frequency`, region == "US"
  )
  `risk free` <- if (`return frequency` == "day"){
    dplyr::select(`risk free`, date = period, rf) %>% 
      dplyr::mutate(date = as.Date(date))
  } else {
    dplyr::select(`risk free`, period, rf) %>%
      dplyr::mutate(
        year = stringr::str_extract(period, "^[0-9]{4}"),
        !! `return frequency` := stringr::str_extract(period, "[0-9]{2}$")
      ) %>% dplyr::select(year, !! `return frequency`, rf)
  }
  
  
  # browser()
  data <- if (`return frequency` == "day"){
    tryCatch(
      { 
        data <- dplyr::left_join(futures, factors, by = "date")  %>%
          dplyr::left_join(`risk free`, by = "date")  %>%
          dplyr::arrange(date) %>% na.omit() %>% as.data.frame()
        data[, c("commodity", factor_names)] <-
          data[, c("commodity", factor_names)] - data[, "rf"]
        dplyr::select(data, -rf)
      },
      error = function(e) { NA }
    )
  } else {
    tryCatch(
      {
        data <- dplyr::left_join(futures, factors, by = "date") %>%
          dplyr::mutate(
            year = lubridate::year(date) %>% as.character(),
            !! `return frequency` := do.call(
              what = `return frequency`, args = list(date)
            ) %>% as.character() %>% ifelse(nchar(.) == 1L, paste0("0", .), .)
          ) %>%
          dplyr::left_join(`risk free`, by = c("year", `return frequency`)) %>%
          dplyr::select(-c(year, !!`return frequency`)) %>%
          dplyr::arrange(date) %>% na.omit() %>% as.data.frame()
        data[, c("commodity", factor_names)] <-
          data[, c("commodity", factor_names)] - data[, "rf"]
        dplyr::select(data, -rf)
      },
      error = function(e) { NA }
    )
  }
  
  
  formula <- paste("commodity", regressors, sep = " ~ ")
  
  model <- tryCatch(
    { stats::lm(rlang::parse_expr(formula), data = data) },
    error = function(e) { NA }
  )
  
  if (! is.na(model)){
    coefficients <- broom::tidy(model)
    rsquared <- stats::summary.lm(model)$r.squared
  } else {
    coefficients <- NA
    rsquared <- NA
  }
  
  # browser()
  tibble::tibble(
    `asset pool` = !! `asset pool`,
    `update frequency` = !! `update frequency`,
    `return frequency` = !! `return frequency`,
    `ranking period` = !! `ranking period`,
    `long threshold` = !! `long threshold`,
    `short threshold` = !! `short threshold`,
    regressors = regressors,
    leg = !! leg,
    commodity = !! `commodity name`,
    period = !! period,
    regime = "all",
    coefficients = list(coefficients),
    rsquared = rsquared
  )
}


### aggregate CHP regimes ####
combinations <- expand.grid(
  unique(`factor returns`$`asset pool`), 
  unique(`factor returns`$`update frequency`), 
  unique(`factor returns`$`return frequency`),
  unique(`factor returns`$`ranking period`), 
  unique(`factor returns`$`long threshold`), 
  unique(`factor returns`$`short threshold`),
  regressors,
  unique(`factor returns`$leg), 
  unique(`futures prices`$`active contract ticker`), 
  unique(periods$period),
  dplyr::filter(`aggregate CHP regimes`, ! is.na(period)) %>% 
    dplyr::distinct(regime) %>% purrr::flatten_chr(),
  stringsAsFactors = F) %>% 
  setNames(
    c(
      "asset pool", "update frequency", "return frequency", "ranking period", 
      "long threshold", "short threshold", "regressors", "leg", "commodity", 
      "period", "regime")
  ) %>%
  dplyr::filter(
    ! ((stringr::str_detect(regressors, "market")) & (leg %in% c("long", "short")))
  )

regimes <- foreach(y = 1L:nrow(combinations), .combine = rbind) %dopar% {
  library(magrittr); library(lubridate)
  
  message(paste(y, nrow(combinations), sep = "/"))
  
  `asset pool` <- combinations[y, "asset pool"]
  `update frequency` <- combinations[y, "update frequency"]
  `return frequency` <- combinations[y, "return frequency"]
  `ranking period` <- combinations[y, "ranking period"]
  `long threshold` <- combinations[y, "long threshold"]
  `short threshold` <- combinations[y, "short threshold"]
  regressors <- combinations[y, "regressors"]
  leg <- combinations[y, "leg"]
  `commodity name` <- combinations[y, "commodity"]
  period <- combinations[y, "period"]
  regime <- combinations[y, "regime"]
  
  start <- dplyr::filter(periods, period == !! period, bound == "start") %>%
    dplyr::select(date) %>% purrr::flatten_chr()
  end <- dplyr::filter(periods, period == !! period, bound == "end") %>%
    dplyr::select(date) %>% purrr::flatten_chr()
  
  futures <- dplyr::filter(
    `futures prices`,
    date >= as.Date(start), date <= as.Date(end),
    `active contract ticker` == !! `commodity name`
  ) %>% dplyr::select(date, commodity = value)
  
  futures <- if (`return frequency` == "day") {
    dplyr::mutate(
      futures, commodity = (commodity / dplyr::lag(commodity, 1L)) - 1L
    )
  } else {
    dplyr::mutate(
      futures,
      year = lubridate::year(date),
      unit = do.call(
        what = eval(parse(text = paste0("lubridate::", `return frequency`))),
        args = list(date)
      )
    ) %>% dplyr::group_by(year, unit) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c(year, unit)) %>%
      dplyr::mutate(commodity = (commodity / dplyr::lag(commodity, 1L)) - 1L)
  }
  
  
  factor_names <- stringr::str_split(regressors, " \\+ ") %>% unlist() %>%
    stringr::str_replace_all("`", "")
  
  factors <- dplyr::filter(
    `factor returns`,
    `asset pool` == !! `asset pool`,
    `update frequency` == !! `update frequency`,
    `return frequency` == !! `return frequency`,
    `ranking period` == !! `ranking period`,
    `long threshold` == !! `long threshold`,
    `short threshold` == !! `short threshold`,
    name %in% factor_names,
    leg == !! leg,
    date >= as.Date(start), date <= as.Date(end)
  ) %>%
    dplyr::select(date, name, return) %>%
    tidyr::pivot_wider(names_from = name, values_from = return)
  
  # browser()
  `risk free` <- dplyr::filter(
    rf, frequency == `return frequency`, region == "US"
  )
  `risk free` <- if (`return frequency` == "day"){
    dplyr::select(`risk free`, date = period, rf) %>% 
      dplyr::mutate(date = as.Date(date))
  } else {
    dplyr::select(`risk free`, period, rf) %>%
      dplyr::mutate(
        year = stringr::str_extract(period, "^[0-9]{4}"),
        !! `return frequency` := stringr::str_extract(period, "[0-9]{2}$")
      ) %>% dplyr::select(year, !! `return frequency`, rf)
  }
  
  # browser()
  data <- if (`return frequency` == "day"){
    tryCatch(
      { 
        data <- dplyr::left_join(futures, factors, by = "date")  %>%
          dplyr::left_join(`risk free`, by = "date")  %>%
          dplyr::arrange(date) %>% na.omit() %>% as.data.frame()
        data[, c("commodity", factor_names)] <-
          data[, c("commodity", factor_names)] - data[, "rf"]
        dplyr::select(data, -rf)
      },
      error = function(e) { NA }
    )
  } else {
    tryCatch(
      {
        data <- dplyr::left_join(futures, factors, by = "date") %>%
          dplyr::mutate(
            year = lubridate::year(date) %>% as.character(),
            !! `return frequency` := do.call(
              what = `return frequency`, args = list(date)
            ) %>% as.character() %>% ifelse(nchar(.) == 1L, paste0("0", .), .)
          ) %>%
          dplyr::left_join(`risk free`, by = c("year", `return frequency`)) %>%
          dplyr::select(-c(year, !!`return frequency`)) %>%
          dplyr::arrange(date) %>% na.omit() %>% as.data.frame()
        data[, c("commodity", factor_names)] <-
          data[, c("commodity", factor_names)] - data[, "rf"]
        dplyr::select(data, -rf) 
      },
      error = function(e) { NA }
    )
  }
  
  regimes <- dplyr::filter(`aggregate CHP regimes`, period == !! period) %>%
    dplyr::select(date, regime)
  data <- dplyr::left_join(data, regimes, by = "date") %>%
    dplyr::filter(regime == !! regime) %>% dplyr::select(-regime)
  
  
  formula <- paste("commodity", regressors, sep = " ~ ")
  
  model <- tryCatch(
    { lm(rlang::parse_expr(formula), data = data) },
    error = function(e) { NA }
  )
  
  if (! is.na(model)){
    coefficients <- broom::tidy(model)
    rsquared <- stats::summary.lm(model)$r.squared
  } else {
    coefficients <- NA
    rsquared <- NA
  }
  
  tibble::tibble(
    `asset pool` = !! `asset pool`,
    `update frequency` = !! `update frequency`,
    `return frequency` = !! `return frequency`,
    `ranking period` = !! `ranking period`,
    `long threshold` = !! `long threshold`,
    `short threshold` = !! `short threshold`,
    regressors = regressors,
    leg = !! leg,
    commodity = !! `commodity name`,
    period = !! period,
    regime = !! regime,
    coefficients = list(coefficients),
    rsquared = rsquared
  )
}


`regressions factors` <- dplyr::bind_rows(whole, regimes)


path_regressions_factors_file <- paste0(
  here::here(), "/explore/results/revision-jfm/regressions-factors.rds"
)
saveRDS(`regressions factors`, path_regressions_factors_file)

