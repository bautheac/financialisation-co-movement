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
raw <- pullit::pull_futures_market(
  source = "storethat", type = "term structure", 
  active_contract_tickers = commodity_futures_tickers, start = start, end = end, 
  TS_positions = 1L:2L, roll_type = "A", roll_days = 0L, roll_months = 0L,
  roll_adjustment = "N", file = storethat_db_path
)

commodity_futures_individual_data_levels <- pullit::get_data(raw) %>%
  dplyr::left_join(
    dplyr::select(
      pullit::get_term_structure_tickers(raw), `active contract ticker`, ticker, `TS position`
      ), 
    by = "ticker"
  ) %>% 
  dplyr::select( `active contract ticker`, ticker, `TS position`, field, date, value)
remove(raw)

### aggregate data ####
commodity_futures_aggregate_data_levels <- pullit::pull_futures_market(
  source = "storethat", type = "aggregate", 
  active_contract_tickers = commodity_futures_tickers, 
  start = start, end = end, file = storethat_db_path
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

## aggregate CHP ####
### aggregate CHP data ####
aggregate_CHP <- make_aggregate_CHP()

### aggregate CHP regimes ####
#### aggregate CHP regimes by year ####
regimes_years <- make_aggregate_CHP_regimes_by_year(aggregate_CHP, commodity_futures_data)


#### aggregate CHP regimes by subperiod ####
regimes_periods <- make_aggregate_CHP_regimes_by_period_dataframe(
  aggregate_CHP, commodity_futures_data, periods
)

#### aggregate CHP regimes ####
aggregate_CHP_regimes <- make_aggregate_CHP_regimes_dataframe(regimes_years, regimes_periods)

## construct commodity index returns ####
US_commodity_futures_tickers <- filter_commodity_futures_tickers(
  commodity_futures_tickers, filter_country = "US"
)

first_period_boundaries <- get_period_boundaries(periods, "past")
last_period_boundaries <- get_period_boundaries(periods, "present")
period_start_date <- first_period_boundaries$start
period_end_date <- last_period_boundaries$end

commodity_futures_index_returns <- make_commodity_futures_index_returns_dataframe(
  commodity_futures_individual_data_levels, US_commodity_futures_tickers, period_start_date, period_end_date
)

# analysis ####
library(furrr)
plan(multisession, workers = parallel::detectCores())

## correlations ####
commodity_pool_tickers <- make_commodity_pool_tickers_dataframe(commodity_futures_tickers)

progressr::with_progress({
  correlations_raw <- make_pairwise_correlations_for_ticker_combinations_dataframe(
    commodity_pool_tickers, commodity_futures_data, aggregate_CHP_regimes, period_dates
  )
  correlations_top_3_averages <- 
    add_top_3_and_average_to_pairwise_correlations_for_ticker_combinations_dataframe(correlations_raw)
  
})

















