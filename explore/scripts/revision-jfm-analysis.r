library(finRes); library(magrittr)

# source functions ####
source(here::here("explore", "scripts", "revision-jfm-functions.r"))

# globals ####
## datasets ####
data("tickers_futures", "tickers_cftc", package = "BBGsymbols")
data("exchanges", package = "fewISOs")

## variables ####
storethat_db_path <- make_path_to_storethat_database()
results_directory_path <- here::here("explore", "results", "revision-jfm")

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

## aggregate CHP ####
### commodity CFTC tickers ####
commodity_CFTC_tickers <- commodity_futures_tickers[
  commodity_futures_tickers %in% tickers_cftc$`active contract ticker`
]

### commodity CFTC data ####
commodity_CFTC_data <- pullit::pull_futures_CFTC(
  source = "storethat", active_contract_tickers = commodity_CFTC_tickers, 
  start = start, end = end, file = storethat_db_path
)

### regimes ####
aggregate_CHP_regimes <- make_aggregate_CHP_regimes_dataframe()

## commodity index returns ####
US_commodity_futures_tickers <- filter_commodity_futures_tickers(
  commodity_futures_tickers, filter_country = "US"
)

first_period_boundaries <- get_period_boundaries(periods, "past")
last_period_boundaries <- get_period_boundaries(periods, "present")

commodity_futures_index_returns <- make_commodity_futures_index_returns_dataframe(
  commodity_futures_individual_data_levels, US_commodity_futures_tickers, 
  first_period_boundaries$start, last_period_boundaries$end
)

## commodity factor returns ####
### futures params ####
TS_positions <- 1L:2L
roll_type <- "A"
roll_days = 0L
roll_months = 0L
roll_adjustment = "N"
data_file = storethat_db_path
### factor params ####
update_frequency = "week"
return_frequency <- "day"
ranking_period <- 26L
long_threshold <- 2/3
short_threshold <- 1/3
weighted = FALSE

commodity_futures_factor_returns <- make_commodity_futures_factor_returns_dataframe(
  US_commodity_futures_tickers, start, end, TS_positions, 
  roll_type, roll_days, roll_months, roll_adjustment, data_file,
  update_frequency, return_frequency, ranking_period, long_threshold, 
  short_threshold, weighted
  )

# analysis ####
library(furrr)
plan(multisession, workers = parallel::detectCores())

## correlations ####
commodity_pool_tickers <- make_commodity_pool_tickers_dataframe(
  commodity_futures_tickers, analysis = "correlations"
  )

### compute raw results ####
correlations_raw <- make_pairwise_correlations_for_ticker_combinations_dataframe(
  commodity_pool_tickers, commodity_futures_data, aggregate_CHP_regimes, period_dates
)

### save raw results ####
path_correlations_raw_file <- paste_forward_slash(results_directory_path, "correlations-raw.rds")
saveRDS(correlations_raw, path_correlations_raw_file)

### summarise results ####
#### load raw dataset (optional) ####
correlations_raw <- readr::read_rds(path_correlations_raw_file)

#### summarise ####
correlations_summary <- 
  add_top_3_and_average_to_pairwise_correlations_for_ticker_combinations_dataframe(correlations_raw)

#### save summary results ####
path_correlations_summary_file <- 
  paste_forward_slash(results_directory_path, "correlations-summary.rds")
saveRDS(correlations_summary, path_correlations_summary_file)

### format results ####
#### load summary dataset (optional) ####
correlations_summary <- readr::read_rds(path_correlations_summary_file)

#### format ####
correlations_formatted <- format_correlation_summary_statistics_into_table(correlations_summary)

#### save formatted results ####
path_correlations_formatted_file <- 
  paste_forward_slash(results_directory_path, "correlations.rds")
saveRDS(correlations_formatted, path_correlations_formatted_file)

## regressions ####
commodity_pool_tickers <- make_commodity_pool_tickers_dataframe(
  commodity_futures_tickers, analysis = "regressions"
)

### index ####
#### raw ####
regressions_index_raw <- make_regressions_index_for_ticker_combinations_dataframe(
  commodity_pool_tickers, commodity_futures_data, commodity_futures_index_returns, 
  aggregate_CHP_regimes, period_dates
)

#### save raw results ####
path_regressions_index_raw_file <- 
  paste_forward_slash(results_directory_path, "regressions-index-raw.rds")
saveRDS(regressions_index_raw, path_regressions_index_raw_file)

#### summarise results ####
##### load raw dataset (optional) ####
# regressions_index_raw <- readr::read_rds(path_regressions_index_raw_file)

##### summarise ####  
regressions_index_summary <- 
  add_top_3_and_average_to_regressions_index_for_ticker_combinations_dataframe(regressions_index_raw)

##### save summary results ####
path_regressions_index_summary_file <- 
  paste_forward_slash(results_directory_path, "regressions-index-summary.rds")
saveRDS(regressions_index_summary, path_regressions_index_summary_file)

#### format ####
##### load summary dataset (optional) ####
# regressions_index_summary <- readr::read_rds(path_regressions_index_summary_file)

##### format ####  
regressions_index_formatted <- 
  format_regression_index_summary_statistics_into_table(regressions_index_summary)

##### save formatted results ####
path_regressions_index_formatted_file <- 
  paste_forward_slash(results_directory_path, "regressions-index.rds")
saveRDS(regressions_index_formatted, path_regressions_index_formatted_file)

### factors ####
#### raw ####
regressions_factors_raw <- make_regressions_factors_for_ticker_combinations_dataframe(
  commodity_pool_tickers, commodity_futures_data, commodity_futures_factor_returns, 
  aggregate_CHP_regimes, period_dates
)

#### save raw results ####
path_regressions_factors_raw_file <- 
  paste_forward_slash(results_directory_path, "regressions-factors-raw.rds")
saveRDS(regressions_factors_raw, path_regressions_factors_raw_file)

#### summarise results ####
##### load raw dataset (optional) ####
# regressions_factors_raw <- readr::read_rds(path_regressions_factors_raw_file)

##### summarise ####  
regressions_factors_summary <- 
  average_Rsquared_by_factor_leg_for_each_type_frequency_field_period_regime_combination(regressions_factors_raw)

##### save summary results ####
path_regressions_factors_summary_file <- 
  paste_forward_slash(results_directory_path, "regressions-factors-summary.rds")
saveRDS(regressions_factors_summary, path_regressions_factors_summary_file)

#### format ####
##### load summary dataset (optional) ####
# regressions_factors_summary <- readr::read_rds(path_regressions_factors_summary_file)

##### format ####  
regressions_factors_formatted <- 
  format_regression_factor_summary_statistics_into_table(regressions_factors_summary)

##### save formatted results ####
path_regressions_factors_formatted_file <- 
  paste_forward_slash(results_directory_path, "regressions-factors.rds")
saveRDS(regressions_factors_formatted, path_regressions_factors_formatted_file)

