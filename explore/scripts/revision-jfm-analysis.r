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
correlations_raw <- make_pairwise_correlations_for_ticker_combinations_dataframe(
  commodity_pool_tickers, commodity_futures_data, aggregate_CHP_regimes, period_dates
)

## regressions ####
regressions_raw <- make_regressions_for_ticker_combinations_dataframe(
  commodity_pool_tickers, commodity_futures_data, commodity_futures_index_returns, 
  aggregate_CHP_regimes, period_dates
)

## save raw results ####
path_correlations_raw_file <- paste0(
  here::here(), "/explore/results/revision-jfm/correlations-raw.rds"
  )
path_regressions_raw_file <- paste0(
  here::here(), "/explore/results/revision-jfm/regressions-raw.rds"
  )

saveRDS(correlations_raw, path_correlations_raw_file)
saveRDS(regressions_raw, path_regressions_raw_file)

# summary ####
## load raw dataset (optional) ####
correlations_raw <- readr::read_rds(path_correlations_raw_file)
regressions_raw <- readr::read_rds(path_regressions_raw_file)
  
## correlations ####  
correlations_summary <- 
  add_top_3_and_average_to_pairwise_correlations_for_ticker_combinations_dataframe(correlations_raw)

## regressions ####  
regressions_summary <- 
  add_top_3_and_average_to_regressions_for_ticker_combinations_dataframe(regressions_raw)

## save summary results ####
path_correlations_summary_file <- paste0(
  here::here(), "/explore/results/revision-jfm/correlations-summary.rds"
)
path_regressions_summary_file <- paste0(
  here::here(), "/explore/results/revision-jfm/regressions-summary.rds"
)

saveRDS(correlations_summary, path_correlations_summary_file)
saveRDS(regressions_summary, path_regressions_summary_file)

# format ####
## load summary dataset (optional) ####
correlations_summary <- readr::read_rds(path_correlations_summary_file)
regressions_summary <- readr::read_rds(path_regressions_summary_file)

## correlations ####  
correlations_formatted <- format_correlation_summary_statistics_into_table(correlations_summary)

## regressions ####  
regressions_formatted <- format_regression_summary_statistics_into_table(regressions_summary)

## save formatted results ####
path_correlations_formatted_file <- paste0(
  here::here(), "/explore/tables/revision-jfm/correlations.rds"
)
path_regressions_formatted_file <- paste0(
  here::here(), "/explore/tables/revision-jfm/regressions.rds"
)

saveRDS(correlations_formatted, path_correlations_formatted_file)
saveRDS(regressions_formatted, path_regressions_formatted_file)

