library(finRes); library(magrittr)

# source functions ####
source(here::here("explore", "scripts", "revision-jfm", "functions.r"))

# load global variables ########################################################
load_global_variables()

# analysis #####################################################################

library(furrr)
plan(multisession, workers = parallel::detectCores())

## regime difference tests #####################################################
### raw ####
regime_difference_tests_raw <- make_regime_difference_tests()


## correlations ################################################################
commodity_pool_tickers <- make_commodity_pool_tickers_dataframe(
  commodity_futures_tickers, analysis = "correlations"
  )

### raw ####
correlations_raw <- make_pairwise_correlations_for_ticker_combinations_dataframe(
  commodity_pool_tickers, commodity_futures_data, aggregate_CHP_regimes, period_dates
)

### summary ####
#### load raw dataset (optional) ####
# path_correlations_raw_file <- paste_forward_slash(results_directory_path, "correlations-raw.rds")
# correlations_raw <- readr::read_rds(path_correlations_raw_file)

#### summarise ####
correlations_summary <- 
  add_top_3_and_average_to_pairwise_correlations_for_ticker_combinations_dataframe(correlations_raw)

### formatted ####
#### load summary dataset (optional) ####
# path_correlations_summary_file <- 
#   paste_forward_slash(results_directory_path, "correlations-summary.rds")
# correlations_summary <- readr::read_rds(path_correlations_summary_file)

#### format ####
correlations_formatted <- 
  format_correlation_summary_statistics_into_table(correlations_summary)

## regressions #################################################################
commodity_pool_tickers <- make_commodity_pool_tickers_dataframe(
  commodity_futures_tickers, analysis = "regressions"
)

### index ######################################################################
#### raw ####
regressions_index_raw <- make_regressions_index_for_ticker_combinations_dataframe(
  commodity_pool_tickers, commodity_futures_data, commodity_futures_index_returns, 
  aggregate_CHP_regimes, period_dates
)

#### summary ####
##### load raw dataset (optional) ####
# path_regressions_index_raw_file <- 
#   paste_forward_slash(results_directory_path, "regressions-index-raw.rds")
# regressions_index_raw <- readr::read_rds(path_regressions_index_raw_file)

##### summarise ####  
regressions_index_summary <- 
  add_top_3_and_average_to_regressions_index_for_ticker_combinations_dataframe(regressions_index_raw)

#### formatted ####
##### load summary dataset (optional) ####
# path_regressions_index_summary_file <- 
#   paste_forward_slash(results_directory_path, "regressions-index-summary.rds")
# regressions_index_summary <- readr::read_rds(path_regressions_index_summary_file)

##### format ####  
regressions_index_formatted <- 
  format_regression_index_summary_statistics_into_table(regressions_index_summary)

### factors ####################################################################
#### raw ####
regressions_factors_raw <- make_regressions_factors_for_ticker_combinations_dataframe(
  commodity_pool_tickers, commodity_futures_data, commodity_futures_factor_returns, 
  aggregate_CHP_regimes, period_dates
)

#### summary ####
##### load raw dataset (optional) ####
# path_regressions_factors_raw_file <- 
#   paste_forward_slash(results_directory_path, "regressions-factors-raw.rds")
# regressions_factors_raw <- readr::read_rds(path_regressions_factors_raw_file)

##### summarise ####  
regressions_factors_summary <- 
  average_Rsquared_by_factor_leg_for_each_type_frequency_field_period_regime_combination(regressions_factors_raw)

#### formatted ####
##### load summary dataset (optional) ####
# path_regressions_factors_summary_file <- 
#   paste_forward_slash(results_directory_path, "regressions-factors-summary.rds")
# regressions_factors_summary <- readr::read_rds(path_regressions_factors_summary_file)

##### format ####  
regressions_factors_formatted <- 
  format_regression_factor_summary_statistics_into_table(regressions_factors_summary)


# export #######################################################################
## regime difference tests #####################################################
### raw ####
path_regime_difference_tests_raw_file <- 
  paste_forward_slash(results_directory_path, "regime-difference-tests-raw.rds")
saveRDS(regime_difference_tests_raw, path_correlations_raw_file)

## correlations ################################################################
### raw ####
path_correlations_raw_file <- paste_forward_slash(results_directory_path, "correlations-raw.rds")
saveRDS(correlations_raw, path_correlations_raw_file)

#### summary ####
path_correlations_summary_file <- 
  paste_forward_slash(results_directory_path, "correlations-summary.rds")
saveRDS(correlations_summary, path_correlations_summary_file)

### formatted ####
path_correlations_formatted_file <- 
  paste_forward_slash(results_directory_path, "correlations.rds")
saveRDS(correlations_formatted, path_correlations_formatted_file)

## regressions #################################################################
commodity_pool_tickers <- make_commodity_pool_tickers_dataframe(
  commodity_futures_tickers, analysis = "regressions"
)

### index ######################################################################
#### raw ####
path_regressions_index_raw_file <- 
  paste_forward_slash(results_directory_path, "regressions-index-raw.rds")
saveRDS(regressions_index_raw, path_regressions_index_raw_file)

#### summary ####
path_regressions_index_summary_file <- 
  paste_forward_slash(results_directory_path, "regressions-index-summary.rds")
saveRDS(regressions_index_summary, path_regressions_index_summary_file)

#### formatted ####
path_regressions_index_formatted_file <- 
  paste_forward_slash(results_directory_path, "regressions-index.rds")
saveRDS(regressions_index_formatted, path_regressions_index_formatted_file)

### factors ####################################################################
#### raw ####
path_regressions_factors_raw_file <- 
  paste_forward_slash(results_directory_path, "regressions-factors-raw.rds")
saveRDS(regressions_factors_raw, path_regressions_factors_raw_file)

#### summary ####
path_regressions_factors_summary_file <- 
  paste_forward_slash(results_directory_path, "regressions-factors-summary.rds")
saveRDS(regressions_factors_summary, path_regressions_factors_summary_file)

#### formatted ####
path_regressions_factors_formatted_file <- 
  paste_forward_slash(results_directory_path, "regressions-factors.rds")
saveRDS(regressions_factors_formatted, path_regressions_factors_formatted_file)



