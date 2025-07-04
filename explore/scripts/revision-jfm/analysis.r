pacman::p_load(finRes)

# load global variables ########################################################
# load_global_variables()
load(here::here("explore", "scripts", "revision-jfm", "globals.RData"))
source(here::here("explore", "scripts", "revision-jfm", "functions.r"))

# analysis #####################################################################
pacman::p_load(furrr)
plan(multisession, workers = parallel::detectCores())


## descriptive stats ###########################################################
### individual assets ##########################################################
#### formatted ####
##### load summary dataset (optional) ####
path_descriptive_stats_assets_summary_file <-
  paste_forward_slash(results_directory_path, "descriptive-statistics-clean.rds")
descriptive_stats_assets_summary <- extract_descriptive_stats_assets_summary()

##### format ####
descriptive_stats_assets_formatted <- format_asset_stats(descriptive_stats_assets_summary)



### equally weighted portfolios ################################################
#### raw ####
descriptive_stats_ew_portfolios_raw <- compute_ew_portfolios_stats_raw()

#### summary ####
##### load raw dataset (optional) ####
# path_descriptive_stats_ew_portfolios_raw_file <- 
#   paste_forward_slash(results_directory_path, "descriptive-stats-ew-portfolios-raw.rds")
# descriptive_stats_ew_portfolios_raw <- readr::read_rds(path_descriptive_stats_ew_portfolios_raw_file)

##### summarise ####
descriptive_stats_ew_portfolios_summary <- 
  summarise_ew_portfolios_stats(descriptive_stats_ew_portfolios_raw)

#### formatted ####
##### load summary dataset (optional) ####
# path_descriptive_stats_ew_portfolios_summary_file <- 
#   paste_forward_slash(results_directory_path, "descriptive-stats-ew-portfolios-summary.rds")
# descriptive_stats_ew_portfolios_summary <- 
#   readr::read_rds(path_descriptive_stats_ew_portfolios_summary_file)

##### format ####
descriptive_stats_ew_portfolios_formatted <- 
  format_ew_portfolios_stats(descriptive_stats_ew_portfolios_summary)



## regime difference tests #####################################################
### raw ####
regime_difference_tests_raw <- make_regime_difference_tests()

### summary ####
#### load raw dataset (optional) ####
# path_regime_difference_tests_raw_file <- 
#   paste_forward_slash(results_directory_path, "regime-difference-tests-raw.rds")
# regime_difference_tests_raw <- readr::read_rds(path_regime_difference_tests_raw_file)

#### summarise ####
regime_difference_tests_summary <- extract_pvalues_from_test_objects(regime_difference_tests_raw)

### formatted ####
#### load summary dataset (optional) ####
path_regime_difference_tests_summary_file <-
  paste_forward_slash(results_directory_path, "regime-difference-tests-summary.rds")
regime_difference_tests_summary <- readr::read_rds(path_regime_difference_tests_summary_file)

#### format ####
regime_difference_tests_formatted <- 
  format_regime_difference_tests_summary_into_table(regime_difference_tests_summary)


## correlations ################################################################
### inner ####
commodity_pool_tickers <- make_commodity_pool_tickers_dataframe(
  commodity_futures_tickers, analysis = "correlations - inner"
)
#### raw ####
correlations_inner_raw <- make_inner_correlations_for_ticker_combinations_dataframe(
  commodity_pool_tickers, commodity_futures_data, aggregate_CHP_regimes, period_dates
)

#### summary ####
##### load raw dataset (optional) ####
# path_correlations_inner_raw_file <- paste_forward_slash(results_directory_path, "correlations-inner-raw.rds")
# correlations_inner_raw <- readr::read_rds(path_correlations_inner_raw_file)

##### summarise ####
correlations_inner_summary <- 
  add_top_3_and_average_to_inner_correlations_for_ticker_combinations_dataframe(correlations_inner_raw)

#### formatted ####
##### load summary dataset (optional) ####
# path_correlations_inner_summary_file <-
#   paste_forward_slash(results_directory_path, "correlations-inner-summary.rds")
# correlations_inner_summary <- readr::read_rds(path_correlations_inner_summary_file)

##### format ####
correlations_inner_formatted <- 
  format_inner_correlations_summary_statistics_into_table(correlations_inner_summary)

### cross ####
#### US ####
commodity_pool_tickers <- make_commodity_pool_tickers_dataframe(
  commodity_futures_tickers, analysis = "correlations - cross - US"
)

##### raw ####
correlations_cross_US_raw <- make_global_correlations_for_ticker_combinations_dataframe(
  commodity_pool_tickers, commodity_futures_data, aggregate_CHP_regimes, period_dates, "US"
)

##### summary ####
###### load raw dataset (optional) ####
# path_correlations_cross_US_raw_file <- paste_forward_slash(results_directory_path, "correlations-cross-US-raw.rds")
# correlations_cross_US_raw <- readr::read_rds(path_correlations_cross_US_raw_file)

###### summarise ####
correlations_cross_US_summary <- summarise_cross_correlations(correlations_cross_US_raw)

##### formatted ####
###### load summary dataset (optional) ####
# path_correlations_cross_US_summary_file <-
#   paste_forward_slash(results_directory_path, "correlations-cross-US-summary.rds")
# correlations_cross_US_summary <- readr::read_rds(path_correlations_cross_US_summary_file)

###### format ####
correlations_cross_US_formatted <- format_cross_correlation_averages(correlations_cross_US_summary)

#### global ####
commodity_pool_tickers <- make_commodity_pool_tickers_dataframe(
  commodity_futures_tickers, analysis = "correlations - cross - global"
)

##### raw ####
correlations_cross_global_raw <- make_global_correlations_for_ticker_combinations_dataframe(
  commodity_pool_tickers, commodity_futures_data, aggregate_CHP_regimes, period_dates, "global"
)

##### summary ####
###### load raw dataset (optional) ####
# path_correlations_cross_global_raw_file <- paste_forward_slash(results_directory_path, "correlations-cross-global-raw.rds")
# correlations_cross_global_raw <- readr::read_rds(path_correlations_cross_global_raw_file)

###### summarise ####
correlations_cross_global_summary <- summarise_cross_correlations(correlations_cross_global_raw)

##### formatted ####
###### load summary dataset (optional) ####
# path_correlations_cross_global_summary_file <-
#   paste_forward_slash(results_directory_path, "correlations-cross-global-summary.rds")
# correlations_cross_global_summary <- readr::read_rds(path_correlations_cross_global_summary_file)

###### format ####
correlations_cross_global_formatted <- format_cross_correlation_averages(correlations_cross_global_summary)


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
## descriptive stats ###########################################################
### individual assets ##########################################################
### formatted ####
path_descriptive_stats_assets_formatted_file <-
  paste_forward_slash(results_directory_path, "descriptive-statistics.rds")
saveRDS(descriptive_stats_assets_formatted, path_descriptive_stats_assets_formatted_file)

### equally weighted portfolios ################################################
#### raw ####
path_descriptive_stats_ew_portfolios_raw_file <- 
  paste_forward_slash(results_directory_path, "descriptive-stats-ew-portfolios-raw.rds")
saveRDS(descriptive_stats_ew_portfolios_raw, path_descriptive_stats_ew_portfolios_raw_file)

#### summary ####
path_descriptive_stats_ew_portfolios_summary_file <- 
  paste_forward_slash(results_directory_path, "descriptive-stats-ew-portfolios-summary.rds")
saveRDS(descriptive_stats_ew_portfolios_summary, path_descriptive_stats_ew_portfolios_summary_file)

### formatted ####
path_descriptive_stats_ew_portfolios_formatted_file <- 
  paste_forward_slash(results_directory_path, "descriptive-stats-ew-portfolios.rds")
saveRDS(descriptive_stats_ew_portfolios_formatted, path_descriptive_stats_ew_portfolios_formatted_file)


## regime difference tests #####################################################
### raw ####
path_regime_difference_tests_raw_file <- 
  paste_forward_slash(results_directory_path, "regime-difference-tests-raw.rds")
saveRDS(regime_difference_tests_raw, path_regime_difference_tests_raw_file)

#### summary ####
path_regime_difference_tests_summary_file <- 
  paste_forward_slash(results_directory_path, "regime-difference-tests-summary.rds")
saveRDS(regime_difference_tests_summary, path_regime_difference_tests_summary_file)

### formatted ####
path_regime_difference_tests_formatted_file <- 
  paste_forward_slash(results_directory_path, "regime-difference-tests.rds")
saveRDS(regime_difference_tests_formatted, path_regime_difference_tests_formatted_file)

## correlations ################################################################
### inner ####
#### raw ####
path_correlations_inner_raw_file <- paste_forward_slash(results_directory_path, "correlations-inner-raw.rds")
saveRDS(correlations_inner_raw, path_correlations_inner_raw_file)

##### summary ####
path_correlations_inner_summary_file <- 
  paste_forward_slash(results_directory_path, "correlations-inner-summary.rds")
saveRDS(correlations_inner_summary, path_correlations_inner_summary_file)

#### formatted ####
path_correlations_inner_formatted_file <- 
  paste_forward_slash(results_directory_path, "correlations-inner.rds")
saveRDS(correlations_inner_formatted, path_correlations_inner_formatted_file)

### cross ####
#### US ####
##### raw ####
path_correlations_cross_US_raw_file <- paste_forward_slash(results_directory_path, "correlations-cross-US-raw.rds")
saveRDS(correlations_cross_US_raw, path_correlations_cross_US_raw_file)

###### summary ####
path_correlations_cross_US_summary_file <- 
  paste_forward_slash(results_directory_path, "correlations-cross-US-summary.rds")
saveRDS(correlations_cross_US_summary, path_correlations_cross_US_summary_file)

##### formatted ####
path_correlations_cross_US_formatted_file <- 
  paste_forward_slash(results_directory_path, "correlations-cross-US.rds")
saveRDS(correlations_cross_US_formatted, path_correlations_cross_US_formatted_file)

#### global ####
##### raw ####
path_correlations_cross_global_raw_file <- paste_forward_slash(results_directory_path, "correlations-cross-global-raw.rds")
saveRDS(correlations_cross_global_raw, path_correlations_cross_global_raw_file)

###### summary ####
path_correlations_cross_global_summary_file <- 
  paste_forward_slash(results_directory_path, "correlations-cross-global-summary.rds")
saveRDS(correlations_cross_global_summary, path_correlations_cross_global_summary_file)

##### formatted ####
path_correlations_cross_global_formatted_file <- 
  paste_forward_slash(results_directory_path, "correlations-cross-global.rds")
saveRDS(correlations_cross_global_formatted, path_correlations_cross_global_formatted_file)

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



