# utils ########################################################################
paste_forward_slash <- function(...) paste(..., sep = "/")

# load environment variables ###################################################
## variables ###################################################################
### local function #############################################################
make_path_to_storethat_database <- function(){
  paste0(here::here(), "/data/storethat.sqlite")
}

make_periods_data_frame <- function(period_bounds){
  purrr::imap_dfr(period_bounds, function(bounds, name){
    purrr::imap_dfr(bounds, function(date, bound){
      tibble::tibble(bound = bound, date = date)
    }) %>% dplyr::mutate(period = name) %>% dplyr::select(period, dplyr::everything())
  })
}

load_variables <- function(){
  ## variables ####
  storethat_db_path <<- make_path_to_storethat_database() # assign to parent environment.
  results_directory_path <<- here::here("explore", "results", "revision-jfm")

  period_bounds <- list(
    past = list(start = "1997-07-01", end = "2003-12-31"),
    financialization = list(start = "2004-01-01", end = "2008-09-14"),
    crisis = list(start = "2008-09-15", end = "2013-06-19"),
    present = list(start = "2013-06-20", end = "2018-12-31")
  )

  periods <<- make_periods_data_frame(period_bounds)
  assign(
    "rf", readr::read_rds(here::here("data", "risk-free-rate.rds")), envir = parent.frame()
  ) # conflict with stat::rf if parent environment; can't use <<- here.
}

## load data ###################################################################
### functions ##################################################################
get_commodity_futures_individual_data_levels <- function(
    commodity_futures_tickers, start, end, storethat_db_path
    ){
  
  raw_data_object <- pullit::pull_futures_market(
    source = "storethat", type = "term structure", 
    active_contract_tickers = commodity_futures_tickers, start = start, end = end, 
    TS_positions = 1L:2L, roll_type = "A", roll_days = 0L, roll_months = 0L,
    roll_adjustment = "N", file = storethat_db_path
  )
  
  raw_data <- pullit::get_data(raw_data_object)
  term_structure_tickers <- dplyr::select(
    pullit::get_term_structure_tickers(raw_data_object), `active contract ticker`, ticker, `TS position`
  )
  
    dplyr::left_join(raw_data, term_structure_tickers, by = "ticker") %>% 
    dplyr::select( `active contract ticker`, ticker, `TS position`, field, date, value)
}

get_commodity_futures_aggregate_data_levels <- function(
    commodity_futures_tickers, start, end, storethat_db_path
    ){
  
  pullit::pull_futures_market(
    source = "storethat", type = "aggregate", 
    active_contract_tickers = commodity_futures_tickers, 
    start = start, end = end, file = storethat_db_path
  )
}

get_period_boundaries <- function(periods, period_id){
  start <- dplyr::filter(periods, period == period_id, bound == "start") %>% 
    dplyr::select(date) %>% purrr::flatten_chr()
  end <- dplyr::filter(periods, period == period_id, bound == "end") %>% 
    dplyr::select(date) %>% purrr::flatten_chr()
  
  list(start = start, end = end)
}

get_period_ids <- function(periods_dataset){unique(periods_dataset$period)}

make_time_series <- function(data_levels, frequency){
  
  frequency <- ifelse(frequency == "day", "yday", frequency)
  year_unit_function <- get(frequency, envir = asNamespace('lubridate'))
  
  dplyr::mutate(
    data_levels, year = lubridate::year(date),
    unit = do.call(what = year_unit_function, args = list(date))
  ) %>%
    dplyr::group_by(dplyr::across(-dplyr::all_of(c("date", "value")))) %>%
    dplyr::filter(dplyr::row_number() == dplyr::n()) %>% dplyr::ungroup() %>%
    dplyr::select(-c("year", "unit"))
}

make_ticker_country_sector_subsector_dataframe <- function(tickers){
  data("tickers_futures", package = "BBGsymbols")
  data("exchanges", package = "fewISOs")
  
  df <- dplyr::left_join(
    dplyr::filter(tickers_futures, ticker %in% tickers) %>% 
      dplyr::select(ticker, sector, subsector, MIC), 
    dplyr::select(exchanges, MIC, country), 
    by = "MIC"
  ) %>% dplyr::select(-MIC)
  
  rm(tickers_futures, exchanges, envir = .GlobalEnv); return(df)
}

make_ticker_country_dataframe <- function(tickers){
  data("tickers_futures", package = "BBGsymbols")
  data("exchanges", package = "fewISOs")
  
  df <- dplyr::left_join(
    dplyr::filter(tickers_futures, ticker %in% tickers) %>% dplyr::select(ticker, MIC), 
    dplyr::select(exchanges, MIC, country), 
    by = "MIC"
  ) %>% dplyr::select(-MIC)
  
  rm(tickers_futures, exchanges, envir = .GlobalEnv); return(df)
}

filter_commodity_futures_tickers <- function(
    all_tickers, filter_country = "all", filter_sector = "all", filter_subsector = "all"
){
  
  tickers <- make_ticker_country_sector_subsector_dataframe(all_tickers)
  
  if (filter_country != "all") tickers <- dplyr::filter(tickers, country == filter_country)
  if (filter_sector != "all") tickers <- dplyr::filter(tickers, sector == filter_sector)
  if (filter_subsector != "all") tickers <- dplyr::filter(tickers, subsector == filter_subsector)
  
  tickers$ticker
}

make_US_commodity_pool_combinations_for_correlations_analysis <- function(){
  
  tibble::tibble(
    country = rep("US", 8L),
    sector = c(rep("agriculturals", 4L), rep("energy", 2L), rep("metals", 2L)),
    subsector = c(
      "all", "grains", "livestock", "softs", "all", "petroleum", "all", "precious"
      )
  )
}

make_US_commodity_pool_combinations_for_regressions_analysis <- function(){
  
  tibble::tibble(
    country = rep("US", 10L),
    sector = c(rep("agriculturals", 4L), rep("energy", 3L), rep("metals", 3L)),
    subsector = c(
      "all", "grains", "livestock", "softs", "all", "gas", "petroleum", "all", "base", "precious"
      )
  )
}

make_commodity_pool_tickers_dataframe_for_country_sectors_subsectors_combinations <- function(combinations, all_tickers){
  
  dplyr::rowwise(combinations) %>% 
    dplyr::mutate(
      tickers = list(filter_commodity_futures_tickers(all_tickers, country, sector, subsector))
    )
}

make_commodity_pool_tickers_dataframe <- function(all_tickers, analysis = c("correlations", "regressions")){
  
  all_all_all <- tibble::tibble(
    country = "all", sector = "all", subsector = "all", 
    tickers = list(filter_commodity_futures_tickers(all_tickers))
    )
  
  US_all_all <- tibble::tibble(
    country = "US", sector = "all", subsector = "all", 
    tickers = list(filter_commodity_futures_tickers(all_tickers, "US"))
  )

  US_combinations <- if(analysis == "correlations"){
    make_US_commodity_pool_combinations_for_correlations_analysis()
  } else {
    make_US_commodity_pool_combinations_for_regressions_analysis()
  }
  US_combinations <-
    make_commodity_pool_tickers_dataframe_for_country_sectors_subsectors_combinations(
      US_combinations, all_tickers
    )
  
  GB_all_all <- tibble::tibble(
    country = "GB", sector = "all", subsector = "all", 
    tickers = list(filter_commodity_futures_tickers(all_tickers, "GB"))
  )

  dplyr::bind_rows(all_all_all, US_all_all, US_combinations, GB_all_all)
}

### commodity futures levels data ##############################################
#### individual data ####
#### aggregate data ####
#### both ####
##### local functions ####
bind_individual_and_aggregate_commodity_futures_dataframes <- function(individual, aggregate){
  dplyr::filter(individual, `TS position` == 1L) %>%
    dplyr::select(`active contract ticker`, field, date, value) %>%
    dplyr::bind_rows(dplyr::rename(
      pullit::get_data(aggregate), `active contract ticker` = ticker)
    )
}

### commodity futures relative changes data ####################################
#### local functions ####
compute_commodity_futures_relative_changes <- function(levels){

    dplyr::group_by(levels, dplyr::across(-dplyr::all_of(c("date", "value")))) %>%
      dplyr::mutate(value = (value / dplyr::lag(value)) - 1L) %>% dplyr::ungroup()
}

make_commodity_futures_relative_changes_dataframe <- function(commodity_futures_data_levels){
  
  lapply(c("day", "week", "month"), function(frequency){
    make_time_series(commodity_futures_data_levels, frequency) %>%
      compute_commodity_futures_relative_changes() %>%
      dplyr::mutate(frequency = as.factor(frequency))
  }) %>% dplyr::bind_rows() %>% dplyr::select(frequency, dplyr::everything())
}

### bind levels and relative changes ###########################################
#### local functions ####
make_commodity_futures_dataframe <- function(levels, relative_changes){
  dplyr::mutate(levels, type = "level", frequency = NA) %>%
    dplyr::bind_rows(
      dplyr::mutate(relative_changes, type = "return")
    ) %>% dplyr::select(type, frequency, dplyr::everything()) %>%
    dplyr::arrange(type, frequency, `active contract ticker`, field, date)
}

### period dates ###############################################################
make_period_dates_timeseries <- function(commodity_futures_data, period_boundaries){
  
  period_ids <- unique(period_boundaries$period)
  lapply(period_ids, function(id){
    period_start_date <- dplyr::filter(period_boundaries, period == id, bound == "start") %>% 
      dplyr::select(date) %>% purrr::flatten_chr()
    period_end_date <- dplyr::filter(period_boundaries, period == id, bound == "end") %>% 
      dplyr::select(date) %>% purrr::flatten_chr()
    dplyr::distinct(commodity_futures_data, date) %>% 
      dplyr::filter(date >= period_start_date, date < period_end_date) %>%
      dplyr::mutate(period = id) %>% dplyr::arrange(date)
  }) %>% dplyr::bind_rows()
}

### aggregate CHP ##############################################################
#### aggregate CHP data ####
##### local functions ####
load_CFTC_data <- function(){
  data("tickers_cftc", package = "BBGsymbols")
  
  df <- dplyr::left_join(
    commodity_CFTC_data@data, 
    dplyr::select(tickers_cftc, MIC, format, underlying, unit, participant, position, ticker), 
    by = "ticker"
  )
  
  rm(tickers_cftc, envir = .GlobalEnv); return(df)
}

filter_CFTC_data <- function(raw_CFTC_data){
  dplyr::filter(
    raw_CFTC_data,
    format == "legacy", participant == "commercial", underlying == "futures only", 
    unit == "contracts", position %in% c("long", "short")
  )
}

compute_individual_CHP <- function(filtered_CFTC_data){
  dplyr::select(filtered_CFTC_data, `active contract ticker`, position, date, value) %>% 
    dplyr::group_by(`active contract ticker`) %>%
    tidyr::spread(position, value) %>% 
    dplyr::mutate(pressure = long / (long + short)) %>% 
    dplyr::select(`active contract ticker`, date, pressure) %>% dplyr::ungroup()
}

compute_aggregate_CHP <- function(individual_CHP){
  dplyr::group_by(individual_CHP, date) %>% 
    dplyr::summarise(aggregate_CHP = mean(pressure, na.rm = T), .groups = "drop") 
}

make_aggregate_CHP <- function(){
  load_CFTC_data() %>% filter_CFTC_data() %>% compute_individual_CHP() %>%
    compute_aggregate_CHP()
}

#### aggregate CHP regimes ####
##### local functions ####
match_regimes_to_futures_data_dates <- function(regimes, commodity_futures_data_dates){
  
  dplyr::mutate(
    commodity_futures_data_dates, year = lubridate::year(date), week = lubridate::week(date)
  ) %>%
    dplyr::left_join(regimes, by = c("year", "week")) %>% 
    dplyr::select(-week) %>% dplyr::filter(! is.na(regime))
}

##### aggregate CHP regimes by year ####
###### local functions ####
compute_regimes_by_year <- function(aggregate_CHP){
  
  dplyr::mutate(aggregate_CHP, year = lubridate::year(date), week = lubridate::week(date)) %>% 
    dplyr::group_by(year) %>% 
    dplyr::mutate(regime = ifelse(aggregate_CHP < median(aggregate_CHP), "backwardation", "contango")) %>% 
    dplyr::ungroup() %>% dplyr::select(year, week, regime)
}

format_dataframe_aggregate_CHP_regimes_by_year <- function(aggregate_CHP_regimes){
  
  dplyr::mutate(aggregate_CHP_regimes) %>% dplyr::select(date, year, regime )%>% 
    dplyr::arrange(date)
}

make_aggregate_CHP_regimes_by_year <- function(aggregate_CHP_data, commodity_futures_data){
  
  commodity_futures_data_dates <- dplyr::distinct(commodity_futures_data, date) %>% 
    dplyr::arrange(date)
  
  compute_regimes_by_year(aggregate_CHP_data) %>% 
    match_regimes_to_futures_data_dates(commodity_futures_data_dates) %>%
    format_dataframe_aggregate_CHP_regimes_by_year()
}

##### aggregate CHP regimes by subperiod ####
###### local functions ####
compute_regimes_for_period <- function(aggregate_CHP_data, period_start_date, period_end_date){
  
  dplyr::filter(aggregate_CHP_data, date >= as.Date(period_start_date), date <= as.Date(period_end_date)) %>% 
    dplyr::mutate(
      regime = ifelse(aggregate_CHP < median(aggregate_CHP), "backwardation", "contango"),
      year = lubridate::year(date), week = lubridate::week(date)
    ) %>% 
    dplyr::select(year, week, regime)
}

get_commodity_futures_data_dates_for_period <- function(
    commodity_futures_data, period_start_date, period_end_date
){
  
  dplyr::distinct(commodity_futures_data, date) %>% 
    dplyr::filter(date >= as.Date(period_start_date), date <= as.Date(period_end_date))
}

format_dataframe_aggregate_CHP_regimes_for_period <- function(aggregate_CHP_regimes, period_id){
  
  dplyr::mutate(aggregate_CHP_regimes, period = period_id) %>% 
    dplyr::select(date, period, regime) %>% dplyr::arrange(date)
}

make_aggregate_CHP_regimes_for_period <- function(
    aggregate_CHP_data, commodity_futures_data, periods, period_id
){
  
  period_boundaries <- get_period_boundaries(periods, period_id)
  period_start_date <- period_boundaries$start; period_end_date <- period_boundaries$end
  
  commodity_fututures_data_dates_for_period <- get_commodity_futures_data_dates_for_period(
    commodity_futures_individual_data_levels, period_start_date, period_end_date
  )
  
  compute_regimes_for_period(aggregate_CHP_data, period_start_date, period_end_date) %>%
    match_regimes_to_futures_data_dates(commodity_fututures_data_dates_for_period) %>%
    format_dataframe_aggregate_CHP_regimes_for_period(period_id)
}

make_aggregate_CHP_regimes_by_period_dataframe <- function(
    aggregate_CHP, commodity_futures_individual_data_levels, periods
){
  
  period_ids <- get_period_ids(periods)
  lapply(period_ids, function(period_id){
    make_aggregate_CHP_regimes_for_period(
      aggregate_CHP, commodity_futures_individual_data_levels, periods, period_id
    )
  }) %>% dplyr::bind_rows()
}

##### aggregate CHP regimes ####
###### local functions ####
make_aggregate_CHP_regimes_dataframe <- function(){
  
  aggregate_CHP <- make_aggregate_CHP()
  
  regimes_years <- make_aggregate_CHP_regimes_by_year(aggregate_CHP, commodity_futures_data)
  
  regimes_periods <- make_aggregate_CHP_regimes_by_period_dataframe(
    aggregate_CHP, commodity_futures_data, periods
  )
  
  tibble::tibble(timespan = c("year", "period"), regimes = list(regimes_years, regimes_periods))
}

### construct commodity index returns ##########################################
#### local functions ####
compute_commodity_futures_index_returns <- function(price_levels){
  
  dplyr::group_by(price_levels, `active contract ticker`) %>%
    dplyr::mutate(return = value/dplyr::lag(value) - 1L) %>%
    dplyr::select(-value) %>% dplyr::group_by(date) %>%
    dplyr::summarise(return = mean(return, na.rm = T), .groups = "drop")
}

make_commodity_futures_index_returns_dataframe <- function(
    dataset, tickers, date_start, date_end
){

  price_levels_daily <- dplyr::filter(
    dataset, `active contract ticker` %in% tickers, `TS position` == 1L, 
    field == "PX_LAST", date >= date_start, date <= date_end
  ) %>% dplyr::select(`active contract ticker`, date, value)
  
  returns_daily <- compute_commodity_futures_index_returns(price_levels_daily)
  
  price_levels_weekly <- make_time_series(price_levels_daily, "week")
  returns_weekly <- compute_commodity_futures_index_returns(price_levels_weekly)
  
  price_levels_monthly <- make_time_series(price_levels_daily, "month")
  returns_monthly <- compute_commodity_futures_index_returns(price_levels_monthly)
  
  tibble::tibble(
    frequency = c("day", "week", "month"),
    returns = list(returns_daily, returns_weekly, returns_monthly)
  )
}

### construct commodity factor returns #########################################
#### local functions ####
get_data_for_factor_construction <- function(
    tickers, start_date, end_date, 
    TS_positions, roll_type, roll_days, roll_months, roll_adjustment, data_file
){
  data("tickers_cftc", package = "BBGsymbols")
  
  message("\n pull commodity futures levels individual data")
  commodity_futures_data <- pullit::pull_futures_market(
    source = "storethat", type = "term structure", active_contract_tickers = tickers,
    start = start_date, end = end_date, TS_positions = TS_positions, 
    roll_type = roll_type, roll_days = roll_days, roll_months = roll_months, 
    roll_adjustment = roll_adjustment, file = data_file
  )
  
  message("\n pull commodity futures levels aggegate data")
  commodity_aggregate_data <- pullit::pull_futures_market(
    source = "storethat", type = "aggregate", active_contract_tickers = tickers,
    start = start_date, end = end_date, file = data_file
  )
  
  commodity_CFTC_tickers <- tickers[tickers %in% tickers_cftc$`active contract ticker`]
  
  message("\n pull commodity CFTC data")
  commodity_CFTC_data <- pullit::pull_futures_CFTC(
    source = "storethat", active_contract_tickers = commodity_CFTC_tickers, 
    start = start_date, end = end_date, file = data_file
  )
  
  rm(tickers_cftc, envir = .GlobalEnv)
  list(
    `futures individual` = commodity_futures_data, `futures aggregate` = commodity_aggregate_data,
    cftc = commodity_CFTC_data
    )
}

construct_factor_objects <- function(
    data, update_frequency, return_frequency, ranking_period, long_threshold, 
    short_threshold, weighted
){
  
  message("\n make market factor")
  market <- factorem::market_factor(
    data = data$`futures individual`, return_frequency = return_frequency, long = T
  )
  
  message("\n make CHP factor")
  CHP <- factorem::CHP_factor(
    price_data = data$`futures individual`, CHP_data = data$cftc, 
    update_frequency = update_frequency, return_frequency = return_frequency,
    ranking_period = ranking_period, 
    long_threshold = long_threshold, short_threshold = short_threshold, 
    weighted = weighted
  )
  
  message("\n make open interest nearby factor")
  `open interest nearby` <- factorem::OI_nearby_factor(
    data = data$`futures individual`, update_frequency = update_frequency,
    return_frequency = return_frequency, ranking_period = ranking_period,
    long_threshold = long_threshold, short_threshold = short_threshold,
    weighted = weighted
  )

  message("\n make open interest aggregate factor")
  `open interest aggregate` <- factorem::OI_aggregate_factor(
    price_data = data$`futures individual`, aggregate_data = data$`futures aggregate`,
    update_frequency = update_frequency, return_frequency = return_frequency,
    ranking_period = ranking_period,
    long_threshold = long_threshold, short_threshold = short_threshold,
    weighted = weighted
  )

  message("\n make term structure factor")
  `term structure` <- factorem::TS_factor(
    data = data$`futures individual`, update_frequency = update_frequency,
    return_frequency = return_frequency, front = 1L, back = 2L,
    ranking_period = ranking_period, long_threshold = long_threshold,
    short_threshold = short_threshold, weighted = weighted
  )

  list(
    market = market, CHP = CHP, `open interest nearby` = `open interest nearby`,
    `open interest aggregate` = `open interest aggregate`, `term structure` = `term structure`
  )
}

extract_factor_returns_into_dataframe <- function(factor_objects){
  
  furrr::future_imap_dfr(
    factor_objects, function(factor_object, factor_name){
      tidyr::gather(factor_object@returns, leg, return, -date) %>%
        dplyr::filter(! is.na(return)) %>%
        dplyr::mutate(name = gsub("`", "", factor_name))
    }
  ) %>% dplyr::relocate(name, .before = 1L) %>% tibble::as_tibble()
}

make_commodity_futures_factor_returns_dataframe <- function(
    tickers, start_date, end_date, 
    TS_positions, roll_type, roll_days, roll_months, roll_adjustment, data_file,
    update_frequency, return_frequency, ranking_period, long_threshold, 
    short_threshold, weighted
    ){
  
  data <- get_data_for_factor_construction(
    tickers, start_date, end_date, 
    TS_positions, roll_type, roll_days, roll_months, roll_adjustment, data_file
  )

  factors <- construct_factor_objects(
    data, update_frequency, return_frequency, ranking_period, long_threshold, 
    short_threshold, weighted
  )
  
  extract_factor_returns_into_dataframe(factors)
}

## load all ####################################################################
load_futures_levels_data <- function(){
  # individual data ####
  message("\n pull commodity futures levels individual data")
  commodity_futures_individual_data_levels <<- get_commodity_futures_individual_data_levels(
    commodity_futures_tickers, date_start, date_end, storethat_db_path
  )
  
  # aggregate data ####
  message("\n pull commodity futures levels aggregate data")
  commodity_futures_aggregate_data_levels <- get_commodity_futures_aggregate_data_levels(
    commodity_futures_tickers, date_start, date_end, storethat_db_path
  )
  
  # combined ####
  commodity_futures_data_levels <<- bind_individual_and_aggregate_commodity_futures_dataframes(
    commodity_futures_individual_data_levels, commodity_futures_aggregate_data_levels
  )
}

load_commodity_futures_data <- function(){
  message("\n load commodity futures data")
  # commodity futures levels data ####
  load_futures_levels_data()
  
  # commodity futures relative changes data ####
  commodity_futures_relative_changes <- 
    make_commodity_futures_relative_changes_dataframe(commodity_futures_data_levels)
  
  ## combined ####
  commodity_futures_data <<- make_commodity_futures_dataframe(
    commodity_futures_data_levels, commodity_futures_relative_changes
  )
}  

load_aggregate_CHP_data <- function(){
  data("tickers_cftc", package = "BBGsymbols")
  
  # commodity CFTC tickers ####
  commodity_CFTC_tickers <- commodity_futures_tickers[
    commodity_futures_tickers %in% tickers_cftc$`active contract ticker`
  ]
  
  # commodity CFTC data ####
  message("\n pull commodity CFTC data")
  commodity_CFTC_data <<- pullit::pull_futures_CFTC(
    source = "storethat", active_contract_tickers = commodity_CFTC_tickers, 
    start = date_start, end = date_end, file = storethat_db_path
  )
  
  # regimes ####
  rm(tickers_cftc, envir = .GlobalEnv)
  aggregate_CHP_regimes <<- make_aggregate_CHP_regimes_dataframe()
} 

load_commodity_index_returns <- function(){

  US_commodity_futures_tickers <<- filter_commodity_futures_tickers(
    commodity_futures_tickers, filter_country = "US"
  )
  
  first_period_boundaries <- get_period_boundaries(periods, "past")
  last_period_boundaries <- get_period_boundaries(periods, "present")
  
  commodity_futures_index_returns <<- make_commodity_futures_index_returns_dataframe(
    commodity_futures_individual_data_levels, US_commodity_futures_tickers, 
    first_period_boundaries$start, last_period_boundaries$end
  )
} 

load_commodity_factor_returns <- function(){

  # futures params ####
  TS_positions <- 1L:2L
  roll_type <- "A"
  roll_days = 0L
  roll_months = 0L
  roll_adjustment = "N"
  data_file = storethat_db_path
  # factor params ####
  update_frequency = "week"
  return_frequency <- "day"
  ranking_period <- 26L
  long_threshold <- 2/3
  short_threshold <- 1/3
  weighted = FALSE
  
  message("\n load commodity factors data")
  commodity_futures_factor_returns <<- make_commodity_futures_factor_returns_dataframe(
    US_commodity_futures_tickers, date_start, date_end, TS_positions, 
    roll_type, roll_days, roll_months, roll_adjustment, data_file,
    update_frequency, return_frequency, ranking_period, long_threshold, 
    short_threshold, weighted
  )
} 


load_data <- function(){
  ## time boundaries ####
  date_start <<- "1996-01-01"; date_end <<- "2018-12-14"
  
  ## individual commodity futures tickers ####
  commodity_futures_tickers <<- c(
    "BOA Comdty", "C A Comdty", "CCA Comdty", "CLA Comdty", "CTA Comdty", 
    "FCA Comdty", "GCA Comdty", "HGA Comdty", "HOA Comdty", "JOA Comdty", 
    "KCA Comdty", "LAA Comdty", "LBA Comdty", "LCA Comdty", "LHA Comdty", 
    "LLA Comdty", "LNA Comdty", "LPA Comdty", "LTA Comdty", "LXA Comdty", 
    "NGA Comdty", "O A Comdty", "PAA Comdty", "PLA Comdty", "S A Comdty", 
    "SBA Comdty", "SIA Comdty", "SMA Comdty", "W A Comdty", "XBWA Comdty"
  )
  
  ## commodity futures data ####
  load_commodity_futures_data()
  
  ## period dates ####
  period_dates <<- make_period_dates_timeseries(commodity_futures_data, periods)
  
  ## aggregate CHP ####
  load_aggregate_CHP_data()
  
  ## commodity index returns ####
  load_commodity_index_returns()
  
  ## commodity factor returns ####
  load_commodity_factor_returns()
}

load_global_variables <- function(){

  load_variables()
  load_data()
  
  rm(
    commodity_CFTC_data, periods, date_start, date_end, storethat_db_path,
    commodity_futures_data_levels, commodity_futures_individual_data_levels,
    US_commodity_futures_tickers,
    envir = .GlobalEnv
    )
}

# analysis #####################################################################
## local functions #############################################################
get_all_tickers <- function(`futures individual dataset`) {
  data("tickers_futures", package = "BBGsymbols")
  data("exchanges", package = "fewISOs")
  
  tickers <- dplyr::distinct(`futures individual dataset`, `active contract ticker`) %>% 
    purrr::flatten_chr()
  tickers <- dplyr::left_join(
    dplyr::filter(tickers_futures, ticker %in% tickers) %>% dplyr::select(ticker, MIC),
    dplyr::select(exchanges, MIC, country), 
    by = "MIC"
  ) %>% dplyr::select(ticker) %>% purrr::flatten_chr()
  
  rm(tickers_futures, exchanges, envir = .GlobalEnv); return(tickers)
}

compute_analysis_by_year_whole <- function(commodity_futures_data, analysis_function){

  dplyr::mutate(commodity_futures_data, year = lubridate::year(date)) %>%
    dplyr::group_by(dplyr::across(-dplyr::all_of(c("active contract ticker", "date", "value")))) %>%
    tidyr::spread(`active contract ticker`, value) %>% tidyr::nest() %>%
    dplyr::mutate(results = purrr::map(data, analysis_function)) %>%
    dplyr::ungroup()
}

compute_analysis_by_year_regimes <- function(commodity_futures_data, analysis_function){
  
  dplyr::group_by(
    commodity_futures_data, dplyr::across(-dplyr::all_of(c("active contract ticker", "date", "value")))
    ) %>%
    tidyr::spread(`active contract ticker`, value) %>% tidyr::nest() %>%
    dplyr::mutate(results = purrr::map(data, analysis_function)) %>% 
    dplyr::filter(! is.na(regime))
}

add_aggregate_CHP_regimes_to_commodity_futures_data <- function(
    commodity_futures_data, aggregate_CHP_regimes_by_year
){
  dplyr::inner_join(commodity_futures_data, aggregate_CHP_regimes_by_year, by = "date")
}

compute_analysis_by_year <- function(
    commodity_futures_data, aggregate_CHP_regimes_by_year, analysis_function
    ){
  
  whole <- compute_analysis_by_year_whole(commodity_futures_data, analysis_function)

  commodity_futures_data_with_aggregate_CHP_regimes <- 
    add_aggregate_CHP_regimes_to_commodity_futures_data(commodity_futures_data, aggregate_CHP_regimes_by_year)

  regimes <- compute_analysis_by_year_regimes(
    commodity_futures_data_with_aggregate_CHP_regimes, analysis_function
    )
 
  list(whole = whole, regimes = regimes)
}

make_analysis_by_year_dataframe <- function(
    commodity_futures_data, aggregate_CHP_regimes_by_year, analysis_function
    ){
  
  analysis <- compute_analysis_by_year(
    commodity_futures_data, aggregate_CHP_regimes_by_year, analysis_function
    )
  
  dplyr::mutate(analysis$whole, regime = "whole period") %>% 
    dplyr::bind_rows(analysis$regimes) %>% 
    dplyr::select(type, frequency, field, year, regime, data, results) %>%
    dplyr::arrange(type, frequency, field, year, regime)
}


add_periods_to_commodity_futures_data <- function(commodity_futures_data, period_dates){
  
  dplyr::left_join(commodity_futures_data, period_dates, by = "date") %>%
    dplyr::filter(! is.na(period))
}

compute_analysis_by_period_whole <- function(commodity_futures_data, analysis_function){

  dplyr::group_by(
    commodity_futures_data, dplyr::across(-dplyr::all_of(c("active contract ticker", "date", "value")))
    ) %>%
    tidyr::spread(`active contract ticker`, value) %>% tidyr::nest() %>%
    dplyr::mutate(results = purrr::map(data, analysis_function)) %>%
    dplyr::ungroup()
}

compute_analysis_by_period_regimes <- function(commodity_futures_data, analysis_function){
  
  dplyr::group_by(
    commodity_futures_data, dplyr::across(-dplyr::all_of(c("active contract ticker", "date", "value")))
    ) %>%
    tidyr::spread(`active contract ticker`, value) %>% tidyr::nest() %>%
    dplyr::mutate(results = purrr::map(data, analysis_function)) %>% 
    dplyr::filter(! is.na(regime))
}

compute_analysis_by_period <- function(
    commodity_futures_data, aggregate_CHP_regimes_by_period, analysis_function
    ){
  
  whole <- compute_analysis_by_period_whole(commodity_futures_data, analysis_function)
  
  commodity_futures_data_with_aggregate_CHP_regimes <- 
    add_aggregate_CHP_regimes_to_commodity_futures_data(
      commodity_futures_data, dplyr::select(aggregate_CHP_regimes_by_period, !period)
      )
  regimes <- compute_analysis_by_period_regimes(
    commodity_futures_data_with_aggregate_CHP_regimes, analysis_function
    )
  
  list(whole = whole, regimes = regimes)
}

make_analysis_by_period_dataframe <- function(
    commodity_futures_data, aggregate_CHP_regimes_by_period, period_dates, analysis_function
    ){
  
  analysis <- compute_analysis_by_period(
    commodity_futures_data, aggregate_CHP_regimes_by_period, analysis_function
    )
  
  dplyr::mutate(analysis$whole, regime = "whole period") %>% 
    dplyr::bind_rows(analysis$regimes) %>% 
    dplyr::select(type, frequency, field, period, regime, data, results) %>%
    dplyr::mutate(period = factor(period, levels = unique(period_dates$period))) %>%
    dplyr::arrange(type, frequency, field, period, regime)
}

compute_analysis_between_tickers <- function(
    tickers, commodity_futures_data, aggregate_CHP_regimes, period_dates,
    analysis_function
    ){
  
  commodity_futures_data <- dplyr::filter(
    commodity_futures_data, `active contract ticker` %in% tickers,
    date >= period_dates$date[[1L]], date <= period_dates$date[[length(period_dates$date)]]
    )
  
  aggregate_CHP_regimes_by_year <- dplyr::filter(aggregate_CHP_regimes, timespan == "year") %>%
    dplyr::select(regimes) %>% purrr::flatten_df()
  analysis_by_year <- make_analysis_by_year_dataframe(
    commodity_futures_data, aggregate_CHP_regimes_by_year, analysis_function
    )
  
  commodity_futures_data_with_periods <- 
    add_periods_to_commodity_futures_data(commodity_futures_data, period_dates)
  aggregate_CHP_regimes_by_period <- dplyr::filter(aggregate_CHP_regimes, timespan == "period") %>%
    dplyr::select(regimes) %>% purrr::flatten_df()
  analysis_by_period <- make_analysis_by_period_dataframe(
    commodity_futures_data_with_periods, aggregate_CHP_regimes_by_period, period_dates, 
    analysis_function
    )
  
  tibble::tibble(timespan = c("year", "period"), results = list(analysis_by_year, analysis_by_period))
}

make_analysis_for_ticker_combinations_dataframe <- function(
    combinations, commodity_futures_data, aggregate_CHP_regimes, period_dates, analysis_function
){
  
  furrr::future_map(combinations, ~{
    compute_analysis_between_tickers(
      .x, commodity_futures_data, aggregate_CHP_regimes, period_dates, analysis_function
    )
  })
}

## regime difference tests #####################################################
make_tickers_list_for_regime_difference_tests <- function(){
  c(
    commodity_futures_tickers,
    unique(commodity_futures_country_indexes_returns$ticker)
    )
}

make_tickers_periods_combinations <- function(){
  tickers <- make_tickers_list_for_regime_difference_tests()
  
  tidyr::expand_grid(tickers, dplyr::distinct(period_dates, period))  %>%
    setNames(c("ticker", "period"))
}

make_period_boundary_dates <- function(period){
  dates <- dplyr::filter(period_dates, period == !!period)$date
  list(
    start = dates[[1L]], end = dates[[NROW(dates)]]
  )
}

extract_commodity_individual_futures_returns <- function(){
  dplyr::filter(
    commodity_futures_data, type == "return", frequency == "day", field == "PX_LAST"
  ) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::select(ticker = `active contract ticker`, date, return = value)
}

combine_commodity_individual_and_country_indexes_futures_returns <- function(){
  individidual_returns <- extract_commodity_individual_futures_returns()
  
  commodity_futures_returns <<- 
    dplyr::bind_rows(individidual_returns, commodity_futures_country_indexes_returns) %>%
    dplyr::arrange(ticker, date)
}

make_returns_timseries_for_ticker_period_combination <- function(ticker){

  dplyr::filter(
    commodity_futures_returns, ticker == !!ticker, 
    date >= date_start, date <= date_end
    ) %>%
    dplyr::select(date, return) %>% dplyr::arrange(date)
}

extract_aggregate_CHP_regimes_for_period <- function(){
  dplyr::filter(aggregate_CHP_regimes, timespan == "period")$regimes[[1L]] %>%
    dplyr::filter(date >= date_start, date <= date_end) %>% dplyr::select(date, regime)
}

split_returns_by_regime <- function(returns, regimes){
  
  dplyr::left_join(returns, regimes, by = "date") %>% 
    dplyr::filter(! is.na(regime)) %>% dplyr::group_by(regime) %>% 
    dplyr::summarise(returns = list(return), .groups = "drop") %>%  
    tibble::deframe()
}

make_returns_list_split_by_regime_for_ticker_period_combination <- 
  function(ticker){
  
  returns <- make_returns_timseries_for_ticker_period_combination(ticker)
  aggregate_CHP_regimes <- extract_aggregate_CHP_regimes_for_period()
  
  split_returns_by_regime(returns, aggregate_CHP_regimes)
}

identify_dominant_regime_for_moment <- function(returns_split_by_regime, moment){
  
  moment_function <- purrr::partial(match.fun(moment), na.rm = TRUE)
  moments <- purrr::map(returns_split_by_regime, moment_function)
  
  names(moments)[which.max(moments)]
}

test_difference_for_mean <- function(returns_split_by_regime){
  
  t.test(returns_split_by_regime$contango, returns_split_by_regime$backwardation)
}

test_difference_for_var <- function(returns_split_by_regime){
  
  var.test(returns_split_by_regime$contango, returns_split_by_regime$backwardation)
}

test_difference_for_moment <- function(returns_split_by_regime, moment){
  
  do.call(what = paste0("test_difference_for_", moment), args = list(returns_split_by_regime))
}

make_analysis_for_combination <- function(ticker, period){
  
  period_boundary_dates <- make_period_boundary_dates(period)
  date_start <<- period_boundary_dates$start; date_end <<- period_boundary_dates$end
  
  returns_split_by_regime <- make_returns_list_split_by_regime_for_ticker_period_combination(ticker)

  results <- purrr::map_df(c("mean", "var"), function(moment){
    tibble::tibble(
      moment = moment, 
      `dominant regime` = identify_dominant_regime_for_moment(returns_split_by_regime, moment),
      `test results` = list(test_difference_for_moment(returns_split_by_regime, moment))
    )
  })
  
  rm(date_start, date_end, envir = .GlobalEnv); return(results)
}

make_commodity_futures_country_indexes_returns_dataframe <- function(){
  
  individual_commodity_returns <- dplyr::filter(
    commodity_futures_data, type == "return", frequency == "day", field == "PX_LAST"
  ) %>% dplyr::select(ticker = `active contract ticker`, date, return = value)
  
    dplyr::left_join(
      individual_commodity_returns,
      make_ticker_country_dataframe(unique(individual_commodity_returns$ticker)),
      by = "ticker"
    ) %>% dplyr::group_by(country, date) %>% 
      dplyr::summarise(return = mean(return, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(ticker = paste(country, "commodities", sep = " ")) %>%
      dplyr::select(ticker, date, return)
}

make_regime_difference_tests <- function(){
  
  commodity_futures_country_indexes_returns <<- 
    make_commodity_futures_country_indexes_returns_dataframe()
  combine_commodity_individual_and_country_indexes_futures_returns()
  
  combinations <- make_tickers_periods_combinations()
  
  results <- dplyr::rowwise(combinations) %>% dplyr::mutate(
    results = list(make_analysis_for_combination(ticker, period)) 
  )
  
  rm(
    commodity_futures_country_indexes_returns, commodity_futures_returns, envir = .GlobalEnv
    )
  return(results)
}

## correlations ################################################################
### local functions ############################################################
compute_correlations <- function(df) {
  cor( dplyr::select(df, -date), use = "pairwise.complete.obs")
}

make_pairwise_correlations_for_ticker_combinations_dataframe <- function(
    combinations, commodity_futures_data, aggregate_CHP_regimes, period_dates
    ){
  
  analysis <- make_analysis_for_ticker_combinations_dataframe(
    combinations$tickers, commodity_futures_data, aggregate_CHP_regimes, period_dates, 
    compute_correlations
  )
  
  dplyr::mutate(combinations, results = analysis)
}

## regressions #################################################################
### index ######################################################################
#### local functions ####
make_commodity_futures_dataset_for_regression_index_analysis <- function(
    commodity_futures_data, commodity_futures_index_returns
){
 
  commodity_futures_individual_returns <- dplyr::filter(
    commodity_futures_data, type == "return", field == "PX_LAST"
    )
  commodity_futures_index_returns <- 
    tidyr::unnest(commodity_futures_index_returns, returns) %>%
    dplyr::mutate(
      type = "return", `active contract ticker` = "index", field = "PX_LAST"
      ) %>%
    dplyr::select(
      type, frequency,`active contract ticker`, field, date, value = return
    )
  
  dplyr::bind_rows(commodity_futures_individual_returns, commodity_futures_index_returns)
}

make_sanitised_data_list_for_regressions_index <- function(df){
  
  tickers <- dplyr::setdiff(names(df), c("date", "index")) %>%
    make.names(unique = TRUE)
  colnames(df) <- make.names(colnames(df), unique = TRUE)
  
  list(sanitised_commodity_futures_tickers = tickers, df = df)
}

compute_regressions_index <- function(df) {
  
  unsanitised_commodity_futures_tickers <- dplyr::setdiff(names(df), c("date", "index"))
  data <- make_sanitised_data_list_for_regressions_index(df)
  
  models <- purrr::map(data$sanitised_commodity_futures_tickers, function(ticker) {
    
    formula <- as.formula(paste(ticker, "~", "index"))
    lm(formula, data = data$df)
  })
  
  tibble::tibble(`active contract ticker` = unsanitised_commodity_futures_tickers, model = models)
}

add_index_to_commodity_pool_tickers_dataframe <- function(commodity_pool_tickers_dataframe){
  
  commodity_pool_tickers_dataframe$tickers <- lapply(
    commodity_pool_tickers_dataframe$tickers, function(tickers) c(tickers, "index")
    )
  
  commodity_pool_tickers_dataframe
}

make_regressions_index_for_ticker_combinations_dataframe <- function(
    combinations, commodity_futures_data, commodity_futures_index_returns, 
    aggregate_CHP_regimes, period_dates
    ){
  
  combinations <- add_index_to_commodity_pool_tickers_dataframe(combinations)
  
  commodity_futures_data <- make_commodity_futures_dataset_for_regression_index_analysis(
    commodity_futures_data, commodity_futures_index_returns
  )
  
  analysis <- make_analysis_for_ticker_combinations_dataframe(
    combinations$tickers, commodity_futures_data, aggregate_CHP_regimes, period_dates, 
    compute_regressions_index
  )
  
  dplyr::mutate(combinations, results = analysis)
}

### factors ####################################################################
#### local functions ####
make_sanitised_data_list_for_regressions_factor <- function(df){
  
  tickers <- names(df)[grep("Comdty", names(df))]
  names_tickers <- make.names(tickers, unique = TRUE)
  factors <- dplyr::setdiff(names(df), c("date", tickers))
  names_factors <- make.names(factors, unique = TRUE)
  colnames(df) <- make.names(colnames(df), unique = TRUE)
  
  list(
    sanitised_commodity_futures_tickers = names_tickers, 
    sanitised_commodity_futures_factor_names = names_factors, 
    df = df
    )
}

compute_regressions_factor <- function(df) {

  unsanitised_commodity_futures_tickers <- names(df)[grep("Comdty", names(df))]
  unsanitised_commodity_futures_factor_names <- 
    dplyr::setdiff(names(df), c("date", unsanitised_commodity_futures_tickers))
  data <- make_sanitised_data_list_for_regressions_factor(df)
  
  combinations <- expand.grid(
    data$sanitised_commodity_futures_tickers, data$sanitised_commodity_futures_factor_names
  ) %>% setNames(c("ticker", "factor"))

  models <- purrr::pmap(combinations, function(ticker, factor){
    formula <- as.formula(paste(ticker, "~", factor))
    lm(formula, data = data$df)
  })
  
  dplyr::mutate(combinations, model = models, factor = gsub("\\.\\.\\.", " - ", factor)) %>%
    tidyr::separate(factor, into = c("factor", "leg"), sep = " - ") %>%
    dplyr::mutate(dplyr::across(c(factor, ticker), ~gsub("\\.", " ", .x))) %>%
    dplyr::rename(`active contract ticker` = ticker) %>% tibble::as_tibble()
}

make_commodity_futures_dataset_for_regression_factor_analysis <- function(
    commodity_futures_data, commodity_futures_factor_returns
    ){
  
  futures_returns <- dplyr::filter(
    commodity_futures_data, type == "return", frequency == "day", field == "PX_LAST"
  )
  
  factor_returns <- dplyr::mutate(
    commodity_futures_factor_returns, `active contract ticker` = paste(name, leg, sep = " - "),
    type = "return", frequency = "day", field = "PX_LAST"
  ) %>% dplyr::select(-c(name, leg)) %>% dplyr::rename(value = return)
  
  dplyr::bind_rows(futures_returns, factor_returns)
}

make_factor_names_from_returns_dataframe <- function(commodity_futures_factor_returns_dataframe){
  
  dplyr::distinct(commodity_futures_factor_returns_dataframe, name, leg) %>% 
    dplyr::mutate(name = paste(name, leg, sep = " - ")) %>% 
    dplyr::select(name) %>% purrr::flatten_chr()
}

add_factors_to_commodity_pool_tickers_dataframe <- function(factors, commodity_pool_tickers_dataframe){
  
  commodity_pool_tickers_dataframe$tickers <- lapply(
    commodity_pool_tickers_dataframe$tickers, function(tickers) c(tickers, factors)
  )
  
  commodity_pool_tickers_dataframe
}

make_regressions_factors_for_ticker_combinations_dataframe <- function(
    combinations, commodity_futures_data, commodity_futures_factor_returns, 
    aggregate_CHP_regimes, period_dates
){
  
  factor_names <- make_factor_names_from_returns_dataframe(commodity_futures_factor_returns)
  combinations <- add_factors_to_commodity_pool_tickers_dataframe(factor_names, combinations)
  
  commodity_futures_data <- make_commodity_futures_dataset_for_regression_factor_analysis(
    commodity_futures_data, commodity_futures_factor_returns
    )
  
  analysis <- make_analysis_for_ticker_combinations_dataframe(
    combinations$tickers, commodity_futures_data, aggregate_CHP_regimes, period_dates, 
    compute_regressions_factor
  )
  
  dplyr::mutate(combinations, results = analysis)
}

# summary ######################################################################
## local functions #############################################################
add_summary_to_analysis_raw_results_for_ticker_combinations_dataframe <- function(
    analysis_raw_results_for_ticker_combinations_dataframe, summary_label, summary_function
){
  
  dplyr::mutate(
    analysis_raw_results_for_ticker_combinations_dataframe, 
    !!summary_label := purrr::map(results, function(results){
      dplyr::mutate(results, results = purrr::map(results, function(results){
        dplyr::mutate(
          results, !!summary_label := furrr::future_map(results, summary_function)
        ) %>% dplyr::select(-c("data", "results"))
      }))
    }))
}

## regime difference tests #####################################################
extract_pvalues_from_test_objects <- function(results){
  
  tidyr::unnest(results, results) %>% 
    dplyr::mutate(`p-value` = purrr::map_dbl(`test results`, ~.$p.value)) %>% 
    dplyr::select(-`test results`)
}

## correlations ################################################################
extract_top_n_pairwise_correlations_from_correlation_matrix <- function(correlation_matrix, n = 3){
  
  pairwise_correlations <- tibble::rownames_to_column(as.data.frame(correlation_matrix), var = "ticker 1") %>%
    tidyr::pivot_longer(-`ticker 1`, names_to = "ticker 2", values_to = "correlation") %>%
    dplyr::filter(`ticker 1` != `ticker 2`) %>% 
    dplyr::mutate(pair = pmin(`ticker 1`, `ticker 2`) %>% paste(pmax(`ticker 1`, `ticker 2`), sep = "-")) %>%
    dplyr::distinct(pair, .keep_all = TRUE) %>% dplyr::select(-pair)
  
  top_n <- dplyr::slice_max(pairwise_correlations, correlation, n = n) %>%
    dplyr::mutate(`top-bottom 3` = rep("top", dplyr::n()))
  bottom_n <- dplyr::slice_min(pairwise_correlations, correlation, n = n) %>%
    dplyr::mutate(`top-bottom 3` = rep("bottom", dplyr::n()))

  dplyr::bind_rows(top_n, bottom_n) %>% dplyr::relocate(`top-bottom 3`, .before = `ticker 1`)
}

compute_average_pairwise_correlations_from_correlation_matrix <- function(correlation_matrix){

  pairwise_correlations <- tibble::rownames_to_column(as.data.frame(correlation_matrix), var = "ticker 1") %>%
    tidyr::pivot_longer(-`ticker 1`, names_to = "ticker 2", values_to = "correlation") %>%
    dplyr::filter(`ticker 1` != `ticker 2`) %>% 
    dplyr::mutate(pair = pmin(`ticker 1`, `ticker 2`) %>% paste(pmax(`ticker 1`, `ticker 2`), sep = "-")) %>%
    dplyr::distinct(pair, .keep_all = TRUE) %>% dplyr::select(-pair)
  
  mean(pairwise_correlations$correlation, na.rm = T)
}

add_top_3_and_average_to_pairwise_correlations_for_ticker_combinations_dataframe <- function(
    pairwise_correlations_for_ticker_combinations_dataframe
){
  
  add_summary_to_analysis_raw_results_for_ticker_combinations_dataframe(
    pairwise_correlations_for_ticker_combinations_dataframe, "top 3",
    extract_top_n_pairwise_correlations_from_correlation_matrix
  ) %>%
    add_summary_to_analysis_raw_results_for_ticker_combinations_dataframe(
      "average", compute_average_pairwise_correlations_from_correlation_matrix
    ) %>% 
    dplyr::select(-c("tickers", "results"))
}

## regressions #################################################################
### index ######################################################################
extract_top_n_betas_from_lm_models <- function(lm_models){
  
  dplyr::mutate(
    lm_models, 
    beta = purrr::map_dbl(model, ~coef(.x)[["index"]]), 
    `p value` = purrr::map_dbl(model, ~base::summary(.x)$coefficients["index", "Pr(>|t|)"]),
    `R squared` = purrr::map_dbl(model, ~base::summary(.x)$r.squared)
  ) %>% 
    dplyr::select(-model) %>% dplyr::rename(ticker = `active contract ticker`) %>% 
    dplyr::arrange(dplyr::desc(beta)) %>% dplyr::slice(1L:3L) %>%
    dplyr::mutate(`top-bottom 3` = factor("top")) %>%
    dplyr::relocate(`top-bottom 3`, .before = dplyr::everything())
}

compute_average_beta_from_lm_models <- function(lm_models){
  
  dplyr::mutate(lm_models, beta = purrr::map_dbl(model, ~coef(.x)[["index"]])) %>% 
    dplyr::select(-model) %>% dplyr::summarise(average = mean(beta, na.rm = T)) %>% 
    dplyr::pull(average)
}

add_top_3_and_average_to_regressions_index_for_ticker_combinations_dataframe <- function(
    regressions_for_ticker_combinations_dataframe
){
  
  add_summary_to_analysis_raw_results_for_ticker_combinations_dataframe(
    regressions_for_ticker_combinations_dataframe, "top 3", extract_top_n_betas_from_lm_models
  ) %>%
    add_summary_to_analysis_raw_results_for_ticker_combinations_dataframe(
      "average", compute_average_beta_from_lm_models
    ) %>% 
    dplyr::select(-c("tickers", "results"))
}

### factor #####################################################################
average_Rsquared_by_factor_leg_for_each_type_frequency_field_period_regime_combination <- function(regressions_factors_raw){
  
  dplyr::mutate(regressions_factors_raw, averages = purrr::map(results, function(averages){
    dplyr::mutate(averages, averages = purrr::map(results, function(averages){
      dplyr::mutate(averages, averages = purrr::map(results, function(averages){
        dplyr::mutate(averages, r.squared = purrr::map_dbl(model, ~base::summary(.x)$r.squared)) %>%
          dplyr::group_by(factor, leg) %>% 
          dplyr::summarise(average = mean(r.squared, na.rm = T), .groups = "drop")
      })) %>% dplyr::select(-c(data, results)) %>% tidyr::unnest(averages)
    })) %>% dplyr::select(-results)
  })) %>% dplyr::select(-c(tickers, results))
}

# format #######################################################################
## local functions #############################################################
unnest_analysis_statistic_results_summary <- function(results_summary, statistic){
  
  statistic_sym <- rlang::sym(statistic)
  
  dplyr::select(results_summary, country, sector, subsector, !!statistic_sym) %>%
    tidyr::unnest(!!statistic_sym) %>% tidyr::unnest(results) %>% tidyr::unnest(!!statistic_sym) %>%
    dplyr::select(
      country, sector, subsector, timespan, period, year, type, frequency, field,
      regime, dplyr::everything()
    )
}

make_field_names <- function(){
  c(
    "close price", "open interest", "volume", "aggregate open interest", 
    "aggregate volume", "ask", "bid", "high", "low", "mid", "open"
  )
}

make_field_tickers <- function(){
  c(
    "PX_LAST", "OPEN_INT", "PX_VOLUME", "FUT_AGGTE_OPEN_INT", 
    "FUT_AGGTE_VOL", "PX_ASK", "PX_BID", "PX_HIGH", "PX_LOW", "PX_MID", "PX_OPEN"
  )
}

make_field_to_name_map <- function(){
  tibble::tibble( field = make_field_tickers(), name = make_field_names())
}

map_solution_to_problem_domain_jargon_in_analysis_unnested_results_summary <- function(
    unnested_results_summary
){
  
  field_to_name_map <- make_field_to_name_map()
  dplyr::left_join(unnested_results_summary, field_to_name_map, by = "field") %>%
    dplyr::select(-field) %>% dplyr::rename(field = name) %>% dplyr::relocate(field, .after = frequency)
}

make_format_factor_levels <- function(){
  list(
    country = c("all", "US", "GB"),
    timespan = c("period", "year"),
    type = c("return", "level"),
    frequency = c("day", "week", "month"),
    field = make_field_names(),
    regime = c("whole period", "backwardation", "contango")
  )
}

mutate_appropriate_columns_to_factors_in_analysis_unnested_results_summary <- function(
    unnested_top_3_results_summary
){
  
  levels <- make_format_factor_levels()
  
  dplyr::mutate(
    unnested_top_3_results_summary,
    country = factor(country, levels = levels$country),
    timespan = factor(timespan, levels = levels$timespan),
    type = factor(type, levels = levels$type),
    frequency = factor(frequency, levels = levels$frequency),
    field = factor(field, levels = levels$field),
    regime = factor(regime, levels = levels$regime)
  )
}

arrange_columns_in_analysis_results_summary <- function(formatted_analysis_results_summary){
  
  dplyr::select(
    formatted_analysis_results_summary,
    field, type, frequency, country, sector, subsector, timespan, period, year, regime, dplyr::everything()
  ) %>%
    dplyr::arrange(
      field, type, frequency, country, sector, subsector, timespan, period, year, regime
    )
}

## regime difference tests #####################################################
format_regime_difference_tests_summary_into_table <- function(summary){
  
  dplyr::mutate(summary, `p-value` = scales::number(`p-value`, accuracy = 0.0001))
}

## correlations ################################################################
### top 3 ######################################################################
#### local functions ####
map_solution_to_problem_domain_jargon_in_correlation_top_3_unnested_results_summary <- function(
    unnested_correlations_top_3_results_summary
){
  data("tickers_futures", package = "BBGsymbols")
  
  summary <- dplyr::left_join(
    unnested_correlations_top_3_results_summary, 
    dplyr::select(tickers_futures, ticker, name, MIC), 
    by = c("ticker 1" = "ticker")
    ) %>%
    dplyr::mutate(`ticker 1` = paste0(name, " (", MIC, ")")) %>% dplyr::select(-c(name, MIC)) %>%
    dplyr::left_join(dplyr::select(tickers_futures, ticker, name, MIC), by = c("ticker 2" = "ticker")) %>%
    dplyr::mutate(`ticker 2` = paste0(name, " (", MIC, ")")) %>% dplyr::select(-c(name, MIC)) %>%
    dplyr::mutate(pair = paste(`ticker 1`, `ticker 2`, sep = " - ")) %>% 
    dplyr::select(-c(`ticker 1`, `ticker 2`)) %>% dplyr::relocate(pair, .before = correlation)
  
  rm(tickers_futures, envir = .GlobalEnv); return(summary)
}
####

format_correlation_summary_statistics_into_table <- function(correlations_summary){
  
  top_3 <- unnest_analysis_statistic_results_summary(correlations_summary, "top 3") %>%
    dplyr::mutate(`top-bottom 3` = factor(`top-bottom 3`, levels = c("top", "bottom"))) %>%
    map_solution_to_problem_domain_jargon_in_correlation_top_3_unnested_results_summary()
  
  average <- unnest_analysis_statistic_results_summary(correlations_summary, "average")
  
  dplyr::left_join(
    top_3, average, 
    by = c("country", "sector", "subsector", "timespan", "period", "year", "type", "frequency", "field", "regime")
    ) %>% dplyr::relocate(average, .after = regime) %>%
    map_solution_to_problem_domain_jargon_in_analysis_unnested_results_summary() %>%
    mutate_appropriate_columns_to_factors_in_analysis_unnested_results_summary() %>%
    dplyr::mutate(dplyr::across(.cols = c(average, correlation), ~round(.x, digits = 4L))) %>%
    arrange_columns_in_analysis_results_summary()
}

## regressions #################################################################
### index ######################################################################
#### top 3 ####
##### local functions ####
map_solution_to_problem_domain_jargon_in_regressions_index_top_3_unnested_results_summary <- function(
    unnested_regressions_top_3_results_summary
){
  data("tickers_futures", package = "BBGsymbols")
  
  summary <- dplyr::left_join(
    unnested_regressions_top_3_results_summary, 
    dplyr::select(tickers_futures, ticker, name, MIC), 
    by = "ticker"
  ) %>%
    dplyr::mutate(commodity = paste0(name, " (", MIC, ")")) %>% 
    dplyr::select(-c(name, MIC, ticker)) %>%
    dplyr::relocate(commodity, .after = regime)
  
  rm(tickers_futures, envir = .GlobalEnv); return(summary)
}
  
arrange_columns_in_regression_index_results_summary <- function(formatted_regression_results_summary){
  
  dplyr::select(
    formatted_regression_results_summary,
    country, sector, subsector, timespan, period, year, regime, dplyr::everything()
  ) %>%
    dplyr::arrange(
      country, sector, subsector, timespan, period, year, regime
    )
}

format_regression_index_summary_statistics_into_table <- function(regressions_summary){
  
  top_3 <- unnest_analysis_statistic_results_summary(regressions_summary, "top 3") %>%
    map_solution_to_problem_domain_jargon_in_regressions_index_top_3_unnested_results_summary()
  
  average <- unnest_analysis_statistic_results_summary(regressions_summary, "average")

  dplyr::left_join(
    top_3, average,
    by = c("country", "sector", "subsector", "timespan", "period", "year", "type", "frequency", "field", "regime")
  ) %>% 
    dplyr::relocate(average, .after = regime) %>%
    map_solution_to_problem_domain_jargon_in_analysis_unnested_results_summary() %>%
    mutate_appropriate_columns_to_factors_in_analysis_unnested_results_summary() %>%
    dplyr::mutate(dplyr::across(c(average, beta, `p value`, `R squared`), ~round(.x, digits = 4L))) %>%
    arrange_columns_in_analysis_results_summary()
}

### factor #####################################################################
unnest_regressions_factors_averages <- function(regressions_summary){
  
  tidyr::unnest(regressions_summary, averages) %>% tidyr::unnest(averages) %>%
    dplyr::select(
      country, sector, subsector, timespan, period, year, type, frequency, field,
      regime, dplyr::everything()
    ) %>% dplyr::relocate(regime, .before = average)
}

mutate_appropriate_columns_to_factors_in_factors_analysis_unnested_results_summary <- function(
    unnested_top_3_results_summary
){
  
  dplyr::mutate(
    unnested_top_3_results_summary,
    factor = factor(
      factor, levels = c("market", "CHP", "open interest nearby", "open interest aggregate", "term structure")
      ),
    leg = factor(leg, levels = c("factor", "long", "short")),
  )
}

format_regression_factor_summary_statistics_into_table <- function(regressions_summary){
  
  unnest_regressions_factors_averages(regressions_summary) %>%
    map_solution_to_problem_domain_jargon_in_analysis_unnested_results_summary() %>%
    mutate_appropriate_columns_to_factors_in_analysis_unnested_results_summary() %>%
    mutate_appropriate_columns_to_factors_in_factors_analysis_unnested_results_summary() %>%
    dplyr::mutate(average = round(average, digits = 4L)) %>%
    dplyr::select(
      field, type, frequency, country, sector, subsector, timespan, period, year, factor, leg, regime, 
      dplyr::everything()
    ) %>%
    dplyr::arrange(
      field, type, frequency, country, sector, subsector, timespan, period, year, factor, leg, regime
    )
}


