



# globals ####
## variables ####
### local function ####
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

## functions ####
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
  
  dplyr::left_join(
    dplyr::filter(tickers_futures, ticker %in% tickers) %>% dplyr::select(ticker, sector, subsector, MIC), 
    dplyr::select(exchanges, MIC, country), 
    by = "MIC"
  ) %>% dplyr::select(-MIC)
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

make_US_commodity_pool_combinations <- function(){
  
  tibble::tibble(
    country = rep("US", 8L),
    sector = c(rep("agriculturals", 4L), rep("energy", 2L), rep("metals", 2L)),
    subsector = c(
      "all", "grains", "livestock", "softs", "all", "petroleum", "all", "precious"
      )
  )
}

make_commodity_pool_tickers_dataframe_for_country_sectors_subsectors_combinations <- function(combinations, all_tickers){
  
  dplyr::rowwise(combinations) %>% 
    dplyr::mutate(
      tickers = list(filter_commodity_futures_tickers(all_tickers, country, sector, subsector))
    )
}

make_commodity_pool_tickers_dataframe <- function(all_tickers){
  
  all_all_all <- tibble::tibble(
    country = "all", sector = "all", subsector = "all", 
    tickers = list(filter_commodity_futures_tickers(all_tickers))
    )
  
  US_all_all <- tibble::tibble(
    country = "US", sector = "all", subsector = "all", 
    tickers = list(filter_commodity_futures_tickers(all_tickers, "US"))
  )
  
  US_combinations <- make_US_commodity_pool_combinations() %>%
    make_commodity_pool_tickers_dataframe_for_country_sectors_subsectors_combinations(all_tickers)
  
  GB <- tibble::tibble(
    country = "GB", sector = "all", subsector = "all", 
    tickers = list(filter_commodity_futures_tickers(all_tickers, "GB"))
  )
  
  dplyr::bind_rows(all_all_all, US_all_all, US_combinations, GB)
}

## commodity futures levels data ####
### individual data ####
### aggregate data ####
### both ####
#### local functions ####
bind_individual_and_aggregate_commodity_futures_dataframes <- function(individual, aggregate){
  dplyr::filter(individual, `TS position` == 1L) %>%
    dplyr::select(`active contract ticker`, field, date, value) %>%
    dplyr::bind_rows(dplyr::rename(
      pullit::get_data(aggregate), `active contract ticker` = ticker)
    )
}

## commodity futures relative changes data ####
### local functions ####
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

## bind levels and relative changes ####
### local functions ####
make_commodity_futures_dataframe <- function(levels, relative_changes){
  dplyr::mutate(levels, type = "level", frequency = NA) %>%
    dplyr::bind_rows(
      dplyr::mutate(relative_changes, type = "return")
    ) %>% dplyr::select(type, frequency, dplyr::everything()) %>%
    dplyr::arrange(type, frequency, `active contract ticker`, field, date)
}

## period dates ####
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

## aggregate CHP ####
### aggregate CHP data ####
#### local functions ####
load_CFTC_data <- function(){
  dplyr::left_join(
    commodity_CFTC_data@data, 
    dplyr::select(tickers_cftc, MIC, format, underlying, unit, participant, position, ticker), 
    by = "ticker"
  )
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
    dplyr::summarise(aggregate_CHP = mean(pressure, na.rm = T)) %>%
    dplyr::ungroup()
}

make_aggregate_CHP <- function(){
  load_CFTC_data() %>% filter_CFTC_data() %>% compute_individual_CHP() %>%
    compute_aggregate_CHP()
}

### aggregate CHP regimes ####
#### local functions ####
match_regimes_to_futures_data_dates <- function(regimes, commodity_futures_data_dates){
  
  dplyr::mutate(
    commodity_futures_data_dates, year = lubridate::year(date), week = lubridate::week(date)
  ) %>%
    dplyr::left_join(regimes, by = c("year", "week")) %>% 
    dplyr::select(-week) %>% dplyr::filter(! is.na(regime))
}

#### aggregate CHP regimes by year ####
##### local functions ####
compute_regimes_by_year <- function(aggregate_CHP){
  
  dplyr::mutate(aggregate_CHP, year = lubridate::year(date), week = lubridate::week(date)) %>% 
    dplyr::group_by(year) %>% 
    dplyr::mutate(regime = ifelse(aggregate_CHP < median(aggregate_CHP), "backwardation", "contango")) %>% 
    dplyr::ungroup() %>% dplyr::select(year, week, regime)
}

format_dataframe_aggregate_CHP_regimes_by_year <- function(aggregate_CHP_regimes){
  
  dplyr::mutate(aggregate_CHP_regimes) %>% dplyr::select(date, year, regime )%>% dplyr::arrange(date)
}

make_aggregate_CHP_regimes_by_year <- function(aggregate_CHP_data, commodity_futures_data){
  
  commodity_futures_data_dates <- dplyr::distinct(commodity_futures_data, date) %>% 
    dplyr::arrange(date)
  
  compute_regimes_by_year(aggregate_CHP_data) %>% 
    match_regimes_to_futures_data_dates(commodity_futures_data_dates) %>%
    format_dataframe_aggregate_CHP_regimes_by_year()
}

#### aggregate CHP regimes by subperiod ####
##### local functions ####
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

#### aggregate CHP regimes ####
##### local functions ####
make_aggregate_CHP_regimes_dataframe <- function(years, periods){
  tibble::tibble(timespan = c("years", "periods"), regimes = list(years, periods))
}

## construct commodity index returns ####
### local functions ####
compute_commodity_futures_index_returns <- function(price_levels){
  
  dplyr::group_by(price_levels, `active contract ticker`) %>%
    dplyr::mutate(return = value/dplyr::lag(value) - 1L) %>%
    dplyr::select(-value) %>% dplyr::group_by(date) %>%
    dplyr::summarise(return = mean(return, na.rm = T)) %>%
    dplyr::ungroup()
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
    frequency = c("daily", "weekly", "monthly"),
    returns = list(returns_daily, returns_weekly, returns_monthly)
  )
}

# analysis ####
## correlations ####
### correlations between all commodities (US + UK) ####
#### local functions ####
get_all_tickers <- function(`futures individual dataset`) {
  tickers <- dplyr::distinct(`futures individual dataset`, `active contract ticker`) %>% 
    purrr::flatten_chr()
  dplyr::left_join(
    dplyr::filter(tickers_futures, ticker %in% tickers) %>% dplyr::select(ticker, MIC),
    dplyr::select(exchanges, MIC, country), 
    by = "MIC"
  ) %>% dplyr::select(ticker) %>% purrr::flatten_chr()
}

compute_correlations <- function(df) {
  cor( dplyr::select(df, -date), use = "pairwise.complete.obs")
}

compute_correlations_by_year_whole <- function(commodity_futures_data){
  
  dplyr::mutate(commodity_futures_data, year = lubridate::year(date)) %>%
    dplyr::group_by(dplyr::across(-dplyr::all_of(c("active contract ticker", "date", "value")))) %>%
    tidyr::spread(`active contract ticker`, value) %>% tidyr::nest() %>%
    dplyr::mutate(correlations = purrr::map(data, compute_correlations)) %>%
    dplyr::ungroup()
}

compute_correlations_by_year_regimes <- function(commodity_futures_data){
  
  dplyr::group_by(
    commodity_futures_data, dplyr::across(-dplyr::all_of(c("active contract ticker", "date", "value")))
    ) %>%
    tidyr::spread(`active contract ticker`, value) %>% tidyr::nest() %>%
    dplyr::mutate(correlations = purrr::map(data, compute_correlations)) %>% 
    dplyr::filter(! is.na(regime))
}

add_aggregate_CHP_regimes_to_commodity_futures_data <- function(
    commodity_futures_data, aggregate_CHP_regimes_by_year
){
  dplyr::left_join(commodity_futures_data, aggregate_CHP_regimes_by_year, by = "date")
}

compute_correlations_by_year <- function(commodity_futures_data, aggregate_CHP_regimes_by_year){
  
  whole <- compute_correlations_by_year_whole(commodity_futures_data)
  
  commodity_futures_data_with_aggregate_CHP_regimes <- 
    add_aggregate_CHP_regimes_to_commodity_futures_data(commodity_futures_data, aggregate_CHP_regimes_by_year)
  regimes <- compute_correlations_by_year_regimes(commodity_futures_data_with_aggregate_CHP_regimes)
  
  list(whole = whole, regimes = regimes)
}

make_correlations_by_year_dataframe <- function(commodity_futures_data, aggregate_CHP_regimes_by_year){
  
  correlations <- compute_correlations_by_year(commodity_futures_data, aggregate_CHP_regimes_by_year)
  
  dplyr::mutate(correlations$whole, regime = "whole period") %>% 
    dplyr::bind_rows(correlations$regimes) %>% 
    dplyr::select(type, frequency, field, year, regime, data, correlations) %>%
    dplyr::arrange(type, frequency, field, year, regime)
}


add_periods_to_commodity_futures_data <- function(commodity_futures_data, period_dates){
  
  dplyr::left_join(commodity_futures_data, period_dates, by = "date") %>%
    dplyr::filter(! is.na(period))
}

compute_correlations_by_period_whole <- function(commodity_futures_data){

  dplyr::group_by(
    commodity_futures_data, dplyr::across(-dplyr::all_of(c("active contract ticker", "date", "value")))
    ) %>%
    tidyr::spread(`active contract ticker`, value) %>% tidyr::nest() %>%
    dplyr::mutate(correlations = purrr::map(data, compute_correlations)) %>%
    dplyr::ungroup()
}

compute_correlations_by_period_regimes <- function(commodity_futures_data){
  
  dplyr::group_by(
    commodity_futures_data, dplyr::across(-dplyr::all_of(c("active contract ticker", "date", "value")))
    ) %>%
    tidyr::spread(`active contract ticker`, value) %>% tidyr::nest() %>%
    dplyr::mutate(correlations = purrr::map(data, compute_correlations)) %>% 
    dplyr::filter(! is.na(regime))
}

compute_correlations_by_period <- function(commodity_futures_data, aggregate_CHP_regimes_by_period){
  
  whole <- compute_correlations_by_period_whole(commodity_futures_data)
  
  commodity_futures_data_with_aggregate_CHP_regimes <- 
    add_aggregate_CHP_regimes_to_commodity_futures_data(
      commodity_futures_data, dplyr::select(aggregate_CHP_regimes_by_period, !period)
      )
  regimes <- compute_correlations_by_period_regimes(commodity_futures_data_with_aggregate_CHP_regimes)
  
  list(whole = whole, regimes = regimes)
}

make_correlations_by_period_dataframe <- function(
    commodity_futures_data, aggregate_CHP_regimes_by_period, period_dates
    ){
  
  correlations <- compute_correlations_by_period(commodity_futures_data, aggregate_CHP_regimes_by_period)
  
  dplyr::mutate(correlations$whole, regime = "whole period") %>% 
    dplyr::bind_rows(correlations$regimes) %>% 
    dplyr::select(type, frequency, field, period, regime, data, correlations) %>%
    dplyr::mutate(period = factor(period, levels = unique(period_dates$period))) %>%
    dplyr::arrange(type, frequency, field, period, regime)
}

compute_pairwise_correlations_between_tickers <- function(
    tickers, commodity_futures_data, aggregate_CHP_regimes, period_dates
    ){
  
  commodity_futures_data <- dplyr::filter(
    commodity_futures_data, `active contract ticker` %in% tickers,
    date >= period_dates$date[[1L]], date <= period_dates$date[[length(period_dates$date)]]
    )
  
  aggregate_CHP_regimes_by_year <- dplyr::filter(aggregate_CHP_regimes, timespan == "years") %>%
    dplyr::select(regimes) %>% purrr::flatten_df()
  correlations_by_year <- make_correlations_by_year_dataframe(
    commodity_futures_data, aggregate_CHP_regimes_by_year
    )
  
  commodity_futures_data_with_periods <- 
    add_periods_to_commodity_futures_data(commodity_futures_data, period_dates)
  aggregate_CHP_regimes_by_period <- dplyr::filter(aggregate_CHP_regimes, timespan == "periods") %>%
    dplyr::select(regimes) %>% purrr::flatten_df()
  correlations_by_period <- make_correlations_by_period_dataframe(
    commodity_futures_data_with_periods, aggregate_CHP_regimes_by_period, period_dates
    )
  
  tibble::tibble(timespan = c("years", "periods"), results = list(correlations_by_year, correlations_by_period))
}

make_pairwise_correlations_for_ticker_combinations_dataframe <- function(
    combinations, commodity_futures_data, aggregate_CHP_regimes, period_dates
    ){
  
  p <- progressr::progressor(steps = nrow(combinations))
  
  correlations <- furrr::future_map(combinations$tickers, ~{
    p()
    compute_pairwise_correlations_between_tickers(
      .x, commodity_futures_data, aggregate_CHP_regimes, period_dates
    )
  })
  
  dplyr::mutate(combinations, correlations = correlations)
}







