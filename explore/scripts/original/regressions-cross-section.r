library(finRes); library(magrittr); library(doParallel); library(lubridate)


# start cluster ####
cluster <- makeCluster(detectCores() - 1L, outfile = "")
registerDoParallel(cluster, cores = detectCores() - 1L)


# globals ####
data("tickers_futures", "tickers_cftc", package = "BBGsymbols")
data("exchanges", package = "fewISOs")
rf <- readr::read_rds(here::here("data", "risk-free-rate.rds"))

storethat <- here::here("data", "storethat.sqlite")


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
# load data ####
start <- "1996-01-01"; end <- "2018-12-31"
# could add COA Comdty (Crude oil-brent - IFEU) ENA Comdty (Crude oil-WTI - IFEU), 
# PGA Comdty (Gasoline-RBOB - IFEU), NVA Comdty (Heating oil - IFEU), 
# FNA Comdty (Natural gas - IFEU), QSA Comdty (Gasoil-low sulfur - IFEU)
`commodity futures tickers` <- c(
  "BOA Comdty", "C A Comdty", "CCA Comdty", "CLA Comdty", "CTA Comdty", 
  "FCA Comdty", "GCA Comdty", "HGA Comdty", "HOA Comdty", "JOA Comdty", 
  "KCA Comdty", "LAA Comdty", "LBA Comdty", "LCA Comdty", "LHA Comdty", 
  "LLA Comdty", "LNA Comdty", "LPA Comdty", "LTA Comdty", "LXA Comdty", 
  "NGA Comdty", "O A Comdty", "PAA Comdty", "PLA Comdty", "S A Comdty", 
  "SBA Comdty", "SIA Comdty", "SMA Comdty", "W A Comdty", "XBWA Comdty"
)

`US commodity futures tickers` <- dplyr::left_join(
  dplyr::select(tickers_futures, ticker, MIC), 
  dplyr::select(exchanges, MIC, country), 
  by = "MIC"
) %>% 
  dplyr::filter(ticker %in% `commodity futures tickers`, country == "US") %>% 
  dplyr::select(ticker) %>% purrr::flatten_chr()

`commodity futures data` <- pullit::pull_futures_market(
  source = "storethat", type = "term structure", 
  active_contract_tickers = `US commodity futures tickers`,
  start = start, end = end, TS_positions = 1L:2L, roll_type = "A", 
  roll_days = 0L, roll_months = 0L,roll_adjustment = "N", file = storethat
)
`commodity futures data` <- pullit::get_data(`commodity futures data`) %>%
  dplyr::left_join(
    dplyr::select(
      pullit::get_term_structure_tickers(`commodity futures data`), 
      `active contract ticker`, ticker, `TS position`
    ),
    by = "ticker"
  ) %>% 
  dplyr::select(
    `active contract ticker`, ticker, `TS position`, field, date, value
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

# aggregate CHP ####
`aggregate CHP` <- dplyr::left_join(
  pullit::get_data(`commodity CFTC data`), 
  dplyr::select(
    tickers_cftc, MIC, format, underlying, unit, participant, position, ticker
  ),
  by = "ticker"
) %>% 
  dplyr::filter(
    format == "legacy", participant == "commercial", underlying == "futures only", 
    unit == "contracts", position %in% c("long", "short")
  ) %>%
  dplyr::select(`active contract ticker`, position, date, value) %>% 
  dplyr::group_by(`active contract ticker`) %>%
  tidyr::spread(position, value) %>% 
  dplyr::mutate(pressure = long / (long + short)) %>% 
  dplyr::select(`active contract ticker`, date, pressure) %>% 
  dplyr::ungroup() %>% dplyr::group_by(date) %>% 
  dplyr::summarise(`aggregate CHP` = mean(pressure, na.rm = T))

years <- dplyr::mutate(
  `aggregate CHP`, year = lubridate::year(date), week = lubridate::week(date)
) %>% dplyr::group_by(year) %>% 
  dplyr::mutate(
    regime = ifelse(
      `aggregate CHP` < median(`aggregate CHP`), 
      "backwardation", 
      "contango")
  ) %>% dplyr::ungroup() %>%
  dplyr::mutate(period = NA) %>% 
  dplyr::select(period, year, week, regime)

years <- dplyr::mutate(
  dplyr::distinct(`commodity futures data`, date), 
  year = lubridate::year(date), 
  week = lubridate::week(date)
) %>%
  dplyr::left_join(years, by = c("year", "week")) %>% 
  dplyr::select(-week) %>% dplyr::filter(! is.na(regime))

subperiods <- lapply(
  unique(periods$period), 
  function(x){
    
    start <- dplyr::filter(periods, period == x, bound == "start") %>% 
      dplyr::select(date) %>% purrr::flatten_chr()
    
    end <- dplyr::filter(periods, period == x, bound == "end") %>% 
      dplyr::select(date) %>% purrr::flatten_chr()
    
    dates <- dplyr::distinct(`commodity futures data`, date) %>% 
      dplyr::filter(date >= as.Date(start), date <= as.Date(end)) %>% 
      dplyr::mutate(year = lubridate::year(date), week = lubridate::week(date))
    
    regimes <- dplyr::filter(
      `aggregate CHP`, date >= as.Date(start), date <= as.Date(end)
    ) %>% 
      dplyr::mutate(
        regime = ifelse(
          `aggregate CHP` < median(`aggregate CHP`), "backwardation", "contango"
        ),
        period = x, year = lubridate::year(date), week = lubridate::week(date)
      ) %>% 
      dplyr::select(year, week, regime)
    
    dplyr::left_join(dates, regimes, by = c("year", "week")) %>% 
      dplyr::filter(! is.na(regime)) %>%
      dplyr::mutate(period = x, year = NA) %>% 
      dplyr::select(-week)
    
  }) %>% 
  dplyr::bind_rows() %>% 
  dplyr::select(date, period, year, regime)

`aggregate CHP regimes` <- dplyr::bind_rows(years, subperiods) %>% 
  dplyr::arrange(period, year, date)








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
weighted = TRUE

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

combinations <- dplyr::distinct(
  `factor returns`, `asset pool`, `update frequency`, `return frequency`,
  `ranking period`, `long threshold`, `short threshold`, leg
) %>% tidyr::expand_grid(
  tibble::tibble(regressors = regressors)
) %>% tidyr::expand_grid(
  tibble::tibble(period = unique(periods$period))
) %>%
  dplyr::filter(
    ! ((stringr::str_detect(regressors, "market")) & (leg %in% c("long", "short")))
  ) %>% dplyr::relocate(regressors, .before = leg)


whole <- foreach(y = 1L:nrow(combinations), .combine = rbind) %dopar% {
  # whole <- foreach(y = 1L:10L, .combine = rbind) %dopar% {
  library(magrittr); library(lubridate)
  message(paste(y, nrow(combinations), sep = "/"))
  
  combination <- dplyr::slice(combinations, y)
  
  factors <- stringr::str_split(combination$regressors, " \\+ ") %>% 
    unlist() %>% stringr::str_replace_all("`", "")
  leg <- combination$leg
  
  period <- combination$period
  start <- dplyr::filter(periods, period == !! period, bound == "start") %>%
    dplyr::select(date) %>% purrr::flatten_chr()
  end <- dplyr::filter(periods, period == !! period, bound == "end") %>%
    dplyr::select(date) %>% purrr::flatten_chr()
  
  factors_returns <- dplyr::filter(
    `factor returns`, name %in% factors, leg == !!leg, 
    date >= start, date <= end
  ) %>% dplyr::select(date, name, return) %>% 
    tidyr::pivot_wider(names_from = name, values_from = return)
  
  prices <- dplyr::filter(
    `futures prices`, 
    `active contract ticker` %in% `US commodity futures tickers`, 
    date >= start, date <= end
  )
  
  
  `return frequency` <- combination$`return frequency`
  
  commodity_returns <- if (`return frequency` == "day") {
    dplyr::group_by(prices, `active contract ticker`) %>%
      dplyr::mutate(value = (value / dplyr::lag(value, 1L)) - 1L) %>% 
      dplyr::ungroup() %>% na.omit()
  } else {
    dplyr::mutate(
      prices, year = lubridate::year(date),
      unit = do.call(
        what = eval(parse(text = paste0("lubridate::", `return frequency`))),
        args = list(date)
      )
    ) %>% dplyr::group_by(`active contract ticker`, year, unit) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
      dplyr::ungroup() %>% dplyr::select(-c(year, unit)) %>%
      dplyr::group_by(`active contract ticker`) %>%
      dplyr::mutate(value = (value / dplyr::lag(value, 1L)) - 1L) %>% 
      dplyr::ungroup() %>% na.omit()
  }
  
  commodity_returns <- tidyr::pivot_wider(
    commodity_returns, names_from = `active contract ticker`, values_from = value
  )
  
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
  
  
  factor_names <- names(factors_returns)[names(factors_returns) != "date"]
  commodity_names <- names(commodity_returns)[names(commodity_returns) != "date"]
  
  
  data <- if (`return frequency` == "day"){
    tryCatch(
      { 
        data <- dplyr::left_join(commodity_returns, factors_returns, by = "date")  %>%
          dplyr::left_join(`risk free`, by = "date")  %>%
          dplyr::arrange(date) %>% na.omit() %>% as.data.frame()
        data[, c(commodity_names, factor_names)] <-
          data[, c(commodity_names, factor_names)] - data[, "rf"]
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
        data[, c(commodity_names, factor_names)] <-
          data[, c(commodity_names, factor_names)] - data[, "rf"]
        dplyr::select(data, -rf)
      },
      error = function(e) { NA }
    )
  }
  
  
  results <- tryCatch(
    { 
      factorem::famamcbeth(
        assets_returns = data[, c("date", commodity_names)] , 
        factor_returns = data[, c("date", factors)], 
        mean = TRUE
      ) 
    }, 
    error = function(e) { NA }
  )
  
  if (! is.na(results)){ betas <- results@betas; lambdas <- results@lambdas 
  } else { betas <- NA; lambdas <- NA }
  
  
  dplyr::mutate(
    combination,
    regime = "all", betas = list(betas), lambdas = list(lambdas)
  )
}



### aggregate CHP regimes ####
combinations <- dplyr::distinct(
  `factor returns`, `asset pool`, `update frequency`, `return frequency`,
  `ranking period`, `long threshold`, `short threshold`, leg
) %>% tidyr::expand_grid(
  tibble::tibble(regressors = regressors)
) %>% tidyr::expand_grid(
  tibble::tibble(period = unique(periods$period))
) %>%
  dplyr::filter(
    ! ((stringr::str_detect(regressors, "market")) & (leg %in% c("long", "short")))
  ) %>% dplyr::relocate(regressors, .before = leg) %>% 
  tidyr::expand_grid(
    tibble::tibble(regime = c("backwardation", "contango"))
  )



regimes <- foreach(y = 1L:nrow(combinations), .combine = rbind) %dopar% {
  # regimes <- foreach(y = 1L:10L, .combine = rbind) %dopar% {
  library(magrittr); library(lubridate)
  message(paste(y, nrow(combinations), sep = "/"))
  
  combination <- dplyr::slice(combinations, y)
  
  factors <- stringr::str_split(combination$regressors, " \\+ ") %>% 
    unlist() %>% stringr::str_replace_all("`", "")
  leg <- combination$leg
  
  period <- combination$period
  start <- dplyr::filter(periods, period == !! period, bound == "start") %>%
    dplyr::select(date) %>% purrr::flatten_chr()
  end <- dplyr::filter(periods, period == !! period, bound == "end") %>%
    dplyr::select(date) %>% purrr::flatten_chr()
  
  factors_returns <- dplyr::filter(
    `factor returns`, name %in% factors, leg == !!leg, 
    date >= start, date <= end
  ) %>% dplyr::select(date, name, return) %>% 
    tidyr::pivot_wider(names_from = name, values_from = return)
  
  prices <- dplyr::filter(
    `futures prices`, 
    `active contract ticker` %in% `US commodity futures tickers`, 
    date >= start, date <= end
  )
  
  
  `return frequency` <- combination$`return frequency`
  
  commodity_returns <- if (`return frequency` == "day") {
    dplyr::group_by(prices, `active contract ticker`) %>%
      dplyr::mutate(value = (value / dplyr::lag(value, 1L)) - 1L) %>% 
      dplyr::ungroup() %>% na.omit()
  } else {
    dplyr::mutate(
      prices, year = lubridate::year(date),
      unit = do.call(
        what = eval(parse(text = paste0("lubridate::", `return frequency`))),
        args = list(date)
      )
    ) %>% dplyr::group_by(`active contract ticker`, year, unit) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
      dplyr::ungroup() %>% dplyr::select(-c(year, unit)) %>%
      dplyr::group_by(`active contract ticker`) %>%
      dplyr::mutate(value = (value / dplyr::lag(value, 1L)) - 1L) %>% 
      dplyr::ungroup() %>% na.omit()
  }
  
  commodity_returns <- tidyr::pivot_wider(
    commodity_returns, names_from = `active contract ticker`, values_from = value
  )
  
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
  
  factor_names <- names(factors_returns)[names(factors_returns) != "date"]
  commodity_names <- names(commodity_returns)[names(commodity_returns) != "date"]
  
  
  data <- if (`return frequency` == "day"){
    tryCatch(
      { 
        data <- dplyr::left_join(commodity_returns, factors_returns, by = "date")  %>%
          dplyr::left_join(`risk free`, by = "date")  %>%
          dplyr::arrange(date) %>% na.omit() %>% as.data.frame()
        data[, c(commodity_names, factor_names)] <-
          data[, c(commodity_names, factor_names)] - data[, "rf"]
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
        data[, c(commodity_names, factor_names)] <-
          data[, c(commodity_names, factor_names)] - data[, "rf"]
        dplyr::select(data, -rf)
      },
      error = function(e) { NA }
    )
  }
  
  regimes <- dplyr::filter(`aggregate CHP regimes`, period == !! period) %>%
    dplyr::select(date, regime)
  regime <- combination$regime
  data <- dplyr::left_join(data, regimes, by = "date") %>%
    dplyr::filter(regime == !! regime) %>% dplyr::select(-regime)
  
  results <- tryCatch(
    { 
      factorem::famamcbeth(
        assets_returns = data[, c("date", commodity_names)] , 
        factor_returns = data[, c("date", factors)], 
        mean = TRUE
      ) 
    }, 
    error = function(e) { NA }
  )
  
  if (! is.na(results)){ betas <- results@betas; lambdas <- results@lambdas 
  } else { betas <- NA; lambdas <- NA }
  
  dplyr::mutate(combination, betas = list(betas), lambdas = list(lambdas))
}


`US commodities ~ factors from US commodities` <- dplyr::bind_rows(whole, regimes)







# factor data - asset pool: factor picks ####
## dependent variables: factor picks ####
`US commodity futures tickers` <- dplyr::left_join(
  dplyr::select(tickers_futures, ticker, MIC), 
  dplyr::select(exchanges, MIC, country), by = "MIC"
) %>% 
  dplyr::filter(ticker %in% `commodity futures tickers`, country == "US") %>% 
  dplyr::select(ticker) %>% purrr::flatten_chr()

`commodity futures data` <- pullit::pull_futures_market(
  source = "storethat", type = "term structure", 
  active_contract_tickers = `US commodity futures tickers`, 
  start = start, end = end, 
  TS_positions = 1L:2L, 
  roll_type = "A", roll_days = 0L, roll_months = 0L, roll_adjustment = "N", 
  file = storethat
)

`commodity futures prices` <- dplyr::left_join(
  `commodity futures data`@data, 
  dplyr::select(
    `commodity futures data`@term_structure_tickers, 
    `active contract ticker`, ticker, `TS position`), 
  by = "ticker") %>%
  dplyr::filter(`TS position` == 1L, field == "PX_LAST") %>% 
  dplyr::select(-c(ticker, field, `TS position`)) 

`factor picks` <- readr::read_rds(
  here::here("explore", "results", "factor-picks.rds")
)
`factors from picks` <- readr::read_rds(
  here::here("explore", "results", "factors-from-picks.rds")
) %>%
  dplyr::mutate(
    `factor name` = stringr::str_replace_all(`factor name`, "open interest", "OI")
  )


### no CHP regime ####
factor_names <- ifelse(
  stringr::str_detect(
    unique(`factors from picks`$`factor name`), pattern = " ", negate = FALSE
  ),
  paste0("`", unique(`factors from picks`$`factor name`), "`"), 
  unique(`factors from picks`$`factor name`)
)

regressors <- lapply(
  seq_along(factor_names),
  function(x){
    combn(factor_names, x) %>% apply(MARGIN = 2L, paste, collapse = " + ")
  }) %>% do.call(c, args = .) %>%
  stringr::str_replace_all("open interest", "OI")


combinations <- dplyr::distinct(
  `factor picks`,
  `asset pool`, leg, factor, period
) %>%
  dplyr::rename(
    `picking factor asset pool` = `asset pool`, 
    `picking factor leg` = leg,
    `picking factor name` = factor
  ) %>% 
  dplyr::left_join(
    dplyr::distinct(
      `factors from picks`, 
      `picking factor asset pool`, `picking factor name`, `picking factor leg`,
      `update frequency`, `return frequency`, `ranking period`, `long threshold`,
      `short threshold`, period
    ), 
    by = c(
      "picking factor asset pool", "picking factor name", "picking factor leg", 
      "period"
    )
  ) %>% 
  tidyr::expand_grid(tibble::tibble(regressors = regressors)) %>%
  tidyr::expand_grid(tibble::tibble(`factor leg` = c("long", "short", "factor"))) %>%
  dplyr::filter(
    ! ((stringr::str_detect(regressors, "market")) & 
         (`factor leg` %in% c("long", "short")))
  )


whole <- foreach(y = 1L:nrow(combinations), .combine = rbind) %dopar% {
  # whole <- foreach(y = 1L:10L, .combine = rbind) %dopar% {
  library(magrittr); library(lubridate)
  message(paste(y, nrow(combinations), sep = "/"))
  
  combination <- dplyr::slice(combinations, y)
  
  factors <- stringr::str_split(combination$regressors, " \\+ ") %>% 
    unlist() %>% stringr::str_replace_all("`", "")
  
  picks <- dplyr::left_join(
    combination, `factor picks`,
    by = c(
      "picking factor asset pool" = "asset pool", "picking factor name" = "factor",
      "picking factor leg" = "leg", "period"
    )
  ) %>% dplyr::select(pick) %>% purrr::flatten_chr()
  
  factors_returns <- dplyr::left_join(
    combination,
    dplyr::select(
      `factors from picks`,
      `picking factor asset pool`, `picking factor name`, `picking factor leg`,
      period, `factor name`, `factor data`
    ),
    by = c(
      "picking factor asset pool", "picking factor name", "picking factor leg",
      "period"
    )
  ) %>% dplyr::mutate(
    returns = purrr::pmap(
      list(`factor leg`, `factor name`, `factor data`), function(leg, name, data){
        if (! (name == "market" & leg %in% c("long", "short")))
          dplyr::select(data@returns, date, leg) %>% 
          rlang::set_names(c("date", "return"))
      })
  ) %>% dplyr::select(factor = `factor name`, returns) %>% tidyr::unnest(returns) %>%
    tidyr::pivot_wider(names_from = factor, values_from = return)
  
  period <- combination$period
  start <- dplyr::filter(periods, period == !! period, bound == "start") %>%
    dplyr::select(date) %>% purrr::flatten_chr()
  end <- dplyr::filter(periods, period == !! period, bound == "end") %>%
    dplyr::select(date) %>% purrr::flatten_chr()
  
  prices <- dplyr::filter(
    `commodity futures prices`,
    `active contract ticker` %in% picks, date >= start, date <= end
  )
  
  
  `return frequency` <- combination$`return frequency`
  
  commodity_returns <- if (`return frequency` == "day") {
    dplyr::group_by(prices, `active contract ticker`) %>%
      dplyr::mutate(value = (value / dplyr::lag(value, 1L)) - 1L) %>% 
      dplyr::ungroup() %>% na.omit()
  } else {
    dplyr::mutate(
      prices, year = lubridate::year(date),
      unit = do.call(
        what = eval(parse(text = paste0("lubridate::", `return frequency`))),
        args = list(date)
      )
    ) %>% dplyr::group_by(`active contract ticker`, year, unit) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
      dplyr::ungroup() %>% dplyr::select(-c(year, unit)) %>%
      dplyr::group_by(`active contract ticker`) %>%
      dplyr::mutate(value = (value / dplyr::lag(value, 1L)) - 1L) %>% 
      dplyr::ungroup() %>% na.omit()
  }
  
  commodity_returns <- tidyr::pivot_wider(
    commodity_returns, names_from = `active contract ticker`, values_from = value
  )
  
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
  
  factor_names <- names(factors_returns)[names(factors_returns) != "date"]
  commodity_names <- names(commodity_returns)[names(commodity_returns) != "date"]
  
  
  data <- if (`return frequency` == "day"){
    tryCatch(
      { 
        data <- dplyr::left_join(commodity_returns, factors_returns, by = "date")  %>%
          dplyr::left_join(`risk free`, by = "date")  %>%
          dplyr::arrange(date) %>% na.omit() %>% as.data.frame()
        data[, c(commodity_names, factor_names)] <-
          data[, c(commodity_names, factor_names)] - data[, "rf"]
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
        data[, c(commodity_names, factor_names)] <-
          data[, c(commodity_names, factor_names)] - data[, "rf"]
        dplyr::select(data, -rf)
      },
      error = function(e) { NA }
    )
  }
  
  results <- tryCatch(
    { 
      factorem::famamcbeth(
        assets_returns = data[, c("date", commodity_names)] , 
        factor_returns = data[, c("date", factors)], 
        mean = TRUE
      ) 
    }, 
    error = function(e) { NA }
  )
  
  if (! is.na(results)){ betas <- results@betas; lambdas <- results@lambdas 
  } else { betas <- NA; lambdas <- NA }
  
  dplyr::mutate(
    combination, regime = "all", betas = list(betas), lambdas = list(lambdas)
  )
}


### backwardation vs. contango ####
combinations <- dplyr::distinct(
  `factor picks`,
  `asset pool`, leg, factor, period
) %>%
  dplyr::rename(
    `picking factor asset pool` = `asset pool`, 
    `picking factor leg` = leg,
    `picking factor name` = factor
  ) %>% 
  dplyr::left_join(
    dplyr::distinct(
      `factors from picks`, 
      `picking factor asset pool`, `picking factor name`, `picking factor leg`,
      `update frequency`, `return frequency`, `ranking period`, `long threshold`,
      `short threshold`, period
    ), 
    by = c(
      "picking factor asset pool", "picking factor name", "picking factor leg", 
      "period"
    )
  ) %>% 
  tidyr::expand_grid(tibble::tibble(regressors = regressors)) %>%
  tidyr::expand_grid(tibble::tibble(`factor leg` = c("long", "short", "factor"))) %>%
  dplyr::filter(
    ! ((stringr::str_detect(regressors, "market")) & 
         (`factor leg` %in% c("long", "short")))
  ) %>% 
  tidyr::expand_grid(
    tibble::tibble(regime = c("backwardation", "contango"))
  )


regimes <- foreach(y = 1L:nrow(combinations), .combine = rbind) %dopar% {
  # regimes <- foreach(y = 1L:10L, .combine = rbind) %dopar% {
  library(magrittr); library(lubridate)
  
  combination <- dplyr::slice(combinations, y)
  
  factors <- stringr::str_split(combination$regressors, " \\+ ") %>% 
    unlist() %>% stringr::str_replace_all("`", "")
  
  picks <- dplyr::left_join(
    combination, `factor picks`,
    by = c(
      "picking factor asset pool" = "asset pool", "picking factor name" = "factor",
      "picking factor leg" = "leg", "period"
    )
  ) %>% dplyr::select(pick) %>% purrr::flatten_chr()
  
  factors_returns <- dplyr::left_join(
    combination,
    dplyr::select(
      `factors from picks`,
      `picking factor asset pool`, `picking factor name`, `picking factor leg`,
      period, `factor name`, `factor data`
    ),
    by = c(
      "picking factor asset pool", "picking factor name", "picking factor leg",
      "period"
    )
  ) %>% dplyr::mutate(
    returns = purrr::pmap(
      list(`factor leg`, `factor name`, `factor data`), function(leg, name, data){
        if (! (name == "market" & leg %in% c("long", "short")))
          dplyr::select(data@returns, date, leg) %>% 
          rlang::set_names(c("date", "return"))
      })
  ) %>% dplyr::select(factor = `factor name`, returns) %>% tidyr::unnest(returns) %>%
    tidyr::pivot_wider(names_from = factor, values_from = return)
  
  period <- combination$period
  start <- dplyr::filter(periods, period == !! period, bound == "start") %>%
    dplyr::select(date) %>% purrr::flatten_chr()
  end <- dplyr::filter(periods, period == !! period, bound == "end") %>%
    dplyr::select(date) %>% purrr::flatten_chr()
  
  prices <- dplyr::filter(
    `commodity futures prices`,
    `active contract ticker` %in% picks, date >= start, date <= end
  )
  
  
  `return frequency` <- combination$`return frequency`
  
  commodity_returns <- if (`return frequency` == "day") {
    dplyr::group_by(prices, `active contract ticker`) %>%
      dplyr::mutate(value = (value / dplyr::lag(value, 1L)) - 1L) %>% 
      dplyr::ungroup() %>% na.omit()
  } else {
    dplyr::mutate(
      prices, year = lubridate::year(date),
      unit = do.call(
        what = eval(parse(text = paste0("lubridate::", `return frequency`))),
        args = list(date)
      )
    ) %>% dplyr::group_by(`active contract ticker`, year, unit) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
      dplyr::ungroup() %>% dplyr::select(-c(year, unit)) %>%
      dplyr::group_by(`active contract ticker`) %>%
      dplyr::mutate(value = (value / dplyr::lag(value, 1L)) - 1L) %>% 
      dplyr::ungroup() %>% na.omit()
  }
  
  commodity_returns <- tidyr::pivot_wider(
    commodity_returns, names_from = `active contract ticker`, values_from = value
  )
  
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
  
  factor_names <- names(factors_returns)[names(factors_returns) != "date"]
  commodity_names <- names(commodity_returns)[names(commodity_returns) != "date"]
  
  
  data <- if (`return frequency` == "day"){
    tryCatch(
      { 
        data <- dplyr::left_join(commodity_returns, factors_returns, by = "date")  %>%
          dplyr::left_join(`risk free`, by = "date")  %>%
          dplyr::arrange(date) %>% na.omit() %>% as.data.frame()
        data[, c(commodity_names, factor_names)] <-
          data[, c(commodity_names, factor_names)] - data[, "rf"]
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
        data[, c(commodity_names, factor_names)] <-
          data[, c(commodity_names, factor_names)] - data[, "rf"]
        dplyr::select(data, -rf)
      },
      error = function(e) { NA }
    )
  }
  
  regimes <- dplyr::filter(`aggregate CHP regimes`, period == !! period) %>%
    dplyr::select(date, regime)
  regime <- combination$regime
  data <- dplyr::left_join(data, regimes, by = "date") %>%
    dplyr::filter(regime == !! regime) %>% dplyr::select(-regime)
  
  results <- tryCatch(
    { 
      factorem::famamcbeth(
        assets_returns = data[, c("date", commodity_names)] , 
        factor_returns = data[, c("date", factors)], 
        mean = TRUE
      ) 
    }, 
    error = function(e) { NA }
  )
  
  if (! is.na(results)){ betas <- results@betas; lambdas <- results@lambdas 
  } else { betas <- NA; lambdas <- NA }
  
  dplyr::mutate(
    combination, betas = list(betas), lambdas = list(lambdas)
  )
}


`factor picks ~ factors from picks` <- dplyr::bind_rows(whole, regimes)





## dependent variables: US commodities ####

### no CHP regime ####
combinations <- dplyr::distinct(
  `factors from picks`, 
  `picking factor asset pool`, `picking factor name`, `picking factor leg`,
  `update frequency`, `return frequency`, `ranking period`, `long threshold`,
  `short threshold`, period
) %>%
  tidyr::expand_grid(tibble::tibble(regressors = regressors)) %>%
  tidyr::expand_grid(tibble::tibble(`factor leg` = c("long", "short", "factor"))) %>%
  dplyr::filter(
    ! ((stringr::str_detect(regressors, "market")) & 
         (`factor leg` %in% c("long", "short")))
  )


whole <- foreach(y = 1L:nrow(combinations), .combine = rbind) %dopar% {
  # whole <- foreach(y = 1L:10L, .combine = rbind) %dopar% {
  library(magrittr); library(lubridate)
  
  message(paste(y, nrow(combinations), sep = "/"))
  
  combination <- dplyr::slice(combinations, y)
  
  factors <- stringr::str_split(combination$regressors, " \\+ ") %>% 
    unlist() %>% stringr::str_replace_all("`", "")
  
  factors_returns <- dplyr::left_join(
    combination,
    dplyr::select(
      `factors from picks`,
      `picking factor asset pool`, `picking factor name`, `picking factor leg`,
      period, `factor name`, `factor data`
    ),
    by = c(
      "picking factor asset pool", "picking factor name", "picking factor leg",
      "period"
    )
  ) %>% dplyr::mutate(
    returns = purrr::pmap(
      list(`factor leg`, `factor name`, `factor data`), function(leg, name, data){
        if (! (name == "market" & leg %in% c("long", "short")))
          dplyr::select(data@returns, date, leg) %>% 
          rlang::set_names(c("date", "return"))
      })
  ) %>% dplyr::select(factor = `factor name`, returns) %>% tidyr::unnest(returns) %>%
    tidyr::pivot_wider(names_from = factor, values_from = return)
  
  period <- combination$period
  start <- dplyr::filter(periods, period == !! period, bound == "start") %>%
    dplyr::select(date) %>% purrr::flatten_chr()
  end <- dplyr::filter(periods, period == !! period, bound == "end") %>%
    dplyr::select(date) %>% purrr::flatten_chr()
  
  prices <- dplyr::filter(
    `commodity futures prices`,
    `active contract ticker` %in% `US commodity futures tickers`, 
    date >= start, date <= end
  )
  
  
  `return frequency` <- combination$`return frequency`
  
  commodity_returns <- if (`return frequency` == "day") {
    dplyr::group_by(prices, `active contract ticker`) %>%
      dplyr::mutate(value = (value / dplyr::lag(value, 1L)) - 1L) %>% 
      dplyr::ungroup() %>% na.omit()
  } else {
    dplyr::mutate(
      prices, year = lubridate::year(date),
      unit = do.call(
        what = eval(parse(text = paste0("lubridate::", `return frequency`))),
        args = list(date)
      )
    ) %>% dplyr::group_by(`active contract ticker`, year, unit) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
      dplyr::ungroup() %>% dplyr::select(-c(year, unit)) %>%
      dplyr::group_by(`active contract ticker`) %>%
      dplyr::mutate(value = (value / dplyr::lag(value, 1L)) - 1L) %>% 
      dplyr::ungroup() %>% na.omit()
  }
  
  commodity_returns <- tidyr::pivot_wider(
    commodity_returns, names_from = `active contract ticker`, values_from = value
  )
  
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
  
  factor_names <- names(factors_returns)[names(factors_returns) != "date"]
  commodity_names <- names(commodity_returns)[names(commodity_returns) != "date"]
  
  
  data <- if (`return frequency` == "day"){
    tryCatch(
      { 
        data <- dplyr::left_join(commodity_returns, factors_returns, by = "date")  %>%
          dplyr::left_join(`risk free`, by = "date")  %>%
          dplyr::arrange(date) %>% na.omit() %>% as.data.frame()
        data[, c(commodity_names, factor_names)] <-
          data[, c(commodity_names, factor_names)] - data[, "rf"]
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
        data[, c(commodity_names, factor_names)] <-
          data[, c(commodity_names, factor_names)] - data[, "rf"]
        dplyr::select(data, -rf)
      },
      error = function(e) { NA }
    )
  }
  
  results <- tryCatch(
    { 
      factorem::famamcbeth(
        assets_returns = data[, c("date", commodity_names)] , 
        factor_returns = data[, c("date", factors)], 
        mean = TRUE
      ) 
    }, 
    error = function(e) { NA }
  )
  
  if (! is.na(results)){ betas <- results@betas; lambdas <- results@lambdas 
  } else { betas <- NA; lambdas <- NA }
  
  dplyr::mutate(
    combination, regime = "all", betas = list(betas), lambdas = list(lambdas)
  )
}


### backwardation vs. contango ####
combinations <- dplyr::distinct(
  `factors from picks`, 
  `picking factor asset pool`, `picking factor name`, `picking factor leg`,
  `update frequency`, `return frequency`, `ranking period`, `long threshold`,
  `short threshold`, period
) %>%
  tidyr::expand_grid(tibble::tibble(regressors = regressors)) %>%
  tidyr::expand_grid(tibble::tibble(`factor leg` = c("long", "short", "factor"))) %>%
  dplyr::filter(
    ! ((stringr::str_detect(regressors, "market")) & 
         (`factor leg` %in% c("long", "short")))
  ) %>% 
  tidyr::expand_grid(tibble::tibble(regime = c("backwardation", "contango")))


regimes <- foreach(y = 1L:nrow(combinations), .combine = rbind) %dopar% {
  # regimes <- foreach(y = 1L:10L, .combine = rbind) %dopar% {
  library(magrittr); library(lubridate)
  
  message(paste(y, nrow(combinations), sep = "/"))
  
  combination <- dplyr::slice(combinations, y)
  
  factors <- stringr::str_split(combination$regressors, " \\+ ") %>% 
    unlist() %>% stringr::str_replace_all("`", "")
  
  factors_returns <- dplyr::left_join(
    combination,
    dplyr::select(
      `factors from picks`,
      `picking factor asset pool`, `picking factor name`, `picking factor leg`,
      period, `factor name`, `factor data`
    ),
    by = c(
      "picking factor asset pool", "picking factor name", "picking factor leg",
      "period"
    )
  ) %>% dplyr::mutate(
    returns = purrr::pmap(
      list(`factor leg`, `factor name`, `factor data`), function(leg, name, data){
        if (! (name == "market" & leg %in% c("long", "short")))
          dplyr::select(data@returns, date, leg) %>% 
          rlang::set_names(c("date", "return"))
      })
  ) %>% dplyr::select(factor = `factor name`, returns) %>% tidyr::unnest(returns) %>%
    tidyr::pivot_wider(names_from = factor, values_from = return)
  
  period <- combination$period
  start <- dplyr::filter(periods, period == !! period, bound == "start") %>%
    dplyr::select(date) %>% purrr::flatten_chr()
  end <- dplyr::filter(periods, period == !! period, bound == "end") %>%
    dplyr::select(date) %>% purrr::flatten_chr()
  
  prices <- dplyr::filter(
    `commodity futures prices`,
    `active contract ticker` %in% `US commodity futures tickers`, 
    date >= start, date <= end
  )
  
  
  `return frequency` <- combination$`return frequency`
  
  commodity_returns <- if (`return frequency` == "day") {
    dplyr::group_by(prices, `active contract ticker`) %>%
      dplyr::mutate(value = (value / dplyr::lag(value, 1L)) - 1L) %>% 
      dplyr::ungroup() %>% na.omit()
  } else {
    dplyr::mutate(
      prices, year = lubridate::year(date),
      unit = do.call(
        what = eval(parse(text = paste0("lubridate::", `return frequency`))),
        args = list(date)
      )
    ) %>% dplyr::group_by(`active contract ticker`, year, unit) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
      dplyr::ungroup() %>% dplyr::select(-c(year, unit)) %>%
      dplyr::group_by(`active contract ticker`) %>%
      dplyr::mutate(value = (value / dplyr::lag(value, 1L)) - 1L) %>% 
      dplyr::ungroup() %>% na.omit()
  }
  
  commodity_returns <- tidyr::pivot_wider(
    commodity_returns, names_from = `active contract ticker`, values_from = value
  )
  
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
  
  factor_names <- names(factors_returns)[names(factors_returns) != "date"]
  commodity_names <- names(commodity_returns)[names(commodity_returns) != "date"]
  
  
  data <- if (`return frequency` == "day"){
    tryCatch(
      { 
        data <- dplyr::left_join(commodity_returns, factors_returns, by = "date")  %>%
          dplyr::left_join(`risk free`, by = "date")  %>%
          dplyr::arrange(date) %>% na.omit() %>% as.data.frame()
        data[, c(commodity_names, factor_names)] <-
          data[, c(commodity_names, factor_names)] - data[, "rf"]
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
        data[, c(commodity_names, factor_names)] <-
          data[, c(commodity_names, factor_names)] - data[, "rf"]
        dplyr::select(data, -rf)
      },
      error = function(e) { NA }
    )
  }
  
  regimes <- dplyr::filter(`aggregate CHP regimes`, period == !! period) %>%
    dplyr::select(date, regime)
  regime <- combination$regime
  data <- dplyr::left_join(data, regimes, by = "date") %>%
    dplyr::filter(regime == !! regime) %>% dplyr::select(-regime)
  
  results <- tryCatch(
    { 
      factorem::famamcbeth(
        assets_returns = data[, c("date", commodity_names)] , 
        factor_returns = data[, c("date", factors)], 
        mean = TRUE
      ) 
    }, 
    error = function(e) { NA }
  )
  
  if (! is.na(results)){ betas <- results@betas; lambdas <- results@lambdas 
  } else { betas <- NA; lambdas <- NA }
  
  dplyr::mutate(
    combination, betas = list(betas), lambdas = list(lambdas)
  )
}


`US commodities ~ factors from picks` <- dplyr::bind_rows(whole, regimes)




















## dependent variables: UK metals ####

### regressor: factor ####
`UK metal futures tickers` <- c(
  "LAA Comdty", "LPA Comdty", "LLA Comdty", "LNA Comdty", "LTA Comdty",
  "LXA Comdty"
)


`commodity futures data` <- pullit::pull_futures_market(
  source = "storethat", type = "term structure", 
  active_contract_tickers = `UK metal futures tickers`, 
  start = start, end = end, 
  TS_positions = 1L:2L, 
  roll_type = "A", roll_days = 0L, roll_months = 0L, roll_adjustment = "N", 
  file = storethat
)

`commodity futures prices` <- dplyr::left_join(
  `commodity futures data`@data, 
  dplyr::select(
    `commodity futures data`@term_structure_tickers, 
    `active contract ticker`, ticker, `TS position`
  ), 
  by = "ticker"
) %>%
  dplyr::filter(`TS position` == 1L, field == "PX_LAST") %>% 
  dplyr::select(-c(ticker, field, `TS position`)) 


#### no CHP regime ####
combinations <- dplyr::distinct(
  `factors from picks`, 
  `picking factor asset pool`, `picking factor name`, `picking factor leg`,
  `update frequency`, `return frequency`, `ranking period`, `long threshold`,
  `short threshold`, period
) %>%
  tidyr::expand_grid(tibble::tibble(regressors = regressors)) %>%
  tidyr::expand_grid(tibble::tibble(`factor leg` = c("long", "short", "factor"))) %>%
  dplyr::filter(
    ! ((stringr::str_detect(regressors, "market")) & 
         (`factor leg` %in% c("long", "short")))
  )


whole <- foreach(y = 1L:nrow(combinations), .combine = rbind) %dopar% {
  # whole <- foreach(y = 1L:10L, .combine = rbind) %dopar% {
  library(magrittr); library(lubridate)
  
  message(paste(y, nrow(combinations), sep = "/"))
  
  combination <- dplyr::slice(combinations, y)
  
  factors <- stringr::str_split(combination$regressors, " \\+ ") %>% 
    unlist() %>% stringr::str_replace_all("`", "")
  
  factors_returns <- dplyr::left_join(
    combination,
    dplyr::select(
      `factors from picks`,
      `picking factor asset pool`, `picking factor name`, `picking factor leg`,
      period, `factor name`, `factor data`
    ),
    by = c(
      "picking factor asset pool", "picking factor name", "picking factor leg",
      "period"
    )
  ) %>% dplyr::mutate(
    returns = purrr::pmap(
      list(`factor leg`, `factor name`, `factor data`), function(leg, name, data){
        if (! (name == "market" & leg %in% c("long", "short")))
          dplyr::select(data@returns, date, leg) %>% 
          rlang::set_names(c("date", "return"))
      })
  ) %>% dplyr::select(factor = `factor name`, returns) %>% tidyr::unnest(returns) %>%
    tidyr::pivot_wider(names_from = factor, values_from = return)
  
  period <- combination$period
  start <- dplyr::filter(periods, period == !! period, bound == "start") %>%
    dplyr::select(date) %>% purrr::flatten_chr()
  end <- dplyr::filter(periods, period == !! period, bound == "end") %>%
    dplyr::select(date) %>% purrr::flatten_chr()
  
  prices <- dplyr::filter(
    `commodity futures prices`,
    `active contract ticker` %in% `UK metal futures tickers`, 
    date >= start, date <= end
  )
  
  
  `return frequency` <- combination$`return frequency`
  
  commodity_returns <- if (`return frequency` == "day") {
    dplyr::group_by(prices, `active contract ticker`) %>%
      dplyr::mutate(value = (value / dplyr::lag(value, 1L)) - 1L) %>% 
      dplyr::ungroup() %>% na.omit()
  } else {
    dplyr::mutate(
      prices, year = lubridate::year(date),
      unit = do.call(
        what = eval(parse(text = paste0("lubridate::", `return frequency`))),
        args = list(date)
      )
    ) %>% dplyr::group_by(`active contract ticker`, year, unit) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
      dplyr::ungroup() %>% dplyr::select(-c(year, unit)) %>%
      dplyr::group_by(`active contract ticker`) %>%
      dplyr::mutate(value = (value / dplyr::lag(value, 1L)) - 1L) %>% 
      dplyr::ungroup() %>% na.omit()
  }
  
  commodity_returns <- tidyr::pivot_wider(
    commodity_returns, names_from = `active contract ticker`, values_from = value
  )
  
  # browser()
  `risk free` <- dplyr::filter(
    rf, frequency == `return frequency`, region == "Europe"
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
  
  factor_names <- names(factors_returns)[names(factors_returns) != "date"]
  commodity_names <- names(commodity_returns)[names(commodity_returns) != "date"]
  
  
  data <- if (`return frequency` == "day"){
    tryCatch(
      { 
        data <- dplyr::left_join(commodity_returns, factors_returns, by = "date")  %>%
          dplyr::left_join(`risk free`, by = "date")  %>%
          dplyr::arrange(date) %>% na.omit() %>% as.data.frame()
        data[, c(commodity_names, factor_names)] <-
          data[, c(commodity_names, factor_names)] - data[, "rf"]
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
        data[, c(commodity_names, factor_names)] <-
          data[, c(commodity_names, factor_names)] - data[, "rf"]
        dplyr::select(data, -rf)
      },
      error = function(e) { NA }
    )
  }
  
  results <- tryCatch(
    { 
      factorem::famamcbeth(
        assets_returns = data[, c("date", commodity_names)] , 
        factor_returns = data[, c("date", factors)], 
        mean = TRUE
      ) 
    }, 
    error = function(e) { NA }
  )
  
  if (! is.na(results)){ betas <- results@betas; lambdas <- results@lambdas 
  } else { betas <- NA; lambdas <- NA }
  
  dplyr::mutate(
    combination, regime = "all", betas = list(betas), lambdas = list(lambdas)
  )
}


### backwardation vs. contango ####
combinations <- dplyr::distinct(
  `factors from picks`, 
  `picking factor asset pool`, `picking factor name`, `picking factor leg`,
  `update frequency`, `return frequency`, `ranking period`, `long threshold`,
  `short threshold`, period
) %>%
  tidyr::expand_grid(tibble::tibble(regressors = regressors)) %>%
  tidyr::expand_grid(tibble::tibble(`factor leg` = c("long", "short", "factor"))) %>%
  dplyr::filter(
    ! ((stringr::str_detect(regressors, "market")) & 
         (`factor leg` %in% c("long", "short")))
  ) %>% 
  tidyr::expand_grid(tibble::tibble(regime = c("backwardation", "contango")))


regimes <- foreach(y = 1L:nrow(combinations), .combine = rbind) %dopar% {
  # regimes <- foreach(y = 1L:10L, .combine = rbind) %dopar% {
  library(magrittr); library(lubridate)
  
  message(paste(y, nrow(combinations), sep = "/"))
  
  combination <- dplyr::slice(combinations, y)
  
  factors <- stringr::str_split(combination$regressors, " \\+ ") %>% 
    unlist() %>% stringr::str_replace_all("`", "")
  
  factors_returns <- dplyr::left_join(
    combination,
    dplyr::select(
      `factors from picks`,
      `picking factor asset pool`, `picking factor name`, `picking factor leg`,
      period, `factor name`, `factor data`
    ),
    by = c(
      "picking factor asset pool", "picking factor name", "picking factor leg",
      "period"
    )
  ) %>% dplyr::mutate(
    returns = purrr::pmap(
      list(`factor leg`, `factor name`, `factor data`), function(leg, name, data){
        if (! (name == "market" & leg %in% c("long", "short")))
          dplyr::select(data@returns, date, leg) %>% 
          rlang::set_names(c("date", "return"))
      })
  ) %>% dplyr::select(factor = `factor name`, returns) %>% tidyr::unnest(returns) %>%
    tidyr::pivot_wider(names_from = factor, values_from = return)
  
  period <- combination$period
  start <- dplyr::filter(periods, period == !! period, bound == "start") %>%
    dplyr::select(date) %>% purrr::flatten_chr()
  end <- dplyr::filter(periods, period == !! period, bound == "end") %>%
    dplyr::select(date) %>% purrr::flatten_chr()
  
  prices <- dplyr::filter(
    `commodity futures prices`,
    `active contract ticker` %in% `UK metal futures tickers`, 
    date >= start, date <= end
  )
  
  
  `return frequency` <- combination$`return frequency`
  
  commodity_returns <- if (`return frequency` == "day") {
    dplyr::group_by(prices, `active contract ticker`) %>%
      dplyr::mutate(value = (value / dplyr::lag(value, 1L)) - 1L) %>% 
      dplyr::ungroup() %>% na.omit()
  } else {
    dplyr::mutate(
      prices, year = lubridate::year(date),
      unit = do.call(
        what = eval(parse(text = paste0("lubridate::", `return frequency`))),
        args = list(date)
      )
    ) %>% dplyr::group_by(`active contract ticker`, year, unit) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
      dplyr::ungroup() %>% dplyr::select(-c(year, unit)) %>%
      dplyr::group_by(`active contract ticker`) %>%
      dplyr::mutate(value = (value / dplyr::lag(value, 1L)) - 1L) %>% 
      dplyr::ungroup() %>% na.omit()
  }
  
  commodity_returns <- tidyr::pivot_wider(
    commodity_returns, names_from = `active contract ticker`, values_from = value
  )
  
  # browser()
  `risk free` <- dplyr::filter(
    rf, frequency == `return frequency`, region == "Europe"
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
  
  factor_names <- names(factors_returns)[names(factors_returns) != "date"]
  commodity_names <- names(commodity_returns)[names(commodity_returns) != "date"]
  
  
  data <- if (`return frequency` == "day"){
    tryCatch(
      { 
        data <- dplyr::left_join(commodity_returns, factors_returns, by = "date")  %>%
          dplyr::left_join(`risk free`, by = "date")  %>%
          dplyr::arrange(date) %>% na.omit() %>% as.data.frame()
        data[, c(commodity_names, factor_names)] <-
          data[, c(commodity_names, factor_names)] - data[, "rf"]
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
        data[, c(commodity_names, factor_names)] <-
          data[, c(commodity_names, factor_names)] - data[, "rf"]
        dplyr::select(data, -rf)
      },
      error = function(e) { NA }
    )
  }
  
  regimes <- dplyr::filter(`aggregate CHP regimes`, period == !! period) %>%
    dplyr::select(date, regime)
  regime <- combination$regime
  data <- dplyr::left_join(data, regimes, by = "date") %>%
    dplyr::filter(regime == !! regime) %>% dplyr::select(-regime)
  
  results <- tryCatch(
    { 
      factorem::famamcbeth(
        assets_returns = data[, c("date", commodity_names)] , 
        factor_returns = data[, c("date", factors)], 
        mean = TRUE
      ) 
    }, 
    error = function(e) { NA }
  )
  
  if (! is.na(results)){ betas <- results@betas; lambdas <- results@lambdas 
  } else { betas <- NA; lambdas <- NA }
  
  dplyr::mutate(
    combination, betas = list(betas), lambdas = list(lambdas)
  )
}

`UK metals ~ factors from picks` <- dplyr::bind_rows(whole, regimes)




regressions <- tibble::tibble(
  analysis = c(
    "US commodities ~ factors from US commodities", "factor picks ~ factors from picks",
    "US commodities ~ factors from picks", "UK metals ~ factors from picks"
  ),
  results = list(
    `US commodities ~ factors from US commodities`,
    `factor picks ~ factors from picks`, `US commodities ~ factors from picks`,
    `UK metals ~ factors from picks`
  )
)
readr::write_rds(
  regressions, 
  path = here::here("explore", "results", "regressions-cross-section.rds")
)

parallel::stopCluster(cluster)
