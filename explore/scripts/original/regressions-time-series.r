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


`US commodities ~ factors from US commodities` <- dplyr::bind_rows(whole, regimes)


# regressions <- tibble::tibble(
#   analysis = c("US commodities ~ factors from US commodities"),
#   results = list(`US commodities ~ factors from US commodities`)
# )
# readr::write_rds(
#   regressions, 
#   path = here::here("explore", "results", "regressions-time-series-new.rds")
# )


















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
  `asset pool`, leg, factor, period, pick
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
  library(magrittr); library(lubridate)
  message(paste(y, nrow(combinations), sep = "/"))
  
  combination <- dplyr::slice(combinations, y)
  
  
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
  
  ticker <- combination$pick
  
  prices <- dplyr::filter(
    `commodity futures prices`,
    `active contract ticker` == ticker, date >= start, date <= end
  )
  
  
  `return frequency` <- combination$`return frequency`
  
  commodity_returns <- if (`return frequency` == "day") {
    dplyr::mutate(
      prices,
      commodity = (value / dplyr::lag(value, 1L)) - 1L
    ) %>% dplyr::select(date, commodity)
  } else {
    dplyr::mutate(
      prices, year = lubridate::year(date),
      unit = do.call(
        what = eval(parse(text = paste0("lubridate::", `return frequency`))),
        args = list(date)
      )
    ) %>% dplyr::group_by(year, unit) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c(year, unit)) %>%
      dplyr::mutate(
        commodity = (value / dplyr::lag(value, 1L)) - 1L
      ) %>% dplyr::select(date, commodity)
  }
  
  
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
  
  
  data <- if (`return frequency` == "day"){
    tryCatch(
      { 
        data <- dplyr::left_join(commodity_returns, factors_returns, by = "date")  %>%
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
  
  
  model <- combination$regressors
  
  formula <- paste("commodity", model, sep = " ~ ")
  
  result  <- tryCatch(
    { lm(rlang::parse_expr(formula), data = data) },
    error = function(e) { NA }
  )
  
  if (! is.na(result)){
    coefficients <- broom::tidy(result)
    rsquared <- stats::summary.lm(result)$r.squared
  } else { coefficients <- NA; rsquared <- NA }
  
  dplyr::mutate(
    combination,
    regime = "all", coefficients = list(coefficients), rsquared = rsquared
  )
}



### backwardation vs. contango ####
combinations <- dplyr::distinct(
  `factor picks`,
  `asset pool`, leg, factor, period, pick
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
  library(magrittr); library(lubridate)
  
  message(paste(y, nrow(combinations), sep = "/"))
  
  combination <- dplyr::slice(combinations, y)
  
  
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
  
  ticker <- combination$pick
  
  prices <- dplyr::filter(
    `commodity futures prices`,
    `active contract ticker` == ticker, date >= start, date <= end
  )
  
  
  `return frequency` <- combination$`return frequency`
  
  commodity_returns <- if (`return frequency` == "day") {
    dplyr::mutate(
      prices,
      commodity = (value / dplyr::lag(value, 1L)) - 1L
    ) %>% dplyr::select(date, commodity)
  } else {
    dplyr::mutate(
      prices, year = lubridate::year(date),
      unit = do.call(
        what = eval(parse(text = paste0("lubridate::", `return frequency`))),
        args = list(date)
      )
    ) %>% dplyr::group_by(year, unit) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c(year, unit)) %>%
      dplyr::mutate(
        commodity = (value / dplyr::lag(value, 1L)) - 1L
      ) %>% dplyr::select(date, commodity)
  }
  
  
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
  
  data <- if (`return frequency` == "day"){
    tryCatch(
      { 
        data <- dplyr::left_join(commodity_returns, factors_returns, by = "date")  %>%
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
  regime <- combination$regime
  data <- dplyr::left_join(data, regimes, by = "date") %>%
    dplyr::filter(regime == !! regime) %>% dplyr::select(-regime)
  
  model <- combination$regressors
  
  formula <- paste("commodity", model, sep = " ~ ")
  
  result  <- tryCatch(
    { lm(rlang::parse_expr(formula), data = data) },
    error = function(e) { NA }
  )
  
  if (! is.na(result)){
    coefficients <- broom::tidy(result)
    rsquared <- stats::summary.lm(result)$r.squared
  } else { coefficients <- NA; rsquared <- NA }
  
  dplyr::mutate(
    combination, coefficients = list(coefficients), rsquared = rsquared
  )
}


`factor picks ~ factors from picks` <- dplyr::bind_rows(whole, regimes)


# regressions <- dplyr::bind_rows(
#   regressions,
#   tibble::tibble(
#     analysis = c("factor picks ~ factors from picks"),
#     results = list(`factor picks ~ factors from picks`)
#   )
# )
# readr::write_rds(
#   regressions, 
#   path = here::here("explore", "results", "regressions-time-series-new.rds")
# )




## dependent variables: US commodities ####
`US commodity futures tickers` <- dplyr::left_join(
  dplyr::select(tickers_futures, ticker, MIC), 
  dplyr::select(exchanges, MIC, country), 
  by = "MIC"
) %>% 
  dplyr::filter(ticker %in% `commodity futures tickers`, country == "US") %>% 
  dplyr::select(ticker) %>% purrr::flatten_chr()

### no CHP regime ####
combinations <- dplyr::distinct(
  `factors from picks`, 
  `picking factor asset pool`, `picking factor name`, `picking factor leg`,
  `update frequency`, `return frequency`, `ranking period`, `long threshold`,
  `short threshold`, period
) %>%
  tidyr::expand_grid(tibble::tibble(commodity = `US commodity futures tickers`)) %>% 
  tidyr::expand_grid(tibble::tibble(regressors = regressors)) %>%
  tidyr::expand_grid(tibble::tibble(`factor leg` = c("long", "short", "factor"))) %>%
  dplyr::filter(
    ! ((stringr::str_detect(regressors, "market")) & 
         (`factor leg` %in% c("long", "short")))
  )


whole <- foreach(y = 1L:nrow(combinations), .combine = rbind) %dopar% {
  library(magrittr); library(lubridate)
  
  message(paste(y, nrow(combinations), sep = "/"))
  
  combination <- dplyr::slice(combinations, y)
  
  # browser()
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
  
  ticker <- combination$commodity
  
  prices <- dplyr::filter(
    `commodity futures prices`,
    `active contract ticker` == ticker, date >= start, date <= end
  )
  
  
  `return frequency` <- combination$`return frequency`
  
  commodity_returns <- if (`return frequency` == "day") {
    dplyr::mutate(
      prices,
      commodity = (value / dplyr::lag(value, 1L)) - 1L
    ) %>% dplyr::select(date, commodity)
  } else {
    dplyr::mutate(
      prices, year = lubridate::year(date),
      unit = do.call(
        what = eval(parse(text = paste0("lubridate::", `return frequency`))),
        args = list(date)
      )
    ) %>% dplyr::group_by(year, unit) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c(year, unit)) %>%
      dplyr::mutate(
        commodity = (value / dplyr::lag(value, 1L)) - 1L
      ) %>% dplyr::select(date, commodity)
  }
  
  
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
  
  
  data <- if (`return frequency` == "day"){
    tryCatch(
      { 
        data <- dplyr::left_join(commodity_returns, factors_returns, by = "date")  %>%
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
  
  
  model <- combination$regressors
  
  formula <- paste("commodity", model, sep = " ~ ")
  
  result  <- tryCatch(
    { lm(rlang::parse_expr(formula), data = data) },
    error = function(e) { NA }
  )
  
  if (! is.na(result)){
    coefficients <- broom::tidy(result)
    rsquared <- stats::summary.lm(result)$r.squared
  } else { coefficients <- NA; rsquared <- NA }
  
  dplyr::mutate(
    combination,
    regime = "all", coefficients = list(coefficients), rsquared = rsquared
  )
}



### backwardation vs. contango ####
combinations <- dplyr::distinct(
  `factors from picks`, 
  `picking factor asset pool`, `picking factor name`, `picking factor leg`,
  `update frequency`, `return frequency`, `ranking period`, `long threshold`,
  `short threshold`, period
) %>%
  tidyr::expand_grid(tibble::tibble(commodity = `US commodity futures tickers`)) %>% 
  tidyr::expand_grid(tibble::tibble(regressors = regressors)) %>%
  tidyr::expand_grid(tibble::tibble(`factor leg` = c("long", "short", "factor"))) %>%
  dplyr::filter(
    ! ((stringr::str_detect(regressors, "market")) & 
         (`factor leg` %in% c("long", "short")))
  ) %>%
  tidyr::expand_grid(tibble::tibble(regime = c("backwardation", "contango")))


regimes <- foreach(y = 1L:nrow(combinations), .combine = rbind) %dopar% {
  library(magrittr); library(lubridate)
  
  message(paste(y, nrow(combinations), sep = "/"))
  
  combination <- dplyr::slice(combinations, y)
  
  # browser()
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
  
  ticker <- combination$commodity
  
  prices <- dplyr::filter(
    `commodity futures prices`,
    `active contract ticker` == ticker, date >= start, date <= end
  )
  
  
  `return frequency` <- combination$`return frequency`
  
  commodity_returns <- if (`return frequency` == "day") {
    dplyr::mutate(
      prices,
      commodity = (value / dplyr::lag(value, 1L)) - 1L
    ) %>% dplyr::select(date, commodity)
  } else {
    dplyr::mutate(
      prices, year = lubridate::year(date),
      unit = do.call(
        what = eval(parse(text = paste0("lubridate::", `return frequency`))),
        args = list(date)
      )
    ) %>% dplyr::group_by(year, unit) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c(year, unit)) %>%
      dplyr::mutate(
        commodity = (value / dplyr::lag(value, 1L)) - 1L
      ) %>% dplyr::select(date, commodity)
  }
  
  
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
  
  data <- if (`return frequency` == "day"){
    tryCatch(
      { 
        data <- dplyr::left_join(commodity_returns, factors_returns, by = "date")  %>%
          dplyr::left_join(`risk free`, by = "date")  %>% dplyr::arrange(date) %>% 
          na.omit() %>% as.data.frame()
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
  regime <- combination$regime
  data <- dplyr::left_join(data, regimes, by = "date") %>%
    dplyr::filter(regime == !! regime) %>% dplyr::select(-regime)
  
  model <- combination$regressors
  
  formula <- paste("commodity", model, sep = " ~ ")
  
  result  <- tryCatch(
    { lm(rlang::parse_expr(formula), data = data) },
    error = function(e) { NA }
  )
  
  if (! is.na(result)){
    coefficients <- broom::tidy(result)
    rsquared <- stats::summary.lm(result)$r.squared
  } else { coefficients <- NA; rsquared <- NA }
  
  dplyr::mutate(
    combination,
    coefficients = list(coefficients), rsquared = rsquared
  )
}


`US commodities ~ factors from picks` <- dplyr::bind_rows(whole, regimes)


# regressions <- dplyr::bind_rows(
#   regressions,
#   tibble::tibble(
#     analysis = c("US commodities ~ factors from picks"),
#     results = list(`US commodities ~ factors from picks`)
#   )
# )
# readr::write_rds(
#   regressions, 
#   path = here::here("explore", "results", "regressions-time-series-new.rds")
# )




















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
  tidyr::expand_grid(tibble::tibble(commodity = `UK metal futures tickers`)) %>% 
  tidyr::expand_grid(tibble::tibble(regressors = regressors)) %>%
  tidyr::expand_grid(tibble::tibble(`factor leg` = c("long", "short", "factor"))) %>%
  dplyr::filter(
    ! ((stringr::str_detect(regressors, "market")) & 
         (`factor leg` %in% c("long", "short")))
  )


whole <- foreach(y = 1L:nrow(combinations), .combine = rbind) %dopar% {
  library(magrittr); library(lubridate)
  
  message(paste(y, nrow(combinations), sep = "/"))
  
  combination <- dplyr::slice(combinations, y)
  
  # browser()
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
  
  ticker <- combination$commodity
  
  prices <- dplyr::filter(
    `commodity futures prices`,
    `active contract ticker` == ticker, date >= start, date <= end
  )
  
  
  `return frequency` <- combination$`return frequency`
  
  commodity_returns <- if (`return frequency` == "day") {
    dplyr::mutate(
      prices,
      commodity = (value / dplyr::lag(value, 1L)) - 1L
    ) %>% dplyr::select(date, commodity)
  } else {
    dplyr::mutate(
      prices, year = lubridate::year(date),
      unit = do.call(
        what = eval(parse(text = paste0("lubridate::", `return frequency`))),
        args = list(date)
      )
    ) %>% dplyr::group_by(year, unit) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c(year, unit)) %>%
      dplyr::mutate(
        commodity = (value / dplyr::lag(value, 1L)) - 1L
      ) %>% dplyr::select(date, commodity)
  }
  
  
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
  
  data <- if (`return frequency` == "day"){
    tryCatch(
      {
        data <- dplyr::left_join(commodity_returns, factors_returns, by = "date")  %>%
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
  
  
  model <- combination$regressors
  
  formula <- paste("commodity", model, sep = " ~ ")
  
  result  <- tryCatch(
    { lm(rlang::parse_expr(formula), data = data) },
    error = function(e) { NA }
  )
  
  if (! is.na(result)){
    coefficients <- broom::tidy(result)
    rsquared <- stats::summary.lm(result)$r.squared
  } else { coefficients <- NA; rsquared <- NA }
  
  dplyr::mutate(
    combination,
    regime = "all", coefficients = list(coefficients), rsquared = rsquared
  )
}


#### backwardation vs. contango ####
combinations <- dplyr::distinct(
  `factors from picks`, 
  `picking factor asset pool`, `picking factor name`, `picking factor leg`,
  `update frequency`, `return frequency`, `ranking period`, `long threshold`,
  `short threshold`, period
) %>%
  tidyr::expand_grid(tibble::tibble(commodity = `UK metal futures tickers`)) %>% 
  tidyr::expand_grid(tibble::tibble(regressors = regressors)) %>%
  tidyr::expand_grid(tibble::tibble(`factor leg` = c("long", "short", "factor"))) %>%
  dplyr::filter(
    ! ((stringr::str_detect(regressors, "market")) & 
         (`factor leg` %in% c("long", "short")))
  ) %>% 
  tidyr::expand_grid(tibble::tibble(regime = c("backwardation", "contango")))


regimes <- foreach(y = 1L:nrow(combinations), .combine = rbind) %dopar% {
  library(magrittr); library(lubridate)
  
  message(paste(y, nrow(combinations), sep = "/"))
  
  combination <- dplyr::slice(combinations, y)
  
  # browser()
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
  
  ticker <- combination$commodity
  
  prices <- dplyr::filter(
    `commodity futures prices`,
    `active contract ticker` == ticker, date >= start, date <= end
  )
  
  
  `return frequency` <- combination$`return frequency`
  
  commodity_returns <- if (`return frequency` == "day") {
    dplyr::mutate(
      prices,
      commodity = (value / dplyr::lag(value, 1L)) - 1L
    ) %>% dplyr::select(date, commodity)
  } else {
    dplyr::mutate(
      prices, year = lubridate::year(date),
      unit = do.call(
        what = eval(parse(text = paste0("lubridate::", `return frequency`))),
        args = list(date)
      )
    ) %>% dplyr::group_by(year, unit) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c(year, unit)) %>%
      dplyr::mutate(
        commodity = (value / dplyr::lag(value, 1L)) - 1L
      ) %>% dplyr::select(date, commodity)
  }
  
  
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
  
  data <- if (`return frequency` == "day"){
    tryCatch(
      { 
        data <- dplyr::left_join(commodity_returns, factors_returns, by = "date")  %>%
          dplyr::left_join(`risk free`, by = "date")  %>% dplyr::arrange(date) %>% 
          na.omit() %>% as.data.frame()
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
  regime <- combination$regime
  data <- dplyr::left_join(data, regimes, by = "date") %>%
    dplyr::filter(regime == !! regime) %>% dplyr::select(-regime)
  
  model <- combination$regressors
  
  formula <- paste("commodity", model, sep = " ~ ")
  
  result  <- tryCatch(
    { lm(rlang::parse_expr(formula), data = data) },
    error = function(e) { NA }
  )
  
  if (! is.na(result)){
    coefficients <- broom::tidy(result)
    rsquared <- stats::summary.lm(result)$r.squared
  } else { coefficients <- NA; rsquared <- NA }
  
  dplyr::mutate(
    combination, coefficients = list(coefficients), rsquared = rsquared
  )
}

`UK metals ~ factors from picks` <- dplyr::bind_rows(whole, regimes)




# regressions <- dplyr::bind_rows(
#   regressions,
#   tibble::tibble(
#     analysis = c("UK metals ~ factors from picks"),
#     results = list(`UK metals ~ factors from picks`)
#   )
# )
# readr::write_rds(
#   regressions, 
#   path = here::here("explore", "results", "regressions-time-series-new.rds")
# )



regressions <- tibble::tibble(
  analysis = c(
    "US commodities ~ factors from US commodities", "factor picks ~ factors from picks",
    "US commodities ~ factors from picks", 
    "UK metals ~ factors from picks"
  ),
  results = list(
    `US commodities ~ factors from US commodities`,
    `factor picks ~ factors from picks`, `US commodities ~ factors from picks`,
    `UK metals ~ factors from picks`
  )
)

readr::write_rds(
  regressions, 
  path = here::here("explore", "results", "regressions-time-series.rds")
)

parallel::stopCluster(cluster)




















































# US commodity returns ~ US individual CHP ####
library(finRes); library(magrittr); library(doParallel); library(lubridate)

# globals ####
data("tickers_futures", "tickers_cftc", package = "BBGsymbols")
data("exchanges", package = "fewISOs")
storethat <- here::here("data", "storethat.sqlite")
periods <- tibble::tibble(
  period = c(
    rep("past", 2L), rep("financialization", 2L), 
    rep("crisis", 2L), rep("present", 2L)
  ), 
  bound = rep(c("start", "end"), 4L),
  date = c(
    "1997-07-01", "2003-12-31", "2004-01-01", "2008-09-14", 
    "2008-09-15", "2013-06-19", "2013-06-20", "2018-12-31"
  )
)

# load data ####
start <- "1996-01-01"; end <- "2018-12-31"
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
`futures prices` <- dplyr::left_join(
  `commodity futures data`@data, 
  dplyr::select(
    `commodity futures data`@term_structure_tickers, 
    `active contract ticker`, ticker, `TS position`),
  by = "ticker"
) %>% 
  dplyr::filter(`TS position` == 1L, field == "PX_LAST") %>% 
  dplyr::select(commodity = `active contract ticker`, date, price = value)

`commodity CFTC tickers` <- `US commodity futures tickers`[
  `US commodity futures tickers` %in% tickers_cftc$`active contract ticker`
]
`commodity CFTC data` <- pullit::pull_futures_CFTC(
  source = "storethat", active_contract_tickers = `commodity CFTC tickers`, 
  start = start, end = end, file = storethat
)
`futures CHP` <- dplyr::left_join(
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
  dplyr::select(commodity = `active contract ticker`, date, pressure) %>%
  dplyr::ungroup()
combinations <- tidyr::expand_grid(
  dplyr::distinct(`futures CHP`, commodity),
  dplyr::distinct(periods, period)
)
`futures CHP` <- purrr::pmap_df(
  combinations, 
  function(commodity, period){
    
    start <- dplyr::filter(periods, period == !! period, bound == "start") %>% 
      dplyr::select(date) %>% purrr::flatten_chr()
    
    end <- dplyr::filter(periods, period == !! period, bound == "end") %>% 
      dplyr::select(date) %>% purrr::flatten_chr()
    
    # browser()
    regimes <- dplyr::filter(
      `futures CHP`, commodity == !! commodity, 
      date >= as.Date(start), date <= as.Date(end)
    ) %>% 
      dplyr::mutate(
        regime = ifelse(
          pressure < median(pressure), "backwardation", "contango"
        ),
        period = !! period
      )
  })

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
`aggregate CHP` <- purrr::map_df(
  unique(periods$period), 
  function(x){
    
    start <- dplyr::filter(periods, period == x, bound == "start") %>% 
      dplyr::select(date) %>% purrr::flatten_chr()
    
    end <- dplyr::filter(periods, period == x, bound == "end") %>% 
      dplyr::select(date) %>% purrr::flatten_chr()
    
    # browser()
    regimes <- dplyr::filter(
      `aggregate CHP`, date >= as.Date(start), date <= as.Date(end)
    ) %>% 
      dplyr::mutate(
        regime = ifelse(
          `aggregate CHP` < median(`aggregate CHP`), "backwardation", "contango"
        ),
        period = x
      )
  })


combinations <- tidyr::expand_grid(
  dplyr::distinct(`futures prices`, commodity),
  dplyr::distinct(periods, period),
  tibble::tibble(regime = c("all", "backwardation", "contango"))
) 

individual <- purrr::map_df(1L:nrow(combinations), function(y){
  
  # message(paste(y, nrow(combinations), sep = "/"))
  combination <- dplyr::slice(combinations, y)
  
  period <- combination$period
  start <- dplyr::filter(periods, period == !! period, bound == "start") %>%
    dplyr::select(date) %>% purrr::flatten_chr()
  end <- dplyr::filter(periods, period == !! period, bound == "end") %>%
    dplyr::select(date) %>% purrr::flatten_chr()
  
  ticker <- combination$commodity
  
  regime <- combination$regime
  # browser()
  CHP <- dplyr::filter(`aggregate CHP`, period == !!period) %>%
    dplyr::rename(aggregate.regime = regime) %>% 
    dplyr::select(-c(period, `aggregate CHP`)) %>%
    dplyr::left_join(
      dplyr::filter(`futures CHP`, commodity == ticker)%>%  
        dplyr::select(-c(period)) %>%
        dplyr::rename(individual.regime = regime), 
      by = "date"
    )
  data <- dplyr::left_join(CHP, `futures prices`, by = c("commodity", "date")) %>%
    dplyr::mutate(
      return = (price / dplyr::lag(price)) - 1,
      pressure.level.contemporaneous = pressure,
      pressure.level.lagged = dplyr::lag(pressure), 
      pressure.change.contemporaneous = (pressure / dplyr::lag(pressure)) - 1,
      pressure.change.lagged = dplyr::lag(pressure.change.contemporaneous)
    ) %>% dplyr::select(-c("price", "commodity"))
  
  
  regressors <- c(
    "pressure.level.contemporaneous", "pressure.level.lagged",
    "pressure.change.contemporaneous", "pressure.change.lagged"
  )
  
  
  if (regime == "all") {
    purrr::map_df(regressors, function(x){
      message(paste0(paste(y, nrow(combinations), sep = "/"), " - regressor:", x))
      # browser()
      formula <- paste("return", x, sep = " ~ ")
      model <- tryCatch(
        { stats::lm(rlang::parse_expr(formula), data = data, na.action = na.omit) },
        error = function(e) { NA }
      )
      if (! is.na(model)){
        coefficients <- broom::tidy(model)
        rsquared <- stats::summary.lm(model)$r.squared
      } else { coefficients <- NA; rsquared <- NA }
      tibble::tibble(
        commodity = ticker, period = period, regressor = x, 
        regime.CHP.type = "none", CHP.regime = "all",
        coefficients = list(coefficients), rsquared = rsquared
      )
    })
  } else {
    # individual 
    individual <- dplyr::filter(data, individual.regime == regime)
    individual <- purrr::map_df(regressors, function(x){
      message(paste0(paste(y, nrow(combinations), sep = "/"), " - regressor:", x))
      # browser()
      formula <- paste("return", x, sep = " ~ ")
      model <- tryCatch(
        { stats::lm(rlang::parse_expr(formula), data = individual, na.action = na.omit) },
        error = function(e) { NA }
      )
      if (! is.na(model)){
        coefficients <- broom::tidy(model)
        rsquared <- stats::summary.lm(model)$r.squared
      } else { coefficients <- NA; rsquared <- NA }
      tibble::tibble(
        commodity = ticker, period = period, regressor = x, 
        regime.CHP.type = "individual", CHP.regime = regime,
        coefficients = list(coefficients), rsquared = rsquared
      )
    })
    # aggregate
    aggregate <- dplyr::filter(data, aggregate.regime == regime)
    aggregate <- purrr::map_df(regressors, function(x){
      message(paste0(paste(y, nrow(combinations), sep = "/"), " - regressor:", x))
      # browser()
      formula <- paste("return", x, sep = " ~ ")
      model <- tryCatch(
        { stats::lm(rlang::parse_expr(formula), data = aggregate, na.action = na.omit) },
        error = function(e) { NA }
      )
      if (! is.na(model)){
        coefficients <- broom::tidy(model)
        rsquared <- stats::summary.lm(model)$r.squared
      } else { coefficients <- NA; rsquared <- NA }
      tibble::tibble(
        commodity = ticker, period = period, regressor = x, 
        regime.CHP.type = "aggregate", CHP.regime = regime,
        coefficients = list(coefficients), rsquared = rsquared
      )
    })
    
    dplyr::bind_rows(individual, aggregate)
  }
}) %>% 
  dplyr::mutate(
    period = factor(period, levels = c("past", "financialization", "crisis", "present"))
  ) %>%
  dplyr::arrange(
    commodity, period, regressor, regime.CHP.type, CHP.regime
  )


regressions <- readr::read_rds(
  path = here::here("explore", "results", "regressions-time-series.rds")
)
regressions <- dplyr::bind_rows(
  regressions,
  tibble::tibble(
    analysis = "US commodity returns ~ US commodity individual CHP",
    results = list(individual)
  )
)
readr::write_rds(
  regressions,
  path = here::here("explore", "results", "regressions-time-series.rds")
)



















































# US commodity returns ~ aggregate CHP ####
library(finRes); library(magrittr); library(doParallel); library(lubridate)

# globals ####
data("tickers_futures", "tickers_cftc", package = "BBGsymbols")
data("exchanges", package = "fewISOs")
storethat <- here::here("data", "storethat.sqlite")
periods <- tibble::tibble(
  period = c(
    rep("past", 2L), rep("financialization", 2L), 
    rep("crisis", 2L), rep("present", 2L)
  ), 
  bound = rep(c("start", "end"), 4L),
  date = c(
    "1997-07-01", "2003-12-31", "2004-01-01", "2008-09-14", 
    "2008-09-15", "2013-06-19", "2013-06-20", "2018-12-31"
  )
)

# load data ####
start <- "1996-01-01"; end <- "2018-12-31"
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
`futures prices` <- dplyr::left_join(
  `commodity futures data`@data, 
  dplyr::select(
    `commodity futures data`@term_structure_tickers, 
    `active contract ticker`, ticker, `TS position`),
  by = "ticker"
) %>% 
  dplyr::filter(`TS position` == 1L, field == "PX_LAST") %>% 
  dplyr::select(commodity = `active contract ticker`, date, price = value)

`commodity CFTC tickers` <- `US commodity futures tickers`[
  `US commodity futures tickers` %in% tickers_cftc$`active contract ticker`
]
`commodity CFTC data` <- pullit::pull_futures_CFTC(
  source = "storethat", active_contract_tickers = `commodity CFTC tickers`, 
  start = start, end = end, file = storethat
)
`futures CHP` <- dplyr::left_join(
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
  dplyr::select(commodity = `active contract ticker`, date, pressure) %>%
  dplyr::ungroup()
combinations <- tidyr::expand_grid(
  dplyr::distinct(`futures CHP`, commodity),
  dplyr::distinct(periods, period)
)
`futures CHP` <- purrr::pmap_df(
  combinations, 
  function(commodity, period){
    
    start <- dplyr::filter(periods, period == !! period, bound == "start") %>% 
      dplyr::select(date) %>% purrr::flatten_chr()
    
    end <- dplyr::filter(periods, period == !! period, bound == "end") %>% 
      dplyr::select(date) %>% purrr::flatten_chr()

    # browser()
    regimes <- dplyr::filter(
      `futures CHP`, commodity == !! commodity, 
      date >= as.Date(start), date <= as.Date(end)
    ) %>% 
      dplyr::mutate(
        regime = ifelse(
          pressure < median(pressure), "backwardation", "contango"
        ),
        period = !! period
      )
  })


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
`aggregate CHP` <- purrr::map_df(
  unique(periods$period), 
  function(x){
    
    start <- dplyr::filter(periods, period == x, bound == "start") %>% 
      dplyr::select(date) %>% purrr::flatten_chr()
    
    end <- dplyr::filter(periods, period == x, bound == "end") %>% 
      dplyr::select(date) %>% purrr::flatten_chr()

    # browser()
    regimes <- dplyr::filter(
      `aggregate CHP`, date >= as.Date(start), date <= as.Date(end)
    ) %>% 
      dplyr::mutate(
        regime = ifelse(
          `aggregate CHP` < median(`aggregate CHP`), "backwardation", "contango"
        ),
        period = x
      )
  })


combinations <- tidyr::expand_grid(
  dplyr::distinct(`futures prices`, commodity),
  dplyr::distinct(periods, period),
  tibble::tibble(regime = c("all", "backwardation", "contango"))
) 

aggregate <- purrr::map_df(1L:nrow(combinations), function(y){
  
  # message(paste(y, nrow(combinations), sep = "/"))
  combination <- dplyr::slice(combinations, y)
  
  period <- combination$period
  start <- dplyr::filter(periods, period == !! period, bound == "start") %>%
    dplyr::select(date) %>% purrr::flatten_chr()
  end <- dplyr::filter(periods, period == !! period, bound == "end") %>%
    dplyr::select(date) %>% purrr::flatten_chr()
  
  ticker <- combination$commodity
  
  regime <- combination$regime
  
  CHP <- dplyr::filter(`aggregate CHP`, period == !!period) %>%
    dplyr::rename(aggregate.regime = regime, pressure = `aggregate CHP`) %>%
    dplyr::select(-period) %>%
    dplyr::left_join(
      dplyr::filter(`futures CHP`, commodity == ticker)%>%  
        dplyr::select(-c(period, pressure)) %>%
        dplyr::rename(individual.regime = regime), 
      by = "date"
    )
  data <- dplyr::left_join(CHP, `futures prices`, by = c("commodity", "date")) %>%
    dplyr::mutate(
      return = (price / dplyr::lag(price)) - 1,
      pressure.level.contemporaneous = pressure,
      pressure.level.lagged = dplyr::lag(pressure), 
      pressure.change.contemporaneous = (pressure / dplyr::lag(pressure)) - 1,
      pressure.change.lagged = dplyr::lag(pressure.change.contemporaneous)
    ) %>% dplyr::select(-c("price", "commodity"))


  regressors <- c(
    "pressure.level.contemporaneous", "pressure.level.lagged",
    "pressure.change.contemporaneous", "pressure.change.lagged"
  )
  
  
  if (regime == "all") {
    purrr::map_df(regressors, function(x){
      message(paste0(paste(y, nrow(combinations), sep = "/"), " - regressor:", x))
      # browser()
      formula <- paste("return", x, sep = " ~ ")
      model <- tryCatch(
        { stats::lm(rlang::parse_expr(formula), data = data, na.action = na.omit) },
        error = function(e) { NA }
      )
      if (! is.na(model)){
        coefficients <- broom::tidy(model)
        rsquared <- stats::summary.lm(model)$r.squared
      } else { coefficients <- NA; rsquared <- NA }
      tibble::tibble(
        commodity = ticker, period = period, regressor = x, 
        regime.CHP.type = "none", CHP.regime = "all",
        coefficients = list(coefficients), rsquared = rsquared
      )
    })
  } else {
    # individual 
    individual <- dplyr::filter(data, individual.regime == regime)
    individual <- purrr::map_df(regressors, function(x){
      message(paste0(paste(y, nrow(combinations), sep = "/"), " - regressor:", x))
      # browser()
      formula <- paste("return", x, sep = " ~ ")
      model <- tryCatch(
        { stats::lm(rlang::parse_expr(formula), data = individual, na.action = na.omit) },
        error = function(e) { NA }
      )
      if (! is.na(model)){
        coefficients <- broom::tidy(model)
        rsquared <- stats::summary.lm(model)$r.squared
      } else { coefficients <- NA; rsquared <- NA }
      tibble::tibble(
        commodity = ticker, period = period, regressor = x, 
        regime.CHP.type = "individual", CHP.regime = regime,
        coefficients = list(coefficients), rsquared = rsquared
      )
    })
    # aggregate
    aggregate <- dplyr::filter(data, aggregate.regime == regime)
    aggregate <- purrr::map_df(regressors, function(x){
      message(paste0(paste(y, nrow(combinations), sep = "/"), " - regressor:", x))
      # browser()
      formula <- paste("return", x, sep = " ~ ")
      model <- tryCatch(
        { stats::lm(rlang::parse_expr(formula), data = aggregate, na.action = na.omit) },
        error = function(e) { NA }
      )
      if (! is.na(model)){
        coefficients <- broom::tidy(model)
        rsquared <- stats::summary.lm(model)$r.squared
      } else { coefficients <- NA; rsquared <- NA }
      tibble::tibble(
        commodity = ticker, period = period, regressor = x, 
        regime.CHP.type = "aggregate", CHP.regime = regime,
        coefficients = list(coefficients), rsquared = rsquared
      )
    })
    
    dplyr::bind_rows(individual, aggregate)
  }
}) %>% 
  dplyr::mutate(
    period = factor(period, levels = c("past", "financialization", "crisis", "present"))
  ) %>%
  dplyr::arrange(
    commodity, period, regressor, regime.CHP.type, CHP.regime
  )

regressions <- readr::read_rds(
  path = here::here("explore", "results", "regressions-time-series.rds")
)
regressions <- dplyr::bind_rows(
  regressions,
  tibble::tibble(
    analysis = "US commodity returns ~ US commodity aggregate CHP",
    results = list(aggregate)
  )
)
readr::write_rds(
  regressions,
  path = here::here("explore", "results", "regressions-time-series.rds")
)



























# UK commodity returns ~ US aggregate CHP ####
library(finRes); library(magrittr); library(doParallel); library(lubridate)

# globals ####
data("tickers_futures", "tickers_cftc", package = "BBGsymbols")
data("exchanges", package = "fewISOs")
storethat <- here::here("data", "storethat.sqlite")
periods <- tibble::tibble(
  period = c(
    rep("past", 2L), rep("financialization", 2L), 
    rep("crisis", 2L), rep("present", 2L)
  ), 
  bound = rep(c("start", "end"), 4L),
  date = c(
    "1997-07-01", "2003-12-31", "2004-01-01", "2008-09-14", 
    "2008-09-15", "2013-06-19", "2013-06-20", "2018-12-31"
  )
)

# load data ####
start <- "1996-01-01"; end <- "2018-12-31"
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

`UK commodity futures tickers` <- dplyr::left_join(
  dplyr::select(tickers_futures, ticker, MIC), 
  dplyr::select(exchanges, MIC, country), 
  by = "MIC"
) %>% 
  dplyr::filter(ticker %in% `commodity futures tickers`, country == "GB") %>% 
  dplyr::select(ticker) %>% purrr::flatten_chr()

`commodity futures data` <- pullit::pull_futures_market(
  source = "storethat", type = "term structure", 
  active_contract_tickers = `UK commodity futures tickers`,
  start = start, end = end, TS_positions = 1L:2L, roll_type = "A", 
  roll_days = 0L, roll_months = 0L,roll_adjustment = "N", file = storethat
)
`futures prices` <- dplyr::left_join(
  `commodity futures data`@data, 
  dplyr::select(
    `commodity futures data`@term_structure_tickers, 
    `active contract ticker`, ticker, `TS position`),
  by = "ticker"
) %>% 
  dplyr::filter(`TS position` == 1L, field == "PX_LAST") %>% 
  dplyr::select(commodity = `active contract ticker`, date, price = value)

`commodity CFTC tickers` <- `US commodity futures tickers`[
  `US commodity futures tickers` %in% tickers_cftc$`active contract ticker`
]
`commodity CFTC data` <- pullit::pull_futures_CFTC(
  source = "storethat", active_contract_tickers = `commodity CFTC tickers`, 
  start = start, end = end, file = storethat
)
`futures CHP` <- dplyr::left_join(
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
  dplyr::select(commodity = `active contract ticker`, date, pressure) %>%
  dplyr::ungroup()
combinations <- tidyr::expand_grid(
  dplyr::distinct(`futures CHP`, commodity),
  dplyr::distinct(periods, period)
)
`futures CHP` <- purrr::pmap_df(
  combinations, 
  function(commodity, period){
    
    start <- dplyr::filter(periods, period == !! period, bound == "start") %>% 
      dplyr::select(date) %>% purrr::flatten_chr()
    
    end <- dplyr::filter(periods, period == !! period, bound == "end") %>% 
      dplyr::select(date) %>% purrr::flatten_chr()
    
    # browser()
    regimes <- dplyr::filter(
      `futures CHP`, commodity == !! commodity, 
      date >= as.Date(start), date <= as.Date(end)
    ) %>% 
      dplyr::mutate(
        regime = ifelse(
          pressure < median(pressure), "backwardation", "contango"
        ),
        period = !! period
      )
  })

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
`aggregate CHP` <- purrr::map_df(
  unique(periods$period), 
  function(x){
    
    start <- dplyr::filter(periods, period == x, bound == "start") %>% 
      dplyr::select(date) %>% purrr::flatten_chr()
    
    end <- dplyr::filter(periods, period == x, bound == "end") %>% 
      dplyr::select(date) %>% purrr::flatten_chr()
    
    # browser()
    regimes <- dplyr::filter(
      `aggregate CHP`, date >= as.Date(start), date <= as.Date(end)
    ) %>% 
      dplyr::mutate(
        regime = ifelse(
          `aggregate CHP` < median(`aggregate CHP`), "backwardation", "contango"
        ),
        period = x
      )
  })


combinations <- tidyr::expand_grid(
  dplyr::distinct(`futures prices`, commodity),
  dplyr::distinct(periods, period),
  tibble::tibble(regime = c("all", "backwardation", "contango"))
) 

aggregate <- purrr::map_df(1L:nrow(combinations), function(y){
  
  # message(paste(y, nrow(combinations), sep = "/"))
  combination <- dplyr::slice(combinations, y)
  
  period <- combination$period
  start <- dplyr::filter(periods, period == !! period, bound == "start") %>%
    dplyr::select(date) %>% purrr::flatten_chr()
  end <- dplyr::filter(periods, period == !! period, bound == "end") %>%
    dplyr::select(date) %>% purrr::flatten_chr()
  
  ticker <- combination$commodity
  
  regime <- combination$regime
  # browser()
  CHP <- dplyr::filter(`aggregate CHP`, period == !!period) %>%
    dplyr::rename(aggregate.regime = regime, pressure = `aggregate CHP`) %>%
    dplyr::select(-period)
  data <- dplyr::left_join(CHP, `futures prices`, by = "date") %>%
    dplyr::mutate(
      return = (price / dplyr::lag(price)) - 1,
      pressure.level.contemporaneous = pressure,
      pressure.level.lagged = dplyr::lag(pressure), 
      pressure.change.contemporaneous = (pressure / dplyr::lag(pressure)) - 1,
      pressure.change.lagged = dplyr::lag(pressure.change.contemporaneous)
    ) %>% dplyr::select(-c("price", "commodity"))
  
  regressors <- c(
    "pressure.level.contemporaneous", "pressure.level.lagged",
    "pressure.change.contemporaneous", "pressure.change.lagged"
  )
  if (regime == "all") {
    purrr::map_df(regressors, function(x){
      message(paste0(paste(y, nrow(combinations), sep = "/"), " - regressor:", x))
      # browser()
      formula <- paste("return", x, sep = " ~ ")
      model <- tryCatch(
        { stats::lm(rlang::parse_expr(formula), data = data, na.action = na.omit) },
        error = function(e) { NA }
      )
      if (! is.na(model)){
        coefficients <- broom::tidy(model)
        rsquared <- stats::summary.lm(model)$r.squared
      } else { coefficients <- NA; rsquared <- NA }
      tibble::tibble(
        commodity = ticker, period = period, regressor = x, 
        regime.CHP.type = "none", CHP.regime = "all",
        coefficients = list(coefficients), rsquared = rsquared
      )
    })
  } else {
    # aggregate
    aggregate <- dplyr::filter(data, aggregate.regime == regime)
    aggregate <- purrr::map_df(regressors, function(x){
      message(paste0(paste(y, nrow(combinations), sep = "/"), " - regressor:", x))
      # browser()
      formula <- paste("return", x, sep = " ~ ")
      model <- tryCatch(
        { stats::lm(rlang::parse_expr(formula), data = aggregate, na.action = na.omit) },
        error = function(e) { NA }
      )
      if (! is.na(model)){
        coefficients <- broom::tidy(model)
        rsquared <- stats::summary.lm(model)$r.squared
      } else { coefficients <- NA; rsquared <- NA }
      tibble::tibble(
        commodity = ticker, period = period, regressor = x, 
        regime.CHP.type = "aggregate", CHP.regime = regime,
        coefficients = list(coefficients), rsquared = rsquared
      )
    })
  }
}) %>% 
  dplyr::mutate(
    period = factor(period, levels = c("past", "financialization", "crisis", "present"))
  ) %>%
  dplyr::arrange(
    commodity, period, regressor, regime.CHP.type, CHP.regime
  )

regressions <- readr::read_rds(
  path = here::here("explore", "results", "regressions-time-series.rds")
)
regressions <- dplyr::bind_rows(
  regressions,
  tibble::tibble(
    analysis = "UK commodity returns ~ US commodity aggregate CHP",
    results = list(aggregate)
  )
)
readr::write_rds(
  regressions,
  path = here::here("explore", "results", "regressions-time-series.rds")
)





regressions <- readr::read_rds(
  path = here::here("explore", "results", "regressions-time-series.rds")
) %>% dplyr::slice(1L:4L)
readr::write_rds(
  regressions,
  path = here::here("explore", "results", "regressions-time-series.rds")
)
