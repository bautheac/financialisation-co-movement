library(finRes); library(magrittr); library(doParallel)
# source("explore/scripts/functions - shared.r")

# start cluster ####
cluster <- makeCluster(detectCores() - 1L, outfile = "")
registerDoParallel(cluster, cores = detectCores() - 1L)

# globals ####
data("tickers_futures", "tickers_cftc", package = "BBGsymbols")
data("exchanges", package = "fewISOs")

storethat <- paste0(here::here(), "/data/storethat.sqlite")

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
# could add COA Comdty (Crude oil-brent - IFEU) ENA Comdty (Crude oil-WTI - IFEU), PGA Comdty (Gasoline-RBOB - IFEU), NVA Comdty (Heating oil - IFEU), 
# FNA Comdty (Natural gas - IFEU), QSA Comdty (Gasoil-low sulfur - IFEU)
`commodity futures tickers` <- c(
  "BOA Comdty", "C A Comdty", "CCA Comdty", "CLA Comdty", "CTA Comdty", 
  "FCA Comdty", "GCA Comdty", "HGA Comdty", "HOA Comdty", "JOA Comdty", 
  "KCA Comdty", "LAA Comdty", "LBA Comdty", "LCA Comdty", "LHA Comdty", 
  "LLA Comdty", "LNA Comdty", "LPA Comdty", "LTA Comdty", "LXA Comdty", 
  "NGA Comdty", "O A Comdty", "PAA Comdty", "PLA Comdty", "S A Comdty", 
  "SBA Comdty", "SIA Comdty", "SMA Comdty", "W A Comdty", "XBWA Comdty",
  "LAA Comdty", "LPA Comdty", "LLA Comdty", "LNA Comdty", "LTA Comdty",
  "LXA Comdty"
)

`commodity futures data` <- pullit::pull_futures_market(
  source = "storethat", type = "term structure", 
  active_contract_tickers = `commodity futures tickers`,
  start = start, end = end, 
  TS_positions = 1L:2L, roll_type = "A", roll_days = 0L, roll_months = 0L,
  roll_adjustment = "N", file = storethat
)

`commodity futures data` <- pullit::get_data(`commodity futures data`) %>%
  dplyr::left_join(
    dplyr::select(
      pullit::get_term_structure_tickers(`commodity futures data`), 
      `active contract ticker`, ticker, `TS position`), 
    by = "ticker"
  ) %>% 
  dplyr::select(
    `active contract ticker`, ticker, `TS position`, field, date, value
  )

`commodity aggregate data` <- pullit::pull_futures_market(
  source = "storethat", type = "aggregate", 
  active_contract_tickers = `commodity futures tickers`, 
  start = start, end = end, file = storethat
)

`commodity CFTC tickers` <- `commodity futures tickers`[
  `commodity futures tickers` %in% tickers_cftc$`active contract ticker`
]

`commodity CFTC data` <- pullit::pull_futures_CFTC(
  source = "storethat", active_contract_tickers = `commodity CFTC tickers`, 
  start = start, end = end, file = storethat
)

`factor picks` <- readRDS(file = "explore/results/factor-picks.rds")

# aggregate CHP ####
`aggregate CHP` <- dplyr::left_join(
  `commodity CFTC data`@data, 
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
) %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(
    regime = ifelse(
      `aggregate CHP` < median(`aggregate CHP`), "backwardation", "contango"
    )
  ) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(period = NA) %>% 
  dplyr::select(period, year, week, regime)

years <- dplyr::mutate(
  dplyr::distinct(`commodity futures data`, date), year = lubridate::year(date), 
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



# between factor picks ####
combinations <- dplyr::distinct(
  `factor picks`, `asset pool`, `update frequency`, `return frequency`, 
  `ranking period`, `long threshold`, `short threshold`, factor, leg, period
) %>% as.data.frame()

## by year ####
years <- foreach(
  y = 1L:nrow(combinations), 
  .combine = dplyr::bind_rows
) %dopar% {
  library(magrittr); library(lubridate)
  
  message(paste(y, nrow(combinations), sep = "/"))
  
  picks <- dplyr::filter(
    `factor picks`,
    `asset pool` == combinations[y, "asset pool"],
    `update frequency` == combinations[y, "update frequency"],
    `return frequency` == combinations[y, "return frequency"],
    `ranking period` == combinations[y, "ranking period"],
    `long threshold` == combinations[y, "long threshold"],
    `short threshold` == combinations[y, "short threshold"],
    factor == combinations[y, "factor"],
    leg == combinations[y, "leg"],
    period == combinations[y, "period"]
  ) %>%
    dplyr::select(pick) %>% purrr::flatten_chr()
  
  start <- dplyr::filter(
    periods, period == combinations[y, "period"], bound == "start"
  ) %>% 
    dplyr::select(date) %>% 
    purrr::flatten_chr()
  
  end <- dplyr::filter(
    periods, period == combinations[y, "period"], bound == "end"
  ) %>% 
    dplyr::select(date) %>% 
    purrr::flatten_chr()
  
  data <- dplyr::filter(`commodity futures data`, `TS position` == 1L) %>%
    dplyr::select(`active contract ticker`, field, date, value) %>%
    dplyr::bind_rows(
      dplyr::rename(
        `commodity aggregate data`@data, `active contract ticker` = ticker
      )
    ) %>%
    dplyr::filter( 
      `active contract ticker` %in% picks,
      date >= as.Date(start), date <= as.Date(end)
    ) %>%
    dplyr::mutate(year = lubridate::year(date))
  
  # levels
  levels <- dplyr::group_by(data, field, year) %>%
    dplyr::do({
      
      data <- tidyr::spread(., `active contract ticker`, value) %>%
        dplyr::select(-c(field, date, year))
      
      tibble::tibble(
        correlations = tryCatch(
          { list(cor(data, use = "pairwise.complete.obs")) },
          error = function(e) { NA }
        )
      )
      
    }) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      period = "year", frequency = "day", type = "levels", regime = "all"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  # returns
  ## days
  days <- dplyr::group_by(data, `active contract ticker`, field) %>%
    dplyr::mutate(
      value = (value / dplyr::lag(value, 1L) - 1L),
      value = ifelse(is.infinite(value), NA, value)
    ) %>%
    dplyr::group_by(field, year) %>% dplyr::do({
      
      data <- tidyr::spread(., `active contract ticker`, value) %>%
        dplyr::select(-c(field, year, date))
      
      tibble::tibble(
        correlations = tryCatch(
          { list(cor(data, use = "pairwise.complete.obs")) },
          error = function(e) { NA }
        )
      )
      
    }) %>% dplyr::ungroup() %>%
    dplyr::mutate(
      period = "year", frequency = "day", type = "returns", regime = "all"
    ) %>%
    dplyr::select(
      type, frequency, field, period, year, regime, correlations
    )
  
  ## other frequencies
  others <- lapply(
    c("week", "month"),
    function(x){
      
      dplyr::mutate(data, unit = do.call(what = x, args = list(date))) %>%
        dplyr::group_by(`active contract ticker`, field, year, unit) %>%
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
        dplyr::group_by(`active contract ticker`, field) %>%
        dplyr::select(-unit) %>%
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L),
          value = ifelse(is.infinite(value), NA, value)
        ) %>%
        dplyr::group_by(field, year) %>% dplyr::do({
          
          data <- tidyr::spread(., `active contract ticker`, value) %>%
            dplyr::select(-c(field, year, date))
          
          tibble::tibble(
            correlations = tryCatch(
              { list(cor(data, use = "pairwise.complete.obs")) },
              error = function(e) { NA }
            )
          )
          
        }) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          period = "year", frequency = x, type = "returns", regime = "all"
        ) %>%
        dplyr::select(type, frequency, field, period, year, regime, correlations)
      
    }) %>%
    dplyr::bind_rows()
  
  whole <- dplyr::bind_rows(list(levels, days, others))
  
  data <- dplyr::left_join(
    data, dplyr::filter(`aggregate CHP regimes`, is.na(period)) %>%
      dplyr::select(date, regime),
    by = "date"
  ) %>%
    dplyr::filter(! is.na(regime))
  
  
  # levels
  levels <- dplyr::group_by(data, field, year, regime) %>% dplyr::do({
    
    data <- tidyr::spread(., `active contract ticker`, value) %>%
      dplyr::select(-c(field, year, regime, date))
    
    tibble::tibble(
      correlations = tryCatch(
        { list(cor(data, use = "pairwise.complete.obs")) },
        error = function(e) { NA }
      )
    )
    
  }) %>% dplyr::ungroup() %>%
    dplyr::mutate(
      period = "year", frequency = "day", type = "levels"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  # returns
  ## days
  days <- dplyr::group_by(data, `active contract ticker`, field) %>%
    dplyr::mutate(
      value = (value / dplyr::lag(value, 1L) - 1L),
      value = ifelse(is.infinite(value), NA, value)
    ) %>%
    dplyr::group_by(field, year, regime) %>% dplyr::do({
      
      data <- tidyr::spread(., `active contract ticker`, value) %>%
        dplyr::select(-c(field, year, regime, date))
      
      tibble::tibble(
        correlations = tryCatch(
          { list(cor(data, use = "pairwise.complete.obs")) },
          error = function(e) { NA }
        )
      )
      
    }) %>% dplyr::ungroup() %>%
    dplyr::mutate(period = "year", frequency = "day", type = "returns") %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  ## other frequencies
  others <- lapply(
    c("week", "month"),
    function(x){
      dplyr::mutate(data, unit = do.call(what = x, args = list(date))) %>%
        dplyr::group_by(`active contract ticker`, field, year, unit) %>%
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
        dplyr::group_by(`active contract ticker`, field) %>%
        dplyr::select(-unit) %>%
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L),
          value = ifelse(is.infinite(value), NA, value)
        ) %>%
        dplyr::group_by(field, year, regime) %>% dplyr::do({
          
          data <- tidyr::spread(., `active contract ticker`, value) %>%
            dplyr::select(-c(field, year, regime, date))
          
          tibble::tibble(
            correlations = tryCatch(
              { list(cor(data, use = "pairwise.complete.obs")) },
              error = function(e) { NA }
            )
          )
          
        }) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(period = "year", frequency = x, type = "returns") %>%
        dplyr::select(type, frequency, field, period, year, regime, correlations)
      
    }) %>% dplyr::bind_rows()
  
  regimes <- dplyr::bind_rows(list(levels, days, others))
  
  dplyr::bind_rows(list(whole, regimes)) %>%
    dplyr::mutate(
      `asset pool` = combinations[y, "asset pool"],
      `update frequency` = combinations[y, "update frequency"],
      `return frequency` = combinations[y, "return frequency"],
      `ranking period` = combinations[y, "ranking period"],
      `long threshold` = combinations[y, "long threshold"],
      `short threshold` = combinations[y, "short threshold"],
      factor = combinations[y, "factor"],
      leg = combinations[y, "leg"],
    ) %>%
    dplyr::select(
      `asset pool`, `update frequency`, `return frequency`, `ranking period`,
      `long threshold`, `short threshold`, factor, leg, dplyr::everything()
    )
}


## by period ####

subperiods <- foreach(
  y = 1L:nrow(combinations), 
  .combine = dplyr::bind_rows
) %dopar% {
  library(magrittr); library(lubridate)
  
  picks <- dplyr::filter(
    `factor picks`, `asset pool` == combinations[y, "asset pool"], 
    `update frequency` == combinations[y, "update frequency"], 
    `return frequency` == combinations[y, "return frequency"],
    `ranking period` == combinations[y, "ranking period"], 
    `long threshold` == combinations[y, "long threshold"], 
    `short threshold` == combinations[y, "short threshold"],
    factor == combinations[y, "factor"], leg == combinations[y, "leg"], 
    period == combinations[y, "period"]
  ) %>% 
    dplyr::select(pick) %>% 
    purrr::flatten_chr()
  
  start <- dplyr::filter(
    periods, period == combinations[y, "period"], bound == "start"
  ) %>% 
    dplyr::select(date) %>% 
    purrr::flatten_chr()
  
  end <- dplyr::filter(
    periods, period == combinations[y, "period"], bound == "end"
  ) %>% 
    dplyr::select(date) %>% 
    purrr::flatten_chr()
  
  data <- dplyr::filter(`commodity futures data`, `TS position` == 1L) %>% 
    dplyr::select(`active contract ticker`, field, date, value) %>% 
    dplyr::bind_rows(
      dplyr::rename(
        `commodity aggregate data`@data, `active contract ticker` = ticker
      )
    ) %>%
    dplyr::filter(
      `active contract ticker` %in% picks, 
      date >= as.Date(start), date <= as.Date(end)
    ) %>% 
    dplyr::mutate(year = lubridate::year(date))
  
  # levels 
  levels <- dplyr::group_by(data, field) %>% 
    dplyr::do({
      
      data <- tidyr::spread(., `active contract ticker`, value) %>% 
        dplyr::select(-c(field, date, year))
      
      tibble::tibble(
        correlations = tryCatch(
          { list(cor(data, use = "pairwise.complete.obs")) }, 
          error = function(e) { NA }
        )
      )
      
    }) %>% dplyr::ungroup() %>% 
    dplyr::mutate(
      period = combinations[y, "period"], year = NA, frequency = "day", 
      type = "levels", regime = "all"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  # returns 
  ## days
  days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
    dplyr::mutate(
      value = (value / dplyr::lag(value, 1L) - 1L), 
      value = ifelse(is.infinite(value), NA, value)
    ) %>%
    dplyr::group_by(field) %>% dplyr::do({
      
      data <- tidyr::spread(., `active contract ticker`, value) %>% 
        dplyr::select(-c(field, year, date))
      tibble::tibble(
        correlations = tryCatch(
          { list(cor(data, use = "pairwise.complete.obs")) }, 
          error = function(e) { NA }
        )
      )
      
    }) %>% dplyr::ungroup() %>% 
    dplyr::mutate(
      period = combinations[y, "period"], 
      year = NA, frequency = "day", type = "returns", regime = "all"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  ## other frequencies
  others <- lapply(
    c("week", "month"), 
    function(x){
      dplyr::mutate(data, unit = do.call(what = x, args = list(date))) %>% 
        dplyr::group_by(`active contract ticker`, field, year, unit) %>% 
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
        dplyr::group_by(`active contract ticker`, field) %>% 
        dplyr::select(-unit) %>%
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L), 
          value = ifelse(is.infinite(value), NA, value)
        ) %>%
        dplyr::group_by(field) %>% dplyr::do({
          
          data <- tidyr::spread(., `active contract ticker`, value) %>% 
            dplyr::select(-c(field, year, date))
          
          tibble::tibble(
            correlations = tryCatch(
              { list(cor(data, use = "pairwise.complete.obs")) }, 
              error = function(e) { NA }
            )
          )
          
        }) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = combinations[y, "period"], year = NA, frequency = x, 
          type = "returns", regime = "all"
        ) %>%
        dplyr::select(type, frequency, field, period, year, regime, correlations)
    }) %>% 
    dplyr::bind_rows()
  
  whole <- dplyr::bind_rows(list(levels, days, others))
  
  data <- dplyr::left_join(
    data, dplyr::filter(`aggregate CHP regimes`, ! is.na(period)) %>% 
      dplyr::select(date, regime), 
    by = "date"
  ) %>% 
    dplyr::filter(! is.na(regime))
  
  # levels 
  levels <- dplyr::group_by(data, field, regime) %>% dplyr::do({
    
    data <- tidyr::spread(., `active contract ticker`, value) %>% 
      dplyr::select(-c(field, year, regime, date))
    tibble::tibble(
      correlations = tryCatch(
        { list(cor(data, use = "pairwise.complete.obs")) }, 
        error = function(e) { NA }
      )
    )
    
  }) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      period = combinations[y, "period"], year = NA, 
      frequency = "day", type = "levels"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  # returns 
  ## days
  days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
    dplyr::mutate(
      value = (value / dplyr::lag(value, 1L) - 1L), 
      value = ifelse(is.infinite(value), NA, value)
    ) %>%
    dplyr::group_by(field, regime) %>% dplyr::do({
      
      data <- tidyr::spread(., `active contract ticker`, value) %>% 
        dplyr::select(-c(field, year, regime, date))
      tibble::tibble(
        correlations = tryCatch(
          { list(cor(data, use = "pairwise.complete.obs")) }, 
          error = function(e) { NA }
        )
      )
      
    }) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      period = combinations[y, "period"], year = NA, 
      frequency = "day", type = "returns"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  ## other frequencies
  others <- lapply(
    c("week", "month"), 
    function(x){
      dplyr::mutate(data, unit = do.call(what = x, args = list(date))) %>% 
        dplyr::group_by(`active contract ticker`, field, year, unit) %>% 
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
        dplyr::group_by(`active contract ticker`, field) %>% 
        dplyr::select(-unit) %>%
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L), 
          value = ifelse(is.infinite(value), NA, value)
        ) %>%
        dplyr::group_by(field, regime) %>% dplyr::do({
          
          data <- tidyr::spread(., `active contract ticker`, value) %>% 
            dplyr::select(-c(field, year, regime, date))
          tibble::tibble(
            correlations = tryCatch(
              { list(cor(data, use = "pairwise.complete.obs")) }, 
              error = function(e) { NA }
            )
          )
          
        }) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = combinations[y, "period"], year = NA, 
          frequency = x, type = "returns"
        ) %>%
        dplyr::select(type, frequency, field, period, year, regime, correlations)
      
    }) %>% 
    dplyr::bind_rows()
  
  regimes <- dplyr::bind_rows(list(levels, days, others))
  
  dplyr::bind_rows(list(whole, regimes)) %>%
    dplyr::mutate(
      `asset pool` = combinations[y, "asset pool"],
      `update frequency` = combinations[y, "update frequency"],
      `return frequency` = combinations[y, "return frequency"],
      `ranking period` = combinations[y, "ranking period"],
      `long threshold` = combinations[y, "long threshold"],
      `short threshold` = combinations[y, "short threshold"],
      factor = combinations[y, "factor"],
      leg = combinations[y, "leg"],
    ) %>%
    dplyr::select(
      `asset pool`, `update frequency`, `return frequency`, `ranking period`,
      `long threshold`, `short threshold`, factor, leg, dplyr::everything()
    )
}

`factor picks` <- dplyr::bind_rows(list(years, subperiods))


# between US commodities ####
tickers <- dplyr::distinct(`commodity futures data`, `active contract ticker`) %>% 
  purrr::flatten_chr()
tickers <- dplyr::left_join(
  dplyr::filter(tickers_futures, ticker %in% tickers) %>% 
    dplyr::select(ticker, MIC),
  dplyr::select(exchanges, MIC, country), 
  by = "MIC"
) %>% 
  dplyr::filter(
    ticker %in% `commodity futures tickers`, country == "US"
  ) %>% dplyr::select(ticker) %>% 
  purrr::flatten_chr()

years <- dplyr::distinct(`commodity futures data`, date) %>% 
  dplyr::mutate(year = lubridate::year(date)) %>% 
  dplyr::distinct(year) %>% 
  purrr::flatten_dbl() %>% 
  as.integer()

years <- foreach(
  y = years,
  .combine = dplyr::bind_rows
) %dopar% {
  library(magrittr); library(lubridate)
  
  data <- dplyr::filter(`commodity futures data`, `TS position` == 1L) %>%
    dplyr::select(`active contract ticker`, field, date, value) %>%
    dplyr::bind_rows(dplyr::rename(
      `commodity aggregate data`@data, `active contract ticker` = ticker)
    ) %>%
    dplyr::mutate(year = lubridate::year(date)) %>%
    dplyr::filter(
      `active contract ticker` %in% tickers,
      year == y
    ) 
  
  # levels
  levels <- dplyr::group_by(data, field) %>% dplyr::do({
    
    data <- tidyr::spread(., `active contract ticker`, value) %>%
      dplyr::select(-c(field, date, year))
    tibble::tibble(correlations = tryCatch(
      { list(cor(data, use = "pairwise.complete.obs")) },
      error = function(e) { NA }
    )
    )
    
  }) %>% dplyr::ungroup() %>%
    dplyr::mutate(
      period = "year", year = y, frequency = "day", type = "levels", regime = "all"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  # returns
  ## days
  days <- dplyr::group_by(data, `active contract ticker`, field) %>%
    dplyr::mutate(
      value = (value / dplyr::lag(value, 1L) - 1L),
      value = ifelse(is.infinite(value), NA, value)
    ) %>%
    dplyr::group_by(field) %>% dplyr::do({
      
      data <- tidyr::spread(., `active contract ticker`, value) %>%
        dplyr::select(-c(field, year, date))
      
      tibble::tibble(correlations = tryCatch(
        { list(cor(data, use = "pairwise.complete.obs")) },
        error = function(e) { NA }
      )
      )
      
    }) %>% dplyr::ungroup() %>%
    dplyr::mutate(
      period = "year", year = y, frequency = "day", type = "returns",
      regime = "all"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  ## other frequencies
  others <- lapply(
    c("week", "month"),
    function(x){
      dplyr::mutate(data, unit = do.call(what = x, args = list(date))) %>%
        dplyr::group_by(`active contract ticker`, field, year, unit) %>%
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
        dplyr::group_by(`active contract ticker`, field) %>%
        dplyr::select(-unit) %>%
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L),
          value = ifelse(is.infinite(value), NA, value)
        ) %>%
        dplyr::group_by(field) %>% dplyr::do({
          
          data <- tidyr::spread(., `active contract ticker`, value) %>%
            dplyr::select(-c(field, year, date))
          tibble::tibble(correlations = tryCatch(
            { list(cor(data, use = "pairwise.complete.obs")) },
            error = function(e) { NA }
          )
          )
          
        }) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          period = "year", year = y, frequency = x,
          type = "returns", regime = "all"
        ) %>%
        dplyr::select(type, frequency, field, period, year, regime, correlations)
    }) %>%
    dplyr::bind_rows()
  
  whole <- dplyr::bind_rows(list(levels, days, others))
  
  data <- dplyr::left_join(
    data, 
    dplyr::filter(`aggregate CHP regimes`, is.na(period)) %>%
      dplyr::select(date, regime),
    by = "date"
  ) %>%
    dplyr::filter(! is.na(regime))
  
  # levels
  levels <- dplyr::group_by(data, field, regime) %>% dplyr::do({
    
    data <- tidyr::spread(., `active contract ticker`, value) %>%
      dplyr::select(-c(field, year, regime, date))
    
    tibble::tibble(correlations = tryCatch(
      { list(cor(data, use = "pairwise.complete.obs")) },
      error = function(e) { NA }
    )
    )
    
  }) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      period = "year", year = y, frequency = "day", type = "levels"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  # returns
  ## days
  days <- dplyr::group_by(data, `active contract ticker`, field) %>%
    dplyr::mutate(
      value = (value / dplyr::lag(value, 1L) - 1L),
      value = ifelse(is.infinite(value), NA, value)
    ) %>%
    dplyr::group_by(field, regime) %>% dplyr::do({
      
      data <- tidyr::spread(., `active contract ticker`, value) %>%
        dplyr::select(-c(field, year, regime, date))
      tibble::tibble(correlations = tryCatch(
        { list(cor(data, use = "pairwise.complete.obs")) },
        error = function(e) { NA }
      )
      )
      
    }) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(period = "year", year = y, frequency = "day", type = "returns") %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  ## other frequencies
  others <- lapply(
    c("week", "month"),
    function(x){
      dplyr::mutate(data, unit = do.call(what = x, args = list(date))) %>%
        dplyr::group_by(`active contract ticker`, field, year, unit) %>%
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
        dplyr::group_by(`active contract ticker`, field) %>%
        dplyr::select(-unit) %>%
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L),
          value = ifelse(is.infinite(value), NA, value)
        ) %>%
        dplyr::group_by(field, regime) %>% dplyr::do({
          
          data <- tidyr::spread(., `active contract ticker`, value) %>%
            dplyr::select(-c(field, year, regime, date))
          tibble::tibble(correlations = tryCatch(
            { list(cor(data, use = "pairwise.complete.obs")) },
            error = function(e) { NA }
          )
          )
          
        }) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          period = "year", year = y, frequency = x, type = "returns"
        ) %>%
        dplyr::select(type, frequency, field, period, year, regime, correlations)
      
    }) %>% dplyr::bind_rows()
  
  regimes <- dplyr::bind_rows(list(levels, days, others))
  dplyr::bind_rows(list(whole, regimes))
}



subperiods <- foreach(
  y = unique(periods$period), 
  .combine = dplyr::bind_rows
) %dopar% {
  library(magrittr); library(lubridate)
  
  start <- dplyr::filter(periods, period == y, bound == "start") %>% 
    dplyr::select(date) %>% purrr::flatten_chr()
  end <- dplyr::filter(periods, period == y, bound == "end") %>% 
    dplyr::select(date) %>% purrr::flatten_chr()
  
  data <- dplyr::filter(`commodity futures data`, `TS position` == 1L) %>% 
    dplyr::select(`active contract ticker`, field, date, value) %>% 
    dplyr::bind_rows(
      dplyr::rename(
        `commodity aggregate data`@data, `active contract ticker` = ticker)
    ) %>%
    dplyr::filter(
      `active contract ticker` %in% tickers, 
      date >= as.Date(start), date <= as.Date(end)
    ) %>% 
    dplyr::mutate(year = lubridate::year(date))
  
  # levels 
  levels <- dplyr::group_by(data, field) %>% dplyr::do({
    
    data <- tidyr::spread(., `active contract ticker`, value) %>% 
      dplyr::select(-c(field, date, year))
    
    tibble::tibble(correlations = tryCatch(
      { list(cor(data, use = "pairwise.complete.obs")) }, 
      error = function(e) { NA }
    )
    )
    
  }) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      period = y, year = NA, frequency = "day", type = "levels", regime = "all"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  # returns 
  ## days
  days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
    dplyr::mutate(
      value = (value / dplyr::lag(value, 1L) - 1L), 
      value = ifelse(is.infinite(value), NA, value)
    ) %>%
    dplyr::group_by(field) %>% dplyr::do({
      
      data <- tidyr::spread(., `active contract ticker`, value) %>% 
        dplyr::select(-c(field, year, date))
      
      tibble::tibble(
        correlations = tryCatch(
          { list(cor(data, use = "pairwise.complete.obs")) }, 
          error = function(e) { NA }
        )
      )
      
    }) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      period = y, year = NA, frequency = "day", type = "returns", regime = "all"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  ## other frequencies
  others <- lapply(
    c("week", "month"), 
    function(x){
      dplyr::mutate(data, unit = do.call(what = x, args = list(date))) %>% 
        dplyr::group_by(`active contract ticker`, field, year, unit) %>% 
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
        dplyr::group_by(`active contract ticker`, field) %>% 
        dplyr::select(-unit) %>%
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L), 
          value = ifelse(is.infinite(value), NA, value)
        ) %>%
        dplyr::group_by(field) %>% dplyr::do({
          
          data <- tidyr::spread(., `active contract ticker`, value) %>% 
            dplyr::select(-c(field, year, date))
          tibble::tibble(
            correlations = tryCatch(
              { list(cor(data, use = "pairwise.complete.obs")) }, 
              error = function(e) { NA }
            )
          )
          
        }) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = y, year = NA, frequency = x, type = "returns", regime = "all"
        ) %>%
        dplyr::select(type, frequency, field, period, year, regime, correlations)
    }) %>% 
    dplyr::bind_rows()
  
  whole <- dplyr::bind_rows(list(levels, days, others))
  
  data <- dplyr::left_join(
    data, dplyr::filter(
      `aggregate CHP regimes`, ! is.na(period)) %>% 
      dplyr::select(date, regime), 
    by = "date"
  ) %>% 
    dplyr::filter(! is.na(regime))
  
  # levels 
  levels <- dplyr::group_by(data, field, regime) %>% dplyr::do({
    
    data <- tidyr::spread(., `active contract ticker`, value) %>% 
      dplyr::select(-c(field, year, regime, date))
    
    tibble::tibble(
      correlations = tryCatch(
        { list(cor(data, use = "pairwise.complete.obs")) }, 
        error = function(e) { NA }
      )
    )
    
  }) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      period = y, year = NA, frequency = "day", type = "levels"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  # returns 
  ## days
  days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
    dplyr::mutate(
      value = (value / dplyr::lag(value, 1L) - 1L), 
      value = ifelse(is.infinite(value), NA, value)
    ) %>%
    dplyr::group_by(field, regime) %>% dplyr::do({
      
      data <- tidyr::spread(., `active contract ticker`, value) %>% 
        dplyr::select(-c(field, year, regime, date))
      tibble::tibble(correlations = tryCatch(
        { list(cor(data, use = "pairwise.complete.obs")) }, 
        error = function(e) { NA }
      )
      )
      
    }) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(period = y, year = NA, frequency = "day", type = "returns") %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  ## other frequencies
  others <- lapply(
    c("week", "month"), 
    function(x){
      dplyr::mutate(data, unit = do.call(what = x, args = list(date))) %>% 
        dplyr::group_by(`active contract ticker`, field, year, unit) %>% 
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
        dplyr::group_by(`active contract ticker`, field) %>% 
        dplyr::select(-unit) %>%
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L), 
          value = ifelse(is.infinite(value), NA, value)
        ) %>%
        dplyr::group_by(field, regime) %>% dplyr::do({
          
          data <- tidyr::spread(., `active contract ticker`, value) %>% 
            dplyr::select(-c(field, year, regime, date))
          
          tibble::tibble(correlations = tryCatch(
            { list(cor(data, use = "pairwise.complete.obs")) }, 
            error = function(e) { NA }
          )
          )
          
        }) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = y, year = NA, frequency = x, type = "returns"
        ) %>%
        dplyr::select(
          type, frequency, field, period, year, regime, correlations
        )
    }) %>% 
    dplyr::bind_rows()
  
  regimes <- dplyr::bind_rows(list(levels, days, others))
  dplyr::bind_rows(list(whole, regimes))
}

`US commodities` <- dplyr::bind_rows(list(years, subperiods))



# between US commodities (excluding metals) ####
tickers <- dplyr::distinct(`commodity futures data`, `active contract ticker`) %>% 
  purrr::flatten_chr()

tickers <- dplyr::left_join(
  dplyr::filter(tickers_futures, ticker %in% tickers) %>% 
    dplyr::select(ticker, sector, MIC), 
  dplyr::select(exchanges, MIC, country), 
  by = "MIC"
) %>% 
  dplyr::filter(
    ticker %in% `commodity futures tickers`, sector != "metals", country == "US"
  ) %>% 
  dplyr::select(ticker) %>% 
  purrr::flatten_chr()

years <- dplyr::distinct(`commodity futures data`, date) %>% 
  dplyr::mutate(year = lubridate::year(date)) %>% 
  dplyr::distinct(year) %>% 
  purrr::flatten_dbl() %>%
  as.integer()

years <- foreach(
  y = years, 
  .combine = dplyr::bind_rows
) %dopar% {
  library(magrittr); library(lubridate)
  
  data <- dplyr::filter(`commodity futures data`, `TS position` == 1L) %>% 
    dplyr::select(`active contract ticker`, field, date, value) %>% 
    dplyr::bind_rows(
      dplyr::rename(
        `commodity aggregate data`@data, 
        `active contract ticker` = ticker
      )
    ) %>% 
    dplyr::mutate(year = lubridate::year(date)) %>%
    dplyr::filter(
      `active contract ticker` %in% tickers,
      year == y
    ) 
  
  
  # levels 
  levels <- dplyr::group_by(data, field) %>% dplyr::do({
    
    data <- tidyr::spread(., `active contract ticker`, value) %>% 
      dplyr::select(-c(field, date, year))
    tibble::tibble(
      correlations = tryCatch(
        { list(cor(data, use = "pairwise.complete.obs")) }, 
        error = function(e) { NA }
      )
    )
    
  }) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      period = "year", year = y, frequency = "day", type = "levels", regime = "all"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  # returns 
  ## days
  days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
    dplyr::mutate(
      value = (value / dplyr::lag(value, 1L) - 1L), 
      value = ifelse(is.infinite(value), NA, value)
    ) %>%
    dplyr::group_by(field) %>% dplyr::do({
      
      data <- tidyr::spread(., `active contract ticker`, value) %>% 
        dplyr::select(-c(field, year, date))
      
      tibble::tibble(correlations = tryCatch(
        { list(cor(data, use = "pairwise.complete.obs")) }, 
        error = function(e) { NA }
      )
      )
      
    }) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      period = "year", year = y, frequency = "day", 
      type = "returns", regime = "all"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  ## other frequencies
  others <- lapply(
    c("week", "month"), 
    function(x){
      dplyr::mutate(data, unit = do.call(what = x, args = list(date))) %>% 
        dplyr::group_by(`active contract ticker`, field, year, unit) %>% 
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
        dplyr::group_by(`active contract ticker`, field) %>% 
        dplyr::select(-unit) %>%
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L), 
          value = ifelse(is.infinite(value), NA, value)
        ) %>%
        dplyr::group_by(field) %>% dplyr::do({
          
          data <- tidyr::spread(., `active contract ticker`, value) %>% 
            dplyr::select(-c(field, year, date))
          tibble::tibble(
            correlations = tryCatch(
              { list(cor(data, use = "pairwise.complete.obs")) }, 
              error = function(e) { NA }
            )
          )
          
        }) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = "year", year = y, frequency = x, type = "returns", 
          regime = "all"
        ) %>%
        dplyr::select(type, frequency, field, period, year, regime, correlations)
    }) %>% 
    dplyr::bind_rows()
  
  whole <- dplyr::bind_rows(list(levels, days, others))
  
  data <- dplyr::left_join(
    data, 
    dplyr::filter(
      `aggregate CHP regimes`, is.na(period)) %>% 
      dplyr::select(date, regime), 
    by = "date") %>% 
    dplyr::filter(! is.na(regime))
  
  # levels 
  levels <- dplyr::group_by(data, field, regime) %>% dplyr::do({
    
    data <- tidyr::spread(., `active contract ticker`, value) %>% 
      dplyr::select(-c(field, year, regime, date))
    tibble::tibble(
      correlations = tryCatch(
        { list(cor(data, use = "pairwise.complete.obs")) }, 
        error = function(e) { NA }
      )
    )
    
  }) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(
      period = "year", year = y, frequency = "day", type = "levels"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  # returns 
  ## days
  days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
    dplyr::mutate(
      value = (value / dplyr::lag(value, 1L) - 1L), 
      value = ifelse(is.infinite(value), NA, value)
    ) %>%
    dplyr::group_by(field, regime) %>% dplyr::do({
      
      data <- tidyr::spread(., `active contract ticker`, value) %>% 
        dplyr::select(-c(field, year, regime, date))
      tibble::tibble(
        correlations = tryCatch(
          { list(cor(data, use = "pairwise.complete.obs")) }, 
          error = function(e) { NA }
        )
      )
      
    }) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(period = "year", year = y, frequency = "day", type = "returns") %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  ## other frequencies
  others <- lapply(
    c("week", "month"), 
    function(x){
      dplyr::mutate(data, unit = do.call(what = x, args = list(date))) %>% 
        dplyr::group_by(`active contract ticker`, field, year, unit) %>% 
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
        dplyr::group_by(`active contract ticker`, field) %>% dplyr::select(-unit) %>%
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L), 
          value = ifelse(is.infinite(value), NA, value)
        ) %>%
        dplyr::group_by(field, regime) %>% dplyr::do({
          
          data <- tidyr::spread(., `active contract ticker`, value) %>% 
            dplyr::select(-c(field, year, regime, date))
          tibble::tibble(
            correlations = tryCatch(
              { list(cor(data, use = "pairwise.complete.obs")) }, 
              error = function(e) { NA }
            )
          )
          
        }) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = "year", year = y, frequency = x, type = "returns"
        ) %>%
        dplyr::select(type, frequency, field, period, year, regime, correlations)
      
    }) %>% 
    dplyr::bind_rows()
  
  regimes <- dplyr::bind_rows(list(levels, days, others))
  
  dplyr::bind_rows(list(whole, regimes))
}

subperiods <- foreach(
  y = unique(periods$period), 
  .combine = dplyr::bind_rows
) %dopar% {
  library(magrittr); library(lubridate)
  
  start <- dplyr::filter(periods, period == y, bound == "start") %>% 
    dplyr::select(date) %>% purrr::flatten_chr()
  end <- dplyr::filter(periods, period == y, bound == "end") %>% 
    dplyr::select(date) %>% purrr::flatten_chr()
  
  data <- dplyr::filter(`commodity futures data`, `TS position` == 1L) %>%
    dplyr::select(`active contract ticker`, field, date, value) %>% 
    dplyr::bind_rows(dplyr::rename(
      `commodity aggregate data`@data, `active contract ticker` = ticker)
    ) %>%
    dplyr::filter(
      `active contract ticker` %in% tickers, 
      date >= as.Date(start), date <= as.Date(end)
    ) %>% 
    dplyr::mutate(year = lubridate::year(date))
  
  # levels 
  levels <- dplyr::group_by(data, field) %>% dplyr::do({
    
    data <- tidyr::spread(., `active contract ticker`, value) %>% 
      dplyr::select(-c(field, date, year))
    tibble::tibble(
      correlations = tryCatch(
        { list(cor(data, use = "pairwise.complete.obs")) },
        error = function(e) { NA }
      )
    )
    
  }) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      period = y, year = NA, frequency = "day", type = "levels", regime = "all"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  # returns 
  ## days
  days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
    dplyr::mutate(
      value = (value / dplyr::lag(value, 1L) - 1L), 
      value = ifelse(is.infinite(value), NA, value)
    ) %>%
    dplyr::group_by(field) %>% dplyr::do({
      
      data <- tidyr::spread(., `active contract ticker`, value) %>% 
        dplyr::select(-c(field, year, date))
      
      tibble::tibble(correlations = tryCatch(
        { list(cor(data, use = "pairwise.complete.obs")) }, 
        error = function(e) { NA }
      )
      )
      
    }) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      period = y, year = NA, frequency = "day", type = "returns", 
      regime = "all"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  ## other frequencies
  others <- lapply(
    c("week", "month"), 
    function(x){
      dplyr::mutate(data, unit = do.call(what = x, args = list(date))) %>% 
        dplyr::group_by(`active contract ticker`, field, year, unit) %>% 
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
        dplyr::group_by(`active contract ticker`, field) %>% 
        dplyr::select(-unit) %>%
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L), 
          value = ifelse(is.infinite(value), NA, value)
        ) %>%
        dplyr::group_by(field) %>% dplyr::do({
          
          data <- tidyr::spread(., `active contract ticker`, value) %>% 
            dplyr::select(-c(field, year, date))
          tibble::tibble(
            correlations = tryCatch(
              { list(cor(data, use = "pairwise.complete.obs")) }, 
              error = function(e) { NA }
            )
          )
          
        }) %>%
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = y, year = NA, frequency = x, type = "returns", regime = "all"
        ) %>%
        dplyr::select(type, frequency, field, period, year, regime, correlations)
    }) %>% 
    dplyr::bind_rows()
  
  whole <- dplyr::bind_rows(list(levels, days, others))
  
  data <- dplyr::left_join(
    data, dplyr::filter(
      `aggregate CHP regimes`, ! is.na(period)) %>% 
      dplyr::select(date, regime), 
    by = "date"
  ) %>% 
    dplyr::filter(! is.na(regime))
  
  # levels 
  levels <- dplyr::group_by(data, field, regime) %>% dplyr::do({
    
    data <- tidyr::spread(., `active contract ticker`, value) %>% 
      dplyr::select(-c(field, year, regime, date))
    tibble::tibble(
      correlations = tryCatch(
        { list(cor(data, use = "pairwise.complete.obs")) }, 
        error = function(e) { NA }
      )
    )
    
  }) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      period = y, year = NA, frequency = "day", type = "levels"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  # returns 
  ## days
  days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
    dplyr::mutate(
      value = (value / dplyr::lag(value, 1L) - 1L), 
      value = ifelse(is.infinite(value), NA, value)
    ) %>%
    dplyr::group_by(field, regime) %>% dplyr::do({
      
      data <- tidyr::spread(., `active contract ticker`, value) %>% 
        dplyr::select(-c(field, year, regime, date))
      tibble::tibble(
        correlations = tryCatch(
          { list(cor(data, use = "pairwise.complete.obs")) }, 
          error = function(e) { NA }
        )
      )
      
    }) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(period = y, year = NA, frequency = "day", type = "returns") %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  ## other frequencies
  others <- lapply(
    c("week", "month"), 
    function(x){
      
      dplyr::mutate(data, unit = do.call(what = x, args = list(date))) %>% 
        dplyr::group_by(`active contract ticker`, field, year, unit) %>%
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
        dplyr::group_by(`active contract ticker`, field) %>% 
        dplyr::select(-unit) %>%
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L), 
          value = ifelse(is.infinite(value), NA, value)
        ) %>%
        dplyr::group_by(field, regime) %>% dplyr::do({
          
          data <- tidyr::spread(., `active contract ticker`, value) %>% 
            dplyr::select(-c(field, year, regime, date))
          
          tibble::tibble(
            correlations = tryCatch(
              { list(cor(data, use = "pairwise.complete.obs")) }, 
              error = function(e) { NA }
            )
          )
          
        }) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = y, year = NA, frequency = x, type = "returns"
        ) %>%
        dplyr::select(type, frequency, field, period, year, regime, correlations)
    }) %>% 
    dplyr::bind_rows()
  
  regimes <- dplyr::bind_rows(list(levels, days, others))
  dplyr::bind_rows(list(whole, regimes))
}

`US commodities (excluding metals)` <- dplyr::bind_rows(list(years, subperiods))






# between US metals ####
tickers <- dplyr::distinct(`commodity futures data`, `active contract ticker`) %>% 
  purrr::flatten_chr()
tickers <- dplyr::left_join(
  dplyr::filter(tickers_futures, ticker %in% tickers) %>% 
    dplyr::select(ticker, sector, MIC),
  dplyr::select(exchanges, MIC, country), 
  by = "MIC") %>% 
  dplyr::filter(
    ticker %in% `commodity futures tickers`, sector == "metals", country == "US"
  ) %>% 
  dplyr::select(ticker) %>% 
  purrr::flatten_chr()

years <- dplyr::distinct(`commodity futures data`, date) %>% 
  dplyr::mutate(year = lubridate::year(date)) %>% 
  dplyr::distinct(year) %>% 
  purrr::flatten_dbl() %>% 
  as.integer()

years <- foreach(
  y = years, 
  .combine = dplyr::bind_rows
) %dopar% {
  library(magrittr); library(lubridate)
  
  data <- dplyr::filter(`commodity futures data`, `TS position` == 1L) %>% 
    dplyr::select(`active contract ticker`, field, date, value) %>% 
    dplyr::bind_rows(
      dplyr::rename(
        `commodity aggregate data`@data, `active contract ticker` = ticker
      )
    ) %>% 
    dplyr::mutate(year = lubridate::year(date)) %>%
    dplyr::filter(
      `active contract ticker` %in% tickers,
      year == y
    ) 
  
  # levels 
  levels <- dplyr::group_by(data, field) %>% 
    dplyr::do({
      data <- tidyr::spread(., `active contract ticker`, value) %>% 
        dplyr::select(-c(field, date, year))
      tibble::tibble(
        correlations = tryCatch(
          { list(cor(data, use = "pairwise.complete.obs")) }, 
          error = function(e) { NA }
        )
      )
    }) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      period = "year", year = y, frequency = "day", type = "levels", 
      regime = "all"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  # returns 
  ## days
  days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
    dplyr::mutate(
      value = (value / dplyr::lag(value, 1L) - 1L), 
      value = ifelse(is.infinite(value), NA, value)
    ) %>%
    dplyr::group_by(field) %>% 
    dplyr::do({
      data <- tidyr::spread(., `active contract ticker`, value) %>% 
        dplyr::select(-c(field, year, date))
      tibble::tibble(
        correlations = tryCatch(
          { list(cor(data, use = "pairwise.complete.obs")) }, 
          error = function(e) { NA }
        )
      )
      
    }) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      period = "year", year = y, frequency = "day", type = "returns", 
      regime = "all"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  ## other frequencies
  others <- lapply(c("week", "month"), function(x){
    dplyr::mutate(data, unit = do.call(what = x, args = list(date))) %>% 
      dplyr::group_by(`active contract ticker`, field, year, unit) %>% 
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
      dplyr::group_by(`active contract ticker`, field) %>% 
      dplyr::select(-unit) %>%
      dplyr::mutate(
        value = (value / dplyr::lag(value, 1L) - 1L), 
        value = ifelse(is.infinite(value), NA, value)
      ) %>%
      dplyr::group_by(field) %>% 
      dplyr::do({
        
        data <- tidyr::spread(., `active contract ticker`, value) %>% 
          dplyr::select(-c(field, year, date))
        
        tibble::tibble(
          correlations = tryCatch(
            { list(cor(data, use = "pairwise.complete.obs")) }, 
            error = function(e) { NA }
          )
        )
      }) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        period = "year", year = y, frequency = x, type = "returns", regime = "all"
      ) %>%
      dplyr::select(type, frequency, field, period, year, regime, correlations)
  }) %>% 
    dplyr::bind_rows()
  
  whole <- dplyr::bind_rows(list(levels, days, others))
  
  data <- dplyr::left_join(
    data, dplyr::filter(`aggregate CHP regimes`, is.na(period)) %>% 
      dplyr::select(date, regime), by = "date") %>% 
    dplyr::filter(! is.na(regime))
  
  # levels 
  levels <- dplyr::group_by(data, field, regime) %>% 
    dplyr::do({
      data <- tidyr::spread(., `active contract ticker`, value) %>% 
        dplyr::select(-c(field, year, regime, date))
      tibble::tibble(
        correlations = tryCatch(
          { list(cor(data, use = "pairwise.complete.obs")) }, 
          error = function(e) { NA }
        )
      )
    }) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      period = "year", year = y, frequency = "day", type = "levels"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  # returns 
  ## days
  days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
    dplyr::mutate(
      value = (value / dplyr::lag(value, 1L) - 1L), 
      value = ifelse(is.infinite(value), NA, value)
    ) %>%
    dplyr::group_by(field, regime) %>% 
    dplyr::do({
      
      data <- tidyr::spread(., `active contract ticker`, value) %>% 
        dplyr::select(-c(field, year, regime, date))
      
      tibble::tibble(
        correlations = tryCatch(
          { list(cor(data, use = "pairwise.complete.obs")) }, 
          error = function(e) { NA }
        )
      )
    }) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      period = "year", year = y, frequency = "day", type = "returns"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  ## other frequencies
  others <- lapply(
    c("week", "month"), 
    function(x){
      dplyr::mutate(data, unit = do.call(what = x, args = list(date))) %>% 
        dplyr::group_by(`active contract ticker`, field, year, unit) %>% 
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
        dplyr::group_by(`active contract ticker`, field) %>% 
        dplyr::select(-unit) %>%
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L), 
          value = ifelse(is.infinite(value), NA, value)
        ) %>%
        dplyr::group_by(field, regime) %>% 
        dplyr::do({
          data <- tidyr::spread(., `active contract ticker`, value) %>% 
            dplyr::select(-c(field, year, regime, date))
          tibble::tibble(
            correlations = tryCatch(
              { list(cor(data, use = "pairwise.complete.obs")) }, 
              error = function(e) { NA }
            )
          )
        }) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = "year", year = y, frequency = x, type = "returns"
        ) %>%
        dplyr::select(type, frequency, field, period, year, regime, correlations)
    }) %>% 
    dplyr::bind_rows()
  
  regimes <- dplyr::bind_rows(list(levels, days, others))
  dplyr::bind_rows(list(whole, regimes))
}

subperiods <- foreach(
  y = unique(periods$period), 
  .combine = dplyr::bind_rows
) %dopar% {
  library(magrittr); library(lubridate)
  
  start <- dplyr::filter(periods, period == y, bound == "start") %>% 
    dplyr::select(date) %>% purrr::flatten_chr()
  end <- dplyr::filter(periods, period == y, bound == "end") %>% 
    dplyr::select(date) %>% purrr::flatten_chr()
  
  data <- dplyr::filter(`commodity futures data`, `TS position` == 1L) %>% 
    dplyr::select(`active contract ticker`, field, date, value) %>% 
    dplyr::bind_rows(
      dplyr::rename(
        `commodity aggregate data`@data, `active contract ticker` = ticker)
    ) %>%
    dplyr::filter(
      `active contract ticker` %in% tickers, 
      date >= as.Date(start), date <= as.Date(end)
    ) %>% 
    dplyr::mutate(year = lubridate::year(date))
  
  # levels 
  levels <- dplyr::group_by(data, field) %>% 
    dplyr::do({
      
      data <- tidyr::spread(., `active contract ticker`, value) %>% 
        dplyr::select(-c(field, date, year))
      
      tibble::tibble(
        correlations = tryCatch(
          { list(cor(data, use = "pairwise.complete.obs")) }, 
          error = function(e) { NA }
        )
      )
    }) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      period = y, year = NA, frequency = "day", type = "levels", regime = "all"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  # returns 
  ## days
  days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
    dplyr::mutate(
      value = (value / dplyr::lag(value, 1L) - 1L), 
      value = ifelse(is.infinite(value), NA, value)
    ) %>%
    dplyr::group_by(field) %>% dplyr::do({
      
      data <- tidyr::spread(., `active contract ticker`, value) %>% 
        dplyr::select(-c(field, year, date))
      tibble::tibble(
        correlations = tryCatch(
          { list(cor(data, use = "pairwise.complete.obs")) }, 
          error = function(e) { NA }
        )
      )
      
    }) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(
      period = y, year = NA, frequency = "day", type = "returns", regime = "all"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  ## other frequencies
  others <- lapply(
    c("week", "month"), 
    function(x){
      dplyr::mutate(data, unit = do.call(what = x, args = list(date))) %>% 
        dplyr::group_by(`active contract ticker`, field, year, unit) %>% 
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
        dplyr::group_by(`active contract ticker`, field) %>% 
        dplyr::select(-unit) %>%
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L), 
          value = ifelse(is.infinite(value), NA, value)
        ) %>%
        dplyr::group_by(field) %>% dplyr::do({
          
          data <- tidyr::spread(., `active contract ticker`, value) %>% 
            dplyr::select(-c(field, year, date))
          tibble::tibble(
            correlations = tryCatch(
              { list(cor(data, use = "pairwise.complete.obs")) }, 
              error = function(e) { NA }
            )
          )
        }) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = y, year = NA, frequency = x, type = "returns", regime = "all"
        ) %>%
        dplyr::select(type, frequency, field, period, year, regime, correlations)
    }) %>% 
    dplyr::bind_rows()
  
  whole <- dplyr::bind_rows(list(levels, days, others))
  
  data <- dplyr::left_join(
    data, dplyr::filter(
      `aggregate CHP regimes`, ! is.na(period)) %>% dplyr::select(date, regime), 
    by = "date"
  ) %>% 
    dplyr::filter(! is.na(regime))
  
  # levels 
  levels <- dplyr::group_by(data, field, regime) %>% 
    dplyr::do({
      
      data <- tidyr::spread(., `active contract ticker`, value) %>% 
        dplyr::select(-c(field, year, regime, date))
      
      tibble::tibble(
        correlations = tryCatch(
          { list(cor(data, use = "pairwise.complete.obs")) }, 
          error = function(e) { NA }
        )
      )
      
    }) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      period = y, year = NA, frequency = "day", type = "levels"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  # returns 
  ## days
  days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
    dplyr::mutate(
      value = (value / dplyr::lag(value, 1L) - 1L), 
      value = ifelse(is.infinite(value), NA, value)
    ) %>%
    dplyr::group_by(field, regime) %>% 
    dplyr::do({
      
      data <- tidyr::spread(., `active contract ticker`, value) %>% 
        dplyr::select(-c(field, year, regime, date))
      
      tibble::tibble(
        correlations = tryCatch(
          { list(cor(data, use = "pairwise.complete.obs")) }, 
          error = function(e) { NA }
        )
      )
    }) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(period = y, year = NA, frequency = "day", type = "returns") %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  ## other frequencies
  others <- lapply(
    c("week", "month"), 
    function(x){
      dplyr::mutate(data, unit = do.call(what = x, args = list(date))) %>% 
        dplyr::group_by(`active contract ticker`, field, year, unit) %>% 
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
        dplyr::group_by(`active contract ticker`, field) %>% 
        dplyr::select(-unit) %>%
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L), 
          value = ifelse(is.infinite(value), NA, value)
        ) %>%
        dplyr::group_by(field, regime) %>% 
        dplyr::do({
          data <- tidyr::spread(., `active contract ticker`, value) %>% 
            dplyr::select(-c(field, year, regime, date))
          tibble::tibble(
            correlations = tryCatch(
              { list(cor(data, use = "pairwise.complete.obs")) },
              error = function(e) { NA }
            )
          )
        }) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(period = y, year = NA, frequency = x, type = "returns") %>%
        dplyr::select(type, frequency, field, period, year, regime, correlations)
    }) %>% 
    dplyr::bind_rows()
  
  regimes <- dplyr::bind_rows(list(levels, days, others))
  dplyr::bind_rows(list(whole, regimes))
}

`US metals` <- dplyr::bind_rows(list(years, subperiods))



# between UK metals ####
tickers <- dplyr::distinct(`commodity futures data`, `active contract ticker`) %>% 
  purrr::flatten_chr()
tickers <- dplyr::left_join(
  dplyr::filter(tickers_futures, ticker %in% tickers) %>% dplyr::select(ticker, sector, MIC),
  dplyr::select(exchanges, MIC, country), 
  by = "MIC"
) %>% 
  dplyr::filter(
    ticker %in% `commodity futures tickers`, sector == "metals", country == "GB"
  ) %>% 
  dplyr::select(ticker) %>% 
  purrr::flatten_chr()

years <- dplyr::filter(
  `commodity futures data`, 
  `active contract ticker` %in% tickers
) %>% dplyr::distinct(date) %>% 
  dplyr::mutate(year = lubridate::year(date)) %>% 
  dplyr::distinct(year) %>% purrr::flatten_dbl() %>% 
  as.integer()

years <- foreach(
  y = years, 
  .combine = dplyr::bind_rows
) %dopar% {
  library(magrittr); library(lubridate)
  
  data <- dplyr::filter(`commodity futures data`, `TS position` == 1L) %>% 
    dplyr::select(`active contract ticker`, field, date, value) %>% 
    dplyr::bind_rows(
      dplyr::rename(
        `commodity aggregate data`@data, `active contract ticker` = ticker)
    ) %>% 
    dplyr::mutate(year = lubridate::year(date)) %>%
    dplyr::filter(
      `active contract ticker` %in% tickers,
      year == y
    ) 
  
  # levels 
  levels <- dplyr::group_by(data, field) %>% 
    dplyr::do({
      data <- tidyr::spread(., `active contract ticker`, value) %>% 
        dplyr::select(-c(field, date, year))
      tibble::tibble(
        correlations = tryCatch(
          { list(cor(data, use = "pairwise.complete.obs")) }, 
          error = function(e) { NA }
        )
      )
    }) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      period = "year", year = y, frequency = "day", type = "levels", regime = "all"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  # returns 
  ## days
  days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
    dplyr::mutate(
      value = (value / dplyr::lag(value, 1L) - 1L), 
      value = ifelse(is.infinite(value), NA, value)
    ) %>%
    dplyr::group_by(field) %>% 
    dplyr::do({
      
      data <- tidyr::spread(., `active contract ticker`, value) %>% 
        dplyr::select(-c(field, year, date))
      
      tibble::tibble(
        correlations = tryCatch(
          { list(cor(data, use = "pairwise.complete.obs")) },
          error = function(e) { NA }
        )
      )
    }) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      period = "year", year = y, frequency = "day", type = "returns", regime = "all"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  ## other frequencies
  others <- lapply(
    c("week", "month"), 
    function(x){
      dplyr::mutate(data, unit = do.call(what = x, args = list(date))) %>% 
        dplyr::group_by(`active contract ticker`, field, year, unit) %>% 
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
        dplyr::group_by(`active contract ticker`, field) %>% 
        dplyr::select(-unit) %>%
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L), 
          value = ifelse(is.infinite(value), NA, value)
        ) %>%
        dplyr::group_by(field) %>% 
        dplyr::do({
          data <- tidyr::spread(., `active contract ticker`, value) %>% 
            dplyr::select(-c(field, year, date))
          tibble::tibble(
            correlations = tryCatch(
              { list(cor(data, use = "pairwise.complete.obs")) }, 
              error = function(e) { NA }
            )
          )
        }) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = "year", year = y, frequency = x, type = "returns", regime = "all"
        ) %>%
        dplyr::select(type, frequency, field, period, year, regime, correlations)
    }) %>% 
    dplyr::bind_rows()
  
  whole <- dplyr::bind_rows(list(levels, days, others))
  
  data <- dplyr::left_join(
    data, 
    dplyr::filter(`aggregate CHP regimes`, is.na(period)) %>% 
      dplyr::select(date, regime), 
    by = "date") %>% 
    dplyr::filter(! is.na(regime))
  
  # levels 
  levels <- dplyr::group_by(data, field, regime) %>% 
    dplyr::do({
      
      data <- tidyr::spread(., `active contract ticker`, value) %>% 
        dplyr::select(-c(field, year, regime, date))
      
      tibble::tibble(
        correlations = tryCatch(
          { list(cor(data, use = "pairwise.complete.obs")) }, 
          error = function(e) { NA }
        )
      )
      
    }) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      period = "year", year = y, frequency = "day", type = "levels"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  # returns 
  ## days
  days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
    dplyr::mutate(
      value = (value / dplyr::lag(value, 1L) - 1L), 
      value = ifelse(is.infinite(value), NA, value)
    ) %>%
    dplyr::group_by(field, regime) %>% 
    dplyr::do({
      
      data <- tidyr::spread(., `active contract ticker`, value) %>% 
        dplyr::select(-c(field, year, regime, date))
      
      tibble::tibble(
        correlations = tryCatch(
          { list(cor(data, use = "pairwise.complete.obs")) },
          error = function(e) { NA }
        )
      )
    }) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      period = "year", year = y, frequency = "day", type = "returns"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  ## other frequencies
  others <- lapply(
    c("week", "month"), 
    function(x){
      dplyr::mutate(data, unit = do.call(what = x, args = list(date))) %>% 
        dplyr::group_by(`active contract ticker`, field, year, unit) %>% 
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
        dplyr::group_by(`active contract ticker`, field) %>% 
        dplyr::select(-unit) %>%
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L), 
          value = ifelse(is.infinite(value), NA, value)
        ) %>%
        dplyr::group_by(field, regime) %>% 
        dplyr::do({
          
          data <- tidyr::spread(., `active contract ticker`, value) %>% 
            dplyr::select(-c(field, year, regime, date))
          tibble::tibble(
            correlations = tryCatch(
              { list(cor(data, use = "pairwise.complete.obs")) }, 
              error = function(e) { NA }
            )
          )
          
        }) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(period = "year", year = y, frequency = x, type = "returns") %>%
        dplyr::select(type, frequency, field, period, year, regime, correlations)
      
    }) %>% 
    dplyr::bind_rows()
  
  regimes <- dplyr::bind_rows(list(levels, days, others))
  dplyr::bind_rows(list(whole, regimes))
}

subperiods <- foreach(
  y = unique(periods$period), 
  .combine = dplyr::bind_rows) %dopar% {
    library(magrittr); library(lubridate)
    
    start <- dplyr::filter(periods, period == y, bound == "start") %>% 
      dplyr::select(date) %>% purrr::flatten_chr()
    end <- dplyr::filter(periods, period == y, bound == "end") %>% 
      dplyr::select(date) %>% purrr::flatten_chr()
    
    data <- dplyr::filter(`commodity futures data`, `TS position` == 1L) %>% 
      dplyr::select(`active contract ticker`, field, date, value) %>% 
      dplyr::bind_rows(
        dplyr::rename(
          `commodity aggregate data`@data, `active contract ticker` = ticker)
      ) %>%
      dplyr::filter(
        `active contract ticker` %in% tickers, 
        date >= as.Date(start), date <= as.Date(end)
      ) %>% 
      dplyr::mutate(year = lubridate::year(date))
    
    # levels 
    levels <- dplyr::group_by(data, field) %>% 
      dplyr::do({
        
        data <- tidyr::spread(., `active contract ticker`, value) %>% 
          dplyr::select(-c(field, date, year))
        
        tibble::tibble(
          correlations = tryCatch(
            { list(cor(data, use = "pairwise.complete.obs")) }, 
            error = function(e) { NA }
          )
        )
        
      }) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        period = y, year = NA, frequency = "day", type = "levels", regime = "all"
      ) %>%
      dplyr::select(type, frequency, field, period, year, regime, correlations)
    
    # returns 
    ## days
    days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
      dplyr::mutate(
        value = (value / dplyr::lag(value, 1L) - 1L), 
        value = ifelse(is.infinite(value), NA, value)
      ) %>%
      dplyr::group_by(field) %>% 
      dplyr::do({
        data <- tidyr::spread(., `active contract ticker`, value) %>% 
          dplyr::select(-c(field, year, date))
        tibble::tibble(
          correlations = tryCatch(
            { list(cor(data, use = "pairwise.complete.obs")) }, 
            error = function(e) { NA }
          )
        )
      }) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        period = y, year = NA, frequency = "day", type = "returns", regime = "all"
      ) %>%
      dplyr::select(type, frequency, field, period, year, regime, correlations)
    
    ## other frequencies
    others <- lapply(
      c("week", "month"), 
      function(x){
        dplyr::mutate(data, unit = do.call(what = x, args = list(date))) %>% 
          dplyr::group_by(`active contract ticker`, field, year, unit) %>% 
          dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
          dplyr::group_by(`active contract ticker`, field) %>%
          dplyr::select(-unit) %>%
          dplyr::mutate(
            value = (value / dplyr::lag(value, 1L) - 1L), 
            value = ifelse(is.infinite(value), NA, value)
          ) %>%
          dplyr::group_by(field) %>% 
          dplyr::do({
            
            data <- tidyr::spread(., `active contract ticker`, value) %>% 
              dplyr::select(-c(field, year, date))
            
            tibble::tibble(
              correlations = tryCatch(
                { list(cor(data, use = "pairwise.complete.obs")) },
                error = function(e) { NA }
              )
            )
            
          }) %>% 
          dplyr::ungroup() %>% 
          dplyr::mutate(
            period = y, year = NA, frequency = x, type = "returns", regime = "all"
          ) %>%
          dplyr::select(type, frequency, field, period, year, regime, correlations)
      }) %>% 
      dplyr::bind_rows()
    
    whole <- dplyr::bind_rows(list(levels, days, others))
    
    data <- dplyr::left_join(
      data, 
      dplyr::filter(`aggregate CHP regimes`, ! is.na(period)) %>% 
        dplyr::select(date, regime), 
      by = "date") %>% 
      dplyr::filter(! is.na(regime))
    
    # levels 
    levels <- dplyr::group_by(data, field, regime) %>% 
      dplyr::do({
        
        data <- tidyr::spread(., `active contract ticker`, value) %>% 
          dplyr::select(-c(field, year, regime, date))
        
        tibble::tibble(
          correlations = tryCatch(
            { list(cor(data, use = "pairwise.complete.obs")) }, 
            error = function(e) { NA }
          )
        )
      }) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        period = y, year = NA, frequency = "day", type = "levels"
      ) %>%
      dplyr::select(type, frequency, field, period, year, regime, correlations)
    # returns 
    ## days
    days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
      dplyr::mutate(
        value = (value / dplyr::lag(value, 1L) - 1L), 
        value = ifelse(is.infinite(value), NA, value)
      ) %>%
      dplyr::group_by(field, regime) %>% 
      dplyr::do({
        
        data <- tidyr::spread(., `active contract ticker`, value) %>% 
          dplyr::select(-c(field, year, regime, date))
        
        tibble::tibble(
          correlations = tryCatch(
            { list(cor(data, use = "pairwise.complete.obs")) }, 
            error = function(e) { NA }
          )
        )
        
      }) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(period = y, year = NA, frequency = "day", type = "returns") %>%
      dplyr::select(type, frequency, field, period, year, regime, correlations)
    
    ## other frequencies
    others <- lapply(
      c("week", "month"), 
      function(x){
        dplyr::mutate(data, unit = do.call(what = x, args = list(date))) %>% 
          dplyr::group_by(`active contract ticker`, field, year, unit) %>% 
          dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
          dplyr::group_by(`active contract ticker`, field) %>% 
          dplyr::select(-unit) %>%
          dplyr::mutate(
            value = (value / dplyr::lag(value, 1L) - 1L), 
            value = ifelse(is.infinite(value), NA, value)
          ) %>%
          dplyr::group_by(field, regime) %>% 
          dplyr::do({
            
            data <- tidyr::spread(., `active contract ticker`, value) %>% 
              dplyr::select(-c(field, year, regime, date))
            
            tibble::tibble(
              correlations = tryCatch(
                { list(cor(data, use = "pairwise.complete.obs")) }, 
                error = function(e) { NA }
              )
            )
            
          }) %>% 
          dplyr::ungroup() %>% 
          dplyr::mutate(period = y, year = NA, frequency = x, type = "returns") %>%
          dplyr::select(type, frequency, field, period, year, regime, correlations)
      }) %>% 
      dplyr::bind_rows()
    
    regimes <- dplyr::bind_rows(list(levels, days, others))
    dplyr::bind_rows(list(whole, regimes))
  }

`UK metals` <- dplyr::bind_rows(list(years, subperiods))

# correlations <- readr::read_rds(
#   paste(here::here(), "explore/results/new/correlations.rds", sep = "/")
# )
# correlations$results[[5L]] <- `UK metals`
# readr::write_rds(
#   correlations,
#   paste(here::here(), "explore/results/new/correlations.rds", sep = "/")
# )





# between US energy ####
tickers <- dplyr::distinct(`commodity futures data`, `active contract ticker`) %>% 
  purrr::flatten_chr()
tickers <- dplyr::left_join(
  dplyr::filter(tickers_futures, ticker %in% tickers) %>% dplyr::select(ticker, sector, MIC),
  dplyr::select(exchanges, MIC, country), 
  by = "MIC"
) %>% 
  dplyr::filter(
    ticker %in% `commodity futures tickers`, sector == "energy", country == "US"
  ) %>% 
  dplyr::select(ticker) %>% 
  purrr::flatten_chr()

years <- dplyr::filter(
  `commodity futures data`, 
  `active contract ticker` %in% tickers
) %>% dplyr::distinct(date) %>% 
  dplyr::mutate(year = lubridate::year(date)) %>% 
  dplyr::distinct(year) %>% purrr::flatten_dbl() %>% 
  as.integer()

years <- foreach(
  y = years, 
  .combine = dplyr::bind_rows
) %dopar% {
  library(magrittr); library(lubridate)
  
  data <- dplyr::filter(`commodity futures data`, `TS position` == 1L) %>% 
    dplyr::select(`active contract ticker`, field, date, value) %>% 
    dplyr::bind_rows(
      dplyr::rename(
        `commodity aggregate data`@data, `active contract ticker` = ticker)
    ) %>% 
    dplyr::mutate(year = lubridate::year(date)) %>%
    dplyr::filter(
      `active contract ticker` %in% tickers,
      year == y
    ) 
  
  # levels 
  levels <- dplyr::group_by(data, field) %>% 
    dplyr::do({
      data <- tidyr::spread(., `active contract ticker`, value) %>% 
        dplyr::select(-c(field, date, year))
      tibble::tibble(
        correlations = tryCatch(
          { list(cor(data, use = "pairwise.complete.obs")) }, 
          error = function(e) { NA }
        )
      )
    }) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      period = "year", year = y, frequency = "day", type = "levels", regime = "all"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  # returns 
  ## days
  days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
    dplyr::mutate(
      value = (value / dplyr::lag(value, 1L) - 1L), 
      value = ifelse(is.infinite(value), NA, value)
    ) %>%
    dplyr::group_by(field) %>% 
    dplyr::do({
      
      data <- tidyr::spread(., `active contract ticker`, value) %>% 
        dplyr::select(-c(field, year, date))
      
      tibble::tibble(
        correlations = tryCatch(
          { list(cor(data, use = "pairwise.complete.obs")) },
          error = function(e) { NA }
        )
      )
    }) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      period = "year", year = y, frequency = "day", type = "returns", regime = "all"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  ## other frequencies
  others <- lapply(
    c("week", "month"), 
    function(x){
      dplyr::mutate(data, unit = do.call(what = x, args = list(date))) %>% 
        dplyr::group_by(`active contract ticker`, field, year, unit) %>% 
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
        dplyr::group_by(`active contract ticker`, field) %>% 
        dplyr::select(-unit) %>%
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L), 
          value = ifelse(is.infinite(value), NA, value)
        ) %>%
        dplyr::group_by(field) %>% 
        dplyr::do({
          data <- tidyr::spread(., `active contract ticker`, value) %>% 
            dplyr::select(-c(field, year, date))
          tibble::tibble(
            correlations = tryCatch(
              { list(cor(data, use = "pairwise.complete.obs")) }, 
              error = function(e) { NA }
            )
          )
        }) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = "year", year = y, frequency = x, type = "returns", regime = "all"
        ) %>%
        dplyr::select(type, frequency, field, period, year, regime, correlations)
    }) %>% 
    dplyr::bind_rows()
  
  whole <- dplyr::bind_rows(list(levels, days, others))
  
  data <- dplyr::left_join(
    data, 
    dplyr::filter(`aggregate CHP regimes`, is.na(period)) %>% 
      dplyr::select(date, regime), 
    by = "date") %>% 
    dplyr::filter(! is.na(regime))
  
  # levels 
  levels <- dplyr::group_by(data, field, regime) %>% 
    dplyr::do({
      
      data <- tidyr::spread(., `active contract ticker`, value) %>% 
        dplyr::select(-c(field, year, regime, date))
      
      tibble::tibble(
        correlations = tryCatch(
          { list(cor(data, use = "pairwise.complete.obs")) }, 
          error = function(e) { NA }
        )
      )
      
    }) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      period = "year", year = y, frequency = "day", type = "levels"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  # returns 
  ## days
  days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
    dplyr::mutate(
      value = (value / dplyr::lag(value, 1L) - 1L), 
      value = ifelse(is.infinite(value), NA, value)
    ) %>%
    dplyr::group_by(field, regime) %>% 
    dplyr::do({
      
      data <- tidyr::spread(., `active contract ticker`, value) %>% 
        dplyr::select(-c(field, year, regime, date))
      
      tibble::tibble(
        correlations = tryCatch(
          { list(cor(data, use = "pairwise.complete.obs")) },
          error = function(e) { NA }
        )
      )
    }) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      period = "year", year = y, frequency = "day", type = "returns"
    ) %>%
    dplyr::select(type, frequency, field, period, year, regime, correlations)
  
  ## other frequencies
  others <- lapply(
    c("week", "month"), 
    function(x){
      dplyr::mutate(data, unit = do.call(what = x, args = list(date))) %>% 
        dplyr::group_by(`active contract ticker`, field, year, unit) %>% 
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
        dplyr::group_by(`active contract ticker`, field) %>% 
        dplyr::select(-unit) %>%
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L), 
          value = ifelse(is.infinite(value), NA, value)
        ) %>%
        dplyr::group_by(field, regime) %>% 
        dplyr::do({
          
          data <- tidyr::spread(., `active contract ticker`, value) %>% 
            dplyr::select(-c(field, year, regime, date))
          tibble::tibble(
            correlations = tryCatch(
              { list(cor(data, use = "pairwise.complete.obs")) }, 
              error = function(e) { NA }
            )
          )
          
        }) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(period = "year", year = y, frequency = x, type = "returns") %>%
        dplyr::select(type, frequency, field, period, year, regime, correlations)
      
    }) %>% 
    dplyr::bind_rows()
  
  regimes <- dplyr::bind_rows(list(levels, days, others))
  dplyr::bind_rows(list(whole, regimes))
}

subperiods <- foreach(
  y = unique(periods$period), 
  .combine = dplyr::bind_rows) %dopar% {
    library(magrittr); library(lubridate)
    
    start <- dplyr::filter(periods, period == y, bound == "start") %>% 
      dplyr::select(date) %>% purrr::flatten_chr()
    end <- dplyr::filter(periods, period == y, bound == "end") %>% 
      dplyr::select(date) %>% purrr::flatten_chr()
    
    data <- dplyr::filter(`commodity futures data`, `TS position` == 1L) %>% 
      dplyr::select(`active contract ticker`, field, date, value) %>% 
      dplyr::bind_rows(
        dplyr::rename(
          `commodity aggregate data`@data, `active contract ticker` = ticker)
      ) %>%
      dplyr::filter(
        `active contract ticker` %in% tickers, 
        date >= as.Date(start), date <= as.Date(end)
      ) %>% 
      dplyr::mutate(year = lubridate::year(date))
    
    # levels 
    levels <- dplyr::group_by(data, field) %>% 
      dplyr::do({
        
        data <- tidyr::spread(., `active contract ticker`, value) %>% 
          dplyr::select(-c(field, date, year))
        
        tibble::tibble(
          correlations = tryCatch(
            { list(cor(data, use = "pairwise.complete.obs")) }, 
            error = function(e) { NA }
          )
        )
        
      }) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        period = y, year = NA, frequency = "day", type = "levels", regime = "all"
      ) %>%
      dplyr::select(type, frequency, field, period, year, regime, correlations)
    
    # returns 
    ## days
    days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
      dplyr::mutate(
        value = (value / dplyr::lag(value, 1L) - 1L), 
        value = ifelse(is.infinite(value), NA, value)
      ) %>%
      dplyr::group_by(field) %>% 
      dplyr::do({
        data <- tidyr::spread(., `active contract ticker`, value) %>% 
          dplyr::select(-c(field, year, date))
        tibble::tibble(
          correlations = tryCatch(
            { list(cor(data, use = "pairwise.complete.obs")) }, 
            error = function(e) { NA }
          )
        )
      }) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        period = y, year = NA, frequency = "day", type = "returns", regime = "all"
      ) %>%
      dplyr::select(type, frequency, field, period, year, regime, correlations)
    
    ## other frequencies
    others <- lapply(
      c("week", "month"), 
      function(x){
        dplyr::mutate(data, unit = do.call(what = x, args = list(date))) %>% 
          dplyr::group_by(`active contract ticker`, field, year, unit) %>% 
          dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
          dplyr::group_by(`active contract ticker`, field) %>%
          dplyr::select(-unit) %>%
          dplyr::mutate(
            value = (value / dplyr::lag(value, 1L) - 1L), 
            value = ifelse(is.infinite(value), NA, value)
          ) %>%
          dplyr::group_by(field) %>% 
          dplyr::do({
            
            data <- tidyr::spread(., `active contract ticker`, value) %>% 
              dplyr::select(-c(field, year, date))
            
            tibble::tibble(
              correlations = tryCatch(
                { list(cor(data, use = "pairwise.complete.obs")) },
                error = function(e) { NA }
              )
            )
            
          }) %>% 
          dplyr::ungroup() %>% 
          dplyr::mutate(
            period = y, year = NA, frequency = x, type = "returns", regime = "all"
          ) %>%
          dplyr::select(type, frequency, field, period, year, regime, correlations)
      }) %>% 
      dplyr::bind_rows()
    
    whole <- dplyr::bind_rows(list(levels, days, others))
    
    data <- dplyr::left_join(
      data, 
      dplyr::filter(`aggregate CHP regimes`, ! is.na(period)) %>% 
        dplyr::select(date, regime), 
      by = "date") %>% 
      dplyr::filter(! is.na(regime))
    
    # levels 
    levels <- dplyr::group_by(data, field, regime) %>% 
      dplyr::do({
        
        data <- tidyr::spread(., `active contract ticker`, value) %>% 
          dplyr::select(-c(field, year, regime, date))
        
        tibble::tibble(
          correlations = tryCatch(
            { list(cor(data, use = "pairwise.complete.obs")) }, 
            error = function(e) { NA }
          )
        )
      }) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        period = y, year = NA, frequency = "day", type = "levels"
      ) %>%
      dplyr::select(type, frequency, field, period, year, regime, correlations)
    # returns 
    ## days
    days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
      dplyr::mutate(
        value = (value / dplyr::lag(value, 1L) - 1L), 
        value = ifelse(is.infinite(value), NA, value)
      ) %>%
      dplyr::group_by(field, regime) %>% 
      dplyr::do({
        
        data <- tidyr::spread(., `active contract ticker`, value) %>% 
          dplyr::select(-c(field, year, regime, date))
        
        tibble::tibble(
          correlations = tryCatch(
            { list(cor(data, use = "pairwise.complete.obs")) }, 
            error = function(e) { NA }
          )
        )
        
      }) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(period = y, year = NA, frequency = "day", type = "returns") %>%
      dplyr::select(type, frequency, field, period, year, regime, correlations)
    
    ## other frequencies
    others <- lapply(
      c("week", "month"), 
      function(x){
        dplyr::mutate(data, unit = do.call(what = x, args = list(date))) %>% 
          dplyr::group_by(`active contract ticker`, field, year, unit) %>% 
          dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
          dplyr::group_by(`active contract ticker`, field) %>% 
          dplyr::select(-unit) %>%
          dplyr::mutate(
            value = (value / dplyr::lag(value, 1L) - 1L), 
            value = ifelse(is.infinite(value), NA, value)
          ) %>%
          dplyr::group_by(field, regime) %>% 
          dplyr::do({
            
            data <- tidyr::spread(., `active contract ticker`, value) %>% 
              dplyr::select(-c(field, year, regime, date))
            
            tibble::tibble(
              correlations = tryCatch(
                { list(cor(data, use = "pairwise.complete.obs")) }, 
                error = function(e) { NA }
              )
            )
            
          }) %>% 
          dplyr::ungroup() %>% 
          dplyr::mutate(period = y, year = NA, frequency = x, type = "returns") %>%
          dplyr::select(type, frequency, field, period, year, regime, correlations)
      }) %>% 
      dplyr::bind_rows()
    
    regimes <- dplyr::bind_rows(list(levels, days, others))
    dplyr::bind_rows(list(whole, regimes))
  }

`US energy` <- dplyr::bind_rows(list(years, subperiods))


# correlations <- readr::read_rds(
#   paste(here::here(), "explore/results/correlations.rds", sep = "/")
# )
# correlations <- dplyr::bind_rows(
#   correlations,
#   tibble::tibble(`asset pool` = "US energy", results = list(`US energy`))
# )


correlations <- tibble::tibble(
  `asset pool` = c(
    "factor picks", "US commodities", 
    "US commodities (excluding metals)", "US metals", "UK metals",
    "US energy"
  ),
  results = list(
    `factor picks`, `US commodities`, `US commodities (excluding metals)`, 
    `US metals`, `UK metals`, `US energy`
  )
)

parallel::stopCluster(cluster)
readr::write_rds(
  correlations,
  paste(here::here(), "explore/results/correlations.rds", sep = "/")
)

