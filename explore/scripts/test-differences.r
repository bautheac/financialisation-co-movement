library(magrittr); library(doParallel); library(storethat); library(pullit)
library(factorem)

# start cluster ####
cluster <- makeCluster(detectCores() - 1L, outfile = ""); registerDoParallel(cluster)

# globals ####
data("tickers_futures", "tickers_cftc", package = "BBGsymbols")
data("exchanges", package = "fewISOs")

storethat <- paste0(here::here(), "/data/storethat.sqlite")


periods <- tibble::tibble(
  period = c(
    rep("past", 2L), 
    rep("financialization", 2L), 
    rep("crisis", 2L), 
    rep("present", 2L)
  ),
  bound = rep(c("start", "end"), 4L),
  date = c(
    "1997-07-01", "2003-12-31", "2004-01-01", "2008-09-14", "2008-09-15", 
    "2013-06-19", "2013-06-20", "2018-12-31"
  )
) %>% dplyr::mutate(period = forcats::as_factor(period))


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
  "SBA Comdty", "SIA Comdty", "SMA Comdty", "W A Comdty", "XBWA Comdty",
  "LAA Comdty", "LPA Comdty", "LLA Comdty", "LNA Comdty", "LTA Comdty",
  "LXA Comdty"
)

`commodity futures data` <- pullit::pull_futures_market(
  source = "storethat", type = "term structure", 
  active_contract_tickers = `commodity futures tickers`,
  start = start, end = end, 
  TS_positions = 1L:2L, 
  roll_type = "A", roll_days = 0L, roll_months = 0L, roll_adjustment = "N", 
  file = storethat
)

`commodity futures data` <- dplyr::left_join(
  pullit::get_data(`commodity futures data`),
  dplyr::select(
    pullit::get_term_structure_tickers(`commodity futures data`), 
    `active contract ticker`, ticker, `TS position`
  ), by = "ticker") %>% 
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

# aggregate CHP ####
`aggregate CHP` <- dplyr::left_join(
  pullit::get_data(`commodity CFTC data`),
  dplyr::select(
    tickers_cftc, MIC, format, underlying, unit, participant, position, ticker
  ),
  by = "ticker"
) %>% 
  dplyr::filter(
    format == "legacy", 
    participant == "commercial", 
    underlying == "futures only", 
    unit == "contracts", 
    position %in% c("long", "short")
  ) %>%
  dplyr::select(
    `active contract ticker`, position, date, value
  ) %>% 
  dplyr::group_by(`active contract ticker`) %>%
  tidyr::spread(position, value) %>% 
  dplyr::mutate(pressure = long / (long + short)) %>% 
  dplyr::select(`active contract ticker`, date, pressure) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(date) %>% 
  dplyr::summarise(`aggregate CHP` = mean(pressure, na.rm = T)) %>%
  dplyr::arrange(date)

years <- dplyr::mutate(
  `aggregate CHP`, year = lubridate::year(date), week = lubridate::week(date)
) %>% dplyr::group_by(year) %>% 
  dplyr::mutate(
    regime = ifelse(
      `aggregate CHP` < median(`aggregate CHP`), 
      "backwardation", "contango"
    )
  ) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(period = NA) %>% 
  dplyr::select(period, year, week, regime)

years <- dplyr::mutate(
  dplyr::distinct(`commodity futures data`, date), 
  year = lubridate::year(date),
  week = lubridate::week(date)
) %>%
  dplyr::left_join(years, by = c("year", "week")) %>% 
  dplyr::select(-week) %>% 
  dplyr::filter(! is.na(regime)) %>%
  dplyr::arrange(date)

subperiods <- lapply(
  unique(periods$period), 
  function(x){
    
    start <- dplyr::filter(periods, period == x, bound == "start") %>% 
      dplyr::select(date) %>% 
      purrr::flatten_chr()
    
    end <- dplyr::filter(periods, period == x, bound == "end") %>% 
      dplyr::select(date) %>% 
      purrr::flatten_chr()
    
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


carry <- dplyr::filter(
  `commodity futures data`, field == "PX_LAST"
) %>% 
  dplyr::select(`active contract ticker`, `TS position`, date, value) %>% 
  dplyr::group_by(`active contract ticker`) %>%
  tidyr::spread(`TS position`, value) %>%
  dplyr::mutate(carry = (`1`/`2`) - 1L) %>% 
  dplyr::select(`active contract ticker`, date, carry) %>%
  tidyr::gather(field, value, -c(`active contract ticker`, date)) %>% 
  dplyr::select(`active contract ticker`, field, date, value) %>%
  dplyr::ungroup()

`market data` <- dplyr::filter(
  `commodity futures data`, `TS position` == 1L
) %>% dplyr::select(`active contract ticker`, field, date, value) %>%
  dplyr::bind_rows(carry) %>% 
  dplyr::bind_rows(
    dplyr::rename(`commodity aggregate data`@data, `active contract ticker` = ticker)
  ) %>% dplyr::arrange(`active contract ticker`, field, date)


# between periods ####
## commodity futures ####
### market variables ####
#### by single commodity ####
combinations <- t(combn(unique(periods$period), 2L)) %>% as.data.frame() %>%
  rlang::set_names(c("period 1", "period 2"))
combinations <- tidyr::expand_grid(
  dplyr::distinct(`market data`, `active contract ticker`, field),
  combinations
) %>% dplyr::arrange(`active contract ticker`, field)


`individual commodities` <- foreach(
  y = 1L:nrow(combinations), .combine = rbind
  # y = 1L:50L, .combine = rbind
) %dopar% {
  library(magrittr); library(lubridate)
  
  message(paste(y, nrow(combinations), sep = "/"))
  
  combination <- dplyr::slice(combinations, y)
  
  start1 <- dplyr::filter(
    periods, period == combination$`period 1`, bound == "start"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  end1 <- dplyr::filter(
    periods, period == combination$`period 1`, bound == "end"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  start2 <- dplyr::filter(
    periods, period == combination$`period 2`, bound == "start"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  end2 <- dplyr::filter(
    periods, period == combination$`period 2`, bound == "end"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  
  data <- dplyr::left_join(
    dplyr::select(combination, `active contract ticker`, field), 
    `market data`, by = c("active contract ticker", "field")
  ) %>% dplyr::filter(date >= as.Date(start1), date <= as.Date(end2))
  
  # Levels 
  levels1 <- dplyr::filter(
    data, date >= as.Date(start1), date <= as.Date(end1)
  )
  levels2 <- dplyr::filter(
    data, date >= as.Date(start2), date <= as.Date(end2)
  )
  
  mean <- tryCatch(
    { stats::t.test(levels1$value, levels2$value) }, 
    error = function(e) { NA }
  )
  var <- tryCatch(
    { var.test(levels1$value, levels2$value, alternative = "two.sided") }, 
    error = function(e) { NA }
  )
  
  mean <- dplyr::mutate(
    combination, type = "levels", frequency = NA, moment = "mean", 
    p.value = tryCatch({ mean$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  var <- dplyr::mutate(
    combination, type = "levels", frequency = NA, moment = "variance", 
    p.value = tryCatch({ var$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  
  levels <- dplyr::bind_rows(mean, var)
  
  
  # returns
  ## days
  days <- dplyr::mutate(
    data,
    value = (value / dplyr::lag(value, 1L) - 1L), 
    value = ifelse(is.infinite(value), NA, value)
  ) 
  
  returns1 <- dplyr::filter(
    days, date >= as.Date(start1), date <= as.Date(end1)
  )
  returns2 <- dplyr::filter(
    days, date >= as.Date(start2), date <= as.Date(end2)
  )
  
  mean <- tryCatch(
    { stats::t.test(returns1$value, returns2$value) }, 
    error = function(e) { NA }
  )
  var <- tryCatch(
    { var.test(returns1$value, returns2$value, alternative = "two.sided") }, 
    error = function(e) { NA }
  )
  
  mean <- dplyr::mutate(
    combination, type = "returns", frequency = "daily", moment = "mean", 
    p.value = tryCatch({ mean$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  var <- dplyr::mutate(
    combination, type = "returns", frequency = "daily", moment = "variance", 
    p.value = tryCatch({ var$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  
  daily <- dplyr::bind_rows(mean, var)
  
  ## other frequencies
  others <- purrr::map_df(c("week", "month"), function(x){
    
    others <- dplyr::mutate(
      data, year = year(date), unit = do.call(what = x, args = list(date))
    ) %>% 
      dplyr::group_by(year, unit) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        value = (value / dplyr::lag(value, 1L) - 1L), 
        value = ifelse(is.infinite(value), NA, value)
      ) %>% dplyr::ungroup() %>% dplyr::select(-unit)
    
    returns1 <- dplyr::filter(
      others, date >= as.Date(start1), date <= as.Date(end1)
    )
    returns2 <- dplyr::filter(
      others, date >= as.Date(start2), date <= as.Date(end2)
    )
    
    mean <- tryCatch(
      { stats::t.test(returns1$value, returns2$value) }, 
      error = function(e) { NA }
    )
    var <-  tryCatch(
      { var.test(returns1$value, returns2$value, alternative = "two.sided") }, 
      error = function(e) { NA }
    )
    
    mean <- dplyr::mutate(
      combination, type = "returns", frequency = x, moment = "mean", 
      p.value = tryCatch({ mean$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    var <- dplyr::mutate(
      combination, type = "returns", frequency = x, moment = "variance", 
      p.value = tryCatch({ var$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    
    dplyr::bind_rows(mean, var)
    
  })
  
  returns <- dplyr::bind_rows(daily, others)
  
  whole <- dplyr::bind_rows(levels, returns) %>% 
    dplyr::mutate(regime = "all") %>% dplyr::relocate(regime, .before = moment)
  
  
  # browser()
  regimes <- purrr::map_df(c("backwardation", "contango"), function(z){
    
    # browser()
    dates1 <- dplyr::filter(
      `aggregate CHP regimes`, period == combination$`period 1`,
      regime == z
    ) %>% dplyr::select(date)
    dates2 <- dplyr::filter(
      `aggregate CHP regimes`, period == combination$`period 2`,
      regime == z
    ) %>% dplyr::select(date)
    
    # Levels 
    levels1 <- dplyr::filter(data, date %in% dates1$date)
    levels2 <- dplyr::filter(data, date %in% dates2$date)
    
    mean <- tryCatch(
      { stats::t.test(levels1$value, levels2$value) }, 
      error = function(e) { NA }
    )
    var <- tryCatch(
      { var.test(levels1$value, levels2$value, alternative = "two.sided") }, 
      error = function(e) { NA }
    )
    
    mean <- dplyr::mutate(
      combination, type = "levels", frequency = NA, moment = "mean", 
      p.value = tryCatch({ mean$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    var <- dplyr::mutate(
      combination, type = "levels", frequency = NA, moment = "variance", 
      p.value = tryCatch({ var$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    
    levels <- dplyr::bind_rows(mean, var)
    
    
    # returns
    ## days
    days <- dplyr::mutate(
      data,
      value = (value / dplyr::lag(value, 1L) - 1L), 
      value = ifelse(is.infinite(value), NA, value)
    ) 
    
    returns1 <- dplyr::filter(days, date %in% dates1$date)
    returns2 <- dplyr::filter(days, date %in% dates2$date)
    
    mean <- tryCatch({ stats::t.test(returns1$value, returns2$value) },
                     error = function(e) { NA })
    var <- tryCatch({ var.test(returns1$value, returns2$value, alternative = "two.sided") },
                    error = function(e) { NA })
    
    mean <- dplyr::mutate(
      combination, type = "returns", frequency = "daily", moment = "mean", 
      p.value = tryCatch({ mean$p.value }, error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    var <- dplyr::mutate(
      combination, type = "returns", frequency = "daily", moment = "variance", 
      p.value = tryCatch({ var$p.value }, error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    
    daily <- dplyr::bind_rows(mean, var)
    
    ## other frequencies
    others <- purrr::map_df(c("week", "month"), function(x){
      
      others <- dplyr::mutate(
        data, year = year(date), unit = do.call(what = x, args = list(date))
      ) %>% 
        dplyr::group_by(year, unit) %>%
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L), 
          value = ifelse(is.infinite(value), NA, value)
        ) %>% dplyr::ungroup() %>% dplyr::select(-unit)
      
      returns1 <- dplyr::filter(others, date %in% dates1$date)
      returns2 <- dplyr::filter(others, date %in% dates2$date)
      
      mean <- tryCatch({ stats::t.test(returns1$value, returns2$value) },
                       error = function(e) { NA })
      var <- tryCatch({ var.test(returns1$value, returns2$value, alternative = "two.sided") },
                      error = function(e) { NA })
      
      mean <- dplyr::mutate(
        combination, type = "returns", frequency = x, moment = "mean", 
        p.value = tryCatch({ mean$p.value }, error = function(e) { NA })
      ) %>% dplyr::select(type, frequency, dplyr::everything())
      var <- dplyr::mutate(
        combination, type = "returns", frequency = x, moment = "variance", 
        p.value = tryCatch({ var$p.value }, error = function(e) { NA })
      ) %>% dplyr::select(type, frequency, dplyr::everything())
      
      dplyr::bind_rows(mean, var)
      
    })
    
    returns <- dplyr::bind_rows(daily, others)
    
    dplyr::bind_rows(levels, returns) %>% 
      dplyr::mutate(regime = z) %>% dplyr::relocate(regime, .before = moment)
    
  })
  
  dplyr::bind_rows(whole, regimes)
} %>% dplyr::arrange(
  type, frequency, `active contract ticker`, field, `period 1`, `period 2`, regime, moment
)



#### by country ####
countries <- dplyr::left_join(
  `market data`,
  dplyr::left_join(
    dplyr::select(tickers_futures, ticker, MIC), 
    dplyr::select(exchanges, MIC, country), 
    by = "MIC"
  ) %>% dplyr::rename(`active contract ticker` = ticker), 
  by = "active contract ticker"
) %>% dplyr::distinct(country)

combinations <- t(combn(unique(periods$period), 2L)) %>% as.data.frame() %>%
  rlang::set_names(c("period 1", "period 2"))
combinations <- tidyr::expand_grid(
  countries, dplyr::distinct(`market data`, field), combinations
) %>% dplyr::arrange(country, field)


countries <- foreach(
  y = 1L:nrow(combinations), .combine = rbind
  # y = 1L:50L, .combine = rbind
) %dopar% {
  library(magrittr); library(lubridate)
  
  message(paste(y, nrow(combinations), sep = "/"))
  
  combination <- dplyr::slice(combinations, y)
  
  start1 <- dplyr::filter(
    periods, period == combination$`period 1`, bound == "start"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  end1 <- dplyr::filter(
    periods, period == combination$`period 1`, bound == "end"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  start2 <- dplyr::filter(
    periods, period == combination$`period 2`, bound == "start"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  end2 <- dplyr::filter(
    periods, period == combination$`period 2`, bound == "end"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  
  data <- dplyr::left_join(
    `market data`,
    dplyr::left_join(
      dplyr::select(tickers_futures, ticker, MIC), 
      dplyr::select(exchanges, MIC, country), 
      by = "MIC"
    ) %>% dplyr::rename(`active contract ticker` = ticker), 
    by = "active contract ticker"
  ) %>% 
    dplyr::filter(
      country == combination$country, field == combination$field,
      date >= as.Date(start1), date <= as.Date(end2)
    ) %>% dplyr::select(-c(country, MIC)) 
  
  levels <- dplyr::group_by(data, date) %>% 
    dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>% dplyr::select(date, value) %>%
    dplyr::arrange(date) %>% na.omit()
  
  
  # Levels 
  levels1 <- dplyr::filter(levels, date >= as.Date(start1), date <= as.Date(end1))
  levels2 <- dplyr::filter(levels, date >= as.Date(start2), date <= as.Date(end2))
  
  mean <- tryCatch(
    { stats::t.test(levels1$value, levels2$value) }, error = function(e) { NA }
  )
  var <- tryCatch(
    { var.test(levels1$value, levels2$value, alternative = "two.sided") }, 
    error = function(e) { NA }
  )
  
  mean <- dplyr::mutate(
    combination, type = "levels", frequency = NA, moment = "mean", 
    p.value = tryCatch({ mean$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  var <- dplyr::mutate(
    combination, type = "levels", frequency = NA, moment = "variance", 
    p.value = tryCatch({ var$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  
  results_levels <- dplyr::bind_rows(mean, var)
  
  
  # returns
  ## days
  days <- dplyr::group_by(data, `active contract ticker`) %>% 
    dplyr::mutate(
      value = (value / dplyr::lag(value, 1L) - 1L), 
      value = ifelse(is.infinite(value), NA, value)
    ) %>% dplyr::ungroup() %>% dplyr::group_by(date) %>%
    dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>% dplyr::select(date, value) %>%
    dplyr::arrange(date) %>% na.omit()
  
  returns1 <- dplyr::filter(days, date >= as.Date(start1), date <= as.Date(end1))
  returns2 <- dplyr::filter(days, date >= as.Date(start2), date <= as.Date(end2))
  
  mean <- tryCatch(
    { stats::t.test(returns1$value, returns2$value) }, error = function(e) { NA }
  )
  var <- tryCatch(
    { var.test(returns1$value, returns2$value, alternative = "two.sided") }, 
    error = function(e) { NA }
  )
  
  mean <- dplyr::mutate(
    combination, type = "returns", frequency = "daily", moment = "mean", 
    p.value = tryCatch({ mean$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  var <- dplyr::mutate(
    combination, type = "returns", frequency = "daily", moment = "variance", 
    p.value = tryCatch({ var$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  
  daily <- dplyr::bind_rows(mean, var)
  
  ## other frequencies
  others <- purrr::map_df(c("week", "month"), function(x){
    
    others <- dplyr::mutate(
      data, year = year(date), unit = do.call(what = x, args = list(date))
    ) %>% 
      dplyr::group_by(year, unit) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
      dplyr::ungroup() %>% dplyr::group_by(`active contract ticker`) %>%
      dplyr::mutate(
        value = (value / dplyr::lag(value, 1L) - 1L), 
        value = ifelse(is.infinite(value), NA, value)
      ) %>% dplyr::ungroup() %>% dplyr::select(-unit) %>% 
      dplyr::group_by(date) %>%
      dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
      dplyr::ungroup() %>% dplyr::select(date, value) %>%
      dplyr::arrange(date) %>% na.omit()
    
    returns1 <- dplyr::filter(
      others, date >= as.Date(start1), date <= as.Date(end1)
    )
    returns2 <- dplyr::filter(
      others, date >= as.Date(start2), date <= as.Date(end2)
    )
    
    mean <- tryCatch(
      { stats::t.test(returns1$value, returns2$value) }, error = function(e) { NA }
    )
    var <-  tryCatch(
      { var.test(returns1$value, returns2$value, alternative = "two.sided") }, 
      error = function(e) { NA }
    )
    
    mean <- dplyr::mutate(
      combination, type = "returns", frequency = x, moment = "mean", 
      p.value = tryCatch({ mean$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    var <- dplyr::mutate(
      combination, type = "returns", frequency = x, moment = "variance", 
      p.value = tryCatch({ var$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    
    dplyr::bind_rows(mean, var)
    
  })
  
  results_returns <- dplyr::bind_rows(daily, others)
  
  whole <- dplyr::bind_rows(results_levels, results_returns) %>% 
    dplyr::mutate(regime = "all") %>% dplyr::relocate(regime, .before = moment)
  
  
  # browser()
  regimes <- purrr::map_df(c("backwardation", "contango"), function(z){
    
    # browser()
    dates1 <- dplyr::filter(
      `aggregate CHP regimes`, period == combination$`period 1`, regime == z
    ) %>% dplyr::select(date)
    dates2 <- dplyr::filter(
      `aggregate CHP regimes`, period == combination$`period 2`, regime == z
    ) %>% dplyr::select(date)
    
    # Levels 
    levels1 <- dplyr::filter(levels, date %in% dates1$date)
    levels2 <- dplyr::filter(levels, date %in% dates2$date)
    
    mean <- tryCatch(
      { stats::t.test(levels1$value, levels2$value) }, error = function(e) { NA }
    )
    var <- tryCatch(
      { var.test(levels1$value, levels2$value, alternative = "two.sided") }, 
      error = function(e) { NA }
    )
    
    mean <- dplyr::mutate(
      combination, type = "levels", frequency = NA, moment = "mean", 
      p.value = tryCatch({ mean$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    var <- dplyr::mutate(
      combination, type = "levels", frequency = NA, moment = "variance", 
      p.value = tryCatch({ var$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    
    results_levels <- dplyr::bind_rows(mean, var)
    
    
    # returns
    ## days
    returns1 <- dplyr::filter(days, date %in% dates1$date)
    returns2 <- dplyr::filter(days, date %in% dates2$date)
    
    mean <- tryCatch({ stats::t.test(returns1$value, returns2$value) },
                     error = function(e) { NA })
    var <- tryCatch({ var.test(returns1$value, returns2$value, alternative = "two.sided") },
                    error = function(e) { NA })
    
    mean <- dplyr::mutate(
      combination, type = "returns", frequency = "daily", moment = "mean", 
      p.value = tryCatch({ mean$p.value }, error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    var <- dplyr::mutate(
      combination, type = "returns", frequency = "daily", moment = "variance", 
      p.value = tryCatch({ var$p.value }, error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    
    daily <- dplyr::bind_rows(mean, var)
    
    ## other frequencies
    others <- purrr::map_df(c("week", "month"), function(x){
      
      others <- dplyr::mutate(
        data, year = year(date), unit = do.call(what = x, args = list(date))
      ) %>% dplyr::group_by(year, unit) %>%
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
        dplyr::ungroup() %>% dplyr::group_by(`active contract ticker`) %>%
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L), 
          value = ifelse(is.infinite(value), NA, value)
        ) %>% dplyr::ungroup() %>% dplyr::select(-unit) %>% 
        dplyr::group_by(date) %>%
        dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
        dplyr::ungroup() %>% dplyr::select(date, value) %>%
        dplyr::arrange(date) %>% na.omit()
      
      returns1 <- dplyr::filter(others, date %in% dates1$date)
      returns2 <- dplyr::filter(others, date %in% dates2$date)
      
      mean <- tryCatch({ stats::t.test(returns1$value, returns2$value) },
                       error = function(e) { NA })
      var <- tryCatch({ var.test(returns1$value, returns2$value, alternative = "two.sided") },
                      error = function(e) { NA })
      
      mean <- dplyr::mutate(
        combination, type = "returns", frequency = x, moment = "mean", 
        p.value = tryCatch({ mean$p.value }, error = function(e) { NA })
      ) %>% dplyr::select(type, frequency, dplyr::everything())
      var <- dplyr::mutate(
        combination, type = "returns", frequency = x, moment = "variance", 
        p.value = tryCatch({ var$p.value }, error = function(e) { NA })
      ) %>% dplyr::select(type, frequency, dplyr::everything())
      
      dplyr::bind_rows(mean, var)
      
    })
    
    results_returns <- dplyr::bind_rows(daily, others)
    
    dplyr::bind_rows(results_levels, results_returns) %>% 
      dplyr::mutate(regime = z) %>% dplyr::relocate(regime, .before = moment)
    
  })
  
  dplyr::bind_rows(whole, regimes) %>% 
    dplyr::mutate(sector = "all", subsector = "all") %>%
    dplyr::relocate(sector, subsector, .after = country)
} %>% dplyr::arrange(
  type, frequency, country, sector, subsector, field, `period 1`, `period 2`, 
  regime, moment
)




#### by sector ####
sectors <- dplyr::left_join(
  `market data`,
  dplyr::left_join(
    dplyr::select(tickers_futures, ticker, MIC, sector), 
    dplyr::select(exchanges, MIC, country), 
    by = "MIC"
  ) %>% dplyr::rename(`active contract ticker` = ticker), 
  by = "active contract ticker"
) %>% dplyr::distinct(country, sector) %>% dplyr::select(country, sector)

combinations <- t(combn(unique(periods$period), 2L)) %>% as.data.frame() %>%
  rlang::set_names(c("period 1", "period 2"))
combinations <- tidyr::expand_grid(
  sectors, dplyr::distinct(`market data`, field), combinations
) %>% dplyr::arrange(country, sector, field)


sectors <- foreach(
  y = 1L:nrow(combinations), .combine = rbind
  # y = 1L:50L, .combine = rbind
) %dopar% {
  library(magrittr); library(lubridate)
  
  message(paste(y, nrow(combinations), sep = "/"))
  
  combination <- dplyr::slice(combinations, y)
  
  start1 <- dplyr::filter(
    periods, period == combination$`period 1`, bound == "start"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  end1 <- dplyr::filter(
    periods, period == combination$`period 1`, bound == "end"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  start2 <- dplyr::filter(
    periods, period == combination$`period 2`, bound == "start"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  end2 <- dplyr::filter(
    periods, period == combination$`period 2`, bound == "end"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  
  data <- dplyr::left_join(
    `market data`,
    dplyr::left_join(
      dplyr::select(tickers_futures, ticker, MIC, sector), 
      dplyr::select(exchanges, MIC, country), 
      by = "MIC"
    ) %>% dplyr::rename(`active contract ticker` = ticker), 
    by = "active contract ticker"
  ) %>% 
    dplyr::filter(
      country == combination$country, sector == combination$sector, 
      field == combination$field,
      date >= as.Date(start1), date <= as.Date(end2)
    ) %>% dplyr::select(-c(country, MIC, sector)) 
  
  levels <- dplyr::group_by(data, date) %>% 
    dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>% dplyr::select(date, value) %>%
    dplyr::arrange(date) %>% na.omit()
  
  
  # Levels 
  levels1 <- dplyr::filter(levels, date >= as.Date(start1), date <= as.Date(end1))
  levels2 <- dplyr::filter(levels, date >= as.Date(start2), date <= as.Date(end2))
  
  mean <- tryCatch(
    { stats::t.test(levels1$value, levels2$value) }, error = function(e) { NA }
  )
  var <- tryCatch(
    { var.test(levels1$value, levels2$value, alternative = "two.sided") }, 
    error = function(e) { NA }
  )
  
  mean <- dplyr::mutate(
    combination, type = "levels", frequency = NA, moment = "mean", 
    p.value = tryCatch({ mean$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  var <- dplyr::mutate(
    combination, type = "levels", frequency = NA, moment = "variance", 
    p.value = tryCatch({ var$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  
  results_levels <- dplyr::bind_rows(mean, var)
  
  
  # returns
  ## days
  days <- dplyr::group_by(data, `active contract ticker`) %>% 
    dplyr::mutate(
      value = (value / dplyr::lag(value, 1L) - 1L), 
      value = ifelse(is.infinite(value), NA, value)
    ) %>% dplyr::ungroup() %>% dplyr::group_by(date) %>%
    dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>% dplyr::select(date, value) %>%
    dplyr::arrange(date) %>% na.omit()
  
  returns1 <- dplyr::filter(days, date >= as.Date(start1), date <= as.Date(end1))
  returns2 <- dplyr::filter(days, date >= as.Date(start2), date <= as.Date(end2))
  
  mean <- tryCatch(
    { stats::t.test(returns1$value, returns2$value) }, error = function(e) { NA }
  )
  var <- tryCatch(
    { var.test(returns1$value, returns2$value, alternative = "two.sided") }, 
    error = function(e) { NA }
  )
  
  mean <- dplyr::mutate(
    combination, type = "returns", frequency = "daily", moment = "mean", 
    p.value = tryCatch({ mean$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  var <- dplyr::mutate(
    combination, type = "returns", frequency = "daily", moment = "variance", 
    p.value = tryCatch({ var$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  
  daily <- dplyr::bind_rows(mean, var)
  
  ## other frequencies
  others <- purrr::map_df(c("week", "month"), function(x){
    
    others <- dplyr::mutate(
      data, year = year(date), unit = do.call(what = x, args = list(date))
    ) %>% 
      dplyr::group_by(year, unit) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
      dplyr::ungroup() %>% dplyr::group_by(`active contract ticker`) %>%
      dplyr::mutate(
        value = (value / dplyr::lag(value, 1L) - 1L), 
        value = ifelse(is.infinite(value), NA, value)
      ) %>% dplyr::ungroup() %>% dplyr::select(-unit) %>% 
      dplyr::group_by(date) %>%
      dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
      dplyr::ungroup() %>% dplyr::select(date, value) %>%
      dplyr::arrange(date) %>% na.omit()
    
    returns1 <- dplyr::filter(
      others, date >= as.Date(start1), date <= as.Date(end1)
    )
    returns2 <- dplyr::filter(
      others, date >= as.Date(start2), date <= as.Date(end2)
    )
    
    mean <- tryCatch(
      { stats::t.test(returns1$value, returns2$value) }, error = function(e) { NA }
    )
    var <-  tryCatch(
      { var.test(returns1$value, returns2$value, alternative = "two.sided") }, 
      error = function(e) { NA }
    )
    
    mean <- dplyr::mutate(
      combination, type = "returns", frequency = x, moment = "mean", 
      p.value = tryCatch({ mean$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    var <- dplyr::mutate(
      combination, type = "returns", frequency = x, moment = "variance", 
      p.value = tryCatch({ var$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    
    dplyr::bind_rows(mean, var)
    
  })
  
  results_returns <- dplyr::bind_rows(daily, others)
  
  whole <- dplyr::bind_rows(results_levels, results_returns) %>% 
    dplyr::mutate(regime = "all") %>% dplyr::relocate(regime, .before = moment)
  
  
  # browser()
  regimes <- purrr::map_df(c("backwardation", "contango"), function(z){
    
    # browser()
    dates1 <- dplyr::filter(
      `aggregate CHP regimes`, period == combination$`period 1`, regime == z
    ) %>% dplyr::select(date)
    dates2 <- dplyr::filter(
      `aggregate CHP regimes`, period == combination$`period 2`, regime == z
    ) %>% dplyr::select(date)
    
    # Levels 
    levels1 <- dplyr::filter(levels, date %in% dates1$date)
    levels2 <- dplyr::filter(levels, date %in% dates2$date)
    
    mean <- tryCatch(
      { stats::t.test(levels1$value, levels2$value) }, error = function(e) { NA }
    )
    var <- tryCatch(
      { var.test(levels1$value, levels2$value, alternative = "two.sided") }, 
      error = function(e) { NA }
    )
    
    mean <- dplyr::mutate(
      combination, type = "levels", frequency = NA, moment = "mean", 
      p.value = tryCatch({ mean$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    var <- dplyr::mutate(
      combination, type = "levels", frequency = NA, moment = "variance", 
      p.value = tryCatch({ var$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    
    results_levels <- dplyr::bind_rows(mean, var)
    
    
    # returns
    ## days
    returns1 <- dplyr::filter(days, date %in% dates1$date)
    returns2 <- dplyr::filter(days, date %in% dates2$date)
    
    mean <- tryCatch({ stats::t.test(returns1$value, returns2$value) },
                     error = function(e) { NA })
    var <- tryCatch({ var.test(returns1$value, returns2$value, alternative = "two.sided") },
                    error = function(e) { NA })
    
    mean <- dplyr::mutate(
      combination, type = "returns", frequency = "daily", moment = "mean", 
      p.value = tryCatch({ mean$p.value }, error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    var <- dplyr::mutate(
      combination, type = "returns", frequency = "daily", moment = "variance", 
      p.value = tryCatch({ var$p.value }, error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    
    daily <- dplyr::bind_rows(mean, var)
    
    ## other frequencies
    others <- purrr::map_df(c("week", "month"), function(x){
      
      others <- dplyr::mutate(
        data, year = year(date), unit = do.call(what = x, args = list(date))
      ) %>% dplyr::group_by(year, unit) %>%
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
        dplyr::ungroup() %>% dplyr::group_by(`active contract ticker`) %>%
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L), 
          value = ifelse(is.infinite(value), NA, value)
        ) %>% dplyr::ungroup() %>% dplyr::select(-unit) %>% 
        dplyr::group_by(date) %>%
        dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
        dplyr::ungroup() %>% dplyr::select(date, value) %>%
        dplyr::arrange(date) %>% na.omit()
      
      returns1 <- dplyr::filter(others, date %in% dates1$date)
      returns2 <- dplyr::filter(others, date %in% dates2$date)
      
      mean <- tryCatch({ stats::t.test(returns1$value, returns2$value) },
                       error = function(e) { NA })
      var <- tryCatch({ var.test(returns1$value, returns2$value, alternative = "two.sided") },
                      error = function(e) { NA })
      
      mean <- dplyr::mutate(
        combination, type = "returns", frequency = x, moment = "mean", 
        p.value = tryCatch({ mean$p.value }, error = function(e) { NA })
      ) %>% dplyr::select(type, frequency, dplyr::everything())
      var <- dplyr::mutate(
        combination, type = "returns", frequency = x, moment = "variance", 
        p.value = tryCatch({ var$p.value }, error = function(e) { NA })
      ) %>% dplyr::select(type, frequency, dplyr::everything())
      
      dplyr::bind_rows(mean, var)
      
    })
    
    results_returns <- dplyr::bind_rows(daily, others)
    
    dplyr::bind_rows(results_levels, results_returns) %>% 
      dplyr::mutate(regime = z) %>% dplyr::relocate(regime, .before = moment)
    
  })
  
  dplyr::bind_rows(whole, regimes) %>% 
    dplyr::mutate(subsector = "all") %>%
    dplyr::relocate(subsector, .after = sector)
} %>% dplyr::arrange(
  type, frequency, country, sector, subsector, field, `period 1`, `period 2`, 
  regime, moment
)




#### by subsector ####
subsectors <- dplyr::left_join(
  `market data`,
  dplyr::left_join(
    dplyr::select(tickers_futures, ticker, MIC, sector, subsector), 
    dplyr::select(exchanges, MIC, country), 
    by = "MIC"
  ) %>% dplyr::rename(`active contract ticker` = ticker), 
  by = "active contract ticker"
) %>% dplyr::distinct(country, sector, subsector) %>% 
  dplyr::select(country, sector, subsector)

combinations <- t(combn(unique(periods$period), 2L)) %>% as.data.frame() %>%
  rlang::set_names(c("period 1", "period 2"))
combinations <- tidyr::expand_grid(
  subsectors, dplyr::distinct(`market data`, field), combinations
) %>% dplyr::arrange(country, sector, subsector, field)


subsectors <- foreach(
  y = 1L:nrow(combinations), .combine = rbind
  # y = 1L:50L, .combine = rbind
) %dopar% {
  library(magrittr); library(lubridate)
  
  message(paste(y, nrow(combinations), sep = "/"))
  
  combination <- dplyr::slice(combinations, y)
  
  start1 <- dplyr::filter(
    periods, period == combination$`period 1`, bound == "start"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  end1 <- dplyr::filter(
    periods, period == combination$`period 1`, bound == "end"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  start2 <- dplyr::filter(
    periods, period == combination$`period 2`, bound == "start"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  end2 <- dplyr::filter(
    periods, period == combination$`period 2`, bound == "end"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  
  data <- dplyr::left_join(
    `market data`,
    dplyr::left_join(
      dplyr::select(tickers_futures, ticker, MIC, sector, subsector), 
      dplyr::select(exchanges, MIC, country), 
      by = "MIC"
    ) %>% dplyr::rename(`active contract ticker` = ticker), 
    by = "active contract ticker"
  ) %>% 
    dplyr::filter(
      country == combination$country, sector == combination$sector, 
      subsector == combination$subsector, field == combination$field,
      date >= as.Date(start1), date <= as.Date(end2)
    ) %>% dplyr::select(-c(country, MIC, sector, subsector)) 
  
  levels <- dplyr::group_by(data, date) %>% 
    dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>% dplyr::select(date, value) %>%
    dplyr::arrange(date) %>% na.omit()
  
  
  # Levels 
  levels1 <- dplyr::filter(levels, date >= as.Date(start1), date <= as.Date(end1))
  levels2 <- dplyr::filter(levels, date >= as.Date(start2), date <= as.Date(end2))
  
  mean <- tryCatch(
    { stats::t.test(levels1$value, levels2$value) }, error = function(e) { NA }
  )
  var <- tryCatch(
    { var.test(levels1$value, levels2$value, alternative = "two.sided") }, 
    error = function(e) { NA }
  )
  
  mean <- dplyr::mutate(
    combination, type = "levels", frequency = NA, moment = "mean", 
    p.value = tryCatch({ mean$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  var <- dplyr::mutate(
    combination, type = "levels", frequency = NA, moment = "variance", 
    p.value = tryCatch({ var$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  
  results_levels <- dplyr::bind_rows(mean, var)
  
  
  # returns
  ## days
  days <- dplyr::group_by(data, `active contract ticker`) %>% 
    dplyr::mutate(
      value = (value / dplyr::lag(value, 1L) - 1L), 
      value = ifelse(is.infinite(value), NA, value)
    ) %>% dplyr::ungroup() %>% dplyr::group_by(date) %>%
    dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>% dplyr::select(date, value) %>%
    dplyr::arrange(date) %>% na.omit()
  
  returns1 <- dplyr::filter(days, date >= as.Date(start1), date <= as.Date(end1))
  returns2 <- dplyr::filter(days, date >= as.Date(start2), date <= as.Date(end2))
  
  mean <- tryCatch(
    { stats::t.test(returns1$value, returns2$value) }, error = function(e) { NA }
  )
  var <- tryCatch(
    { var.test(returns1$value, returns2$value, alternative = "two.sided") }, 
    error = function(e) { NA }
  )
  
  mean <- dplyr::mutate(
    combination, type = "returns", frequency = "daily", moment = "mean", 
    p.value = tryCatch({ mean$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  var <- dplyr::mutate(
    combination, type = "returns", frequency = "daily", moment = "variance", 
    p.value = tryCatch({ var$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  
  daily <- dplyr::bind_rows(mean, var)
  
  ## other frequencies
  others <- purrr::map_df(c("week", "month"), function(x){
    
    others <- dplyr::mutate(
      data, year = year(date), unit = do.call(what = x, args = list(date))
    ) %>% 
      dplyr::group_by(year, unit) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
      dplyr::ungroup() %>% dplyr::group_by(`active contract ticker`) %>%
      dplyr::mutate(
        value = (value / dplyr::lag(value, 1L) - 1L), 
        value = ifelse(is.infinite(value), NA, value)
      ) %>% dplyr::ungroup() %>% dplyr::select(-unit) %>% 
      dplyr::group_by(date) %>%
      dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
      dplyr::ungroup() %>% dplyr::select(date, value) %>%
      dplyr::arrange(date) %>% na.omit()
    
    returns1 <- dplyr::filter(
      others, date >= as.Date(start1), date <= as.Date(end1)
    )
    returns2 <- dplyr::filter(
      others, date >= as.Date(start2), date <= as.Date(end2)
    )
    
    mean <- tryCatch(
      { stats::t.test(returns1$value, returns2$value) }, error = function(e) { NA }
    )
    var <-  tryCatch(
      { var.test(returns1$value, returns2$value, alternative = "two.sided") }, 
      error = function(e) { NA }
    )
    
    mean <- dplyr::mutate(
      combination, type = "returns", frequency = x, moment = "mean", 
      p.value = tryCatch({ mean$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    var <- dplyr::mutate(
      combination, type = "returns", frequency = x, moment = "variance", 
      p.value = tryCatch({ var$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    
    dplyr::bind_rows(mean, var)
    
  })
  
  results_returns <- dplyr::bind_rows(daily, others)
  
  whole <- dplyr::bind_rows(results_levels, results_returns) %>% 
    dplyr::mutate(regime = "all") %>% dplyr::relocate(regime, .before = moment)
  
  
  # browser()
  regimes <- purrr::map_df(c("backwardation", "contango"), function(z){
    
    # browser()
    dates1 <- dplyr::filter(
      `aggregate CHP regimes`, period == combination$`period 1`, regime == z
    ) %>% dplyr::select(date)
    dates2 <- dplyr::filter(
      `aggregate CHP regimes`, period == combination$`period 2`, regime == z
    ) %>% dplyr::select(date)
    
    # Levels 
    levels1 <- dplyr::filter(levels, date %in% dates1$date)
    levels2 <- dplyr::filter(levels, date %in% dates2$date)
    
    mean <- tryCatch(
      { stats::t.test(levels1$value, levels2$value) }, error = function(e) { NA }
    )
    var <- tryCatch(
      { var.test(levels1$value, levels2$value, alternative = "two.sided") }, 
      error = function(e) { NA }
    )
    
    mean <- dplyr::mutate(
      combination, type = "levels", frequency = NA, moment = "mean", 
      p.value = tryCatch({ mean$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    var <- dplyr::mutate(
      combination, type = "levels", frequency = NA, moment = "variance", 
      p.value = tryCatch({ var$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    
    results_levels <- dplyr::bind_rows(mean, var)
    
    
    # returns
    ## days
    returns1 <- dplyr::filter(days, date %in% dates1$date)
    returns2 <- dplyr::filter(days, date %in% dates2$date)
    
    mean <- tryCatch({ stats::t.test(returns1$value, returns2$value) },
                     error = function(e) { NA })
    var <- tryCatch({ var.test(returns1$value, returns2$value, alternative = "two.sided") },
                    error = function(e) { NA })
    
    mean <- dplyr::mutate(
      combination, type = "returns", frequency = "daily", moment = "mean", 
      p.value = tryCatch({ mean$p.value }, error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    var <- dplyr::mutate(
      combination, type = "returns", frequency = "daily", moment = "variance", 
      p.value = tryCatch({ var$p.value }, error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    
    daily <- dplyr::bind_rows(mean, var)
    
    ## other frequencies
    others <- purrr::map_df(c("week", "month"), function(x){
      
      others <- dplyr::mutate(
        data, year = year(date), unit = do.call(what = x, args = list(date))
      ) %>% dplyr::group_by(year, unit) %>%
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
        dplyr::ungroup() %>% dplyr::group_by(`active contract ticker`) %>%
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L), 
          value = ifelse(is.infinite(value), NA, value)
        ) %>% dplyr::ungroup() %>% dplyr::select(-unit) %>% 
        dplyr::group_by(date) %>%
        dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
        dplyr::ungroup() %>% dplyr::select(date, value) %>%
        dplyr::arrange(date) %>% na.omit()
      
      returns1 <- dplyr::filter(others, date %in% dates1$date)
      returns2 <- dplyr::filter(others, date %in% dates2$date)
      
      mean <- tryCatch({ stats::t.test(returns1$value, returns2$value) },
                       error = function(e) { NA })
      var <- tryCatch({ var.test(returns1$value, returns2$value, alternative = "two.sided") },
                      error = function(e) { NA })
      
      mean <- dplyr::mutate(
        combination, type = "returns", frequency = x, moment = "mean", 
        p.value = tryCatch({ mean$p.value }, error = function(e) { NA })
      ) %>% dplyr::select(type, frequency, dplyr::everything())
      var <- dplyr::mutate(
        combination, type = "returns", frequency = x, moment = "variance", 
        p.value = tryCatch({ var$p.value }, error = function(e) { NA })
      ) %>% dplyr::select(type, frequency, dplyr::everything())
      
      dplyr::bind_rows(mean, var)
      
    })
    
    results_returns <- dplyr::bind_rows(daily, others)
    
    dplyr::bind_rows(results_levels, results_returns) %>% 
      dplyr::mutate(regime = z) %>% dplyr::relocate(regime, .before = moment)
    
  })
  
  dplyr::bind_rows(whole, regimes)
} %>% dplyr::arrange(
  type, frequency, country, sector, subsector, field, `period 1`, `period 2`, 
  regime, moment
)



`EW portfolios` <- data.table::rbindlist(list(countries, sectors, subsectors))

`market variables` <- tibble::tibble(
  assets = c("individual commodities", "countries - sectors - subsectors"),
  results = list(`individual commodities`, `EW portfolios`)
)

`commodity futures` <- tibble::tibble(
  analysis = "market variables",
  results = list(`market variables`)
)

periods <- tibble::tibble(
  analysis = "commodity futures",
  results = list(`commodity futures`)
)

readr::write_rds(
  periods,
  here::here("explore", "results", "tests-differences.rds")
)


# stats <- readr::read_rds(
#   here::here("explore", "results", "new", "descriptive-statistics.rds")
# )
# stats$results[[1L]]$results[[2L]]$results[[2L]] <- `EW portfolios`
# readr::write_rds(
#   stats,
#   here::here("explore", "results", "new", "descriptive-statistics.rds")
# )




### CFTC ####
pressure <- dplyr::left_join(
  `commodity CFTC data`@data, 
  dplyr::select(tickers_cftc, MIC, format, underlying, unit, participant, position, ticker), 
  by = "ticker"
) %>% 
  dplyr::filter(
    format == "legacy", underlying == "futures only", unit == "contracts", 
    position %in% c("long", "short")
  ) %>%
  dplyr::select(`active contract ticker`, participant, position, date, value) %>% 
  dplyr::group_by(`active contract ticker`, participant) %>%
  tidyr::spread(position, value) %>% dplyr::mutate(pressure = long / (long + short)) %>% 
  dplyr::select(`active contract ticker`, participant, date, pressure) %>% 
  dplyr::ungroup()


#### by single commodity ####
combinations <- t(combn(unique(periods$period), 2L)) %>% as.data.frame() %>%
  rlang::set_names(c("period 1", "period 2"))
combinations <- tidyr::expand_grid(
  dplyr::distinct(pressure, `active contract ticker`, participant),
  combinations
) %>% dplyr::arrange(`active contract ticker`, participant)


`individual commodities` <- foreach(
  y = 1L:nrow(combinations), .combine = rbind
  # y = 1L:50L, .combine = rbind
) %dopar% {
  library(magrittr); library(lubridate)
  
  message(paste(y, nrow(combinations), sep = "/"))
  
  combination <- dplyr::slice(combinations, y)
  
  start1 <- dplyr::filter(
    periods, period == combination$`period 1`, bound == "start"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  end1 <- dplyr::filter(
    periods, period == combination$`period 1`, bound == "end"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  start2 <- dplyr::filter(
    periods, period == combination$`period 2`, bound == "start"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  end2 <- dplyr::filter(
    periods, period == combination$`period 2`, bound == "end"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  
  data <- dplyr::left_join(
    dplyr::select(combination, `active contract ticker`, participant), 
    pressure, by = c("active contract ticker", "participant")
  ) %>% dplyr::filter(date >= as.Date(start1), date <= as.Date(end2)) %>%
    dplyr::rename(value = pressure)
  
  # Levels 
  levels1 <- dplyr::filter(
    data, date >= as.Date(start1), date <= as.Date(end1)
  )
  levels2 <- dplyr::filter(
    data, date >= as.Date(start2), date <= as.Date(end2)
  )
  
  mean <- tryCatch(
    { stats::t.test(levels1$value, levels2$value) }, 
    error = function(e) { NA }
  )
  var <- tryCatch(
    { var.test(levels1$value, levels2$value, alternative = "two.sided") }, 
    error = function(e) { NA }
  )
  
  mean <- dplyr::mutate(
    combination, type = "levels", frequency = NA, moment = "mean", 
    p.value = tryCatch({ mean$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  var <- dplyr::mutate(
    combination, type = "levels", frequency = NA, moment = "variance", 
    p.value = tryCatch({ var$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  
  levels <- dplyr::bind_rows(mean, var)
  
  
  # returns
  ## other frequencies
  returns <- purrr::map_df(c("week", "month"), function(x){
    
    others <- dplyr::mutate(
      data, year = year(date), unit = do.call(what = x, args = list(date))
    ) %>% 
      dplyr::group_by(year, unit) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        value = (value / dplyr::lag(value, 1L) - 1L), 
        value = ifelse(is.infinite(value), NA, value)
      ) %>% dplyr::ungroup() %>% dplyr::select(-unit)
    
    returns1 <- dplyr::filter(
      others, date >= as.Date(start1), date <= as.Date(end1)
    )
    returns2 <- dplyr::filter(
      others, date >= as.Date(start2), date <= as.Date(end2)
    )
    
    mean <- tryCatch(
      { stats::t.test(returns1$value, returns2$value) }, 
      error = function(e) { NA }
    )
    var <-  tryCatch(
      { var.test(returns1$value, returns2$value, alternative = "two.sided") }, 
      error = function(e) { NA }
    )
    
    mean <- dplyr::mutate(
      combination, type = "returns", frequency = x, moment = "mean", 
      p.value = tryCatch({ mean$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    var <- dplyr::mutate(
      combination, type = "returns", frequency = x, moment = "variance", 
      p.value = tryCatch({ var$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    
    dplyr::bind_rows(mean, var)
    
  })
  
  whole <- dplyr::bind_rows(levels, returns) %>% 
    dplyr::mutate(regime = "all") %>% dplyr::relocate(regime, .before = moment)
  
  
  # browser()
  regimes <- purrr::map_df(c("backwardation", "contango"), function(z){
    
    # browser()
    dates1 <- dplyr::filter(
      `aggregate CHP regimes`, period == combination$`period 1`,
      regime == z
    ) %>% dplyr::select(date)
    dates2 <- dplyr::filter(
      `aggregate CHP regimes`, period == combination$`period 2`,
      regime == z
    ) %>% dplyr::select(date)
    
    # Levels 
    levels1 <- dplyr::filter(data, date %in% dates1$date)
    levels2 <- dplyr::filter(data, date %in% dates2$date)
    
    mean <- tryCatch(
      { stats::t.test(levels1$value, levels2$value) }, 
      error = function(e) { NA }
    )
    var <- tryCatch(
      { var.test(levels1$value, levels2$value, alternative = "two.sided") }, 
      error = function(e) { NA }
    )
    
    mean <- dplyr::mutate(
      combination, type = "levels", frequency = NA, moment = "mean", 
      p.value = tryCatch({ mean$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    var <- dplyr::mutate(
      combination, type = "levels", frequency = NA, moment = "variance", 
      p.value = tryCatch({ var$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    
    levels <- dplyr::bind_rows(mean, var)
    
    
    # returns
    ## other frequencies
    returns <- purrr::map_df(c("week", "month"), function(x){
      
      others <- dplyr::mutate(
        data, year = year(date), unit = do.call(what = x, args = list(date))
      ) %>% 
        dplyr::group_by(year, unit) %>%
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L), 
          value = ifelse(is.infinite(value), NA, value)
        ) %>% dplyr::ungroup() %>% dplyr::select(-unit)
      
      returns1 <- dplyr::filter(others, date %in% dates1$date)
      returns2 <- dplyr::filter(others, date %in% dates2$date)
      
      mean <- tryCatch({ stats::t.test(returns1$value, returns2$value) },
                       error = function(e) { NA })
      var <- tryCatch({ var.test(returns1$value, returns2$value, alternative = "two.sided") },
                      error = function(e) { NA })
      
      mean <- dplyr::mutate(
        combination, type = "returns", frequency = x, moment = "mean", 
        p.value = tryCatch({ mean$p.value }, error = function(e) { NA })
      ) %>% dplyr::select(type, frequency, dplyr::everything())
      var <- dplyr::mutate(
        combination, type = "returns", frequency = x, moment = "variance", 
        p.value = tryCatch({ var$p.value }, error = function(e) { NA })
      ) %>% dplyr::select(type, frequency, dplyr::everything())
      
      dplyr::bind_rows(mean, var)
      
    })
    
    dplyr::bind_rows(levels, returns) %>% 
      dplyr::mutate(regime = z) %>% dplyr::relocate(regime, .before = moment)
    
  })
  
  dplyr::bind_rows(whole, regimes)
} %>% dplyr::arrange(
  type, frequency, `active contract ticker`, participant, `period 1`, `period 2`, 
  regime, moment
)



#### by country ####
countries <- dplyr::left_join(
  pressure,
  dplyr::left_join(
    dplyr::select(tickers_futures, ticker, MIC), 
    dplyr::select(exchanges, MIC, country), 
    by = "MIC"
  ) %>% dplyr::rename(`active contract ticker` = ticker), 
  by = "active contract ticker"
) %>% dplyr::distinct(country)

combinations <- t(combn(unique(periods$period), 2L)) %>% as.data.frame() %>%
  rlang::set_names(c("period 1", "period 2"))
combinations <- tidyr::expand_grid(
  countries, dplyr::distinct(pressure, participant), combinations
) %>% dplyr::arrange(country, participant)


countries <- foreach(
  y = 1L:nrow(combinations), .combine = rbind
  # y = 1L:50L, .combine = rbind
) %dopar% {
  library(magrittr); library(lubridate)
  
  message(paste(y, nrow(combinations), sep = "/"))
  
  combination <- dplyr::slice(combinations, y)
  
  start1 <- dplyr::filter(
    periods, period == combination$`period 1`, bound == "start"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  end1 <- dplyr::filter(
    periods, period == combination$`period 1`, bound == "end"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  start2 <- dplyr::filter(
    periods, period == combination$`period 2`, bound == "start"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  end2 <- dplyr::filter(
    periods, period == combination$`period 2`, bound == "end"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  
  data <- dplyr::left_join(
    pressure,
    dplyr::left_join(
      dplyr::select(tickers_futures, ticker, MIC), 
      dplyr::select(exchanges, MIC, country), 
      by = "MIC"
    ) %>% dplyr::rename(`active contract ticker` = ticker), 
    by = "active contract ticker"
  ) %>% 
    dplyr::filter(
      country == combination$country, participant == combination$participant,
      date >= as.Date(start1), date <= as.Date(end2)
    ) %>% dplyr::select(-c(country, MIC)) %>% dplyr::rename(value = pressure)
  
  levels <- dplyr::group_by(data, date) %>% 
    dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>% dplyr::select(date, value) %>%
    dplyr::arrange(date) %>% na.omit()
  
  
  # Levels 
  levels1 <- dplyr::filter(levels, date >= as.Date(start1), date <= as.Date(end1))
  levels2 <- dplyr::filter(levels, date >= as.Date(start2), date <= as.Date(end2))
  
  mean <- tryCatch(
    { stats::t.test(levels1$value, levels2$value) }, error = function(e) { NA }
  )
  var <- tryCatch(
    { var.test(levels1$value, levels2$value, alternative = "two.sided") }, 
    error = function(e) { NA }
  )
  
  mean <- dplyr::mutate(
    combination, type = "levels", frequency = NA, moment = "mean", 
    p.value = tryCatch({ mean$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  var <- dplyr::mutate(
    combination, type = "levels", frequency = NA, moment = "variance", 
    p.value = tryCatch({ var$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  
  results_levels <- dplyr::bind_rows(mean, var)
  
  
  # returns
  ## other frequencies
  results_returns <- purrr::map_df(c("week", "month"), function(x){
    
    others <- dplyr::mutate(
      data, year = year(date), unit = do.call(what = x, args = list(date))
    ) %>% 
      dplyr::group_by(year, unit) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
      dplyr::ungroup() %>% dplyr::group_by(`active contract ticker`) %>%
      dplyr::mutate(
        value = (value / dplyr::lag(value, 1L) - 1L), 
        value = ifelse(is.infinite(value), NA, value)
      ) %>% dplyr::ungroup() %>% dplyr::select(-unit) %>% 
      dplyr::group_by(date) %>%
      dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
      dplyr::ungroup() %>% dplyr::select(date, value) %>%
      dplyr::arrange(date) %>% na.omit()
    
    returns1 <- dplyr::filter(
      others, date >= as.Date(start1), date <= as.Date(end1)
    )
    returns2 <- dplyr::filter(
      others, date >= as.Date(start2), date <= as.Date(end2)
    )
    
    mean <- tryCatch(
      { stats::t.test(returns1$value, returns2$value) }, error = function(e) { NA }
    )
    var <-  tryCatch(
      { var.test(returns1$value, returns2$value, alternative = "two.sided") }, 
      error = function(e) { NA }
    )
    
    mean <- dplyr::mutate(
      combination, type = "returns", frequency = x, moment = "mean", 
      p.value = tryCatch({ mean$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    var <- dplyr::mutate(
      combination, type = "returns", frequency = x, moment = "variance", 
      p.value = tryCatch({ var$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    
    dplyr::bind_rows(mean, var)
    
  })
  
  whole <- dplyr::bind_rows(results_levels, results_returns) %>% 
    dplyr::mutate(regime = "all") %>% dplyr::relocate(regime, .before = moment)
  
  
  # browser()
  regimes <- purrr::map_df(c("backwardation", "contango"), function(z){
    
    # browser()
    dates1 <- dplyr::filter(
      `aggregate CHP regimes`, period == combination$`period 1`, regime == z
    ) %>% dplyr::select(date)
    dates2 <- dplyr::filter(
      `aggregate CHP regimes`, period == combination$`period 2`, regime == z
    ) %>% dplyr::select(date)
    
    # Levels 
    levels1 <- dplyr::filter(levels, date %in% dates1$date)
    levels2 <- dplyr::filter(levels, date %in% dates2$date)
    
    mean <- tryCatch(
      { stats::t.test(levels1$value, levels2$value) }, error = function(e) { NA }
    )
    var <- tryCatch(
      { var.test(levels1$value, levels2$value, alternative = "two.sided") }, 
      error = function(e) { NA }
    )
    
    mean <- dplyr::mutate(
      combination, type = "levels", frequency = NA, moment = "mean", 
      p.value = tryCatch({ mean$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    var <- dplyr::mutate(
      combination, type = "levels", frequency = NA, moment = "variance", 
      p.value = tryCatch({ var$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    
    results_levels <- dplyr::bind_rows(mean, var)
    
    
    # returns
    ## other frequencies
    results_returns <- purrr::map_df(c("week", "month"), function(x){
      
      others <- dplyr::mutate(
        data, year = year(date), unit = do.call(what = x, args = list(date))
      ) %>% dplyr::group_by(year, unit) %>%
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
        dplyr::ungroup() %>% dplyr::group_by(`active contract ticker`) %>%
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L), 
          value = ifelse(is.infinite(value), NA, value)
        ) %>% dplyr::ungroup() %>% dplyr::select(-unit) %>% 
        dplyr::group_by(date) %>%
        dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
        dplyr::ungroup() %>% dplyr::select(date, value) %>%
        dplyr::arrange(date) %>% na.omit()
      
      returns1 <- dplyr::filter(others, date %in% dates1$date)
      returns2 <- dplyr::filter(others, date %in% dates2$date)
      
      mean <- tryCatch({ stats::t.test(returns1$value, returns2$value) },
                       error = function(e) { NA })
      var <- tryCatch({ var.test(returns1$value, returns2$value, alternative = "two.sided") },
                      error = function(e) { NA })
      
      mean <- dplyr::mutate(
        combination, type = "returns", frequency = x, moment = "mean", 
        p.value = tryCatch({ mean$p.value }, error = function(e) { NA })
      ) %>% dplyr::select(type, frequency, dplyr::everything())
      var <- dplyr::mutate(
        combination, type = "returns", frequency = x, moment = "variance", 
        p.value = tryCatch({ var$p.value }, error = function(e) { NA })
      ) %>% dplyr::select(type, frequency, dplyr::everything())
      
      dplyr::bind_rows(mean, var)
      
    })
    
    dplyr::bind_rows(results_levels, results_returns) %>% 
      dplyr::mutate(regime = z) %>% dplyr::relocate(regime, .before = moment)
    
  })
  
  dplyr::bind_rows(whole, regimes) %>% 
    dplyr::mutate(sector = "all", subsector = "all") %>%
    dplyr::relocate(sector, subsector, .after = country)
} %>% dplyr::arrange(
  type, frequency, country, sector, subsector, participant, `period 1`, `period 2`, 
  regime, moment
)






#### by sector ####
sectors <- dplyr::left_join(
  pressure,
  dplyr::left_join(
    dplyr::select(tickers_futures, ticker, MIC, sector), 
    dplyr::select(exchanges, MIC, country), 
    by = "MIC"
  ) %>% dplyr::rename(`active contract ticker` = ticker), 
  by = "active contract ticker"
) %>% dplyr::distinct(country, sector) %>% dplyr::select(country, sector)

combinations <- t(combn(unique(periods$period), 2L)) %>% as.data.frame() %>%
  rlang::set_names(c("period 1", "period 2"))
combinations <- tidyr::expand_grid(
  sectors, dplyr::distinct(pressure, participant), combinations
) %>% dplyr::arrange(country, sector, participant)


sectors <- foreach(
  y = 1L:nrow(combinations), .combine = rbind
  # y = 1L:50L, .combine = rbind
) %dopar% {
  library(magrittr); library(lubridate)
  
  message(paste(y, nrow(combinations), sep = "/"))
  
  combination <- dplyr::slice(combinations, y)
  
  start1 <- dplyr::filter(
    periods, period == combination$`period 1`, bound == "start"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  end1 <- dplyr::filter(
    periods, period == combination$`period 1`, bound == "end"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  start2 <- dplyr::filter(
    periods, period == combination$`period 2`, bound == "start"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  end2 <- dplyr::filter(
    periods, period == combination$`period 2`, bound == "end"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  
  data <- dplyr::left_join(
    pressure,
    dplyr::left_join(
      dplyr::select(tickers_futures, ticker, MIC, sector), 
      dplyr::select(exchanges, MIC, country), 
      by = "MIC"
    ) %>% dplyr::rename(`active contract ticker` = ticker), 
    by = "active contract ticker"
  ) %>% 
    dplyr::filter(
      country == combination$country, sector == combination$sector, 
      participant == combination$participant,
      date >= as.Date(start1), date <= as.Date(end2)
    ) %>% dplyr::select(-c(country, MIC, sector)) %>%
    dplyr::rename(value = pressure)
  
  levels <- dplyr::group_by(data, date) %>% 
    dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>% dplyr::select(date, value) %>%
    dplyr::arrange(date) %>% na.omit()
  
  
  # Levels 
  levels1 <- dplyr::filter(levels, date >= as.Date(start1), date <= as.Date(end1))
  levels2 <- dplyr::filter(levels, date >= as.Date(start2), date <= as.Date(end2))
  
  mean <- tryCatch(
    { stats::t.test(levels1$value, levels2$value) }, error = function(e) { NA }
  )
  var <- tryCatch(
    { var.test(levels1$value, levels2$value, alternative = "two.sided") }, 
    error = function(e) { NA }
  )
  
  mean <- dplyr::mutate(
    combination, type = "levels", frequency = NA, moment = "mean", 
    p.value = tryCatch({ mean$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  var <- dplyr::mutate(
    combination, type = "levels", frequency = NA, moment = "variance", 
    p.value = tryCatch({ var$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  
  results_levels <- dplyr::bind_rows(mean, var)
  
  
  # returns
  ## other frequencies
  results_returns <- purrr::map_df(c("week", "month"), function(x){
    
    others <- dplyr::mutate(
      data, year = year(date), unit = do.call(what = x, args = list(date))
    ) %>% 
      dplyr::group_by(year, unit) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
      dplyr::ungroup() %>% dplyr::group_by(`active contract ticker`) %>%
      dplyr::mutate(
        value = (value / dplyr::lag(value, 1L) - 1L), 
        value = ifelse(is.infinite(value), NA, value)
      ) %>% dplyr::ungroup() %>% dplyr::select(-unit) %>% 
      dplyr::group_by(date) %>%
      dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
      dplyr::ungroup() %>% dplyr::select(date, value) %>%
      dplyr::arrange(date) %>% na.omit()
    
    returns1 <- dplyr::filter(
      others, date >= as.Date(start1), date <= as.Date(end1)
    )
    returns2 <- dplyr::filter(
      others, date >= as.Date(start2), date <= as.Date(end2)
    )
    
    mean <- tryCatch(
      { stats::t.test(returns1$value, returns2$value) }, error = function(e) { NA }
    )
    var <-  tryCatch(
      { var.test(returns1$value, returns2$value, alternative = "two.sided") }, 
      error = function(e) { NA }
    )
    
    mean <- dplyr::mutate(
      combination, type = "returns", frequency = x, moment = "mean", 
      p.value = tryCatch({ mean$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    var <- dplyr::mutate(
      combination, type = "returns", frequency = x, moment = "variance", 
      p.value = tryCatch({ var$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    
    dplyr::bind_rows(mean, var)
  })
  
  whole <- dplyr::bind_rows(results_levels, results_returns) %>% 
    dplyr::mutate(regime = "all") %>% dplyr::relocate(regime, .before = moment)
  
  
  # browser()
  regimes <- purrr::map_df(c("backwardation", "contango"), function(z){
    
    # browser()
    dates1 <- dplyr::filter(
      `aggregate CHP regimes`, period == combination$`period 1`, regime == z
    ) %>% dplyr::select(date)
    dates2 <- dplyr::filter(
      `aggregate CHP regimes`, period == combination$`period 2`, regime == z
    ) %>% dplyr::select(date)
    
    # Levels 
    levels1 <- dplyr::filter(levels, date %in% dates1$date)
    levels2 <- dplyr::filter(levels, date %in% dates2$date)
    
    mean <- tryCatch(
      { stats::t.test(levels1$value, levels2$value) }, error = function(e) { NA }
    )
    var <- tryCatch(
      { var.test(levels1$value, levels2$value, alternative = "two.sided") }, 
      error = function(e) { NA }
    )
    
    mean <- dplyr::mutate(
      combination, type = "levels", frequency = NA, moment = "mean", 
      p.value = tryCatch({ mean$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    var <- dplyr::mutate(
      combination, type = "levels", frequency = NA, moment = "variance", 
      p.value = tryCatch({ var$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    
    results_levels <- dplyr::bind_rows(mean, var)
    
    
    # returns
    ## other frequencies
    results_returns <- purrr::map_df(c("week", "month"), function(x){
      
      others <- dplyr::mutate(
        data, year = year(date), unit = do.call(what = x, args = list(date))
      ) %>% dplyr::group_by(year, unit) %>%
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
        dplyr::ungroup() %>% dplyr::group_by(`active contract ticker`) %>%
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L), 
          value = ifelse(is.infinite(value), NA, value)
        ) %>% dplyr::ungroup() %>% dplyr::select(-unit) %>% 
        dplyr::group_by(date) %>%
        dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
        dplyr::ungroup() %>% dplyr::select(date, value) %>%
        dplyr::arrange(date) %>% na.omit()
      
      returns1 <- dplyr::filter(others, date %in% dates1$date)
      returns2 <- dplyr::filter(others, date %in% dates2$date)
      
      mean <- tryCatch({ stats::t.test(returns1$value, returns2$value) },
                       error = function(e) { NA })
      var <- tryCatch({ var.test(returns1$value, returns2$value, alternative = "two.sided") },
                      error = function(e) { NA })
      
      mean <- dplyr::mutate(
        combination, type = "returns", frequency = x, moment = "mean", 
        p.value = tryCatch({ mean$p.value }, error = function(e) { NA })
      ) %>% dplyr::select(type, frequency, dplyr::everything())
      var <- dplyr::mutate(
        combination, type = "returns", frequency = x, moment = "variance", 
        p.value = tryCatch({ var$p.value }, error = function(e) { NA })
      ) %>% dplyr::select(type, frequency, dplyr::everything())
      
      dplyr::bind_rows(mean, var)
    })
    
    dplyr::bind_rows(results_levels, results_returns) %>% 
      dplyr::mutate(regime = z) %>% dplyr::relocate(regime, .before = moment)
    
  })
  
  dplyr::bind_rows(whole, regimes) %>% 
    dplyr::mutate(subsector = "all") %>%
    dplyr::relocate(subsector, .after = sector)
} %>% dplyr::arrange(
  type, frequency, country, sector, subsector, participant, `period 1`, `period 2`, 
  regime, moment
)







#### by subsector ####
subsectors <- dplyr::left_join(
  pressure,
  dplyr::left_join(
    dplyr::select(tickers_futures, ticker, MIC, sector, subsector), 
    dplyr::select(exchanges, MIC, country), 
    by = "MIC"
  ) %>% dplyr::rename(`active contract ticker` = ticker), 
  by = "active contract ticker"
) %>% dplyr::distinct(country, sector, subsector) %>% 
  dplyr::select(country, sector, subsector)

combinations <- t(combn(unique(periods$period), 2L)) %>% as.data.frame() %>%
  rlang::set_names(c("period 1", "period 2"))
combinations <- tidyr::expand_grid(
  subsectors, dplyr::distinct(pressure, participant), combinations
) %>% dplyr::arrange(country, sector, subsector, participant)


subsectors <- foreach(
  y = 1L:nrow(combinations), .combine = rbind
  # y = 1L:50L, .combine = rbind
) %dopar% {
  library(magrittr); library(lubridate)
  
  message(paste(y, nrow(combinations), sep = "/"))
  
  combination <- dplyr::slice(combinations, y)
  
  start1 <- dplyr::filter(
    periods, period == combination$`period 1`, bound == "start"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  end1 <- dplyr::filter(
    periods, period == combination$`period 1`, bound == "end"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  start2 <- dplyr::filter(
    periods, period == combination$`period 2`, bound == "start"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  end2 <- dplyr::filter(
    periods, period == combination$`period 2`, bound == "end"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  
  data <- dplyr::left_join(
    pressure,
    dplyr::left_join(
      dplyr::select(tickers_futures, ticker, MIC, sector, subsector), 
      dplyr::select(exchanges, MIC, country), 
      by = "MIC"
    ) %>% dplyr::rename(`active contract ticker` = ticker), 
    by = "active contract ticker"
  ) %>% 
    dplyr::filter(
      country == combination$country, sector == combination$sector, 
      subsector == combination$subsector, 
      participant == combination$participant,
      date >= as.Date(start1), date <= as.Date(end2)
    ) %>% dplyr::select(-c(country, MIC, sector, subsector)) %>%
    dplyr::rename(value = pressure)
  
  levels <- dplyr::group_by(data, date) %>% 
    dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>% dplyr::select(date, value) %>%
    dplyr::arrange(date) %>% na.omit()
  
  
  # Levels 
  levels1 <- dplyr::filter(levels, date >= as.Date(start1), date <= as.Date(end1))
  levels2 <- dplyr::filter(levels, date >= as.Date(start2), date <= as.Date(end2))
  
  mean <- tryCatch(
    { stats::t.test(levels1$value, levels2$value) }, error = function(e) { NA }
  )
  var <- tryCatch(
    { var.test(levels1$value, levels2$value, alternative = "two.sided") }, 
    error = function(e) { NA }
  )
  
  mean <- dplyr::mutate(
    combination, type = "levels", frequency = NA, moment = "mean", 
    p.value = tryCatch({ mean$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  var <- dplyr::mutate(
    combination, type = "levels", frequency = NA, moment = "variance", 
    p.value = tryCatch({ var$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  
  results_levels <- dplyr::bind_rows(mean, var)
  
  
  # returns
  ## other frequencies
  results_returns <- purrr::map_df(c("week", "month"), function(x){
    
    others <- dplyr::mutate(
      data, year = year(date), unit = do.call(what = x, args = list(date))
    ) %>% 
      dplyr::group_by(year, unit) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
      dplyr::ungroup() %>% dplyr::group_by(`active contract ticker`) %>%
      dplyr::mutate(
        value = (value / dplyr::lag(value, 1L) - 1L), 
        value = ifelse(is.infinite(value), NA, value)
      ) %>% dplyr::ungroup() %>% dplyr::select(-unit) %>% 
      dplyr::group_by(date) %>%
      dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
      dplyr::ungroup() %>% dplyr::select(date, value) %>%
      dplyr::arrange(date) %>% na.omit()
    
    returns1 <- dplyr::filter(
      others, date >= as.Date(start1), date <= as.Date(end1)
    )
    returns2 <- dplyr::filter(
      others, date >= as.Date(start2), date <= as.Date(end2)
    )
    
    mean <- tryCatch(
      { stats::t.test(returns1$value, returns2$value) }, error = function(e) { NA }
    )
    var <-  tryCatch(
      { var.test(returns1$value, returns2$value, alternative = "two.sided") }, 
      error = function(e) { NA }
    )
    
    mean <- dplyr::mutate(
      combination, type = "returns", frequency = x, moment = "mean", 
      p.value = tryCatch({ mean$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    var <- dplyr::mutate(
      combination, type = "returns", frequency = x, moment = "variance", 
      p.value = tryCatch({ var$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    
    dplyr::bind_rows(mean, var)
  })
  
  whole <- dplyr::bind_rows(results_levels, results_returns) %>% 
    dplyr::mutate(regime = "all") %>% dplyr::relocate(regime, .before = moment)
  
  
  # browser()
  regimes <- purrr::map_df(c("backwardation", "contango"), function(z){
    
    # browser()
    dates1 <- dplyr::filter(
      `aggregate CHP regimes`, period == combination$`period 1`, regime == z
    ) %>% dplyr::select(date)
    dates2 <- dplyr::filter(
      `aggregate CHP regimes`, period == combination$`period 2`, regime == z
    ) %>% dplyr::select(date)
    
    # Levels 
    levels1 <- dplyr::filter(levels, date %in% dates1$date)
    levels2 <- dplyr::filter(levels, date %in% dates2$date)
    
    mean <- tryCatch(
      { stats::t.test(levels1$value, levels2$value) }, error = function(e) { NA }
    )
    var <- tryCatch(
      { var.test(levels1$value, levels2$value, alternative = "two.sided") }, 
      error = function(e) { NA }
    )
    
    mean <- dplyr::mutate(
      combination, type = "levels", frequency = NA, moment = "mean", 
      p.value = tryCatch({ mean$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    var <- dplyr::mutate(
      combination, type = "levels", frequency = NA, moment = "variance", 
      p.value = tryCatch({ var$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    
    results_levels <- dplyr::bind_rows(mean, var)
    
    
    # returns
    ## other frequencies
    results_returns <- purrr::map_df(c("week", "month"), function(x){
      
      others <- dplyr::mutate(
        data, year = year(date), unit = do.call(what = x, args = list(date))
      ) %>% dplyr::group_by(year, unit) %>%
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
        dplyr::ungroup() %>% dplyr::group_by(`active contract ticker`) %>%
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L), 
          value = ifelse(is.infinite(value), NA, value)
        ) %>% dplyr::ungroup() %>% dplyr::select(-unit) %>% 
        dplyr::group_by(date) %>%
        dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
        dplyr::ungroup() %>% dplyr::select(date, value) %>%
        dplyr::arrange(date) %>% na.omit()
      
      returns1 <- dplyr::filter(others, date %in% dates1$date)
      returns2 <- dplyr::filter(others, date %in% dates2$date)
      
      mean <- tryCatch({ stats::t.test(returns1$value, returns2$value) },
                       error = function(e) { NA })
      var <- tryCatch({ var.test(returns1$value, returns2$value, alternative = "two.sided") },
                      error = function(e) { NA })
      
      mean <- dplyr::mutate(
        combination, type = "returns", frequency = x, moment = "mean", 
        p.value = tryCatch({ mean$p.value }, error = function(e) { NA })
      ) %>% dplyr::select(type, frequency, dplyr::everything())
      var <- dplyr::mutate(
        combination, type = "returns", frequency = x, moment = "variance", 
        p.value = tryCatch({ var$p.value }, error = function(e) { NA })
      ) %>% dplyr::select(type, frequency, dplyr::everything())
      
      dplyr::bind_rows(mean, var)
    })
    
    dplyr::bind_rows(results_levels, results_returns) %>% 
      dplyr::mutate(regime = z) %>% dplyr::relocate(regime, .before = moment)
    
  })
  
  dplyr::bind_rows(whole, regimes)
} %>% dplyr::arrange(
  type, frequency, country, sector, subsector, participant, `period 1`, `period 2`, 
  regime, moment
)


`EW portfolios` <- data.table::rbindlist(list(countries, sectors, subsectors))

`position variables` <- tibble::tibble(
  assets = c("individual commodities", "countries - sectors - subsectors"),
  results = list(`individual commodities`, `EW portfolios`)
)


`commodity futures` <- dplyr::bind_rows(
  `commodity futures`,
  tibble::tibble(
    analysis = "position variables",
    results = list(`position variables`)
  )
)


`by period` <- tibble::tibble(
  analysis = "commodity futures",
  results = list(`commodity futures`)
)










## factors ####
### load data ####
start <- "1996-01-01"; end <- "2018-12-31"

`commodity futures tickers` <- dplyr::left_join(
  dplyr::select(
    tickers_futures, ticker, MIC), dplyr::select(exchanges, MIC, country), 
  by = "MIC"
) %>% 
  dplyr::filter(ticker %in% `commodity futures tickers`, country == "US") %>% 
  dplyr::select(ticker) %>% 
  purrr::flatten_chr()

`commodity futures data` <- pullit::pull_futures_market(
  source = "storethat", type = "term structure",
  active_contract_tickers = `commodity futures tickers`, start = start, end = end, 
  TS_positions = 1L:2L, roll_type = "A", roll_days = 0L, roll_months = 0L,
  roll_adjustment = "N", file = storethat
)

`commodity aggregate data` <- pullit::pull_futures_market(
  source = "storethat", type = "aggregate", 
  active_contract_tickers = `commodity futures tickers`, start = start, end = end, 
  file = storethat
)
`commodity CFTC tickers` <- `commodity futures tickers`[
  `commodity futures tickers` %in% tickers_cftc$`active contract ticker`
]
`commodity CFTC data` <- pullit::pull_futures_CFTC(
  source = "storethat", active_contract_tickers = `commodity CFTC tickers`, 
  start = start, end = end, file = storethat
)


### construct factors - asset pool: US commodities ####

update_frequency = "week"
return_frequency <- "day"
ranking_period <- 26L
long_threshold <- 2/3
short_threshold <- 1/3
weighted = TRUE

market <- factorem::market_factor(
  data = `commodity futures data`, 
  return_frequency = return_frequency, 
  long = T
)

CHP <- factorem::CHP_factor(
  price_data = `commodity futures data`, CHP_data = `commodity CFTC data`, 
  update_frequency = update_frequency, return_frequency = return_frequency,
  ranking_period = ranking_period, long_threshold = long_threshold, 
  short_threshold = short_threshold, weighted = weighted
)

`open interest nearby` <- factorem::OI_nearby_factor(
  data = `commodity futures data`, update_frequency = update_frequency, 
  return_frequency = return_frequency, ranking_period = ranking_period, 
  long_threshold = long_threshold, short_threshold = short_threshold, 
  weighted = weighted
)

`open interest aggregate` <- factorem::OI_aggregate_factor(
  price_data = `commodity futures data`, 
  aggregate_data = `commodity aggregate data`, update_frequency = update_frequency,
  return_frequency = return_frequency, ranking_period = ranking_period, 
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
    market, CHP, `open interest nearby`, `open interest aggregate`, 
    `term structure`)
)

returns <- foreach(
  y = 1L:nrow(factors), 
  .combine = dplyr::bind_rows
) %dopar% { 
  
  library(data.table)
  
  factors$factor[[y]]@returns[, `asset pool` := factors$`asset pool`[y]][, name := factors$name[y]] 
  
} %>% 
  tidyr::gather(leg, return, -c(date, name, `asset pool`)) %>% 
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


## factors ####
combinations <- t(combn(unique(periods$period), 2L)) %>% as.data.frame() %>%
  rlang::set_names(c("period 1", "period 2"))
combinations <- tidyr::expand_grid(
  dplyr::distinct(
    returns, 
    `asset pool`, `update frequency`, `return frequency`, `ranking period`, 
    `long threshold`, `short threshold`, name, leg
  ), combinations
) %>% dplyr::arrange(name, leg)


US_commodities <- foreach(
  y = 1L:nrow(combinations), .combine = rbind
  # y = 1L:50L, .combine = rbind
) %dopar% {
  library(magrittr); library(lubridate)
  
  message(paste(y, nrow(combinations), sep = "/"))
  
  combination <- dplyr::slice(combinations, y)
  
  start1 <- dplyr::filter(
    periods, period == combination$`period 1`, bound == "start"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  end1 <- dplyr::filter(
    periods, period == combination$`period 1`, bound == "end"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  start2 <- dplyr::filter(
    periods, period == combination$`period 2`, bound == "start"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  end2 <- dplyr::filter(
    periods, period == combination$`period 2`, bound == "end"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  
  data <- dplyr::left_join(
    dplyr::select(combination, -c(`period 1`, `period 2`)), 
    returns, 
    by = c(
      "asset pool", "update frequency", "return frequency", "ranking period", 
      "long threshold", "short threshold", "name", "leg"
    )
  ) %>% dplyr::filter(date >= as.Date(start1), date <= as.Date(end2))
  
  
  whole1 <- dplyr::filter(
    data, date >= as.Date(start1), date <= as.Date(end1)
  )
  whole2 <- dplyr::filter(
    data, date >= as.Date(start2), date <= as.Date(end2)
  )
  
  mean <- tryCatch(
    { stats::t.test(whole1$return, whole2$return) }, 
    error = function(e) { NA }
  )
  var <- tryCatch(
    { var.test(whole1$return, whole2$return, alternative = "two.sided") }, 
    error = function(e) { NA }
  )
  
  mean <- dplyr::mutate(
    combination, moment = "mean", 
    p.value = tryCatch({ mean$p.value },error = function(e) { NA })
  ) 
  var <- dplyr::mutate(
    combination, moment = "variance", 
    p.value = tryCatch({ var$p.value },error = function(e) { NA })
  ) 
  
  whole <- dplyr::bind_rows(mean, var) %>%
    dplyr::mutate(regime = "all") %>% dplyr::relocate(regime, .before = moment)
  
  
  # browser()
  regimes <- purrr::map_df(c("backwardation", "contango"), function(z){
    
    # browser()
    dates1 <- dplyr::filter(
      `aggregate CHP regimes`, period == combination$`period 1`, regime == z
    ) %>% dplyr::select(date)
    dates2 <- dplyr::filter(
      `aggregate CHP regimes`, period == combination$`period 2`, regime == z
    ) %>% dplyr::select(date)
    
    # Levels 
    regime1 <- dplyr::filter(data, date %in% dates1$date)
    regime2 <- dplyr::filter(data, date %in% dates2$date)
    
    mean <- tryCatch(
      { stats::t.test(regime1$return, regime2$return) }, 
      error = function(e) { NA }
    )
    var <- tryCatch(
      { var.test(regime1$return, regime2$return, alternative = "two.sided") }, 
      error = function(e) { NA }
    )
    
    mean <- dplyr::mutate(
      combination, moment = "mean", 
      p.value = tryCatch({ mean$p.value },error = function(e) { NA })
    ) 
    var <- dplyr::mutate(
      combination, moment = "variance", 
      p.value = tryCatch({ var$p.value },error = function(e) { NA })
    ) 
    
    dplyr::bind_rows(mean, var) %>% 
      dplyr::mutate(regime = z) %>% dplyr::relocate(regime, .before = moment)
    
  })
  
  dplyr::bind_rows(whole, regimes)
} %>% dplyr::arrange(
  name, leg, `period 1`, `period 2`, regime, moment
)


factors <- tibble::tibble(
  `asset pool` = "US commodities", results = list(US_commodities)
)



`by period` <- dplyr::bind_rows(
  `by period`,
  tibble::tibble(
    analysis = "factors", results = list(factors)
  )
)
# `by period` <- readr::read_rds(
#   here::here("explore", "results", "tests-differences.rds")
# )

differences <- tibble::tibble(
  analysis = "periods", results = list(`by period`)
)

readr::write_rds(
  differences,
  here::here("explore", "results", "tests-differences.rds")
)


















# between regimes ####

## commodity futures ####
### market variables ####
#### by single commodity ####
combinations <- tidyr::expand_grid(
  dplyr::distinct(`market data`, `active contract ticker`, field),
  dplyr::distinct(periods, period)
) %>% dplyr::arrange(`active contract ticker`, field, period)


`individual commodities` <- foreach(y = 1L:nrow(combinations), .combine = rbind) %dopar% {
  # `individual commodities` <- foreach(y = 1L:50L, .combine = rbind) %dopar% {
  library(magrittr); library(lubridate)
  
  message(paste(y, nrow(combinations), sep = "/"))
  
  combination <- dplyr::slice(combinations, y)
  
  start <- dplyr::filter(
    periods, period == combination$period, bound == "start"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  end <- dplyr::filter(
    periods, period == combination$period, bound == "end"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  
  data <- dplyr::left_join(
    dplyr::select(combination, `active contract ticker`, field), 
    `market data`, by = c("active contract ticker", "field")
  ) %>% dplyr::filter(date >= as.Date(start), date <= as.Date(end))
  
  
  regimes <- dplyr::filter(`aggregate CHP regimes`, period == combination$period) %>%
    dplyr::select(date, regime)
  
  # Levels 
  backwardation <- dplyr::left_join(data, regimes, by = "date") %>%
    dplyr::filter(
      date >= as.Date(start), date <= as.Date(end), regime == "backwardation"
    )
  contango <- dplyr::left_join(data, regimes, by = "date") %>%
    dplyr::filter(
      date >= as.Date(start), date <= as.Date(end), regime == "contango"
    )
  
  mean <- tryCatch(
    { stats::t.test(backwardation$value, contango$value) }, 
    error = function(e) { NA }
  )
  var <- tryCatch(
    { var.test(backwardation$value, contango$value, alternative = "two.sided") }, 
    error = function(e) { NA }
  )
  
  mean <- dplyr::mutate(
    combination, type = "levels", frequency = NA, moment = "mean", 
    p.value = tryCatch({ mean$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  var <- dplyr::mutate(
    combination, type = "levels", frequency = NA, moment = "variance", 
    p.value = tryCatch({ var$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  
  levels <- dplyr::bind_rows(mean, var)
  
  
  # returns
  ## days
  days <- dplyr::mutate(
    data,
    value = (value / dplyr::lag(value, 1L) - 1L), 
    value = ifelse(is.infinite(value), NA, value)
  ) 
  
  backwardation <- dplyr::left_join(days, regimes, by = "date") %>%
    dplyr::filter(
      date >= as.Date(start), date <= as.Date(end), regime == "backwardation"
    )
  contango <- dplyr::left_join(days, regimes, by = "date") %>%
    dplyr::filter(
      date >= as.Date(start), date <= as.Date(end), regime == "contango"
    )
  
  
  mean <- tryCatch(
    { stats::t.test(backwardation$value, contango$value) }, 
    error = function(e) { NA }
  )
  var <- tryCatch(
    { var.test(backwardation$value, contango$value, alternative = "two.sided") }, 
    error = function(e) { NA }
  )
  
  mean <- dplyr::mutate(
    combination, type = "returns", frequency = "daily", moment = "mean", 
    p.value = tryCatch({ mean$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  var <- dplyr::mutate(
    combination, type = "returns", frequency = "daily", moment = "variance", 
    p.value = tryCatch({ var$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  
  daily <- dplyr::bind_rows(mean, var)
  
  ## other frequencies
  others <- purrr::map_df(c("week", "month"), function(x){
    
    others <- dplyr::mutate(
      data, year = year(date), unit = do.call(what = x, args = list(date))
    ) %>% 
      dplyr::group_by(year, unit) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        value = (value / dplyr::lag(value, 1L) - 1L), 
        value = ifelse(is.infinite(value), NA, value)
      ) %>% dplyr::ungroup() %>% dplyr::select(-unit)
    
    backwardation <- dplyr::left_join(others, regimes, by = "date") %>%
      dplyr::filter(
        date >= as.Date(start), date <= as.Date(end), regime == "backwardation"
      )
    contango <- dplyr::left_join(others, regimes, by = "date") %>%
      dplyr::filter(
        date >= as.Date(start), date <= as.Date(end), regime == "contango"
      )
    
    mean <- tryCatch(
      { stats::t.test(backwardation$value, contango$value) }, 
      error = function(e) { NA }
    )
    var <-  tryCatch(
      { var.test(backwardation$value, contango$value, alternative = "two.sided") }, 
      error = function(e) { NA }
    )
    
    mean <- dplyr::mutate(
      combination, type = "returns", frequency = x, moment = "mean", 
      p.value = tryCatch({ mean$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    var <- dplyr::mutate(
      combination, type = "returns", frequency = x, moment = "variance", 
      p.value = tryCatch({ var$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    
    dplyr::bind_rows(mean, var)
    
  })
  
  returns <- dplyr::bind_rows(daily, others)
  
  dplyr::bind_rows(levels, returns)
  
} %>% dplyr::arrange(
  type, frequency, `active contract ticker`, field, period, moment
)


#### by country ####
countries <- dplyr::left_join(
  `market data`,
  dplyr::left_join(
    dplyr::select(tickers_futures, ticker, MIC), 
    dplyr::select(exchanges, MIC, country), 
    by = "MIC"
  ) %>% dplyr::rename(`active contract ticker` = ticker), 
  by = "active contract ticker"
) %>% dplyr::distinct(country)

combinations <- tidyr::expand_grid(
  countries,
  dplyr::distinct(`market data`, field),
  dplyr::distinct(periods, period)
) %>% dplyr::arrange(country, field, period)


countries <- foreach(y = 1L:nrow(combinations), .combine = rbind) %dopar% {
  # countries <- foreach(y = 1L:10L, .combine = rbind) %dopar% {  
  library(magrittr); library(lubridate)
  
  message(paste(y, nrow(combinations), sep = "/"))
  
  combination <- dplyr::slice(combinations, y)
  
  start <- dplyr::filter(
    periods, period == combination$period, bound == "start"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  end <- dplyr::filter(
    periods, period == combination$period, bound == "end"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  
  
  data <- dplyr::left_join(
    `market data`,
    dplyr::left_join(
      dplyr::select(tickers_futures, ticker, MIC), 
      dplyr::select(exchanges, MIC, country), 
      by = "MIC"
    ) %>% dplyr::rename(`active contract ticker` = ticker), 
    by = "active contract ticker"
  ) %>% 
    dplyr::filter(
      country == combination$country, field == combination$field,
      date >= as.Date(start), date <= as.Date(end)
    ) %>% dplyr::select(-c(country, MIC)) 
  
  levels <- dplyr::group_by(data, date) %>% 
    dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>% dplyr::select(date, value) %>%
    dplyr::arrange(date) %>% na.omit()
  
  regimes <- dplyr::filter(`aggregate CHP regimes`, period == combination$period) %>%
    dplyr::select(date, regime)
  
  # Levels 
  backwardation <- dplyr::left_join(levels, regimes, by = "date") %>%
    dplyr::filter(
      date >= as.Date(start), date <= as.Date(end), regime == "backwardation"
    )
  contango <- dplyr::left_join(levels, regimes, by = "date") %>%
    dplyr::filter(
      date >= as.Date(start), date <= as.Date(end), regime == "contango"
    )
  
  mean <- tryCatch(
    { stats::t.test(backwardation$value, contango$value) }, error = function(e) { NA }
  )
  var <- tryCatch(
    { var.test(backwardation$value, contango$value, alternative = "two.sided") }, 
    error = function(e) { NA }
  )
  
  mean <- dplyr::mutate(
    combination, type = "levels", frequency = NA, moment = "mean", 
    p.value = tryCatch({ mean$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  var <- dplyr::mutate(
    combination, type = "levels", frequency = NA, moment = "variance", 
    p.value = tryCatch({ var$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  
  results_levels <- dplyr::bind_rows(mean, var)
  
  
  # returns
  ## days
  days <- dplyr::group_by(data, `active contract ticker`) %>% 
    dplyr::mutate(
      value = (value / dplyr::lag(value, 1L) - 1L), 
      value = ifelse(is.infinite(value), NA, value)
    ) %>% dplyr::ungroup() %>% dplyr::group_by(date) %>%
    dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>% dplyr::select(date, value) %>%
    dplyr::arrange(date) %>% na.omit()
  
  backwardation <- dplyr::left_join(days, regimes, by = "date") %>%
    dplyr::filter(
      date >= as.Date(start), date <= as.Date(end), regime == "backwardation"
    )
  contango <- dplyr::left_join(days, regimes, by = "date") %>%
    dplyr::filter(
      date >= as.Date(start), date <= as.Date(end), regime == "contango"
    )
  
  
  mean <- tryCatch(
    { stats::t.test(backwardation$value, contango$value) }, error = function(e) { NA }
  )
  var <- tryCatch(
    { var.test(backwardation$value, contango$value, alternative = "two.sided") }, 
    error = function(e) { NA }
  )
  
  mean <- dplyr::mutate(
    combination, type = "returns", frequency = "daily", moment = "mean", 
    p.value = tryCatch({ mean$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  var <- dplyr::mutate(
    combination, type = "returns", frequency = "daily", moment = "variance", 
    p.value = tryCatch({ var$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  
  daily <- dplyr::bind_rows(mean, var)
  
  ## other frequencies
  others <- purrr::map_df(c("week", "month"), function(x){
    
    others <- dplyr::mutate(
      data, year = year(date), unit = do.call(what = x, args = list(date))
    ) %>% 
      dplyr::group_by(year, unit) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
      dplyr::ungroup() %>% dplyr::group_by(`active contract ticker`) %>%
      dplyr::mutate(
        value = (value / dplyr::lag(value, 1L) - 1L), 
        value = ifelse(is.infinite(value), NA, value)
      ) %>% dplyr::ungroup() %>% dplyr::select(-unit) %>% 
      dplyr::group_by(date) %>%
      dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
      dplyr::ungroup() %>% dplyr::select(date, value) %>%
      dplyr::arrange(date) %>% na.omit()
    
    backwardation <- dplyr::left_join(others, regimes, by = "date") %>%
      dplyr::filter(
        date >= as.Date(start), date <= as.Date(end), regime == "backwardation"
      )
    contango <- dplyr::left_join(others, regimes, by = "date") %>%
      dplyr::filter(
        date >= as.Date(start), date <= as.Date(end), regime == "contango"
      )
    
    mean <- tryCatch(
      { stats::t.test(backwardation$value, contango$value) }, error = function(e) { NA }
    )
    var <-  tryCatch(
      { var.test(backwardation$value, contango$value, alternative = "two.sided") }, 
      error = function(e) { NA }
    )
    
    mean <- dplyr::mutate(
      combination, type = "returns", frequency = x, moment = "mean", 
      p.value = tryCatch({ mean$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    var <- dplyr::mutate(
      combination, type = "returns", frequency = x, moment = "variance", 
      p.value = tryCatch({ var$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    
    dplyr::bind_rows(mean, var)
    
  })
  
  results_returns <- dplyr::bind_rows(daily, others)
  
  dplyr::bind_rows(results_levels, results_returns) %>% 
    dplyr::mutate(sector = "all", subsector = "all") %>%
    dplyr::relocate(sector, subsector, .after = country)
} %>% dplyr::arrange(
  type, frequency, country, sector, subsector, field, period, moment
)




#### by sector ####
sectors <- dplyr::left_join(
  `market data`,
  dplyr::left_join(
    dplyr::select(tickers_futures, ticker, MIC, sector), 
    dplyr::select(exchanges, MIC, country), 
    by = "MIC"
  ) %>% dplyr::rename(`active contract ticker` = ticker), 
  by = "active contract ticker"
) %>% dplyr::distinct(country, sector) %>% dplyr::select(country, sector)

combinations <- tidyr::expand_grid(
  sectors,
  dplyr::distinct(`market data`, field),
  dplyr::distinct(periods, period)
) %>% dplyr::arrange(country, sector, field, period)


sectors <- foreach(y = 1L:nrow(combinations), .combine = rbind) %dopar% {
  # sectors <- foreach(y = 1L:50L, .combine = rbind) %dopar% {
  library(magrittr); library(lubridate)
  
  message(paste(y, nrow(combinations), sep = "/"))
  
  combination <- dplyr::slice(combinations, y)
  
  start <- dplyr::filter(
    periods, period == combination$period, bound == "start"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  end <- dplyr::filter(
    periods, period == combination$period, bound == "end"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  
  
  data <- dplyr::left_join(
    `market data`,
    dplyr::left_join(
      dplyr::select(tickers_futures, ticker, MIC, sector), 
      dplyr::select(exchanges, MIC, country), 
      by = "MIC"
    ) %>% dplyr::rename(`active contract ticker` = ticker), 
    by = "active contract ticker"
  ) %>% 
    dplyr::filter(
      country == combination$country, sector == combination$sector, 
      field == combination$field,
      date >= as.Date(start), date <= as.Date(end)
    ) %>% dplyr::select(-c(country, MIC, sector)) 
  
  levels <- dplyr::group_by(data, date) %>% 
    dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>% dplyr::select(date, value) %>%
    dplyr::arrange(date) %>% na.omit()
  
  regimes <- dplyr::filter(`aggregate CHP regimes`, period == combination$period) %>%
    dplyr::select(date, regime)
  
  # Levels 
  backwardation <- dplyr::left_join(levels, regimes, by = "date") %>%
    dplyr::filter(
      date >= as.Date(start), date <= as.Date(end), regime == "backwardation"
    )
  contango <- dplyr::left_join(levels, regimes, by = "date") %>%
    dplyr::filter(
      date >= as.Date(start), date <= as.Date(end), regime == "contango"
    )
  
  mean <- tryCatch(
    { stats::t.test(backwardation$value, contango$value) }, error = function(e) { NA }
  )
  var <- tryCatch(
    { var.test(backwardation$value, contango$value, alternative = "two.sided") }, 
    error = function(e) { NA }
  )
  
  mean <- dplyr::mutate(
    combination, type = "levels", frequency = NA, moment = "mean", 
    p.value = tryCatch({ mean$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  var <- dplyr::mutate(
    combination, type = "levels", frequency = NA, moment = "variance", 
    p.value = tryCatch({ var$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  
  results_levels <- dplyr::bind_rows(mean, var)
  
  
  # returns
  ## days
  days <- dplyr::group_by(data, `active contract ticker`) %>% 
    dplyr::mutate(
      value = (value / dplyr::lag(value, 1L) - 1L), 
      value = ifelse(is.infinite(value), NA, value)
    ) %>% dplyr::ungroup() %>% dplyr::group_by(date) %>%
    dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>% dplyr::select(date, value) %>%
    dplyr::arrange(date) %>% na.omit()
  
  backwardation <- dplyr::left_join(days, regimes, by = "date") %>%
    dplyr::filter(
      date >= as.Date(start), date <= as.Date(end), regime == "backwardation"
    )
  contango <- dplyr::left_join(days, regimes, by = "date") %>%
    dplyr::filter(
      date >= as.Date(start), date <= as.Date(end), regime == "contango"
    )
  
  mean <- tryCatch(
    { stats::t.test(backwardation$value, contango$value) }, error = function(e) { NA }
  )
  var <- tryCatch(
    { var.test(backwardation$value, contango$value, alternative = "two.sided") }, 
    error = function(e) { NA }
  )
  
  mean <- dplyr::mutate(
    combination, type = "returns", frequency = "daily", moment = "mean", 
    p.value = tryCatch({ mean$p.value }, error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  var <- dplyr::mutate(
    combination, type = "returns", frequency = "daily", moment = "variance", 
    p.value = tryCatch({ var$p.value }, error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  
  daily <- dplyr::bind_rows(mean, var)
  
  ## other frequencies
  others <- purrr::map_df(c("week", "month"), function(x){
    
    others <- dplyr::mutate(
      data, year = year(date), unit = do.call(what = x, args = list(date))
    ) %>% 
      dplyr::group_by(year, unit) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
      dplyr::ungroup() %>% dplyr::group_by(`active contract ticker`) %>%
      dplyr::mutate(
        value = (value / dplyr::lag(value, 1L) - 1L), 
        value = ifelse(is.infinite(value), NA, value)
      ) %>% dplyr::ungroup() %>% dplyr::select(-unit) %>% 
      dplyr::group_by(date) %>%
      dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
      dplyr::ungroup() %>% dplyr::select(date, value) %>%
      dplyr::arrange(date) %>% na.omit()
    
    backwardation <- dplyr::left_join(others, regimes, by = "date") %>%
      dplyr::filter(
        date >= as.Date(start), date <= as.Date(end), regime == "backwardation"
      )
    contango <- dplyr::left_join(others, regimes, by = "date") %>%
      dplyr::filter(
        date >= as.Date(start), date <= as.Date(end), regime == "contango"
      )
    
    mean <- tryCatch(
      { stats::t.test(backwardation$value, contango$value) }, error = function(e) { NA }
    )
    var <-  tryCatch(
      { var.test(backwardation$value, contango$value, alternative = "two.sided") }, 
      error = function(e) { NA }
    )
    
    mean <- dplyr::mutate(
      combination, type = "returns", frequency = x, moment = "mean", 
      p.value = tryCatch({ mean$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    var <- dplyr::mutate(
      combination, type = "returns", frequency = x, moment = "variance", 
      p.value = tryCatch({ var$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    
    dplyr::bind_rows(mean, var)
    
  })
  
  results_returns <- dplyr::bind_rows(daily, others)
  
  dplyr::bind_rows(results_levels, results_returns) %>% 
    dplyr::mutate(subsector = "all") %>%
    dplyr::relocate(subsector, .after = sector)
} %>% dplyr::arrange(
  type, frequency, country, sector, subsector, field, period, moment
)



#### by subsector ####
subsectors <- dplyr::left_join(
  `market data`,
  dplyr::left_join(
    dplyr::select(tickers_futures, ticker, MIC, sector, subsector), 
    dplyr::select(exchanges, MIC, country), 
    by = "MIC"
  ) %>% dplyr::rename(`active contract ticker` = ticker), 
  by = "active contract ticker"
) %>% dplyr::distinct(country, sector, subsector) %>% 
  dplyr::select(country, sector, subsector)

combinations <- tidyr::expand_grid(
  subsectors,
  dplyr::distinct(`market data`, field),
  dplyr::distinct(periods, period)
) %>% dplyr::arrange(country, sector, subsector, field, period)



subsectors <- foreach(y = 1L:nrow(combinations), .combine = rbind) %dopar% {
  # subsectors <- foreach(y = 1L:50L, .combine = rbind) %dopar% {
  library(magrittr); library(lubridate)
  
  message(paste(y, nrow(combinations), sep = "/"))
  
  combination <- dplyr::slice(combinations, y)
  
  start <- dplyr::filter(
    periods, period == combination$period, bound == "start"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  end <- dplyr::filter(
    periods, period == combination$period, bound == "end"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  
  data <- dplyr::left_join(
    `market data`,
    dplyr::left_join(
      dplyr::select(tickers_futures, ticker, MIC, sector, subsector), 
      dplyr::select(exchanges, MIC, country), 
      by = "MIC"
    ) %>% dplyr::rename(`active contract ticker` = ticker), 
    by = "active contract ticker"
  ) %>% 
    dplyr::filter(
      country == combination$country, sector == combination$sector, 
      subsector == combination$subsector, field == combination$field,
      date >= as.Date(start), date <= as.Date(end)
    ) %>% dplyr::select(-c(country, MIC, sector, subsector)) 
  
  levels <- dplyr::group_by(data, date) %>% 
    dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>% dplyr::select(date, value) %>%
    dplyr::arrange(date) %>% na.omit()
  
  regimes <- dplyr::filter(`aggregate CHP regimes`, period == combination$period) %>%
    dplyr::select(date, regime)
  
  # Levels 
  backwardation <- dplyr::left_join(levels, regimes, by = "date") %>%
    dplyr::filter(
      date >= as.Date(start), date <= as.Date(end), regime == "backwardation"
    )
  contango <- dplyr::left_join(levels, regimes, by = "date") %>%
    dplyr::filter(
      date >= as.Date(start), date <= as.Date(end), regime == "contango"
    )
  
  mean <- tryCatch(
    { stats::t.test(backwardation$value, contango$value) }, error = function(e) { NA }
  )
  var <- tryCatch(
    { var.test(backwardation$value, contango$value, alternative = "two.sided") }, 
    error = function(e) { NA }
  )
  
  mean <- dplyr::mutate(
    combination, type = "levels", frequency = NA, moment = "mean", 
    p.value = tryCatch({ mean$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  var <- dplyr::mutate(
    combination, type = "levels", frequency = NA, moment = "variance", 
    p.value = tryCatch({ var$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  
  results_levels <- dplyr::bind_rows(mean, var)
  
  
  # returns
  ## days
  days <- dplyr::group_by(data, `active contract ticker`) %>% 
    dplyr::mutate(
      value = (value / dplyr::lag(value, 1L) - 1L), 
      value = ifelse(is.infinite(value), NA, value)
    ) %>% dplyr::ungroup() %>% dplyr::group_by(date) %>%
    dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>% dplyr::select(date, value) %>%
    dplyr::arrange(date) %>% na.omit()
  
  backwardation <- dplyr::left_join(days, regimes, by = "date") %>%
    dplyr::filter(
      date >= as.Date(start), date <= as.Date(end), regime == "backwardation"
    )
  contango <- dplyr::left_join(days, regimes, by = "date") %>%
    dplyr::filter(
      date >= as.Date(start), date <= as.Date(end), regime == "contango"
    )
  
  mean <- tryCatch(
    { stats::t.test(backwardation$value, contango$value) }, error = function(e) { NA }
  )
  var <- tryCatch(
    { var.test(backwardation$value, contango$value, alternative = "two.sided") }, 
    error = function(e) { NA }
  )
  
  mean <- dplyr::mutate(
    combination, type = "returns", frequency = "daily", moment = "mean", 
    p.value = tryCatch({ mean$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  var <- dplyr::mutate(
    combination, type = "returns", frequency = "daily", moment = "variance", 
    p.value = tryCatch({ var$p.value },error = function(e) { NA })
  ) %>% dplyr::select(type, frequency, dplyr::everything())
  
  daily <- dplyr::bind_rows(mean, var)
  
  ## other frequencies
  others <- purrr::map_df(c("week", "month"), function(x){
    
    others <- dplyr::mutate(
      data, year = year(date), unit = do.call(what = x, args = list(date))
    ) %>% 
      dplyr::group_by(year, unit) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
      dplyr::ungroup() %>% dplyr::group_by(`active contract ticker`) %>%
      dplyr::mutate(
        value = (value / dplyr::lag(value, 1L) - 1L), 
        value = ifelse(is.infinite(value), NA, value)
      ) %>% dplyr::ungroup() %>% dplyr::select(-unit) %>% 
      dplyr::group_by(date) %>%
      dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
      dplyr::ungroup() %>% dplyr::select(date, value) %>%
      dplyr::arrange(date) %>% na.omit()
    
    backwardation <- dplyr::left_join(others, regimes, by = "date") %>%
      dplyr::filter(
        date >= as.Date(start), date <= as.Date(end), regime == "backwardation"
      )
    contango <- dplyr::left_join(others, regimes, by = "date") %>%
      dplyr::filter(
        date >= as.Date(start), date <= as.Date(end), regime == "contango"
      )
    
    mean <- tryCatch(
      { stats::t.test(backwardation$value, contango$value) }, error = function(e) { NA }
    )
    var <-  tryCatch(
      { var.test(backwardation$value, contango$value, alternative = "two.sided") }, 
      error = function(e) { NA }
    )
    
    mean <- dplyr::mutate(
      combination, type = "returns", frequency = x, moment = "mean", 
      p.value = tryCatch({ mean$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    var <- dplyr::mutate(
      combination, type = "returns", frequency = x, moment = "variance", 
      p.value = tryCatch({ var$p.value },error = function(e) { NA })
    ) %>% dplyr::select(type, frequency, dplyr::everything())
    
    dplyr::bind_rows(mean, var)
    
  })
  
  results_returns <- dplyr::bind_rows(daily, others)
  
  dplyr::bind_rows(results_levels, results_returns)
  
} %>% dplyr::arrange(
  type, frequency, country, sector, subsector, field, period, moment
)



`EW portfolios` <- data.table::rbindlist(list(countries, sectors, subsectors))

`market variables` <- tibble::tibble(
  assets = c("individual commodities", "countries - sectors - subsectors"),
  results = list(`individual commodities`, `EW portfolios`)
)

`commodity futures` <- tibble::tibble(
  analysis = "market variables",
  results = list(`market variables`)
)

`by regime` <- tibble::tibble(
  analysis = "commodity futures",
  results = list(`commodity futures`)
)


# differences <- readr::read_rds(
#   here::here("explore", "results", "tests-differences.rds")
# )


## factors ####
### load data ####
start <- "1996-01-01"; end <- "2018-12-31"

`commodity futures tickers` <- dplyr::left_join(
  dplyr::select(
    tickers_futures, ticker, MIC), dplyr::select(exchanges, MIC, country), 
  by = "MIC"
) %>% dplyr::filter(ticker %in% `commodity futures tickers`, country == "US") %>% 
  dplyr::select(ticker) %>% purrr::flatten_chr()

`commodity futures data` <- pullit::pull_futures_market(
  source = "storethat", type = "term structure",
  active_contract_tickers = `commodity futures tickers`, start = start, end = end, 
  TS_positions = 1L:2L, roll_type = "A", roll_days = 0L, roll_months = 0L,
  roll_adjustment = "N", file = storethat
)

`commodity aggregate data` <- pullit::pull_futures_market(
  source = "storethat", type = "aggregate", 
  active_contract_tickers = `commodity futures tickers`, start = start, end = end, 
  file = storethat
)
`commodity CFTC tickers` <- `commodity futures tickers`[
  `commodity futures tickers` %in% tickers_cftc$`active contract ticker`
]
`commodity CFTC data` <- pullit::pull_futures_CFTC(
  source = "storethat", active_contract_tickers = `commodity CFTC tickers`, 
  start = start, end = end, file = storethat
)


### construct factors - asset pool: US commodities ####
update_frequency = "week"
return_frequency <- "day"
ranking_period <- 26L
long_threshold <- 2/3
short_threshold <- 1/3
weighted = TRUE

market <- factorem::market_factor(
  data = `commodity futures data`, 
  return_frequency = return_frequency, 
  long = T
)

CHP <- factorem::CHP_factor(
  price_data = `commodity futures data`, CHP_data = `commodity CFTC data`, 
  update_frequency = update_frequency, return_frequency = return_frequency,
  ranking_period = ranking_period, long_threshold = long_threshold, 
  short_threshold = short_threshold, weighted = weighted
)

`open interest nearby` <- factorem::OI_nearby_factor(
  data = `commodity futures data`, update_frequency = update_frequency, 
  return_frequency = return_frequency, ranking_period = ranking_period, 
  long_threshold = long_threshold, short_threshold = short_threshold, 
  weighted = weighted
)

`open interest aggregate` <- factorem::OI_aggregate_factor(
  price_data = `commodity futures data`, 
  aggregate_data = `commodity aggregate data`, update_frequency = update_frequency,
  return_frequency = return_frequency, ranking_period = ranking_period, 
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
    market, CHP, `open interest nearby`, `open interest aggregate`, 
    `term structure`)
)

returns <- foreach(y = 1L:nrow(factors), .combine = dplyr::bind_rows) %dopar% { 
  library(data.table)
  
  factors$factor[[y]]@returns[, `asset pool` := factors$`asset pool`[y]][, name := factors$name[y]] 
  
} %>% 
  tidyr::gather(leg, return, -c(date, name, `asset pool`)) %>% 
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


## factors ####
combinations <- tidyr::expand_grid(
  dplyr::distinct(
    returns, 
    `asset pool`, `update frequency`, `return frequency`, `ranking period`, 
    `long threshold`, `short threshold`, name, leg
  ), 
  dplyr::distinct(periods, period)
) %>% dplyr::arrange(name, leg, period)


US_commodities <- foreach(y = 1L:nrow(combinations), .combine = rbind) %dopar% {
  # US_commodities <- foreach(y = 1L:50L, .combine = rbind) %dopar% {
  library(magrittr); library(lubridate)
  
  message(paste(y, nrow(combinations), sep = "/"))
  
  combination <- dplyr::slice(combinations, y)
  
  start <- dplyr::filter(
    periods, period == combination$period, bound == "start"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  end <- dplyr::filter(
    periods, period == combination$period, bound == "end"
  ) %>% dplyr::select(date) %>% purrr::flatten_chr()
  
  data <- dplyr::left_join(
    dplyr::select(combination, -c(period)), 
    returns, 
    by = c(
      "asset pool", "update frequency", "return frequency", "ranking period", 
      "long threshold", "short threshold", "name", "leg"
    )
  ) %>% dplyr::filter(date >= as.Date(start), date <= as.Date(end))
  
  regimes <- dplyr::filter(`aggregate CHP regimes`, period == combination$period) %>%
    dplyr::select(date, regime)
  
  backwardation <- dplyr::left_join(data, regimes, by = "date") %>%
    dplyr::filter(
      date >= as.Date(start), date <= as.Date(end), regime == "backwardation"
    )
  contango <- dplyr::left_join(data, regimes, by = "date") %>%
    dplyr::filter(
      date >= as.Date(start), date <= as.Date(end), regime == "contango"
    )
  
  mean <- tryCatch(
    { stats::t.test(backwardation$return, contango$return) }, 
    error = function(e) { NA }
  )
  var <- tryCatch(
    { var.test(backwardation$return, contango$return, alternative = "two.sided") }, 
    error = function(e) { NA }
  )
  
  mean <- dplyr::mutate(
    combination, moment = "mean", 
    p.value = tryCatch({ mean$p.value },error = function(e) { NA })
  ) 
  var <- dplyr::mutate(
    combination, moment = "variance", 
    p.value = tryCatch({ var$p.value },error = function(e) { NA })
  ) 
  
  dplyr::bind_rows(mean, var)
  
} %>% dplyr::arrange(name, leg, period, moment)


factors <- tibble::tibble(
  `asset pool` = "US commodities", results = list(US_commodities)
)






### construct factors - asset pool: factor picks ####
factors <- readr::read_rds(
  here::here("explore", "results", "factors-from-picks.rds")
)

returns <- dplyr::mutate(
  factors,
  returns = purrr::map(`factor data`, function(x){
    if (class(x) == "MarketFactor")
      dplyr::mutate(x@returns, long = NA_real_, short = NA_real_) %>%
      dplyr::select(date, long, short, factor)
    else x@returns
  })
) %>% dplyr::select(-`factor data`) %>% tidyr::unnest(returns) %>% 
  dplyr::mutate(year = NA) %>% dplyr::relocate(year, .after = period) %>%
  tidyr::pivot_longer(
    long:factor, names_to = "leg", values_to = "return"
  ) %>% dplyr::filter(!is.na(return)) %>%
  dplyr::mutate(year = lubridate::year(date))


combinations <- dplyr::distinct(
  returns, 
  `picking factor asset pool`, `picking factor leg`, `picking factor name`,
  `asset pool`, `update frequency`, `return frequency`, `ranking period`, 
  `long threshold`, `short threshold`, period, `factor name`, leg
)


factors_from_picks <- foreach(y = 1L:nrow(combinations), .combine = rbind) %dopar% {
  # US_commodities <- foreach(y = 1L:50L, .combine = rbind) %dopar% {
  library(magrittr); library(lubridate)
  
  message(paste(y, nrow(combinations), sep = "/"))
  
  combination <- dplyr::slice(combinations, y)
  
  data <- dplyr::left_join(
    combination, 
    returns, 
    by = c(
      "picking factor asset pool", "picking factor leg", "picking factor name",
      "asset pool", "update frequency", "return frequency", "ranking period", 
      "long threshold", "short threshold", "factor name", "leg", "period"
    )
  )
  
  regimes <- dplyr::filter(`aggregate CHP regimes`, period == combination$period) %>%
    dplyr::select(date, regime)
  
  backwardation <- dplyr::left_join(data, regimes, by = "date") %>%
    dplyr::filter(regime == "backwardation")
  contango <- dplyr::left_join(data, regimes, by = "date") %>%
    dplyr::filter(regime == "contango")
  
  mean <- tryCatch(
    { stats::t.test(backwardation$return, contango$return) }, 
    error = function(e) { NA }
  )
  var <- tryCatch(
    { var.test(backwardation$return, contango$return, alternative = "two.sided") }, 
    error = function(e) { NA }
  )
  
  mean <- dplyr::mutate(
    combination, moment = "mean", 
    p.value = tryCatch({ mean$p.value },error = function(e) { NA })
  ) 
  var <- dplyr::mutate(
    combination, moment = "variance", 
    p.value = tryCatch({ var$p.value },error = function(e) { NA })
  ) 
  
  dplyr::bind_rows(mean, var)
  
}


factors <- tibble::tibble(
  `asset pool` = c("US commodities", "factor picks"),
  results = list(US_commodities, factors_from_picks)
)





# differences <- readr::read_rds(
#   here::here("explore", "results", "tests-differences.rds")
# )

`by regime` <- dplyr::bind_rows(
  `by regime`,
  tibble::tibble(
    analysis = "factors", results = list(factors)
  )
)



differences <- tibble::tibble(
  analysis = c("periods", "regimes"), results = list(`by period`, `by regime`)
)


differences <- readr::write_rds(
  differences,
  here::here("explore", "results", "tests-differences.rds")
)


