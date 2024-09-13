library(magrittr)
library(doParallel)
library(storethat)
library(pullit)
library(factorem)

# start cluster ####
cluster <- makeCluster(detectCores(), outfile = ""); registerDoParallel(cluster)

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
  dplyr::summarise(`aggregate CHP` = mean(pressure, na.rm = T))

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
  dplyr::filter(! is.na(regime))

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


# temp <- dplyr::select(tickers_futures, name, ticker, sector, subsector, MIC) %>% dplyr::left_join(dplyr::select(exchanges, MIC, country), by = "MIC") %>%
#   dplyr::filter(ticker %in% `commodity futures tickers`) %>% dplyr::arrange(country, sector, subsector, ticker)

# commodity futures ####
## market variables ####
### by single commodity ####
#### by year ####
years <- foreach(
  y = unique(`commodity futures data`$`active contract ticker`), 
  .combine = rbind
) %dopar% {
  library(magrittr); library(lubridate)
  message(y)
  
  carry <- dplyr::filter(
    `commodity futures data`, `active contract ticker` == y, field == "PX_LAST"
  ) %>% 
    dplyr::select(`active contract ticker`, `TS position`, date, value) %>% 
    tidyr::spread(`TS position`, value) %>%
    dplyr::mutate(carry = (`1`/`2`) - 1L) %>% 
    dplyr::select(`active contract ticker`, date, carry) %>%
    tidyr::gather(field, value, -c(`active contract ticker`, date)) %>% 
    dplyr::select(`active contract ticker`, field, date, value)
  
  data <- dplyr::filter(
    `commodity futures data`, 
    `active contract ticker` == y, as.integer(`TS position`) == 1L
  ) %>% 
    dplyr::select(`active contract ticker`, field, date, value)
  
  data <- data.table::rbindlist(
    list(
      data, 
      carry, 
      dplyr::rename(
        `commodity aggregate data`@data, 
        `active contract ticker` = ticker
      ) %>%
        dplyr::filter(`active contract ticker` == y)
    )
  ) %>% 
    dplyr::arrange(`active contract ticker`, field, date) %>% 
    dplyr::mutate(year = year(date))
  
  # levels 
  levels <- dplyr::group_by(data, `active contract ticker`, field, year) %>% 
    dplyr::summarise(
      min = min(value, na.rm = T), 
      max = max(value, na.rm = T),
      mean = list(
        tryCatch(
          { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
          error = function(e) { NA }
        )
      ),
      sd = sd(value, na.rm = T)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      period = "year", frequency = NA, type = "levels", regime = "all"
    ) %>%
    dplyr::select(
      type, period, frequency, `active contract ticker`, 
      field, year, regime, min, max, mean, sd
    )
  
  # returns 
  ## days
  days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
    dplyr::mutate(
      value = (value / dplyr::lag(value, 1L) - 1L), 
      value = ifelse(is.infinite(value), NA, value)
    ) %>%
    dplyr::group_by(`active contract ticker`, field, year) %>% 
    dplyr::summarise(
      min = min(value, na.rm = T), 
      max = max(value, na.rm = T),
      mean = list(tryCatch(
        { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
        error = function(e) { NA }
      )
      ),
      sd = sd(value, na.rm = T)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      period = "year", frequency = "daily", type = "returns", regime = "all"
    ) %>%
    dplyr::select(
      type, period, frequency, `active contract ticker`, 
      field, year, regime, min, max, mean, sd
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
        dplyr::group_by(`active contract ticker`, field, year) %>%
        dplyr::summarise(
          min = min(value, na.rm = T), 
          max = max(value, na.rm = T),
          mean = list(
            tryCatch(
              { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
              error = function(e) { NA }
            )
          ),
          sd = sd(value, na.rm = T)
        ) %>%
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = "year", frequency = paste0(x, "ly"), 
          type = "returns", regime = "all"
        ) %>%
        dplyr::select(
          type, period, frequency, `active contract ticker`, field, year, 
          regime, min, max, mean, sd
        )
    }) %>% 
    data.table::rbindlist()
  
  whole <- data.table::rbindlist(list(levels, days, others))
  
  data <- dplyr::left_join(
    data, 
    dplyr::filter(`aggregate CHP regimes`, is.na(period)) %>%
      dplyr::select(date, regime), 
    by = "date"
  ) %>%
    dplyr::filter(! is.na(regime))
  
  # levels 
  levels <- dplyr::group_by(
    data, `active contract ticker`, field, year, regime
  ) %>% 
    dplyr::summarise(
      min = min(value, na.rm = T), 
      max = max(value, na.rm = T),
      mean = list(
        tryCatch(
          { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
          error = function(e) { NA }
        )
      ),
      sd = sd(value, na.rm = T)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(period = "year", frequency = NA, type = "levels") %>%
    dplyr::select(
      type, period, frequency, `active contract ticker`, 
      field, year, regime, min, max, mean, sd
    )
  
  # returns 
  ## days
  days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
    dplyr::mutate(
      value = (value / dplyr::lag(value, 1L) - 1L), 
      value = ifelse(is.infinite(value), NA, value)
    ) %>%
    dplyr::group_by(`active contract ticker`, field, year, regime) %>% 
    dplyr::summarise(
      min = min(value, na.rm = T), 
      max = max(value, na.rm = T),
      mean = list(tryCatch(
        { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
        error = function(e) { NA }
      )
      ),
      sd = sd(value, na.rm = T)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(period = "year", frequency = "daily", type = "returns") %>%
    dplyr::select(
      type, period, frequency, `active contract ticker`, 
      field, year, regime, min, max, mean, sd
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
        dplyr::group_by(`active contract ticker`, field, year, regime) %>%
        dplyr::summarise(
          min = min(value, na.rm = T), 
          max = max(value, na.rm = T),
          mean = list(tryCatch(
            { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) },
            error = function(e) { NA }
          )
          ),
          sd = sd(value, na.rm = T)
        ) %>%
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = "year", frequency = paste0(x, "ly"), type = "returns"
        ) %>%
        dplyr::select(
          type, period, frequency, `active contract ticker`, 
          field, year, regime, min, max, mean, sd
        )
    }) %>% 
    data.table::rbindlist()
  
  regimes <- data.table::rbindlist(list(levels, days, others))
  
  data.table::rbindlist(list(whole, regimes))
}

#### by subperiod ####
subperiods <- foreach(
  y = unique(`commodity futures data`$`active contract ticker`), 
  .combine = rbind
) %dopar% {
  library(magrittr); library(lubridate)
  
  carry <- dplyr::filter(
    `commodity futures data`, `active contract ticker` == y, field == "PX_LAST"
  ) %>% dplyr::select(`active contract ticker`, `TS position`, date, value) %>% 
    tidyr::spread(`TS position`, value) %>%
    dplyr::mutate(carry = (`1`/`2`) - 1L) %>% 
    dplyr::select(`active contract ticker`, date, carry) %>%
    tidyr::gather(field, value, -c(`active contract ticker`, date)) %>% 
    dplyr::select(`active contract ticker`, field, date, value)
  
  data <- dplyr::filter(
    `commodity futures data`, 
    `active contract ticker` == y, 
    as.integer(`TS position`) == 1L
  ) %>% 
    dplyr::select(`active contract ticker`, field, date, value)
  
  whole <- lapply(
    unique(periods$period), 
    function(z){
      
      start <- dplyr::filter(periods, period == z, bound == "start") %>% 
        dplyr::select(date) %>% 
        purrr::flatten_chr()
      
      end <- dplyr::filter(periods, period == z, bound == "end") %>% 
        dplyr::select(date) %>% 
        purrr::flatten_chr()
      
      data <- data.table::rbindlist(
        list(data, 
             carry, 
             dplyr::rename(
               `commodity aggregate data`@data, `active contract ticker` = ticker
             ) %>%
               dplyr::filter(`active contract ticker` == y)
        )
      ) %>% 
        dplyr::arrange(`active contract ticker`, field, date) %>% 
        dplyr::filter(date >= as.Date(start), date <= as.Date(end))
      
      # levels
      levels <- dplyr::group_by(data, `active contract ticker`, field) %>% 
        dplyr::summarise(
          min = min(value, na.rm = T), 
          max = max(value, na.rm = T),
          mean = list(tryCatch(
            { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
            error = function(e) { NA }
          )
          ),
          sd = sd(value, na.rm = T)
        ) %>%
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = z, frequency = NA, type = "levels", year = NA, regime = "all"
        ) %>%
        dplyr::select(
          type, period, frequency, `active contract ticker`, 
          field, year, regime, min, max, mean, sd
        )
      
      # returns 
      ## days
      days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L), 
          value = ifelse(is.infinite(value), NA, value)
        ) %>%
        dplyr::group_by(`active contract ticker`, field) %>% 
        dplyr::summarise(
          min = min(value, na.rm = T), 
          max = max(value, na.rm = T),
          mean = list(tryCatch(
            { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
            error = function(e) { NA }
          )
          ),
          sd = sd(value, na.rm = T)) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = z, frequency = "daily", type = "returns", 
          year = NA, regime = "all"
        ) %>%
        dplyr::select(
          type, period, frequency, `active contract ticker`, 
          field, year, regime, min, max, mean, sd
        )
      
      ## other frequencies
      others <- lapply(
        c("week", "month"), 
        function(x){
          
          dplyr::mutate(
            data, year = year(date), unit = do.call(what = x, args = list(date))
          ) %>% 
            dplyr::group_by(`active contract ticker`, field, year, unit) %>%
            dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
            dplyr::group_by(`active contract ticker`, field) %>% 
            dplyr::select(-c(year, unit)) %>%
            dplyr::mutate(
              value = (value / dplyr::lag(value, 1L) - 1L), 
              value = ifelse(is.infinite(value), NA, value)
            ) %>%
            dplyr::group_by(`active contract ticker`, field) %>%
            dplyr::summarise(
              min = min(value, na.rm = T), 
              max = max(value, na.rm = T),
              mean = list(tryCatch(
                { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
                error = function(e) { NA }
              )
              ),
              sd = sd(value, na.rm = T)
            ) %>%
            dplyr::ungroup() %>% 
            dplyr::mutate(
              period = z, frequency = paste0(x, "ly"), 
              type = "returns", year = NA, regime = "all"
            ) %>%
            dplyr::select(
              type, period, frequency, `active contract ticker`, 
              field, year, regime, min, max, mean, sd
            )
        }) %>% data.table::rbindlist()
      
      data.table::rbindlist(list(levels, days, others))
      
    }) %>% 
    data.table::rbindlist()
  
  
  regimes <- lapply(
    unique(periods$period), 
    function(z){
      
      start <- dplyr::filter(periods, period == z, bound == "start") %>% 
        dplyr::select(date) %>% 
        purrr::flatten_chr()
      
      end <- dplyr::filter(periods, period == z, bound == "end") %>% 
        dplyr::select(date) %>% 
        purrr::flatten_chr()
      
      regimes <- dplyr::filter(`aggregate CHP regimes`, period == z) %>% 
        dplyr::select(date, regime)
      
      data <- data.table::rbindlist(
        list(
          data, 
          carry, 
          dplyr::rename(
            `commodity aggregate data`@data, 
            `active contract ticker` = ticker
          ) %>%
            dplyr::filter(`active contract ticker` == y)
        )
      ) %>%
        dplyr::arrange(`active contract ticker`, field, date) %>% 
        dplyr::filter(date >= as.Date(start), date <= as.Date(end)) %>%
        dplyr::left_join(regimes, by = "date") %>% 
        dplyr::filter(! is.na(regime))
      
      # levels
      levels <- dplyr::group_by(data, `active contract ticker`, field, regime) %>% 
        dplyr::summarise(
          min = min(value, na.rm = T), 
          max = max(value, na.rm = T),
          mean = list(tryCatch(
            { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
            error = function(e) { NA }
          )
          ),
          sd = sd(value, na.rm = T)
        ) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(period = z, frequency = NA, type = "levels", year = NA) %>%
        dplyr::select(
          type, period, frequency, `active contract ticker`, 
          field, year, regime, min, max, mean, sd
        )
      
      # returns 
      ## days
      days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L), 
          value = ifelse(is.infinite(value), NA, value)
        ) %>%
        dplyr::group_by(`active contract ticker`, field, regime) %>% 
        dplyr::summarise(
          min = min(value, na.rm = T), 
          max = max(value, na.rm = T),
          mean = list(tryCatch(
            { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
            error = function(e) { NA }
          )
          ),
          sd = sd(value, na.rm = T)
        ) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = z, frequency = "daily", type = "returns", year = NA
        ) %>%
        dplyr::select(
          type, period, frequency, `active contract ticker`, field, 
          year, regime, min, max, mean, sd
        )
      
      ## other frequencies
      others <- lapply(
        c("week", "month"), 
        function(x){
          
          dplyr::mutate(
            data, year = year(date), unit = do.call(what = x, args = list(date))
          ) %>% 
            dplyr::group_by(`active contract ticker`, field, year, unit) %>%
            dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
            dplyr::group_by(`active contract ticker`, field) %>% 
            dplyr::select(-c(year, unit)) %>%
            dplyr::mutate(
              value = (value / dplyr::lag(value, 1L) - 1L), 
              value = ifelse(is.infinite(value), NA, value)
            ) %>%
            dplyr::group_by(`active contract ticker`, field, regime) %>%
            dplyr::summarise(
              min = min(value, na.rm = T), 
              max = max(value, na.rm = T),
              mean = list(tryCatch(
                { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
                error = function(e) { NA }
              )
              ),
              sd = sd(value, na.rm = T)
            ) %>%
            dplyr::ungroup() %>% 
            dplyr::mutate(
              period = z, frequency = paste0(x, "ly"), type = "returns", year = NA
            ) %>%
            dplyr::select(
              type, period, frequency, `active contract ticker`, 
              field, year, regime, min, max, mean, sd
            )
        }) %>% data.table::rbindlist()
      
      data.table::rbindlist(list(levels, days, others))
      
    }) %>% 
    data.table::rbindlist()
  
  data.table::rbindlist(list(whole, regimes))
}

`individual commodities` <- data.table::rbindlist(list(years, subperiods))


### by country ####
#### by year ####
carry <- dplyr::filter(`commodity futures data`, field == "PX_LAST") %>% 
  dplyr::select(`active contract ticker`, `TS position`, date, value) %>% 
  tidyr::spread(`TS position`, value) %>%
  dplyr::mutate(carry = (`1`/`2`) - 1L) %>% 
  dplyr::select(`active contract ticker`, date, carry) %>%
  tidyr::gather(field, value, -c(`active contract ticker`, date)) %>% 
  dplyr::select(`active contract ticker`, field, date, value)

data <- dplyr::filter(
  `commodity futures data`, as.integer(`TS position`) == 1L
) %>% 
  dplyr::select(`active contract ticker`, field, date, value)

data <- data.table::rbindlist(
  list(
    data, 
    carry, 
    dplyr::rename(
      `commodity aggregate data`@data, `active contract ticker` = ticker
    )
  )
) %>% 
  dplyr::arrange(`active contract ticker`, field, date) %>%
  dplyr::left_join(
    dplyr::left_join(
      dplyr::select(tickers_futures, ticker, MIC), 
      dplyr::select(exchanges, MIC, country), 
      by = "MIC"
    ) %>%
      dplyr::rename(`active contract ticker` = ticker), 
    by = "active contract ticker"
  ) %>% 
  dplyr::select(-MIC)

years <- foreach(
  y = unique(data$country), 
  .combine = rbind) %dopar% {
    library(magrittr); library(lubridate)
    
    data <- dplyr::filter(data, country == y) %>% dplyr::select(-country)
    
    # levels
    levels <- dplyr::group_by(data, field, date) %>% 
      dplyr::summarise(value = mean(value, na.rm = T)) %>% 
      dplyr::mutate(year = lubridate::year(date)) %>% 
      dplyr::group_by(field, year) %>% 
      dplyr::summarise(
        min = min(value, na.rm = T), 
        max = max(value, na.rm = T),
        mean = list(tryCatch(
          { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
          error = function(e) { NA }
        )
        ),
        sd = sd(value, na.rm = T)
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        period = "year", frequency = NA, type = "levels", 
        country = y, sector = "all", subsector = "all", regime = "all"
      ) %>%
      dplyr::select(
        type, period, frequency, country, sector, subsector, 
        field, year, regime, min, max, mean, sd
      )
    
    # returns 
    ## days
    days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
      dplyr::mutate(
        value = (value / dplyr::lag(value, 1L) - 1L), 
        value = ifelse(is.infinite(value), NA, value)
      ) %>% 
      dplyr::group_by(field, date) %>% 
      dplyr::summarise(value = mean(value, na.rm = T)) %>% 
      dplyr::mutate(year = lubridate::year(date)) %>%
      dplyr::group_by(field, year) %>% 
      dplyr::summarise(
        min = min(value, na.rm = T), 
        max = max(value, na.rm = T),
        mean = list(tryCatch(
          { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
          error = function(e) { NA }
        )
        ),
        sd = sd(value, na.rm = T)
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        period = "year", frequency = "daily", type = "returns", 
        country = y, sector = "all", subsector = "all", regime = "all"
      ) %>%
      dplyr::select(
        type, period, frequency, country, sector, subsector, 
        field, year, regime, min, max, mean, sd
      )
    
    ## other frequencies
    others <- lapply(
      c("week", "month"), 
      function(x){
        
        dplyr::mutate(
          data, 
          year = year(date), 
          unit = do.call(what = x, args = list(date))
        ) %>% dplyr::group_by(field, year, unit) %>%
          dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
          dplyr::group_by(`active contract ticker`, field) %>% 
          dplyr::select(-unit) %>%
          dplyr::mutate(
            value = (value / dplyr::lag(value, 1L) - 1L), 
            value = ifelse(is.infinite(value), NA, value)
          ) %>%
          dplyr::group_by(field, date) %>% 
          dplyr::summarise(value = mean(value, na.rm = T)) %>% 
          dplyr::mutate(year = lubridate::year(date)) %>%
          dplyr::group_by(field, year) %>%
          dplyr::summarise(
            min = min(value, na.rm = T), 
            max = max(value, na.rm = T),
            mean = list(tryCatch(
              { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
              error = function(e) { NA }
            )
            ),
            sd = sd(value, na.rm = T)
          ) %>%
          dplyr::ungroup() %>% 
          dplyr::mutate(
            period = "year", frequency = paste0(x, "ly"), type = "returns", 
            country = y, sector = "all", subsector = "all", regime = "all"
          ) %>%
          dplyr::select(
            type, period, frequency, country, sector, subsector, 
            field, year, regime, min, max, mean, sd
          )
        
      }) %>% 
      data.table::rbindlist()
    
    whole <- data.table::rbindlist(list(levels, days, others))
    
    
    # levels 
    levels <- dplyr::group_by(data, field, date) %>% 
      dplyr::summarise(value = mean(value, na.rm = T)) %>% 
      dplyr::mutate(year = lubridate::year(date)) %>% 
      dplyr::left_join(
        dplyr::filter(`aggregate CHP regimes`, is.na(period)) %>% 
          dplyr::select(date, regime), 
        by = "date"
      ) %>%
      dplyr::filter(! is.na(regime)) %>% 
      dplyr::group_by(field, year, regime) %>% 
      dplyr::summarise(
        min = min(value, na.rm = T), 
        max = max(value, na.rm = T),
        mean = list(tryCatch(
          { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
          error = function(e) { NA }
        )
        ),
        sd = sd(value, na.rm = T)
      ) %>%
      dplyr::ungroup() %>% 
      dplyr::mutate(
        period = "year", frequency = NA, type = "levels", 
        country = y, sector = "all", subsector = "all"
      ) %>%
      dplyr::select(
        type, period, frequency, country, sector, 
        subsector, field, year, regime, min, max, mean, sd
      )
    
    # returns 
    ## days
    days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
      dplyr::mutate(
        value = (value / dplyr::lag(value, 1L) - 1L), 
        value = ifelse(is.infinite(value), NA, value)
      ) %>%
      dplyr::group_by(field, date) %>% 
      dplyr::summarise(value = mean(value, na.rm = T)) %>% 
      dplyr::mutate(year = lubridate::year(date)) %>%
      dplyr::left_join(
        dplyr::filter(`aggregate CHP regimes`, is.na(period)) %>% 
          dplyr::select(date, regime), 
        by = "date"
      ) %>% 
      dplyr::filter(! is.na(regime)) %>% 
      dplyr::group_by(field, year, regime) %>% 
      dplyr::summarise(
        min = min(value, na.rm = T), 
        max = max(value, na.rm = T),
        mean = list(tryCatch(
          { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) },
          error = function(e) { NA }
        )
        ),
        sd = sd(value, na.rm = T)
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        period = "year", frequency = "daily", type = "returns", 
        country = y, sector = "all", subsector = "all"
      ) %>%
      dplyr::select(
        type, period, frequency, country, sector, subsector, 
        field, year, regime, min, max, mean, sd
      )
    
    ## other frequencies
    others <- lapply(
      c("week", "month"), 
      function(x){
        
        dplyr::mutate(
          data, year = year(date), unit = do.call(what = x, args = list(date))
        ) %>% dplyr::group_by(field, year, unit) %>%
          dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
          dplyr::group_by(`active contract ticker`, field) %>% 
          dplyr::select(-unit) %>%
          dplyr::mutate(
            value = (value / dplyr::lag(value, 1L) - 1L), 
            value = ifelse(is.infinite(value), NA, value)
          ) %>%
          dplyr::group_by(field, date) %>% 
          dplyr::summarise(value = mean(value, na.rm = T)) %>% 
          dplyr::mutate(year = lubridate::year(date)) %>%
          dplyr::left_join(
            dplyr::filter(`aggregate CHP regimes`, is.na(period)) %>% 
              dplyr::select(date, regime), 
            by = "date"
          ) %>%
          dplyr::filter(! is.na(regime)) %>% 
          dplyr::group_by(field, year, regime) %>%
          dplyr::summarise(
            min = min(value, na.rm = T), 
            max = max(value, na.rm = T),
            mean = list(tryCatch(
              { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
              error = function(e) { NA }
            )
            ),
            sd = sd(value, na.rm = T)
          ) %>%
          dplyr::ungroup() %>% 
          dplyr::mutate(
            period = "year", frequency = paste0(x, "ly"), 
            type = "returns", country = y, sector = "all", subsector = "all"
          ) %>%
          dplyr::select(
            type, period, frequency, country, sector, subsector, 
            field, year, regime, min, max, mean, sd
          )
      }) %>% 
      data.table::rbindlist()
    
    regimes <- data.table::rbindlist(list(levels, days, others))
    
    data.table::rbindlist(list(whole, regimes))
  }

#### by subperiod ####
subperiods <- foreach(
  y = unique(data$country), 
  .combine = rbind
) %dopar% {
  library(magrittr); library(lubridate)
  
  lapply(
    unique(periods$period), 
    function(z){
      
      start <- dplyr::filter(periods, period == z, bound == "start") %>% 
        dplyr::select(date) %>% purrr::flatten_chr()
      
      end <- dplyr::filter(periods, period == z, bound == "end") %>% 
        dplyr::select(date) %>% purrr::flatten_chr()
      
      data <- dplyr::filter(
        data, country == y, date >= as.Date(start), date <= as.Date(end)
      ) %>%  dplyr::select(-country)
      
      # levels
      levels <- dplyr::group_by(data, field, date) %>% 
        dplyr::summarise(value = mean(value, na.rm = T)) %>% 
        dplyr::group_by(field) %>% 
        dplyr::summarise(
          min = min(value, na.rm = T), 
          max = max(value, na.rm = T),
          mean = list(tryCatch(
            { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
            error = function(e) { NA }
          )
          ),
          sd = sd(value, na.rm = T)
        ) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = z, frequency = NA, type = "levels", year = NA, country = y,
          sector = "all", subsector = "all", regime = "all"
        ) %>%
        dplyr::select(
          type, period, frequency, country, sector, subsector, field, 
          year, regime, min, max, mean, sd
        )
      
      # returns 
      ## days
      days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L), 
          value = ifelse(is.infinite(value), NA, value)
        ) %>%
        dplyr::group_by(field, date) %>% 
        dplyr::summarise(value = mean(value, na.rm = T)) %>%
        dplyr::group_by(field) %>% 
        dplyr::summarise(
          min = min(value, na.rm = T), 
          max = max(value, na.rm = T),
          mean = list(tryCatch(
            { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
            error = function(e) { NA }
          )
          ),
          sd = sd(value, na.rm = T)
        ) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = z, frequency = "daily", type = "returns", year = NA, 
          country = y, sector = "all", subsector = "all", regime = "all"
        ) %>%
        dplyr::select(
          type, period, frequency, country, sector, subsector, 
          field, year, regime, min, max, mean, sd
        )
      
      ## other frequencies
      others <- lapply(
        c("week", "month"), 
        function(x){
          
          dplyr::mutate(
            data, year = year(date), unit = do.call(what = x, args = list(date))
          ) %>% dplyr::group_by(field, year, unit) %>%
            dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
            dplyr::group_by(`active contract ticker`, field) %>% 
            dplyr::select(-unit) %>%
            dplyr::mutate(
              value = (value / dplyr::lag(value, 1L) - 1L), 
              value = ifelse(is.infinite(value), NA, value)
            ) %>%
            dplyr::group_by(field, date) %>% 
            dplyr::summarise(value = mean(value, na.rm = T)) %>%
            dplyr::group_by(field) %>%
            dplyr::summarise(
              min = min(value, na.rm = T), 
              max = max(value, na.rm = T),
              mean = list(tryCatch(
                { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
                error = function(e) { NA }
              )
              ),
              sd = sd(value, na.rm = T)
            ) %>% 
            dplyr::ungroup() %>% 
            dplyr::mutate(
              period = z, frequency = paste0(x, "ly"), type = "returns", year = NA, 
              country = y, sector = "all", subsector = "all", regime = "all"
            ) %>%
            dplyr::select(
              type, period, frequency, country, sector, subsector, 
              field, year, regime, min, max, mean, sd
            )
          
        }) %>% 
        data.table::rbindlist()
      
      whole <- data.table::rbindlist(list(levels, days, others))
      
      # levels
      levels <- dplyr::group_by(data, field, date) %>% 
        dplyr::summarise(value = mean(value, na.rm = T)) %>% 
        dplyr::left_join(
          dplyr::filter(`aggregate CHP regimes`, ! is.na(period)) %>% 
            dplyr::select(date, regime), 
          by = "date") %>% 
        dplyr::filter(! is.na(regime)) %>% 
        dplyr::group_by(field, regime) %>% 
        dplyr::summarise(
          min = min(value, na.rm = T), 
          max = max(value, na.rm = T),
          mean = list(tryCatch(
            { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
            error = function(e) { NA }
          )
          ),
          sd = sd(value, na.rm = T)
        ) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = z, frequency = NA, type = "levels", year = NA, country = y,
          sector = "all", subsector = "all"
        ) %>%
        dplyr::select(
          type, period, frequency, country, sector, subsector, 
          field, year, regime, min, max, mean, sd
        )
      
      # returns 
      ## days
      days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L), 
          value = ifelse(is.infinite(value), NA, value)
        ) %>%
        dplyr::group_by(field, date) %>% 
        dplyr::summarise(value = mean(value, na.rm = T)) %>%
        dplyr::left_join(
          dplyr::filter(`aggregate CHP regimes`, ! is.na(period)) %>% 
            dplyr::select(date, regime), 
          by = "date") %>% 
        dplyr::filter(! is.na(regime)) %>% 
        dplyr::group_by(field, regime) %>% 
        dplyr::summarise(
          min = min(value, na.rm = T), 
          max = max(value, na.rm = T),
          mean = list(tryCatch(
            { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
            error = function(e) { NA }
          )
          ),
          sd = sd(value, na.rm = T)
        ) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = z, frequency = "daily", type = "returns", year = NA, 
          country = y, sector = "all", subsector = "all"
        ) %>%
        dplyr::select(
          type, period, frequency, country, sector, subsector, field, year, 
          regime, min, max, mean, sd
        )
      
      ## other frequencies
      others <- lapply(
        c("week", "month"), 
        function(x){
          
          dplyr::mutate(
            data, year = year(date), unit = do.call(what = x, args = list(date))
          ) %>% 
            dplyr::group_by(field, year, unit) %>%
            dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
            dplyr::group_by(`active contract ticker`, field) %>% 
            dplyr::select(-unit) %>%
            dplyr::mutate(
              value = (value / dplyr::lag(value, 1L) - 1L),
              value = ifelse(is.infinite(value), NA, value)
            ) %>%
            dplyr::group_by(field, date) %>% 
            dplyr::summarise(value = mean(value, na.rm = T)) %>%
            dplyr::left_join(
              dplyr::filter(`aggregate CHP regimes`, ! is.na(period)) %>% 
                dplyr::select(date, regime), 
              by = "date"
            ) %>%
            dplyr::filter(! is.na(regime)) %>% 
            dplyr::group_by(field, regime) %>%
            dplyr::summarise(
              min = min(value, na.rm = T), 
              max = max(value, na.rm = T),
              mean = list(tryCatch(
                { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
                error = function(e) { NA }
              )
              ),
              sd = sd(value, na.rm = T)
            ) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(
              period = z, frequency = paste0(x, "ly"), type = "returns", 
              year = NA, country = y,sector = "all", subsector = "all", 
              regime = "all"
            ) %>%
            dplyr::select(
              type, period, frequency, country, sector, subsector, 
              field, year, regime, min, max, mean, sd
            )
          
        }) %>% 
        data.table::rbindlist()
      
      regimes <- data.table::rbindlist(list(levels, days, others))
      
      data.table::rbindlist(list(whole, regimes))
      
    }) %>% data.table::rbindlist()
}

countries <- data.table::rbindlist(list(years, subperiods))


### by sector ####
#### by year ####
carry <- dplyr::filter(`commodity futures data`, field == "PX_LAST") %>% 
  dplyr::select(`active contract ticker`, `TS position`, date, value) %>% 
  tidyr::spread(`TS position`, value) %>%
  dplyr::mutate(carry = (`1`/`2`) - 1L) %>% 
  dplyr::select(`active contract ticker`, date, carry) %>%
  tidyr::gather(field, value, -c(`active contract ticker`, date)) %>% 
  dplyr::select(`active contract ticker`, field, date, value)

data <- dplyr::filter(
  `commodity futures data`, as.integer(`TS position`) == 1L
) %>% dplyr::select(`active contract ticker`, field, date, value)

data <- data.table::rbindlist(
  list(
    data, 
    carry, 
    dplyr::rename(
      `commodity aggregate data`@data, 
      `active contract ticker` = ticker)
  )
) %>% 
  dplyr::arrange(`active contract ticker`, field, date) %>%
  dplyr::left_join(
    dplyr::select(tickers_futures, `active contract ticker` = ticker, MIC, sector), 
    by = "active contract ticker"
  ) %>%
  dplyr::left_join(dplyr::select(exchanges, MIC, country), by = "MIC") %>% 
  dplyr::select(-MIC)

combinations <- dplyr::distinct(data, country, sector) %>% as.data.frame()

years <- foreach(
  y = 1L:nrow(combinations), 
  .combine = rbind) %dopar% {
    library(magrittr); library(lubridate)
    
    data <- dplyr::filter(
      data, sector == combinations[y, "sector"], 
      country == combinations[y, "country"]
    ) %>% dplyr::select(-c(country, sector))
    
    # levels
    levels <- dplyr::group_by(data, field, date) %>% 
      dplyr::summarise(value = mean(value, na.rm = T)) %>% 
      dplyr::mutate(year = lubridate::year(date)) %>% 
      dplyr::group_by(field, year) %>% 
      dplyr::summarise(
        min = min(value, na.rm = T), 
        max = max(value, na.rm = T),
        mean = list(tryCatch(
          { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
          error = function(e) { NA }
        )
        ),
        sd = sd(value, na.rm = T)
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        period = "year", frequency = NA, type = "levels", 
        country = combinations[y, "country"], sector = combinations[y, "sector"], 
        subsector = "all", regime = "all"
      ) %>%
      dplyr::select(
        type, period, frequency, country, sector, subsector, 
        field, year, regime, min, max, mean, sd
      )
    
    # returns 
    ## days
    days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
      dplyr::mutate(
        value = (value / dplyr::lag(value, 1L) - 1L), 
        value = ifelse(is.infinite(value), NA, value)
      ) %>%
      dplyr::group_by(field, date) %>% 
      dplyr::summarise(value = mean(value, na.rm = T)) %>% 
      dplyr::mutate(year = lubridate::year(date)) %>% 
      dplyr::group_by(field, year) %>% 
      dplyr::summarise(
        min = min(value, na.rm = T), 
        max = max(value, na.rm = T),
        mean = list(tryCatch(
          { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
          error = function(e) { NA }
        )
        ),
        sd = sd(value, na.rm = T)
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        period = "year", frequency = "daily", type = "returns", 
        country = combinations[y, "country"],
        sector = combinations[y, "sector"], subsector = "all", regime = "all"
      ) %>%
      dplyr::select(
        type, period, frequency, country, sector, subsector, 
        field, year, regime, min, max, mean, sd
      )
    
    ## other frequencies
    others <- lapply(
      c("week", "month"), 
      function(x){
        
        dplyr::mutate(
          data, year = year(date), unit = do.call(what = x, args = list(date))
        ) %>% 
          dplyr::group_by(`active contract ticker`, field, year, unit) %>% 
          dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
          dplyr::group_by(`active contract ticker`, field) %>% 
          dplyr::select(-unit) %>%
          dplyr::mutate(
            value = (value / dplyr::lag(value, 1L) - 1L), 
            value = ifelse(is.infinite(value), NA, value)
          ) %>%
          dplyr::group_by(field, date) %>% 
          dplyr::summarise(value = mean(value, na.rm = T)) %>% 
          dplyr::mutate(year = lubridate::year(date)) %>% 
          dplyr::group_by(field, year) %>% 
          dplyr::summarise(
            min = min(value, na.rm = T),
            max = max(value, na.rm = T),
            mean = list(tryCatch(
              { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) },
              error = function(e) { NA }
            )
            ),
            sd = sd(value, na.rm = T)
          ) %>% 
          dplyr::ungroup() %>% 
          dplyr::mutate(
            period = "year", frequency = paste0(x, "ly"), type = "returns", 
            country = combinations[y, "country"], 
            sector = combinations[y, "sector"], subsector = "all", regime = "all"
          ) %>%
          dplyr::select(
            type, period, frequency, country, sector, subsector, field, 
            year, regime, min, max, mean, sd
          )
        
      }) %>% 
      data.table::rbindlist()
    
    whole <- data.table::rbindlist(list(levels, days, others))
    
    # levels
    levels <- dplyr::group_by(data, field, date) %>% 
      dplyr::summarise(value = mean(value, na.rm = T)) %>% 
      dplyr::mutate(year = lubridate::year(date)) %>% 
      dplyr::left_join(
        dplyr::filter(`aggregate CHP regimes`, is.na(period)) %>% 
          dplyr::select(date, regime), 
        by = "date"
      ) %>% 
      dplyr::filter(! is.na(regime)) %>% 
      dplyr::group_by(field, year, regime) %>% 
      dplyr::summarise(
        min = min(value, na.rm = T), 
        max = max(value, na.rm = T),
        mean = list(tryCatch(
          { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
          error = function(e) { NA }
        )
        ),
        sd = sd(value, na.rm = T)
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        period = "year", frequency = NA, type = "levels", 
        country = combinations[y, "country"], sector = combinations[y, "sector"], 
        subsector = "all"
      ) %>%
      dplyr::select(
        type, period, frequency, country, sector, subsector, field, year, 
        regime, min, max, mean, sd
      )
    
    # returns 
    ## days
    days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
      dplyr::mutate(
        value = (value / dplyr::lag(value, 1L) - 1L), 
        value = ifelse(is.infinite(value), NA, value)
      ) %>%
      dplyr::group_by(field, date) %>% 
      dplyr::summarise(value = mean(value, na.rm = T)) %>% 
      dplyr::mutate(year = lubridate::year(date)) %>% 
      dplyr::left_join(
        dplyr::filter(`aggregate CHP regimes`, is.na(period)) %>% 
          dplyr::select(date, regime), 
        by = "date"
      ) %>% 
      dplyr::filter(! is.na(regime)) %>% 
      dplyr::group_by(field, year, regime) %>% 
      dplyr::summarise(
        min = min(value, na.rm = T), 
        max = max(value, na.rm = T),
        mean = list(tryCatch(
          { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
          error = function(e) { NA }
        )
        ),
        sd = sd(value, na.rm = T)
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        period = "year", frequency = "daily", type = "returns", 
        country = combinations[y, "country"], sector = combinations[y, "sector"], 
        subsector = "all"
      ) %>%
      dplyr::select(
        type, period, frequency, country, sector, subsector, field, year, 
        regime, min, max, mean, sd
      )
    
    ## other frequencies
    others <- lapply(
      c("week", "month"), 
      function(x){
        
        dplyr::mutate(
          data, year = year(date), unit = do.call(what = x, args = list(date))
        ) %>% dplyr::group_by(`active contract ticker`, field, year, unit) %>% 
          dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
          dplyr::group_by(`active contract ticker`, field) %>% 
          dplyr::select(-unit) %>%
          dplyr::mutate(
            value = (value / dplyr::lag(value, 1L) - 1L), 
            value = ifelse(is.infinite(value), NA, value)
          ) %>%
          dplyr::group_by(field, date) %>% 
          dplyr::summarise(value = mean(value, na.rm = T)) %>% 
          dplyr::mutate(year = lubridate::year(date)) %>% 
          dplyr::left_join(
            dplyr::filter(`aggregate CHP regimes`, is.na(period)) %>% 
              dplyr::select(date, regime), 
            by = "date"
          ) %>%
          dplyr::filter(! is.na(regime)) %>% 
          dplyr::group_by(field, year, regime) %>%
          dplyr::summarise(
            min = min(value, na.rm = T), 
            max = max(value, na.rm = T),
            mean = list(tryCatch(
              { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
              error = function(e) { NA }
            )
            ),
            sd = sd(value, na.rm = T)
          ) %>% 
          dplyr::ungroup() %>% 
          dplyr::mutate(
            period = "year", frequency = paste0(x, "ly"), type = "returns", 
            country = combinations[y, "country"], sector = combinations[y, "sector"], 
            subsector = "all"
          ) %>%
          dplyr::select(
            type, period, frequency, country, sector, subsector, 
            field, year, regime, min, max, mean, sd
          )
        
      }) %>% 
      data.table::rbindlist()
    
    regimes <- data.table::rbindlist(list(levels, days, others))
    
    data.table::rbindlist(list(whole, regimes))
  }

#### by subperiod ####
subperiods <- foreach(
  y = 1L:nrow(combinations), 
  .combine = rbind
) %dopar% {
  library(magrittr); library(lubridate)
  lapply(
    unique(periods$period), 
    function(z){
      
      start <- dplyr::filter(periods, period == z, bound == "start") %>% 
        dplyr::select(date) %>% purrr::flatten_chr()
      
      end <- dplyr::filter(periods, period == z, bound == "end") %>% 
        dplyr::select(date) %>% purrr::flatten_chr()
      
      data <- dplyr::filter(
        data, sector == combinations[y, "sector"], 
        country == combinations[y, "country"],
        date >= as.Date(start), date <= as.Date(end)
      ) %>% 
        dplyr::select(-c(country, sector))
      
      # levels
      levels <- dplyr::group_by(data, field, date) %>% 
        dplyr::summarise(value = mean(value, na.rm = T)) %>% 
        dplyr::group_by(field) %>% 
        dplyr::summarise(
          min = min(value, na.rm = T), 
          max = max(value, na.rm = T),
          mean = list(tryCatch(
            { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
            error = function(e) { NA }
          )
          ),
          sd = sd(value, na.rm = T)
        ) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = z, frequency = NA, type = "levels", year = NA, 
          country = combinations[y, "country"], 
          sector = combinations[y, "sector"], subsector = "all", regime = "all"
        ) %>%
        dplyr::select(
          type, period, frequency, country, sector, subsector, field, year, 
          regime, min, max, mean, sd
        )
      
      # returns 
      ## days
      days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L), 
          value = ifelse(is.infinite(value), NA, value)
        ) %>%
        dplyr::group_by(field, date) %>% 
        dplyr::summarise(value = mean(value, na.rm = T)) %>%
        dplyr::group_by(field) %>% 
        dplyr::summarise(
          min = min(value, na.rm = T), 
          max = max(value, na.rm = T),
          mean = list(tryCatch(
            { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
            error = function(e) { NA }
          )
          ),
          sd = sd(value, na.rm = T)
        ) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = z, frequency = "daily", type = "returns", year = NA, 
          country = combinations[y, "country"], 
          sector = combinations[y, "sector"], subsector = "all", regime = "all"
        ) %>%
        dplyr::select(
          type, period, frequency, country, sector, subsector, field, year, 
          regime, min, max, mean, sd
        )
      
      ## other frequencies
      others <- lapply(
        c("week", "month"), 
        function(x){
          
          dplyr::mutate(
            data, year = year(date), unit = do.call(what = x, args = list(date))
          ) %>% 
            dplyr::group_by(field, year, unit) %>%
            dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
            dplyr::group_by(`active contract ticker`, field) %>% 
            dplyr::select(-unit) %>%
            dplyr::mutate(
              value = (value / dplyr::lag(value, 1L) - 1L), 
              value = ifelse(is.infinite(value), NA, value)
            ) %>%
            dplyr::group_by(field, date) %>% 
            dplyr::summarise(value = mean(value, na.rm = T)) %>%
            dplyr::group_by(field) %>%
            dplyr::summarise(
              min = min(value, na.rm = T), 
              max = max(value, na.rm = T),
              mean = list(tryCatch(
                { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
                error = function(e) { NA }
              )
              ),
              sd = sd(value, na.rm = T)
            ) %>% 
            dplyr::ungroup() %>% 
            dplyr::mutate(
              period = z, frequency = paste0(x, "ly"), type = "returns", year = NA, 
              country = combinations[y, "country"], 
              sector = combinations[y, "sector"], subsector = "all", regime = "all"
            ) %>%
            dplyr::select(
              type, period, frequency, country, sector, subsector, field, year, 
              regime, min, max, mean, sd
            )
        }) %>% 
        data.table::rbindlist()
      
      whole <- data.table::rbindlist(list(levels, days, others))
      
      # levels
      levels <- dplyr::group_by(data, field, date) %>% 
        dplyr::summarise(value = mean(value, na.rm = T)) %>% 
        dplyr::left_join(
          dplyr::filter(`aggregate CHP regimes`, ! is.na(period)) %>% 
            dplyr::select(date, regime), 
          by = "date"
        ) %>% 
        dplyr::filter(! is.na(regime)) %>% 
        dplyr::group_by(field, regime) %>% 
        dplyr::summarise(
          min = min(value, na.rm = T), 
          max = max(value, na.rm = T),
          mean = list(tryCatch(
            { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
            error = function(e) { NA }
          )
          ),
          sd = sd(value, na.rm = T)
        ) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = z, frequency = NA, type = "levels", year = NA, 
          country = combinations[y, "country"], 
          sector = combinations[y, "sector"], subsector = "all"
        ) %>%
        dplyr::select(
          type, period, frequency, country, sector, subsector, field, year, 
          regime, min, max, mean, sd
        )
      
      # returns 
      ## days
      days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L), 
          value = ifelse(is.infinite(value), NA, value)
        ) %>%
        dplyr::group_by(field, date) %>% 
        dplyr::summarise(value = mean(value, na.rm = T)) %>%
        dplyr::left_join(
          dplyr::filter(`aggregate CHP regimes`, ! is.na(period)) %>% 
            dplyr::select(date, regime), 
          by = "date"
        ) %>% 
        dplyr::filter(! is.na(regime)) %>% 
        dplyr::group_by(field, regime) %>% 
        dplyr::summarise(
          min = min(value, na.rm = T), 
          max = max(value, na.rm = T),
          mean = list(tryCatch(
            { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
            error = function(e) { NA }
          )
          ),
          sd = sd(value, na.rm = T)
        ) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = z, frequency = "daily", type = "returns", year = NA, 
          country = combinations[y, "country"], 
          sector = combinations[y, "sector"], subsector = "all") %>%
        dplyr::select(
          type, period, frequency, country, sector, subsector, field, year,
          regime, min, max, mean, sd
        )
      
      ## other frequencies
      others <- lapply(
        c("week", "month"), 
        function(x){
          
          dplyr::mutate(
            data, year = year(date), unit = do.call(what = x, args = list(date))
          ) %>% dplyr::group_by(field, year, unit) %>%
            dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
            dplyr::group_by(`active contract ticker`, field) %>% 
            dplyr::select(-unit) %>%
            dplyr::mutate(
              value = (value / dplyr::lag(value, 1L) - 1L), 
              value = ifelse(is.infinite(value), NA, value)
            ) %>%
            dplyr::group_by(field, date) %>% 
            dplyr::summarise(value = mean(value, na.rm = T)) %>%
            dplyr::left_join(
              dplyr::filter(`aggregate CHP regimes`, ! is.na(period)) %>%
                dplyr::select(date, regime), 
              by = "date"
            ) %>% dplyr::filter(! is.na(regime)) %>%
            dplyr::group_by(field, regime) %>%
            dplyr::summarise(
              min = min(value, na.rm = T), 
              max = max(value, na.rm = T),
              mean = list(tryCatch(
                { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
                error = function(e) { NA }
              )
              ),
              sd = sd(value, na.rm = T)
            ) %>% 
            dplyr::ungroup() %>% 
            dplyr::mutate(
              period = z, frequency = paste0(x, "ly"), type = "returns", year = NA, 
              country = combinations[y, "country"], 
              sector = combinations[y, "sector"], subsector = "all"
            ) %>%
            dplyr::select(
              type, period, frequency, country, sector, subsector, field, year, 
              regime, min, max, mean, sd
            )
        }) %>% 
        data.table::rbindlist()
      
      regimes <- data.table::rbindlist(list(levels, days, others))
      
      data.table::rbindlist(list(whole, regimes))
      
    }) %>% 
    data.table::rbindlist()
}

sectors <- data.table::rbindlist(list(years, subperiods))

### by subsector ####
#### by year ####
carry <- dplyr::filter(`commodity futures data`, field == "PX_LAST") %>% 
  dplyr::select(`active contract ticker`, `TS position`, date, value) %>% 
  tidyr::spread(`TS position`, value) %>%
  dplyr::mutate(carry = (`1`/`2`) - 1L) %>% 
  dplyr::select(`active contract ticker`, date, carry) %>%
  tidyr::gather(field, value, -c(`active contract ticker`, date)) %>% 
  dplyr::select(`active contract ticker`, field, date, value)

data <- dplyr::filter(`commodity futures data`, as.integer(`TS position`) == 1L) %>% 
  dplyr::select(`active contract ticker`, field, date, value)

data <- data.table::rbindlist(
  list(
    data,
    carry, 
    dplyr::rename(`commodity aggregate data`@data, `active contract ticker` = ticker)
  )
) %>% 
  dplyr::arrange(`active contract ticker`, field, date) %>%
  dplyr::left_join(
    dplyr::select(
      tickers_futures, `active contract ticker` = ticker, MIC, sector, subsector
    ), 
    by = "active contract ticker"
  ) %>%
  dplyr::left_join(dplyr::select(exchanges, MIC, country), by = "MIC") %>% 
  dplyr::select(-MIC)

combinations <- dplyr::distinct(data, country, sector, subsector) %>% 
  as.data.frame()

years <- foreach(
  y = 1L:nrow(combinations), 
  .combine = rbind) %dopar% {
    library(magrittr); library(lubridate)
    
    data <- dplyr::filter(
      data, country == combinations[y, "country"], 
      sector == combinations[y, "sector"], subsector == combinations[y, "subsector"]
    ) %>%
      dplyr::select(-c(country, sector, subsector))
    
    # levels
    levels <- dplyr::group_by(data, field, date) %>% 
      dplyr::summarise(value = mean(value, na.rm = T)) %>% 
      dplyr::mutate(year = lubridate::year(date)) %>% 
      dplyr::group_by(field, year) %>% 
      dplyr::summarise(
        min = min(value, na.rm = T), 
        max = max(value, na.rm = T),
        mean = list(tryCatch(
          { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
          error = function(e) { NA }
        )
        ),
        sd = sd(value, na.rm = T)
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        period = "year", frequency = NA, type = "levels", 
        country = combinations[y, "country"], sector = combinations[y, "sector"], 
        subsector = combinations[y, "subsector"], regime = "all"
      ) %>%
      dplyr::select(
        type, period, frequency, country, sector, subsector, field, year, 
        regime, min, max, mean, sd
      )
    
    # returns 
    ## days
    days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
      dplyr::mutate(
        value = (value / dplyr::lag(value, 1L) - 1L), 
        value = ifelse(is.infinite(value), NA, value)
      ) %>%
      dplyr::group_by(field, date) %>% 
      dplyr::summarise(value = mean(value, na.rm = T)) %>% 
      dplyr::mutate(year = lubridate::year(date)) %>% 
      dplyr::group_by(field, year) %>% 
      dplyr::summarise(
        min = min(value, na.rm = T), 
        max = max(value, na.rm = T),
        mean = list(tryCatch(
          { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
          error = function(e) { NA }
        )
        ),
        sd = sd(value, na.rm = T)
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        period = "year", frequency = "daily", type = "returns", 
        country = combinations[y, "country"], sector = combinations[y, "sector"], 
        subsector = combinations[y, "subsector"], regime = "all"
      ) %>%
      dplyr::select(
        type, period, frequency, country, sector, subsector, 
        field, year, regime, min, max, mean, sd
      )
    
    ## other frequencies
    others <- lapply(
      c("week", "month"), 
      function(x){
        
        dplyr::mutate(
          data, year = year(date), unit = do.call(what = x, args = list(date))
        ) %>% dplyr::group_by(`active contract ticker`, field, year, unit) %>%
          dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
          dplyr::group_by(`active contract ticker`, field) %>% 
          dplyr::select(-unit) %>%
          dplyr::mutate(
            value = (value / dplyr::lag(value, 1L) - 1L), 
            value = ifelse(is.infinite(value), NA, value)
          ) %>%
          dplyr::group_by(field, date) %>% 
          dplyr::summarise(value = mean(value, na.rm = T)) %>% 
          dplyr::mutate(year = lubridate::year(date)) %>%
          dplyr::group_by(field, year) %>%
          dplyr::summarise(
            min = min(value, na.rm = T), 
            max = max(value, na.rm = T),
            mean = list(tryCatch(
              { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
              error = function(e) { NA }
            )
            ),
            sd = sd(value, na.rm = T)
          ) %>% 
          dplyr::ungroup() %>% 
          dplyr::mutate(
            period = "year", frequency = paste0(x, "ly"), type = "returns", 
            country = combinations[y, "country"], sector = combinations[y, "sector"], 
            subsector = combinations[y, "subsector"], regime = "all"
          ) %>%
          dplyr::select(
            type, period, frequency, country, sector, subsector, field, year, 
            regime, min, max, mean, sd
          )
      }) %>% 
      data.table::rbindlist()
    
    whole <- data.table::rbindlist(list(levels, days, others))
    
    # levels
    levels <- dplyr::group_by(data, field, date) %>% 
      dplyr::summarise(value = mean(value, na.rm = T)) %>% 
      dplyr::mutate(year = lubridate::year(date)) %>% 
      dplyr::left_join(
        dplyr::filter(`aggregate CHP regimes`, is.na(period)) %>% 
          dplyr::select(date, regime), 
        by = "date"
      ) %>% 
      dplyr::filter(! is.na(regime)) %>% 
      dplyr::group_by(field, year, regime) %>% 
      dplyr::summarise(
        min = min(value, na.rm = T), 
        max = max(value, na.rm = T),
        mean = list(tryCatch(
          { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
          error = function(e) { NA }
        )
        ),
        sd = sd(value, na.rm = T)
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        period = "year", frequency = NA, type = "levels", 
        country = combinations[y, "country"], sector = combinations[y, "sector"], 
        subsector = combinations[y, "subsector"]
      ) %>%
      dplyr::select(
        type, period, frequency, country, sector, subsector, 
        field, year, regime, min, max, mean, sd
      )
    
    # returns 
    ## days
    days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
      dplyr::mutate(
        value = (value / dplyr::lag(value, 1L) - 1L), 
        value = ifelse(is.infinite(value), NA, value)
      ) %>%
      dplyr::group_by(field, date) %>% 
      dplyr::summarise(value = mean(value, na.rm = T)) %>% 
      dplyr::mutate(year = lubridate::year(date)) %>% 
      dplyr::left_join(
        dplyr::filter(`aggregate CHP regimes`, is.na(period)) %>% 
          dplyr::select(date, regime), 
        by = "date"
      ) %>% 
      dplyr::filter(! is.na(regime)) %>% dplyr::group_by(field, year, regime) %>% 
      dplyr::summarise(
        min = min(value, na.rm = T), 
        max = max(value, na.rm = T),
        mean = list(tryCatch(
          { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
          error = function(e) { NA }
        )
        ),
        sd = sd(value, na.rm = T)
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        period = "year", frequency = "daily", type = "returns", 
        country = combinations[y, "country"], sector = combinations[y, "sector"], 
        subsector = combinations[y, "subsector"]
      ) %>%
      dplyr::select(
        type, period, frequency, country, sector, subsector, field, year, regime, 
        min, max, mean, sd
      )
    
    ## other frequencies
    others <- lapply(
      c("week", "month"), 
      function(x){
        
        dplyr::mutate(
          data, year = year(date), unit = do.call(what = x, args = list(date))
        ) %>% dplyr::group_by(`active contract ticker`, field, year, unit) %>% 
          dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
          dplyr::group_by(`active contract ticker`, field) %>% 
          dplyr::select(-unit) %>%
          dplyr::mutate(
            value = (value / dplyr::lag(value, 1L) - 1L), 
            value = ifelse(is.infinite(value), NA, value)
          ) %>%
          dplyr::group_by(field, date) %>% 
          dplyr::summarise(value = mean(value, na.rm = T)) %>% 
          dplyr::mutate(year = lubridate::year(date)) %>%
          dplyr::left_join(
            dplyr::filter(`aggregate CHP regimes`, is.na(period)) %>% 
              dplyr::select(date, regime), 
            by = "date"
          ) %>% dplyr::filter(! is.na(regime)) %>%
          dplyr::group_by(field, year, regime) %>%
          dplyr::summarise(
            min = min(value, na.rm = T), 
            max = max(value, na.rm = T),
            mean = list(tryCatch(
              { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
              error = function(e) { NA }
            )
            ),
            sd = sd(value, na.rm = T)) %>% 
          dplyr::ungroup() %>% 
          dplyr::mutate(
            period = "year", frequency = paste0(x, "ly"), type = "returns", 
            country = combinations[y, "country"], sector = combinations[y, "sector"], 
            subsector = combinations[y, "subsector"]
          ) %>%
          dplyr::select(
            type, period, frequency, country, sector, subsector, field, year, 
            regime, min, max, mean, sd
          )
      }) %>% data.table::rbindlist()
    
    regimes <- data.table::rbindlist(list(levels, days, others))
    
    data.table::rbindlist(list(whole, regimes))
  }

#### by subperiod ####
subperiods <- foreach(
  y = 1L:nrow(combinations), 
  .combine = rbind
) %dopar% {
  library(magrittr); library(lubridate)
  lapply(
    unique(periods$period), 
    function(z){
      
      start <- dplyr::filter(periods, period == z, bound == "start") %>% 
        dplyr::select(date) %>% purrr::flatten_chr()
      end <- dplyr::filter(periods, period == z, bound == "end") %>% 
        dplyr::select(date) %>% purrr::flatten_chr()
      
      data <- dplyr::filter(
        data, country == combinations[y, "country"], 
        sector == combinations[y, "sector"], 
        subsector == combinations[y, "subsector"],
        date >= as.Date(start), date <= as.Date(end)
      ) %>% 
        dplyr::select(-c(country, sector))
      
      # levels
      levels <- dplyr::group_by(data, field, date) %>% 
        dplyr::summarise(value = mean(value, na.rm = T)) %>% 
        dplyr::group_by(field) %>% 
        dplyr::summarise(
          min = min(value, na.rm = T), 
          max = max(value, na.rm = T),
          mean = list(tryCatch(
            { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
            error = function(e) { NA }
          )
          ), sd = sd(value, na.rm = T)
        ) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = z, frequency = NA, type = "levels", year = NA, 
          country = combinations[y, "country"], 
          sector = combinations[y, "sector"], 
          subsector = combinations[y, "subsector"], regime = "all"
        ) %>%
        dplyr::select(
          type, period, frequency, country, sector, subsector, field, year, 
          regime, min, max, mean, sd
        )
      
      # returns 
      ## days
      days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L), 
          value = ifelse(is.infinite(value), NA, value)
        ) %>%
        dplyr::group_by(field, date) %>% 
        dplyr::summarise(value = mean(value, na.rm = T)) %>%
        dplyr::group_by(field) %>% 
        dplyr::summarise(
          min = min(value, na.rm = T),
          max = max(value, na.rm = T),
          mean = list(tryCatch(
            { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
            error = function(e) { NA }
          )
          ), sd = sd(value, na.rm = T)
        ) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = z, frequency = "daily", type = "returns", year = NA, 
          country = combinations[y, "country"], sector = combinations[y, "sector"], 
          subsector = combinations[y, "subsector"], regime = "all"
        ) %>%
        dplyr::select(
          type, period, frequency, country, sector, subsector, field, year, 
          regime, min, max, mean, sd
        )
      
      ## other frequencies
      others <- lapply(
        c("week", "month"), 
        function(x){
          dplyr::mutate(
            data, year = year(date), unit = do.call(what = x, args = list(date))
          ) %>% dplyr::group_by(field, year, unit) %>% 
            dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
            dplyr::group_by(`active contract ticker`, field) %>% 
            dplyr::select(-unit) %>%
            dplyr::mutate(
              value = (value / dplyr::lag(value, 1L) - 1L), 
              value = ifelse(is.infinite(value), NA, value)
            ) %>%
            dplyr::group_by(field, date) %>% 
            dplyr::summarise(value = mean(value, na.rm = T)) %>%
            dplyr::group_by(field) %>%
            dplyr::summarise(
              min = min(value, na.rm = T), 
              max = max(value, na.rm = T),
              mean = list(tryCatch(
                { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
                error = function(e) { NA }
              )
              ),
              sd = sd(value, na.rm = T)
            ) %>% 
            dplyr::ungroup() %>% 
            dplyr::mutate(
              period = z, frequency = paste0(x, "ly"), type = "returns", year = NA, 
              country = combinations[y, "country"], sector = combinations[y, "sector"], 
              subsector = combinations[y, "subsector"], regime = "all"
            ) %>%
            dplyr::select(
              type, period, frequency, country, sector, subsector, field, year, 
              regime, min, max, mean, sd
            )
        }) %>% data.table::rbindlist()
      
      whole <- data.table::rbindlist(list(levels, days, others))
      
      # levels
      levels <- dplyr::group_by(data, field, date) %>% 
        dplyr::summarise(value = mean(value, na.rm = T)) %>% 
        dplyr::left_join(
          dplyr::filter(`aggregate CHP regimes`, ! is.na(period)) %>% 
            dplyr::select(date, regime), 
          by = "date") %>% 
        dplyr::filter(! is.na(regime)) %>% 
        dplyr::group_by(field, regime) %>% 
        dplyr::summarise(
          min = min(value, na.rm = T), 
          max = max(value, na.rm = T),
          mean = list(
            tryCatch(
              { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
              error = function(e) { NA }
            )
          ), sd = sd(value, na.rm = T)
        ) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = z, frequency = NA, type = "levels", year = NA, 
          country = combinations[y, "country"], sector = combinations[y, "sector"], 
          subsector = combinations[y, "subsector"]
        ) %>%
        dplyr::select(
          type, period, frequency, country, sector, subsector, field, year, 
          regime, min, max, mean, sd
        )
      
      # returns 
      ## days
      days <- dplyr::group_by(data, `active contract ticker`, field) %>% 
        dplyr::mutate(
          value = (value / dplyr::lag(value, 1L) - 1L), 
          value = ifelse(is.infinite(value), NA, value)
        ) %>%
        dplyr::group_by(field, date) %>% 
        dplyr::summarise(value = mean(value, na.rm = T)) %>%
        dplyr::left_join(
          dplyr::filter(`aggregate CHP regimes`, ! is.na(period)) %>% 
            dplyr::select(date, regime), 
          by = "date"
        ) %>% 
        dplyr::filter(! is.na(regime)) %>% 
        dplyr::group_by(field, regime) %>% 
        dplyr::summarise(
          min = min(value, na.rm = T), 
          max = max(value, na.rm = T),
          mean = list(
            tryCatch(
              { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
              error = function(e) { NA }
            )
          ), sd = sd(value, na.rm = T)
        ) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = z, frequency = "daily", type = "returns", year = NA, 
          country = combinations[y, "country"], sector = combinations[y, "sector"], 
          subsector = combinations[y, "subsector"]
        ) %>%
        dplyr::select(
          type, period, frequency, country, sector, subsector, field, year, 
          regime, min, max, mean, sd
        )
      
      ## other frequencies
      others <- lapply(
        c("week", "month"), 
        function(x){
          dplyr::mutate(
            data, year = year(date), unit = do.call(what = x, args = list(date))
          ) %>% 
            dplyr::group_by(field, year, unit) %>% 
            dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
            dplyr::group_by(`active contract ticker`, field) %>% 
            dplyr::select(-unit) %>%
            dplyr::mutate(
              value = (value / dplyr::lag(value, 1L) - 1L), 
              value = ifelse(is.infinite(value), NA, value)
            ) %>%
            dplyr::group_by(field, date) %>% 
            dplyr::summarise(value = mean(value, na.rm = T)) %>%
            dplyr::left_join(
              dplyr::filter(`aggregate CHP regimes`, ! is.na(period)) %>% 
                dplyr::select(date, regime), 
              by = "date"
            ) %>% 
            dplyr::filter(! is.na(regime)) %>% 
            dplyr::group_by(field, regime) %>% 
            dplyr::summarise(
              min = min(value, na.rm = T), 
              max = max(value, na.rm = T),
              mean = list(
                tryCatch(
                  { t.test(value, alternative = "two.sided", mu = 0, na.rm = T) }, 
                  error = function(e) { NA }
                )
              ), sd = sd(value, na.rm = T)
            ) %>% 
            dplyr::ungroup() %>% 
            dplyr::mutate(
              period = z, frequency = paste0(x, "ly"), type = "returns", year = NA, 
              country = combinations[y, "country"], sector = combinations[y, "sector"], 
              subsector = combinations[y, "subsector"]
            ) %>%
            dplyr::select(
              type, period, frequency, country, sector, subsector, field, year, 
              regime, min, max, mean, sd
            )
        }) %>% 
        data.table::rbindlist()
      
      regimes <- data.table::rbindlist(list(levels, days, others))
      
      data.table::rbindlist(list(whole, regimes))
      
    }) %>% data.table::rbindlist()
}

subsectors <- data.table::rbindlist(list(years, subperiods))

`EW portfolios` <- data.table::rbindlist(list(countries, sectors, subsectors))

`market variables` <- tibble::tibble(
  assets = c("individual commodities", "countries - sectors - subsectors"),
  results = list(`individual commodities`, `EW portfolios`)
)


# stats <- readr::read_rds(
#   here::here("explore", "results", "new", "descriptive-statistics.rds")
# )
# stats$results[[1L]]$results[[2L]]$results[[2L]] <- `EW portfolios`
# readr::write_rds(
#   stats,
#   here::here("explore", "results", "new", "descriptive-statistics.rds")
# )


## CFTC ####
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

### by single commodity ####
#### by year ####
years <- foreach(
  y = unique(pressure$`active contract ticker`), 
  .combine = rbind
) %dopar% {
  library(magrittr); library(lubridate)
  
  data <- dplyr::filter(pressure, `active contract ticker` == y) %>% 
    dplyr::select(`active contract ticker`, participant, date, pressure) %>% 
    dplyr::mutate(year = year(date))
  
  # levels
  levels <- dplyr::group_by(data, `active contract ticker`, participant, year) %>% 
    dplyr::summarise(
      min = min(pressure, na.rm = T), 
      max = max(pressure, na.rm = T),
      mean = list(
        tryCatch(
          { t.test(pressure, alternative = "two.sided", mu = 0, na.rm = T) }, 
          error = function(e) { NA }
        )
      ),
      sd = sd(pressure, na.rm = T)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(period = "year", frequency = NA, type = "levels") %>%
    dplyr::select(
      type, period, frequency, `active contract ticker`, participant, year, 
      min, max, mean, sd
    )
  
  # returns 
  ## weeks
  weeks <- dplyr::group_by(data, `active contract ticker`, participant) %>% 
    dplyr::mutate(
      pressure = (pressure / dplyr::lag(pressure, 1L) - 1L), 
      pressure = ifelse(is.infinite(pressure), NA, pressure)
    ) %>%
    dplyr::group_by(`active contract ticker`, participant, year) %>% 
    dplyr::summarise(
      min = min(pressure, na.rm = T), 
      max = max(pressure, na.rm = T),
      mean = list(
        tryCatch(
          { t.test(pressure, alternative = "two.sided", mu = 0, na.rm = T) }, 
          error = function(e) { NA }
        )
      ),
      sd = sd(pressure, na.rm = T)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(period = "year", frequency = "weekly", type = "returns") %>%
    dplyr::select(
      type, period, frequency, `active contract ticker`, participant, year,
      min, max, mean, sd
    )
  
  ## other frequencies
  others <- lapply(
    c("month"), 
    function(x){
      
      dplyr::mutate(data, unit = do.call(what = x, args = list(date))) %>% 
        dplyr::group_by(participant, year, unit) %>% 
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
        dplyr::group_by(participant) %>% dplyr::select(-unit) %>%
        dplyr::mutate(
          pressure = (pressure / dplyr::lag(pressure, 1L) - 1L), 
          pressure = ifelse(is.infinite(pressure), NA, pressure)
        ) %>%
        dplyr::group_by(`active contract ticker`, participant, year) %>% 
        dplyr::summarise(
          min = min(pressure, na.rm = T), 
          max = max(pressure, na.rm = T),
          mean = list(
            tryCatch(
              { t.test(pressure, alternative = "two.sided", mu = 0, na.rm = T) },
              error = function(e) { NA }
            )
          ),
          sd = sd(pressure, na.rm = T)
        ) %>%
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = "year", frequency = paste0(x, "ly"), type = "returns"
        ) %>%
        dplyr::select(
          type, period, frequency, `active contract ticker`, participant, year, 
          min, max, mean, sd
        )
    }) %>% 
    data.table::rbindlist()
  
  data.table::rbindlist(list(levels, weeks, others))
}

#### by subperiod ####
subperiods <- foreach(
  y = unique(pressure$`active contract ticker`), 
  .combine = rbind
) %dopar% {
  library(magrittr); library(lubridate)
  
  data <- dplyr::filter(pressure, `active contract ticker` == y) %>% 
    dplyr::select(`active contract ticker`, participant, date, pressure)
  
  lapply(
    unique(periods$period), 
    function(z){
      
      start <- dplyr::filter(periods, period == z, bound == "start") %>% 
        dplyr::select(date) %>% purrr::flatten_chr()
      end <- dplyr::filter(periods, period == z, bound == "end") %>% 
        dplyr::select(date) %>% purrr::flatten_chr()
      
      data <- dplyr::filter(data, date >= as.Date(start), date <= as.Date(end))
      
      # levels
      levels <- dplyr::group_by(data, `active contract ticker`, participant) %>% 
        dplyr::summarise(
          min = min(pressure, na.rm = T), 
          max = max(pressure, na.rm = T),
          mean = list(
            tryCatch(
              { t.test(pressure, alternative = "two.sided", mu = 0, na.rm = T) }, 
              error = function(e) { NA }
            )
          ), sd = sd(pressure, na.rm = T)
        ) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(period = z, frequency = NA, type = "levels", year = NA) %>%
        dplyr::select(
          type, period, frequency, `active contract ticker`, participant, year, 
          min, max, mean, sd
        )
      
      # returns 
      ## weeks
      weeks <- dplyr::group_by(data, `active contract ticker`, participant) %>% 
        dplyr::mutate(
          pressure = (pressure / dplyr::lag(pressure, 1L) - 1L), 
          pressure = ifelse(is.infinite(pressure), NA, pressure)
        ) %>%
        dplyr::group_by(`active contract ticker`, participant) %>% 
        dplyr::summarise(
          min = min(pressure, na.rm = T), max = max(pressure, na.rm = T),
          mean = list(
            tryCatch(
              { t.test(pressure, alternative = "two.sided", mu = 0, na.rm = T) }, 
              error = function(e) { NA }
            )
          ),
          sd = sd(pressure, na.rm = T)
        ) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = z, frequency = "weekly", type = "returns", year = NA
        ) %>%
        dplyr::select(
          type, period, frequency, `active contract ticker`, participant, year, 
          min, max, mean, sd
        )
      
      ## other frequencies
      others <- lapply(
        c("month"), 
        function(x){
          
          dplyr::mutate(
            data, year = year(date), unit = do.call(what = x, args = list(date))
          ) %>% dplyr::group_by(participant, year, unit) %>% 
            dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
            dplyr::group_by(participant) %>% dplyr::select(-c(year, unit)) %>%
            dplyr::mutate(
              pressure = (pressure / dplyr::lag(pressure, 1L) - 1L), 
              pressure = ifelse(is.infinite(pressure), NA, pressure)
            ) %>%
            dplyr::group_by(`active contract ticker`, participant) %>% 
            dplyr::summarise(
              min = min(pressure, na.rm = T), 
              max = max(pressure, na.rm = T),
              mean = list(tryCatch(
                { t.test(pressure, alternative = "two.sided", mu = 0, na.rm = T) }, 
                error = function(e) { NA }
              )
              ),
              sd = sd(pressure, na.rm = T)
            ) %>% 
            dplyr::ungroup() %>% 
            dplyr::mutate(
              period = z, frequency = paste0(x, "ly"), type = "returns", year = NA
            ) %>%
            dplyr::select(
              type, period, frequency, `active contract ticker`, participant, year, 
              min, max, mean, sd
            )
        }) %>% 
        data.table::rbindlist()
      
      data.table::rbindlist(list(levels, weeks, others))
    }) %>% data.table::rbindlist()
}

`individual commodities` <- data.table::rbindlist(list(years, subperiods))


### by country ####
#### by year ####
data <- dplyr::left_join(
  pressure, 
  dplyr::left_join(
    dplyr::select(tickers_futures, ticker, MIC), 
    dplyr::select(exchanges, MIC, country), 
    by = "MIC"
  ) %>%
    dplyr::rename(`active contract ticker` = ticker), 
  by = "active contract ticker"
) %>% dplyr::select(-MIC)

combinations <- dplyr::distinct(data, country) %>% as.data.frame()

years <- foreach(
  y = 1L:nrow(combinations), 
  .combine = rbind) %dopar% {
    library(magrittr); library(lubridate)
    
    data <- dplyr::filter(data, country == combinations[y, "country"]) %>% 
      dplyr::select(-country)
    
    # levels
    levels <- dplyr::group_by(data, participant, date) %>% 
      dplyr::summarise(pressure = mean(pressure, na.rm = T)) %>% 
      dplyr::mutate(year = lubridate::year(date)) %>% 
      dplyr::group_by(participant, year) %>% 
      dplyr::summarise(
        min = min(pressure, na.rm = T), 
        max = max(pressure, na.rm = T),
        mean = list(
          tryCatch(
            { t.test(pressure, alternative = "two.sided", mu = 0, na.rm = T) }, 
            error = function(e) { NA }
          )
        ),
        sd = sd(pressure, na.rm = T)
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        period = "year", frequency = NA, type = "levels", 
        country = combinations[y, "country"], sector = "all", subsector = "all"
      ) %>%
      dplyr::select(
        type, period, frequency, country, sector, subsector, participant, year, 
        min, max, mean, sd
      )
    
    # returns 
    ## weeks
    weeks <- dplyr::group_by(data, `active contract ticker`, participant) %>% 
      dplyr::mutate(
        pressure = (pressure / dplyr::lag(pressure, 1L) - 1L), 
        pressure = ifelse(is.infinite(pressure), NA, pressure)
      ) %>%
      dplyr::group_by(participant, date) %>% 
      dplyr::summarise(pressure = mean(pressure, na.rm = T)) %>% 
      dplyr::mutate(year = lubridate::year(date)) %>%
      dplyr::group_by(participant, year) %>% 
      dplyr::summarise(
        min = min(pressure, na.rm = T), 
        max = max(pressure, na.rm = T),
        mean = list(
          tryCatch(
            { t.test(pressure, alternative = "two.sided", mu = 0, na.rm = T) }, 
            error = function(e) { NA }
          )
        ),
        sd = sd(pressure, na.rm = T)
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        period = "year", frequency = "weekly", type = "returns", 
        country = combinations[y, "country"], sector = "all", subsector = "all"
      ) %>%
      dplyr::select(
        type, period, frequency, country, sector, subsector, participant, year, 
        min, max, mean, sd
      )
    
    ## other frequencies
    others <- lapply(
      c("month"), 
      function(x){
        dplyr::mutate(
          data, year = year(date), unit = do.call(what = x, args = list(date))
        ) %>% dplyr::group_by(participant, year, unit) %>% 
          dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
          dplyr::group_by(`active contract ticker`, participant) %>% 
          dplyr::select(-unit) %>%
          dplyr::mutate(
            pressure = (pressure / dplyr::lag(pressure, 1L) - 1L), 
            pressure = ifelse(is.infinite(pressure), NA, pressure)
          ) %>%
          dplyr::group_by(participant, date) %>% 
          dplyr::summarise(pressure = mean(pressure, na.rm = T)) %>% 
          dplyr::mutate(year = lubridate::year(date)) %>%
          dplyr::group_by(participant, year) %>% 
          dplyr::summarise(
            min = min(pressure, na.rm = T), 
            max = max(pressure, na.rm = T),
            mean = list(
              tryCatch(
                { t.test(pressure, alternative = "two.sided", mu = 0, na.rm = T) }, 
                error = function(e) { NA }
              )
            ),
            sd = sd(pressure, na.rm = T)
          ) %>% 
          dplyr::ungroup() %>% 
          dplyr::mutate(
            period = "year", frequency = paste0(x, "ly"), type = "returns", 
            country = combinations[y, "country"], sector = "all", subsector = "all"
          ) %>%
          dplyr::select(
            type, period, frequency, country, sector, subsector, participant, 
            year, min, max, mean, sd
          )
        
      }) %>% 
      data.table::rbindlist()
    
    data.table::rbindlist(list(levels, weeks, others))
  }

#### by subperiod ####
subperiods <- foreach(y = 1L:nrow(combinations), .combine = rbind) %dopar% {
  library(magrittr); library(lubridate)
  lapply(unique(periods$period), function(z){
    
    start <- dplyr::filter(periods, period == z, bound == "start") %>% 
      dplyr::select(date) %>% purrr::flatten_chr()
    end <- dplyr::filter(periods, period == z, bound == "end") %>% 
      dplyr::select(date) %>% purrr::flatten_chr()
    
    data <- dplyr::filter(
      data, country == combinations[y, "country"], 
      date >= as.Date(start), date <= as.Date(end)
    ) %>% 
      dplyr::select(-country)
    
    # levels
    levels <- dplyr::group_by(data, participant, date) %>% 
      dplyr::summarise(pressure = mean(pressure, na.rm = T)) %>% 
      dplyr::group_by(participant) %>% 
      dplyr::summarise(
        min = min(pressure, na.rm = T), 
        max = max(pressure, na.rm = T),
        mean = list(tryCatch(
          { t.test(pressure, alternative = "two.sided", mu = 0, na.rm = T) }, 
          error = function(e) { NA }
        )
        ),
        sd = sd(pressure, na.rm = T)
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        period = z, frequency = NA, type = "levels", year = NA, 
        country = combinations[y, "country"], sector = "all", subsector = "all"
      ) %>%
      dplyr::select(
        type, period, frequency, country, sector, subsector, participant, 
        year, min, max, mean, sd
      )
    
    # returns 
    ## weeks
    weeks <- dplyr::group_by(data, `active contract ticker`, participant) %>% 
      dplyr::mutate(
        pressure = (pressure / dplyr::lag(pressure, 1L) - 1L), 
        pressure = ifelse(is.infinite(pressure), NA, pressure)
      ) %>%
      dplyr::group_by(participant, date) %>% 
      dplyr::summarise(pressure = mean(pressure, na.rm = T)) %>% 
      dplyr::group_by(participant) %>% 
      dplyr::summarise(
        min = min(pressure, na.rm = T), 
        max = max(pressure, na.rm = T),
        mean = list(tryCatch(
          { t.test(pressure, alternative = "two.sided", mu = 0, na.rm = T) }, 
          error = function(e) { NA }
        )
        ),
        sd = sd(pressure, na.rm = T)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        period = z, frequency = "weekly", type = "returns", year = NA, 
        country = combinations[y, "country"], sector = "all", subsector = "all"
      ) %>%
      dplyr::select(
        type, period, frequency, country, sector, subsector, participant, year, 
        min, max, mean, sd
      )
    
    ## other frequencies
    others <- lapply(
      c("month"), 
      function(x){
        
        dplyr::mutate(
          data, year = year(date), unit = do.call(what = x, args = list(date))
        ) %>% dplyr::group_by(participant, year, unit) %>% 
          dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
          dplyr::group_by(`active contract ticker`, participant) %>% 
          dplyr::select(-unit) %>%
          dplyr::mutate(
            pressure = (pressure / dplyr::lag(pressure, 1L) - 1L), 
            pressure = ifelse(is.infinite(pressure), NA, pressure)
          ) %>%
          dplyr::group_by(participant, date) %>% 
          dplyr::summarise(pressure = mean(pressure, na.rm = T)) %>% 
          dplyr::group_by(participant) %>% 
          dplyr::summarise(
            min = min(pressure, na.rm = T), 
            max = max(pressure, na.rm = T),
            mean = list(tryCatch(
              { t.test(pressure, alternative = "two.sided", mu = 0, na.rm = T) }, 
              error = function(e) { NA }
            )
            ),
            sd = sd(pressure, na.rm = T)
          ) %>% 
          dplyr::ungroup() %>% 
          dplyr::mutate(period = z, frequency = paste0(x, "ly"), type = "returns", year = NA, country = combinations[y, "country"], 
                        sector = "all", subsector = "all") %>%
          dplyr::select(type, period, frequency, country, sector, subsector, participant, year, min, max, mean, sd)
      }) %>% data.table::rbindlist()
    
    data.table::rbindlist(list(levels, weeks, others))
  }) %>% data.table::rbindlist()
}

countries <- data.table::rbindlist(list(years, subperiods))

### by sector ####
#### by year ####
data <- dplyr::select(
  pressure, `active contract ticker`, participant, date, pressure
) %>%
  dplyr::left_join(
    dplyr::select(
      tickers_futures, `active contract ticker` = ticker, MIC, sector
    ), 
    by = "active contract ticker"
  ) %>%
  dplyr::left_join(dplyr::select(exchanges, MIC, country), by = "MIC") %>% 
  dplyr::select(-MIC)

combinations <- dplyr::distinct(data, country, sector) %>% as.data.frame()

years <- foreach(
  y = 1L:nrow(combinations), 
  .combine = rbind
) %dopar% {
  library(magrittr); library(lubridate)
  
  data <- dplyr::filter(
    data, country == combinations[y, "country"], 
    sector == combinations[y, "sector"]
  ) %>% dplyr::select(-c(country, sector))
  
  # levels
  levels <- dplyr::group_by(data, participant, date) %>% 
    dplyr::summarise(pressure = mean(pressure, na.rm = T)) %>% 
    dplyr::mutate(year = lubridate::year(date)) %>% 
    dplyr::group_by(participant, year) %>% 
    dplyr::summarise(
      min = min(pressure, na.rm = T), 
      max = max(pressure, na.rm = T),
      mean = list(tryCatch(
        { t.test(pressure, alternative = "two.sided", mu = 0, na.rm = T) },
        error = function(e) { NA }
      )
      ),
      sd = sd(pressure, na.rm = T)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      period = "year", frequency = NA, type = "levels", 
      country = combinations[y, "country"], sector = combinations[y, "sector"], 
      subsector = "all"
    ) %>%
    dplyr::select(
      type, period, frequency, country, sector, subsector, participant, 
      year, min, max, mean, sd
    )
  
  # returns 
  ## weeks
  weeks <- dplyr::group_by(data, `active contract ticker`, participant) %>% 
    dplyr::mutate(
      pressure = (pressure / dplyr::lag(pressure, 1L) - 1L), 
      pressure = ifelse(is.infinite(pressure), NA, pressure)
    ) %>%
    dplyr::group_by(participant, date) %>% 
    dplyr::summarise(pressure = mean(pressure, na.rm = T)) %>% 
    dplyr::mutate(year = lubridate::year(date)) %>%
    dplyr::group_by(participant, year) %>% 
    dplyr::summarise(
      min = min(pressure, na.rm = T), 
      max = max(pressure, na.rm = T),
      mean = list(tryCatch(
        { t.test(pressure, alternative = "two.sided", mu = 0, na.rm = T) }, 
        error = function(e) { NA }
      )
      ),
      sd = sd(pressure, na.rm = T)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      period = "year", frequency = "weekly", type = "returns", 
      country = combinations[y, "country"],
      sector = combinations[y, "sector"], subsector = "all"
    ) %>%
    dplyr::select(
      type, period, frequency, country, sector, subsector, participant, year,
      min, max, mean, sd
    )
  
  ## other frequencies
  others <- lapply(
    c("month"), 
    function(x){
      dplyr::mutate(
        data, year = year(date), unit = do.call(what = x, args = list(date))
      ) %>% 
        dplyr::group_by(participant, year, unit) %>%
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
        dplyr::group_by(`active contract ticker`, participant) %>% 
        dplyr::select(-unit) %>%
        dplyr::mutate(
          pressure = (pressure / dplyr::lag(pressure, 1L) - 1L), 
          pressure = ifelse(is.infinite(pressure), NA, pressure)
        ) %>% dplyr::group_by(participant, date) %>% 
        dplyr::summarise(pressure = mean(pressure, na.rm = T)) %>% 
        dplyr::mutate(year = lubridate::year(date)) %>%
        dplyr::group_by(participant, year) %>%
        dplyr::summarise(
          min = min(pressure, na.rm = T), max = max(pressure, na.rm = T),
          mean = list(tryCatch(
            { t.test(pressure, alternative = "two.sided", mu = 0, na.rm = T) }, 
            error = function(e) { NA }
          )
          ),
          sd = sd(pressure, na.rm = T)
        ) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(period = "year", frequency = paste0(x, "ly"), type = "returns", country = combinations[y, "country"], 
                      sector = combinations[y, "sector"], subsector = "all") %>%
        dplyr::select(type, period, frequency, country, sector, subsector, participant, year, min, max, mean, sd)
      
    }) %>% 
    data.table::rbindlist()
  
  data.table::rbindlist(list(levels, weeks, others))
}

#### by subperiod ####
subperiods <- foreach(
  y = 1L:nrow(combinations), 
  .combine = rbind
) %dopar% {
  library(magrittr); library(lubridate)
  lapply(
    unique(periods$period), 
    function(z){
      
      start <- dplyr::filter(periods, period == z, bound == "start") %>% 
        dplyr::select(date) %>% purrr::flatten_chr()
      end <- dplyr::filter(periods, period == z, bound == "end") %>% 
        dplyr::select(date) %>% purrr::flatten_chr()
      
      data <- dplyr::filter(
        data, country == combinations[y, "country"], 
        sector == combinations[y, "sector"],
        date >= as.Date(start), date <= as.Date(end)
      ) %>% dplyr::select(-c(country, sector))
      
      # levels
      levels <- dplyr::group_by(data, participant, date) %>% 
        dplyr::summarise(pressure = mean(pressure, na.rm = T)) %>% 
        dplyr::group_by(participant) %>% 
        dplyr::summarise(
          min = min(pressure, na.rm = T), 
          max = max(pressure, na.rm = T),
          mean = list(tryCatch(
            { t.test(pressure, alternative = "two.sided", mu = 0, na.rm = T) }, 
            error = function(e) { NA }
          )
          ),
          sd = sd(pressure, na.rm = T)
        ) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = z, frequency = NA, type = "levels", year = NA, 
          country = combinations[y, "country"], sector = combinations[y, "sector"], 
          subsector = "all"
        ) %>%
        dplyr::select(
          type, period, frequency, country, sector, subsector, participant, year, 
          min, max, mean, sd
        )
      
      # returns 
      ## weeks
      weeks <- dplyr::group_by(data, `active contract ticker`, participant) %>% 
        dplyr::mutate(
          pressure = (pressure / dplyr::lag(pressure, 1L) - 1L), 
          pressure = ifelse(is.infinite(pressure), NA, pressure)
        ) %>%
        dplyr::group_by(participant, date) %>% 
        dplyr::summarise(pressure = mean(pressure, na.rm = T)) %>% 
        dplyr::group_by(participant) %>% 
        dplyr::summarise(
          min = min(pressure, na.rm = T), 
          max = max(pressure, na.rm = T),
          mean = list(
            tryCatch(
              { t.test(pressure, alternative = "two.sided", mu = 0, na.rm = T) }, 
              error = function(e) { NA }
            )
          ),
          sd = sd(pressure, na.rm = T)
        ) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = z, frequency = "weekly", type = "returns", year = NA, 
          country = combinations[y, "country"], sector = combinations[y, "sector"], 
          subsector = "all"
        ) %>%
        dplyr::select(
          type, period, frequency, country, sector, subsector, participant, year, 
          min, max, mean, sd
        )
      
      ## other frequencies
      others <- lapply(
        c("month"), 
        function(x){
          
          dplyr::mutate(
            data, year = year(date), unit = do.call(what = x, args = list(date))
          ) %>% 
            dplyr::group_by(participant, year, unit) %>% 
            dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
            dplyr::group_by(`active contract ticker`, participant) %>% 
            dplyr::select(-unit) %>%
            dplyr::mutate(
              pressure = (pressure / dplyr::lag(pressure, 1L) - 1L), 
              pressure = ifelse(is.infinite(pressure), NA, pressure)
            ) %>%
            dplyr::group_by(participant, date) %>% 
            dplyr::summarise(pressure = mean(pressure, na.rm = T)) %>% 
            dplyr::group_by(participant) %>% 
            dplyr::summarise(
              min = min(pressure, na.rm = T), 
              max = max(pressure, na.rm = T),
              mean = list(
                tryCatch(
                  { t.test(pressure, alternative = "two.sided", mu = 0, na.rm = T) }, 
                  error = function(e) { NA }
                )
              ),
              sd = sd(pressure, na.rm = T)
            ) %>% 
            dplyr::ungroup() %>% 
            dplyr::mutate(
              period = z, frequency = paste0(x, "ly"), type = "returns", 
              year = NA, country = combinations[y, "country"], 
              sector = combinations[y, "sector"], 
              subsector = "all"
            ) %>%
            dplyr::select(
              type, period, frequency, country, sector, subsector, participant, 
              year, min, max, mean, sd
            )
        }) %>% 
        data.table::rbindlist()
      
      data.table::rbindlist(list(levels, weeks, others))
    }) %>% data.table::rbindlist()
}

sectors <- data.table::rbindlist(list(years, subperiods))


### by subsector ####
#### by year ####
data <- dplyr::select(
  pressure, `active contract ticker`, participant, date, pressure
) %>%
  dplyr::left_join(
    dplyr::select(
      tickers_futures, `active contract ticker` = ticker, MIC, sector, subsector), 
    by = "active contract ticker"
  ) %>%
  dplyr::left_join(dplyr::select(exchanges, MIC, country), by = "MIC") %>% 
  dplyr::select(-MIC)

combinations <- dplyr::distinct(data, country, sector, subsector) %>% 
  as.data.frame()

years <- foreach(
  y = 1L:nrow(combinations), 
  .combine = rbind
) %dopar% {
  library(magrittr); library(lubridate)
  
  data <- dplyr::filter(
    data, country == combinations[y, "country"], sector == combinations[y, "sector"], 
    subsector == combinations[y, "subsector"]
  ) %>% dplyr::select(-c(country, sector, subsector))
  
  # levels
  levels <- dplyr::group_by(data, participant, date) %>% 
    dplyr::summarise(pressure = mean(pressure, na.rm = T)) %>% 
    dplyr::mutate(year = lubridate::year(date)) %>% 
    dplyr::group_by(participant, year) %>% 
    dplyr::summarise(
      min = min(pressure, na.rm = T), 
      max = max(pressure, na.rm = T),
      mean = list(tryCatch(
        { t.test(pressure, alternative = "two.sided", mu = 0, na.rm = T) }, 
        error = function(e) { NA }
      )
      ), sd = sd(pressure, na.rm = T)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      period = "year", frequency = NA, type = "levels", 
      country = combinations[y, "country"], sector = combinations[y, "sector"], 
      subsector = combinations[y, "subsector"]
    ) %>%
    dplyr::select(
      type, period, frequency, country, sector, subsector, participant, 
      year, min, max, mean, sd
    )
  
  # returns 
  ## weeks
  weeks <- dplyr::group_by(data, `active contract ticker`, participant) %>% 
    dplyr::mutate(
      pressure = (pressure / dplyr::lag(pressure, 1L) - 1L), 
      pressure = ifelse(is.infinite(pressure), NA, pressure)
    ) %>%
    dplyr::group_by(participant, date) %>% 
    dplyr::summarise(pressure = mean(pressure, na.rm = T)) %>% 
    dplyr::mutate(year = lubridate::year(date)) %>%
    dplyr::group_by(participant, year) %>% 
    dplyr::summarise(
      min = min(pressure, na.rm = T), 
      max = max(pressure, na.rm = T),
      mean = list(tryCatch(
        { t.test(pressure, alternative = "two.sided", mu = 0, na.rm = T) }, 
        error = function(e) { NA }
      )
      ), sd = sd(pressure, na.rm = T)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      period = "year", frequency = "weekly", type = "returns",
      country = combinations[y, "country"], sector = combinations[y, "sector"], 
      subsector = combinations[y, "subsector"]
    ) %>%
    dplyr::select(
      type, period, frequency, country, sector, subsector, participant, year, 
      min, max, mean, sd
    )
  
  ## other frequencies
  others <- lapply(
    c("month"), 
    function(x){
      dplyr::mutate(
        data, year = year(date), unit = do.call(what = x, args = list(date))
      ) %>% dplyr::group_by(participant, year, unit) %>% 
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
        dplyr::group_by(`active contract ticker`, participant) %>% 
        dplyr::select(-unit) %>%
        dplyr::mutate(
          pressure = (pressure / dplyr::lag(pressure, 1L) - 1L), 
          pressure = ifelse(is.infinite(pressure), NA, pressure)
        ) %>%
        dplyr::group_by(participant, date) %>% 
        dplyr::summarise(pressure = mean(pressure, na.rm = T)) %>% 
        dplyr::mutate(year = lubridate::year(date)) %>%
        dplyr::group_by(participant, year) %>% 
        dplyr::summarise(
          min = min(pressure, na.rm = T), 
          max = max(pressure, na.rm = T),
          mean = list(tryCatch(
            { t.test(pressure, alternative = "two.sided", mu = 0, na.rm = T) }, 
            error = function(e) { NA }
          )
          ),
          sd = sd(pressure, na.rm = T)
        ) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          period = "year", frequency = paste0(x, "ly"), type = "returns", 
          country = combinations[y, "country"], sector = combinations[y, "sector"], 
          subsector = combinations[y, "subsector"]) %>%
        dplyr::select(
          type, period, frequency, country, sector, subsector, participant, 
          year, min, max, mean, sd
        )
      
    }) %>% 
    data.table::rbindlist()
  
  data.table::rbindlist(list(levels, weeks, others))
}


#### by subperiod ####
subperiods <- foreach(
  y = 1L:nrow(combinations), 
  .combine = rbind) %dopar% {
    library(magrittr); library(lubridate)
    lapply(
      unique(periods$period), 
      function(z){
        
        start <- dplyr::filter(periods, period == z, bound == "start") %>% 
          dplyr::select(date) %>% purrr::flatten_chr()
        end <- dplyr::filter(periods, period == z, bound == "end") %>% 
          dplyr::select(date) %>% purrr::flatten_chr()
        
        data <- dplyr::filter(
          data, country == combinations[y, "country"], 
          sector == combinations[y, "sector"], 
          subsector == combinations[y, "subsector"],
          date >= as.Date(start), date <= as.Date(end)
        ) %>% 
          dplyr::select(-c(country, sector, subsector))
        
        # levels
        levels <- dplyr::group_by(data, participant, date) %>% 
          dplyr::summarise(pressure = mean(pressure, na.rm = T)) %>% 
          dplyr::group_by(participant) %>% 
          dplyr::summarise(
            min = min(pressure, na.rm = T), 
            max = max(pressure, na.rm = T),
            mean = list(tryCatch(
              { t.test(pressure, alternative = "two.sided", mu = 0, na.rm = T) }, 
              error = function(e) { NA }
            )
            ),
            sd = sd(pressure, na.rm = T)
          ) %>% 
          dplyr::ungroup() %>% 
          dplyr::mutate(
            period = z, frequency = NA, type = "levels", year = NA, 
            country = combinations[y, "country"], sector = combinations[y, "sector"], 
            subsector = combinations[y, "subsector"]
          ) %>%
          dplyr::select(
            type, period, frequency, country, sector, subsector, participant, 
            year, min, max, mean, sd
          )
        
        # returns 
        ## weeks
        weeks <- dplyr::group_by(data, `active contract ticker`, participant) %>% 
          dplyr::mutate(
            pressure = (pressure / dplyr::lag(pressure, 1L) - 1L), 
            pressure = ifelse(is.infinite(pressure), NA, pressure)
          ) %>%
          dplyr::group_by(participant, date) %>% 
          dplyr::summarise(pressure = mean(pressure, na.rm = T)) %>% 
          dplyr::group_by(participant) %>% 
          dplyr::summarise(
            min = min(pressure, na.rm = T), 
            max = max(pressure, na.rm = T),
            mean = list(tryCatch({
              t.test(pressure, alternative = "two.sided", mu = 0, na.rm = T) },
              error = function(e) { NA }
            )
            ),
            sd = sd(pressure, na.rm = T)
          ) %>% 
          dplyr::ungroup() %>% 
          dplyr::mutate(
            period = z, frequency = "weekly", type = "returns", year = NA, 
            country = combinations[y, "country"],
            sector = combinations[y, "sector"], subsector = combinations[y, "subsector"]
          ) %>%
          dplyr::select(
            type, period, frequency, country, sector, subsector, participant, 
            year, min, max, mean, sd
          )
        
        ## other frequencies
        others <- lapply(
          c("month"), 
          function(x){
            
            dplyr::mutate(
              data, 
              year = year(date), 
              unit = do.call(what = x, args = list(date))
            ) %>% 
              dplyr::group_by(participant, year, unit) %>%
              dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
              dplyr::group_by(`active contract ticker`, participant) %>% 
              dplyr::select(-unit) %>%
              dplyr::mutate(
                pressure = (pressure / dplyr::lag(pressure, 1L) - 1L), 
                pressure = ifelse(is.infinite(pressure), NA, pressure)
              ) %>%
              dplyr::group_by(participant, date) %>% 
              dplyr::summarise(pressure = mean(pressure, na.rm = T)) %>%
              dplyr::group_by(participant) %>%
              dplyr::summarise(
                min = min(pressure, na.rm = T), 
                max = max(pressure, na.rm = T),
                mean = list(tryCatch(
                  { t.test(pressure, alternative = "two.sided", mu = 0, na.rm = T) }, 
                  error = function(e) { NA }
                )
                ), sd = sd(pressure, na.rm = T)
              ) %>% 
              dplyr::ungroup() %>% 
              dplyr::mutate(
                period = z, frequency = paste0(x, "ly"), type = "returns", year = NA, 
                country = combinations[y, "country"], sector = combinations[y, "sector"], 
                subsector = combinations[y, "subsector"]
              ) %>%
              dplyr::select(
                type, period, frequency, country, sector, subsector, participant, 
                year, min, max, mean, sd
              )
            
          }) %>% 
          data.table::rbindlist()
        
        
        data.table::rbindlist(list(levels, weeks, others))
        
      }) %>% 
      data.table::rbindlist()
  }

subsectors <- data.table::rbindlist(list(years, subperiods))

`EW portfolios` <- data.table::rbindlist(list(countries, sectors, subsectors))

CFTC <- tibble::tibble(
  assets = c("individual commodities", "countries - sectors - subsectors"),
  results = list(`individual commodities`, `EW portfolios`)
)

`commodity futures` <- tibble::tibble(
  analysis = c("market variables", "CFTC"), 
  results = list(`market variables`, CFTC)
)


statistics <- tibble::tibble(
  analysis = c("commodity futures"), 
  results = list(`commodity futures`)
)

readr::write_rds(
  statistics,
  here::here("explore", "results", "descriptive-statistics.rds")
)

statistics <- readr::read_rds(
  here::here("explore", "results", "descriptive-statistics.rds")
)

# factors ####
## load data ####
start <- "1996-01-01"
end <- "2018-12-31"

`commodity futures tickers` <- dplyr::left_join(
  dplyr::select(
    tickers_futures, ticker, MIC), dplyr::select(exchanges, MIC, country), 
  by = "MIC"
) %>% 
  dplyr::filter(ticker %in% `commodity futures tickers`, country == "US") %>% 
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


## construct factors - asset pool: US commodities ####
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


## factor descriptive statistics ####
combinations <- dplyr::distinct(
  returns, `asset pool`, `update frequency`, `return frequency`, `ranking period`, 
  `long threshold`, `short threshold`, name, leg
)
#### by year ####
years <- lapply(
  1L:nrow(combinations), 
  function(y){
    
    `asset pool` <- combinations$`asset pool`[y]
    `update frequency` <- combinations$`update frequency`[y]
    `return frequency` <- combinations$`return frequency`[y]
    `ranking period` <- combinations$`ranking period`[y]
    `long threshold` <- combinations$`long threshold`[y]
    `short threshold` <- combinations$`short threshold`[y]
    name <- combinations$name[y]
    leg <- combinations$leg[y]
    
    returns <- dplyr::filter(
      returns, `asset pool` == !! `asset pool`, 
      `update frequency` == !! `update frequency`, 
      `return frequency` == !! `return frequency`,
      `ranking period` == !! `ranking period`, 
      `long threshold` == !! `long threshold`, 
      `short threshold` == !! `short threshold`,
      name == !! name, leg == !! leg
    ) %>% dplyr::mutate(period = NA, year = lubridate::year(date))
    
    whole <- dplyr::group_by(
      returns, `asset pool`, `update frequency`, `return frequency`,
      `ranking period`, `long threshold`, `short threshold`, name, leg,
      period, year
    ) %>%
      dplyr::summarise(
        min = min(return, na.rm = T),
        max = max(return, na.rm = T),
        mean = list(tryCatch(
          { t.test(return, alternative = "two.sided", mu = 0, na.rm = T) },
          error = function(e) { NA }
        )
        ), sd = sd(return, na.rm = T)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(regime = "all") %>%
      dplyr::select(
        `asset pool`, `update frequency`, `return frequency`, `ranking period`,
        `long threshold`, `short threshold`, name, leg, period, year, regime,
        min, max, mean, sd
      )
    
    returns <- dplyr::left_join(
      returns, dplyr::filter(`aggregate CHP regimes`, is.na(period)) %>% 
        dplyr::select(date, regime), 
      by = "date"
    ) %>% 
      dplyr::filter(! is.na(regime))
    
    regimes <- dplyr::group_by(
      returns, `asset pool`, `update frequency`, `return frequency`, 
      `ranking period`, `long threshold`, `short threshold`, name, leg, 
      period, year, regime
    ) %>%
      dplyr::summarise(
        min = min(return, na.rm = T), 
        max = max(return, na.rm = T),
        mean = list(tryCatch(
          { t.test(return, alternative = "two.sided", mu = 0, na.rm = T) }, 
          error = function(e) { NA }
        )
        ),
        sd = sd(return, na.rm = T)
      ) %>% 
      dplyr::ungroup()
    
    data.table::rbindlist(list(whole, regimes), use.names = T)
    
  }) %>% 
  dplyr::bind_rows()

#### by subperiod ####
subperiods <- lapply(
  1L:nrow(combinations), 
  function(y){
    
    lapply(
      unique(periods$period), 
      function(x){
        
        start <- dplyr::filter(periods, period == x, bound == "start")$date
        end <- dplyr::filter(periods, period == x, bound == "end")$date
        
        
        `asset pool` <- combinations$`asset pool`[y]
        `update frequency` <- combinations$`update frequency`[y]
        `return frequency` <- combinations$`return frequency`[y]
        `ranking period` <- combinations$`ranking period`[y]
        `long threshold` <- combinations$`long threshold`[y]
        `short threshold` <- combinations$`short threshold`[y]
        name <- combinations$name[y]
        leg <- combinations$leg[y]
        
        returns <- dplyr::filter(
          returns, `asset pool` == !! `asset pool`, 
          `update frequency` == !! `update frequency`, 
          `return frequency` == !! `return frequency`,
          `ranking period` == !! `ranking period`, 
          `long threshold` == !! `long threshold`,
          `short threshold` == !! `short threshold`,
          name == !! name, leg == !! leg, 
          date >= as.Date(start), date <= as.Date(date)
        ) %>%
          dplyr::mutate(period = x, year = NA)
        
        whole <- dplyr::group_by(
          returns, `asset pool`, `update frequency`, `return frequency`, 
          `ranking period`, `long threshold`, `short threshold`, name, leg, 
          period, year
        ) %>%
          dplyr::summarise(
            min = min(return, na.rm = T),
            max = max(return, na.rm = T), 
            mean = list(tryCatch(
              { t.test(return, alternative = "two.sided", mu = 0, na.rm = T) }, 
              error = function(e) { NA }
            )
            ), sd = sd(return, na.rm = T)
          ) %>% 
          dplyr::ungroup() %>% 
          dplyr::mutate(regime = "all") %>%
          dplyr::select(
            `asset pool`, `update frequency`, `return frequency`, `ranking period`,
            `long threshold`, `short threshold`, name, leg, period, year, regime,
            min, max, mean, sd
          )
        
        
        returns <- dplyr::left_join(
          returns, 
          dplyr::filter(`aggregate CHP regimes`, ! is.na(period)) %>% 
            dplyr::select(date, regime), 
          by = "date") %>% 
          dplyr::filter(! is.na(regime))
        
        regimes <- dplyr::group_by(
          returns, `asset pool`, `update frequency`, `return frequency`, 
          `ranking period`, `long threshold`, `short threshold`, name, leg, period, 
          year, regime
        ) %>%
          dplyr::summarise(
            min = min(return, na.rm = T), 
            max = max(return, na.rm = T),
            mean = list(tryCatch(
              { t.test(return, alternative = "two.sided", mu = 0, na.rm = T) }, 
              error = function(e) { NA }
            )
            ),
            sd = sd(return, na.rm = T)
          ) %>% 
          dplyr::ungroup()
        
        data.table::rbindlist(list(whole, regimes), use.names = T)
        
      }) %>% 
      data.table::rbindlist(use.names = T)
    
  }) %>% 
  dplyr::bind_rows()

US_commodities <- data.table::rbindlist(list(years, subperiods))




## construct factors - asset pool: factor picks ####

factors <- readr::read_rds(
  here::here("explore", "results", "factors-from-picks.rds")
)

## factor descriptive statistics ####
returns <- dplyr::mutate(
  factors,
  returns = purrr::map(`factor data`, function(x){
    if (class(x) == "MarketFactor")
      dplyr::mutate(x@returns, long = NA_real_, short = NA_real_) %>%
      dplyr::select(date, long, short, factor)
    else x@returns
  })
) %>% 
  dplyr::select(-`factor data`) %>%
  tidyr::unnest(returns) %>% 
  dplyr::mutate(year = NA) %>%
  dplyr::relocate(year, .after = period) %>%
  tidyr::pivot_longer(
    long:factor, names_to = "leg", values_to = "return"
  ) %>% dplyr::filter(!is.na(return)) %>%
  dplyr::mutate(year = lubridate::year(date))


#### by year ####

##### whole ####
whole <- dplyr::group_by(
  returns,
  `picking factor asset pool`, `picking factor leg`, 
  `picking factor name`, `asset pool`, `update frequency`, 
  `return frequency`, `ranking period`, `long threshold`,
  `short threshold`, `year`, `factor name`,
  `leg`
) %>% dplyr::summarise(
  min = min(return, na.rm = T),
  max = max(return, na.rm = T),
  mean = list(tryCatch(
    { t.test(return, alternative = "two.sided", mu = 0, na.rm = T) },
    error = function(e) { NA }
  )
  ), sd = sd(return, na.rm = T)
) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(regime = "all") %>%
  dplyr::select(
    `picking factor asset pool`, `picking factor leg`, 
    `picking factor name`, `asset pool`, `update frequency`, 
    `return frequency`, `ranking period`, `long threshold`,
    `short threshold`, name = `factor name`, leg,  
    `year`, regime, min, max, mean, sd
  )

##### regimes ####
returns_regimes <- dplyr::left_join(
  returns, dplyr::filter(`aggregate CHP regimes`, is.na(period)) %>% 
    dplyr::select(date, regime), 
  by = "date"
) %>% 
  dplyr::filter(! is.na(regime))

regimes <- dplyr::group_by(
  returns_regimes, 
  `picking factor asset pool`, `picking factor leg`, 
  `picking factor name`, `asset pool`, `update frequency`, 
  `return frequency`, `ranking period`, `long threshold`,
  `short threshold`, name = `factor name`, leg, 
  `year`, regime
) %>%
  dplyr::summarise(
    min = min(return, na.rm = T), 
    max = max(return, na.rm = T),
    mean = list(tryCatch(
      { t.test(return, alternative = "two.sided", mu = 0, na.rm = T) }, 
      error = function(e) { NA }
    )
    ),
    sd = sd(return, na.rm = T)
  ) %>% 
  dplyr::ungroup()

years <- data.table::rbindlist(
  list(whole, regimes), use.names = T
) %>% 
  dplyr::mutate( period = NA) %>%
  dplyr::relocate(period, .before = year)


#### by subperiod ####
##### whole ####
whole <- dplyr::group_by(
  returns,
  `picking factor asset pool`, `picking factor leg`, 
  `picking factor name`, `asset pool`, `update frequency`, 
  `return frequency`, `ranking period`, `long threshold`,
  `short threshold`, period, `factor name`,
  `leg`
) %>% dplyr::summarise(
  min = min(return, na.rm = T),
  max = max(return, na.rm = T),
  mean = list(tryCatch(
    { t.test(return, alternative = "two.sided", mu = 0, na.rm = T) },
    error = function(e) { NA }
  )
  ), sd = sd(return, na.rm = T)
) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(regime = "all") %>%
  dplyr::select(
    `picking factor asset pool`, `picking factor leg`, 
    `picking factor name`, `asset pool`, `update frequency`, 
    `return frequency`, `ranking period`, `long threshold`,
    `short threshold`, name = `factor name`, leg,  
    period, regime, min, max, mean, sd
  )

##### regimes ####
returns_regimes <- dplyr::left_join(
  returns, dplyr::filter(`aggregate CHP regimes`, is.na(period)) %>% 
    dplyr::select(date, regime), 
  by = "date"
) %>% 
  dplyr::filter(! is.na(regime))

regimes <- dplyr::group_by(
  returns_regimes, 
  `picking factor asset pool`, `picking factor leg`, 
  `picking factor name`, `asset pool`, `update frequency`, 
  `return frequency`, `ranking period`, `long threshold`,
  `short threshold`, name = `factor name`, leg, 
  period, regime
) %>%
  dplyr::summarise(
    min = min(return, na.rm = T), 
    max = max(return, na.rm = T),
    mean = list(tryCatch(
      { t.test(return, alternative = "two.sided", mu = 0, na.rm = T) }, 
      error = function(e) { NA }
    )
    ),
    sd = sd(return, na.rm = T)
  ) %>% 
  dplyr::ungroup()

subperiods <- data.table::rbindlist(
  list(whole, regimes), use.names = T
) %>% 
  dplyr::mutate(year = NA) %>%
  dplyr::relocate(year, .after = period)

factors_from_picks <- data.table::rbindlist(list(years, subperiods))


factors <- tibble::tibble(
  `asset pool` = c("US commodities", "factor picks"),
  results = list(US_commodities, factors_from_picks)
)



statistics <- tibble::tibble(
  analysis = c("commodity futures", "factors"), 
  results = list(`commodity futures`, factors)
)

parallel::stopCluster(cluster)

readr::write_rds(
  statistics,
  here::here("explore", "results", "descriptive-statistics.rds")
)








