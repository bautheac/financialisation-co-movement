library(finRes)
library(magrittr)
library(doParallel)
# source("explore/scripts/functions - shared.r")

# start cluster ####
cluster <- makeCluster(detectCores() - 1L);
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
  date = c("1997-07-01", "2003-12-31", "2004-01-01", "2008-09-14", 
           "2008-09-15", "2013-06-19", "2013-06-20", "2018-12-31")
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
  "SBA Comdty", "SIA Comdty", "SMA Comdty", "W A Comdty", "XBWA Comdty"
)

`commodity futures data` <- pullit::pull_futures_market(
  source = "storethat", type = "term structure", 
  active_contract_tickers = `commodity futures tickers`,
  start = start, end = end, 
  TS_positions = 1L:2L, roll_type = "A", roll_days = 0L, roll_months = 0L,
  roll_adjustment = "N", file = storethat
)

`commodity futures data` <- get_data(`commodity futures data`) %>%
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

# aggregate CHP ####
`aggregate CHP` <- dplyr::left_join(
  `commodity CFTC data`@data, 
  dplyr::select(
    tickers_cftc, MIC, format, underlying, unit, participant, position, ticker),
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
      `aggregate CHP` < median(`aggregate CHP`), "backwardation", "contango"
    )
  ) %>% dplyr::ungroup() %>%
  dplyr::mutate(period = NA) %>% 
  dplyr::select(period, year, week, regime)

years <- dplyr::mutate(
  dplyr::distinct(`commodity futures data`, date), 
  year = lubridate::year(date), week = lubridate::week(date)
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
        ), period = x, year = lubridate::year(date), week = lubridate::week(date)
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
`commodity futures tickers` <- dplyr::left_join(
  dplyr::select(tickers_futures, ticker, MIC), 
  dplyr::select(exchanges, MIC, country), 
  by = "MIC"
) %>% 
  dplyr::filter(ticker %in% `commodity futures tickers`, country == "US") %>% 
  dplyr::select(ticker) %>% 
  purrr::flatten_chr()

`commodity futures data` <- pullit::pull_futures_market(
  source = "storethat", type = "term structure", 
  active_contract_tickers = `commodity futures tickers`,
  start = start, end = end, TS_positions = 1L:2L, roll_type = "A", 
  roll_days = 0L, roll_months = 0L, roll_adjustment = "N", file = storethat
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

# construct factors - asset pool: US commodities ####
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
  price_data = `commodity futures data`, aggregate_data = `commodity aggregate data`, 
  update_frequency = update_frequency, return_frequency = return_frequency, 
  ranking_period = ranking_period, long_threshold = long_threshold,
  short_threshold = short_threshold, weighted = weighted
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

`factor returns` <- foreach(
  y = 1L:nrow(factors), 
  .combine = dplyr::bind_rows
) %dopar% { 
  library(data.table)
  
  factors$factor[[y]]@returns[
    , `asset pool` := factors$`asset pool`[y]
    ][, name := factors$name[y]] 
  
} %>% 
  tidyr::gather(leg, return, -c(date, name, `asset pool`)) %>% 
  dplyr::filter(! is.na(return)) %>% 
  dplyr::mutate(
    `update frequency` = update_frequency, `return frequency` = return_frequency, 
    `ranking period` = ranking_period,
    `long threshold` = long_threshold, `short threshold` = short_threshold
  ) %>%
  dplyr::select(
    `asset pool`, `update frequency`, `return frequency`, `ranking period`, 
    `long threshold`, `short threshold`, name, leg, date, return
  )

`futures prices` <- dplyr::left_join(
  `commodity futures data`@data, 
  dplyr::select(
    `commodity futures data`@term_structure_tickers, 
    `active contract ticker`, ticker, `TS position`
  ),
  by = "ticker"
) %>% 
  dplyr::filter(`TS position` == 1L, field == "PX_LAST") %>% 
  dplyr::select(`active contract ticker`, date, value)


# factor picks ####
`selection threshold` <- 1L/3L
`factor returns` <- dplyr::filter(`factor returns`, leg == "factor")

combinations <- expand.grid(
  unique(`futures prices`$`active contract ticker`), 
  unique(periods$period), stringsAsFactors = F
) %>% 
  setNames(c("commodity", "period"))

combinations <- lapply(
  1L:nrow(combinations), 
  function(y){
    dplyr::distinct(
      `factor returns`, `asset pool`, `update frequency`, `return frequency`, 
      `ranking period`, `long threshold`, `short threshold`, name, leg
    ) %>%
      dplyr::mutate(
        commodity = combinations[y, "commodity"], 
        period = combinations[y, "period"]
      )
  }) %>% dplyr::bind_rows()

## R-squares ####
`r-squared` <- foreach(
  y = 1L:nrow(combinations), 
  .combine = rbind
) %dopar% {
  library(magrittr); library(lubridate)
  
  `asset pool` <- combinations[y, "asset pool"]
  `update frequency` <- combinations[y, "update frequency"]
  `return frequency` <- combinations[y, "return frequency"]
  `ranking period` <- combinations[y, "ranking period"]
  `long threshold` <- combinations[y, "long threshold"]
  `short threshold` <- combinations[y, "short threshold"]
  `factor name` <- combinations[y, "name"]
  leg <- combinations[y, "leg"]
  `commodity name` <- combinations[y, "commodity"]
  period <- combinations[y, "period"]
  
  start <- dplyr::filter(periods, period == !! period, bound == "start") %>% 
    dplyr::select(date) %>% purrr::flatten_chr()
  end <- dplyr::filter(periods, period == !! period, bound == "end") %>% 
    dplyr::select(date) %>% purrr::flatten_chr()
  
  futures <- dplyr::filter(
    `futures prices`, 
    date >= as.Date(start), date <= as.Date(end), 
    `active contract ticker` == !! `commodity name`
  ) %>% 
    dplyr::select(date, commodity = value)
  
  futures <- if (`return frequency` == "day") {
    dplyr::mutate(
      futures, commodity = (commodity / dplyr::lag(commodity, 1L)) - 1L
    )
  } else {
    dplyr::mutate(
      futures, year = lubridate::year(date), 
      unit = do.call(what = `return frequency`, args = list(date))
    ) %>% 
      dplyr::group_by(year, unit) %>% 
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
      dplyr::select(-c(year, unit)) %>% 
      dplyr::mutate(
        commodity = (commodity / dplyr::lag(commodity, 1L)) - 1L
      )
  }
  
  factor <- dplyr::filter(
    `factor returns`, `asset pool` == !! `asset pool`, 
    `update frequency` == !! `update frequency`, 
    `return frequency` == !! `return frequency`,
    `ranking period` == !! `ranking period`, 
    `long threshold` == !! `long threshold`, 
    `short threshold` == !! `short threshold`,
    name == !! `factor name`, leg == !! leg, 
    date >= as.Date(start), date <= as.Date(end)
  ) %>%
    dplyr::select(date, factor = return)
  browser()
  data <- dplyr::full_join(futures, factor, by = "date")
  model <- tryCatch(
    { lm(commodity ~ factor, data = data) }, 
    error = function(e) { NA }
  )
  
  tibble::tibble(
    `asset pool` = !! `asset pool`, `update frequency` = !! `update frequency`, 
    `return frequency` = !! `return frequency`, 
    `ranking period` = !! `ranking period`, `long threshold` = !! `long threshold`, 
    `short threshold` = !! `short threshold`, factor = !! `factor name`, 
    leg = !! leg, commodity = !! `commodity name`, period = !! period, 
    model = list(model)
  )
} %>% 
  dplyr::mutate(period = forcats::as_factor(period))

## factor picks ####
`factor picks` <- dplyr::mutate(
  `r-squared`, 
  `r-squared` = purrr::map(model, function(x) stats::summary.lm(x)$r.squared)
) %>%
  dplyr::select(-model) %>% 
  tidyr::unnest(`r-squared`) %>% 
  dplyr::group_by(
    `asset pool`, `update frequency`, `return frequency`, `ranking period`, 
    `long threshold`, `short threshold`, factor, leg, period
  ) %>%
  dplyr::do(
    dplyr::select(., commodity, `r-squared`) %>% 
      dplyr::arrange(dplyr::desc(`r-squared`)) %>% 
      dplyr::slice(1L:ceiling(`selection threshold` * dplyr::n())) %>%
      dplyr::select(pick = commodity, `r-squared`)
  ) %>%
  dplyr::ungroup()


# proportion of factor picks in factors ####
`factor positions` <- foreach(
  y = 1L:nrow(factors), 
  .combine = dplyr::bind_rows
) %dopar% { 
  library(data.table)
  
  factors$factor[[y]]@positions[
    , `asset pool` := factors$`asset pool`[y]
    ][, factor := factors$name[y]] 
  
} %>% dplyr::mutate(
  `update frequency` = update_frequency, `return frequency` = return_frequency, 
  `ranking period` = ranking_period, `long threshold` = long_threshold, 
  `short threshold` = short_threshold
) %>%
  dplyr::select(
    `asset pool`, `update frequency`, `return frequency`, `ranking period`, 
    `long threshold`, `short threshold`, factor, date, name, position
  ) %>%
  dplyr::arrange(
    `asset pool`, `update frequency`, `return frequency`, `ranking period`, 
    `long threshold`, `short threshold`, factor, date, position
  )

`factor positions` <- lapply(
  unique(periods$period), 
  function(x){
    
    start <- dplyr::filter(periods, period == x, bound == "start") %>%
      dplyr::select(date) %>% purrr::flatten_chr()
    end <- dplyr::filter(periods, period == x, bound == "end") %>% 
      dplyr::select(date) %>% purrr::flatten_chr()
    
    dplyr::filter(`factor positions`, date >= as.Date(start), date <= as.Date(end)) %>% 
      dplyr::mutate(period = x) %>%
      dplyr::select(
        `asset pool`, `update frequency`, `return frequency`, `ranking period`, 
        `long threshold`, `short threshold`, factor, period,  date, name, position
      )
  }) %>% 
  dplyr::bind_rows()

combinations <- expand.grid(
  unique(`factor picks`$factor), 
  unique(periods$period), 
  stringsAsFactors = F
) %>% 
  setNames(c("factor", "period"))

combinations <- lapply(
  unique(periods$period), 
  function(y){
    dplyr::distinct(
      `factor returns`, `asset pool`, `update frequency`, `return frequency`, 
      `ranking period`, `long threshold`, `short threshold`, factor = name, leg
    ) %>% 
      dplyr::mutate(period = y)
  }) %>% 
  dplyr::bind_rows()


proportions <- lapply(1L:nrow(combinations), function(y){
  
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
    dplyr::select(pick) %>% 
    purrr::flatten_chr()
  
  positions <- dplyr::filter(
    `factor positions`, 
    `asset pool` == combinations[y, "asset pool"],
    `update frequency` == combinations[y, "update frequency"], 
    `return frequency` == combinations[y, "return frequency"],
    `ranking period` == combinations[y, "ranking period"], 
    `long threshold` == combinations[y, "long threshold"], 
    `short threshold` == combinations[y, "short threshold"],
    factor == combinations[y, "factor"], 
    period == combinations[y, "period"]
  ) %>% 
    dplyr::select(date, name, position)
  
  
  proportions <- lapply(
    c("long", "short"), 
    function(x){ 
      # browser()
      
      occurences <- dplyr::filter(positions, position == x) %>% 
        dplyr::filter(name %in% picks) %>% dplyr::group_by(name) %>% 
        dplyr::tally()
      total <- dplyr::filter(positions, position == x) %>% 
        dplyr::filter(name %in% picks) %>% na.omit() %>% 
        dplyr::distinct(date) %>% nrow()
      
      proportions <- if (combinations[y, "factor"] == "market"){
        dplyr::mutate(occurences, proportion = 1L
        ) %>%
          dplyr::mutate(leg = x) %>% 
          dplyr::select(pick = name, leg, proportion)
      } else {
        dplyr::mutate(occurences, proportion = n / total) %>%
          dplyr::mutate(leg = x) %>% 
          dplyr::select(pick = name, leg, proportion)
      }
      
      
      if (nrow(proportions) == 0L) 
        tibble::tibble(pick = picks) %>% 
        dplyr::mutate(leg = x, proportion = 0L) 
      else proportions
      
    }) %>% 
    dplyr::bind_rows()
  
  proportions <- tidyr::spread(proportions, leg, proportion)
  proportions[is.na(proportions)] <- 0L
  
  dplyr::mutate(
    proportions, `asset pool` = combinations[y, "asset pool"], 
    `update frequency` = combinations[y, "update frequency"], 
    `return frequency` = combinations[y, "return frequency"],
    `ranking period` = combinations[y, "ranking period"], 
    `long threshold` = combinations[y, "long threshold"], 
    `short threshold` = combinations[y, "short threshold"],
    factor = combinations[y, "factor"], period = combinations[y, "period"]
  ) %>%
    dplyr::select(
      `asset pool`, `update frequency`, `return frequency`, `ranking period`,
      `long threshold`, `short threshold`, `short threshold`, factor, period, 
      pick, long, short
    )
}) %>% dplyr::bind_rows()



parallel::stopCluster(cluster)
saveRDS(`factor picks`, file = here::here("explore", "results", "factor-picks.rds"))
saveRDS(proportions, file = here::here("explore", "results", "proportions.rds"))
