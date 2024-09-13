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

`factor picks` <- readRDS(file = here::here("explore", "results", "factor-picks.rds"))


update_frequency = "week"; return_frequency <- "day"
ranking_period <- 26L
long_threshold <- 2/3; short_threshold <- 1/3
weighted = TRUE

combinations <- dplyr::distinct(
  `factor picks`, `asset pool`, `update frequency`, `return frequency`, 
  `ranking period`, `long threshold`, `short threshold`, factor, leg, period
) %>% as.data.frame()

`factors from picks` <- foreach(y = 1L:nrow(combinations), .combine = dplyr::bind_rows) %dopar% {
  library(magrittr); library(lubridate)
  message(paste(y, nrow(combinations), sep = "/"))
  
  picks <- dplyr::filter(
    `factor picks`, `asset pool` == combinations[y, "asset pool"], 
    `update frequency` == combinations[y, "update frequency"], 
    `return frequency` == combinations[y, "return frequency"],
    `ranking period` == combinations[y, "ranking period"], 
    `long threshold` == combinations[y, "long threshold"], 
    `short threshold` == combinations[y, "short threshold"],
    factor == combinations[y, "factor"], leg == combinations[y, "leg"], 
    period == combinations[y, "period"]) %>% dplyr::select(pick) %>% 
    purrr::flatten_chr()
  
  start <- dplyr::filter(periods, period == combinations[y, "period"], bound == "start") %>% 
    dplyr::select(date) %>% purrr::flatten_chr()
  `start backward` <- RQuantLib::advance(
    dates = as.Date(start), calendar = "UnitedStates/NYSE", n = -(ranking_period + 2L),
    timeUnit = dplyr::case_when(update_frequency == "day" ~ 0L, update_frequency == "week" ~ 1L,
                                update_frequency == "month" ~ 2L, update_frequency == "year" ~ 3L))
  `start backward` <- as.character(`start backward`)
  end <- dplyr::filter(periods, period == combinations[y, "period"], bound == "end") %>% 
    dplyr::select(date) %>% purrr::flatten_chr()
  
  `commodity futures data` <- pullit::pull_futures_market(
    source = "storethat", type = "term structure", active_contract_tickers = picks, 
    start = `start backward`, end = end, TS_positions = 1L:2L,
    roll_type = "A", roll_days = 0L, roll_months = 0L, roll_adjustment = "N", file = storethat
  )
  `commodity aggregate data` <- pullit::pull_futures_market(
    source = "storethat", type = "aggregate", active_contract_tickers = picks, 
    start = `start backward`, end = end, file = storethat
  )
  `commodity CFTC data` <- pullit::pull_futures_CFTC(
    source = "storethat", active_contract_tickers = picks, start = `start backward`, end = end, 
    file = storethat
  )
  
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
    return_frequency = return_frequency, front = 1L, back = 2L, ranking_period = ranking_period, 
    long_threshold = long_threshold, short_threshold = short_threshold, weighted = weighted
  )
  tibble::tibble(`factor name` = c("market", "CHP", "open interest nearby", "open interest aggregate", "term structure"), 
                 `factor data` = list(market, CHP, `open interest nearby`, `open interest aggregate`, `term structure`)) %>% 
    dplyr::mutate(`picking factor asset pool` = combinations[y, "asset pool"], `picking factor leg` = combinations[y, "leg"], `picking factor name` = combinations[y, "factor"], 
                  `asset pool` = "factor picks", `update frequency` = combinations[y, "update frequency"], `return frequency` = combinations[y, "return frequency"], 
                  `ranking period` = combinations[y, "ranking period"], `long threshold` = combinations[y, "long threshold"], `short threshold` = combinations[y, "short threshold"], 
                  period = combinations[y, "period"]) %>%
    dplyr::select(`picking factor asset pool`, `picking factor leg`, `picking factor name`, `asset pool`, `update frequency`, `return frequency`, `ranking period`, `long threshold`, 
                  `short threshold`, period, `factor name`, `factor data`)
}


parallel::stopCluster(cluster)
saveRDS(`factors from picks`, file = "explore/results/factors-from-picks.rds")








