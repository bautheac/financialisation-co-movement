# Globals ######################################################################
## datasets ####
data("tickers_futures", "tickers_cftc", package = "BBGsymbols")
data("exchanges", package = "fewISOs")

## variables ####
factors <- 
  c("market", "CHP", "open interest nearby", "open interest aggregate", "term structure")
period_levels <- c("past", "financialisation", "crisis", "post-crisis")
results_directory_path <- here::here("explore", "results", "revision-jfm")
sector_levels <- c("all", "agriculturals", "energy", "metals")
sort_levels <- c(
  "all-all-all", "US-all-all", "US-agriculturals-all", "US-agriculturals-grains",
  "US-agriculturals-livestock", "US-agriculturals-softs", "US-energy-all", 
  "US-energy-gas", "US-energy-petroleum", "US-metals-all", "US-metals-base", 
  "US-metals-precious", "GB-all-all"
)
portfolio_levels <- c("countries", "sectors", "subsectors")
subsector_levels <- 
  c("all", "grains", "livestock", "softs", "gas", "petroleum", "base", "precious")
regime_levels <- c("whole period", "backwardation", "contango")
tables_directory_path <- here::here("explore", "tables", "revision-jfm")
commodity_futures_tickers <- c(
  "BOA Comdty", "C A Comdty", "CCA Comdty", "CLA Comdty", "CTA Comdty", 
  "FCA Comdty", "GCA Comdty", "HGA Comdty", "HOA Comdty", "JOA Comdty", 
  "KCA Comdty", "LAA Comdty", "LBA Comdty", "LCA Comdty", "LHA Comdty", 
  "LLA Comdty", "LNA Comdty", "LPA Comdty", "LTA Comdty", "LXA Comdty", 
  "NGA Comdty", "O A Comdty", "PAA Comdty", "PLA Comdty", "S A Comdty", 
  "SBA Comdty", "SIA Comdty", "SMA Comdty", "W A Comdty", "XBWA Comdty",
  "LAA Comdty", "LPA Comdty", "LLA Comdty", "LNA Comdty", "LTA Comdty",
  "LXA Comdty"
)

## functions ####
make_asset_name_country_sector_subsector_dataframe <- function(tickers){
  
  dplyr::left_join(
    dplyr::filter(tickers_futures, ticker %in% tickers) |> 
      dplyr::select(ticker, name, sector, subsector, MIC), 
    dplyr::select(exchanges, MIC, country), 
    by = "MIC"
  ) |> dplyr::select(-ticker)
}
significance <- function(p.value, estimate){
  if(p.value <= 0.01) paste0("***", estimate)
  else if (p.value > 0.01 && p.value <= 0.05) paste0("**", estimate)
  else if (p.value > 0.05 && p.value <= 0.10) paste0("*", estimate)
  else estimate
}
sort_table_by_country_sector_subsector <- function(tb, sort_levels){
  dplyr::mutate(
    tb,
    sort = paste(country, sector, subsector, sep = "-"),
    sort = factor(sort, levels = sort_levels)
  ) |> dplyr::arrange(sort) |> dplyr::select(-sort)
}

long_to_short_asset_name_map <- tibble::tribble(
  ~long,                                     ~short,
  "Orange juice-frozen concentrated (IFUS)", "Orange juice (IFUS)",
  "Lumber-random length (XCME)",             "Lumber (XCME)",
  "Aluminium-primary (XLME)",                "Aluminium (XLME)",
  "Lead-refined pig (XLME)",                 "Lead (XLME)",
  "Nickel-primary (XLME)",                   "Nickel (XLME)",
  "Tin-refined (XLME)",                      "Tin (XLME)"
)

# assets taxonomy ##############################################################
`assets taxonomy` <- 
  make_asset_name_country_sector_subsector_dataframe(commodity_futures_tickers) |>
  dplyr::mutate(
    asset = paste0(name, " (", MIC, ")"),
    country = factor(country, levels = c("US", "GB"))
    ) |> dplyr::select(-c(name, MIC)) |>
  dplyr::relocate(asset, .before = dplyr::everything()) |>
  dplyr::relocate(country, .after = asset) |>
  dplyr::arrange(country, sector, subsector) 

readr::write_rds(`assets taxonomy`, slituR::paste_forward_slash(tables_directory_path, "assets-taxonomy.rds"))

# descriptive stats ############################################################
## individual assets ###########################################################
stats <- readr::read_rds(
  slituR::paste_forward_slash(results_directory_path, "descriptive-statistics-clean.rds")
  )

commodities_market_individuals <- dplyr::filter(stats, analysis == "commodity futures") |>
  dplyr::select(results) |> tidyr::unnest(results) |>
  dplyr::filter(analysis == "market variables") |>
  dplyr::select(results) |> tidyr::unnest(results) |>
  dplyr::filter(assets == "individual commodities") |>
  dplyr::select(results) |> tidyr::unnest(results) |>
  dplyr::filter(
    type == "returns", period != "year", frequency == "daily", 
    field == "close price"
  ) |> dplyr::select(-c(ticker, type, frequency, field, year)) |>
  dplyr::mutate_if(.predicate = is.list, .funs = unlist)

commodities_market_countries <- dplyr::filter(stats, analysis == "commodity futures") |>
  dplyr::select(results) |> tidyr::unnest(results) |>
  dplyr::filter(analysis == "market variables") |>
  dplyr::select(results) |> tidyr::unnest(results) |>
  dplyr::filter(assets == "countries - sectors - subsectors") |>
  dplyr::select(results) |> tidyr::unnest(results) |>
  dplyr::filter(
    sector == "all", subsector == "all", field == "close price",
    type == "returns", period != "year", frequency == "daily"
  ) |> dplyr::select(-c(type, frequency, field, year)) |>
  dplyr::mutate_if(.predicate = is.list, .funs = unlist)

### whole ####
commodities <- dplyr::filter(commodities_market_individuals, regime == "all") |>
  dplyr::select(-c(regime, min, max)) |> 
  dplyr::mutate(
    mean = slituR::percentize(mean), sd = slituR::percentize(sd),
    mean = purrr::map2_chr(p.value, mean, ~ significance(.x, .y))
  ) |> dplyr::select(-p.value) |>
  tidyr::pivot_longer(
    cols = c("mean", "sd"), names_to = "estimate", values_to = "value"
  ) |> tidyr::pivot_wider(names_from = "period", values_from = "value") |>
  dplyr::arrange(dplyr::desc(country), sector, subsector, commodity) |>
  dplyr::select(-c(country, sector, subsector)) |>
  dplyr::rename(asset = commodity)

countries <- dplyr::filter(commodities_market_countries, regime == "all") |>
  dplyr::select(-c(regime, min, max, sector, subsector)) |> 
  dplyr::mutate(asset = paste(country, "commodities", sep = " ")) |>
  dplyr::select(-country) |> 
  dplyr::mutate(
    mean = slituR::percentize(mean), sd = slituR::percentize(sd),
    mean = purrr::map2_chr(p.value, mean, ~ significance(.x, .y))
  ) |> dplyr::select(-p.value) |>
  tidyr::pivot_longer(
    cols = c("mean", "sd"), names_to = "estimate", values_to = "value"
  ) |> tidyr::pivot_wider(names_from = "period", values_from = "value")

descriptive_stats_individuals_whole <- dplyr::bind_rows(commodities, countries)

### regimes ####
commodities <- dplyr::filter(commodities_market_individuals, regime != "all") |>
  dplyr::select(-c(min, max)) |> 
  dplyr::mutate(
    mean = slituR::percentize(mean), sd = slituR::percentize(sd),
    mean = purrr::map2_chr(p.value, mean, ~ significance(.x, .y))
  ) |> dplyr::select(-p.value) |>
  tidyr::pivot_longer(
    cols = c("mean", "sd"), names_to = "estimate", values_to = "value"
  ) |> tidyr::pivot_wider(names_from = "period", values_from = "value") |>
  dplyr::arrange(dplyr::desc(country), sector, subsector, commodity) |>
  dplyr::select(-c(country, sector, subsector)) |>
  dplyr::rename(asset = commodity)

countries <- dplyr::filter(commodities_market_countries, regime != "all") |>
  dplyr::select(-c(min, max, sector, subsector)) |> 
  dplyr::mutate(asset = paste(country, "commodities", sep = " ")) |>
  dplyr::select(-country) |> 
  dplyr::mutate(
    mean = slituR::percentize(mean), sd = slituR::percentize(sd),
    mean = purrr::map2_chr(p.value, mean, ~ significance(.x, .y))
  ) |> dplyr::select(-p.value) |>
  tidyr::pivot_longer(
    cols = c("mean", "sd"), names_to = "estimate", values_to = "value"
  ) |> tidyr::pivot_wider(names_from = "period", values_from = "value")

descriptive_stats_individuals_regimes <- dplyr::bind_rows(commodities, countries)

### combined ####
commodities <- dplyr::select(commodities_market_individuals, -c(min, max)) |> 
  dplyr::mutate(
    mean = slituR::percentize(mean), sd = slituR::percentize(sd),
    mean = purrr::map2_chr(p.value, mean, ~ significance(.x, .y))
  ) |> dplyr::select(-p.value) |>
  tidyr::pivot_longer(
    cols = c("mean", "sd"), names_to = "estimate", values_to = "value"
  ) |> tidyr::pivot_wider(names_from = "period", values_from = "value") |>
  dplyr::arrange(dplyr::desc(country), sector, subsector, commodity, regime) |>
  dplyr::select(-c(country, sector, subsector)) |>
  dplyr::rename(asset = commodity)

countries <- dplyr::select(commodities_market_countries, -c(min, max, sector, subsector)) |> 
  dplyr::mutate(asset = paste(country, "commodities", sep = " ")) |>
  dplyr::select(-country) |> 
  dplyr::mutate(
    mean = slituR::percentize(mean), sd = slituR::percentize(sd),
    mean = purrr::map2_chr(p.value, mean, ~ significance(.x, .y))
  ) |> dplyr::select(-p.value) |>
  tidyr::pivot_longer(
    cols = c("mean", "sd"), names_to = "estimate", values_to = "value"
  ) |> tidyr::pivot_wider(names_from = "period", values_from = "value")

# q: this only works in the html output, μ and σ appear as blank in the pdf output; how to fix this?

descriptive_stats_individuals_combined <- dplyr::bind_rows(commodities, countries) |>
  dplyr::mutate(
    regime = ifelse(regime == "all", "whole period", regime),
    estimate = ifelse(estimate == "mean", estimate, "volatility")
    ) |>
  dplyr::left_join(long_to_short_asset_name_map, by = c("asset" = "long")) |>
  dplyr::mutate(asset = dplyr::coalesce(short, asset)) |>
  dplyr::select(-short)

readr::write_rds(
  descriptive_stats_individuals_combined, 
  slituR::paste_forward_slash(tables_directory_path, "stats-individual-assets.rds")
)

## equally weighted portfolio ##################################################
stats <- readr::read_rds(
  slituR::paste_forward_slash(results_directory_path, "descriptive-stats-ew-portfolios.rds")
)

descriptive_stats_ew_portfolios <- dplyr::filter(stats, timespan == "period") |> 
  dplyr::select(-timespan) |>
  dplyr::mutate(
    period = ifelse(period == "financialization", "financialisation", period),
    period = ifelse(period == "present", "post-crisis", period),
    period = factor(period, levels = period_levels),
    regime = ifelse(regime == "all", "whole period", regime),
    significance = slituR::significance(p_value),
    dplyr::across(c(mean, volatility), slituR::percentize),
    mean = paste0(significance, mean)
  ) |> 
  dplyr::select(-c(p_value, significance)) |>
  dplyr::arrange(period) |>
  sort_table_by_country_sector_subsector(sort_levels) |>
  tidyr::pivot_longer(cols = c("mean", "volatility"), names_to = "estimate", values_to = "value") |> 
  tidyr::pivot_wider(names_from = "period", values_from = "value")

readr::write_rds(
  descriptive_stats_ew_portfolios, 
  slituR::paste_forward_slash(tables_directory_path, "stats-ew-portfolios.rds")
)


# regime difference tests ######################################################
regime_difference_tests <- readr::read_rds(
  slituR::paste_forward_slash(results_directory_path, "regime-difference-tests.rds")
)

ticker_asset_map <- dplyr::left_join(
    dplyr::filter(tickers_futures, ticker %in% commodity_futures_tickers) |> 
      dplyr::select(ticker, name, sector, subsector, MIC), 
    dplyr::select(exchanges, MIC, country), 
    by = "MIC"
  )
  
regime_difference_tests <- 
  dplyr::left_join(regime_difference_tests, ticker_asset_map, by = c("ticker" = "ticker")) |>
  dplyr::mutate(
    name = ifelse(is.na(name), ticker, name),
    name = ifelse(is.na(MIC), name, paste0(name, " (", MIC, ")")),
    moment = ifelse(moment == "mean", "mean", "volatility"),
    country = factor(country, levels = c("US", "GB")),
    significance = slituR::significance(`p-value`),
    `p-value` = slituR::percentize(as.numeric(`p-value`)),
    sector = factor(sector, levels = sector_levels),
    subsector = factor(subsector, levels = subsector_levels),
    period = ifelse(period == "financialization", "financialisation", period),
    period = ifelse(period == "present", "post-crisis", period),
    period = factor(period, levels = period_levels)
    ) |>
  dplyr::arrange(country, sector, subsector, name, period) |>
  dplyr::select(asset = name, period, moment, `dominant regime`, `p-value`, significance)

individuals <- dplyr::filter(
    regime_difference_tests, !asset %in% c("US commodities", "GB commodities")
  )
countries = dplyr::filter(
  regime_difference_tests, asset %in% c("US commodities", "GB commodities")
  ) |> dplyr::mutate(
    asset = factor(asset, levels = c("US commodities", "GB commodities"))
  ) |> dplyr::arrange(asset, period)

regime_difference_tests <- dplyr::bind_rows(individuals, countries) |>
  dplyr::left_join(long_to_short_asset_name_map, by = c("asset" = "long")) |>
  dplyr::mutate(asset = dplyr::coalesce(short, asset)) |>
  dplyr::select(-short)

readr::write_rds(
  regime_difference_tests, 
  slituR::paste_forward_slash(tables_directory_path, "regime-difference-tests.rds")
)

# individual asset stats & regime difference tests #############################
## original ####################################################################
stats_individuals <- tidyr::pivot_longer(
  descriptive_stats_individuals_combined, c(past, financialization, crisis, present),
  names_to = "period", values_to = "value"
) |> dplyr::filter(regime == "whole period") |> dplyr::select(-regime) |>
  dplyr::mutate(
    period = ifelse(period == "financialization", "financialisation", period),
    period = ifelse(period == "present", "post-crisis", period),
    period = factor(period, levels = period_levels)
  )

regime_tests <- dplyr::mutate(
  regime_difference_tests, 
  summary = paste0("b", ifelse(`dominant regime` == "backwardation", ">", "<"), "c", significance)
) |>
  dplyr::select(asset, period, estimate = moment, `difference test summary` = summary)

individual_asset_stats_regime_difference_tests <- 
  dplyr::left_join(stats_individuals, regime_tests, by = c("asset", "estimate", "period")) |>
  dplyr::mutate(value = paste0(value, " (", `difference test summary`, ")")) |> 
  dplyr::select(asset, estimate, period, value) |>
  tidyr::pivot_wider(names_from = "period", values_from = "value")

readr::write_rds(
  individual_asset_stats_regime_difference_tests, 
  slituR::paste_forward_slash(tables_directory_path, "asset-stats-regime-difference-tests-combined.rds")
)

## revision jfm ################################################################
ew_stats <- readr::read_rds(
  here::here("explore", "shiny", "original", "results", "descriptive-statistics-clean.rds")
)$results[[1L]]$results[[1L]]$results[[2L]] |> dplyr::filter(
  type == "returns", period != "year", frequency == "daily", sector == "all", 
  subsector == "all", field == "close price"
) |> 
  dplyr::select(country, sector, subsector, period, regime, mean, p_value = p.value, volatility = sd) |>
  dplyr::mutate(
    period = ifelse(period == "financialization", "financialisation", period),
    period = ifelse(period == "present", "post-crisis", period),
    period = factor(period, levels = period_levels),
    regime = ifelse(regime == "all", "whole period", regime),
    significance = slituR::significance(p_value),
    dplyr::across(c(mean, volatility), slituR::percentize),
    mean = paste0(significance, mean)
  ) |> 
  dplyr::select(-c(p_value, significance)) |>
  dplyr::arrange(period) |>
  sort_table_by_country_sector_subsector(sort_levels) |>
  tidyr::pivot_longer(cols = c("mean", "volatility"), names_to = "estimate", values_to = "value") |> 
  tidyr::pivot_wider(names_from = "period", values_from = "value") |>
  dplyr::mutate(asset = paste(country, "commodities", sep = " ")) |>
  dplyr::select(-dplyr::contains(c("country", "sector"))) |>
  dplyr::relocate(asset, .before = everything())

ew_regime_tests <- dplyr::filter(regime_difference_tests, grepl("commodities", asset)) |>
  dplyr::select(asset:moment, significance) |> dplyr::rename(estimate = moment) |>
  tidyr::pivot_wider(names_from = "period", values_from = "significance") |>
  dplyr::mutate(regime = "backwardation vs. contango")

ew_asset_stats_regime_difference_tests <- dplyr::bind_rows(ew_stats, ew_regime_tests) |> 
  dplyr::mutate(dplyr::across(asset:estimate, ~ factor(.x, levels = unique(.x)))) |>
  dplyr::relocate(estimate, .before = regime) |>
  dplyr::arrange(asset, estimate, regime)

readr::write_rds(
  ew_asset_stats_regime_difference_tests, 
  slituR::paste_forward_slash(tables_directory_path, "ew-stats-regime-difference-tests-combined.rds")
)




# correlations #################################################################
## inner ####
correlations_inner <- readr::read_rds(
  slituR::paste_forward_slash(results_directory_path, "correlations-inner.rds")
)

### By period ####
correlations_inner_periods <- dplyr::filter(
  correlations_inner, field == "close price", type == "return", frequency == "day", 
  timespan == "period", regime == "whole period"
) |>
  dplyr::select(country, sector, subsector, period, average) |>
  dplyr::group_by(country, sector, subsector, period) |>
  dplyr::slice_tail(n = 1L) |> dplyr::ungroup() |>
  tidyr::pivot_wider(names_from = "period", values_from = "average") |>
  sort_table_by_country_sector_subsector(sort_levels)

readr::write_rds(
  correlations_inner_periods, 
  slituR::paste_forward_slash(tables_directory_path, "correlations-inner-periods.rds")
)

### By year ####
correlations_inner_years <- dplyr::filter(
  correlations_inner, field == "close price", type == "return", frequency == "day", 
  timespan == "year", regime == "whole period"
) |>
  dplyr::select(country, sector, subsector, year, average) |>
  dplyr::group_by(country, sector, subsector, year) |>
  dplyr::slice_tail(n = 1L) |> dplyr::ungroup() |>
  dplyr::mutate(
    decade = slituR::floor_year_to_nearest_decade(year),
    year = slituR::remove_decade_from_year(year)
    ) |>
  sort_table_by_country_sector_subsector(sort_levels) |>
  tidyr::pivot_wider(names_from = "year", values_from = "average") |>
  dplyr::relocate(`0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, .after = dplyr::last_col()) |>
  dplyr::mutate(
    dplyr::across(c(`0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`), ~ format(.x, scientific = FALSE)),
    dplyr::across(c(`0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`), ~ stringr::str_replace_all(.x, "    NA", ""))
    )

readr::write_rds(
  correlations_inner_years, 
  slituR::paste_forward_slash(tables_directory_path, "correlations-inner-years.rds")
)

## cross ####
format_correlations_cross_by_period <- function(correlations_cross){

  dplyr::filter(correlations_cross, timespan == "period") |>
    tidyr::unnest(summary) |> dplyr::select(-timespan) |> 
    dplyr::mutate(
      portfolios = ifelse(
        pool == "country", "countries", ifelse(stringr::str_ends(pool, "s"), pool, paste0(pool, "s"))
      ),
      regime = factor(regime, levels = regime_levels),
      period = as.character(period),
      period = ifelse(period == "financialization", "financialisation", period),
      period = ifelse(period == "present", "post-crisis", period),
      period = factor(period, levels = period_levels)
    ) |> dplyr::select(-pool) |> 
    dplyr::relocate(portfolios, .before = 1L) |>
    dplyr::arrange(portfolios, period, regime) |>
    tidyr::pivot_wider(names_from = "period", values_from = "average")
}

format_correlations_cross_by_year <- function(correlations_cross){
  
  dplyr::filter(correlations_cross, timespan == "year") |>
    tidyr::unnest(summary) |> dplyr::filter(regime == "whole period") |> 
    dplyr::mutate(
      portfolios = ifelse(
        pool == "country", "countries", ifelse(stringr::str_ends(pool, "s"), pool, paste0(pool, "s"))
      ),
      decade = slituR::floor_year_to_nearest_decade(year),
      year = slituR::remove_decade_from_year(year)
    ) |>
    dplyr::select(-c(timespan, regime, pool)) |> 
    dplyr::arrange(portfolios, decade, year) |>
    tidyr::pivot_wider(names_from = "year", values_from = "average") |>
    dplyr::relocate(`0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, .after = dplyr::last_col()) |>
    dplyr::mutate(
      dplyr::across(c(`0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`), ~ format(.x, scientific = FALSE)),
      dplyr::across(c(`0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`), ~ stringr::str_replace_all(.x, "    NA", ""))
    )
}

### US ####
correlations_cross_US <- readr::read_rds(
  slituR::paste_forward_slash(results_directory_path, "correlations-cross-US.rds")
)
correlations_cross_US$summary[[1L]]
#### By period ####
##### all pools #####
correlations_cross_US_periods_all <- format_correlations_cross_by_period(correlations_cross_US)


##### sub-sectors #####
correlations_cross_US_periods_subsectors <- format_correlations_cross_by_period(correlations_cross_US) |> 
  dplyr::filter(portfolios == "subsectors") |> dplyr::select(-portfolios)

#### By year ####
##### all pools #####
correlations_cross_US_years_all <- format_correlations_cross_by_year(correlations_cross_US)

##### sub-sectors #####
correlations_cross_US_years_subsectors <- format_correlations_cross_by_year(correlations_cross_US) |> 
  dplyr::filter(portfolios == "subsectors") |> dplyr::select(-portfolios)


### global ####
correlations_cross_global <- readr::read_rds(
  slituR::paste_forward_slash(results_directory_path, "correlations-cross-global.rds")
)

#### By period ####
##### all pools #####
correlations_cross_global_periods_all <- format_correlations_cross_by_period(correlations_cross_global)

##### sub-sectors #####
correlations_cross_global_periods_subsectors <- format_correlations_cross_by_period(correlations_cross_global) |> 
  dplyr::filter(portfolios == "subsectors") |> dplyr::select(-portfolios)


#### By year ####
##### all pools #####
correlations_cross_global_years_all <- format_correlations_cross_by_year(correlations_cross_global)

##### sub-sectors #####
correlations_cross_global_years_subsectors <- format_correlations_cross_by_year(correlations_cross_global) |> 
  dplyr::filter(portfolios == "subsectors") |> dplyr::select(-portfolios)

# regressions ##################################################################

## US commodity returns ~ CHP ####
regressions_CHP <- readr::read_rds(
  slituR::paste_forward_slash(results_directory_path, "regressions-time-series-clean.rds")
)

### US commodity returns ~ US commodity individual CHP ####
subsectors <- dplyr::filter(
  regressions_CHP, analysis == "US commodity returns ~ US commodity individual CHP"
) |> dplyr::select(results) |> tidyr::unnest(results) |> 
  dplyr::filter(regressor == "pressure change contemporaneous") |>
  dplyr::group_by(country, sector, subsector, period, regressor, regime.CHP.type, CHP.regime) |>
  dplyr::summarise(`average rsquared` = mean(rsquared, na.rm = TRUE), .groups = "drop")
sectors <- dplyr::filter(
  regressions_CHP, analysis == "US commodity returns ~ US commodity individual CHP"
) |> dplyr::select(results) |> tidyr::unnest(results) |> 
  dplyr::filter(regressor == "pressure change contemporaneous") |>
  dplyr::group_by(country, sector, period, regressor, regime.CHP.type, CHP.regime) |>
  dplyr::summarise(`average rsquared` = mean(rsquared, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(subsector = "all")
countries <- dplyr::filter(
  regressions_CHP, analysis == "US commodity returns ~ US commodity individual CHP"
) |> dplyr::select(results) |> tidyr::unnest(results) |> 
  dplyr::filter(regressor == "pressure change contemporaneous") |>
  dplyr::group_by(country, period, regressor, regime.CHP.type, CHP.regime) |>
  dplyr::summarise(`average rsquared` = mean(rsquared, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(sector = "all", subsector = "all")

`US commodity returns ~ US commodity individual CHP` <- dplyr::bind_rows(
  countries, sectors, subsectors 
) |>
  dplyr::mutate(
    regressor = "Δ% commodity CHP", 
    `average rsquared` = slituR::percentize(`average rsquared`),
    sector = factor(sector, levels = sector_levels),
    subsector = factor(subsector, levels = subsector_levels)
  ) |> 
  dplyr::filter(regime.CHP.type == "none") |>
  dplyr::select(regressor, sector, subsector, period, `average rsquared`) |>
  dplyr::arrange(sector, subsector, period) |>
  tidyr::pivot_wider(names_from = period, values_from = `average rsquared`)

### US commodity returns ~ US commodity aggregate CHP ####
subsectors <- dplyr::filter(
  regressions_CHP, analysis == "US commodity returns ~ US commodity aggregate CHP"
  ) |> dplyr::select(results) |> tidyr::unnest(results) |> 
  dplyr::filter(regressor == "pressure change contemporaneous") |>
  dplyr::group_by(
    country, sector, subsector, period, regressor, regime.CHP.type, CHP.regime
  ) |> dplyr::summarise(`average rsquared` = mean(rsquared, na.rm = TRUE), .groups = "drop")
sectors <- dplyr::filter(
  regressions_CHP, analysis == "US commodity returns ~ US commodity aggregate CHP"
) |> dplyr::select(results) |> tidyr::unnest(results) |> 
  dplyr::filter(regressor == "pressure change contemporaneous") |>
  dplyr::group_by(country, sector, period, regressor, regime.CHP.type, CHP.regime) |>
  dplyr::summarise(`average rsquared` = mean(rsquared, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(subsector = "all")
countries <- dplyr::filter(
  regressions_CHP, analysis == "US commodity returns ~ US commodity aggregate CHP"
) |> dplyr::select(results) |> tidyr::unnest(results) |> 
  dplyr::filter(regressor == "pressure change contemporaneous") |>
  dplyr::group_by(country, period, regressor, regime.CHP.type, CHP.regime) |>
  dplyr::summarise(`average rsquared` = mean(rsquared, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(sector = "all", subsector = "all")

`US commodity returns ~ US commodity aggregate CHP` <- dplyr::bind_rows(
  countries, sectors, subsectors 
) |>
  dplyr::mutate(
    regressor = "Δ% aggregate CHP",
    `average rsquared` = slituR::percentize(`average rsquared`),
    sector = factor(sector, levels = sector_levels),
    subsector = factor(subsector, levels = subsector_levels)
  ) |> 
  dplyr::filter(regime.CHP.type == "none") |>
  dplyr::select(regressor, sector, subsector, period, `average rsquared`) |>
  dplyr::arrange(sector, subsector, period) |>
  tidyr::pivot_wider(names_from = period, values_from = `average rsquared`)

`US commodity returns ~ CHP` <- dplyr::bind_rows(
  `US commodity returns ~ US commodity individual CHP`, 
  `US commodity returns ~ US commodity aggregate CHP`
  )

## all commodity returns ~ market index ####
regressions_index <- readr::read_rds(
  slituR::paste_forward_slash(results_directory_path, "regressions-index.rds")
)

`all commodity returns ~ market index` <- dplyr::filter(
  regressions_index, field == "close price", type == "return", frequency == "day", 
  timespan == "period", regime == "whole period",
  !(country == "US" & sector == "all" & subsector == "all")
) |>
  dplyr::select(country, sector, subsector, period, average) |>
  dplyr::group_by(country, sector, subsector, period) |>
  dplyr::slice_tail(n = 1L) |> dplyr::ungroup() |>
  tidyr::pivot_wider(names_from = "period", values_from = "average") |>
  sort_table_by_country_sector_subsector(sort_levels)

## all commodity returns ~ factors ####
regressions_factors <- readr::read_rds(
  slituR::paste_forward_slash(results_directory_path, "regressions-factors.rds")
)

`all commodity returns ~ factors - long` <- dplyr::filter(
  regressions_factors, field == "close price", type == "return", frequency == "day", 
  timespan == "period", factor != "open interest aggregate", leg == "factor"
) |>
  dplyr::mutate(
    factor = factor |> stringr::str_replace_all("open interest nearby", "open interest")
    ) |>
  dplyr::select(country, sector, subsector, period, factor, regime, average) |>
  dplyr::mutate(average = slituR::percentize(average)) |>
  tidyr::pivot_wider(names_from = "period", values_from = "average") |>
  sort_table_by_country_sector_subsector(sort_levels)


`all commodity returns ~ factors - short` <- dplyr::filter(
  regressions_factors, field == "close price", type == "return", frequency == "day", 
  timespan == "period", factor != "open interest aggregate", leg == "factor", 
  regime == "whole period"
) |>
  dplyr::select(country, sector, subsector, period, factor, average) |>
  dplyr::mutate(
    factor = factor |> stringr::str_replace_all("open interest nearby", "open interest"),
    average = slituR::percentize(average)
  ) |>
  tidyr::pivot_wider(names_from = "period", values_from = "average") 


# export ####
tables <- tibble::tribble(
    ~analysis,                                              ~results,
    "stats - individual assets - whole",                    descriptive_stats_individuals_whole,
    "stats - individual assets - regimes",                  descriptive_stats_individuals_regimes,
    "stats - individual assets - combined",                 descriptive_stats_individuals_combined,
    "stats - equally weighted portfolios",                  descriptive_stats_ew_portfolios,
    "regime difference tests",                              regime_difference_tests,
    "stats-individuals - regime difference tests",          individual_asset_stats_regime_difference_tests,
    "stats-ew - regime difference tests",                   ew_asset_stats_regime_difference_tests,
    "regressions - US returns ~ US CHP",                    `US commodity returns ~ CHP`,
    "correlations - inner - periods",                       correlations_inner_periods,
    "correlations - inner - years",                         correlations_inner_years,
    "correlations - cross - US - periods - all",            correlations_cross_US_periods_all,
    "correlations - cross - US - periods - subsectors",     correlations_cross_US_periods_subsectors,
    "correlations - cross - US - years - all",              correlations_cross_US_years_all,
    "correlations - cross - US - years - subsectors",       correlations_cross_US_years_subsectors,
    "correlations - cross - global - periods - all",        correlations_cross_global_periods_all,
    "correlations - cross - global - periods - subsectors", correlations_cross_global_periods_subsectors,
    "correlations - cross - global - years - all",          correlations_cross_global_years_all,
    "correlations - cross - global - years - subsectors",   correlations_cross_global_years_subsectors,
    "regressions - all returns ~ market index",             `all commodity returns ~ market index`,
    "regressions - all returns ~ factors - long",           `all commodity returns ~ factors - long`,
    "regressions - all returns ~ factors - short",          `all commodity returns ~ factors - short`,
    "assets taxonomy",                                      `assets taxonomy`
  )

readr::write_rds(
  tables, slituR::paste_forward_slash(tables_directory_path, "tables-formatted.rds")
  )

rm(list = ls())
