library(magrittr)


split_analysis_summary_results_into_category_dataframes <- function(analysis_summary_results){
  
  analysis_periods <- dplyr::filter(analysis_summary_results, timespan == "period") %>%
    dplyr::select(-c(timespan, year))
  
  analysis_periods_average <- dplyr::select(
    analysis_periods,
    field, type, frequency, country, sector, subsector, period, regime, average
  ) %>% 
    dplyr::group_by(field, type, frequency, country, sector, subsector, period, regime) %>%
    dplyr::filter(dplyr::row_number() == dplyr::n())
  
  analysis_periods_top_bottom_3 <- dplyr::select(analysis_periods, -average)
  
  analysis_years <- dplyr::filter(analysis_summary_results, timespan == "year") %>%
    dplyr::select(-c(timespan, period))
  
  analysis_years_average <- dplyr::select(
    analysis_years,
    field, type, frequency, country, sector, subsector, year, regime, average
  ) %>% 
    dplyr::group_by(field, type, frequency, country, sector, subsector, year, regime) %>%
    dplyr::filter(dplyr::row_number() == dplyr::n())
  
  analysis_years_top_bottom_3 <- dplyr::select(analysis_years, -average)
  
  list(
    periods = list(Average = analysis_periods_average, `Top(-bottom) 3` = analysis_periods_top_bottom_3),
    years = list(Average = analysis_years_average, `Top(-bottom) 3` = analysis_years_top_bottom_3)
  )
}


construct_path <- function() {
  shiny_env <- Sys.getenv("SHINY_ENV")
  if (shiny_env == "local") {
    # Local machine
    path_directory <- here::here("explore", "shiny", "revision-jfm")
  } else {
    # Server
    path_directory <- here::here()
  }
  
  return(path_directory)
}
path_directory <- construct_path()


correlations_raw <- readr::read_rds(file.path(path_directory, "results", "correlations.rds"))
correlations_split <- split_analysis_summary_results_into_category_dataframes(correlations_raw)

regressions_index_raw <- readr::read_rds(file.path(path_directory, "results", "regressions-index.rds"))
regressions_index_split <- split_analysis_summary_results_into_category_dataframes(regressions_index_raw)

regressions_factors_raw <- readr::read_rds(file.path(path_directory, "results", "regressions-factors.rds"))
regressions_factors_split <- split_analysis_summary_results_into_category_dataframes(regressions_factors_raw)

results <- list(
  correlations = correlations_split, 
  regressions = list(index = regressions_index_split, factors = regressions_factors_split)
  )