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
    periods = list(average = analysis_periods_average, top_bottom_3 = analysis_periods_top_bottom_3),
    years = list(average = analysis_years_average, top_bottom_3 = analysis_years_top_bottom_3)
  )
}


correlations_raw <- readr::read_rds(paste0(here::here(), "/explore/tables/revision-jfm/correlations.rds"))
correlations_split <- split_analysis_summary_results_into_category_dataframes(correlations_raw)

regressions_raw <- readr::read_rds(paste0(here::here(), "/explore/tables/revision-jfm/regressions.rds"))
regressions_split <- split_analysis_summary_results_into_category_dataframes(regressions_raw)

results <- list(correlations = correlations_split, regressions = regressions_split)





