library(magrittr)

extract_analysis_summary_results_average_table <- function(
    analysis_summary_results_category, category, summary_group_variables = c(), 
    summary_analysis_variables
    ){
  
  category_sym <- rlang::sym(category)
  group_vars <- rlang::syms(summary_group_variables)
  analysis_vars <- rlang::syms(summary_analysis_variables)
  
  dplyr::select(
    analysis_summary_results_category,
    field, type, frequency, country, sector, subsector, !!category_sym, 
    !!!group_vars, regime, !!!analysis_vars
  ) %>% 
    dplyr::group_by(
      field, type, frequency, country, sector, subsector, !!category_sym, 
      !!!group_vars, regime
      ) %>%
    dplyr::filter(dplyr::row_number() == dplyr::n()) %>% dplyr::ungroup()
}


extract_analysis_summary_results_top_3_table <- function(
    analysis_summary_results_category, category, summary_group_variables = c(), 
    summary_analysis_variables
    ){
  
  dplyr::select(analysis_summary_results_category, -average)
}


extract_analysis_summary_results_by_category <- function(
    analysis_summary_results, category, variables
    ){
  
  analysis <- dplyr::filter(analysis_summary_results, timespan == category) %>%
    dplyr::select(-c("timespan", ifelse(category == "period", "year", "period")))
  
  lapply(seq_along(variables$calls), function(i){
    fun <- paste0("extract_analysis_summary_results_", variables$calls[[i]],  "_table")
    args <- list(
      analysis, category, 
      variables$variables[[i]]$summary_group_variables, variables$variables[[i]]$summary_analysis_variables
      )
    do.call(fun, args = args)
  }) %>% setNames(names(variables$variables))
}



split_analysis_summary_results_into_category_dataframes <- function(
    analysis_summary_results, helpers
    ){
  
  lapply(seq_along(helpers), function(i){
    fun <- extract_analysis_summary_results_by_category
    args <- list(
      analysis_summary_results, names(helpers)[i], helpers[[i]]
      )
    do.call(fun, args = args)
  }) %>% setNames(names(helpers))
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


correlations_inner_raw <- readr::read_rds(file.path(path_directory, "results", "correlations-inner.rds"))
correlations_inner_helpers <- list(
  period = list(
    calls = c("average", "top_3"), 
    variables = list(
      Average = list(summary_group_variables = c(), summary_analysis_variables = "average"),
      `Top(-bottom) 3` = list(summary_group_variables = c(), summary_analysis_variables = c("top-bottom 3", "pair", "correlation"))
      )
  ),
  year = list(
    calls = c("average", "top_3"), 
    variables = list(
      Average = list(summary_group_variables = c(), summary_analysis_variables = "average"),
      `Top(-bottom) 3` = list(summary_group_variables = c(), summary_analysis_variables = c("top-bottom 3", "pair", "correlation"))
    )
  )
)
correlations_inner_split <- split_analysis_summary_results_into_category_dataframes(
  correlations_inner_raw, correlations_inner_helpers
  )

regressions_index_raw <- readr::read_rds(file.path(path_directory, "results", "regressions-index.rds"))
regressions_index_helpers <- list(
  period = list(
    calls = c("average", "top_3"), 
    variables = list(
      Average = list(summary_group_variables = c(), summary_analysis_variables = "average"),
      `Top(-bottom) 3` = list(summary_group_variables = c(), summary_analysis_variables = c("top-bottom 3", "beta", "p value", "R squared"))
    )
  ),
  year = list(
    calls = c("average", "top_3"), 
    variables = list(
      Average = list(summary_group_variables = c(), summary_analysis_variables = "average"),
      `Top(-bottom) 3` = list(summary_group_variables = c(), summary_analysis_variables = c("top-bottom 3", "beta", "p value", "R squared"))
    )
  )
)
regressions_index_split <- split_analysis_summary_results_into_category_dataframes(
  regressions_index_raw, regressions_index_helpers
  )

regressions_factors_raw <- readr::read_rds(file.path(path_directory, "results", "regressions-factors.rds"))
regressions_factors_helpers <- list(
  period = list(
    calls = "average", 
    variables = list(
      Average = list(summary_group_variables = c("factor", "leg"), summary_analysis_variables = "average")
    )
  ),
  year = list(
    calls = "average", 
    variables = list(
      Average = list(summary_group_variables = c("factor", "leg"), summary_analysis_variables = "average")
    )
  )
)
regressions_factors_split <- split_analysis_summary_results_into_category_dataframes(regressions_factors_raw, regressions_factors_helpers)


results <- list(
  correlations = list(inner = correlations_inner_split), 
  regressions = list(index = regressions_index_split, factors = regressions_factors_split)
)
