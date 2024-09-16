
library(magrittr)



statistics <- readr::read_rds(
  paste(here::here(), "explore/results/new/descriptive statistics.rds", sep = "/")
)

commodity_futures <- dplyr::mutate(
  statistics$results[[1L]],
  results = purrr::map(results, function(y){
    dplyr::mutate(
      y, 
      results = purrr::map(results, function(x){
        dplyr::mutate(
          x, 
          p.value = purrr::map(mean, function(a){
            tryCatch(
              { a$p.value }, 
              error=function(cond) {NA_real_}
            )
          }),
          mean = purrr::map(mean, function(a){
            tryCatch(
              { a$estimate }, 
              error=function(cond) {NA_real_}
            )
          })
        ) %>%
          dplyr::relocate(sd, .after = last_col())
      })
    )
  })
)


factors <- dplyr::mutate(
  statistics$results[[2L]],
  p.value = purrr::map(mean, function(a){
    tryCatch(
      { a$p.value }, 
      error=function(cond) {NA_real_}
    )
  }),
  mean = purrr::map(mean, function(a){
    tryCatch(
      { a$estimate }, 
      error=function(cond) {NA_real_}
    )
    }
  )
) %>%
  dplyr::relocate(sd, .after = last_col())


statistics_neat <- dplyr::mutate(
  statistics, results = list(commodity_futures, factors)
)



statistics <- readr::write_rds(
  statistics_neat,
  paste(
    here::here(), "explore/results/new/descriptive statistics - clean.rds", sep = "/"
  )
) 

