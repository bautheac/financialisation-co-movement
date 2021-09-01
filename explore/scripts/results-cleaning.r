library(magrittr)
data(tickers_futures, package = "BBGsymbols")
data(exchanges, package = "fewISOs")

commodity_names <- dplyr::right_join(
  dplyr::select(exchanges, MIC, country),
  dplyr::select(
    tickers_futures, ticker, commodity_name = name, MIC, sector, subsector
  ),
  by = "MIC"
) %>% dplyr::select(ticker, commodity_name, MIC, country, sector, subsector) %>%
  dplyr::mutate(
    commodity_name = ifelse(
      nchar(commodity_name) > 15L,
      stringr::str_extract(commodity_name, "^.+?(?=-)"),
      commodity_name
    ),
    commodity_name = stringr::str_replace_all(paste0(commodity_name, " (", MIC, ")"), "  ", " ")
  ) %>% dplyr::select(-MIC)

fields <- tibble::tibble(
  symbol = c(
    "carry", "FUT_AGGTE_OPEN_INT", "FUT_AGGTE_VOL", "OPEN_INT", "PX_ASK", "PX_BID", 
    "PX_HIGH", "PX_LAST", "PX_LOW", "PX_MID", "PX_OPEN", "PX_VOLUME"
  ),
  field_name = c(
    "carry", "aggregate open interest", "aggregate volume", "open interest", 
    "ask price", "bid price", "high price", "close price", "low price", "mid price", 
    "open price", "volume"
  )
)





# Descriptive statistics ####
statistics <- readr::read_rds(
  here::here("explore", "results", "descriptive-statistics.rds")
)

## Extract mean and p.value from raw model ####
commodity_futures <- dplyr::mutate(
  statistics$results[[1L]],
  results = purrr::map(results, function(y){
    dplyr::mutate(
      y, 
      results = purrr::map(results, function(x){
        
        # browser()
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
  
  results = purrr::map(results, function(y){
    dplyr::mutate(
      y,
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


## Annualise returns and standard deviations ####
frequencies <- tibble::tibble(
  frequency = c("daily", "weekly", "monthly"),
  number_days = c(252L, 52L, 12L)
)

commodity_futures <- dplyr::mutate(
  commodity_futures,
  results = purrr::map(results, function(x){
    dplyr::mutate(
      x,
      results = purrr::map(results, function(y){
        dplyr::left_join(y, frequencies, by = "frequency") %>%
          dplyr::mutate(
            mean = purrr::pmap_dbl(
              list(type, mean, number_days), 
              function(a, b, c) if (a == "returns") b * c else b
            ),
            sd = purrr::pmap_dbl(
              list(type, sd, number_days), 
              function(a, b, c) if (a == "returns") b * sqrt(c) else b
            )
          ) %>%
          dplyr::select(-number_days)
      })
    )
  })
)



frequencies <- tibble::tibble(
  `return frequency` = c("day", "week", "month"),
  number_days = c(252L, 52L, 12L)
)

factors <- dplyr::mutate(
  factors,
  results = purrr::map(results, function(y){
    dplyr::left_join(y, frequencies, by = "return frequency") %>%
      dplyr::mutate(
        mean = purrr::map2_dbl(
          mean, number_days, 
          function(y, z) tryCatch(
            { y * z }, 
            error=function(cond) {NA_real_}
          )
        ),
        sd = purrr::map2_dbl(
          sd, number_days, 
          function(y, z) tryCatch(
            { y * sqrt(z) }, 
            error=function(cond) {NA_real_}
          )
        )
      ) %>%
      dplyr::select(-number_days)
  })
)



## Gather ####
statistics_clean <- dplyr::mutate(
  statistics, results = list(commodity_futures, factors)
)



## Swap tickers for commodity names, field symbols for names ####
statistics_clean$results[[1L]] <- dplyr::mutate(
  statistics_clean$results[[1L]],
  results = purrr::map(results, function(y){
    dplyr::mutate(
      y, 
      results = purrr::map(results, function(x){
        
        results <- if ("active contract ticker" %in% names(x)){
          dplyr::left_join(
            x, commodity_names, by = c("active contract ticker" = "ticker")
          ) %>% 
            dplyr::select(
              ticker = `active contract ticker`, 
              commodity = commodity_name, 
              dplyr::everything()
            ) 
        } else x
        
        
        if ("field" %in% names(x)){
          dplyr::left_join(
            results, fields, by = c("field" = "symbol")
          ) %>% 
            dplyr::mutate(field = field_name) %>% 
            dplyr::select(-field_name)
        } else results
      })
    )
  })
)

## Save ####
readr::write_rds(
  statistics_clean,
  here::here("explore", "results", "descriptive-statistics-clean.rds")
) 
readr::write_rds(
  statistics_clean,
  here::here(
    "explore", "shiny", "financialization-asset-pricing", "results",
    "descriptive-statistics-clean.rds"
  )
) 









# Tests of differences ####
differences <- readr::read_rds(
  here::here("explore", "results", "tests-differences.rds")
)

periods <- dplyr::filter(differences, analysis == "periods") %>%
  dplyr::select(results) %>% tidyr::unnest(results)

## periods ####
### Swap tickers for commodity names, field symbols for names ####
periods$results[[1L]] <- dplyr::mutate(
  periods$results[[1L]],
  results = purrr::map(results, function(y){
    dplyr::mutate(
      y, 
      results = purrr::map(results, function(x){
        # browser()
        results <- if ("active contract ticker" %in% names(x)){
          dplyr::left_join(
            x, commodity_names, by = c("active contract ticker" = "ticker")
          ) %>% 
            dplyr::select(
              ticker = `active contract ticker`, 
              commodity = commodity_name, 
              dplyr::everything()
            ) 
        } else x
        
        
        if ("field" %in% names(x)){
          dplyr::left_join(
            results, fields, by = c("field" = "symbol")
          ) %>% 
            dplyr::mutate(field = field_name) %>% 
            dplyr::select(-field_name)
        } else results
      })
    )
  })
)


regimes <- dplyr::filter(differences, analysis == "regimes") %>%
  dplyr::select(results) %>% tidyr::unnest(results)

## regimes ####
### Swap tickers for commodity names, field symbols for names ####
regimes$results[[1L]] <- dplyr::mutate(
  regimes$results[[1L]],
  results = purrr::map(results, function(y){
    dplyr::mutate(
      y, 
      results = purrr::map(results, function(x){
        # browser()
        results <- if ("active contract ticker" %in% names(x)){
          dplyr::left_join(
            x, commodity_names, by = c("active contract ticker" = "ticker")
          ) %>% 
            dplyr::select(
              ticker = `active contract ticker`, 
              commodity = commodity_name, 
              dplyr::everything()
            ) 
        } else x
        
        
        if ("field" %in% names(x)){
          dplyr::left_join(
            results, fields, by = c("field" = "symbol")
          ) %>% 
            dplyr::mutate(field = field_name) %>% 
            dplyr::select(-field_name)
        } else results
      })
    )
  })
)


differences <- tibble::tibble(
  analysis = c("periods", "regimes"), results = list(periods, regimes)
)

## Save ####
readr::write_rds(
  differences,
  here::here("explore", "results", "tests-differences-clean.rds")
) 
readr::write_rds(
  differences,
  here::here(
    "explore", "shiny", "financialization-asset-pricing", "results",
    "tests-differences-clean.rds"
  )
) 






# Correlations ####

correlations <- readr::read_rds(
  here::here("explore", "results", "correlations.rds")
)

## Calculate average correlation
correlations_clean <- dplyr::mutate(
  correlations, 
  results = purrr::map(results, function(x){
    dplyr::mutate(
      x,
      average_correlation = purrr::map(correlations, function(y){
        mean(y[upper.tri(y)])
      }) %>% purrr::flatten_dbl()
    )
  }) 
)

## Swap tickers for commodity names
correlations_clean <- dplyr::mutate(
  correlations_clean,
  results = purrr:::map(results, function(x){
    cor_names <- purrr::map(x$correlations, function(y){
      # browser()
      names <- tibble::tibble(ticker = attr(y, "dimnames")[[1L]]) %>%
        dplyr::left_join(commodity_names, by = "ticker")
      correlations <- tibble::as_tibble(y) 
      names(correlations) <- names$commodity_name
      dplyr::mutate(correlations, commodity = names$commodity_name) %>%
        dplyr::select(commodity, dplyr::everything())
    })
    # browser()
    x$correlations <- cor_names
    dplyr::left_join(
      x, fields, by = c("field" = "symbol")
    ) %>% 
      dplyr::mutate(field = field_name) %>% 
      dplyr::select(-field_name)
    
  })
)


## Save
readr::write_rds(
  correlations_clean,
  here::here("explore", "results", "correlations-clean.rds")
)
readr::write_rds(
  correlations_clean,
  here::here(
    "explore", "shiny", "financialization-asset-pricing", "results",
    "correlations-clean.rds"
  )
)






# Regressions ####
## Time series ####
### factors ####
regressions <- readr::read_rds(
  here::here("explore", "results", "regressions-time-series.rds")
) %>% dplyr::filter(stringr::str_detect(analysis, "factor"))

#### Swap tickers for commodity names
regressions_factors_clean <- dplyr::mutate(
  regressions,
  results = purrr::map(results, function(x){
    # browser()
    results <- if ("commodity" %in% names(x)){
      dplyr::left_join(x, commodity_names, by = c("commodity" = "ticker")) %>%
        dplyr::select(-commodity) %>% 
        dplyr::rename(commodity = commodity_name) %>%
        dplyr::relocate(country, sector, subsector, commodity, .before = period)
    } else if ("pick" %in% names(x)){
      dplyr::left_join(x, commodity_names, by = c("pick" = "ticker")) %>%
        dplyr::select(-pick) %>% dplyr::rename(pick = commodity_name) %>%
        dplyr::relocate(country, sector, subsector, pick, .before = regime)
    }
    
    results <- dplyr::mutate(
      results, 
      coefficients = purrr::map(coefficients, function(y){
        browser()
        tryCatch({ 
          dplyr::mutate(
            y, 
            term = stringr::str_replace_all(term, "\\(Intercept\\)", "alpha"),
            term = stringr::str_replace_all(term, "`", "")
          )}, error = function(e) { NA }
        )
      }),
      regressors = stringr::str_replace_all(regressors, "`", "")
    ) %>% na.omit()
    
    if ("leg" %in% names(x)){
      dplyr::arrange(
        results,
        `asset pool`, `update frequency`, `return frequency`, `ranking period`, 
        `long threshold`, `short threshold`, regressors, leg, period, regime
      )
    } else {
      dplyr::arrange(
        results,
        `update frequency`, `return frequency`, `ranking period`, 
        `long threshold`, `short threshold`, period, regressors, regime
      )
    }
  }) 
) 

### CHP ####
regressions <- readr::read_rds(
  here::here("explore", "results", "regressions-time-series.rds")
) %>% dplyr::filter(stringr::str_detect(analysis, "CHP"))

#### Swap tickers for commodity names
regressions_CHP_clean <- dplyr::mutate(
  regressions,
  results = purrr::map(results, function(x){
    # browser()
    results <- if ("commodity" %in% names(x)){
      dplyr::left_join(x, commodity_names, by = c("commodity" = "ticker")) %>%
        dplyr::select(-commodity) %>% 
        dplyr::rename(commodity = commodity_name) %>%
        dplyr::relocate(country, sector, subsector, commodity, .before = period)
    } else if ("pick" %in% names(x)){
      dplyr::left_join(x, commodity_names, by = c("pick" = "ticker")) %>%
        dplyr::select(-pick) %>% dplyr::rename(pick = commodity_name) %>%
        dplyr::relocate(country, sector, subsector, pick, .before = regime)
    }
    
    # browser()
    results <- dplyr::mutate(
      results, 
      coefficients = purrr::map(coefficients, function(y){
        # browser()
        tryCatch({ 
          dplyr::mutate(
            y, 
            term = stringr::str_replace_all(term, "\\(Intercept\\)", "intercept"),
            term = stringr::str_replace_all(term, "\\.", " ")
          )}, error = function(e) { NA }
        )
      }),
      regressor = stringr::str_replace_all(regressor, "\\.", " ")
    )
  }) 
) 


## Save
regressions_clean <- dplyr::bind_rows(regressions_factors_clean, regressions_CHP_clean)

readr::write_rds(
  regressions_clean, 
  here::here("explore", "results", "regressions-time-series-clean.rds")
)
readr::write_rds(
  regressions_clean, 
  here::here(
    "explore", "shiny", "financialization-asset-pricing", "results",
    "regressions-time-series-clean.rds"
  )
)

## Cross-section ####
regressions <- readr::read_rds(
  here::here("explore", "results", "regressions-cross-section.rds")
)

## Swap tickers for commodity names
regressions_clean <- dplyr::mutate(
  regressions,
  results = purrr::map(results, function(x){
    # browser()
    dplyr::mutate(
      x,
      betas = purrr::map(betas, function(y){
        tryCatch(
          { 
            dplyr::left_join(y, commodity_names, by = "ticker") %>%
              dplyr::select(-ticker) %>%
              dplyr::select(
                country, sector, subsector, commodity= commodity_name, 
                dplyr::everything()
              ) %>% 
              dplyr::arrange(country, sector, subsector, commodity) 
          }, 
          error = function(e) { NA }
        )
      }),
      lambdas = purrr::map(lambdas, function(y){
        tryCatch(
          { 
            dplyr::mutate(
              y, term = stringr::str_replace_all(term, "`", "")
            )
          }, 
          error = function(e) { NA }
        )
      }),
      regressors = stringr::str_replace_all(regressors, "`", "")
    )
  }) 
) 

## Save
readr::write_rds(
  regressions_clean, 
  here::here("explore", "results", "regressions-cross-section-clean.rds")
)
readr::write_rds(
  regressions_clean, 
  here::here(
    "explore", "shiny", "financialization-asset-pricing", "results",
    "regressions-cross-section-clean.rds"
  )
)

















# Factor picks ####

## Swap tickers for commodity names
picks <- readr::read_rds(
  here::here("explore", "results", "factor-picks.rds")
) %>%
  dplyr::mutate_if(.predicate = is.factor, .funs = as.character) %>% 
  dplyr::left_join(commodity_names, by = c("pick" = "ticker")) %>%
  dplyr::select(-pick) %>% dplyr::rename(pick = commodity_name) %>%
  dplyr::relocate(country, sector, subsector, pick, .before = `r-squared`)

## Save
readr::write_rds(
  picks, here::here("explore", "results", "factor-picks-clean.rds")
)
readr::write_rds(
  picks, 
  here::here(
    "explore", "shiny", "financialization-asset-pricing", "results",
    "factor-picks-clean.rds"
  )
)





# Proportions ####

## Swap tickers for commodity names
proportions <- readr::read_rds(
  here::here("explore", "results", "proportions.rds")
) %>%
  dplyr::left_join(commodity_names, by = c("pick" = "ticker")) %>%
  dplyr::select(-pick) %>% dplyr::rename(pick = commodity_name) %>%
  dplyr::relocate(country, sector, subsector, pick, .before = long)

## Save
readr::write_rds(
  proportions, here::here("explore", "results", "proportions-clean.rds")
)
readr::write_rds(
  proportions, 
  here::here(
    "explore", "shiny", "financialization-asset-pricing", "results",
    "proportions-clean.rds"
  )
)








