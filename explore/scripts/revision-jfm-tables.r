library(magrittr)

significance <- function(p.value, estimate){
  if(p.value <= 0.01) paste0("***", estimate)
  else if (p.value > 0.01 && p.value <= 0.05) paste0("**", estimate)
  else if (p.value > 0.05 && p.value <= 0.10) paste0("*", estimate)
  else estimate
}

percentize <- function(value) paste0(round(value, digits = 4L) * 100, "%")

factors <- c("market", "CHP", "open interest nearby", "open interest aggregate", "term structure")
sector_levels <- c("all", "agriculturals", "energy", "metals")
subsector_levels <- c("all", "grains", "livestock", "softs", "gas", "petroleum", "base", "precious")

# `US commodities ~ factors from US commodities ####
path <- paste0(here::here(), "/explore/results/revision-jfm/regressions-factor.rds")
raw <- readRDS(path)

formatted <- dplyr::filter(raw, factor %in% factors, timespan == "period") %>%
  dplyr::select(-c(timespan, year, field, type, frequency, sector, subsector)) %>%
  dplyr::mutate(
    period = factor(period, levels = c("past", "financialization", "crisis", "present")),
    factor = factor(factor, levels = factors),
    average = percentize(average)
  ) %>% dplyr::relocate(regime, .after = leg) %>% 
  dplyr::arrange(country, factor, leg, period, regime) %>%
  tidyr::pivot_wider(names_from = "period", values_from = "average")

path <- paste0(here::here(), "/explore/tables/revision-jfm/regressions-factors.csv")
readr::write_csv(formatted, path)

