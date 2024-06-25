library(magrittr)

significance <- function(p.value, estimate){
  if(p.value <= 0.01) paste0("***", estimate)
  else if (p.value > 0.01 && p.value <= 0.05) paste0("**", estimate)
  else if (p.value > 0.05 && p.value <= 0.10) paste0("*", estimate)
  else estimate
}

percentize <- function(value) paste0(round(value, digits = 4L) * 100, "%")

factors <- c("market", "CHP", "`OI nearby`", "`OI aggregate`", "`term structure`")
sector_levels <- c("all", "agriculturals", "energy", "metals")
subsector_levels <- c("all", "grains", "livestock", "softs", "gas", "petroleum", "base", "precious")

# `US commodities ~ factors from US commodities ####
path <- paste0(here::here(), "/explore/results/revision-jfm/regressions-factors.rds")
raw <- readRDS(path)

formatted <- dplyr::filter(raw, regressors %in% factors) %>%
  dplyr::group_by(regressors, leg, period, regime) %>%
  dplyr::summarise(`average rsquared` = mean(rsquared, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    period = factor(period, levels = c("past", "financialization", "crisis", "present")),
    regressors = factor(regressors, levels = c("market", factors[ factors != "market"])),
    `average rsquared` = percentize(`average rsquared`)
  ) %>% dplyr::arrange(regressors, leg, period, regime) %>%
  tidyr::pivot_wider(names_from = "period", values_from = "average rsquared") %>%
  dplyr::rename(factor = regressors) %>% dplyr::filter(factor != "`OI aggregate`")

path <- paste0(here::here(), "/explore/tables/revision-jfm/regressions-factors.csv")
readr::write_csv(formatted, path)

