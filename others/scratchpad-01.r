

old <- readr::read_rds(
  paste(here::here(), "explore/results/new/regressions.rds", sep = "/")
)

new <- readr::read_rds(
  paste(here::here(), "explore/results/new/regressions-new.rds", sep = "/")
)

head(new$results[[2L]], 100L) %>% dplyr::select()
tail(new$results[[2L]], 100L)

head(new$results[[2L]], 100L)$model
