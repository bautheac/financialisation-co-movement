

config <- list(
  renv.config.cache.enabled = FALSE,
  renv.config.install.transactional = FALSE
)

options(config)

settings <- list(
  r.version = 3.6, use.cache = FALSE, vcs.ignore.cellar = TRUE,
  vcs.ignore.library = TRUE, vcs.ignore.local = TRUE
)

renv::init(settings = settings)

