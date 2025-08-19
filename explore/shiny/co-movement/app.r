library(dplyr)
library(DT)
library(here) 
library(magrittr)
library(purrr)
library(readr)
library(rlang)
library(shiny) 
library(shinydashboard)
library(tidyr)



app_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (identical(Sys.getenv("SHINY_ENV"), "local")) {
  app_dir <- here::here("explore", "shiny", "revision-jfm")
}

source(file.path(app_dir, "datasets.r"))
source(file.path(app_dir, "functions.r"))
source(file.path(app_dir, "modules", "results_summary_table.r"))
source(file.path(app_dir, "modules", "results_summary_category.r"))
source(file.path(app_dir, "modules", "results_summary.r"))

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Stats", tabName = "stats",
             menuItem("Descriptive", tabName = "stats-descriptive"),
             menuItem("Regime ≠ tests", tabName = "stats-difference-tests")
             ),
    menuItem("Correlations", tabName = "correlations",
             menuItem("Inner", tabName = "correlations-inner"),
             menuItem("Cross", tabName = "correlations-cross", 
                      menuItem("US", tabName = "correlations-cross-US"),
                      menuItem("Global", tabName = "correlations-cross-global")
            )
    
    ),
    menuItem("Regressions",
             menuItem("Index", tabName = "regressions-index"),
             menuItem("Factors", tabName = "regressions-factors")
    )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "stats-descriptive", results_summary_UI("stats-descriptive", list(
      period = results$stats$descriptive$period, year = results$stats$descriptive$year
    ))),
    tabItem(tabName = "stats-difference-tests", results_summary_UI("stats-difference-tests", list(
      period = results$stats$regime_tests$period
    ))),
    tabItem(tabName = "correlations-inner", results_summary_UI("correlations-inner", list(
      period = results$correlations$inner$period, year = results$correlations$inner$year
    ))),
    tabItem(tabName = "correlations-cross-US", results_summary_UI("correlations-cross-US", list(
      period = results$correlations$cross$US$period, year = results$correlations$cross$US$year
    ))),
    tabItem(tabName = "correlations-cross-global", results_summary_UI("correlations-cross-global", list(
      period = results$correlations$cross$global$period, year = results$correlations$cross$global$year
    ))),
    tabItem(tabName = "correlations-cross", h2("Cross")),
    tabItem(tabName = "regressions-index", results_summary_UI("regressions-index", list(
      period = results$regressions$index$period, year = results$regressions$index$year
    ))),
    tabItem(tabName = "regressions-factors", results_summary_UI("regressions-factors", list(
      period = results$regressions$factors$period, year = results$regressions$factors$year
    )))
  )
)

ui <- dashboardPage(
  dashboardHeader(title = "Co-movement"), sidebar, body, skin = "black"
)

server <- function(input, output) {
  
  results_summary_Server("stats-descriptive", list(
    period = results$stats$descriptive$period, year = results$stats$descriptive$year
  ))
  
  results_summary_Server("stats-difference-tests", list(period = results$stats$regime_tests$period))
  
  results_summary_Server("correlations-inner", list(
    period = results$correlations$inner$period, year = results$correlations$inner$year
  ))
  
  results_summary_Server("correlations-cross-US", list(
    period = results$correlations$cross$US$period, year = results$correlations$cross$US$year
  ))
  results_summary_Server("correlations-cross-global", list(
    period = results$correlations$cross$global$period, year = results$correlations$cross$global$year
  ))
  
  results_summary_Server("regressions-index", list(
    period = results$regressions$index$period, year = results$regressions$index$year
  ))

  results_summary_Server("regressions-factors", list(
    period = results$regressions$factors$period, year = results$regressions$index$year
  ))
}

shinyApp(ui, server)
