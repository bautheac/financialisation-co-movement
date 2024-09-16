library(shinydashboard)
library(shiny)

construct_path <- function() {
  shiny_env <- Sys.getenv("SHINY_ENV")
  if (shiny_env == "local") {
    path_directory <- here::here("explore", "shiny", "revision-jfm")
  } else {
    path_directory <- here::here()
  }
  
  return(path_directory)
}
path_directory <- construct_path()

source(file.path(path_directory, "datasets.r"))
source(file.path(path_directory, "functions.r"))
source(file.path(path_directory, "modules", "results_summary_table.r"))
source(file.path(path_directory, "modules", "results_summary_category.r"))
source(file.path(path_directory, "modules", "results_summary.r"))

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Correlations", tabName = "correlations", icon = icon("table")),
    menuItem("Regressions", icon = icon("table"),
             menuSubItem("Index", tabName = "regressions-index"),
             menuSubItem("Factors", tabName = "regressions-factors")
    )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "correlations", results_summary_UI("correlations", list(
      period = results$correlations$period, year = results$correlations$year
    ))),
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
  results_summary_Server("correlations", list(
    period = results$correlations$period, year = results$correlations$year
  ))
  
  results_summary_Server("regressions-index", list(
    period = results$regressions$index$period, year = results$regressions$index$year
  ))

  results_summary_Server("regressions-factors", list(
    period = results$regressions$factors$period, year = results$regressions$index$year
  ))
}

shinyApp(ui, server)
