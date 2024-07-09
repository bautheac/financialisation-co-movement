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
    menuItem("Regressions", tabName = "regressions", icon = icon("table"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "correlations", results_summary_UI("correlations", list(
      period = results$correlations$periods, year = results$correlations$years
    ))),
    tabItem(tabName = "regressions", results_summary_UI("regressions", list(
      period = results$regressions$index$periods, year = results$regressions$index$years
    )))
  )
)

ui <- dashboardPage(
  dashboardHeader(title = "Co-movement"), sidebar, body, skin = "black"
)

server <- function(input, output) {
  results_summary_Server("correlations", list(
    period = results$correlations$periods, year = results$correlations$years
  ))
  
  results_summary_Server("regressions", list(
    period = results$regressions$index$periods, year = results$regressions$index$years
  ))
}

shinyApp(ui, server)
