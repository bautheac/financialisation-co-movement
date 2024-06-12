library(shinydashboard)
library(shiny)



path_directory <- file.path(here::here(), "explore", "shiny", "revision-jfm")

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
    tabItem(tabName = "correlations", results_summary_UI("correlations")),
    tabItem(tabName = "regressions", results_summary_UI("regressions"))
  )
)

ui <- dashboardPage(
  
  dashboardHeader(title = "Co-movement"), sidebar, body, skin = "black"
)


server <- function(input, output) {
  
  results_summary_Server("correlations", results$correlations)
  results_summary_Server("regressions", results$regressions)
}


shinyApp(ui, server)