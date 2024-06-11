
library(shinydashboard)
library(shiny)
library(magrittr)

linebreaks <- function(n){HTML(strrep(br(), n))}

source("datasets.r")


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Correlations", tabName = "correlations", icon = icon("table")),
    menuItem("Regressions", tabName = "regressions", icon = icon("table"))
  )
)



body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "correlations",
      h2("By period"),
      p(linebreaks(3L)),
      h3("Averages"),
      fluidRow(DT::dataTableOutput('correlations_periods_average'), width = "100%", height = "auto", fill = TRUE),
      p(linebreaks(3L)),
      h3("Top-bottom 3"),
      fluidRow(DT::dataTableOutput('correlations_periods_top_bottom_3'), width = "100%", height = "auto", fill = TRUE),
      p(linebreaks(7L)),
      h2("By year"),
      p(linebreaks(3L)),
      h3("Averages"),
      fluidRow(DT::dataTableOutput('correlations_years_average'), width = "100%", height = "auto", fill = TRUE),
      p(linebreaks(3L)),
      h3("Top-bottom 3"),
      fluidRow(DT::dataTableOutput('correlations_years_top_bottom_3'), width = "100%", height = "auto", fill = TRUE),
      
    ),
    tabItem(
      tabName = "regressions",
      fluidRow(DT::dataTableOutput('regressions'), width = "100%", height = "auto", fill = TRUE)
    )
  )
)
      




ui <- dashboardPage(
  dashboardHeader(title = "Co-movement"), sidebar, body, skin = "black"
)

server <- function(input, output) {

  output$correlations_periods_average <- DT::renderDataTable(
    correlations_periods_average, filter = 'top', options = list(scrollX = TRUE)
  )
  output$correlations_periods_top_bottom_3 <- DT::renderDataTable(
    correlations_periods_top_bottom_3, filter = 'top', options = list(scrollX = TRUE)
  )
  output$correlations_years_average <- DT::renderDataTable(
    correlations_years_average, filter = 'top', options = list(scrollX = TRUE)
  )
  output$correlations_years_top_bottom_3 <- DT::renderDataTable(
    correlations_years_top_bottom_3, filter = 'top', options = list(scrollX = TRUE)
  )
  
  
  output$regressions <- DT::renderDataTable(
    regressions, filter = 'top', options = list(scrollX = TRUE)
  )
}

shinyApp(ui, server)