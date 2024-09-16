library(shinydashboard)
library(shiny)

results_summary_table_UI <- function(id, table_name){
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(6, align = "left", h3(table_name)),
      column(6, align = "right", downloadButton(ns('download'), 'Download .csv')
      )
    ),
    fluidRow(DT::dataTableOutput(ns("table")), width = "100%", height = "auto", fill = TRUE),
    p(linebreaks(3L))
  )
}


results_summary_table_Server <- function(id, table) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    table_reactive <- reactive(table)
    
    output$table <- DT::renderDataTable(
      table_reactive(), filter = 'top', options = list(scrollX = TRUE)
    )
    output$download <- downloadHandler(
      filename = function() {
        paste0("results-", Sys.time(), ".csv")
      },
      content = function(file) {
        readr::write_csv(table_reactive(), file)
      }
    )
  })
}