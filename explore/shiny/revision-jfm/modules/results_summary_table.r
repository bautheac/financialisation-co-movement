library(shinydashboard)
library(shiny)

results_summary_table_UI <- function(id, table_name){
  
  ns <- NS(id)
  
  tagList(
    h3(table_name),
    fluidRow(DT::dataTableOutput(ns("table")), width = "100%", height = "auto", fill = TRUE),
  )
}


results_summary_table_Server <- function(id, table) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$table <- DT::renderDataTable(
      table, filter = 'top', options = list(scrollX = TRUE)
    )
  })
}