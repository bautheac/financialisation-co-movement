library(shinydashboard)
library(shiny)

results_summary_category_UI <- function(id, category_name){
  
  ns <- NS(id)

  tagList(
    h2(paste("By", category_name)),
    p(linebreaks(3L)),
    results_summary_table_UI(ns("average"), "Average"),
    p(linebreaks(3L)),
    results_summary_table_UI(ns("top_bottom_3"), "Top-bottom 3"),
  )
}


results_summary_category_Server <- function(id, results) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns

    results_summary_table_Server("average", results$average)
    results_summary_table_Server("top_bottom_3", results$top_bottom_3)
  })
}