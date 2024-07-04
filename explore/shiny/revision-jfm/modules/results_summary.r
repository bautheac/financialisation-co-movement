library(shinydashboard)
library(shiny)

results_summary_UI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    results_summary_category_UI(ns("period"), "period"),
    p(linebreaks(7L)),
    results_summary_category_UI(ns("year"), "year"),
  )
}

results_summary_Server <- function(id, results) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    results_summary_category_Server("period", results$periods)
    results_summary_category_Server("year", results$years)
  })
}