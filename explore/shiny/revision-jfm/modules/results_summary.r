library(shinydashboard)
library(shiny)

results_summary_UI <- function(id, categories) {
  
  ns <- NS(id)
  
  tagList(
    lapply(seq_along(categories), function(i) {
      results_summary_category_UI(ns(names(categories)[i]), names(categories)[i], categories[[i]])
    })
  )
}

results_summary_Server <- function(id, results) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    lapply(seq_along(results), function(i) {
      results_summary_category_Server(names(results)[i], results[[i]])
    })
  })
}
