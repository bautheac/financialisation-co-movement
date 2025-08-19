
results_summary_category_UI <- function(id, category_name, table_list){
  
  ns <- NS(id)
  
  tagList(
    h2(paste("By", category_name)),
    p(linebreaks(3L)),
    lapply(seq_along(table_list), function(i) {
      results_summary_table_UI(ns(names(table_list)[i]), names(table_list)[i])
    }),
  )
}

results_summary_category_Server <- function(id, table_list) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    lapply(seq_along(table_list), function(i) {
      results_summary_table_Server(names(table_list)[i], table_list[[i]])
    })
  })
}
