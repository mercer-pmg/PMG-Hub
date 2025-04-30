sleeve_input <- function(id, df){
  
  sleeves <- df |>
    dplyr::arrange(model_agg) |>
    dplyr::pull(model_agg) |>
    unique()
  
  shiny::selectInput(
    inputId   = id, 
    label     = NULL, 
    choices   = c("Choose Sleeve" = "", sleeves),
    selectize = TRUE,
    width     = "100%")
  
}