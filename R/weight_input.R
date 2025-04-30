weight_input <- function(id){
  
  shiny::numericInput(
    inputId   = id, 
    label     = NULL, 
    value     = 0,
    min       = 0,
    max       = 100,
    step      = 1,
    width     = "100%")
  
}