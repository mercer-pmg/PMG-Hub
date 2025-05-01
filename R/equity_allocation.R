equity_allocation <- function(x){
  
  tibble::tibble(
    
    sleeve = c(x$equity1,
               x$equity2,
               x$equity3,
               x$equity4,
               x$equity5,
               x$equity6,
               x$equity7,
               x$equity8,
               x$equity9,
               x$equity10),
    
    weight = c(x$equity_weight1,
               x$equity_weight2,
               x$equity_weight3,
               x$equity_weight4,
               x$equity_weight5,
               x$equity_weight6,
               x$equity_weight7,
               x$equity_weight8,
               x$equity_weight9,
               x$equity_weight10)
  ) |>
    dplyr::filter(sleeve != "") |>
    dplyr::filter(weight > 0)
  
}
  