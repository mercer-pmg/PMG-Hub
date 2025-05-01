fixed_allocation <- function(x) {
  
  tibble::tibble(
    
    sleeve = c(x$fixed1,
               x$fixed2,
               x$fixed3,
               x$fixed4,
               x$fixed5,
               x$fixed6,
               x$fixed7,
               x$fixed8,
               x$fixed9,
               x$fixed10),
    
    weight = c(x$fixed_weight1,
               x$fixed_weight2,
               x$fixed_weight3,
               x$fixed_weight4,
               x$fixed_weight5,
               x$fixed_weight6,
               x$fixed_weight7,
               x$fixed_weight8,
               x$fixed_weight9,
               x$fixed_weight10)
  ) |>
    dplyr::filter(sleeve != "") |>
    dplyr::filter(weight > 0)
}