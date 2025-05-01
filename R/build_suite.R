crack_name <- function(suite_name, w){
  
  is_tm <- stringr::str_detect(suite_name, " TM ")
  
  if(is_tm){suite_name <- stringr::str_remove(suite_name, " TM")}
  
  bar <- stringr::str_split(suite_name, "\\(", simplify = TRUE) |> as.character()
  
  bar[2] <- paste0("(", bar[2])
  
  bar <- bar |> stringr::str_trim()
  
  if(is_tm){
    bar <- paste(bar[1], w, "TM", bar[2])
    
  } else {
    bar <- paste(bar[1], w, bar[2])
  }
  
  
  
  return(bar)
  
}

build_suite <- function(x,y,w, suite_name, ids = platform){
  
  w <- as.numeric(w)
  
  ids <- ids |> dplyr::select(model_agg, model_agg_id) |> dplyr::distinct()
  
  equity <- x |> 
    dplyr::mutate(weight = weight * w/100) |>
    dplyr::left_join(ids, by = dplyr::join_by("sleeve" == "model_agg"))
  
  fixed  <- y |> 
    dplyr::mutate(weight = weight *(100-w)/100) |>
    dplyr::left_join(ids, by = dplyr::join_by(sleeve == model_agg))
  
  sleeve_strategy <- dplyr::bind_rows(equity, fixed) |>
    dplyr::mutate(
      Name                    = crack_name(suite_name, w),
      `Model Agg`             = sleeve,
      `Model Agg Id`          = model_agg_id,
      `Target Allocation`     = weight,
      `Excluded Rebal Sleeve` = FALSE
      
    ) |>
    dplyr::filter(`Target Allocation` != 0) |>
    dplyr::select(Name, `Model Agg`, `Model Agg Id`, `Target Allocation`, `Excluded Rebal Sleeve`)
  
  admin_sleeves <- tibble::tibble(
    Name                    = crack_name(suite_name, w),
    `Model Agg`             = c("Income Sweep", 
                                "Unsupervised", 
                                "UnAssigned", 
                                "Transitional"),
    `Model Agg Id`          = c(69140, 69269, 69277, 69278),
    `Target Allocation`     = 0,
    `Excluded Rebal Sleeve` = TRUE
  )
  
  sleeve_strategy <- dplyr::bind_rows(sleeve_strategy, admin_sleeves) |>
    dplyr::mutate(
      `Sleeve Strategy Id`              = "<new>",
      `Contribution Allocation Method`  = "Most Out Of Balance",
      `Distribution Allocation Method`  = "Most Out Of Balance",
      `Auto Rebal Frequency`            = "None",
      `Tolerance Percent`               = 10,
      `Strategy Detail Id`              = "<new>",
      `Sleeve Type`                     = "Normal",
      `Tolerance Upper`                 = `Target Allocation`/`Tolerance Percent`,
      `Tolerance Lower`                 = `Tolerance Upper`,
      `Auto Rebal Month`                = NA,
      `Auto Rebal Day`                  = NA,
      `Entity Id`                       = NA,
      Entity                            = NA,
      `Management Style`                = NA,
      `Subadvisor Id`                   = NA,
      `Contribution Allocation`         = NA,
      `Distribution Allocation`         = NA, 
      `Auto Trade Types`                = NA,
      `Substitute Detail Id`            = NA,
      `Substitute Id`                   = NA,
      `Asset Level Allowed`             = NA,
      `Allowed Asset Classification ID` = NA
    ) 
  
  
  return(sleeve_strategy)
  
  
}