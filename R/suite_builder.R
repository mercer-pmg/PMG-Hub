
suite_builderUI <- function(id, platform) {
  
  # Helper functions for module UI
  ns <- shiny::NS(id)
  
  sleeve_input <- function(inputID, df){
    
    sleeves <- df |>
      dplyr::arrange(model_agg) |>
      dplyr::pull(model_agg) |>
      unique()
    
    shiny::selectInput(
      inputId   = ns(inputID),
      label     = NULL,
      choices   = c("Choose Sleeve" = "", sleeves),
      selectize = TRUE,
      width     = "100%")
    
  }
  
  weight_input <- function(inputID){
    
    shiny::numericInput(
      inputId   = ns(inputID), 
      label     = NULL, 
      value     = 0,
      min       = 0,
      max       = 100,
      step      = 1,
      width     = "100%")
    
  }
  
  # Module UI
  shiny::tagList(
    h1("Suite Builder"),
    
    "Define equity and fixed income allocations. Generate a suite of sleeve strategies.",
    
    br(),
    br(),
    
    fluidRow(
      
      column(
        width = 4,
        shiny::textInput(inputId = ns("suite"), label = "Suite Name")),
      
      column(
        width = 6,
        shiny::checkboxGroupInput(
          inputId  = ns("strategies"), 
          label    = "Strategies to Build",
          inline   = TRUE,
          choices  = seq(from = 100, to = 10, by = -10),
          selected = seq(from = 100, to = 10, by = -10)))
      
    ),
    
    fluidRow(
      
      column(
        width = 6, 
        h3("Equity Sleeves"),
        
        column(
          width = 9, 
          paste0("equity", 1:7) |> purrr::map(sleeve_input, df = platform)),
        
        column(
          width = 3, 
          paste0("equity_weight", 1:7) |> purrr::map(weight_input))
        
      ),
      
      column(
        width = 6, 
        h3("Fixed Income Sleeves"),
        
        column(
          width = 9,
          paste0("fixed", 1:7) |> purrr::map(sleeve_input, df = platform)
          
        ),
        
        column(
          width = 3,
          paste0("fixed_weight", 1:7) |> purrr::map(weight_input)
          
        )
      )
    ),
    
    br(),
    br(),
    
    fluidRow(
      column(
        width = 2, 
        shiny::downloadButton(ns("downloadSuite")))
    )
  )
  
  
  
}

suite_builderServer <- function(id, platform) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    equity_allocation <- reactive(
      tibble::tibble(
        
        sleeve = c(
          input$equity1, 
          input$equity2,
          input$equity3,
          input$equity4,
          input$equity5,
          input$equity6,
          input$equity7),
        
        weight = c(
          input$equity_weight1, 
          input$equity_weight2,
          input$equity_weight3,
          input$equity_weight4,
          input$equity_weight5,
          input$equity_weight6,
          input$equity_weight7)
        
      ) |>
        
        dplyr::filter(sleeve != "") |>
        dplyr::filter(weight > 0)
    )
    
    fixed_allocation <- reactive(
      tibble::tibble(
        
        sleeve = c(
          input$fixed1, 
          input$fixed2,
          input$fixed3,
          input$fixed4,
          input$fixed5,
          input$fixed6,
          input$fixed7),
        
        weight = c(
          input$fixed_weight1, 
          input$fixed_weight2,
          input$fixed_weight3,
          input$fixed_weight4,
          input$fixed_weight5,
          input$fixed_weight6,
          input$fixed_weight7)
        
      ) |>
        
        dplyr::filter(sleeve != "") |>
        dplyr::filter(weight > 0)
    )
    
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
    
    output$downloadSuite <- downloadHandler(
      
      filename = function() {
        paste0(input$suite, ".xlsx")
      },
      
      content  = function(file) {
        openxlsx::write.xlsx(
          x = input$strategies |>
            purrr::map_dfr(
              .f         = build_suite,
              x          = equity_allocation(),
              y          = fixed_allocation(),
              suite_name = input$suite,
              ids        = platform),
          
          file)}
    )
  }
  )
}