bs_bundlerUI <- function(id){
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    h1("Blended Strategy Bundler"),
    
    "Select blended strategies to bundle. Bundle them.",
    
    br(),
    br(),
    
    shiny::fileInput(
      inputId  = ns("blended_strategies"), 
      label    = "Select Blended Strategies to Bundle",
      multiple = TRUE),
    
    shiny::downloadButton(ns("download"))
    
  )
  
  
  
}


bs_bundlerServer <- function(id){
  
  moduleServer(
    id,
    
    function(input, output, session) {
      
      userFile <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$blended_strategies, message = FALSE))
        input$blended_strategies
      })
      
      bundle <- reactive({
        
        dat <- userFile()$datapath |> 
          purrr::map(readr::read_csv, show_col_types = FALSE) |> 
          dplyr::bind_rows()
        
        test_distinct <- function(x, df = dat){
          
          y <- df |> dplyr::filter(Name == x) |> dplyr::filter(`Target Allocation` != 0)
          n <- y |> nrow()
          
          matches <- df |> dplyr::filter(Name != x) |> dplyr::select(Name) |> dplyr::distinct()
          
          for(i in 1:n){
            foo <- df |>
              dplyr::filter(Modelagg == y$Modelagg[i] & `Target Allocation` == y$`Target Allocation`[i]) |>
              dplyr::pull(Name)
            
            matches <- matches |> dplyr::filter(Name %in% foo)
            
          }
          
          if(nrow(matches) > 0){
            return(FALSE)
          } else {
            return(TRUE)
          }
        }
        
        distinct_strategies <- dat |> dplyr::select(Name) |> dplyr::distinct()
        
        distinct_strategies <- distinct_strategies |>
          dplyr::rowwise() |>
          dplyr::mutate(is_distinct = Name |> test_distinct()) |>
          dplyr::filter(!is_distinct)
        
        if(nrow(distinct_strategies) > 0){
          
          distinct_strategies
               
        } else {
          
          dat
        }
        
      })
          
          output$download <- downloadHandler(
            
            filename = kdot::dated_filename("Blended Strategy Bundle", "xlsx"),
            
            content = function(file) {
              openxlsx::write.xlsx(x = bundle(), file)
              
            }
          )
          
          return(bundle)
          
    }
      )
}