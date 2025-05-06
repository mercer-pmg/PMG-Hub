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
        userFile()$datapath |> 
          purrr::map(readr::read_csv, show_col_types = FALSE) |> 
          dplyr::bind_rows()
      })
      
      output$download <- downloadHandler(
        
        filename = "blended_strategy_bundle.xlsx",
        
        content = function(file) {
          openxlsx::write.xlsx(x = bundle(), file)
          
        }
      )
      
      return(bundle)
      
    }
  )
}