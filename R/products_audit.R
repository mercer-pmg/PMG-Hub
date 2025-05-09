productsAuditUI <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    h1("Orion Products Audit"),
    
    "Streamline a Sisyphusian audit.",
    
    br(),
    br(),
    
    shiny::fluidPage(
      column(width = 6, 
             
             
             # Step 1
             h3("Step 1: Create classification workbook"),
             
             "Upload the Orion Local Products export. File should be XLSX.",
             shiny::fileInput(
               inputId  = ns("orion_export"), 
               label    = NULL,
               multiple = FALSE),
             
             "Download the classification workbook.",
             br(),
             shiny::downloadButton(ns("class_wb")),
             
             br(),
             br(),
             br(),
             br(),
             

             # Step 2
             h3("Step 2: Create Orion Product Local Update import."),
             
             "Upload the classification workbook.",
             shiny::fileInput(
               inputId  = ns("classified_wb"), 
               label    = NULL,
               multiple = FALSE),
             
             "Download Orion Product Local Update import.",
             br(),
             shiny::downloadButton(ns("orion_import")),
             
             br(),
             br(),
             br(),
             br(),
             

             # Step 3
             h3("Step 3: Create table of updated risk categories for Ops."),
             
             "Upload products held in sleeved accounts. Orion query 39768. Broker Dealer ID(s) = 0. Direct download as XLSX.",
             shiny::fileInput(
               inputId  = ns("products_held"), 
               label    = NULL,
               multiple = FALSE),
             
             "Download the relevant risk category updates for Investment Operations.", 
             br(),
             shiny::downloadButton(ns("ops_update"))
             
      )
    )
  )
}



productsAuditServer <- function(id) {
  
  shiny::moduleServer(
    id,
    
    function(input, output, session) {
      
      # Classification workbook
      userFile <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$orion_export, message = FALSE))
        input$orion_export
      })
      
      take_export <- reactive({
        userFile()$datapath |> readr::read_csv() 
      })
      
      output$class_wb <- downloadHandler(
        
        filename = "foo.xlsx",
        
        content = function(file) {
          openxlsx::write.xlsx(x = take_export(), file)
          
        }
      )
      
      # Orion import
      userFile2 <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$classified_wb, message = FALSE))
        input$classified_wb
      })
      
      take_classified_wb <- reactive({
        userFile2()$datapath |> openxlsx::read.xlsx() 
      })
      
      output$orion_import <- downloadHandler(
        
        filename = "ABC123.csv",
        
        content = function(file) {
          readr::write_csv(x = take_classified_wb(), file)
          
        }
      )
      
      # Ops update
      userFile3 <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$products_held, message = FALSE))
        input$products_held
      })
      
      read_held <- reactive({
        userFile3()$datapath |> openxlsx::read.xlsx() 
      })
      
      output$ops_update <- downloadHandler(
        
        filename = "Ops Update.xlsx",
        
        content = function(file) {
          openxlsx::write.xlsx(x = list(take_classified_wb(), read_held()), file)
          
        }
      )
      
    }
  )
  
}