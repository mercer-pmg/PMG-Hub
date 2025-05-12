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
             h3("Step 1: Create product classification workbook"),
             
             "Upload the Orion Local Products export. File should be a CSV.",
             shiny::fileInput(
               inputId  = ns("orion_export"), 
               label    = NULL,
               multiple = FALSE),
             
             shiny::downloadButton(
               outputId = ns("class_wb"),
               label    = "Download Classification Workbook")
      ),
      
      column(width = 6,     
             
             # Step 2
             h3("Step 2: Create Orion import and update for Ops"),
             
             "Upload the Orion Local Products export. File should be a CSV.",
             shiny::fileInput(
               inputId  = ns("orion_export2"), 
               label    = NULL,
               multiple = FALSE),
             
             "Upload the classification workbook.",
             shiny::fileInput(
               inputId  = ns("classified_wb"), 
               label    = NULL,
               multiple = FALSE),
             
             "Upload products held in sleeved accounts. Orion query 39768. Broker Dealer ID(s) = 0. Direct download as XLSX.",
             shiny::fileInput(
               inputId  = ns("products_held"), 
               label    = NULL,
               multiple = FALSE),
             
             shiny::downloadButton(
               outputId = ns("orion_import"),
               label    = "Download Orion Import"),
             
             shiny::downloadButton(
               outputId = ns("ops_update"),
               label    = "Download Ops Update")
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
        
        filename = kdot::dated_filename("Orion Product Classifications", "xlsx"),
        
        content = function(file) {
          openxlsx::saveWorkbook(
            wb = take_export() |> kdot::create_classification_wb(), 
            file)
          
        }
      )
      
      # Orion import
      userFile2 <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$classified_wb, message = FALSE))
        input$classified_wb
      })
      
      output$orion_import <- downloadHandler(
        
        filename = kdot::dated_filename("Orion Product Local Update Import", "xlsx"),

        content = function(file) {
          openxlsx::write.xlsx(
            x = userFile2()$datapath |> kdot::orion_product_import(), 
            file
          )
        }
      )
      
      # Ops update
      create_ops_update <- function(x,y,z) {
        
        framework <- readr::read_csv("MA Product Classification Framework.csv")
        
        upload <- x |> kdot::orion_product_import()
        sleeved_products <- y |> openxlsx::read.xlsx()
        all_local <- z |> readr::read_csv()
        
        port_ops <- dplyr::left_join(
          x  = upload |> dplyr::select(`Product ID`, `Risk Category ID`),
          y  = all_local |> dplyr::select(`Product ID`, Ticker, CUSIP, `Risk Category Name`),
          by = "Product ID")
        
        port_ops <- port_ops |>
          dplyr::mutate(`Previous Risk Category` = `Risk Category Name`) |>
          dplyr::select(-`Risk Category Name`)
        
        port_ops <- dplyr::left_join(
          x = port_ops,
          y = framework |> dplyr::select(`Risk Category ID`, `Risk Category`) |> dplyr::distinct(),
          by = "Risk Category ID")
        
        port_ops <- port_ops |>
          dplyr::mutate(`New Risk Category` = `Risk Category`) |>
          dplyr::select(-`Risk Category`, -`Risk Category ID`)
        
        port_ops <- port_ops |> dplyr::filter(`New Risk Category` != `Previous Risk Category`)
        
        port_ops <- port_ops |>
          dplyr::filter(CUSIP %in% sleeved_products$CUSIP)
        
        return(port_ops)
      }
      
      
      userFile4 <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$orion_export2, message = FALSE))
        input$orion_export2
      })
      
      
      userFile3 <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$products_held, message = FALSE))
        input$products_held
      })
      
      read_held <- reactive({
        userFile3()$datapath |> openxlsx::read.xlsx() 
      })
      
      output$ops_update <- downloadHandler(
        
        filename = kdot::dated_filename("Updated Risk Categories", "xlsx"),

        content = function(file) {
          openxlsx::write.xlsx(
            x = create_ops_update(userFile2()$datapath, userFile3()$datapath, userFile4()$datapath), 
            file)
          
        }
      )
    }
  )
}