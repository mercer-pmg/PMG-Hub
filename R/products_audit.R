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
  
  # ## Create workbook----
  # create_classification_wb <- function(all_local) {
  #   
  #   reviewed   <- all_local |> dplyr::filter(!`Auto Assigned`) |> dplyr::mutate(`Assigned Asset Class` = NA)
  #   unreviewed <- all_local |> dplyr::filter(`Auto Assigned`)
  #   unreviewed_aum <- unreviewed |> dplyr::filter(AUM != 0)
  #   
  #   # Remove CUSIPs with underscore, dash
  #   unreviewed_aum <- unreviewed_aum |>
  #     dplyr::filter(!stringr::str_detect(CUSIP, "_")) |>
  #     dplyr::filter(!stringr::str_detect(CUSIP, "-"))
  #   
  #   dat <- unreviewed_aum
  #   
  #   dat <- dat |>
  #     dplyr::mutate(
  #       `Product Sub Type Name` = `Product Sub Type Name` |> stringr::str_remove_all("/"),
  #       `Assigned Asset Class`  = NA)
  #   
  #   keys_n <- dat |>
  #     dplyr::group_by(`Product Sub Type Name`) |>
  #     dplyr::summarise(n = dplyr::n()) |>
  #     dplyr::arrange(dplyr::desc(n)) |>
  #     dplyr::mutate(
  #       `Product Sub Type Name` = `Product Sub Type Name` |>
  #         stringr::str_replace_na()
  #     )
  #   
  #   dat <- dat |> dplyr::group_by(`Product Sub Type Name`)
  #   keys <- dplyr::group_keys(dat) |> dplyr::pull() |> stringr::str_replace_na()
  #   
  #   dat <- dat |> dplyr::group_split()
  #   dat <- dat |> purrr::map(as.data.frame)
  #   names(dat) <- keys
  #   
  #   dat[["Mutual Fund"]] <- dat[["Mutual Fund"]] |>
  #     dplyr::mutate(Segment = NA)
  #   
  #   wb <- openxlsx::createWorkbook()
  #   
  #   openxlsx::addWorksheet(wb, "Status")
  #   openxlsx::writeData(wb, "Status", keys_n)
  #   
  #   for(i in 1:length(dat)) {
  #     openxlsx::addWorksheet(wb, names(dat)[i])
  #     openxlsx::writeDataTable(wb, names(dat)[i], dat[[i]])
  #   }
  #   
  #   openxlsx::addWorksheet(wb, "Classified")
  #   openxlsx::writeDataTable(wb, "Classified", reviewed)
  #   
  #   # Add asset classes
  #   framework <- readr::read_csv("MA Product Classification Framework.csv")
  #   
  #   openxlsx::addWorksheet(wb, "Asset Classes", visible = FALSE)
  #   openxlsx::writeData(wb, "Asset Classes", x = framework)
  #   
  #   add_classes <- function(x){
  #     
  #     openxlsx::dataValidation(
  #       wb    = wb,
  #       sheet = x,
  #       cols  = 13,
  #       rows  = 2:(keys_n$n[i]+1),
  #       type  = "list",
  #       value = paste0("'Asset Classes'!$D$2:$D$", (nrow(framework)+1)))
  #     
  #     openxlsx::addStyle(
  #       wb    = wb,
  #       sheet = x,
  #       style = openxlsx::createStyle(fgFill = "#C5D9F1"),
  #       cols  = 13,
  #       rows  = 2:(keys_n$n[i]+1))
  #     
  #   }
  #   
  #   for(i in 1:length(keys)){
  #     add_classes(keys_n$`Product Sub Type Name`[i])
  #   }
  #   
  #   openxlsx::dataValidation(
  #     wb = wb,
  #     sheet = "Classified",
  #     cols = 13,
  #     rows = 2:(nrow(reviewed)+1),
  #     type = "list",
  #     value = paste0("'Asset Classes'!$D$2:$D$", (nrow(framework)+1))
  #   )
  #   
  #   openxlsx::addStyle(
  #     wb    = wb,
  #     sheet = "Classified",
  #     style = openxlsx::createStyle(fgFill = "#C5D9F1"),
  #     cols  = 13,
  #     rows  = 2:(nrow(reviewed)+1)
  #   )
  #   
  #   
  #   # Add FactSet functions
  #   openxlsx::writeFormula(
  #     wb       = wb,
  #     sheet    = "Mutual Fund",
  #     x        = paste0('FDS(D', 2:(nrow(dat[["Mutual Fund"]])+1),',"FFD_SEG")'),
  #     startRow = 2,
  #     startCol = 14
  #   )
  #   
  #   return(wb)
  #   
  #   
  # }
  
  
  
  
  create_orion_import <- function(sheet_with_assignments) {
    
    framework <- readr::read_csv("MA Product Classification Framework.csv")
    
    sheet_names <- openxlsx::getSheetNames(sheet_with_assignments)
    bad_names   <- c("__FDSCACHE__","Asset Classes", "Status")
    sheet_names <- sheet_names[!sheet_names %in% bad_names]
    
    get_assignments <- function(sheet_name){
      
      print(sheet_name)
      
      dat <- openxlsx::read.xlsx(
        xlsxFile  = sheet_with_assignments,
        sheet     = sheet_name,
        sep.names = " ") |>
        dplyr::select(`Product ID`, CUSIP, `Assigned Asset Class`) |>
        dplyr::mutate(`Assigned Asset Class` = as.character(`Assigned Asset Class`))
      
      return(dat)
      
    }
    
    upload <- sheet_names |> purrr::map_df(get_assignments)
    
    upload <- upload |>
      dplyr::filter(!is.na(`Assigned Asset Class`)) |>
      dplyr::mutate(`Asset Class` = `Assigned Asset Class`) |>
      dplyr::select(-`Assigned Asset Class`)
    
    upload <- dplyr::left_join(
      x  = upload,
      y  = framework,
      by = "Asset Class")
    
    upload <- upload |>
      dplyr::select(`Product ID`, `Risk Category ID`, `Asset Class ID`)
    
    upload <- upload |>  
      dplyr::mutate(
        `Product Name Override`  = NA,
        Color                    = NA,
        `Is Auto Assigned`       = FALSE,
        `S&P Bond Rating`        = NA,
        `Moody Bond Rating`      = NA,
        `Product Status`         = NA,
        `Is Disabled`            = NA,
        `Is Custodial Cash`      = NA,
        `Is Managed`             = NA,
        `Use Global Tax Setting` = NA,
        `Federally Taxable`      = NA,
        `State Taxable`          = NA,
        `Annual Income Rate`     = NA,
        `ADV Asset Category`     = NA,
        `Is ADV Reportable`      = NA,
        `Is 13F Reportable`      = NA,
        `Has Fees`               = NA)
    
    upload <- upload |>
      dplyr::select(
        `Product ID`,
        `Product Name Override`,
        `Asset Class ID`,
        `Risk Category ID`,
        `Color`,
        `Is Auto Assigned`,
        `S&P Bond Rating`,
        `Moody Bond Rating`,
        `Product Status`,
        `Is Disabled`,
        `Is Custodial Cash`,
        `Is Managed`,
        `Use Global Tax Setting`,
        `Federally Taxable`,
        `State Taxable`,
        `Annual Income Rate`,
        `ADV Asset Category`,
        `Is ADV Reportable`,
        `Is 13F Reportable`,
        `Has Fees`)
    
    return(upload)
    
    
  }
  
  
  
  
  
  
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
        
        filename = paste0("Orion Product Classifications - ", format(Sys.Date(), "%Y.%m.%d"), ".xlsx"),
        
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
      
      take_classified_wb <- reactive({
        userFile2()$datapath |> openxlsx::read.xlsx() 
      })
      
      output$orion_import <- downloadHandler(
        
        filename = paste0("Orion Product Local Update Import - ", format(Sys.Date(), "%Y.%m.%d"), ".xlsx"),
        
        content = function(file) {
          openxlsx::write.xlsx(
            x = userFile2()$datapath |> create_orion_import(), 
            file
          )
        }
      )
      
      # Ops update
      create_ops_update <- function(x,y,z) {
        
        framework <- readr::read_csv("MA Product Classification Framework.csv")
        
        
        upload <- x |> create_orion_import()
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
        
        filename = paste0("Updated Risk Categories - ", format(Sys.Date(), "%Y.%m.%d"), ".xlsx"),
        
        content = function(file) {
          openxlsx::write.xlsx(
            x = create_ops_update(userFile2()$datapath, userFile3()$datapath, userFile4()$datapath), 
            file)
          
        }
      )
      
    }
  )
  
}