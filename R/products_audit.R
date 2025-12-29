productsAuditUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    h1("Orion Products Audit"),
    "Streamline a Sisyphusian audit.",
    br(),
    br(),
    shiny::fluidPage(
      column(
        width = 6,

        # Step 1
        h3("Step 1: Create product classification workbook"),
        "All Orion products ever owned. Query 10635 OR the Local Products export. File should be a CSV.",
        shiny::fileInput(
          inputId  = ns("orion_export"),
          label    = NULL,
          multiple = FALSE
        ),
        shiny::checkboxInput(
          inputId = ns("include_predictions"),
          label = "Include predictions",
          value = TRUE
        ),
        shiny::downloadButton(
          outputId = ns("class_wb"),
          label    = "Download Classification Workbook"
        )
      ),
      column(
        width = 6,

        # Step 2
        h3("Step 2: Create Orion import and update for Ops"),
        "Upload the classification workbook.",
        shiny::fileInput(
          inputId  = ns("classified_wb"),
          label    = NULL,
          multiple = FALSE
        ),
        "Upload products held in sleeved accounts. Orion query 39768. Broker Dealer ID(s) = 0. Direct download as XLSX.",
        shiny::fileInput(
          inputId  = ns("products_held"),
          label    = NULL,
          multiple = FALSE
        ),
        shiny::downloadButton(
          outputId = ns("orion_import"),
          label    = "Download Orion Import"
        ),
        shiny::downloadButton(
          outputId = ns("ops_update"),
          label    = "Download Ops Update"
        )
      )
    )
  )
}


productsAuditServer <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      detect_old_format <- function(df) {
        "AUM" %in% colnames(df)
      }

      transform_orion_data <- function(df) {
        if (detect_old_format(df)) {
          return(df)
        }

        df <- df |>
          dplyr::select(
            `Product ID`, `Product Name`, TICKER, cusip, Value, `Product Type`,
            `Product Sub Type`, CategoryName, RiskFullName, `Asset Class ID`, Description, IsAutoAssign
          )

        colnames(df) <- c(
          "Product ID", "Product Name", "Ticker", "CUSIP", "AUM", "Product Type", "Product Sub Type Name",
          "Product Category Name", "Risk Category Name", "Asset Class ID", "Asset Class Description", "Auto Assigned"
        )

        df |>
          dplyr::group_by(`Product ID`) |>
          dplyr::summarise(
            `Product Name` = dplyr::first(`Product Name`),
            Ticker = dplyr::first(Ticker),
            CUSIP = dplyr::first(CUSIP),
            AUM = sum(AUM, na.rm = TRUE),
            `Product Type` = dplyr::first(`Product Type`),
            `Product Sub Type Name` = dplyr::first(`Product Sub Type Name`),
            `Product Category Name` = dplyr::first(`Product Category Name`),
            `Risk Category Name` = dplyr::first(`Risk Category Name`),
            `Asset Class ID` = dplyr::first(`Asset Class ID`),
            `Asset Class Description` = dplyr::first(`Asset Class Description`),
            `Auto Assigned` = dplyr::first(`Auto Assigned`)
          ) |>
          dplyr::ungroup()
      }

      create_ops_update <- function(upload, sleeved_products, all_local) {
        cat("[DEBUG] Creating Ops update\n")
        cat("[DEBUG] Loading product classification framework from AWS\n")
        framework <- kdot::get_product_classification_framework()
        cat("[DEBUG] Product classification framework loaded successfully\n")

        result <- upload |>
          dplyr::select(`Product ID`, `Risk Category ID`) |>
          dplyr::left_join(
            all_local |> dplyr::select(`Product ID`, Ticker, CUSIP, `Risk Category Name`),
            by = "Product ID"
          ) |>
          dplyr::mutate(`Previous Risk Category` = `Risk Category Name`) |>
          dplyr::select(-`Risk Category Name`) |>
          dplyr::left_join(
            framework |> dplyr::select(`Risk Category ID`, `Risk Category`) |> dplyr::distinct(),
            by = "Risk Category ID"
          ) |>
          dplyr::mutate(`New Risk Category` = `Risk Category`) |>
          dplyr::select(-`Risk Category`, -`Risk Category ID`) |>
          dplyr::mutate(`Previous Risk Category` = stringr::str_replace_na(`Previous Risk Category`)) |>
          dplyr::filter(
            `New Risk Category` != `Previous Risk Category`,
            CUSIP %in% sleeved_products$CUSIP
          )

        print("fire 🔥")

        return(result)
      }

      # Read and transform Orion export
      take_export <- reactive({
        validate(need(input$orion_export, message = FALSE))
        input$orion_export$datapath |>
          readr::read_csv(show_col_types = FALSE) |>
          transform_orion_data()
      })

      # Download handler for classification workbook
      output$class_wb <- downloadHandler(
        filename = kdot::dated_filename("Orion Product Classifications", "xlsx"),
        content = function(file) {
          shiny::withProgress(message = "Creating classification workbook", value = 0, {
            shiny::incProgress(1 / 3, detail = "Reading data")
            data <- take_export()

            shiny::incProgress(1 / 3, detail = "Building workbook")
            wb <- kdot::create_classification_wb(data, include_predictions = input$include_predictions)

            shiny::incProgress(1 / 3, detail = "Saving file")
            openxlsx::saveWorkbook(wb = wb, file)
          })
        }
      )

      # Read classified workbook
      orion_import_xlsx <- reactive({
        validate(need(input$classified_wb, message = FALSE))
        input$classified_wb$datapath |> kdot::orion_product_import()
      })

      # Download handler for Orion import
      output$orion_import <- downloadHandler(
        filename = kdot::dated_filename("Orion Product Local Update Import", "xlsx"),
        content = function(file) {
          shiny::withProgress(message = "Creating Orion import", value = 0, {
            shiny::incProgress(1 / 2, detail = "Processing classifications")
            data <- orion_import_xlsx()

            shiny::incProgress(1 / 2, detail = "Saving file")
            openxlsx::write.xlsx(x = data, file)
            print(paste("Orion import:", nrow(data), "records"))
          })
        }
      )

      # Read products held in sleeved accounts
      read_held <- reactive({
        validate(need(input$products_held, message = FALSE))
        input$products_held$datapath |> openxlsx::read.xlsx()
      })

      # Download handler for ops update
      output$ops_update <- downloadHandler(
        filename = kdot::dated_filename("Updated Risk Categories", "xlsx"),
        content = function(file) {
          shiny::withProgress(message = "Creating Ops update", value = 0, {
            shiny::incProgress(1 / 4, detail = "Loading data")
            import_data <- orion_import_xlsx()
            held_data <- read_held()
            export_data <- take_export()

            shiny::incProgress(1 / 4, detail = "Comparing risk categories")
            update_data <- create_ops_update(import_data, held_data, export_data)

            shiny::incProgress(1 / 4, detail = "Saving file")
            openxlsx::write.xlsx(x = update_data, file)
            print(paste("Risk Category output:", nrow(update_data), "records"))

            shiny::incProgress(1 / 4, detail = "fire 🔥")
          })
        }
      )
    }
  )
}
