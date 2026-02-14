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
      ),
      column(
        width = 12,

        # Step 3
        h3("Step 3: Check for delisted securities"),
        "Upload Bloomberg delisted assets list as CSV (headers in row 4, data starts row 6). Compares against tickers from the query 10635 file (Step 1).",
        shiny::fileInput(
          inputId  = ns("delisted_csv"),
          label    = NULL,
          multiple = FALSE
        ),
        shiny::actionButton(
          inputId = ns("delisted_submit"),
          label   = "Check for delisted securities"
        ),
        shiny::downloadButton(
          outputId = ns("do_not_buy_sell_import"),
          label    = "Download Do Not Buy/Sell Import (XLSX)"
        ),
        shiny::div(style = "margin-top: 1em;"),
        shiny::uiOutput(outputId = ns("delisted_status")),
        shiny::tableOutput(outputId = ns("delisted_table"))
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

      # Step 3: Delisted securities check - runs only on submit
      delisted_submit_clicked <- shiny::reactiveVal(FALSE)
      delisted_is_loading <- shiny::reactiveVal(FALSE)
      delisted_result_store <- shiny::reactiveVal(NULL)

      shiny::observeEvent(input$delisted_submit, {
        delisted_submit_clicked(TRUE)
        delisted_is_loading(TRUE)
        result <- tryCatch({
          shiny::validate(
            shiny::need(input$orion_export, "Upload the query 10635 file (Step 1)."),
            shiny::need(input$delisted_csv, "Upload the Bloomberg delisted CSV.")
          )
          exp <- take_export()
          ticker_col <- if ("Ticker" %in% names(exp)) "Ticker" else "TICKER"
          product_tickers <- unique(exp[[ticker_col]])
          if (is.null(product_tickers)) product_tickers <- character(0)
          out <- kdot::check_delisted_products(product_tickers, input$delisted_csv$datapath)
          aum_by_ticker <- exp |>
            dplyr::group_by(.data[[ticker_col]]) |>
            dplyr::summarise(AUM = sum(AUM, na.rm = TRUE), .groups = "drop")
          out |>
            dplyr::left_join(aum_by_ticker, by = c("cleaned_ticker" = ticker_col)) |>
            dplyr::relocate(AUM, .after = cleaned_ticker)
        }, error = function(e) {
          delisted_is_loading(FALSE)
          stop(e)
        })
        delisted_result_store(result)
        delisted_is_loading(FALSE)
      })

      delisted_check_result <- shiny::reactive({
        shiny::req(delisted_result_store())
      })

      output$delisted_status <- shiny::renderUI({
        if (!delisted_submit_clicked()) {
          return(shiny::p("Upload the query 10635 and Bloomberg delisted CSV, then click Check to run."))
        }
        if (delisted_is_loading()) {
          return(shiny::div(
            shiny::icon("spinner", class = "fa-spin fa-fw"),
            " Processing..."
          ))
        }
        NULL
      })

      output$delisted_table <- shiny::renderTable({
        delisted_check_result()
      })

      output$do_not_buy_sell_import <- shiny::downloadHandler(
        filename = kdot::dated_filename("Do Not Buy Sell Import", "xlsx"),
        content = function(file) {
          shiny::validate(shiny::need(delisted_result_store(), "Run the delisted check first."))
          shiny::withProgress(message = "Creating import file", value = 0, {
            shiny::incProgress(0.5, detail = "Building workbook")
            result <- delisted_result_store()
            trading_import <- tibble::tibble(
              `Entity ID` = "",
              Level = "Firm",
              Ticker = result$cleaned_ticker,
              `Sell Priority` = "Do Not Sell",
              `Buy Priority` = "Do Not Buy",
              Excluded = ""
            )
            shiny::incProgress(0.5, detail = "Saving file")
            openxlsx::write.xlsx(trading_import, file, sheetName = "Sheet1")
          })
        }
      )
    }
  )
}
