# Output paths for auto-save. Set CLASSIFICATION_WB, ORION_IMPORT, OPS_UPDATE in .Renviron.
products_audit_paths <- list(
  classification_wb = Sys.getenv("CLASSIFICATION_WB"),
  orion_import = Sys.getenv("ORION_IMPORT"),
  ops_update = Sys.getenv("OPS_UPDATE")
)

# Find most recent file in folder (by modification time)
find_most_recent_in_dir <- function(dir, pattern) {
  if (!dir.exists(dir)) {
    return(NULL)
  }
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0L) {
    return(NULL)
  }
  info <- file.info(files)
  files[which.max(info$mtime)]
}

find_most_recent_class_wb <- function() {
  find_most_recent_in_dir(
    products_audit_paths$classification_wb,
    "Orion Product Classifications - .*\\.xlsx$"
  )
}

find_most_recent_orion_import <- function() {
  find_most_recent_in_dir(
    products_audit_paths$orion_import,
    "Orion Product Local Update Import - .*\\.xlsx$"
  )
}

find_most_recent_ops_update <- function() {
  find_most_recent_in_dir(
    products_audit_paths$ops_update,
    "Updated Risk Categories - .*\\.xlsx$"
  )
}

productsAuditUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$script(shiny::HTML(
      "
      Shiny.addCustomMessageHandler('copyToClipboard', function(text) {
        if (navigator.clipboard && navigator.clipboard.writeText) {
          navigator.clipboard.writeText(text).then(function() {
            Shiny.setInputValue('clipboard_copied', Math.random(), {priority: 'event'});
          });
        }
      });
    "
    )),
    h1("Orion Products Audit"),
    "Streamline a Sisyphusian audit.",
    br(),
    shiny::checkboxInput(
      inputId = ns("auto_save"),
      label = "Auto-save to configured folders",
      value = TRUE
    ),
    br(),
    shiny::fluidPage(
      column(
        width = 6,

        # Step 1
        h3("Step 1: Create product classification workbook"),
        "All Orion products ever owned. Query 10635 OR the Local Products export. File should be a CSV.",
        shiny::fileInput(
          inputId = ns("orion_export"),
          label = NULL,
          multiple = FALSE
        ),
        shiny::checkboxInput(
          inputId = ns("include_predictions"),
          label = "Include predictions",
          value = TRUE
        ),
        shiny::fluidRow(
          shiny::column(width = 6, shiny::uiOutput(ns("class_wb_button"))),
          shiny::column(
            width = 6,
            shiny::actionButton(
              inputId = ns("open_most_recent_class_wb"),
              label = "Open Most Recent"
            )
          )
        )
      ),
      column(
        width = 6,

        # Step 2
        h3("Step 2: Create Orion import and update for Ops"),
        "Upload the filled-out classification workbook.",
        shiny::fluidRow(
          shiny::column(
            8,
            shiny::fileInput(
              inputId = ns("classified_wb"),
              label = NULL,
              multiple = FALSE
            ),
            shiny::uiOutput(ns("classified_wb_loaded_status"))
          ),
          shiny::column(
            4,
            shiny::actionButton(
              inputId = ns("load_most_recent_class_wb"),
              label = "Load Most Recent"
            )
          )
        ),
        "Upload products held in sleeved accounts. Query 39768. Broker Dealer ID(s) = 0. CSV.",
        shiny::fileInput(
          inputId = ns("products_held"),
          label = NULL,
          multiple = FALSE,
          accept = ".csv"
        ),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::uiOutput(ns("orion_import_button")),
            shiny::actionButton(
              inputId = ns("open_most_recent_orion_import"),
              label = "Open Orion Import",
              style = "margin-top: 4px;"
            )
          ),
          shiny::column(
            6,
            shiny::uiOutput(ns("ops_update_button")),
            shiny::actionButton(
              inputId = ns("open_most_recent_ops_update"),
              label = "Open Ops Update",
              style = "margin-top: 4px;"
            )
          )
        ),
        shiny::uiOutput(ns("orion_import_filename"))
      ),
      column(
        width = 12,

        # Step 3
        h3("Step 3: Check for delisted securities"),
        "Upload Bloomberg delisted assets list as CSV (headers in row 4, data starts row 6). Compares against tickers from the query 10635 file (Step 1).",
        shiny::fileInput(
          inputId = ns("delisted_csv"),
          label = NULL,
          multiple = FALSE
        ),
        shiny::actionButton(
          inputId = ns("delisted_submit"),
          label = "Check for delisted securities"
        ),
        shiny::downloadButton(
          outputId = ns("do_not_buy_sell_import"),
          label = "Download Do Not Buy/Sell Import (XLSX)"
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
      ns <- session$ns

      detect_old_format <- function(df) {
        "AUM" %in% colnames(df)
      }

      transform_orion_data <- function(df) {
        if (detect_old_format(df)) {
          return(df)
        }

        df <- df |>
          dplyr::select(
            `Product ID`,
            `Product Name`,
            TICKER,
            cusip,
            Value,
            `Product Type`,
            `Product Sub Type`,
            CategoryName,
            RiskFullName,
            `Asset Class ID`,
            Description,
            IsAutoAssign
          )

        colnames(df) <- c(
          "Product ID",
          "Product Name",
          "Ticker",
          "CUSIP",
          "AUM",
          "Product Type",
          "Product Sub Type Name",
          "Product Category Name",
          "Risk Category Name",
          "Asset Class ID",
          "Asset Class Description",
          "Auto Assigned"
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
        cat("[DEBUG] Loading product classification framework from menu data\n")
        framework <- readr::read_csv(
          file = file.path("data", "MA Product Classification Framework.csv"),
          show_col_types = FALSE
        )
        cat("[DEBUG] Product classification framework loaded successfully\n")

        result <- upload |>
          dplyr::select(`Product ID`, `Risk Category ID`) |>
          dplyr::left_join(
            all_local |>
              dplyr::select(`Product ID`, Ticker, CUSIP, `Risk Category Name`),
            by = "Product ID"
          ) |>
          dplyr::mutate(`Previous Risk Category` = `Risk Category Name`) |>
          dplyr::select(-`Risk Category Name`) |>
          dplyr::left_join(
            framework |>
              dplyr::select(`Risk Category ID`, `Risk Category`) |>
              dplyr::distinct(),
            by = "Risk Category ID"
          ) |>
          dplyr::mutate(`New Risk Category` = `Risk Category`) |>
          dplyr::select(-`Risk Category`, -`Risk Category ID`) |>
          dplyr::mutate(
            `Previous Risk Category` = stringr::str_replace_na(
              `Previous Risk Category`
            )
          ) |>
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

      # Filename helpers for copy-to-clipboard display
      class_wb_fn <- shiny::reactive(kdot::dated_filename(
        "Orion Product Classifications",
        "xlsx"
      ))
      orion_import_fn <- shiny::reactive(kdot::dated_filename(
        "Orion Product Local Update Import",
        "xlsx"
      ))
      ops_update_fn <- shiny::reactive(kdot::dated_filename(
        "Updated Risk Categories",
        "xlsx"
      ))

      orion_import_created <- shiny::reactiveVal(FALSE)
      orion_import_count <- shiny::reactiveVal(NULL)
      ops_update_count <- shiny::reactiveVal(NULL)
      classified_wb_path <- shiny::reactiveVal(NULL)
      classified_wb_display_name <- shiny::reactiveVal(NULL)

      orion_import_fn_no_ext <- shiny::reactive({
        fn <- orion_import_fn()
        sub("\\.xlsx$", "", fn, ignore.case = TRUE)
      })

      output$orion_import_filename <- shiny::renderUI({
        parts <- list()
        if (orion_import_created()) {
          fn_display <- orion_import_fn_no_ext()
          parts[[length(parts) + 1L]] <- shiny::tags$div(
            style = "margin-bottom: 6px; display: flex; align-items: center; gap: 6px; flex-wrap: wrap;",
            shiny::tags$span(
              style = "color: #2e7d32; font-weight: 500; margin-right: 6px;",
              shiny::icon("check-circle"),
              "Saved"
            ),
            shiny::tags$code(
              id = ns("orion_import_name_display"),
              style = "user-select: all; padding: 2px 4px; background: #f5f5f5; border-radius: 2px;",
              fn_display
            ),
            shiny::actionButton(
              inputId = ns("copy_orion_import_name"),
              label = NULL,
              icon = shiny::icon("copy"),
              title = "Copy to clipboard",
              style = "padding: 2px 6px;"
            )
          )
        }
        if (!is.null(orion_import_count())) {
          parts[[length(parts) + 1L]] <- shiny::tags$div(
            style = "margin-top: 4px;",
            "Orion Import: ",
            shiny::tags$strong(orion_import_count(), " records")
          )
        }
        if (!is.null(ops_update_count())) {
          parts[[length(parts) + 1L]] <- shiny::tags$div(
            style = "margin-top: 4px;",
            shiny::tags$span(
              style = "color: #2e7d32; font-weight: 500; margin-right: 6px;",
              shiny::icon("check-circle"),
              "Saved"
            ),
            "Ops Update: ",
            shiny::tags$strong(ops_update_count(), " records")
          )
        }
        if (length(parts) == 0L) {
          return(NULL)
        }
        shiny::tags$div(
          style = "margin-top: 8px; padding: 8px; font-size: 0.9em; background: #f8f9fa; border-radius: 4px; border-left: 3px solid #2e7d32;",
          parts
        )
      })
      # Conditional buttons: actionButton when auto-save, downloadButton otherwise
      output$class_wb_button <- shiny::renderUI({
        if (shiny::isTruthy(input$auto_save)) {
          shiny::actionButton(
            ns("save_class_wb"),
            "Save Classification Workbook"
          )
        } else {
          shiny::downloadButton(
            ns("class_wb"),
            "Download Classification Workbook"
          )
        }
      })
      output$orion_import_button <- shiny::renderUI({
        if (shiny::isTruthy(input$auto_save)) {
          shiny::actionButton(ns("save_orion_import"), "Save Orion Import")
        } else {
          shiny::downloadButton(ns("orion_import"), "Download Orion Import")
        }
      })
      output$ops_update_button <- shiny::renderUI({
        if (shiny::isTruthy(input$auto_save)) {
          shiny::actionButton(ns("save_ops_update"), "Save Ops Update")
        } else {
          shiny::downloadButton(ns("ops_update"), "Download Ops Update")
        }
      })

      # Auto-save: write directly to configured paths
      shiny::observeEvent(input$save_class_wb, {
        dir.create(
          products_audit_paths$classification_wb,
          recursive = TRUE,
          showWarnings = FALSE
        )
        out_path <- file.path(
          products_audit_paths$classification_wb,
          class_wb_fn()
        )
        shiny::withProgress(
          message = "Saving classification workbook",
          value = 0,
          {
            shiny::incProgress(1 / 3, detail = "Reading data")
            data <- take_export()
            shiny::incProgress(1 / 3, detail = "Building workbook")
            wb <- kdot::create_classification_wb(
              data,
              include_predictions = input$include_predictions
            )
            shiny::incProgress(1 / 3, detail = "Saving file")
            openxlsx::saveWorkbook(wb = wb, out_path)
          }
        )
        shiny::showNotification(paste("Saved to", out_path), type = "message")
      })
      shiny::observeEvent(input$save_orion_import, {
        dir.create(
          products_audit_paths$orion_import,
          recursive = TRUE,
          showWarnings = FALSE
        )
        out_path <- file.path(
          products_audit_paths$orion_import,
          orion_import_fn()
        )
        shiny::withProgress(message = "Saving Orion import", value = 0, {
          shiny::incProgress(1 / 2, detail = "Processing")
          data <- orion_import_xlsx()
          shiny::incProgress(1 / 2, detail = "Saving")
          openxlsx::write.xlsx(x = data, out_path)
        })
        orion_import_created(TRUE)
        orion_import_count(nrow(data))
        shiny::showNotification(paste("Saved to", out_path), type = "message")
      })
      shiny::observeEvent(input$save_ops_update, {
        dir.create(
          products_audit_paths$ops_update,
          recursive = TRUE,
          showWarnings = FALSE
        )
        out_path <- file.path(products_audit_paths$ops_update, ops_update_fn())
        shiny::withProgress(message = "Saving Ops update", value = 0, {
          shiny::incProgress(1 / 4, detail = "Loading data")
          import_data <- orion_import_xlsx()
          held_data <- read_held()
          export_data <- take_export()
          shiny::incProgress(1 / 4, detail = "Comparing risk categories")
          update_data <- create_ops_update(import_data, held_data, export_data)
          shiny::incProgress(1 / 4, detail = "Saving")
          openxlsx::write.xlsx(x = update_data, out_path)
        })
        ops_update_count(nrow(update_data))
        shiny::showNotification(paste("Saved to", out_path), type = "message")
      })

      # Download handler for classification workbook (when auto-save is off)
      output$class_wb <- downloadHandler(
        filename = kdot::dated_filename(
          "Orion Product Classifications",
          "xlsx"
        ),
        content = function(file) {
          shiny::withProgress(
            message = "Creating classification workbook",
            value = 0,
            {
              shiny::incProgress(1 / 3, detail = "Reading data")
              data <- take_export()

              shiny::incProgress(1 / 3, detail = "Building workbook")
              wb <- kdot::create_classification_wb(
                data,
                include_predictions = input$include_predictions
              )

              shiny::incProgress(1 / 3, detail = "Saving file")
              openxlsx::saveWorkbook(wb = wb, file)
            }
          )
        }
      )

      # Sync file input to classified_wb_path when user uploads
      shiny::observeEvent(input$classified_wb, {
        if (shiny::isTruthy(input$classified_wb)) {
          classified_wb_path(input$classified_wb$datapath)
          classified_wb_display_name(input$classified_wb$name)
        }
      })

      output$classified_wb_loaded_status <- shiny::renderUI({
        name <- classified_wb_display_name()
        if (is.null(name) || name == "") {
          return(NULL)
        }
        shiny::tags$div(
          style = "margin-top: 4px; font-size: 0.9em; color: #2e7d32;",
          shiny::icon("check-circle", style = "margin-right: 4px;"),
          "Loaded: ",
          shiny::tags$code(
            style = "padding: 2px 4px; background: #e8f5e9; border-radius: 2px;",
            name
          )
        )
      })

      # Open most recent classification workbook (Step 1)
      shiny::observeEvent(input$open_most_recent_class_wb, {
        path <- find_most_recent_class_wb()
        if (is.null(path)) {
          shiny::showNotification(
            "No classification workbook found in folder.",
            type = "warning"
          )
        } else {
          if (Sys.info()[["sysname"]] == "Windows") {
            shell.exec(path)
          } else {
            utils::browseURL(paste0("file://", path))
          }
          shiny::showNotification(
            paste("Opened", basename(path)),
            type = "message"
          )
        }
      })

      # Copy Orion Import filename to clipboard
      shiny::observeEvent(input$copy_orion_import_name, {
        session$sendCustomMessage("copyToClipboard", orion_import_fn_no_ext())
        shiny::showNotification(
          "Copied to clipboard",
          type = "message",
          duration = 2
        )
      })

      # Open most recent Orion Import and Ops Update
      shiny::observeEvent(input$open_most_recent_orion_import, {
        path <- find_most_recent_orion_import()
        if (is.null(path)) {
          shiny::showNotification(
            "No Orion Import file found in folder.",
            type = "warning"
          )
        } else {
          if (Sys.info()[["sysname"]] == "Windows") {
            shell.exec(path)
          } else {
            utils::browseURL(paste0("file://", path))
          }
          shiny::showNotification(
            paste("Opened", basename(path)),
            type = "message"
          )
        }
      })
      shiny::observeEvent(input$open_most_recent_ops_update, {
        path <- find_most_recent_ops_update()
        if (is.null(path)) {
          shiny::showNotification(
            "No Ops Update file found in folder.",
            type = "warning"
          )
        } else {
          if (Sys.info()[["sysname"]] == "Windows") {
            shell.exec(path)
          } else {
            utils::browseURL(paste0("file://", path))
          }
          shiny::showNotification(
            paste("Opened", basename(path)),
            type = "message"
          )
        }
      })

      # Load most recent classification workbook (Step 2)
      shiny::observeEvent(input$load_most_recent_class_wb, {
        path <- find_most_recent_class_wb()
        if (is.null(path)) {
          shiny::showNotification(
            "No classification workbook found in folder.",
            type = "warning"
          )
        } else {
          classified_wb_path(path)
          classified_wb_display_name(basename(path))
          shiny::showNotification(
            paste("Loaded", basename(path)),
            type = "message"
          )
        }
      })

      # Read classified workbook (from file upload or Load Most Recent)
      orion_import_xlsx <- reactive({
        path <- classified_wb_path()
        shiny::validate(shiny::need(
          path,
          "Upload a classification workbook or click Load Most Recent."
        ))
        tryCatch(
          path |> kdot::orion_product_import(),
          error = function(e) {
            shiny::showNotification(
              paste("Error reading workbook:", conditionMessage(e)),
              type = "error",
              duration = 10
            )
            stop(e)
          }
        )
      })

      # Download handler for Orion import
      output$orion_import <- downloadHandler(
        filename = kdot::dated_filename(
          "Orion Product Local Update Import",
          "xlsx"
        ),
        content = function(file) {
          shiny::withProgress(message = "Creating Orion import", value = 0, {
            shiny::incProgress(1 / 2, detail = "Processing classifications")
            data <- orion_import_xlsx()

            shiny::incProgress(1 / 2, detail = "Saving file")
            openxlsx::write.xlsx(x = data, file)
            orion_import_created(TRUE)
            orion_import_count(nrow(data))
          })
        }
      )

      # Read products held in sleeved accounts (CSV only)
      read_held <- reactive({
        validate(need(input$products_held, message = FALSE))
        path <- input$products_held$datapath
        df <- readr::read_csv(path, show_col_types = FALSE)
        # Normalize CUSIP column name (case-insensitive)
        cusip_col <- grep(
          "^cusip$",
          names(df),
          ignore.case = TRUE,
          value = TRUE
        )
        if (length(cusip_col) > 0L && cusip_col != "CUSIP") {
          names(df)[names(df) == cusip_col] <- "CUSIP"
        }
        df
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
            update_data <- create_ops_update(
              import_data,
              held_data,
              export_data
            )

            shiny::incProgress(1 / 4, detail = "Saving file")
            openxlsx::write.xlsx(x = update_data, file)
            ops_update_count(nrow(update_data))

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
        message("[delisted] Starting Bloomberg delisted securities check")
        result <- tryCatch(
          {
            shiny::validate(
              shiny::need(
                input$orion_export,
                "Upload the query 10635 file (Step 1)."
              ),
              shiny::need(
                input$delisted_csv,
                "Upload the Bloomberg delisted CSV."
              )
            )
            exp <- take_export()
            ticker_col <- if ("Ticker" %in% names(exp)) "Ticker" else "TICKER"
            product_tickers <- unique(exp[[ticker_col]])
            if (is.null(product_tickers)) {
              product_tickers <- character(0)
            }
            message(
              "[delisted] Product tickers from Orion export: ",
              length(product_tickers)
            )
            out <- kdot::check_delisted_products(
              product_tickers,
              input$delisted_csv$datapath
            )
            message("[delisted] Check complete. Matches: ", nrow(out))
            aum_by_ticker <- exp |>
              dplyr::group_by(.data[[ticker_col]]) |>
              dplyr::summarise(AUM = sum(AUM, na.rm = TRUE), .groups = "drop")
            out |>
              dplyr::left_join(
                aum_by_ticker,
                by = c("cleaned_ticker" = ticker_col)
              ) |>
              dplyr::relocate(AUM, .after = cleaned_ticker)
          },
          error = function(e) {
            delisted_is_loading(FALSE)
            message("[delisted] Error: ", conditionMessage(e))
            stop(e)
          }
        )
        delisted_result_store(result)
        delisted_is_loading(FALSE)
      })

      delisted_check_result <- shiny::reactive({
        shiny::req(delisted_result_store())
      })

      output$delisted_status <- shiny::renderUI({
        if (!delisted_submit_clicked()) {
          return(shiny::p(
            "Upload the query 10635 and Bloomberg delisted CSV, then click Check to run."
          ))
        }
        if (delisted_is_loading()) {
          return(shiny::div(
            shiny::icon("spinner", class = "fa-spin fa-fw"),
            " Processing..."
          ))
        }
        result <- delisted_result_store()
        if (!is.null(result) && nrow(result) == 0L) {
          return(shiny::div(
            style = "margin-top: 8px; padding: 12px; background: #e8f5e9; border-radius: 4px; border-left: 4px solid #2e7d32;",
            shiny::icon(
              "check-circle",
              style = "color: #2e7d32; margin-right: 6px;"
            ),
            shiny::tags$strong("No delisted securities found."),
            " All product tickers are clear."
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
          shiny::validate(shiny::need(
            delisted_result_store(),
            "Run the delisted check first."
          ))
          shiny::withProgress(message = "Creating import file", value = 0, {
            shiny::incProgress(0.5, detail = "Building workbook")
            result <- delisted_result_store()
            trading_import <- tibble::tibble(
              `Entity ID` = 22500,
              Level = "Firm",
              Ticker = result$cleaned_ticker,
              `Sell Priority` = "Do Not Sell",
              `Buy Priority` = "Do Not Buy",
              Exclude = ""
            )
            shiny::incProgress(0.5, detail = "Saving file")
            openxlsx::write.xlsx(trading_import, file, sheetName = "Sheet1")
          })
        }
      )
    }
  )
}
