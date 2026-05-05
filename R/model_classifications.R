.model_class_id_columns <- function() {
  c(
    "modelId",
    "model_id",
    "riskCategoryId",
    "risk_category_id",
    "assetClassId",
    "asset_class_id"
  )
}

# Coerce known ID columns to integer for preview (read_csv often yields doubles).
.format_model_class_preview <- function(df) {
  for (col in intersect(.model_class_id_columns(), names(df))) {
    x <- df[[col]]
    if (is.numeric(x)) {
      df[[col]] <- suppressWarnings(as.integer(x))
    }
  }
  df
}

modelClassificationsUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h1("Update Model Classifications (Risk Category and Asset Class)"),
    shiny::p(
      "Upload a CSV to set ",
      shiny::tags$code("riskCategoryId"),
      " and/or ",
      shiny::tags$code("assetClassId"),
      " for a given Orion model agg ID. "
    ),
    shiny::tags$ul(
      shiny::tags$li(
        shiny::tags$strong("Required:"),
        " column ",
        shiny::tags$code("modelId"),
        " (or ",
        shiny::tags$code("model_id"),
        ")."
      ),
      shiny::tags$li(
        "At least one of ",
        shiny::tags$code("riskCategoryId"),
        " / ",
        shiny::tags$code("assetClassId"),
        ". Blank or NA skips that field for the row."
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::wellPanel(
          shiny::h4("API Token"),
          shiny::passwordInput(
            inputId = ns("token"),
            label = NULL,
            placeholder = "Paste your API token here (or set MA_ORION_API_TOKEN env var)",
            width = "100%",
            value = if (Sys.getenv("MA_ORION_API_TOKEN") != "") {
              paste(rep("*", kdot::get_token_mask_length()), collapse = "")
            } else {
              ""
            }
          ),
          shiny::actionButton(
            inputId = ns("save_token"),
            label = "Save Token",
            class = "btn-primary",
            width = "100%",
            style = "margin-top: 10px; background-color: #9b1c7a; border-color: #9b1c7a;"
          )
        )
      ),
      shiny::column(
        width = 6,
        shiny::wellPanel(
          shiny::h4("Example CSV"),
          shiny::downloadButton(
            outputId = ns("download_example"),
            label = "Download example",
            class = "btn-default",
            style = "width: 100%;"
          )
        )
      )
    ),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::wellPanel(
          shiny::fileInput(
            inputId = ns("csv_file"),
            label = "CSV file",
            accept = c(".csv", "text/csv"),
            width = "100%"
          ),
          shiny::uiOutput(ns("csv_validation")),
          shiny::h4("Preview"),
          DT::dataTableOutput(ns("csv_preview_table")),
          shiny::actionButton(
            inputId = ns("run_updates"),
            label = "Run updates",
            class = "btn-danger",
            width = "100%",
            style = "margin-top: 15px;"
          ),
          shiny::uiOutput(ns("run_summary")),
          shiny::br(),
          DT::dataTableOutput(ns("results_table"))
        )
      )
    )
  )
}

modelClassificationsServer <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      parsed_df <- shiny::reactiveVal(NULL)
      parse_error <- shiny::reactiveVal(NULL)
      results_df <- shiny::reactiveVal(NULL)

      shiny::observeEvent(input$save_token, {
        token <- kdot::get_token(input$token)
        if (is.null(token) || token == "") {
          shiny::showNotification("No token to save", type = "error")
          return()
        }

        renv_path <- ".Renviron"
        env_line <- paste0("MA_ORION_API_TOKEN=", token)

        tryCatch(
          {
            # Read existing .Renviron
            if (file.exists(renv_path)) {
              renv_lines <- readLines(renv_path)
              # Remove existing MA_ORION_API_TOKEN line if present
              renv_lines <- renv_lines[
                !grepl("^MA_ORION_API_TOKEN=", renv_lines)
              ]
            } else {
              renv_lines <- character(0)
            }

            # Add new token line
            renv_lines <- c(renv_lines, env_line)
            writeLines(renv_lines, renv_path)

            # Update environment variable in current session
            Sys.setenv(MA_ORION_API_TOKEN = token)

            shiny::showNotification(
              "Token saved to .Renviron file. Restart R session for permanent effect.",
              type = "message",
              duration = 5
            )
          },
          error = function(e) {
            shiny::showNotification(
              paste("Failed to save token:", e$message),
              type = "error"
            )
          }
        )
      })

      output$download_example <- shiny::downloadHandler(
        filename = "model-classification-update-example.csv",
        content = function(file) {
          lines <- c(
            "modelId,riskCategoryId,assetClassId",
            "12345,99,",
            "12346,,101"
          )
          writeLines(lines, file, useBytes = TRUE)
        }
      )

      shiny::observeEvent(input$csv_file, {
        results_df(NULL)
        parse_error(NULL)
        parsed_df(NULL)

        shiny::req(input$csv_file)
        path <- input$csv_file$datapath

        df <- tryCatch(
          readr::read_csv(path, show_col_types = FALSE),
          error = function(e) {
            parse_error(paste("Could not read CSV:", e$message))
            NULL
          }
        )

        if (is.null(df)) {
          if (is.null(parse_error())) {
            parse_error("Could not read CSV.")
          }
          return()
        }

        nm <- names(df)
        ok_model <- ("modelId" %in% nm) || ("model_id" %in% nm)
        ok_rc <- ("riskCategoryId" %in% nm) || ("risk_category_id" %in% nm)
        ok_ac <- ("assetClassId" %in% nm) || ("asset_class_id" %in% nm)

        if (!ok_model) {
          parse_error("Missing column modelId (or model_id).")
          return()
        }
        if (!ok_rc && !ok_ac) {
          parse_error(
            "Need at least one of riskCategoryId / assetClassId (or snake_case names)."
          )
          return()
        }

        parse_error(NULL)
        parsed_df(df)
      })

      output$csv_validation <- shiny::renderUI({
        err <- parse_error()
        if (!is.null(err)) {
          shiny::div(
            class = "alert alert-danger",
            role = "alert",
            err
          )
        } else if (is.null(parsed_df())) {
          NULL
        } else {
          shiny::div(
            class = "alert alert-success",
            role = "alert",
            paste0(nrow(parsed_df()), " row(s) loaded. Ready to run.")
          )
        }
      })

      output$csv_preview_table <- DT::renderDT(
        {
          shiny::req(parsed_df())
          prev <- .format_model_class_preview(parsed_df())
          DT::datatable(
            prev,
            class = "stripe hover cell-border",
            options = list(
              scrollY = "420px",
              scrollX = TRUE,
              scrollCollapse = TRUE,
              paging = FALSE,
              ordering = TRUE,
              searching = TRUE,
              info = TRUE,
              dom = "fti"
            ),
            rownames = FALSE
          )
        },
        server = FALSE
      )

      shiny::observeEvent(input$run_updates, {
        shiny::req(parsed_df())

        token <- kdot::get_token(input$token)
        if (is.null(token) || !nzchar(trimws(token))) {
          shiny::showNotification("API token required", type = "error")
          return()
        }

        n <- nrow(parsed_df())
        shiny::showModal(shiny::modalDialog(
          title = "Confirm production updates",
          shiny::p(
            "You are about to update ",
            shiny::tags$strong(n),
            " trading model(s) in ",
            shiny::tags$strong("production Orion"),
            "."
          ),
          shiny::p("This cannot be undone from this app."),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(
              ns("confirm_run"),
              "Confirm update",
              class = "btn-danger"
            )
          ),
          easyClose = TRUE
        ))
      })

      shiny::observeEvent(input$confirm_run, {
        shiny::removeModal()
        shiny::req(parsed_df())

        token <- kdot::get_token(input$token)
        if (is.null(token) || !nzchar(trimws(token))) {
          shiny::showNotification("API token required", type = "error")
          return()
        }

        df <- parsed_df()

        shiny::withProgress(message = "Updating trading models...", value = 0, {
          res <- tryCatch(
            kdot::batch_update_trading_model_classifications(df, token),
            error = function(e) {
              shiny::showNotification(
                paste("Batch failed:", e$message),
                type = "error",
                duration = 10
              )
              NULL
            }
          )
          shiny::incProgress(1)
        })

        if (is.null(res)) {
          return()
        }

        results_df(res)

        ok <- sum(res$success, na.rm = TRUE)
        bad <- sum(!res$success, na.rm = TRUE)
        shiny::showNotification(
          paste0("Complete: ", ok, " succeeded, ", bad, " failed."),
          type = if (bad > 0) "warning" else "message",
          duration = 8
        )
      })

      output$run_summary <- shiny::renderUI({
        res <- results_df()
        if (is.null(res)) {
          return(NULL)
        }
        ok <- sum(res$success, na.rm = TRUE)
        bad <- sum(!res$success, na.rm = TRUE)
        shiny::div(
          class = "well",
          shiny::h4("Last run results"),
          shiny::p(paste0("Succeeded: ", ok, ", Failed: ", bad))
        )
      })

      output$results_table <- DT::renderDT({
        shiny::req(results_df())
        DT::datatable(
          results_df(),
          options = get_dt_options("model_classification_results"),
          extensions = "Buttons",
          rownames = FALSE,
          filter = "top"
        )
      })
    }
  )
}
