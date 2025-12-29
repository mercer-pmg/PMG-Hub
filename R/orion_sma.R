# Orion SMA Settings Module

# Module Configuration
PRODUCTS_CSV_PATH <- "long-short-products.csv"

# UI Helper Functions ----

# Mask account number for display
mask_account_number <- function(account_number) {
  if (is.null(account_number) || account_number == "N/A" || nchar(account_number) <= 4) {
    return(ifelse(is.null(account_number), "N/A", account_number))
  }
  paste0("****", substr(account_number, nchar(account_number) - 3, nchar(account_number)))
}

# Safe NULL comparison helper
safe_equals <- function(a, b) {
  if (is.null(a) && is.null(b)) {
    return(TRUE)
  }
  if (is.null(a) || is.null(b)) {
    return(FALSE)
  }
  a == b
}

# Extract SMA values from account data
extract_sma_values <- function(account_data) {
  if (is.null(account_data) || !is.list(account_data) || is.null(account_data$sma)) {
    return(list(isSMA = FALSE, eclipseSMA = "False", smaAssetID = NULL))
  }

  sma <- account_data$sma
  is_sma_value <- if (!is.null(sma$isSma)) as.logical(sma$isSma) else FALSE
  if (length(is_sma_value) == 0 || is.na(is_sma_value)) {
    is_sma_value <- FALSE
  }

  list(
    isSMA = is_sma_value,
    eclipseSMA = if (!is.null(sma$eclipseSMA)) as.character(sma$eclipseSMA) else "False",
    smaAssetID = if (!is.null(sma$smaAssetId)) as.integer(sma$smaAssetId) else NULL
  )
}

# Parse account IDs from text input
parse_account_ids <- function(account_ids_text) {
  account_ids_raw <- strsplit(trimws(account_ids_text), "[,\n]")[[1]]
  account_ids_raw <- trimws(account_ids_raw)
  account_ids_raw <- account_ids_raw[account_ids_raw != ""]

  account_ids <- c()
  for (id in account_ids_raw) {
    id_clean <- trimws(id)
    if (id_clean != "" && grepl("^[0-9]+$", id_clean)) {
      account_ids <- c(account_ids, id_clean)
    }
  }
  account_ids
}

# Get DT table options
get_dt_options <- function(filename_prefix = "results") {
  list(
    pageLength = 25,
    lengthMenu = list(c(10, 25, 50, 100, -1), c(10, 25, 50, 100, "All")),
    scrollX = TRUE,
    dom = "Bfrtip",
    buttons = list(
      list(extend = "copy", exportOptions = list(modifier = list(page = "all"))),
      list(extend = "csv", filename = filename_prefix, exportOptions = list(modifier = list(page = "all"))),
      list(extend = "excel", filename = filename_prefix, exportOptions = list(modifier = list(page = "all")))
    )
  )
}

# Load products from AWS
load_products_from_aws <- function() {
  cat("[DEBUG] Loading products from AWS\n")
  products_df <- kdot::get_long_short_products()
  cat("[DEBUG] AWS loaded successfully, rows:", nrow(products_df), "\n")

  # Extract Product ID and Product Name
  products_list <- Map(
    function(id, name) list(id = as.integer(id), name = as.character(name)),
    products_df$`Product ID`,
    products_df$`Product Name`
  )

  return(products_list)
}

# Load model aggregates from AWS
load_model_aggregates_from_aws <- function() {
  cat("[DEBUG] Loading model aggregates from AWS\n")
  model_aggs_df <- kdot::get_model_aggregates()
  cat("[DEBUG] AWS loaded successfully, rows:", nrow(model_aggs_df), "\n")

  # Remove any rows with missing values
  model_aggs_df <- model_aggs_df[!is.na(model_aggs_df$modelAggId) & !is.na(model_aggs_df$modelName), ]

  # Extract Model Aggregate ID and Model Name
  model_aggs_list <- Map(
    function(id, name) list(id = as.integer(id), name = as.character(name)),
    model_aggs_df$modelAggId,
    model_aggs_df$modelName
  )
  
  return(model_aggs_list)
}

# Validate bulk CSV structure and data
validate_bulk_csv <- function(csv_data) {
  if (is.null(csv_data) || nrow(csv_data) == 0) {
    return(list(valid = FALSE, error = "CSV file is empty or could not be read"))
  }

  # Normalize column names (case-insensitive, handle spaces/underscores)
  col_names <- tolower(gsub("[_ ]", "", names(csv_data)))

  # Find account ID column
  account_col_idx <- which(col_names %in% c("accountid", "account_id", "account"))
  if (length(account_col_idx) == 0) {
    return(list(valid = FALSE, error = "CSV must contain an 'Account ID' column"))
  }
  account_col_name <- names(csv_data)[account_col_idx[1]]

  # Find product ID column
  product_col_idx <- which(col_names %in% c("productid", "product_id", "product"))
  if (length(product_col_idx) == 0) {
    return(list(valid = FALSE, error = "CSV must contain a 'Product ID' column"))
  }
  product_col_name <- names(csv_data)[product_col_idx[1]]

  # Extract and validate data
  account_ids <- csv_data[[account_col_name]]
  product_ids <- csv_data[[product_col_name]]

  # Remove rows with missing values
  valid_rows <- !is.na(account_ids) & !is.na(product_ids) &
    trimws(as.character(account_ids)) != "" &
    trimws(as.character(product_ids)) != ""

  if (sum(valid_rows) == 0) {
    return(list(valid = FALSE, error = "No valid rows found in CSV (all rows have missing Account ID or Product ID)"))
  }

  # Convert to numeric and filter valid numeric IDs
  account_ids_numeric <- suppressWarnings(as.integer(account_ids[valid_rows]))
  product_ids_numeric <- suppressWarnings(as.integer(product_ids[valid_rows]))

  numeric_valid <- !is.na(account_ids_numeric) & !is.na(product_ids_numeric) &
    account_ids_numeric > 0 & product_ids_numeric > 0

  if (sum(numeric_valid) == 0) {
    return(list(valid = FALSE, error = "No valid numeric IDs found in CSV"))
  }

  # Create validated data frame
  validated_data <- data.frame(
    AccountID = account_ids_numeric[numeric_valid],
    ProductID = product_ids_numeric[numeric_valid],
    stringsAsFactors = FALSE
  )

  return(list(
    valid = TRUE,
    data = validated_data,
    account_col = account_col_name,
    product_col = product_col_name,
    total_rows = nrow(validated_data)
  ))
}

# UI Function ----

orionSmaUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    h1("Orion SMA Settings Updater"),
    shiny::fluidPage(
      # Top row: Account Number Search (full width)
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::wellPanel(
            h4("Search Account by Number"),
            shiny::fluidRow(
              shiny::column(
                width = 8,
                shiny::textInput(
                  inputId = ns("account_search_number"),
                  label = NULL,
                  placeholder = "Enter account number to search",
                  width = "100%"
                )
              ),
              shiny::column(
                width = 4,
                shiny::actionButton(
                  inputId = ns("search_account"),
                  label = "Search",
                  class = "btn-primary",
                  width = "100%",
                  style = "margin-top: 0;"
                )
              )
            ),
            shiny::htmlOutput(ns("account_search_results"))
          )
        )
      ),

      # Second row: Token and Account ID side by side
      shiny::fluidRow(
        # Column 1: API Token
        shiny::column(
          width = 6,
          shiny::wellPanel(
            h4("API Token"),
            shiny::passwordInput(
              inputId = ns("token"),
              label = NULL,
              placeholder = "Paste your API token here (or set MA_ORION_API_TOKEN env var)",
              width = "100%",
              value = if (Sys.getenv("MA_ORION_API_TOKEN") != "") paste(rep("*", kdot::get_token_mask_length()), collapse = "") else ""
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
        # Column 2: Account ID
        shiny::column(
          width = 6,
          shiny::wellPanel(
            h4("Account Information"),
            shiny::textInput(
              inputId = ns("account_id"),
              label = NULL,
              placeholder = "Enter Account ID",
              width = "100%"
            ),
            shiny::actionButton(
              inputId = ns("fetch_account"),
              label = "Fetch Account Info",
              class = "btn-primary",
              width = "100%",
              style = "margin-top: 10px;"
            )
          )
        )
      ),

      # Third row: Account info and Asset injection side by side
      shiny::fluidRow(
        # Column 1: Account info display and Process Steps
        shiny::column(
          width = 6,
          shiny::wellPanel(
            h4("Account Information"),
            shiny::htmlOutput(ns("account_info"))
          ),
          shiny::tags$details(
            class = "well",
            open = "", # Auto-expand the details element
            style = "margin-bottom: 20px; margin-top: 15px;",
            shiny::tags$summary(
              style = "cursor: pointer; font-weight: bold; font-size: 18px; margin-bottom: 10px; display: flex; justify-content: space-between; align-items: center;",
              shiny::span("Process Steps"),
              shiny::actionButton(
                inputId = ns("clear_steps"),
                label = "Clear",
                class = "btn-sm",
                style = "margin-left: 10px; padding: 2px 8px; font-size: 12px;"
              )
            ),
            shiny::htmlOutput(ns("steps"))
          )
        ),
        # Column 2: Asset injection and SMA settings
        shiny::column(
          width = 6,
          shiny::wellPanel(
            h4("Asset Injection"),
            shiny::selectInput(
              inputId = ns("product"),
              label = "Product:",
              choices = c("No products loaded"),
              width = "100%"
            ),
            shiny::actionButton(
              inputId = ns("inject_asset"),
              label = "Inject Asset",
              class = "btn-primary",
              width = "100%",
              style = "margin-top: 10px;"
            )
          ),
          shiny::wellPanel(
            h4("SMA Settings"),
            shiny::checkboxInput(
              inputId = ns("is_sma"),
              label = "Is SMA",
              value = FALSE
            ),
            shiny::selectInput(
              inputId = ns("asset"),
              label = "SMA Asset ID:",
              choices = c("None"),
              width = "100%"
            ),
            shiny::actionButton(
              inputId = ns("update_sma"),
              label = "Update SMA Settings",
              class = "btn-primary",
              width = "100%"
            )
          ),
          shiny::wellPanel(
            h4("Model Aggregate Assignment"),
            shiny::selectInput(
              inputId = ns("model_agg"),
              label = "Model Aggregate:",
              choices = c("No models loaded"),
              width = "100%"
            ),
            shiny::actionButton(
              inputId = ns("update_model_agg"),
              label = "Update Model Aggregate",
              class = "btn-primary",
              width = "100%",
              style = "margin-top: 10px;"
            )
          )
        )
      ),

      # Fourth row: Bulk CSV Processing
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::tags$details(
            class = "well",
            style = "margin-bottom: 20px;",
            shiny::tags$summary(
              style = "cursor: pointer; font-weight: bold; font-size: 18px; margin-bottom: 10px;",
              "Bulk CSV Processing"
            ),
            shiny::p("Upload a CSV file with 'Account ID' and 'Product ID' columns. The system will automatically:"),
            shiny::tags$ul(
              shiny::tags$li("Check if each product exists in the account"),
              shiny::tags$li("Inject missing products"),
              shiny::tags$li("Configure SMA settings for all products")
            ),
            shiny::fileInput(
              inputId = ns("bulk_csv_file"),
              label = "CSV File (Account ID, Product ID)",
              accept = c(".csv", "text/csv"),
              width = "100%"
            ),
            shiny::htmlOutput(ns("csv_preview")),
            shiny::actionButton(
              inputId = ns("process_bulk_csv"),
              label = "Process Bulk CSV",
              class = "btn-primary",
              width = "100%",
              style = "margin-top: 10px; background-color: #28a745; border-color: #28a745;"
            ),
            shiny::htmlOutput(ns("bulk_progress")),
            shiny::br(),
            shiny::htmlOutput(ns("bulk_results_summary")),
            shiny::br(),
            DT::dataTableOutput(ns("bulk_results_table"))
          )
        )
      ),

      # Fifth row: SMA Settings Checker
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::tags$details(
            class = "well",
            style = "margin-bottom: 20px;",
            shiny::tags$summary(
              style = "cursor: pointer; font-weight: bold; font-size: 18px; margin-bottom: 10px;",
              "SMA Settings Checker"
            ),
            shiny::p("Enter a list of Account IDs (one per line or comma-separated) to check SMA settings for each account."),
            shiny::tags$ul(
              shiny::tags$li("Checks if SMA assets exist in each account"),
              shiny::tags$li("Returns SMA settings (isSMA, eclipseSMA, smaAssetID)"),
              shiny::tags$li("Displays results in a table")
            ),
            shiny::textAreaInput(
              inputId = ns("checker_account_ids"),
              label = "Account IDs (one per line or comma-separated):",
              placeholder = "1440289\n137764\n1402836",
              rows = 5,
              width = "100%"
            ),
            shiny::actionButton(
              inputId = ns("run_checker"),
              label = "Check SMA Settings",
              class = "btn-primary",
              width = "100%",
              style = "margin-top: 10px; background-color: #17a2b8; border-color: #17a2b8;"
            ),
            shiny::htmlOutput(ns("checker_progress")),
            shiny::br(),
            shiny::htmlOutput(ns("checker_summary")),
            shiny::br(),
            DT::dataTableOutput(ns("checker_results_table"))
          )
        )
      )
    )
  )
}

# Server Function ----

orionSmaServer <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      # Reactive values
      values <- shiny::reactiveValues(
        account_data = NULL,
        products = list(),
        assets = list(),
        asset_product_map = NULL,
        expected_values = NULL,
        refresh_trigger = 0,
        steps_history = "", # Store cumulative steps history
        previous_account_id = NULL, # Track previous account ID to detect new account
        account_search_results = NULL, # Store account search results
        bulk_csv_data = NULL,
        bulk_results = NULL,
        bulk_processing = FALSE,
        checker_results = NULL,
        checker_processing = FALSE
      )

      # Helper function to append to steps history
      append_to_steps <- function(new_text) {
        separator <- if (values$steps_history != "") "<br><br><hr><br>" else ""
        values$steps_history <<- paste0(values$steps_history, separator, new_text)
        output$steps <- shiny::renderUI(shiny::HTML(values$steps_history))
      }

      # Helper function to fetch and populate account
      fetch_and_populate_account <- function(account_id, token) {
        # Clear steps history if this is a new account ID
        if (!is.null(values$previous_account_id) && values$previous_account_id != account_id) {
          values$steps_history <- ""
          output$steps <- shiny::renderUI(shiny::HTML(""))
        }
        values$previous_account_id <<- account_id

        shiny::withProgress(message = "Fetching account information...", value = 0, {
          shiny::incProgress(0.5)

          account_result <- kdot::get_account(account_id, token, expand = "Sma,Portfolio")

          shiny::incProgress(1)

          if (account_result$success) {
            values$account_data <- account_result$data
            populate_settings()
            shiny::showNotification("Account info loaded successfully", type = "message")
          } else {
            shiny::showNotification(account_result$error, type = "error")
          }
        })
      }

      # Helper function to render progress message
      render_progress_message <- function(message) {
        shiny::HTML(paste0(
          "<div style='margin-top: 10px; padding: 10px; background-color: #e7f3ff; border: 1px solid #b3d9ff; border-radius: 4px;'>",
          "<strong>Processing:</strong> ", message, "</div>"
        ))
      }

      # Track if data has been loaded
      products_loaded <- shiny::reactiveVal(FALSE)
      model_aggs_loaded <- shiny::reactiveVal(FALSE)

      # Load products lazily (only when user first interacts with the tab)
      load_products_if_needed <- function() {
        if (!products_loaded()) {
          products_list <- load_products_from_aws()
          values$products <- products_list

          if (length(products_list) > 0) {
            product_names <- sapply(products_list, function(x) x$name)
            product_ids <- sapply(products_list, function(x) x$id)
            names(product_ids) <- product_names
            values$product_map <- product_ids
            values$approved_product_ids <- product_ids

            shiny::updateSelectInput(
              session = session,
              inputId = "product",
              choices = product_names,
              selected = product_names[1]
            )
          }
          products_loaded(TRUE)
        }
      }

      # Load model aggregates lazily (only when user first interacts with the tab)
      load_model_aggregates_if_needed <- function() {
        if (!model_aggs_loaded()) {
          model_aggs_list <- load_model_aggregates_from_aws()
          values$model_aggregates <- model_aggs_list

          if (length(model_aggs_list) > 0) {
            model_names <- sapply(model_aggs_list, function(x) x$name)
            model_ids <- sapply(model_aggs_list, function(x) x$id)
            names(model_ids) <- model_names
            values$model_agg_map <- model_ids

            shiny::updateSelectInput(
              session = session,
              inputId = "model_agg",
              choices = model_names,
              selected = model_names[1]
            )
          }
          model_aggs_loaded(TRUE)
        }
      }

      # Load data when user first interacts with key inputs
      shiny::observeEvent(input$fetch_account, {
        load_products_if_needed()
        load_model_aggregates_if_needed()
      }, ignoreInit = TRUE)

      shiny::observeEvent(input$search_account, {
        load_products_if_needed()
        load_model_aggregates_if_needed()
      }, ignoreInit = TRUE)

      shiny::observeEvent(input$inject_asset, {
        load_products_if_needed()
      }, ignoreInit = TRUE)

      shiny::observeEvent(input$update_model_agg, {
        load_model_aggregates_if_needed()
      }, ignoreInit = TRUE)

      # Save token to .Renviron
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
              renv_lines <- renv_lines[!grepl("^MA_ORION_API_TOKEN=", renv_lines)]
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

      # Search for account by number
      shiny::observeEvent(input$search_account, {
        account_number <- trimws(input$account_search_number)
        token <- kdot::get_token(input$token)

        if (account_number == "") {
          shiny::showNotification("Please enter an account number to search", type = "error")
          return()
        }

        if (is.null(token) || token == "") {
          shiny::showNotification("API token not found", type = "error")
          return()
        }

        shiny::withProgress(message = "Searching for account...", value = 0, {
          shiny::incProgress(0.5)

          search_result <- kdot::get_accounts_by_number(account_number, token, exact_match = FALSE)

          shiny::incProgress(1)

          if (search_result$success) {
            response_data <- search_result$data
            values$account_search_results <- response_data

            if (length(response_data) == 0 || (is.data.frame(response_data) && nrow(response_data) == 0)) {
              output$account_search_results <- shiny::renderUI({
                shiny::HTML("<p style='color: #666; margin-top: 10px;'>No accounts found matching that number.</p>")
              })
            } else {
              # Convert to list if it's a data frame
              accounts_list <- if (is.data.frame(response_data)) {
                lapply(1:nrow(response_data), function(i) as.list(response_data[i, ]))
              } else {
                response_data
              }

              # Build results HTML
              results_html <- paste0(
                "<div style='margin-top: 10px;'>",
                "<strong>Found ", length(accounts_list), " account(s):</strong><br><br>"
              )

              for (i in seq_along(accounts_list)) {
                account <- accounts_list[[i]]
                account_id <- if (!is.null(account$id)) account$id else "N/A"
                account_name <- if (!is.null(account$name)) account$name else "N/A"
                account_number_display <- if (!is.null(account$number)) account$number else "N/A"
                account_custodian <- if (!is.null(account$custodian)) account$custodian else "N/A"
                is_active <- if (!is.null(account$isActive)) account$isActive else FALSE

                results_html <- paste0(
                  results_html,
                  "<div style='border: 1px solid #ddd; padding: 10px; margin-bottom: 10px; border-radius: 4px; background-color: #f9f9f9;'>",
                  "<strong>Account ID:</strong> ", account_id, "<br>",
                  "<strong>Name:</strong> ", account_name, "<br>",
                  "<strong>Number:</strong> ", account_number_display, "<br>",
                  "<strong>Custodian:</strong> ", account_custodian, "<br>",
                  "<strong>Active:</strong> ", ifelse(is_active, "Yes", "No"), "<br>",
                  "<button class='btn btn-sm' onclick=\"Shiny.setInputValue('", session$ns("select_account_id"), "', '", account_id, "', {priority: 'event'})\" style='margin-top: 5px; background-color: #9b1c7a; border-color: #9b1c7a; color: white;'>Select This Account</button>",
                  "</div>"
                )
              }

              results_html <- paste0(results_html, "</div>")
              output$account_search_results <- shiny::renderUI({
                shiny::HTML(results_html)
              })
            }
          } else {
            output$account_search_results <- shiny::renderUI({
              shiny::HTML(paste0("<p style='color: #d32f2f; margin-top: 10px;'>Error: ", search_result$error, "</p>"))
            })
            shiny::showNotification(search_result$error, type = "error")
          }
        })
      })

      # Handle account selection from search results
      shiny::observeEvent(input$select_account_id, {
        selected_id <- input$select_account_id
        if (!is.null(selected_id) && selected_id != "") {
          shiny::updateTextInput(
            session = session,
            inputId = "account_id",
            value = as.character(selected_id)
          )

          # Auto-fetch account info
          token_raw <- kdot::get_token(input$token)

          if (is.null(token_raw) || token_raw == "") {
            shiny::showNotification("API token not found", type = "error")
            return()
          }

          fetch_and_populate_account(as.character(selected_id), token_raw)
        }
      })

      # Fetch account info
      shiny::observeEvent(input$fetch_account, {
        account_id_raw <- input$account_id
        token_raw <- kdot::get_token(input$token)

        validation <- kdot::validate_inputs(account_id_raw, token_raw)
        if (!validation$valid) {
          shiny::showNotification(validation$error, type = "error")
          return()
        }

        fetch_and_populate_account(validation$account_id, validation$token)
      })

      # Clear process steps when refresh button is clicked
      shiny::observeEvent(input$clear_steps, {
        values$steps_history <- ""
        output$steps <- shiny::renderUI(shiny::HTML(""))
        shiny::showNotification("Process steps cleared", type = "message")
      })

      # Populate settings from account data
      populate_settings <- function() {
        if (is.null(values$account_data)) {
          return()
        }

        account_name <- ifelse(is.null(values$account_data$name), "N/A", values$account_data$name)
        account_number_raw <- values$account_data$number
        account_number <- ifelse(is.null(account_number_raw), "N/A", account_number_raw)

        sma_values <- extract_sma_values(values$account_data)

        # Extract model name from portfolio (when Portfolio is expanded)
        model_name <- if (!is.null(values$account_data$portfolio) &&
          !is.null(values$account_data$portfolio$modelName)) {
          as.character(values$account_data$portfolio$modelName)
        } else if (!is.null(values$account_data$modelName)) {
          # Fallback: check if modelName is at top level
          as.character(values$account_data$modelName)
        } else {
          "N/A"
        }

        # Use helper function to mask account number
        masked_number <- mask_account_number(account_number)

        # Update account info display
        info_text <- paste0(
          "<strong>Account:</strong> ", account_name, " (", masked_number, ")<br><br>",
          "<strong>Model Name:</strong> ", model_name, "<br><br>",
          "<strong>Current SMA Status:</strong><br>",
          "&nbsp;&nbsp;• isSMA: ", sma_values$isSMA, "<br>",
          "&nbsp;&nbsp;• eclipseSMA: ", sma_values$eclipseSMA, "<br>",
          "&nbsp;&nbsp;• assetID: ", ifelse(is.null(sma_values$smaAssetID), "None", sma_values$smaAssetID)
        )

        output$account_info <- shiny::renderUI({
          shiny::HTML(info_text)
        })

        # Update product dropdown
        if (length(values$products) > 0) {
          product_names <- sapply(values$products, function(x) x$name)
          shiny::updateSelectInput(
            session = session,
            inputId = "product",
            choices = product_names,
            selected = product_names[1]
          )
        }

        # Update isSMA checkbox
        shiny::updateCheckboxInput(
          session = session,
          inputId = "is_sma",
          value = sma_values$isSMA
        )

        # Load assets
        load_assets()

        # Set selected asset if SMA asset ID exists
        if (!is.null(sma_values$smaAssetID)) {
          set_selected_asset(sma_values$smaAssetID)
        }
      }

      # Load assets from account
      load_assets <- function() {
        if (is.null(values$account_data)) {
          return()
        }

        account_id <- trimws(input$account_id)
        if (account_id == "") {
          return()
        }

        token <- kdot::get_token(input$token)
        if (is.null(token) || token == "") {
          return()
        }
        assets_result <- kdot::get_account_assets(account_id, token)

        if (!assets_result$success) {
          cat("[ERROR] Failed to fetch assets:", assets_result$error, "\n")
          shiny::showNotification(assets_result$error, type = "error")
          return()
        }

        # Assets are already parsed by get_account_assets
        assets <- assets_result$data
        assets_list <- list()
        asset_product_map <- list()

        # Process each asset (matching Python logic)
        for (asset in assets) {
          if (is.list(asset)) {
            asset_id <- if (!is.null(asset$id)) as.integer(asset$id) else NULL
            product_id <- if (!is.null(asset$productId)) as.integer(asset$productId) else NULL
            asset_name <- ifelse(is.null(asset$name), paste("Asset", asset_id), asset$name)

            if (!is.null(asset_id)) {
              assets_list[[length(assets_list) + 1]] <- list(id = asset_id, name = asset_name)
              if (!is.null(product_id)) {
                asset_product_map[[as.character(asset_id)]] <- product_id
              }
            }
          } else if (is.numeric(asset) || is.character(asset)) {
            # Handle case where asset is just an ID (like Python handles int/str)
            asset_id <- as.integer(asset)
            if (!is.na(asset_id)) {
              assets_list[[length(assets_list) + 1]] <- list(id = asset_id, name = paste("Asset", asset_id))
              # No product_id for simple ID format
            }
          }
        }

        cat("[DEBUG] Processed", length(assets_list), "assets,", length(asset_product_map), "with product IDs\n")

        # Remove duplicates
        seen_ids <- c()
        unique_assets <- Filter(function(asset_item) {
          if (!asset_item$id %in% seen_ids) {
            seen_ids <<- c(seen_ids, asset_item$id)
            return(TRUE)
          }
          return(FALSE)
        }, assets_list)

        # Build unique product map
        unique_product_map <- list()
        for (asset_item in unique_assets) {
          asset_id_str <- as.character(asset_item$id)
          if (asset_id_str %in% names(asset_product_map)) {
            unique_product_map[[asset_id_str]] <- asset_product_map[[asset_id_str]]
          }
        }

        # Sort by name
        if (length(unique_assets) > 0) {
          names_order <- order(sapply(unique_assets, function(x) tolower(x$name)))
          unique_assets <- unique_assets[names_order]
        }

        # Filter by approved products (using original asset_product_map like Python version)
        filtered_assets <- list()

        if (length(asset_product_map) > 0 && length(values$approved_product_ids) > 0) {
          # Get approved product IDs as a simple vector for comparison
          approved_ids <- unname(values$approved_product_ids)

          for (asset_item in unique_assets) {
            asset_id_str <- as.character(asset_item$id)
            # Check original asset_product_map (not unique_product_map) - matches Python logic
            if (asset_id_str %in% names(asset_product_map)) {
              product_id <- asset_product_map[[asset_id_str]]
              if (!is.null(product_id) && product_id %in% approved_ids) {
                filtered_assets[[length(filtered_assets) + 1]] <- asset_item
              }
            }
          }
        }

        cat("[DEBUG] Found", length(filtered_assets), "approved asset(s)\n")
        values$assets_list <- filtered_assets
        values$asset_product_map <- asset_product_map # Store original map, not unique

        # Update asset dropdown
        if (length(filtered_assets) > 0) {
          asset_choices <- sapply(filtered_assets, function(x) paste0(x$name, " (", x$id, ")"))
          shiny::updateSelectInput(
            session = session,
            inputId = "asset",
            choices = asset_choices,
            selected = asset_choices[1]
          )
          shiny::showNotification(
            paste("Found", length(filtered_assets), "approved asset(s)"),
            type = "message"
          )
        } else {
          shiny::updateSelectInput(
            session = session,
            inputId = "asset",
            choices = c("None"),
            selected = "None"
          )
          shiny::showNotification(
            "No approved assets found in account. Inject an asset from the approved products list.",
            type = "warning"
          )
        }
      }

      # Set selected asset in dropdown
      set_selected_asset <- function(asset_id) {
        if (length(values$assets_list) == 0) {
          return()
        }

        for (asset_item in values$assets_list) {
          if (asset_item$id == asset_id) {
            asset_display <- paste0(asset_item$name, " (", asset_item$id, ")")
            shiny::updateSelectInput(
              session = session,
              inputId = "asset",
              selected = asset_display
            )
            break
          }
        }
      }

      # Inject asset
      shiny::observeEvent(input$inject_asset, {
        account_id_raw <- input$account_id
        token_raw <- kdot::get_token(input$token)

        validation <- kdot::validate_inputs(account_id_raw, token_raw)
        if (!validation$valid) {
          shiny::showNotification(validation$error, type = "error")
          return()
        }

        account_id <- validation$account_id
        token <- validation$token

        if (is.null(values$account_data)) {
          shiny::showNotification("Please load account information first", type = "error")
          return()
        }

        selected_product <- input$product
        if (is.null(selected_product) || selected_product == "No products loaded") {
          shiny::showNotification("Please select a product from the dropdown", type = "error")
          return()
        }

        product_id <- values$product_map[[selected_product]]
        if (is.null(product_id)) {
          shiny::showNotification("Invalid product selected", type = "error")
          return()
        }

        account_number <- values$account_data$number
        if (is.null(account_number)) {
          shiny::showNotification("Account number not found in account data", type = "error")
          return()
        }

        cat("[DEBUG] Injecting asset - Product:", product_id, "\n")

        # Use helper function to mask account number
        masked_account_number <- mask_account_number(account_number)

        # Update steps - append to history
        steps_text <- paste0(
          "<strong>Asset Injection Process:</strong><br><br>",
          "<strong>Step 1:</strong> Preparing asset injection...<br>",
          "&nbsp;&nbsp;• Product: ", selected_product, "<br>",
          "&nbsp;&nbsp;• Product ID: ", product_id, "<br>",
          "&nbsp;&nbsp;• Account Number: ", masked_account_number, "<br>",
          "&nbsp;&nbsp;• Status: Manually Managed<br><br>",
          "<strong>Step 2:</strong> Creating asset via API...<br>"
        )
        append_to_steps(steps_text)

        shiny::withProgress(message = "Injecting asset...", value = 0, {
          shiny::incProgress(0.5)

          inject_result <- kdot::post_asset(as.integer(account_id), account_number, product_id, token)

          shiny::incProgress(1)

          if (inject_result$success) {
            new_asset_id <- inject_result$asset_id
            cat("[DEBUG] Asset injected, ID:", new_asset_id, "\n")

            steps_text <- paste0(
              "<br><strong>Step 3:</strong> Asset created successfully!<br>",
              if (!is.null(new_asset_id)) paste0("&nbsp;&nbsp;• New Asset ID: ", new_asset_id, "<br>") else "",
              "<br><strong>Step 4:</strong> Refreshing assets list...<br>"
            )

            append_to_steps(steps_text)
            shiny::showNotification("Asset injected successfully!", type = "message")

            # Refresh assets
            load_assets()

            if (!is.null(new_asset_id)) {
              set_selected_asset(new_asset_id)
              shiny::updateCheckboxInput(session = session, inputId = "is_sma", value = TRUE)

              steps_text <- paste0(
                "<br><strong>Step 5:</strong> Process Complete!<br>",
                "&nbsp;&nbsp;• Asset ", new_asset_id, " automatically selected<br>",
                "&nbsp;&nbsp;• Ready to configure SMA settings<br>"
              )
            } else {
              steps_text <- paste0(
                "<br><strong>Step 5:</strong> Process Complete!<br>",
                "&nbsp;&nbsp;• Asset injected but ID not found in response<br>",
                "&nbsp;&nbsp;• Please select the asset manually from dropdown<br>"
              )
            }

            append_to_steps(steps_text)
          } else {
            cat("[ERROR] Asset injection failed:", inject_result$error, "\n")
            shiny::showNotification(inject_result$error, type = "error")
            error_text <- paste0(
              "<strong>Asset Injection Failed:</strong><br>",
              inject_result$error, "<br>"
            )
            append_to_steps(error_text)
          }
        })
      })

      # Update SMA settings
      shiny::observeEvent(input$update_sma, {
        account_id_raw <- input$account_id
        token_raw <- kdot::get_token(input$token)

        validation <- kdot::validate_inputs(account_id_raw, token_raw)
        if (!validation$valid) {
          shiny::showNotification(validation$error, type = "error")
          return()
        }

        account_id <- validation$account_id
        token <- validation$token

        selected_asset <- input$asset
        if (is.null(selected_asset) || selected_asset == "None" || grepl("No assets", selected_asset)) {
          shiny::showNotification(
            "Please inject an approved asset first, or select an existing approved asset from the dropdown",
            type = "error"
          )
          return()
        }

        # Extract asset ID from selection
        if (grepl("\\(", selected_asset) && grepl("\\)", selected_asset)) {
          asset_id_str <- regmatches(selected_asset, regexpr("\\([0-9]+\\)", selected_asset))
          asset_id_str <- gsub("[()]", "", asset_id_str)
          sma_asset_id <- as.integer(asset_id_str)
        } else {
          shiny::showNotification("Please select an asset from the dropdown", type = "error")
          return()
        }

        is_sma <- input$is_sma
        cat("[DEBUG] Updating SMA - Asset:", sma_asset_id, ", isSMA:", is_sma, "\n")

        # Get current SMA values for comparison
        current_sma_values <- extract_sma_values(values$account_data)

        old_values <- list(
          isSMA = current_sma_values$isSMA,
          eclipseSMA = current_sma_values$eclipseSMA,
          smaAssetID = current_sma_values$smaAssetID
        )

        values$expected_values <- list(
          isSMA = is_sma,
          eclipseSMA = "False",
          smaAssetID = sma_asset_id
        )

        # Preview update - append to history
        sma_object <- list(
          isSma = is_sma,
          eclipseSMA = "False",
          smaAssetId = sma_asset_id
        )
        preview_text <- paste0(
          "<strong>SMA Settings Update Process:</strong><br><br>",
          "<strong>Step 1:</strong> Review Update<br><br>",
          "Account ID: ", account_id, "<br><br>",
          "<strong>SMA Object to Update:</strong><br>",
          "<pre>", jsonlite::toJSON(sma_object, pretty = TRUE, auto_unbox = TRUE), "</pre><br>",
          "<strong>Step 2:</strong> Sending update to API...<br>"
        )
        append_to_steps(preview_text)

        shiny::withProgress(message = "Updating SMA settings...", value = 0, {
          shiny::incProgress(0.5)
          sma_result <- kdot::put_account_sma(account_id, is_sma, sma_asset_id, token)

          shiny::incProgress(1)

          if (sma_result$success) {
            cat("[DEBUG] SMA settings updated\n")
            new_values <- list(
              isSMA = is_sma,
              eclipseSMA = "False",
              smaAssetID = sma_asset_id
            )

            # Format change messages inline
            old_is_sma <- if (is.null(old_values$isSMA) || length(old_values$isSMA) == 0) FALSE else old_values$isSMA
            new_is_sma <- if (is.null(new_values$isSMA) || length(new_values$isSMA) == 0) FALSE else new_values$isSMA
            
            is_sma_change <- if (old_is_sma != new_is_sma) {
              paste0("&nbsp;&nbsp;• isSMA: ", old_is_sma, " → ", new_is_sma, "<br>")
            } else {
              paste0("&nbsp;&nbsp;• isSMA: ", new_is_sma, " (no change)<br>")
            }

            sma_asset_id_old <- ifelse(is.null(old_values$smaAssetID), "N/A", old_values$smaAssetID)
            sma_asset_id_change <- if (sma_asset_id_old != new_values$smaAssetID) {
              paste0("&nbsp;&nbsp;• smaAssetID: ", sma_asset_id_old, " → ", new_values$smaAssetID, "<br>")
            } else {
              paste0("&nbsp;&nbsp;• smaAssetID: ", new_values$smaAssetID, " (no change)<br>")
            }

            success_msg <- paste0(
              "<strong>Step 3:</strong> Update Complete!<br><br>",
              "Account ID: ", account_id, "<br><br>",
              "<strong>Changes applied:</strong><br>",
              is_sma_change,
              if (old_values$eclipseSMA != "False") {
                paste0("&nbsp;&nbsp;• eclipseSMA: ", old_values$eclipseSMA, " → \"False\" (always set to string \"False\")<br>")
              } else {
                "&nbsp;&nbsp;• eclipseSMA: \"False\" (no change)<br>"
              },
              sma_asset_id_change,
              "<br><strong>Step 4:</strong> Refreshing account data to verify...<br>"
            )

            append_to_steps(success_msg)
            shiny::showNotification("SMA settings updated successfully!", type = "message")

            # Trigger refresh after a short delay
            shiny::invalidateLater(500, session)
            values$refresh_trigger <- values$refresh_trigger + 1
          } else {
            cat("[ERROR] SMA update failed:", sma_result$error, "\n")
            shiny::showNotification(sma_result$error, type = "error")
            error_text <- paste0(
              "<br><strong>Step 3:</strong> Update Failed<br>",
              sma_result$error, "<br>"
            )
            append_to_steps(error_text)
          }
        })
      })

      # Observe refresh trigger to verify SMA update
      shiny::observe({
        shiny::req(values$refresh_trigger > 0)
        shiny::req(values$expected_values)

        account_id <- trimws(input$account_id)
        token <- kdot::get_token(input$token)

        if (account_id != "" && !is.null(token) && token != "") {
          verify_result <- kdot::get_account(account_id, token, expand = "Sma,Portfolio")

          if (verify_result$success) {
            values$account_data <- verify_result$data

            # Verify values
            actual_values <- extract_sma_values(verify_result$data)
            expected <- values$expected_values

            is_sma_match <- actual_values$isSMA == expected$isSMA
            eclipse_sma_match <- actual_values$eclipseSMA == expected$eclipseSMA

            # Use helper function for NULL comparison
            asset_id_match <- safe_equals(actual_values$smaAssetID, expected$smaAssetID)

            all_match <- is_sma_match && eclipse_sma_match && asset_id_match

            cat("[DEBUG] Verification:", ifelse(all_match, "PASSED", "FAILED"), "\n")

            verify_msg <- paste0(
              "<br><strong>Step 5:</strong> ", ifelse(all_match, "Verification Complete!", "Verification Warning"), "<br><br>",
              if (all_match) "All values confirmed successfully:<br>" else "Some values don't match expected:<br>"
            )

            fields <- list(
              list(name = "isSMA", actual = actual_values$isSMA, expected = expected$isSMA),
              list(name = "eclipseSMA", actual = actual_values$eclipseSMA, expected = expected$eclipseSMA),
              list(name = "smaAssetID", actual = actual_values$smaAssetID, expected = expected$smaAssetID)
            )

            for (field in fields) {
              # Use helper function for NULL comparison
              match <- safe_equals(field$actual, field$expected)

              if (all_match || match) {
                verify_msg <- paste0(
                  verify_msg, "&nbsp;&nbsp;• ", field$name, ": ",
                  ifelse(is.null(field$actual), "NULL", as.character(field$actual)), "<br>"
                )
              } else {
                verify_msg <- paste0(
                  verify_msg,
                  "&nbsp;&nbsp;• ", field$name, ": Expected ",
                  ifelse(is.null(field$expected), "NULL", as.character(field$expected)),
                  ", Got ",
                  ifelse(is.null(field$actual), "NULL", as.character(field$actual)), "<br>"
                )
              }
            }

            append_to_steps(verify_msg)
            populate_settings()
            values$expected_values <- NULL # Clear after verification
          }
        }
      })

      # Update Model Aggregate
      shiny::observeEvent(input$update_model_agg, {
        account_id_raw <- input$account_id
        token_raw <- kdot::get_token(input$token)

        validation <- kdot::validate_inputs(account_id_raw, token_raw)
        if (!validation$valid) {
          shiny::showNotification(validation$error, type = "error")
          return()
        }

        account_id <- validation$account_id
        token <- validation$token

        selected_model <- input$model_agg
        if (is.null(selected_model) || selected_model == "No models loaded") {
          shiny::showNotification("Please select a model aggregate from the dropdown", type = "error")
          return()
        }

        model_agg_id <- values$model_agg_map[[selected_model]]
        if (is.null(model_agg_id)) {
          shiny::showNotification("Invalid model aggregate selected", type = "error")
          return()
        }

        cat("[DEBUG] Updating model aggregate - Model:", selected_model, ", ID:", model_agg_id, "\n")

        # Update steps - append to history
        steps_text <- paste0(
          "<strong>Model Aggregate Update Process:</strong><br><br>",
          "<strong>Step 1:</strong> Preparing model aggregate assignment...<br>",
          "&nbsp;&nbsp;• Account ID: ", account_id, "<br>",
          "&nbsp;&nbsp;• Model Aggregate: ", selected_model, "<br>",
          "&nbsp;&nbsp;• Model Aggregate ID: ", model_agg_id, "<br><br>",
          "<strong>Step 2:</strong> Sending update to API...<br>"
        )
        append_to_steps(steps_text)

        shiny::withProgress(message = "Updating model aggregate...", value = 0, {
          shiny::incProgress(0.5)

          result <- kdot::patch_account_model_aggregate(as.integer(account_id), model_agg_id, token)

          shiny::incProgress(1)

          if (result$success) {
            cat("[DEBUG] Model aggregate updated successfully\n")

            steps_text <- paste0(
              "<br><strong>Step 3:</strong> Update Complete!<br><br>",
              "Account ID: ", account_id, "<br>",
              "Model Aggregate: ", selected_model, " (ID: ", model_agg_id, ")<br>",
              "<br><strong>Model aggregate assigned successfully!</strong><br>"
            )

            append_to_steps(steps_text)
            shiny::showNotification("Model aggregate updated successfully!", type = "message")

            # Refresh account data to show updated model
            account_result <- kdot::get_account(account_id, token, expand = "Sma,Portfolio")
            if (account_result$success) {
              values$account_data <- account_result$data
              populate_settings()
            }
          } else {
            cat("[ERROR] Model aggregate update failed:", result$error, "\n")
            shiny::showNotification(result$error, type = "error")
            error_text <- paste0(
              "<br><strong>Step 3:</strong> Update Failed<br>",
              result$error, "<br>"
            )
            append_to_steps(error_text)
          }
        })
      })

      # Handle CSV file upload and preview
      shiny::observeEvent(input$bulk_csv_file, {
        req(input$bulk_csv_file)

        tryCatch(
          {
            csv_data <- readr::read_csv(input$bulk_csv_file$datapath, show_col_types = FALSE)
            validation <- validate_bulk_csv(csv_data)

            if (validation$valid) {
              values$bulk_csv_data <- validation$data
              values$bulk_results <- NULL

              preview_text <- paste0(
                "<div style='margin-top: 10px; padding: 10px; background-color: #d4edda; border: 1px solid #c3e6cb; border-radius: 4px;'>",
                "<strong>✓ CSV validated successfully!</strong><br>",
                "Found ", validation$total_rows, " valid rows.<br>",
                "<small>Columns: ", validation$account_col, ", ", validation$product_col, "</small>",
                "</div>"
              )

              output$csv_preview <- shiny::renderUI(shiny::HTML(preview_text))
            } else {
              values$bulk_csv_data <- NULL
              preview_text <- paste0(
                "<div style='margin-top: 10px; padding: 10px; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px;'>",
                "<strong>✗ CSV validation failed:</strong><br>",
                validation$error,
                "</div>"
              )
              output$csv_preview <- shiny::renderUI(shiny::HTML(preview_text))
              output$bulk_results_summary <- shiny::renderUI(shiny::HTML(""))
              output$bulk_results_table <- NULL
            }
          },
          error = function(e) {
            values$bulk_csv_data <- NULL
            preview_text <- paste0(
              "<div style='margin-top: 10px; padding: 10px; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px;'>",
              "<strong>✗ Error reading CSV:</strong><br>",
              e$message,
              "</div>"
            )
            output$csv_preview <- shiny::renderUI(shiny::HTML(preview_text))
          }
        )
      })

      # Handle bulk CSV processing
      shiny::observeEvent(input$process_bulk_csv, {
        # Validate token
        token <- kdot::get_token(input$token)
        if (is.null(token) || token == "") {
          shiny::showNotification("API token required for bulk processing", type = "error")
          return()
        }

        # Validate CSV data
        if (is.null(values$bulk_csv_data) || nrow(values$bulk_csv_data) == 0) {
          shiny::showNotification("Please upload a valid CSV file first", type = "error")
          return()
        }

        # Prevent multiple simultaneous runs
        if (values$bulk_processing) {
          shiny::showNotification("Bulk processing already in progress", type = "warning")
          return()
        }

        values$bulk_processing <- TRUE
        csv_data <- values$bulk_csv_data
        total_rows <- nrow(csv_data)
        results_list <- list()

        # Clear previous results
        output$bulk_results <- shiny::renderUI(shiny::HTML(""))

        # Process each row
        shiny::withProgress(message = "Processing bulk CSV...", value = 0, {
          for (i in 1:total_rows) {
            account_id <- csv_data$AccountID[i]
            product_id <- csv_data$ProductID[i]

            # Update progress
            progress_msg <- paste0(
              "Processing row ", i, " of ", total_rows, "<br>",
              "Account ID: ", account_id, " | Product ID: ", product_id
            )
            shiny::incProgress(1 / total_rows, detail = progress_msg)

            output$bulk_progress <- shiny::renderUI(render_progress_message(progress_msg))

            # Process this account-product pair
            result <- kdot::process_single_account_product(account_id, product_id, token)

            # Store result
            results_list[[i]] <- result

            # Small delay to avoid overwhelming the API
            Sys.sleep(0.1)
          }
        })

        # Convert results to data frame
        results_df <- tryCatch(
          {
            do.call(rbind, lapply(results_list, function(r) {
              data.frame(
                AccountID = r$account_id,
                ProductID = r$product_id,
                Status = r$status,
                Message = r$message,
                AssetID = ifelse(is.null(r$asset_id), NA, r$asset_id),
                stringsAsFactors = FALSE
              )
            }))
          },
          error = function(e) {
            stop(e)
          }
        )

        values$bulk_results <- results_df
        values$bulk_processing <- FALSE

        # Clear progress
        output$bulk_progress <- shiny::renderUI(shiny::HTML(""))

        # Display results
        success_count <- sum(results_df$Status == "Success")
        error_count <- sum(results_df$Status == "Error")
        skipped_count <- sum(results_df$Status %in% c("Skipped (already exists)", "Success (already existed)"))
        partial_count <- sum(results_df$Status == "Partial Success")

        summary_text <- paste0(
          "<div style='margin-top: 15px; padding: 15px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 4px;'>",
          "<h5>Processing Complete!</h5>",
          "<strong>Summary:</strong><br>",
          "&nbsp;&nbsp;• Total processed: ", total_rows, "<br>",
          "&nbsp;&nbsp;• Success: <span style='color: #28a745;'>", success_count, "</span><br>",
          "&nbsp;&nbsp;• Skipped (already existed): <span style='color: #ffc107;'>", skipped_count, "</span><br>",
          "&nbsp;&nbsp;• Partial success: <span style='color: #fd7e14;'>", partial_count, "</span><br>",
          "&nbsp;&nbsp;• Errors: <span style='color: #dc3545;'>", error_count, "</span><br>",
          "</div>"
        )

        output$bulk_results_summary <- shiny::renderUI(shiny::HTML(summary_text))

        # Create results table using helper function
        output$bulk_results_table <- tryCatch(
          {
            DT::renderDT({
              DT::datatable(
                results_df,
                options = get_dt_options("bulk_results"),
                extensions = "Buttons",
                rownames = FALSE,
                filter = "top"
              ) |>
                DT::formatStyle(
                  "Status",
                  backgroundColor = DT::styleEqual(
                    c("Success", "Success (already existed)", "Skipped (already exists)", "Partial Success", "Error"),
                    c("#d4edda", "#d4edda", "#fff3cd", "#ffeaa7", "#f8d7da")
                  )
                )
            })
          },
          error = function(e) {
            shiny::showNotification(paste("Error rendering results table:", e$message), type = "error")
            NULL
          }
        )

        shiny::showNotification(
          paste("Bulk processing complete! Processed", total_rows, "rows."),
          type = "message",
          duration = 5
        )
      })

      # Handle SMA Settings Checker
      shiny::observeEvent(input$run_checker, {
        # Validate token
        token <- kdot::get_token(input$token)
        if (is.null(token) || token == "") {
          shiny::showNotification("API token required for checking SMA settings", type = "error")
          return()
        }

        # Parse account IDs from input
        account_ids_text <- trimws(input$checker_account_ids)
        if (account_ids_text == "") {
          shiny::showNotification("Please enter at least one Account ID", type = "error")
          return()
        }

        # Prevent multiple simultaneous runs
        if (values$checker_processing) {
          shiny::showNotification("Checker already in progress", type = "warning")
          return()
        }

        values$checker_processing <- TRUE

        # Use helper function to parse account IDs
        account_ids <- parse_account_ids(account_ids_text)

        if (length(account_ids) == 0) {
          shiny::showNotification("No valid Account IDs found", type = "error")
          values$checker_processing <- FALSE
          return()
        }

        total_accounts <- length(account_ids)
        results_list <- list()

        # Clear previous results
        output$checker_results_table <- NULL

        # Process each account
        shiny::withProgress(message = "Checking SMA settings...", value = 0, {
          for (i in 1:total_accounts) {
            account_id <- account_ids[i]

            # Update progress
            progress_msg <- paste0("Checking account ", i, " of ", total_accounts, ": ", account_id)
            shiny::incProgress(1 / total_accounts, detail = progress_msg)

            output$checker_progress <- shiny::renderUI(render_progress_message(progress_msg))

            # Check this account
            result <- kdot::check_account_sma(account_id, token)
            results_list[[i]] <- result

            # Small delay to avoid overwhelming the API
            Sys.sleep(0.1)
          }
        })

        # Convert results to data frame
        results_df <- tryCatch(
          {
            do.call(rbind, lapply(results_list, function(r) {
              # Use helper function to mask account number
              masked_account_number <- mask_account_number(r$account_number)

              data.frame(
                AccountID = r$account_id,
                AccountName = ifelse(is.null(r$account_name), "N/A", r$account_name),
                AccountNumber = masked_account_number,
                isSMA = r$isSMA,
                eclipseSMA = r$eclipseSMA,
                smaAssetID = ifelse(is.null(r$smaAssetID), "None", as.character(r$smaAssetID)),
                Status = r$status,
                Message = r$message,
                stringsAsFactors = FALSE
              )
            }))
          },
          error = function(e) {
            shiny::showNotification(paste("Error creating results table:", e$message), type = "error")
            NULL
          }
        )

        values$checker_results <- results_df
        values$checker_processing <- FALSE

        # Clear progress
        output$checker_progress <- shiny::renderUI(shiny::HTML(""))

        if (!is.null(results_df)) {
          # Display summary
          success_count <- sum(results_df$Status == "Success")
          error_count <- sum(results_df$Status == "Error")
          sma_enabled_count <- sum(results_df$isSMA == TRUE)
          sma_asset_count <- sum(results_df$smaAssetID != "None")

          summary_text <- paste0(
            "<div style='margin-top: 15px; padding: 15px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 4px;'>",
            "<h5>Check Complete!</h5>",
            "<strong>Summary:</strong><br>",
            "&nbsp;&nbsp;• Total checked: ", total_accounts, "<br>",
            "&nbsp;&nbsp;• Successfully retrieved: <span style='color: #28a745;'>", success_count, "</span><br>",
            "&nbsp;&nbsp;• Errors: <span style='color: #dc3545;'>", error_count, "</span><br>",
            "&nbsp;&nbsp;• Accounts with SMA enabled: <span style='color: #17a2b8;'>", sma_enabled_count, "</span><br>",
            "&nbsp;&nbsp;• Accounts with SMA Asset ID: <span style='color: #17a2b8;'>", sma_asset_count, "</span><br>",
            "</div>"
          )

          output$checker_summary <- shiny::renderUI(shiny::HTML(summary_text))

          # Create results table using helper function
          output$checker_results_table <- DT::renderDT({
            DT::datatable(
              results_df,
              options = get_dt_options("sma_checker_results"),
              extensions = "Buttons",
              rownames = FALSE,
              filter = "top"
            ) |>
              DT::formatStyle(
                "Status",
                backgroundColor = DT::styleEqual(
                  c("Success", "Error"),
                  c("#d4edda", "#f8d7da")
                )
              ) |>
              DT::formatStyle(
                "isSMA",
                backgroundColor = DT::styleEqual(
                  c(TRUE, FALSE),
                  c("#d1ecf1", "#fff3cd")
                )
              )
          })

          shiny::showNotification(
            paste("SMA check complete! Processed", total_accounts, "account(s)."),
            type = "message",
            duration = 5
          )
        }
      })
    }
  )
}
