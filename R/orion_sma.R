# Orion SMA Settings Module

# API Configuration
BASE_URL <- "https://api.orionadvisor.com"
PRODUCTS_CSV_PATH <- "long-short-products.csv"
VERIFY_SSL <- FALSE
TOKEN_MASK_LENGTH <- 100L

# Helper Functions ----

# Load products from CSV file
load_products_from_csv <- function(csv_path = PRODUCTS_CSV_PATH) {
  if (!file.exists(csv_path)) {
    return(list())
  }

  cat("[DEBUG] Loading products from CSV:", csv_path, "\n")
  tryCatch(
    {
      products_df <- readr::read_csv(csv_path, show_col_types = FALSE)
      cat("[DEBUG] CSV loaded successfully, rows:", nrow(products_df), "\n")

      # Extract Product ID and Product Name
      products_list <- Map(
        function(id, name) list(id = as.integer(id), name = as.character(name)),
        products_df$`Product ID`,
        products_df$`Product Name`
      )

      cat("[DEBUG] Products loaded successfully, total:", length(products_list), "\n")
      return(products_list)
    },
    error = function(e) {
      cat("[ERROR] Error loading products CSV:", e$message, "\n")
      return(list())
    }
  )
}

# Get token from input or environment variable
get_token <- function(token_input = NULL) {
  # Check input token first
  if (!is.null(token_input)) {
    token_trimmed <- trimws(token_input)
    masked_value <- paste(rep("*", TOKEN_MASK_LENGTH), collapse = "")
    if (nzchar(token_trimmed) && token_trimmed != masked_value) {
      return(token_trimmed)
    }
  }

  # Fall back to environment variable
  env_token <- Sys.getenv("MA_ORION_API_TOKEN")
  if (nzchar(env_token)) env_token else NULL
}

# Get request headers
get_headers <- function(token) {
  list(
    "Authorization" = paste("Bearer", token),
    "Accept" = "application/json",
    "Content-Type" = "application/json"
  )
}

# Extract asset ID from asset data
extract_asset_id <- function(asset_data) {
  if (!is.list(asset_data)) {
    return(NULL)
  }

  # Asset injection response has id at top level
  if (!is.null(asset_data$id)) {
    return(as.integer(asset_data$id))
  }

  # Assets list also has id at top level, but check portfolio as fallback
  if ("portfolio" %in% names(asset_data) && !is.null(asset_data$portfolio$id)) {
    return(as.integer(asset_data$portfolio$id))
  }

  return(NULL)
}

# Extract product ID from asset data
extract_product_id <- function(asset_data) {
  if (!is.list(asset_data)) {
    return(NULL)
  }

  # Assets list has productId at top level
  if (!is.null(asset_data$productId)) {
    return(as.integer(asset_data$productId))
  }

  # Asset injection response has productId nested in portfolio
  if ("portfolio" %in% names(asset_data) && !is.null(asset_data$portfolio$productId)) {
    return(as.integer(asset_data$portfolio$productId))
  }

  return(NULL)
}

# Extract SMA values from account data
extract_sma_values <- function(account_data) {
  if (!is.list(account_data)) {
    return(list(isSMA = FALSE, eclipseSMA = "None", smaAssetID = NULL))
  }

  sma <- account_data$sma
  if (is.null(sma) || !is.list(sma)) {
    return(list(isSMA = FALSE, eclipseSMA = "None", smaAssetID = NULL))
  }

  list(
    isSMA = ifelse(is.null(sma$isSma), FALSE, as.logical(sma$isSma)),
    eclipseSMA = ifelse(is.null(sma$eclipseSMA), "None", as.character(sma$eclipseSMA)),
    smaAssetID = if (!is.null(sma$smaAssetId)) as.integer(sma$smaAssetId) else NULL
  )
}

# Build asset payload for injection
build_asset_payload <- function(account_id, account_number, product_id) {
  list(
    portfolio = list(
      accountNumber = account_number,
      productId = product_id,
      accountId = account_id,
      isManaged = TRUE,
      isActive = TRUE,
      isAdvisorOnly = TRUE,
      status = "Manually Managed",
      currentShares = 0,
      currentValue = 0
    )
  )
}

# API request wrapper
api_request <- function(method, endpoint, json_data = NULL, params = NULL, token, error_context = "") {
  tryCatch(
    {
      url <- paste0(BASE_URL, endpoint)
      headers <- get_headers(token)

      config <- httr::config(ssl_verifypeer = VERIFY_SSL)

      if (method == "GET") {
        response <- httr::GET(
          url,
          do.call(httr::add_headers, headers),
          query = params,
          config = config,
          httr::timeout(30)
        )
      } else if (method == "POST") {
        response <- httr::POST(
          url,
          do.call(httr::add_headers, headers),
          body = json_data,
          encode = "json",
          config = config,
          httr::timeout(30)
        )
      } else if (method == "PUT") {
        response <- httr::PUT(
          url,
          do.call(httr::add_headers, headers),
          body = json_data,
          encode = "json",
          config = config,
          httr::timeout(30)
        )
      } else {
        return(list(success = FALSE, error = paste("Unsupported HTTP method:", method)))
      }

      status_code <- httr::status_code(response)

      if (status_code %in% c(200, 201, 204)) {
        return(list(success = TRUE, response = response, status_code = status_code))
      } else {
        error_msg <- handle_api_response(response, error_context)
        return(list(success = FALSE, error = error_msg, status_code = status_code))
      }
    },
    error = function(e) {
      return(list(success = FALSE, error = paste("Network Error: Failed to connect to API:", e$message)))
    }
  )
}

# Handle API response errors
handle_api_response <- function(response, error_context = "") {
  status_code <- httr::status_code(response)
  response_text <- httr::content(response, as = "text", encoding = "UTF-8")
  response_preview <- substr(response_text, 1, 200)

  if (status_code == 400) {
    return(paste("Validation Error - Bad Request:", error_context, "\n\nResponse:", response_preview))
  } else if (status_code == 401) {
    return("Authentication Error - Invalid API token")
  } else if (status_code == 404) {
    return(paste("Not Found - Resource not found:", error_context, "\n\nResponse:", response_preview))
  } else if (status_code == 405) {
    return(paste("API Error - Method Not Allowed (405):", error_context, "\n\nResponse:", response_preview))
  } else if (status_code == 500) {
    return(paste("Server Error - Internal server error:", error_context, "\n\nResponse:", response_preview))
  } else {
    return(paste("API Error", status_code, ":", response_preview))
  }
}

# Validate account ID
validate_account_id <- function(account_id) {
  if (is.null(account_id) || trimws(account_id) == "") {
    return(FALSE)
  }

  account_id_clean <- trimws(account_id)
  if (grepl("^[0-9]+$", account_id_clean)) {
    return(TRUE)
  }

  return(FALSE)
}

# Validate inputs (account ID and token)
validate_inputs <- function(account_id, token) {
  account_id <- trimws(account_id)
  if (account_id == "" || !validate_account_id(account_id)) {
    return(list(valid = FALSE, error = "Please enter a valid Account ID"))
  }
  if (is.null(token) || token == "") {
    return(list(valid = FALSE, error = "API token not found"))
  }
  return(list(valid = TRUE, account_id = account_id, token = token))
}

# Append to steps history
append_steps <- function(history, new_text, add_separator = FALSE) {
  separator <- if (add_separator && history != "") "<br><br><hr><br>" else ""
  paste0(history, separator, new_text)
}

# Format change message for verification
format_change_message <- function(field_name, old_val, new_val) {
  if (old_val != new_val) {
    paste0("&nbsp;&nbsp;• ", field_name, ": ", old_val, " → ", new_val, "<br>")
  } else {
    paste0("&nbsp;&nbsp;• ", field_name, ": ", new_val, " (no change)<br>")
  }
}

# UI Function ----

orionSmaUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    h1("Orion SMA Settings Updater"),
    shiny::fluidPage(
      # Top row: Token and Account ID side by side
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
              value = if (Sys.getenv("MA_ORION_API_TOKEN") != "") paste(rep("*", TOKEN_MASK_LENGTH), collapse = "") else ""
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

      # Second row: Account info and Asset injection side by side
      shiny::fluidRow(
        # Column 1: Account info display and Process Steps
        shiny::column(
          width = 6,
          shiny::wellPanel(
            h4("Account Information"),
            shiny::htmlOutput(ns("account_info"))
          ),
          shiny::wellPanel(
            h4("Process Steps"),
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
        products = NULL,
        product_map = NULL,
        approved_product_ids = NULL,
        assets_list = NULL,
        asset_product_map = NULL,
        expected_values = NULL,
        refresh_trigger = 0,
        steps_history = "" # Store cumulative steps history
      )

      # Load products on module initialization
      shiny::observe({
        products_list <- load_products_from_csv()
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
      })


      # Save token to .Renviron
      shiny::observeEvent(input$save_token, {
        token <- get_token(input$token)
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


      # Fetch account info
      shiny::observeEvent(input$fetch_account, {
        account_id_raw <- input$account_id
        token_raw <- get_token(input$token)

        validation <- validate_inputs(account_id_raw, token_raw)
        if (!validation$valid) {
          cat("[ERROR]", validation$error, "\n")
          shiny::showNotification(validation$error, type = "error")
          return()
        }

        account_id <- validation$account_id
        token <- validation$token
        cat("[DEBUG] Fetching account:", account_id, "\n")

        shiny::withProgress(message = "Fetching account information...", value = 0, {
          shiny::incProgress(0.5)

          result <- api_request(
            method = "GET",
            endpoint = paste0("/api/v1/Portfolio/Accounts/Verbose/", account_id),
            params = list(expand = "Sma"),
            token = token,
            error_context = paste("Account ID", account_id)
          )

          shiny::incProgress(1)

          if (result$success) {
            response_data <- jsonlite::fromJSON(httr::content(result$response, as = "text", encoding = "UTF-8"))
            values$account_data <- response_data
            populate_settings()
            shiny::showNotification("Account info loaded successfully", type = "message")
            cat("[DEBUG] Account loaded successfully\n")
          } else {
            cat("[ERROR] API request failed:", result$error, "\n")
            shiny::showNotification(result$error, type = "error")
          }
        })
      })

      # Populate settings from account data
      populate_settings <- function() {
        if (is.null(values$account_data)) {
          return()
        }

        account_name <- ifelse(is.null(values$account_data$name), "N/A", values$account_data$name)
        account_number <- ifelse(is.null(values$account_data$number), "N/A", values$account_data$number)

        sma_values <- extract_sma_values(values$account_data)

        # Mask account number
        if (account_number != "N/A" && nchar(account_number) > 4) {
          masked_number <- paste0("****", substr(account_number, nchar(account_number) - 3, nchar(account_number)))
        } else {
          masked_number <- account_number
        }

        # Update account info display
        info_text <- paste0(
          "<strong>Account:</strong> ", account_name, " (", masked_number, ")<br><br>",
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

        token <- get_token(input$token)
        if (is.null(token) || token == "") {
          return()
        }
        result <- api_request(
          method = "GET",
          endpoint = paste0("/api/v1/Portfolio/Accounts/", account_id, "/Assets"),
          token = token,
          error_context = paste("Loading assets for Account ID", account_id)
        )

        if (!result$success) {
          cat("[ERROR] Failed to fetch assets:", result$error, "\n")
          shiny::showNotification(result$error, type = "error")
          return()
        }

        assets_data <- jsonlite::fromJSON(httr::content(result$response, as = "text", encoding = "UTF-8"), simplifyDataFrame = FALSE)

        # API returns direct array of assets
        if (is.data.frame(assets_data)) {
          assets <- lapply(1:nrow(assets_data), function(i) {
            as.list(assets_data[i, ])
          })
        } else if (is.list(assets_data)) {
          assets <- assets_data
        } else {
          assets <- list()
        }
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
        token_raw <- get_token(input$token)

        validation <- validate_inputs(account_id_raw, token_raw)
        if (!validation$valid) {
          cat("[ERROR]", validation$error, "\n")
          shiny::showNotification(validation$error, type = "error")
          return()
        }

        account_id <- validation$account_id
        token <- validation$token

        if (is.null(values$account_data)) {
          cat("[ERROR] No account data loaded\n")
          shiny::showNotification("Please load account information first", type = "error")
          return()
        }

        selected_product <- input$product
        if (is.null(selected_product) || selected_product == "No products loaded") {
          cat("[ERROR] No product selected\n")
          shiny::showNotification("Please select a product from the dropdown", type = "error")
          return()
        }

        product_id <- values$product_map[[selected_product]]
        if (is.null(product_id)) {
          cat("[ERROR] Invalid product selected\n")
          shiny::showNotification("Invalid product selected", type = "error")
          return()
        }

        account_number <- values$account_data$number
        if (is.null(account_number)) {
          cat("[ERROR] Account number not found\n")
          shiny::showNotification("Account number not found in account data", type = "error")
          return()
        }

        cat("[DEBUG] Injecting asset - Product:", product_id, "\n")

        # Update steps - append to history
        steps_text <- paste0(
          "<strong>Asset Injection Process:</strong><br><br>",
          "<strong>Step 1:</strong> Preparing asset injection...<br>",
          "&nbsp;&nbsp;• Product: ", selected_product, "<br>",
          "&nbsp;&nbsp;• Product ID: ", product_id, "<br>",
          "&nbsp;&nbsp;• Account Number: ", account_number, "<br>",
          "&nbsp;&nbsp;• Status: Manually Managed<br><br>",
          "<strong>Step 2:</strong> Creating asset via API...<br>"
        )
        values$steps_history <- append_steps(values$steps_history, steps_text, add_separator = TRUE)
        output$steps <- shiny::renderUI(shiny::HTML(values$steps_history))

        shiny::withProgress(message = "Injecting asset...", value = 0, {
          shiny::incProgress(0.5)

          payload <- build_asset_payload(as.integer(account_id), account_number, product_id)
          result <- api_request(
            method = "POST",
            endpoint = "/api/v1/Portfolio/Assets/Verbose",
            json_data = payload,
            token = token,
            error_context = "Invalid data. Check Product ID."
          )

          shiny::incProgress(1)

          if (result$success) {
            response_data <- jsonlite::fromJSON(httr::content(result$response, as = "text", encoding = "UTF-8"))
            new_asset_id <- extract_asset_id(response_data)
            cat("[DEBUG] Asset injected, ID:", new_asset_id, "\n")

            steps_text <- paste0(
              "<br><strong>Step 3:</strong> Asset created successfully!<br>"
            )

            if (!is.null(new_asset_id)) {
              steps_text <- paste0(
                steps_text,
                "&nbsp;&nbsp;• New Asset ID: ", new_asset_id, "<br>"
              )
            }

            steps_text <- paste0(
              steps_text,
              "<br><strong>Step 4:</strong> Refreshing assets list...<br>"
            )

            values$steps_history <- append_steps(values$steps_history, steps_text)
            output$steps <- shiny::renderUI(shiny::HTML(values$steps_history))

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

            values$steps_history <- append_steps(values$steps_history, steps_text)
            output$steps <- shiny::renderUI(shiny::HTML(values$steps_history))
          } else {
            cat("[ERROR] Asset injection failed:", result$error, "\n")
            shiny::showNotification(result$error, type = "error")
            error_text <- paste0(
              "<strong>Asset Injection Failed:</strong><br>",
              result$error, "<br>"
            )
            values$steps_history <- append_steps(values$steps_history, error_text, add_separator = TRUE)
            output$steps <- shiny::renderUI(shiny::HTML(values$steps_history))
          }
        })
      })

      # Update SMA settings
      shiny::observeEvent(input$update_sma, {
        account_id_raw <- input$account_id
        token_raw <- get_token(input$token)

        validation <- validate_inputs(account_id_raw, token_raw)
        if (!validation$valid) {
          cat("[ERROR]", validation$error, "\n")
          shiny::showNotification(validation$error, type = "error")
          return()
        }

        account_id <- validation$account_id
        token <- validation$token

        selected_asset <- input$asset
        if (is.null(selected_asset) || selected_asset == "None" || grepl("No assets", selected_asset)) {
          cat("[ERROR] No asset selected\n")
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
          cat("[ERROR] Could not extract asset ID from selection\n")
          shiny::showNotification("Please select an asset from the dropdown", type = "error")
          return()
        }

        is_sma <- input$is_sma
        cat("[DEBUG] Updating SMA - Asset:", sma_asset_id, ", isSMA:", is_sma, "\n")

        sma_object <- list(
          isSma = is_sma,
          eclipseSMA = "False",
          smaAssetId = sma_asset_id
        )

        payload <- list(
          id = NULL,
          modelingInfo = NULL,
          sma = sma_object
        )

        # Get current SMA values for comparison
        current_sma_values <- if (!is.null(values$account_data)) {
          extract_sma_values(values$account_data)
        } else {
          list(isSMA = FALSE, eclipseSMA = "False", smaAssetID = NULL)
        }

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
        preview_text <- paste0(
          "<strong>SMA Settings Update Process:</strong><br><br>",
          "<strong>Step 1:</strong> Review Update<br><br>",
          "Account ID: ", account_id, "<br><br>",
          "<strong>SMA Object to Update:</strong><br>",
          "<pre>", jsonlite::toJSON(sma_object, pretty = TRUE, auto_unbox = TRUE), "</pre><br>",
          "<strong>Step 2:</strong> Sending update to API...<br>"
        )
        values$steps_history <- append_steps(values$steps_history, preview_text, add_separator = TRUE)
        output$steps <- shiny::renderUI(shiny::HTML(values$steps_history))

        shiny::withProgress(message = "Updating SMA settings...", value = 0, {
          shiny::incProgress(0.5)
          result <- api_request(
            method = "PUT",
            endpoint = paste0("/api/v1/Portfolio/Accounts/UpdateDoNotTradeAndSma/", account_id),
            json_data = payload,
            token = token,
            error_context = "SMA settings update"
          )

          shiny::incProgress(1)

          if (result$success) {
            cat("[DEBUG] SMA settings updated\n")
            new_values <- list(
              isSMA = is_sma,
              eclipseSMA = "False",
              smaAssetID = sma_asset_id
            )

            success_msg <- paste0(
              "<strong>Step 3:</strong> Update Complete!<br><br>",
              "Account ID: ", account_id, "<br><br>",
              "<strong>Changes applied:</strong><br>",
              format_change_message("isSMA", old_values$isSMA, new_values$isSMA),
              if (old_values$eclipseSMA != "False") {
                paste0("&nbsp;&nbsp;• eclipseSMA: ", old_values$eclipseSMA, " → \"False\" (always set to string \"False\")<br>")
              } else {
                "&nbsp;&nbsp;• eclipseSMA: \"False\" (no change)<br>"
              },
              format_change_message(
                "smaAssetID",
                ifelse(is.null(old_values$smaAssetID), "N/A", old_values$smaAssetID),
                new_values$smaAssetID
              ),
              "<br><strong>Step 4:</strong> Refreshing account data to verify...<br>"
            )

            values$steps_history <- append_steps(values$steps_history, success_msg)
            output$steps <- shiny::renderUI(shiny::HTML(values$steps_history))

            shiny::showNotification("SMA settings updated successfully!", type = "message")

            # Trigger refresh after a short delay
            shiny::invalidateLater(500, session)
            values$refresh_trigger <- values$refresh_trigger + 1
          } else {
            cat("[ERROR] SMA update failed:", result$error, "\n")
            shiny::showNotification(result$error, type = "error")
            error_text <- paste0(
              "<br><strong>Step 3:</strong> Update Failed<br>",
              result$error, "<br>"
            )
            values$steps_history <- append_steps(values$steps_history, error_text)
            output$steps <- shiny::renderUI(shiny::HTML(values$steps_history))
          }
        })
      })

      # Observe refresh trigger to verify SMA update
      shiny::observe({
        shiny::req(values$refresh_trigger > 0)
        shiny::req(values$expected_values)

        account_id <- trimws(input$account_id)
        token <- get_token(input$token)

        if (account_id != "" && !is.null(token) && token != "") {
          result <- api_request(
            method = "GET",
            endpoint = paste0("/api/v1/Portfolio/Accounts/Verbose/", account_id),
            params = list(expand = "Sma"),
            token = token,
            error_context = paste("Account ID", account_id)
          )

          if (result$success) {
            response_data <- jsonlite::fromJSON(httr::content(result$response, as = "text", encoding = "UTF-8"))
            values$account_data <- response_data

            # Verify values
            actual_values <- extract_sma_values(response_data)
            expected <- values$expected_values

            is_sma_match <- actual_values$isSMA == expected$isSMA
            eclipse_sma_match <- actual_values$eclipseSMA == expected$eclipseSMA

            # Handle NULL comparison properly
            if (is.null(actual_values$smaAssetID) && is.null(expected$smaAssetID)) {
              asset_id_match <- TRUE
            } else if (is.null(actual_values$smaAssetID) || is.null(expected$smaAssetID)) {
              asset_id_match <- FALSE
            } else {
              asset_id_match <- actual_values$smaAssetID == expected$smaAssetID
            }

            all_match <- is_sma_match && eclipse_sma_match && asset_id_match

            cat("[DEBUG] Verification:", ifelse(all_match, "PASSED", "FAILED"), "\n")

            verify_msg <- paste0(
              "<br><strong>Step 5:</strong> ", ifelse(all_match, "Verification Complete!", "Verification Warning"), "<br><br>"
            )

            if (all_match) {
              verify_msg <- paste0(verify_msg, "All values confirmed successfully:<br>")
            } else {
              verify_msg <- paste0(verify_msg, "Some values don't match expected:<br>")
            }

            fields <- list(
              list(name = "isSMA", actual = actual_values$isSMA, expected = expected$isSMA),
              list(name = "eclipseSMA", actual = actual_values$eclipseSMA, expected = expected$eclipseSMA),
              list(name = "smaAssetID", actual = actual_values$smaAssetID, expected = expected$smaAssetID)
            )

            for (field in fields) {
              match <- field$actual == field$expected
              if (all_match || match) {
                verify_msg <- paste0(verify_msg, "&nbsp;&nbsp;• ", field$name, ": ", field$actual, "<br>")
              } else {
                verify_msg <- paste0(
                  verify_msg,
                  "&nbsp;&nbsp;• ", field$name, ": Expected ", field$expected, ", Got ", field$actual, "<br>"
                )
              }
            }

            values$steps_history <- append_steps(values$steps_history, verify_msg)
            output$steps <- shiny::renderUI(shiny::HTML(values$steps_history))
            populate_settings()
            values$expected_values <- NULL # Clear after verification
          }
        }
      })
    }
  )
}
