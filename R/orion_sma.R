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

# Extract account number from account data
extract_account_number <- function(account_data) {
  if (!is.null(account_data$number)) {
    return(account_data$number)
  } else if (!is.null(account_data$accountNumber)) {
    return(account_data$accountNumber)
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

      # Build headers inline
      headers <- list(
        "Authorization" = paste("Bearer", token),
        "Accept" = "application/json",
        "Content-Type" = "application/json"
      )

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
        response_text <- httr::content(response, as = "text", encoding = "UTF-8")
        response_preview <- substr(response_text, 1, 200)

        if (status_code == 400) {
          error_msg <- paste("Validation Error - Bad Request:", error_context, "\n\nResponse:", response_preview)
        } else if (status_code == 401) {
          error_msg <- "Authentication Error - Invalid API token"
        } else if (status_code == 404) {
          error_msg <- paste("Not Found - Resource not found:", error_context, "\n\nResponse:", response_preview)
        } else if (status_code == 405) {
          error_msg <- paste("API Error - Method Not Allowed (405):", error_context, "\n\nResponse:", response_preview)
        } else if (status_code == 500) {
          error_msg <- paste("Server Error - Internal server error:", error_context, "\n\nResponse:", response_preview)
        } else {
          error_msg <- paste("API Error", status_code, ":", response_preview)
        }

        return(list(success = FALSE, error = error_msg, status_code = status_code))
      }
    },
    error = function(e) {
      return(list(success = FALSE, error = paste("Network Error: Failed to connect to API:", e$message)))
    }
  )
}

# Validate inputs (account ID and token)
validate_inputs <- function(account_id, token) {
  account_id <- trimws(account_id)

  if (account_id == "" || !grepl("^[0-9]+$", account_id)) {
    return(list(valid = FALSE, error = "Please enter a valid Account ID"))
  }

  if (is.null(token) || token == "") {
    return(list(valid = FALSE, error = "API token not found"))
  }

  return(list(valid = TRUE, account_id = account_id, token = token))
}


# Validate bulk CSV structure and data
validate_bulk_csv <- function(csv_data) {
  # #region agent log
  log_file <- "c:\\Users\\AustinBurks\\OneDrive - Mercer Advisors\\Documents\\github\\PMG-Hub\\.cursor\\debug.log"
  tryCatch(
    {
      log_entry <- jsonlite::toJSON(list(
        id = paste0("log_", as.integer(Sys.time()), "_", sample(1000:9999, 1)),
        timestamp = as.integer(Sys.time() * 1000),
        location = "R/orion_sma.R:255",
        message = "validate_bulk_csv entry",
        data = list(
          csv_is_null = is.null(csv_data),
          csv_nrow = ifelse(is.null(csv_data), 0, nrow(csv_data)),
          csv_cols = ifelse(is.null(csv_data), "NULL", paste(names(csv_data), collapse = ","))
        ),
        sessionId = "debug-session",
        runId = "run1",
        hypothesisId = "A"
      ), auto_unbox = TRUE)
      cat(log_entry, "\n", file = log_file, append = TRUE)
    },
    error = function(e) {}
  )
  # #endregion

  if (is.null(csv_data) || nrow(csv_data) == 0) {
    # #region agent log
    tryCatch(
      {
        log_entry <- jsonlite::toJSON(list(
          id = paste0("log_", as.integer(Sys.time()), "_", sample(1000:9999, 1)),
          timestamp = as.integer(Sys.time() * 1000),
          location = "R/orion_sma.R:258",
          message = "validate_bulk_csv early return - empty",
          data = list(valid = FALSE),
          sessionId = "debug-session",
          runId = "run1",
          hypothesisId = "A"
        ), auto_unbox = TRUE)
        cat(log_entry, "\n", file = log_file, append = TRUE)
      },
      error = function(e) {}
    )
    # #endregion
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

  # #region agent log
  tryCatch(
    {
      log_entry <- jsonlite::toJSON(list(
        id = paste0("log_", as.integer(Sys.time()), "_", sample(1000:9999, 1)),
        timestamp = as.integer(Sys.time() * 1000),
        location = "R/orion_sma.R:314",
        message = "validate_bulk_csv exit - success",
        data = list(
          valid = TRUE,
          total_rows = nrow(validated_data),
          account_col = account_col_name,
          product_col = product_col_name,
          sample_account_id = ifelse(nrow(validated_data) > 0, validated_data$AccountID[1], NA),
          sample_product_id = ifelse(nrow(validated_data) > 0, validated_data$ProductID[1], NA)
        ),
        sessionId = "debug-session",
        runId = "run1",
        hypothesisId = "A"
      ), auto_unbox = TRUE)
      cat(log_entry, "\n", file = log_file, append = TRUE)
    },
    error = function(e) {}
  )
  # #endregion

  return(list(
    valid = TRUE,
    data = validated_data,
    account_col = account_col_name,
    product_col = product_col_name,
    total_rows = nrow(validated_data)
  ))
}

# Check if product exists in account assets
check_product_in_account <- function(account_id, product_id, token) {
  result <- api_request(
    method = "GET",
    endpoint = paste0("/api/v1/Portfolio/Accounts/", account_id, "/Assets"),
    token = token,
    error_context = paste("Checking assets for Account ID", account_id)
  )

  if (!result$success) {
    return(list(exists = FALSE, asset_id = NULL, error = result$error))
  }

  tryCatch(
    {
      assets_data <- jsonlite::fromJSON(
        httr::content(result$response, as = "text", encoding = "UTF-8"),
        simplifyDataFrame = FALSE
      )

      # Handle different response formats (matching load_assets pattern)
      if (is.data.frame(assets_data)) {
        # Convert data frame to list of lists
        assets <- lapply(1:nrow(assets_data), function(i) {
          as.list(assets_data[i, ])
        })
      } else if (is.list(assets_data)) {
        # API returns direct array of assets
        assets <- assets_data
      } else {
        assets <- list()
      }

      # Check each asset for matching product ID
      for (asset in assets) {
        if (is.list(asset)) {
          asset_product_id <- extract_product_id(asset)
          asset_id <- extract_asset_id(asset)

          if (!is.null(asset_product_id) && asset_product_id == product_id && !is.null(asset_id)) {
            return(list(exists = TRUE, asset_id = asset_id, error = NULL))
          }
        }
      }

      return(list(exists = FALSE, asset_id = NULL, error = NULL))
    },
    error = function(e) {
      return(list(exists = FALSE, asset_id = NULL, error = paste("Error parsing assets:", e$message)))
    }
  )
}

# Check SMA settings for a single account
check_account_sma <- function(account_id, token) {
  result <- list(
    account_id = account_id,
    account_name = NULL,
    account_number = NULL,
    isSMA = FALSE,
    eclipseSMA = "None",
    smaAssetID = NULL,
    status = "Unknown",
    message = ""
  )

  # Fetch account data with SMA expansion
  account_result <- api_request(
    method = "GET",
    endpoint = paste0("/api/v1/Portfolio/Accounts/Verbose/", account_id),
    params = list(expand = "Sma"),
    token = token,
    error_context = paste("Account ID", account_id)
  )

  if (!account_result$success) {
    result$status <- "Error"
    result$message <- account_result$error
    return(result)
  }

  tryCatch(
    {
      account_data <- jsonlite::fromJSON(
        httr::content(account_result$response, as = "text", encoding = "UTF-8")
      )

      # Extract account info
      result$account_name <- ifelse(is.null(account_data$name), "N/A", account_data$name)
      result$account_number <- extract_account_number(account_data)

      # Extract SMA values
      sma_values <- extract_sma_values(account_data)
      result$isSMA <- sma_values$isSMA
      result$eclipseSMA <- sma_values$eclipseSMA
      result$smaAssetID <- sma_values$smaAssetID

      result$status <- "Success"
      result$message <- "SMA settings retrieved successfully"
    },
    error = function(e) {
      result$status <- "Error"
      result$message <- paste("Error parsing account data:", e$message)
    }
  )

  return(result)
}

# Process a single account-product pair
process_single_account_product <- function(account_id, product_id, token) {
  # #region agent log
  log_file <- "c:\\Users\\AustinBurks\\OneDrive - Mercer Advisors\\Documents\\github\\PMG-Hub\\.cursor\\debug.log"
  tryCatch(
    {
      log_entry <- jsonlite::toJSON(list(
        id = paste0("log_", as.integer(Sys.time()), "_", sample(1000:9999, 1)),
        timestamp = as.integer(Sys.time() * 1000),
        location = "R/orion_sma.R:370",
        message = "process_single_account_product entry",
        data = list(
          account_id = account_id,
          product_id = product_id,
          token_is_null = is.null(token),
          token_length = ifelse(is.null(token), 0, nchar(token))
        ),
        sessionId = "debug-session",
        runId = "run1",
        hypothesisId = "D"
      ), auto_unbox = TRUE)
      cat(log_entry, "\n", file = log_file, append = TRUE)
    },
    error = function(e) {}
  )
  # #endregion

  result <- list(
    account_id = account_id,
    product_id = product_id,
    status = "Unknown",
    message = "",
    asset_id = NULL
  )

  # Step 1: Fetch account data to get account number
  account_result <- api_request(
    method = "GET",
    endpoint = paste0("/api/v1/Portfolio/Accounts/Verbose/", account_id),
    params = list(expand = "Sma"),
    token = token,
    error_context = paste("Account ID", account_id)
  )

  if (!account_result$success) {
    result$status <- "Error"
    result$message <- paste("Failed to fetch account:", account_result$error)
    return(result)
  }

  account_data <- jsonlite::fromJSON(
    httr::content(account_result$response, as = "text", encoding = "UTF-8")
  )

  # Extract account number
  account_number <- extract_account_number(account_data)

  if (is.null(account_number)) {
    result$status <- "Error"
    result$message <- "Account number not found in account data"
    return(result)
  }

  # Step 2: Check if product already exists in account
  check_result <- check_product_in_account(account_id, product_id, token)

  if (!is.null(check_result$error) && check_result$error != "") {
    result$status <- "Error"
    result$message <- paste("Error checking assets:", check_result$error)
    return(result)
  }

  asset_id <- NULL

  if (check_result$exists) {
    # Product already exists, use existing asset ID
    asset_id <- check_result$asset_id
    result$status <- "Skipped (already exists)"
    result$message <- paste("Product already exists in account with Asset ID:", asset_id)
  } else {
    # Step 3: Inject asset
    payload <- build_asset_payload(as.integer(account_id), account_number, product_id)
    inject_result <- api_request(
      method = "POST",
      endpoint = "/api/v1/Portfolio/Assets/Verbose",
      json_data = payload,
      token = token,
      error_context = paste("Injecting product", product_id, "into account", account_id)
    )

    if (!inject_result$success) {
      result$status <- "Error"
      result$message <- paste("Failed to inject asset:", inject_result$error)
      return(result)
    }

    # Extract new asset ID from response
    response_data <- jsonlite::fromJSON(
      httr::content(inject_result$response, as = "text", encoding = "UTF-8")
    )
    asset_id <- extract_asset_id(response_data)

    if (is.null(asset_id)) {
      result$status <- "Error"
      result$message <- "Asset injected but Asset ID not found in response"
      return(result)
    }

    result$status <- "Injected"
    result$message <- paste("Asset injected successfully, Asset ID:", asset_id)
  }

  result$asset_id <- asset_id

  # Step 4: Update SMA settings
  sma_object <- list(
    isSma = TRUE,
    eclipseSMA = "False",
    smaAssetId = asset_id
  )

  sma_payload <- list(
    id = NULL,
    modelingInfo = NULL,
    sma = sma_object
  )

  sma_result <- api_request(
    method = "PUT",
    endpoint = paste0("/api/v1/Portfolio/Accounts/UpdateDoNotTradeAndSma/", account_id),
    json_data = sma_payload,
    token = token,
    error_context = paste("Updating SMA settings for Account ID", account_id)
  )

  if (!sma_result$success) {
    result$status <- ifelse(result$status == "Skipped (already exists)", "Partial Success", "Partial Success")
    result$message <- paste0(result$message, " | SMA update failed: ", sma_result$error)
    return(result)
  }

  # Success!
  if (result$status == "Skipped (already exists)") {
    result$status <- "Success (already existed)"
  } else {
    result$status <- "Success"
  }
  result$message <- paste0(result$message, " | SMA settings updated successfully")

  # #region agent log
  tryCatch(
    {
      log_entry <- jsonlite::toJSON(list(
        id = paste0("log_", as.integer(Sys.time()), "_", sample(1000:9999, 1)),
        timestamp = as.integer(Sys.time() * 1000),
        location = "R/orion_sma.R:480",
        message = "process_single_account_product exit",
        data = list(
          account_id = result$account_id,
          product_id = result$product_id,
          status = result$status,
          asset_id = result$asset_id,
          message_length = nchar(result$message)
        ),
        sessionId = "debug-session",
        runId = "run1",
        hypothesisId = "D"
      ), auto_unbox = TRUE)
      cat(log_entry, "\n", file = log_file, append = TRUE)
    },
    error = function(e) {}
  )
  # #endregion

  return(result)
}

# UI Function ----

orionSmaUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$style(HTML("
      details.well summary {
        list-style: none;
        position: relative;
        padding-left: 25px;
      }
      details.well summary::-webkit-details-marker {
        display: none;
      }
      details.well summary::before {
        content: '▶';
        position: absolute;
        left: 0;
        color: #666;
        font-size: 14px;
        transition: transform 0.2s;
      }
      details.well[open] summary::before {
        content: '▼';
        transform: rotate(0deg);
      }
    ")),
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
          shiny::tags$details(
            class = "well",
            open = "", # Auto-expand the details element
            style = "margin-bottom: 20px;",
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
          )
        )
      ),

      # Third row: Bulk CSV Processing
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

      # Fourth row: SMA Settings Checker
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
        bulk_csv_data = NULL,
        bulk_results = NULL,
        bulk_processing = FALSE,
        checker_results = NULL,
        checker_processing = FALSE
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

        # Clear steps history if this is a new account ID
        if (!is.null(values$previous_account_id) && values$previous_account_id != account_id) {
          values$steps_history <- ""
          output$steps <- shiny::renderUI(shiny::HTML(""))
        }
        values$previous_account_id <- account_id

        cat("[DEBUG] Fetching account:", account_id, "\n")

        shiny::withProgress(message = "Fetching account information...", value = 0, {
          shiny::incProgress(0.5)

          result <- api_request(
            method = "GET",
            endpoint = paste0("/api/v1/Portfolio/Accounts/Verbose/", account_id),
            params = list(expand = "Sma,Portfolio"),
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
        account_number_raw <- extract_account_number(values$account_data)
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

        # Mask account number
        if (account_number != "N/A" && nchar(account_number) > 4) {
          masked_number <- paste0("****", substr(account_number, nchar(account_number) - 3, nchar(account_number)))
        } else {
          masked_number <- account_number
        }

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

        account_number <- extract_account_number(values$account_data)
        if (is.null(account_number)) {
          cat("[ERROR] Account number not found\n")
          shiny::showNotification("Account number not found in account data", type = "error")
          return()
        }

        cat("[DEBUG] Injecting asset - Product:", product_id, "\n")

        # Mask account number (same logic as populate_settings)
        masked_account_number <- if (account_number != "N/A" && nchar(account_number) > 4) {
          paste0("****", substr(account_number, nchar(account_number) - 3, nchar(account_number)))
        } else {
          account_number
        }

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
        separator <- if (values$steps_history != "") "<br><br><hr><br>" else ""
        values$steps_history <- paste0(values$steps_history, separator, steps_text)
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

            values$steps_history <- paste0(values$steps_history, steps_text)
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

            values$steps_history <- paste0(values$steps_history, steps_text)
            output$steps <- shiny::renderUI(shiny::HTML(values$steps_history))
          } else {
            cat("[ERROR] Asset injection failed:", result$error, "\n")
            shiny::showNotification(result$error, type = "error")
            error_text <- paste0(
              "<strong>Asset Injection Failed:</strong><br>",
              result$error, "<br>"
            )
            separator <- if (values$steps_history != "") "<br><br><hr><br>" else ""
            values$steps_history <- paste0(values$steps_history, separator, error_text)
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
        separator <- if (values$steps_history != "") "<br><br><hr><br>" else ""
        values$steps_history <- paste0(values$steps_history, separator, preview_text)
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

            # Format change messages inline
            is_sma_change <- if (old_values$isSMA != new_values$isSMA) {
              paste0("&nbsp;&nbsp;• isSMA: ", old_values$isSMA, " → ", new_values$isSMA, "<br>")
            } else {
              paste0("&nbsp;&nbsp;• isSMA: ", new_values$isSMA, " (no change)<br>")
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

            values$steps_history <- paste0(values$steps_history, success_msg)
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
            values$steps_history <- paste0(values$steps_history, error_text)
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
              # Handle NULL comparison properly to avoid logical(0) result
              if (is.null(field$actual) && is.null(field$expected)) {
                match <- TRUE
              } else if (is.null(field$actual) || is.null(field$expected)) {
                match <- FALSE
              } else {
                match <- field$actual == field$expected
              }

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

            values$steps_history <- paste0(values$steps_history, verify_msg)
            output$steps <- shiny::renderUI(shiny::HTML(values$steps_history))
            populate_settings()
            values$expected_values <- NULL # Clear after verification
          }
        }
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
        # #region agent log
        log_file <- ".cursor/debug.log"
        tryCatch(
          {
            log_entry <- jsonlite::toJSON(list(
              id = paste0("log_", as.integer(Sys.time()), "_", sample(1000:9999, 1)),
              timestamp = as.integer(Sys.time() * 1000),
              location = "R/orion_sma.R:1409",
              message = "bulk_csv_processing entry",
              data = list(
                bulk_csv_data_is_null = is.null(values$bulk_csv_data),
                bulk_csv_data_nrow = ifelse(is.null(values$bulk_csv_data), 0, nrow(values$bulk_csv_data)),
                bulk_processing = values$bulk_processing
              ),
              sessionId = "debug-session",
              runId = "run1",
              hypothesisId = "B"
            ), auto_unbox = TRUE)
            cat(log_entry, "\n", file = log_file, append = TRUE)
          },
          error = function(e) {}
        )
        # #endregion

        # Validate token
        token <- get_token(input$token)
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

            # #region agent log
            tryCatch(
              {
                log_entry <- jsonlite::toJSON(list(
                  id = paste0("log_", as.integer(Sys.time()), "_", sample(1000:9999, 1)),
                  timestamp = as.integer(Sys.time() * 1000),
                  location = "R/orion_sma.R:1439",
                  message = "bulk_csv_processing loop iteration",
                  data = list(
                    iteration = i,
                    total_rows = total_rows,
                    account_id = account_id,
                    product_id = product_id,
                    results_list_length = length(results_list)
                  ),
                  sessionId = "debug-session",
                  runId = "run1",
                  hypothesisId = "B"
                ), auto_unbox = TRUE)
                cat(log_entry, "\n", file = log_file, append = TRUE)
              },
              error = function(e) {}
            )
            # #endregion

            # Update progress
            progress_msg <- paste0(
              "Processing row ", i, " of ", total_rows, "<br>",
              "Account ID: ", account_id, " | Product ID: ", product_id
            )
            shiny::incProgress(1 / total_rows, detail = progress_msg)

            output$bulk_progress <- shiny::renderUI(shiny::HTML(
              paste0(
                "<div style='margin-top: 10px; padding: 10px; background-color: #e7f3ff; border: 1px solid #b3d9ff; border-radius: 4px;'>",
                "<strong>Processing:</strong> ", progress_msg, "</div>"
              )
            ))

            # Process this account-product pair
            result <- process_single_account_product(account_id, product_id, token)

            # #region agent log
            tryCatch(
              {
                log_entry <- jsonlite::toJSON(list(
                  id = paste0("log_", as.integer(Sys.time()), "_", sample(1000:9999, 1)),
                  timestamp = as.integer(Sys.time() * 1000),
                  location = "R/orion_sma.R:1461",
                  message = "bulk_csv_processing result stored",
                  data = list(
                    iteration = i,
                    result_status = result$status,
                    result_has_asset_id = !is.null(result$asset_id),
                    result_is_list = is.list(result),
                    result_keys = paste(names(result), collapse = ",")
                  ),
                  sessionId = "debug-session",
                  runId = "run1",
                  hypothesisId = "B"
                ), auto_unbox = TRUE)
                cat(log_entry, "\n", file = log_file, append = TRUE)
              },
              error = function(e) {}
            )
            # #endregion

            # Store result
            results_list[[i]] <- result

            # Small delay to avoid overwhelming the API
            Sys.sleep(0.1)
          }
        })

        # Convert results to data frame
        # #region agent log
        tryCatch(
          {
            log_entry <- jsonlite::toJSON(list(
              id = paste0("log_", as.integer(Sys.time()), "_", sample(1000:9999, 1)),
              timestamp = as.integer(Sys.time() * 1000),
              location = "R/orion_sma.R:1469",
              message = "bulk_csv_processing before results_df creation",
              data = list(
                results_list_length = length(results_list),
                first_result_is_null = ifelse(length(results_list) > 0, is.null(results_list[[1]]), NA),
                first_result_keys = ifelse(length(results_list) > 0 && !is.null(results_list[[1]]), paste(names(results_list[[1]]), collapse = ","), "N/A")
              ),
              sessionId = "debug-session",
              runId = "run1",
              hypothesisId = "B"
            ), auto_unbox = TRUE)
            cat(log_entry, "\n", file = log_file, append = TRUE)
          },
          error = function(e) {}
        )
        # #endregion

        results_df <- tryCatch(
          {
            do.call(rbind, lapply(results_list, function(r) {
              # #region agent log
              tryCatch(
                {
                  log_entry <- jsonlite::toJSON(list(
                    id = paste0("log_", as.integer(Sys.time()), "_", sample(1000:9999, 1)),
                    timestamp = as.integer(Sys.time() * 1000),
                    location = "R/orion_sma.R:1475",
                    message = "bulk_csv_processing creating result row",
                    data = list(
                      r_is_null = is.null(r),
                      r_is_list = is.list(r),
                      r_has_account_id = ifelse(is.list(r), "account_id" %in% names(r), FALSE),
                      r_has_product_id = ifelse(is.list(r), "product_id" %in% names(r), FALSE),
                      r_has_status = ifelse(is.list(r), "status" %in% names(r), FALSE)
                    ),
                    sessionId = "debug-session",
                    runId = "run1",
                    hypothesisId = "B"
                  ), auto_unbox = TRUE)
                  cat(log_entry, "\n", file = log_file, append = TRUE)
                },
                error = function(e) {}
              )
              # #endregion

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
            # #region agent log
            tryCatch(
              {
                log_entry <- jsonlite::toJSON(list(
                  id = paste0("log_", as.integer(Sys.time()), "_", sample(1000:9999, 1)),
                  timestamp = as.integer(Sys.time() * 1000),
                  location = "R/orion_sma.R:1495",
                  message = "bulk_csv_processing results_df creation ERROR",
                  data = list(error = e$message),
                  sessionId = "debug-session",
                  runId = "run1",
                  hypothesisId = "B"
                ), auto_unbox = TRUE)
                cat(log_entry, "\n", file = log_file, append = TRUE)
              },
              error = function(e2) {}
            )
            # #endregion
            stop(e)
          }
        )

        # #region agent log
        tryCatch(
          {
            log_entry <- jsonlite::toJSON(list(
              id = paste0("log_", as.integer(Sys.time()), "_", sample(1000:9999, 1)),
              timestamp = as.integer(Sys.time() * 1000),
              location = "R/orion_sma.R:1505",
              message = "bulk_csv_processing after results_df creation",
              data = list(
                results_df_nrow = nrow(results_df),
                results_df_ncol = ncol(results_df),
                results_df_cols = paste(names(results_df), collapse = ","),
                results_df_is_null = is.null(results_df)
              ),
              sessionId = "debug-session",
              runId = "run1",
              hypothesisId = "B"
            ), auto_unbox = TRUE)
            cat(log_entry, "\n", file = log_file, append = TRUE)
          },
          error = function(e) {}
        )
        # #endregion

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

        # Create results table
        # #region agent log
        tryCatch(
          {
            log_entry <- jsonlite::toJSON(list(
              id = paste0("log_", as.integer(Sys.time()), "_", sample(1000:9999, 1)),
              timestamp = as.integer(Sys.time() * 1000),
              location = "R/orion_sma.R:1507",
              message = "bulk_csv_processing before DT table render",
              data = list(
                results_df_nrow = nrow(results_df),
                results_df_ncol = ncol(results_df),
                results_df_has_status_col = "Status" %in% names(results_df),
                dt_available = requireNamespace("DT", quietly = TRUE)
              ),
              sessionId = "debug-session",
              runId = "run1",
              hypothesisId = "C"
            ), auto_unbox = TRUE)
            cat(log_entry, "\n", file = log_file, append = TRUE)
          },
          error = function(e) {}
        )
        # #endregion

        output$bulk_results_table <- tryCatch(
          {
            DT::renderDT({
              DT::datatable(
                results_df,
                options = list(
                  pageLength = 25,
                  lengthMenu = list(c(10, 25, 50, 100, -1), c(10, 25, 50, 100, "All")),
                  scrollX = TRUE,
                  dom = "Bfrtip",
                  buttons = list(
                    list(extend = "copy", exportOptions = list(modifier = list(page = "all"))),
                    list(extend = "csv", filename = "bulk_results", exportOptions = list(modifier = list(page = "all"))),
                    list(extend = "excel", filename = "bulk_results", exportOptions = list(modifier = list(page = "all")))
                  )
                ),
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
            # #region agent log
            tryCatch(
              {
                log_entry <- jsonlite::toJSON(list(
                  id = paste0("log_", as.integer(Sys.time()), "_", sample(1000:9999, 1)),
                  timestamp = as.integer(Sys.time() * 1000),
                  location = "R/orion_sma.R:1535",
                  message = "bulk_csv_processing DT table render ERROR",
                  data = list(error = e$message),
                  sessionId = "debug-session",
                  runId = "run1",
                  hypothesisId = "C"
                ), auto_unbox = TRUE)
                cat(log_entry, "\n", file = log_file, append = TRUE)
              },
              error = function(e2) {}
            )
            # #endregion
            shiny::showNotification(paste("Error rendering results table:", e$message), type = "error")
            NULL
          }
        )

        # #region agent log
        tryCatch(
          {
            log_entry <- jsonlite::toJSON(list(
              id = paste0("log_", as.integer(Sys.time()), "_", sample(1000:9999, 1)),
              timestamp = as.integer(Sys.time() * 1000),
              location = "R/orion_sma.R:1545",
              message = "bulk_csv_processing complete",
              data = list(
                total_rows = total_rows,
                success_count = success_count,
                error_count = error_count
              ),
              sessionId = "debug-session",
              runId = "run1",
              hypothesisId = "E"
            ), auto_unbox = TRUE)
            cat(log_entry, "\n", file = log_file, append = TRUE)
          },
          error = function(e) {}
        )
        # #endregion

        shiny::showNotification(
          paste("Bulk processing complete! Processed", total_rows, "rows."),
          type = "message",
          duration = 5
        )
      })

      # Handle SMA Settings Checker
      shiny::observeEvent(input$run_checker, {
        # Validate token
        token <- get_token(input$token)
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

        # Parse account IDs (support both comma-separated and newline-separated)
        account_ids_raw <- strsplit(account_ids_text, "[,\n]")[[1]]
        account_ids_raw <- trimws(account_ids_raw)
        account_ids_raw <- account_ids_raw[account_ids_raw != ""]

        # Validate account IDs
        account_ids <- c()
        for (id in account_ids_raw) {
          id_clean <- trimws(id)
          if (id_clean != "" && grepl("^[0-9]+$", id_clean)) {
            account_ids <- c(account_ids, id_clean)
          }
        }

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

            output$checker_progress <- shiny::renderUI(shiny::HTML(
              paste0(
                "<div style='margin-top: 10px; padding: 10px; background-color: #e7f3ff; border: 1px solid #b3d9ff; border-radius: 4px;'>",
                "<strong>Processing:</strong> ", progress_msg, "</div>"
              )
            ))

            # Check this account
            result <- check_account_sma(account_id, token)
            results_list[[i]] <- result

            # Small delay to avoid overwhelming the API
            Sys.sleep(0.1)
          }
        })

        # Convert results to data frame
        results_df <- tryCatch(
          {
            do.call(rbind, lapply(results_list, function(r) {
              # Mask account number
              masked_account_number <- if (!is.null(r$account_number) && r$account_number != "N/A" && nchar(r$account_number) > 4) {
                paste0("****", substr(r$account_number, nchar(r$account_number) - 3, nchar(r$account_number)))
              } else {
                ifelse(is.null(r$account_number), "N/A", r$account_number)
              }

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

          # Create results table
          output$checker_results_table <- DT::renderDT({
            DT::datatable(
              results_df,
              options = list(
                pageLength = 25,
                lengthMenu = list(c(10, 25, 50, 100, -1), c(10, 25, 50, 100, "All")),
                scrollX = TRUE,
                dom = "Bfrtip",
                buttons = list(
                  list(extend = "copy", exportOptions = list(modifier = list(page = "all"))),
                  list(extend = "csv", filename = "sma_checker_results", exportOptions = list(modifier = list(page = "all"))),
                  list(extend = "excel", filename = "sma_checker_results", exportOptions = list(modifier = list(page = "all")))
                )
              ),
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
