# Orion Advisor API Client Utilities
# This module provides functions for interacting with the Orion Advisor API,
# including account management, asset operations, and SMA settings.

# API Configuration
BASE_URL <- "https://api.orionadvisor.com"
VERIFY_SSL <- FALSE
TOKEN_MASK_LENGTH <- 100L

# Token Management ----

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

# Validate token helper
validate_token <- function(token_input) {
    token <- get_token(token_input)
    if (is.null(token) || token == "") {
        return(list(valid = FALSE, error = "API token not found"))
    }
    list(valid = TRUE, token = token)
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

# API Request Wrapper ----

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

# Payload Builders ----

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

# Response Parsing Helpers ----

# Parse assets response from API
parse_assets_response <- function(assets_data) {
    if (is.data.frame(assets_data)) {
        lapply(1:nrow(assets_data), function(i) as.list(assets_data[i, ]))
    } else if (is.list(assets_data)) {
        assets_data
    } else {
        list()
    }
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

# API Endpoint Functions ----

# Get account information (verbose)
get_account_verbose <- function(account_id, token, expand = NULL) {
    params <- NULL
    if (!is.null(expand)) {
        params <- list(expand = expand)
    }

    result <- api_request(
        method = "GET",
        endpoint = paste0("/api/v1/Portfolio/Accounts/Verbose/", account_id),
        params = params,
        token = token,
        error_context = paste("Account ID", account_id)
    )

    if (!result$success) {
        return(list(success = FALSE, error = result$error, data = NULL))
    }

    tryCatch(
        {
            account_data <- jsonlite::fromJSON(
                httr::content(result$response, as = "text", encoding = "UTF-8")
            )
            return(list(success = TRUE, error = NULL, data = account_data))
        },
        error = function(e) {
            return(list(success = FALSE, error = paste("Error parsing account data:", e$message), data = NULL))
        }
    )
}

# Get account assets
get_account_assets <- function(account_id, token) {
    result <- api_request(
        method = "GET",
        endpoint = paste0("/api/v1/Portfolio/Accounts/", account_id, "/Assets"),
        token = token,
        error_context = paste("Loading assets for Account ID", account_id)
    )

    if (!result$success) {
        return(list(success = FALSE, error = result$error, data = list()))
    }

    tryCatch(
        {
            assets_data <- jsonlite::fromJSON(
                httr::content(result$response, as = "text", encoding = "UTF-8"),
                simplifyDataFrame = FALSE
            )
            assets <- parse_assets_response(assets_data)
            return(list(success = TRUE, error = NULL, data = assets))
        },
        error = function(e) {
            return(list(success = FALSE, error = paste("Error parsing assets:", e$message), data = list()))
        }
    )
}

# Search accounts by number
search_accounts_by_number <- function(account_number, token, exact_match = FALSE) {
    result <- api_request(
        method = "GET",
        endpoint = paste0("/api/v1/Portfolio/Accounts/Simple/Search/Number/", account_number),
        params = list(exactMatch = exact_match),
        token = token,
        error_context = paste("Searching for account number", account_number)
    )

    if (!result$success) {
        return(list(success = FALSE, error = result$error, data = list()))
    }

    tryCatch(
        {
            accounts_data <- jsonlite::fromJSON(
                httr::content(result$response, as = "text", encoding = "UTF-8")
            )
            # Handle both list and single dict responses
            if (is.data.frame(accounts_data)) {
                accounts_list <- lapply(1:nrow(accounts_data), function(i) as.list(accounts_data[i, ]))
            } else if (is.list(accounts_data) && !is.null(accounts_data$id)) {
                accounts_list <- list(accounts_data)
            } else {
                accounts_list <- accounts_data
            }
            return(list(success = TRUE, error = NULL, data = accounts_list))
        },
        error = function(e) {
            return(list(success = FALSE, error = paste("Error parsing accounts:", e$message), data = list()))
        }
    )
}

# Inject/create asset
inject_asset <- function(account_id, account_number, product_id, token) {
    payload <- build_asset_payload(as.integer(account_id), account_number, product_id)

    result <- api_request(
        method = "POST",
        endpoint = "/api/v1/Portfolio/Assets/Verbose",
        json_data = payload,
        token = token,
        error_context = paste("Injecting product", product_id, "into account", account_id)
    )

    if (!result$success) {
        return(list(success = FALSE, error = result$error, data = NULL))
    }

    tryCatch(
        {
            response_data <- jsonlite::fromJSON(
                httr::content(result$response, as = "text", encoding = "UTF-8")
            )
            asset_id <- extract_asset_id(response_data)
            return(list(success = TRUE, error = NULL, data = response_data, asset_id = asset_id))
        },
        error = function(e) {
            return(list(success = FALSE, error = paste("Error parsing response:", e$message), data = NULL, asset_id = NULL))
        }
    )
}

# Update SMA settings
update_sma_settings <- function(account_id, is_sma, sma_asset_id, token, eclipse_sma = "False") {
    sma_object <- list(
        isSma = is_sma,
        eclipseSMA = eclipse_sma,
        smaAssetId = sma_asset_id
    )

    sma_payload <- list(
        id = NULL,
        modelingInfo = NULL,
        sma = sma_object
    )

    result <- api_request(
        method = "PUT",
        endpoint = paste0("/api/v1/Portfolio/Accounts/UpdateDoNotTradeAndSma/", account_id),
        json_data = sma_payload,
        token = token,
        error_context = paste("Updating SMA settings for Account ID", account_id)
    )

    return(list(success = result$success, error = if (!result$success) result$error else NULL))
}

# High-Level API Functions ----

# Check if product exists in account assets
check_product_in_account <- function(account_id, product_id, token) {
    assets_result <- get_account_assets(account_id, token)

    if (!assets_result$success) {
        return(list(exists = FALSE, asset_id = NULL, error = assets_result$error))
    }

    assets <- assets_result$data

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

    account_result <- get_account_verbose(account_id, token, expand = "Sma")

    if (!account_result$success) {
        result$status <- "Error"
        result$message <- account_result$error
        return(result)
    }

    account_data <- account_result$data

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

    return(result)
}

# Process a single account-product pair (inject asset and update SMA)
process_single_account_product <- function(account_id, product_id, token) {
    result <- list(
        account_id = account_id,
        product_id = product_id,
        status = "Unknown",
        message = "",
        asset_id = NULL
    )

    # Step 1: Fetch account data to get account number
    account_result <- get_account_verbose(account_id, token, expand = "Sma")

    if (!account_result$success) {
        result$status <- "Error"
        result$message <- paste("Failed to fetch account:", account_result$error)
        return(result)
    }

    account_data <- account_result$data

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
        inject_result <- inject_asset(account_id, account_number, product_id, token)

        if (!inject_result$success) {
            result$status <- "Error"
            result$message <- paste("Failed to inject asset:", inject_result$error)
            return(result)
        }

        asset_id <- inject_result$asset_id

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
    sma_result <- update_sma_settings(account_id, TRUE, asset_id, token)

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

    return(result)
}
