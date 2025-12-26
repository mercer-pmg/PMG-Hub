# Orion Advisor API Client Utilities

# API Configuration
BASE_URL <- "https://api.orionadvisor.com"
VERIFY_SSL <- FALSE
TIMEOUT <- 30L
MAX_RETRIES <- 3L
RETRY_DELAY <- 1.0
TOKEN_MASK_LENGTH <- 200L

# Token Management ----

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

# Validate inputs (account ID and token)
validate_inputs <- function(account_id, token, account_id_label = "Account ID") {
    if (is.null(account_id) || length(account_id) == 0) {
        return(list(valid = FALSE, error = paste(account_id_label, "cannot be NULL or empty")))
    }

    account_id <- trimws(as.character(account_id))

    if (account_id == "" || !grepl("^[0-9]+$", account_id)) {
        return(list(valid = FALSE, error = paste("Please enter a valid", account_id_label)))
    }

    if (is.null(token) || length(token) == 0 || trimws(as.character(token)) == "") {
        return(list(valid = FALSE, error = "API token not found"))
    }

    return(list(valid = TRUE, account_id = account_id, token = trimws(as.character(token))))
}

orion_request <- function(method, endpoint, json_data = NULL, params = NULL, token, error_context = "") {
    # Validate method
    if (!method %in% c("GET", "POST", "PUT", "PATCH", "DELETE")) {
        return(list(success = FALSE, error = paste("Unsupported HTTP method:", method), status_code = NULL))
    }

    # Validate token
    if (is.null(token) || trimws(token) == "") {
        return(list(success = FALSE, error = "API token is required", status_code = NULL))
    }

    url <- paste0(BASE_URL, endpoint)
    attempt <- 0
    last_error <- NULL

    while (attempt <= MAX_RETRIES) {
        result <- tryCatch(
            {
                # Build request using httr2
                req <- httr2::request(url) |>
                    httr2::req_method(method) |>
                    httr2::req_headers(
                        "Authorization" = paste("Bearer", token),
                        "Accept" = "application/json",
                        "Content-Type" = "application/json"
                    ) |>
                    httr2::req_options(ssl_verifypeer = VERIFY_SSL) |>
                    httr2::req_timeout(TIMEOUT)

                # Add query parameters for GET requests
                if (!is.null(params) && method == "GET") {
                    req <- do.call(httr2::req_url_query, c(list(req), params))
                }

                # Add JSON body for POST, PUT, PATCH requests
                if (!is.null(json_data) && method %in% c("POST", "PUT", "PATCH")) {
                    req <- req |> httr2::req_body_json(json_data)
                }

                # Perform request
                response <- httr2::req_perform(req)
                status_code <- httr2::resp_status(response)

                # Handle rate limiting (429 Too Many Requests)
                if (status_code == 429 && attempt < MAX_RETRIES) {
                    retry_after <- httr2::resp_header(response, "Retry-After")
                    delay <- if (!is.null(retry_after)) {
                        as.numeric(retry_after)
                    } else {
                        RETRY_DELAY * (2^attempt) # Exponential backoff
                    }
                    Sys.sleep(delay)
                    attempt <<- attempt + 1
                    return(NULL) # Signal to continue loop
                }

                # Handle server errors (5xx) with retry
                if (status_code >= 500 && status_code < 600 && attempt < MAX_RETRIES) {
                    delay <- RETRY_DELAY * (2^attempt) # Exponential backoff
                    Sys.sleep(delay)
                    attempt <<- attempt + 1
                    return(NULL) # Signal to continue loop
                }

                # Success or non-retryable error
                if (status_code %in% c(200, 201, 204)) {
                    return(list(success = TRUE, response = response, status_code = status_code))
                } else {
                    response_text <- tryCatch(
                        httr2::resp_body_string(response),
                        error = function(e) "[Unable to read response body]"
                    )
                    response_preview <- substr(response_text, 1, 200)

                    error_msg <- .format_error_message(status_code, error_context, response_preview)
                    return(list(success = FALSE, error = error_msg, status_code = status_code, response = response))
                }
            },
            error = function(e) {
                last_error <<- e
                # Retry on network errors
                if (attempt < MAX_RETRIES) {
                    delay <- RETRY_DELAY * (2^attempt)
                    Sys.sleep(delay)
                    attempt <<- attempt + 1
                    return(NULL) # Signal to continue loop
                } else {
                    return(list(
                        success = FALSE,
                        error = paste("Network Error: Failed to connect to API:", e$message),
                        status_code = NULL
                    ))
                }
            }
        )

        # If we got a result (not NULL), return it
        if (!is.null(result)) {
            return(result)
        }
    }

    # If we get here, all retries failed
    if (!is.null(last_error)) {
        return(list(
            success = FALSE,
            error = paste("Network Error: Failed after", MAX_RETRIES, "retries:", last_error$message),
            status_code = NULL
        ))
    }

    # Fallback (should never reach here)
    return(list(success = FALSE, error = "Unknown error occurred", status_code = NULL))
}

# Helper function to format error messages
.format_error_message <- function(status_code, error_context, response_preview) {
    if (status_code == 400) {
        return(paste("Validation Error - Bad Request:", error_context, "\n\nResponse:", response_preview))
    } else if (status_code == 401) {
        return("Authentication Error - Invalid API token")
    } else if (status_code == 403) {
        return(paste("Forbidden - Access denied:", error_context))
    } else if (status_code == 404) {
        return(paste("Not Found - Resource not found:", error_context, "\n\nResponse:", response_preview))
    } else if (status_code == 405) {
        return(paste("API Error - Method Not Allowed (405):", error_context, "\n\nResponse:", response_preview))
    } else if (status_code == 429) {
        return(paste("Rate Limit Error - Too many requests:", error_context))
    } else if (status_code >= 500 && status_code < 600) {
        return(paste("Server Error - Internal server error:", error_context, "\n\nResponse:", response_preview))
    } else {
        return(paste("API Error", status_code, ":", response_preview))
    }
}

# Response Parsing Helpers ----

parse_json_response <- function(result, error_context = "parsing response", simplify_vector = TRUE) {
    if (!result$success) {
        return(list(success = FALSE, error = result$error, data = NULL))
    }

    if (is.null(result$response)) {
        return(list(success = FALSE, error = paste("Error", error_context, ": No response object available"), data = NULL))
    }

    tryCatch(
        {
            parsed_data <- httr2::resp_body_json(result$response, simplifyVector = simplify_vector)
            return(list(success = TRUE, error = NULL, data = parsed_data))
        },
        error = function(e) {
            # Try to get raw response for debugging
            raw_response <- tryCatch(
                httr2::resp_body_string(result$response),
                error = function(e2) "[Unable to read response]"
            )
            return(list(
                success = FALSE,
                error = paste("Error", error_context, ":", e$message, "\nRaw response:", substr(raw_response, 1, 200)),
                data = NULL
            ))
        }
    )
}

parse_assets_response <- function(assets_data) {
    if (is.data.frame(assets_data)) {
        lapply(1:nrow(assets_data), function(i) as.list(assets_data[i, ]))
    } else if (is.list(assets_data)) {
        assets_data
    } else {
        list()
    }
}

standardize_api_result <- function(result) {
    list(success = result$success, error = if (!result$success) result$error else NULL)
}

# API Endpoint Functions ----

get_account <- function(account_id, token, expand = NULL) {
    params <- if (!is.null(expand)) list(expand = expand) else NULL

    result <- orion_request(
        method = "GET",
        endpoint = paste0("/api/v1/Portfolio/Accounts/Verbose/", account_id),
        params = params,
        token = token,
        error_context = paste("Account ID", account_id)
    )

    parse_result <- parse_json_response(result, error_context = "parsing account data")
    if (!parse_result$success) {
        return(list(success = FALSE, error = parse_result$error, data = NULL))
    }

    return(list(success = TRUE, error = NULL, data = parse_result$data))
}

get_account_assets <- function(account_id, token) {
    result <- orion_request(
        method = "GET",
        endpoint = paste0("/api/v1/Portfolio/Accounts/", account_id, "/Assets"),
        token = token,
        error_context = paste("Loading assets for Account ID", account_id)
    )

    parse_result <- parse_json_response(result, error_context = "parsing assets")
    if (!parse_result$success) {
        return(list(success = FALSE, error = parse_result$error, data = list()))
    }

    assets <- parse_assets_response(parse_result$data)
    return(list(success = TRUE, error = NULL, data = assets))
}

get_accounts_by_number <- function(account_number, token, exact_match = FALSE) {
    result <- orion_request(
        method = "GET",
        endpoint = paste0("/api/v1/Portfolio/Accounts/Simple/Search/Number/", account_number),
        params = list(exactMatch = exact_match),
        token = token,
        error_context = paste("Searching for account number", account_number)
    )

    parse_result <- parse_json_response(result, error_context = "parsing accounts")
    if (!parse_result$success) {
        return(list(success = FALSE, error = parse_result$error, data = list()))
    }

    accounts_data <- parse_result$data
    # Handle both list and single dict responses
    if (is.data.frame(accounts_data)) {
        accounts_list <- lapply(1:nrow(accounts_data), function(i) as.list(accounts_data[i, ]))
    } else if (is.list(accounts_data) && !is.null(accounts_data$id)) {
        accounts_list <- list(accounts_data)
    } else {
        accounts_list <- accounts_data
    }
    return(list(success = TRUE, error = NULL, data = accounts_list))
}

post_asset <- function(account_id, account_number, product_id, token) {
    payload <- list(
        portfolio = list(
            accountNumber = account_number,
            productId = product_id,
            accountId = as.integer(account_id),
            isManaged = TRUE,
            isActive = TRUE,
            isAdvisorOnly = TRUE,
            status = "Manually Managed",
            currentShares = 0,
            currentValue = 0
        )
    )

    result <- orion_request(
        method = "POST",
        endpoint = "/api/v1/Portfolio/Assets/Verbose",
        json_data = payload,
        token = token,
        error_context = paste("Injecting product", product_id, "into account", account_id)
    )

    parse_result <- parse_json_response(result, error_context = "parsing response")
    if (!parse_result$success) {
        return(list(success = FALSE, error = parse_result$error, data = NULL, asset_id = NULL))
    }

    response_data <- parse_result$data
    asset_id <- as.integer(response_data$id)
    return(list(success = TRUE, error = NULL, data = response_data, asset_id = asset_id))
}

put_account_sma <- function(account_id, is_sma, sma_asset_id, token, eclipse_sma = "False") {
    sma_payload <- list(
        id = NULL,
        modelingInfo = NULL,
        sma = list(
            isSma = is_sma,
            eclipseSMA = eclipse_sma,
            smaAssetId = sma_asset_id
        )
    )

    result <- orion_request(
        method = "PUT",
        endpoint = paste0("/api/v1/Portfolio/Accounts/UpdateDoNotTradeAndSma/", account_id),
        json_data = sma_payload,
        token = token,
        error_context = paste("Updating SMA settings for Account ID", account_id)
    )

    return(standardize_api_result(result))
}

# High-Level API Functions ----

check_product_in_account <- function(account_id, product_id, token) {
    assets_result <- get_account_assets(account_id, token)

    if (!assets_result$success) {
        return(list(exists = FALSE, asset_id = NULL, error = assets_result$error))
    }

    assets <- assets_result$data

    for (asset in assets) {
        if (is.list(asset)) {
            asset_product_id <- asset$productId
            asset_id <- asset$id

            if (!is.null(asset_product_id) && asset_product_id == product_id && !is.null(asset_id)) {
                return(list(exists = TRUE, asset_id = asset_id, error = NULL))
            }
        }
    }

    return(list(exists = FALSE, asset_id = NULL, error = NULL))
}

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

    account_result <- get_account(account_id, token, expand = "Sma")

    if (!account_result$success) {
        result$status <- "Error"
        result$message <- account_result$error
        return(result)
    }

    account_data <- account_result$data

    result$account_name <- ifelse(is.null(account_data$name), "N/A", account_data$name)
    result$account_number <- account_data$number

    sma <- account_data$sma
    result$isSMA <- as.logical(sma$isSma)
    result$eclipseSMA <- as.character(sma$eclipseSMA)
    result$smaAssetID <- as.integer(sma$smaAssetId)

    result$status <- "Success"
    result$message <- "SMA settings retrieved successfully"

    return(result)
}

patch_account_model_aggregate <- function(account_id, model_agg_id, token) {
    result <- orion_request(
        method = "PATCH",
        endpoint = paste0("/api/v1/Portfolio/Accounts/", account_id, "/ModelAgg/", model_agg_id),
        token = token,
        error_context = paste("Updating model aggregate", model_agg_id, "for Account ID", account_id)
    )

    return(standardize_api_result(result))
}

process_single_account_product <- function(account_id, product_id, token) {
    result <- list(
        account_id = account_id,
        product_id = product_id,
        status = "Unknown",
        message = "",
        asset_id = NULL
    )

    # Step 1: Fetch account data to get account number
    account_result <- get_account(account_id, token, expand = "Sma")

    if (!account_result$success) {
        result$status <- "Error"
        result$message <- paste("Failed to fetch account:", account_result$error)
        return(result)
    }

    account_data <- account_result$data

    # Extract account number
    account_number <- account_data$number

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
        inject_result <- post_asset(account_id, account_number, product_id, token)

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
    sma_result <- put_account_sma(account_id, TRUE, asset_id, token)

    if (!sma_result$success) {
        result$status <- "Partial Success"
        result$message <- paste0(result$message, " | SMA update failed: ", sma_result$error)
        return(result)
    }

    if (result$status == "Skipped (already exists)") {
        result$status <- "Success (already existed)"
    } else {
        result$status <- "Success"
    }
    result$message <- paste0(result$message, " | SMA settings updated successfully")

    return(result)
}
