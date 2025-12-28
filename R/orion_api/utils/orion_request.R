# Orion Request Function

# Source config for constants
source("R/orion_api/utils/config.R")

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

orion_request <- function(method, endpoint, json_data = NULL, params = NULL, token, error_context = "", timeout = NULL) {
    # Validate method
    if (!method %in% c("GET", "POST", "PUT", "PATCH", "DELETE")) {
        return(list(success = FALSE, error = paste("Unsupported HTTP method:", method), status_code = NULL))
    }

    # Validate token
    if (is.null(token) || trimws(token) == "") {
        return(list(success = FALSE, error = "API token is required", status_code = NULL))
    }

    # Use provided timeout or default
    request_timeout <- if (!is.null(timeout)) timeout else TIMEOUT

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
                    httr2::req_timeout(request_timeout)

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
