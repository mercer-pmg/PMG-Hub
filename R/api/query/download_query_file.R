# Download Query File Function
# Source the main API utilities
source("R/api/orion_api.R")

#' Download and read query result file from location URL
#'
#' Downloads a file from the provided location URL and reads it into a tibble.
#' Used for CSV format queries that return a 201 status with location header.
#'
#' @param location_url The URL from the location header
#' @param format File format (default: "csv")
#' @param token API token
#'
#' @return A tibble containing the query results
download_query_file <- function(location_url, format = "csv", token) {
    # Build request with longer timeout for file downloads
    req <- httr2::request(location_url) |>
        httr2::req_headers(
            "Authorization" = paste("Bearer", token),
            "Accept" = "*/*"
        ) |>
        httr2::req_progress() |>
        httr2::req_timeout(QUERY_GENERATION_TIMEOUT)

    resp <- httr2::req_perform(req)
    status_code <- httr2::resp_status(resp)

    if (status_code < 200 || status_code >= 300) {
        stop(sprintf("Failed to download query file: HTTP %d", status_code))
    }

    # Save to temporary file
    temp_file <- tempfile(fileext = paste0(".", format))
    writeBin(httr2::resp_body_raw(resp), temp_file)

    # Read file based on format
    if (format == "csv") {
        df <- read.csv(temp_file, stringsAsFactors = FALSE)
    } else {
        stop(sprintf("Unsupported file format: %s", format))
    }

    # Clean up temp file
    unlink(temp_file)

    return(tibble::as_tibble(df))
}

