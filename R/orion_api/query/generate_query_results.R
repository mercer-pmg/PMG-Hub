# Generate Query Results Function

#' Generate query results
#'
#' Generates results for a custom reporting query. Handles CSV format with file downloads
#' and JSON formats (Table, SlickGrid) with direct response parsing.
#'
#' @param query_payload The query payload (from get_query_parameters)
#' @param format Output format: "csv", "Table", or "SlickGrid" (default: "csv")
#' @param token API token (optional, will use get_token() if not provided)
#'
#' @return A tibble containing the query results
generate_query_results <- function(query_payload, format = "csv", token = NULL) {
    if (is.null(token)) {
        token <- get_token()
        if (is.null(token)) {
            stop("API token not found")
        }
    }

    if (is.null(query_payload$id)) {
        stop("Query payload must contain an 'id' field")
    }

    # Use longer timeout for query generation
    result <- orion_request(
        method = "POST",
        endpoint = paste0("/api/v1/Reporting/Custom/", query_payload$id, "/Generate/", format),
        json_data = query_payload,
        token = token,
        error_context = paste("Generating query results for Query", query_payload$id, "in", format, "format"),
        timeout = QUERY_GENERATION_TIMEOUT
    )

    if (!result$success) {
        stop(result$error)
    }

    status_code <- result$status_code

    # Handle CSV format - expects 201 with location header
    if (format == "csv") {
        if (status_code == 201) {
            location <- httr2::resp_header(result$response, "location")
            if (is.null(location)) {
                stop("CSV format returned 201 but no location header found")
            }
            return(download_query_file(location, format, token))
        } else {
            stop(sprintf("Unexpected status code %d for CSV format. Expected 201 with location header.", status_code))
        }
    } else if (format %in% c("Table", "SlickGrid")) {
        # Handle JSON formats
        if (status_code == 200) {
            parse_result <- parse_json_response(result, error_context = "parsing query results", simplify_vector = TRUE)
            if (!parse_result$success) {
                stop(parse_result$error)
            }
            return(tibble::as_tibble(parse_result$data))
        } else {
            stop(sprintf("Unexpected status code %d for %s format. Expected 200.", status_code, format))
        }
    } else {
        stop(sprintf("Unsupported format: %s", format))
    }
}

