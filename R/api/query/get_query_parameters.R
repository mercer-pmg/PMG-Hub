# Get Query Parameters Function
# Source the main API utilities
source("R/api/orion_api.R")

#' Get query parameters for a custom reporting query
#'
#' Retrieves the query definition and parameters for a specified query ID.
#'
#' @param query_id The query ID
#' @param token API token (optional, will use get_token() if not provided)
#'
#' @return A standardized result list with success status, error message (if any), and query data
get_query_parameters <- function(query_id, token = NULL) {
    if (is.null(token)) {
        token <- get_token()
        if (is.null(token)) {
            return(standardize_result(FALSE, "API token not found", NULL))
        }
    }

    result <- orion_request_json(
        method = "GET",
        endpoint = paste0("/api/v1/Reporting/Custom/", query_id),
        token = token,
        error_context = paste("Fetching parameters for Query", query_id),
        simplify_vector = TRUE
    )

    return(result)
}

