# Run Orion Query Function

#' Run an Orion custom reporting query
#'
#' High-level function to execute a custom reporting query with prompt updates.
#'
#' @param query_id The query ID
#' @param prompt_updates Named list where names are prompt text patterns (case-insensitive) 
#'   and values are the new values to set
#' @param format Output format: "csv", "Table", or "SlickGrid" (default: "csv")
#' @param token API token (optional, will use get_token() if not provided)
#'
#' @return A tibble containing the query results
#'
#' @examples
#' # Update prompts using named list
#' results <- run_orion_query(
#'     query_id = 40790,
#'     prompt_updates = list(
#'         "Enter Edited Start Date" = "1/1/2020",
#'         "Enter Edited End Date" = "12/31/2023"
#'     )
#' )
run_orion_query <- function(query_id, prompt_updates = NULL, format = "csv", token = NULL) {
    if (is.null(token)) {
        token <- get_token()
        if (is.null(token)) {
            stop("API token not found")
        }
    }

    # Get query parameters
    query_result <- get_query_parameters(query_id, token)
    if (!query_result$success) {
        stop(query_result$error)
    }

    query_payload <- query_result$data

    # Update prompts if provided
    if (!is.null(prompt_updates) && length(prompt_updates) > 0) {
        prompts <- query_payload$prompts

        # Named list where names are patterns
        for (pattern in names(prompt_updates)) {
            prompts <- update_query_prompts_by_text(prompts, pattern, prompt_updates[[pattern]])
        }

        query_payload$prompts <- prompts
    }

    # Generate query results
    return(generate_query_results(query_payload, format, token))
}

