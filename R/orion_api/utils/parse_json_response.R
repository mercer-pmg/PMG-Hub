# Parse JSON Response Function

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
