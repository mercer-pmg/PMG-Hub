# Orion Request JSON Function

# Source dependencies
source("R/orion_api/utils/orion_request.R")
source("R/orion_api/utils/parse_json_response.R")
source("R/orion_api/utils/standardize_result.R")

orion_request_json <- function(method,
                               endpoint,
                               token,
                               error_context = "",
                               params = NULL,
                               json_data = NULL,
                               simplify_vector = TRUE,
                               timeout = NULL) {
    result <- orion_request(
        method = method,
        endpoint = endpoint,
        params = params,
        json_data = json_data,
        token = token,
        error_context = error_context,
        timeout = timeout
    )

    parse_result <- parse_json_response(result, error_context = error_context, simplify_vector = simplify_vector)
    if (!parse_result$success) {
        return(standardize_result(FALSE, parse_result$error, NULL))
    }

    standardize_result(TRUE, NULL, parse_result$data)
}
