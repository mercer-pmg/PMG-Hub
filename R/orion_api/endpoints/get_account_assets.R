# Get Account Assets Function

# Source dependencies
source("R/orion_api/utils/orion_request_json.R")
source("R/orion_api/utils/parse_assets_response.R")
source("R/orion_api/utils/standardize_result.R")

get_account_assets <- function(account_id, token) {
    res <- orion_request_json(
        method = "GET",
        endpoint = paste0("/api/v1/Portfolio/Accounts/", account_id, "/Assets"),
        token = token,
        error_context = paste("Loading assets for Account ID", account_id),
        # assets can be large/nested; keep it as list to avoid unwanted simplification
        simplify_vector = FALSE
    )

    if (!res$success) {
        return(standardize_result(FALSE, res$error, list()))
    }

    standardize_result(TRUE, NULL, parse_assets_response(res$data))
}
