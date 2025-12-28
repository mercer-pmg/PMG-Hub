# Get Account Function

# Source dependencies
source("R/orion_api/utils/orion_request_json.R")

get_account <- function(account_id, token, expand = NULL) {
    params <- if (!is.null(expand)) list(expand = expand) else NULL

    orion_request_json(
        method = "GET",
        endpoint = paste0("/api/v1/Portfolio/Accounts/Verbose/", account_id),
        params = params,
        token = token,
        error_context = paste("Account ID", account_id),
        simplify_vector = TRUE
    )
}
