# Get Accounts By Number Function

# Source dependencies
source("R/orion_api/utils/orion_request_json.R")
source("R/orion_api/utils/standardize_result.R")

get_accounts_by_number <- function(account_number, token, exact_match = FALSE) {
    res <- orion_request_json(
        method = "GET",
        endpoint = paste0("/api/v1/Portfolio/Accounts/Simple/Search/Number/", account_number),
        params = list(exactMatch = exact_match),
        token = token,
        error_context = paste("Searching for account number", account_number),
        simplify_vector = TRUE
    )

    if (!res$success) {
        return(standardize_result(FALSE, res$error, list()))
    }

    accounts_data <- res$data
    # Handle both list and single dict responses
    if (is.data.frame(accounts_data)) {
        accounts_list <- lapply(1:nrow(accounts_data), function(i) as.list(accounts_data[i, ]))
    } else if (is.list(accounts_data) && !is.null(accounts_data$id)) {
        accounts_list <- list(accounts_data)
    } else {
        accounts_list <- accounts_data
    }
    standardize_result(TRUE, NULL, accounts_list)
}
