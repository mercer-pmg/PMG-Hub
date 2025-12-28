# Post Asset Function

# Source dependencies
source("R/orion_api/utils/orion_request_json.R")

post_asset <- function(account_id, account_number, product_id, token) {
    payload <- list(
        portfolio = list(
            accountNumber = account_number,
            productId = product_id,
            accountId = as.integer(account_id),
            isManaged = TRUE,
            isActive = TRUE,
            isAdvisorOnly = TRUE,
            status = "Manually Managed",
            currentShares = 0,
            currentValue = 0
        )
    )

    res <- orion_request_json(
        method = "POST",
        endpoint = "/api/v1/Portfolio/Assets/Verbose",
        json_data = payload,
        token = token,
        error_context = paste("Injecting product", product_id, "into account", account_id),
        simplify_vector = TRUE
    )

    if (!res$success) {
        return(list(success = FALSE, error = res$error, data = NULL, asset_id = NULL))
    }

    response_data <- res$data
    asset_id <- as.integer(response_data$id)
    return(list(success = TRUE, error = NULL, data = response_data, asset_id = asset_id))
}
