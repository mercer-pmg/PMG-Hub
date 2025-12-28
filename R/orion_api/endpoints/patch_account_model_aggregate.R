# Patch Account Model Aggregate Function

# Source dependencies
source("R/orion_api/utils/orion_request.R")
source("R/orion_api/utils/standardize_result.R")

patch_account_model_aggregate <- function(account_id, model_agg_id, token) {
    result <- orion_request(
        method = "PATCH",
        endpoint = paste0("/api/v1/Portfolio/Accounts/", account_id, "/ModelAgg/", model_agg_id),
        token = token,
        error_context = paste("Updating model aggregate", model_agg_id, "for Account ID", account_id)
    )

    standardize_result(result$success, if (!result$success) result$error else NULL, NULL)
}
