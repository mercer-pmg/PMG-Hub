# Put Account SMA Function

# Source dependencies
source("R/orion_api/utils/orion_request.R")
source("R/orion_api/utils/standardize_result.R")

put_account_sma <- function(account_id, is_sma, sma_asset_id, token, eclipse_sma = "False") {
    sma_payload <- list(
        id = NULL,
        modelingInfo = NULL,
        sma = list(
            isSma = is_sma,
            eclipseSMA = eclipse_sma,
            smaAssetId = sma_asset_id
        )
    )

    result <- orion_request(
        method = "PUT",
        endpoint = paste0("/api/v1/Portfolio/Accounts/UpdateDoNotTradeAndSma/", account_id),
        json_data = sma_payload,
        token = token,
        error_context = paste("Updating SMA settings for Account ID", account_id)
    )

    standardize_result(result$success, if (!result$success) result$error else NULL, NULL)
}
