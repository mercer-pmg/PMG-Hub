# Check Account SMA Function

# Source dependencies
source("R/orion_api/endpoints/get_account.R")
source("R/orion_api/utils/extract_sma_values.R")

check_account_sma <- function(account_id, token) {
    result <- list(
        account_id = account_id,
        account_name = NULL,
        account_number = NULL,
        isSMA = FALSE,
        eclipseSMA = "None",
        smaAssetID = NULL,
        status = "Unknown",
        message = ""
    )

    account_result <- get_account(account_id, token, expand = "Sma")

    if (!account_result$success) {
        result$status <- "Error"
        result$message <- account_result$error
        return(result)
    }

    account_data <- account_result$data

    result$account_name <- ifelse(is.null(account_data$name), "N/A", account_data$name)
    result$account_number <- account_data$number

    sma_values <- extract_sma_values(account_data)
    result$isSMA <- sma_values$isSMA
    result$eclipseSMA <- sma_values$eclipseSMA
    result$smaAssetID <- sma_values$smaAssetID

    result$status <- "Success"
    result$message <- "SMA settings retrieved successfully"

    return(result)
}
