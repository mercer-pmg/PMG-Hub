# Find Asset ID By Product Function

# Source dependencies
source("R/orion_api/endpoints/get_account_assets.R")

find_asset_id_by_product <- function(account_id, product_id, token) {
    assets_result <- get_account_assets(account_id, token)

    if (!assets_result$success) {
        return(list(found = FALSE, asset_id = NULL, error = assets_result$error))
    }

    assets <- assets_result$data

    for (asset in assets) {
        if (is.list(asset)) {
            asset_product_id <- asset$productId
            asset_id <- asset$id

            if (!is.null(asset_product_id) && asset_product_id == product_id && !is.null(asset_id)) {
                return(list(found = TRUE, asset_id = asset_id, error = NULL))
            }
        }
    }

    return(list(found = FALSE, asset_id = NULL, error = NULL))
}
