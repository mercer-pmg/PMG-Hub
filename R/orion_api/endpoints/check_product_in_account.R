# Check Product In Account Function

# Source dependencies
source("R/orion_api/endpoints/find_asset_id_by_product.R")

# Backward compatible wrapper (older callers)
check_product_in_account <- function(account_id, product_id, token) {
    res <- find_asset_id_by_product(account_id, product_id, token)
    list(exists = res$found, asset_id = res$asset_id, error = res$error)
}
