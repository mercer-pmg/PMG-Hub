# Get Assets By Product ID Function

# Source all utils (loads all utility functions)
source("R/orion_api/utils/utils.R")

# Helper function to get assets for a single product ID
.get_assets_by_product_id_single <- function(product_id, token) {
    res <- orion_request_json(
        method = "GET",
        endpoint = "/api/v1/Portfolio/Assets",
        params = list(productId = product_id),
        token = token,
        error_context = paste("Loading assets for Product ID", product_id),
        simplify_vector = FALSE
    )

    if (!res$success) {
        return(list(success = FALSE, error = res$error, data = list()))
    }

    list(success = TRUE, error = NULL, data = parse_assets_response(res$data))
}

# Main function that accepts single product ID or vector/list of product IDs
get_assets_by_product_id <- function(product_id, token, return_tibble = TRUE) {
    # Handle single product ID or vector/list
    if (length(product_id) == 1) {
        result <- .get_assets_by_product_id_single(product_id, token)
        if (!result$success) {
            return(standardize_result(FALSE, result$error, NULL))
        }
        
        if (return_tibble && length(result$data) > 0) {
            assets_tibble <- dplyr::bind_rows(result$data)
            return(standardize_result(TRUE, NULL, assets_tibble))
        }
        return(standardize_result(TRUE, NULL, result$data))
    }
    
    # Handle multiple product IDs
    all_assets <- list()
    errors <- list()
    
    for (pid in product_id) {
        result <- .get_assets_by_product_id_single(pid, token)
        if (result$success) {
            all_assets <- c(all_assets, result$data)
        } else {
            errors[[as.character(pid)]] <- result$error
        }
    }
    
    if (length(all_assets) == 0) {
        error_msg <- if (length(errors) > 0) {
            paste("Failed to retrieve assets for all product IDs:", paste(names(errors), collapse = ", "))
        } else {
            "No assets found for any product ID"
        }
        return(standardize_result(FALSE, error_msg, NULL))
    }
    
    if (return_tibble) {
        assets_tibble <- dplyr::bind_rows(all_assets)
        return(standardize_result(TRUE, NULL, assets_tibble))
    }
    
    standardize_result(TRUE, NULL, all_assets)
}
