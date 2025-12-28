# Orion Advisor API Client
# Main entry point that sources all utilities, endpoints, and query functions

# Source all utility functions
source("R/orion_api/utils/utils.R")

# Source endpoint functions (in dependency order)
source("R/orion_api/endpoints/get_account.R")
source("R/orion_api/endpoints/get_account_assets.R")
source("R/orion_api/endpoints/get_accounts_by_number.R")
source("R/orion_api/endpoints/post_asset.R")
source("R/orion_api/endpoints/get_assets_by_product_id.R")
source("R/orion_api/endpoints/find_asset_id_by_product.R")
source("R/orion_api/endpoints/check_product_in_account.R")
source("R/orion_api/endpoints/put_account_sma.R")
source("R/orion_api/endpoints/check_account_sma.R")
source("R/orion_api/endpoints/patch_account_model_aggregate.R")
source("R/orion_api/endpoints/process_single_account_product.R")

# Source query functions (in dependency order)
source("R/orion_api/query/get_query_parameters.R")
source("R/orion_api/query/update_query_prompts_by_text.R")
source("R/orion_api/query/download_query_file.R")
source("R/orion_api/query/generate_query_results.R")
source("R/orion_api/query/run_orion_query.R")
