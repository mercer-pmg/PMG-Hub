# Get Token Function

# Source config for TOKEN_MASK_LENGTH
source("R/orion_api/utils/config.R")

get_token <- function(token_input = NULL) {
    # Check input token first
    if (!is.null(token_input)) {
        token_trimmed <- trimws(token_input)
        masked_value <- paste(rep("*", TOKEN_MASK_LENGTH), collapse = "")
        if (nzchar(token_trimmed) && token_trimmed != masked_value) {
            return(token_trimmed)
        }
    }

    # Fall back to environment variable
    env_token <- Sys.getenv("MA_ORION_API_TOKEN")
    if (nzchar(env_token)) env_token else NULL
}
