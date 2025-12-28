# Update Query Prompts Function

#' Update query prompts by matching prompt text pattern
#'
#' Updates prompt default values by matching against prompt text using pattern matching.
#'
#' @param prompts The prompts object (can be data.frame or list)
#' @param prompt_text_pattern Pattern to match against prompt text (case-insensitive)
#' @param new_value New value to set for matching prompts
#'
#' @return Updated prompts object
update_query_prompts_by_text <- function(prompts, prompt_text_pattern, new_value) {
    if (is.data.frame(prompts)) {
        # Handle prompts as data frame
        prompt_idx <- which(grepl(prompt_text_pattern, prompts$prompt, ignore.case = TRUE))
        if (length(prompt_idx) > 0) {
            prompts$defaultValue[prompt_idx] <- new_value
        }
    } else if (is.list(prompts)) {
        # Handle prompts as list
        prompt_idx <- which(sapply(prompts, function(p) {
            if (is.list(p) && !is.null(p$prompt)) {
                grepl(prompt_text_pattern, p$prompt, ignore.case = TRUE)
            } else {
                FALSE
            }
        }))
        if (length(prompt_idx) > 0) {
            for (idx in prompt_idx) {
                prompts[[idx]]$defaultValue <- new_value
            }
        }
    }
    return(prompts)
}

