# Validate Inputs Function

validate_inputs <- function(account_id, token, account_id_label = "Account ID") {
    if (is.null(account_id) || length(account_id) == 0) {
        return(list(valid = FALSE, error = paste(account_id_label, "cannot be NULL or empty")))
    }

    account_id <- trimws(as.character(account_id))

    if (account_id == "" || !grepl("^[0-9]+$", account_id)) {
        return(list(valid = FALSE, error = paste("Please enter a valid", account_id_label)))
    }

    if (is.null(token) || length(token) == 0 || trimws(as.character(token)) == "") {
        return(list(valid = FALSE, error = "API token not found"))
    }

    return(list(valid = TRUE, account_id = account_id, token = trimws(as.character(token))))
}
