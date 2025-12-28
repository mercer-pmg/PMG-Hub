# Parse Assets Response Function

parse_assets_response <- function(assets_data) {
    if (is.data.frame(assets_data)) {
        lapply(1:nrow(assets_data), function(i) as.list(assets_data[i, ]))
    } else if (is.list(assets_data)) {
        assets_data
    } else {
        list()
    }
}
