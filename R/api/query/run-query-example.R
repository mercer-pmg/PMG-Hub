# Example: Running Orion Query 40790 (Sleeve Strategy Edit History)
# This demonstrates how to use the generic query functions to run a specific query

# Source the generic Orion API functions and query utilities
source("../orion_api.R")
source("run_orion_query.R")

# Query ID
query_id <- 40790

# Date parameters
start_date <- "1/1/2015"
end_date <- format(Sys.Date(), "%m/%d/%Y")

# Optional: Get query parameters to display query title
message(sprintf("Fetching parameters for Query %d...", query_id))
query_result <- get_query_parameters(query_id)

if (!query_result$success) {
    stop(query_result$error)
}

message(sprintf("Query: %s", query_result$data$title))
message(sprintf("Setting date range: %s to %s", start_date, end_date))

# Prepare prompt updates as a named list
# Names are prompt text patterns (case-insensitive), values are the new values
prompt_updates <- list(
    "Enter Edited Start Date" = start_date,
    "Enter Edited End Date" = end_date
)

# Run the query using the generic function
message(sprintf("Generating query results in csv format..."))
results_tbl <- run_orion_query(
    query_id = query_id,
    prompt_updates = prompt_updates,
    format = "csv"
)

message(sprintf("Query completed. Retrieved %d rows and %d columns.", nrow(results_tbl), ncol(results_tbl)))

# Optional: Save results to CSV file
output_file <- "sleeve_history.csv"
if (!is.null(output_file)) {
    write.csv(results_tbl, file = output_file, row.names = FALSE)
    message(sprintf("Results saved to: %s", output_file))
}

# Results are now available in results_tbl
# View the results
print(results_tbl)
