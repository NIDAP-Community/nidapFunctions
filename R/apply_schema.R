#####################
# Note that this allows you to perfrom the same operation as if cliking the apply schema for csv input
# But please note that if the column names or values does not meet the foundry dataset schema requirement
# This method may fail.

library(httr)
library(jsonlite)

apply_schema <- function(rid, branch_name, token){
  schema_create_url <- sprintf("https://nidap.nih.gov/foundry-schema-inference/api/datasets/%s/branches/%s/schema", rid, branch_name)
  
  # First POST request
  response <- POST(schema_create_url,
                   add_headers(Authorization = paste0("Bearer ", key),
                               "Content-Type" = "application/json"),
                   body = '{}',
                   encode = "json")
  
  if (status_code(response) == 200) {
    print("Schema Acquisition Success")
  } else {
    error_message <- content(response, "text")
    print(paste("Schema Acquisition Error:", error_message))
  }
  
  
  # Extracting "foundrySchema"
  response_content <- content(response, "text")
  foundrySchema <- fromJSON(response_content)$data$foundrySchema
  
  # Cleaning up the foundrySchema
  # foundrySchema <- sub('^"(.*)"$', '\\1', foundrySchema)
  
  schema_set_url <- sprintf("https://nidap.nih.gov/foundry-metadata/api/schemas/datasets/%s/branches/%s", rid, branch_name)
  
  # Second POST request
  update_response <- POST(schema_set_url,
                   add_headers(Authorization = paste0("Bearer ", key),
                               "Content-Type" = "application/json"),
                   body = foundrySchema,
                   encode = "json")
  
  response_content <- content(update_response, "text")

  if (status_code(update_response) == 200) {
    print("Schema Update Success")
  } else {
    error_message <- content(update_response, "text")
    print(paste("Schema Update Error:", error_message))
  }
  
}
