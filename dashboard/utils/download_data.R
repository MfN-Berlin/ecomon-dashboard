# -----------------------------------------------------------------------------
# download_data.R
# This file contains functions for building and executing GraphQL queries to
# fetch data for CSV export. It includes:
#
# 1. `build_download_query`: Constructs a GraphQL query to retrieve model
#    inference results based on the provided parameters.
#
# 2. `get_download_data`: Executes the GraphQL query, processes the response,
#    and returns the data in a format suitable for CSV export.
#
# These functions are used to enable users to download activity-related data
# directly from the dashboard.
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# Function: build_download_query
# Description:
#   Constructs a GraphQL query to fetch model inference results for a specific
#   species, model, site, year, and confidence threshold.
#
# Parameters:
#   - species_id (integer): The ID of the species to filter by.
#   - model_id (integer): The ID of the model to filter by.
#   - site_id (integer): The ID of the site to filter by.
#   - year (integer): The year to filter by.
#   - threshold (numeric): The confidence threshold (default = 0.5).
#
# Returns:
#   - A string containing the GraphQL query.
# -----------------------------------------------------------------------------
build_download_query <- function(species_id, model_id, site_id, year, threshold = 0.5) {
  start_date <- sprintf("%d-01-01T00:00:00", year)
  end_date <- sprintf("%d-01-01T00:00:00", year + 1)

  download_query <- sprintf('
  query GetDownloadData {
    model_inference_results(
      where: {
        model_id: { _eq: %d }
        label_id: { _eq: %d }
        confidence: { _gte: %f }
        record: {
          site_id: { _eq: %d }
          record_datetime: {
            _gte: "%s"
            _lt: "%s"
          }
        }
      }
      order_by: { confidence: desc }
    ) {
      confidence
      start_time
      end_time
      model {
        name
      }
      label {
        name
      }
      record {
        site {
          name
          prefix
        }
        record_datetime
      }
    }
  }
  ', model_id, species_id, threshold, site_id, start_date, end_date)

  return(download_query)
}

# -----------------------------------------------------------------------------
# Function: get_download_data
# Description:
#   Executes the GraphQL query built by `build_download_query`, validates the
#   response, and processes the data into a CSV-ready format.
#
# Parameters:
#   - species_id (integer): The ID of the species to filter by.
#   - model_id (integer): The ID of the model to filter by.
#   - site_id (integer): The ID of the site to filter by.
#   - year (integer): The year to filter by.
#   - threshold (numeric): The confidence threshold (default = 0.5).
#
# Returns:
#   - A data.frame containing the processed data for CSV export.
# -----------------------------------------------------------------------------
get_download_data <- function(species_id, model_id, site_id, year, threshold = 0.5) {
  download_query <- build_download_query(species_id, model_id, site_id, year, threshold)

  download_response <- POST(
    url = hasura_url,
    add_headers(.headers = hasura_headers),
    body = list(query = download_query),
    encode = "json"
  )

  # Validate response
  if (http_status(download_response)$category != "Success") {
    stop("Failed to fetch download data from Hasura: ",
         content(download_response, "text", encoding = "UTF-8"))
  }

  download_data <- content(download_response, "parsed", simplifyVector = TRUE)

  # Check for GraphQL errors
  if (!is.null(download_data$errors)) {
    stop("Download data GraphQL query failed: ", paste(download_data$errors, collapse = ", "))
  }

  inference_results <- download_data$data$model_inference_results

  # Return empty data.frame if no results
  if (is.null(inference_results) || length(inference_results) == 0) {
    return(data.frame(
      site_prefix = character(0),
      site_name = character(0),
      record_datetime = character(0),
      start_time = character(0),
      end_time = character(0),
      model_name = character(0),
      species = character(0),
      confidence = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  # Flatten nested data by accessing the nested data.frames directly
  csv_data <- data.frame(
    site_prefix = inference_results$record$site$prefix,
    site_name = inference_results$record$site$name,
    record_datetime = inference_results$record$record_datetime,
    start_time = inference_results$start_time,
    end_time = inference_results$end_time,
    model_name = inference_results$model$name,
    species = inference_results$label$name,
    confidence = inference_results$confidence,
    stringsAsFactors = FALSE
  )

  return(csv_data)
}

# Helper function for null coalescing
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}