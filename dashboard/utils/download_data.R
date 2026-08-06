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
    model_inference_results_max_confidence(
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
        filename
      }
    }
  }
  ', model_id, species_id, threshold, site_id, start_date, end_date)

  return(download_query)
}

# -----------------------------------------------------------------------------
# Function: build_thresholds_query
# Description:
#   Constructs a GraphQL query to fetch the latest thresholds (experimental,
#   preliminary, final) for a specific model and label combination.
#
# Parameters:
#   - model_id (integer): The ID of the model to filter by.
#   - label_id (integer): The ID of the label to filter by.
#
# Returns:
#   - A string containing the GraphQL query.
# -----------------------------------------------------------------------------
build_thresholds_query <- function(model_id, label_id) {
  thresholds_query <- sprintf('
  query GetThresholds {
    thresholds(
      where: {
        model_id: { _eq: %d }
        label_id: { _eq: %d }
      }
      order_by: { set_at: desc }
    ) {
      threshold_type
      threshold
      set_at
    }
  }
  ', model_id, label_id)

  return(thresholds_query)
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

  # Fetch thresholds for this model and label
  thresholds_query <- build_thresholds_query(model_id, species_id)
  thresholds_response <- POST(
    url = hasura_url,
    add_headers(.headers = hasura_headers),
    body = list(query = thresholds_query),
    encode = "json"
  )

  # Validate thresholds response
  if (http_status(thresholds_response)$category != "Success") {
    stop("Failed to fetch thresholds from Hasura: ",
         content(thresholds_response, "text", encoding = "UTF-8"))
  }

  thresholds_data <- content(thresholds_response, "parsed", simplifyVector = TRUE)

  # Check for GraphQL errors in thresholds query
  if (!is.null(thresholds_data$errors)) {
    stop("Thresholds GraphQL query failed: ", paste(thresholds_data$errors, collapse = ", "))
  }

  # Process thresholds into a named vector by threshold_type
  # Get the latest (first) threshold for each type from the ordered results
  threshold_values <- c(
    experimental = NA_real_,
    preliminary = NA_real_,
    final = NA_real_
  )

  if (!is.null(thresholds_data$data$thresholds) && length(thresholds_data$data$thresholds) > 0) {
    thresholds_list <- thresholds_data$data$thresholds
    # Results are ordered by set_at desc, so first occurrence of each type is the latest
    seen_types <- character(0)
    for (t in seq_along(thresholds_list$threshold_type)) {
      threshold_type <- thresholds_list$threshold_type[t]
      if (!(threshold_type %in% seen_types) && threshold_type %in% c("experimental", "preliminary", "final")) {
        seen_types <- c(seen_types, threshold_type)
        threshold_val <- as.numeric(thresholds_list$threshold[t])
        threshold_values[[threshold_type]] <- threshold_val
      }
    }
  }

  # Return empty data.frame if no results
  if (is.null(inference_results) || length(inference_results) == 0) {
    return(data.frame(
      site_prefix = character(0),
      site_name = character(0),
      date = character(0),
      time = character(0),
      filename = character(0),
      start_time = character(0),
      end_time = character(0),
      model_name = character(0),
      species = character(0),
      confidence = numeric(0),
      ManualValidation = character(0),
      VocalizationTypeCode = character(0),
      Manual_Trigger = character(0),
      Manual_OtherSounds_5s = character(0),
      Notes_on_complete_snippet_10_15s = character(0),
      experimental_threshold = character(0),
      preliminary_threshold = character(0),
      final_threshold = character(0),
      stringsAsFactors = FALSE
    ))
  }

  # Flatten nested data by accessing the nested data.frames directly
  num_rows <- length(inference_results$confidence)
  
  # Split record_datetime into date and time
  # Format is "YYYY-MM-DD HH:MM:SS" based on the database
  datetime_values <- inference_results$record$record_datetime
  date_values <- substr(datetime_values, 1, 10)
  time_values <- substr(datetime_values, 12, 19)
  
  csv_data <- data.frame(
    site_prefix = inference_results$record$site$prefix,
    site_name = inference_results$record$site$name,
    date = date_values,
    time = time_values,
    filename = inference_results$record$filename,
    start_time = inference_results$start_time,
    end_time = inference_results$end_time,
    model_name = inference_results$model$name,
    species = inference_results$label$name,
    confidence = inference_results$confidence,
    ManualValidation = rep("", num_rows),
    VocalizationTypeCode = rep("", num_rows),
    Manual_Trigger = rep("", num_rows),
    Manual_OtherSounds_5s = rep("", num_rows),
    Notes_on_complete_snippet_10_15s = rep("", num_rows),
    experimental_threshold = rep(threshold_values["experimental"], num_rows),
    preliminary_threshold = rep(threshold_values["preliminary"], num_rows),
    final_threshold = rep(threshold_values["final"], num_rows),
    stringsAsFactors = FALSE
  )

  # Convert threshold columns: NA to empty string for clean CSV/Excel output
  csv_data$experimental_threshold <- ifelse(is.na(csv_data$experimental_threshold), "", as.character(csv_data$experimental_threshold))
  csv_data$preliminary_threshold <- ifelse(is.na(csv_data$preliminary_threshold), "", as.character(csv_data$preliminary_threshold))
  csv_data$final_threshold <- ifelse(is.na(csv_data$final_threshold), "", as.character(csv_data$final_threshold))

  return(csv_data)
}

# Helper function for null coalescing
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}