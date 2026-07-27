# Setup Hasura configuration
hasura_url <- Sys.getenv("HASURA_URL")
hasura_headers <- c(
  "Content-Type" = "application/json",
  "x-hasura-admin-secret" = Sys.getenv("HASURA_SECRET")
)

# PERFORMANCE OPTIMIZATION NOTES:
# For optimal performance, ensure these database indexes exist:
#
# Records table:
# CREATE INDEX IF NOT EXISTS idx_records_site_datetime ON records(site_id, record_datetime);
#
# Model inference results table - INDEX PRIORITY ANALYSIS:
#
# CRITICAL (Primary query filter pattern):
# CREATE INDEX IF NOT EXISTS idx_mir_compound ON model_inference_results(model_id, label_id, confidence, record_id);
# - Covers ALL query patterns: model_id + label_id + confidence filtering with record_id access
# - Supports confidence >= threshold queries efficiently with range scan
# - Enables fast joins between records and model_inference_results
# - Single index covers both aggregate queries (count) and detail queries
#
# SECONDARY (For record_id lookups):
# CREATE INDEX IF NOT EXISTS idx_mir_record_id ON model_inference_results(record_id);
# - Needed when querying model_inference_results from records side (nested queries)
# - Supports foreign key joins efficiently
# - Smaller index for fast record-specific lookups
#
# REDUNDANT (Remove for better write performance):
# -- CREATE INDEX IF NOT EXISTS idx_mir_model_label_confidence ON model_inference_results(model_id, label_id, confidence);
# -- This index is completely covered by idx_mir_compound, so it can be dropped to improve INSERT/UPDATE performance
#
# QUERY PATTERN ANALYSIS:
# 1. Threshold filtering: WHERE model_id = ? AND label_id = ? AND confidence >= ? → idx_mir_compound (optimal)
# 2. Record joins: WHERE record_id = ? → idx_mir_record_id (optimal)
# 3. Combined queries: Both patterns above → Both indexes complement each other
# 4. Aggregate distinct: Uses compound index for filtering + record_id for distinct operation

# -----------------------------------------------------------------------------
# HELPER FUNCTIONS
# -----------------------------------------------------------------------------

# Generic function to execute GraphQL queries
execute_graphql_query <- function(query, query_name = "GraphQL") {
  cat(query_name, "query:\n", query, "\n\n")

  response <- POST(
    url = hasura_url,
    add_headers(.headers = hasura_headers),
    body = list(query = query),
    encode = "json"
  )

  # Validate HTTP response
  if (http_status(response)$category != "Success") {
    stop("Failed to execute ", query_name, " query: ",
         content(response, "text", encoding = "UTF-8"))
  }

  # Parse and validate GraphQL response
  data <- content(response, "parsed", simplifyVector = TRUE)

  if (!is.null(data$errors)) {
    stop(query_name, " query failed: ", paste(data$errors, collapse = ", "))
  }

  return(data$data)
}

# Helper to create date range filter
create_date_range <- function(year) {
  list(
    start = sprintf("%d-01-01T00:00:00", year),
    end = sprintf("%d-01-01T00:00:00", year + 1)
  )
}

# Helper to validate single result
validate_single_result <- function(results, entity_type, entity_id) {
  if (length(results) == 0) {
    stop(entity_type, " with ID ", entity_id, " not found")
  }
  results[1, ]
}

# -----------------------------------------------------------------------------
# MAIN FUNCTIONS
# -----------------------------------------------------------------------------

# Get model information from Hasura
get_model_info <- function(model_id) {
  cat("Getting model info for model_id:", model_id, "\n")

  query <- sprintf('
    query GetModelInfo {
      models(where: { id: { _eq: %d } }) {
        id
        name
      }
    }
  ', model_id)

  data <- execute_graphql_query(query, "Model info")
  model_info <- validate_single_result(data$models, "Model", model_id)

  return(list(
    id = model_info$id,
    name = model_info$name
  ))
}

# Get site information from Hasura
get_site_info <- function(site_id) {
  cat("Getting site info for site_id:", site_id, "\n")

  query <- sprintf('
    query GetSiteInfo {
      sites(where: { id: { _eq: %d } }) {
        id
        name
        location {
          lat
          long
        }
      }
    }
  ', site_id)

  data <- execute_graphql_query(query, "Site info")
  site_info <- validate_single_result(data$sites, "Site", site_id)

  return(list(
    id = site_info$id,
    name = site_info$name,
    latitude = site_info$location$lat,
    longitude = site_info$location$long
  ))
}

# Get count of unique record_datetime above threshold
get_count_above_threshold <- function(species_id, model_id, site_id, year, threshold = 0.5) {
  cat("Getting count of unique record_datetime above threshold...\n")

  dates <- create_date_range(year)

  query <- sprintf('
    query CountUniqueRecordDatetimes {
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
      ) {
        record_id
      }
    }
  ', model_id, species_id, threshold, site_id, dates$start, dates$end)

  data <- execute_graphql_query(query, "Count")
  # Ensure the correct count is calculated
  count_result <- if (is.data.frame(data$model_inference_results_max_confidence)) {
    nrow(data$model_inference_results_max_confidence)
  } else {
    length(data$model_inference_results_max_confidence)
  }

  # Log the computed count
  message("Unique record_datetime count above threshold: ", count_result)
  return(count_result)
}

# Build OPTIMIZED GraphQL queries for data loading
build_queries <- function(species_id, model_id, site_id, year, threshold) {
  dates <- create_date_range(year)

  all_records_query <- sprintf('
    query GetAllRecords {
      records(
        where: {
          site_id: { _eq: %d }
          record_datetime: {
            _gte: "%s"
            _lt: "%s"
          }
        },
        order_by: { record_datetime: asc }
      ) {
        id
        record_datetime
      }
    }
  ', site_id, dates$start, dates$end)

  # OPTIMIZED: Remove nested record filter - use direct join for better performance
  inference_max_query <- sprintf('
    query GetInferenceResults {
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
      ) {
        record_id
        confidence
      }
    }
  ', model_id, species_id, threshold, site_id, dates$start, dates$end)

  list(
    all_records = all_records_query,
    inference_max = inference_max_query
  )
}

# Get species information from Hasura
get_species_info <- function(species_id) {
  cat("Getting species info for species_id:", species_id, "\n")

  query <- sprintf('
    query GetSpeciesInfo {
      labels(where: { id: { _eq: %d } }) {
        id
        name
      }
    }
  ', species_id)

  data <- execute_graphql_query(query, "Species info")
  species_info <- validate_single_result(data$labels, "Species", species_id)

  return(list(
    id = species_info$id,
    name = species_info$name
  ))
}

# Fetch the latest threshold for a given label and model
get_latest_threshold <- function(label_id, model_id) {
  cat("Fetching latest threshold for label_id:", label_id, "model_id:", model_id, "\n")

  query <- sprintf('
    query GetLatestThreshold {
      thresholds(
        where: {
          label_id: { _eq: %d }
          model_id: { _eq: %d }
          threshold_type: { _neq: "final" }
        }
        order_by: { set_at: desc }
        limit: 1
      ) {
        threshold
        threshold_type
      }
    }
  ', label_id, model_id)

  data <- tryCatch({
    execute_graphql_query(query, "Latest threshold")
  }, error = function(e) {
    message("Error executing GraphQL query:", e$message)
    data <- NULL
  })
  message("First query returned data structure:", str(data))

  # If no non-NULL threshold found, try to get a NULL threshold
  if (is.null(data) || !is.list(data) || !exists("thresholds", where = data) || 
      is.null(data$thresholds) || length(data$thresholds) == 0) {
    query_null <- sprintf('\n      query GetLatestNullThreshold {\n        thresholds(\n          where: {\n            label_id: { _eq: %d }\n            model_id: { _eq: %d }\n            threshold_type: { _is_null: true }\n          }\n          order_by: { set_at: desc }\n          limit: 1\n        ) {\n          threshold\n          threshold_type\n        }\n      }\n    ', label_id, model_id)
    
    data <- tryCatch({
      execute_graphql_query(query_null, "Latest null threshold")
    }, error = function(e) {
      message("Error executing GraphQL query:", e$message)
      data <- NULL
    })
  }

  # Check if data is a list and contains the expected structure
  if (is.list(data) && exists("thresholds", where = data) && !is.null(data$thresholds) && length(data$thresholds) > 0) {
    # With simplifyVector=TRUE, data$thresholds might be a data frame or a list
    first_elem <- data$thresholds
    
    # If it's a data frame, get the first row
    if (is.data.frame(first_elem)) {
      first_elem <- first_elem[1, ]
    } else if (is.list(first_elem) && length(first_elem) > 0) {
      # If it's a list of objects (multiple rows), get the first one
      first_elem <- first_elem[[1]]
    }
    
    # Check if the first element is a list/object with named fields
    if (is.list(first_elem) && length(first_elem) > 0 && !is.null(names(first_elem))) {
      # New format with threshold_type field
      threshold_type <- first_elem$threshold_type
      is_final_val <- first_elem$is_final
      threshold_val <- first_elem$threshold
      
      # Handle NULL threshold_type based on is_final value
      # If both are NULL, treat as preliminary for backwards compatibility
      if (is.null(threshold_type)) {
        if (!is.null(is_final_val)) {
          if (is_final_val) {
            threshold_type <- "final"
          } else {
            threshold_type <- "preliminary"
          }
        } else {
          # Both threshold_type and is_final are NULL - treat as preliminary
          threshold_type <- "preliminary"
        }
      } else {
        # Normalize the threshold_type: trim whitespace and convert to lowercase
        threshold_type <- tolower(trimws(threshold_type))
      }
      return(list(threshold = threshold_val, threshold_type = threshold_type))
    } else if (is.numeric(first_elem)) {
      # Old format - just a numeric value (shouldn't happen with current query, but handle it)
      message("Warning: Received numeric value instead of object from GraphQL")
      return(list(threshold = first_elem, threshold_type = "preliminary"))
    } else {
      message("Unexpected data structure in thresholds: ", str(first_elem))
      return(NULL)
    }
  } else {
    message("Data does not contain expected structure or is NULL")
    return(NULL)
  }
}

# Fetch the latest final threshold for a given label and model (threshold_type = "final")
get_latest_final_threshold <- function(label_id, model_id) {
  cat("Fetching latest FINAL threshold for label_id:", label_id, "model_id:", model_id, "\n")

  query <- sprintf('\n    query GetLatestFinalThreshold {\n      thresholds(\n        where: {\n          label_id: { _eq: %d }\n          model_id: { _eq: %d }\n          threshold_type: { _eq: "final" }\n        }\n        order_by: { set_at: desc }\n        limit: 1\n      ) {\n        threshold\n        threshold_type\n      }\n    }\n  ', label_id, model_id)

  data <- tryCatch({
    execute_graphql_query(query, "Latest final threshold")
  }, error = function(e) {
    message("Error executing GraphQL query:", e$message)
    return(NULL)
  })

  if (is.list(data) && exists("thresholds", where = data) && !is.null(data$thresholds) && length(data$thresholds) > 0) {
    first_elem <- data$thresholds[[1]]
    
    if (is.list(first_elem) && length(first_elem) > 0 && !is.null(names(first_elem))) {
      threshold_type <- first_elem$threshold_type
      threshold_val <- first_elem$threshold
      return(list(threshold = threshold_val, threshold_type = threshold_type))
    } else if (is.numeric(first_elem)) {
      # Old format - just a numeric value
      message("Warning: Received numeric value instead of object from GraphQL")
      return(list(threshold = first_elem, threshold_type = "final"))
    } else {
      message("Unexpected data structure in final thresholds: ", str(first_elem))
      return(NULL)
    }
  }
  return(NULL)
}

# Store the current threshold value in the thresholds table
store_threshold <- function(label_id, model_id, threshold_value, threshold_type = "experimental") {
  cat("Storing threshold for label_id:", label_id, "model_id:", model_id, "threshold:", threshold_value, "type:", threshold_type, "\n")

  mutation <- sprintf('\n    mutation InsertThreshold {\n      insert_thresholds_one(object: {\n        label_id: %d,\n        model_id: %d,\n        threshold: %f,\n        threshold_type: "%s"\n      }) {\n        id\n        label_id\n        model_id\n        threshold\n        set_at\n        threshold_type\n      }\n    }\n  ', label_id, model_id, threshold_value, threshold_type)

  data <- execute_graphql_query(mutation, "Store threshold")
  return(data$insert_thresholds_one)
}

# Store the current threshold value as final in the thresholds table
store_final_threshold <- function(label_id, model_id, threshold_value) {
  cat("Storing final threshold for label_id:", label_id, "model_id:", model_id, "threshold:", threshold_value, "\n")

  mutation <- sprintf('\n    mutation InsertFinalThreshold {\n      insert_thresholds_one(object: {\n        label_id: %d,\n        model_id: %d,\n        threshold: %f,\n        threshold_type: "final",
        is_final: true\n      }) {\n        id\n        label_id\n        model_id\n        threshold\n        set_at\n        threshold_type\n      }\n    }\n  ', label_id, model_id, threshold_value)

  data <- execute_graphql_query(mutation, "Store final threshold")
  return(data$insert_thresholds_one)
}

# Store the current threshold value as preliminary in the thresholds table
store_preliminary_threshold <- function(label_id, model_id, threshold_value) {
  cat("Storing preliminary threshold for label_id:", label_id, "model_id:", model_id, "threshold:", threshold_value, "\n")

  mutation <- sprintf('\n    mutation InsertPreliminaryThreshold {\n      insert_thresholds_one(object: {\n        label_id: %d,\n        model_id: %d,\n        threshold: %f,\n        threshold_type: "preliminary"\n      }) {\n        id\n        label_id\n        model_id\n        threshold\n        set_at\n        threshold_type\n      }\n    }\n  ', label_id, model_id, threshold_value)

  data <- execute_graphql_query(mutation, "Store preliminary threshold")
  return(data$insert_thresholds_one)
}

# Check if a final threshold exists for a given label and model
final_threshold_exists <- function(label_id, model_id) {
  cat("Checking if final threshold exists for label_id:", label_id, "model_id:", model_id, "\n")

  query <- sprintf('
    query CheckFinalThreshold {
      thresholds(
        where: {
          label_id: { _eq: %d }
          model_id: { _eq: %d }
          threshold_type: { _eq: "final" }
        }
        limit: 1
      ) {
        id
      }
    }
  ', label_id, model_id)

  data <- tryCatch({
    execute_graphql_query(query, "Check final threshold")
  }, error = function(e) {
    message("Error executing GraphQL query:", e$message)
    return(FALSE)
  })

  if (is.list(data) && exists("thresholds", where = data) && 
      !is.null(data$thresholds) && length(data$thresholds) > 0) {
    return(TRUE)
  }
  return(FALSE)
}

# Reset threshold to system default in the thresholds table
reset_threshold_to_default <- function(label_id, model_id, threshold_value) {
  cat("Resetting threshold for label_id:", label_id, "model_id:", model_id, "to default:", threshold_value, "\n")

  # First try to update any existing record
  mutation <- sprintf('\n    mutation UpdateThresholdToDefault {\n      update_thresholds(\n        where: { label_id: { _eq: %d }, model_id: { _eq: %d } },\n        _set: { threshold: %f, threshold_type: "default", is_final: false }\n      ) {\n        affected_rows\n        returning {\n          id\n          label_id\n          model_id\n          threshold\n          threshold_type\n          is_final\n        }\n      }\n    }\n  ', label_id, model_id, threshold_value)

  data <- execute_graphql_query(mutation, "Reset threshold to default")
  
  # If no rows were updated, insert a new default threshold record
  if (data$update_thresholds$affected_rows == 0) {
    insert_mutation <- sprintf('\n      mutation InsertDefaultThreshold {\n        insert_thresholds_one(object: {\n          label_id: %d,\n          model_id: %d,\n          threshold: %f,\n          threshold_type: "default",
          is_final: false\n        }) {\n          id\n          label_id\n          model_id\n          threshold\n          threshold_type\n          is_final\n        }\n      }\n    ', label_id, model_id, threshold_value)
    data <- execute_graphql_query(insert_mutation, "Insert default threshold")
  }
  
  return(data$update_thresholds)
}
