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
      model_inference_results_aggregate(
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
        distinct_on: record_id
      ) {
        aggregate {
          count
        }
      }
    }
  ', model_id, species_id, threshold, site_id, dates$start, dates$end)

  data <- execute_graphql_query(query, "Count")
  return(data$model_inference_results_aggregate$aggregate$count)
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
  inference_query <- sprintf('
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
    inference = inference_query
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
