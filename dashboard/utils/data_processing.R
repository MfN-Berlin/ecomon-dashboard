# data_processing.R
# This file contains functions for loading, processing, and transforming data
# for use in the heatmap visualization. It includes functions for querying
# data from Hasura, processing the results, and preparing the data for plotting.

# Function to load and process data
load_and_process_data <- function(species_id, model_id, site_id, year, threshold) {
  query_results <- execute_hasura_queries(species_id, model_id, site_id, year, threshold)
  print(query_results$inference_results)  # Debug: Print the raw query results
  events_df <- process_query_results(query_results$all_records, query_results$inference_results)
  final_data <- transform_to_heatmap(events_df)
  final_data
}

# Build GraphQL queries
build_queries <- function(species_id, model_id, site_id, year, threshold) {
  start_date <- sprintf("%d-01-01T00:00:00", year)
  end_date <- sprintf("%d-01-01T00:00:00", year + 1)

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
  ', site_id, start_date, end_date)

  inference_query <- sprintf('
  query GetInferenceResults {
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
    ) {
      record_id
      confidence
    }
  }
  ', model_id, species_id, threshold, site_id, start_date, end_date)

  list(
    all_records = all_records_query,
    inference = inference_query
  )
}

# Get count of unique record_datetime above threshold
get_count_above_threshold <- function(species_id, model_id, site_id, year, threshold = 0.5) {
  start_date <- sprintf("%d-01-01T00:00:00", year)
  end_date <- sprintf("%d-01-01T00:00:00", year + 1)

  count_query <- sprintf('
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
  ', model_id, species_id, threshold, site_id, start_date, end_date)

  # Debug: Print the query
  cat("Count query:\n", count_query, "\n\n")

  # Execute the query
  count_response <- POST(
    url = hasura_url,
    add_headers(.headers = hasura_headers),
    body = list(query = count_query),
    encode = "json"
  )

  # Validate response
  if (http_status(count_response)$category != "Success") {
    stop("Failed to fetch count from Hasura: ",
         content(count_response, "text", encoding = "UTF-8"))
  }

  count_data <- content(count_response, "parsed", simplifyVector = TRUE)

  # Check for GraphQL errors
  if (!is.null(count_data$errors)) {
    stop("Count query failed: ", paste(count_data$errors, collapse = ", "))
  }

  # Extract and return the count
  count_result <- count_data$data$model_inference_results_aggregate$aggregate$count

  return(count_result)
}

# Execute Hasura queries
execute_hasura_queries <- function(species_id, model_id, site_id, year, threshold) {
  queries <- build_queries(species_id, model_id, site_id, year, threshold)

  all_records_response <- POST(
    url = hasura_url,
    add_headers(.headers = hasura_headers),
    body = list(query = queries$all_records),
    encode = "json"
  )

  inference_response <- POST(
    url = hasura_url,
    add_headers(.headers = hasura_headers),
    body = list(query = queries$inference),
    encode = "json"
  )

  validate_responses(all_records_response, inference_response)

  all_records_data <- content(all_records_response, "parsed", simplifyVector = TRUE)
  inference_data <- content(inference_response, "parsed", simplifyVector = TRUE)

  check_graphql_errors(all_records_data, inference_data)

  all_records <- all_records_data$data$records
  inference_results <- inference_data$data$model_inference_results

  validate_extracted_data(all_records, inference_results)

  list(
    all_records = all_records,
    inference_results = inference_results
  )
}

# Validate HTTP responses
validate_responses <- function(all_records_response, inference_response) {
  if (http_status(all_records_response)$category != "Success") {
    stop("Failed to fetch all records from Hasura: ",
         content(all_records_response, "text", encoding = "UTF-8"))
  }
  if (http_status(inference_response)$category != "Success") {
    stop("Failed to fetch inference results from Hasura: ",
         content(inference_response, "text", encoding = "UTF-8"))
  }
}

# Check for GraphQL errors
check_graphql_errors <- function(all_records_data, inference_data) {
  if (!is.null(all_records_data$errors)) {
    stop("All records GraphQL query failed")
  }
  if (!is.null(inference_data$errors)) {
    stop("Inference results GraphQL query failed")
  }
}

# Validate extracted data
validate_extracted_data <- function(all_records, inference_results) {
  if (is.null(all_records) || length(all_records) == 0) {
    stop("No data returned from Hasura.")
  }
}

# Process query results into events dataframe - OPTIMIZED
process_query_results <- function(all_records, inference_results) {
  # Convert to data.table immediately for better performance
  if (is.data.frame(all_records)) {
    all_records_dt <- setDT(all_records)
  } else {
    # Convert list to data.table directly
    all_records_dt <- rbindlist(all_records)
  }

  # Create inference lookup as data.table for faster joins
  if (!is.null(inference_results) && length(inference_results) > 0) {
    if (is.data.frame(inference_results)) {
      inference_dt <- setDT(inference_results)
    } else {
      inference_dt <- rbindlist(inference_results)
    }
    # Set key for faster joining
    setkey(inference_dt, record_id)
    setkey(all_records_dt, id)

    # Fast join instead of lookup
    events_dt <- inference_dt[all_records_dt, on = .(record_id = id)]
    events_dt[is.na(confidence), confidence := 0]

    # Select and rename columns
    events_dt <- events_dt[, .(DateTime = record_datetime, Value = confidence)]
  } else {
    # No inference results, set all values to 0
    events_dt <- all_records_dt[, .(DateTime = record_datetime, Value = 0)]
  }

  events_dt
}

# Transform events dataframe to heatmap data - OPTIMIZED
transform_to_heatmap <- function(events_dt) {
  # Keep as data.table throughout the pipeline
  events_dt <- convert_to_wintertime_optimized(events_dt)
  heatmap_data <- create_heatmap_data_optimized(events_dt)
  heatmap_data
}

# Convert to winter time function - HEAVILY OPTIMIZED
convert_to_wintertime_optimized <- function(events_dt) {
  # Parse DateTime column properly (it has ISO format with T separator)
  events_dt[, DateTime := as.POSIXct(DateTime, format = "%Y-%m-%dT%H:%M:%S", tz = "Etc/GMT-1")]

  # Convert to Europe/Berlin (with DST)
  events_dt[, DateTime_Berlin := as.POSIXct(as.numeric(DateTime), origin = "1970-01-01", tz = "Europe/Berlin")]

  # Convert to CET (winter time) by subtracting 1 hour when in CEST (summer time)
  events_dt[, DateTime_Winter := DateTime_Berlin]

  # Check if we're in summer time (CEST) and convert to winter time (CET)
  events_dt[, is_dst := as.POSIXlt(DateTime_Berlin)$isdst == 1]
  events_dt[is_dst == TRUE, DateTime_Winter := DateTime_Berlin - 3600]  # Subtract 1 hour if in CEST

  # Extract date and floored time (10 minute intervals)
  events_dt[, Date := as.Date(DateTime_Winter)]

  # Optimized time flooring - divide by 600 seconds (10 minutes), floor, multiply back
  events_dt[, Time_numeric := as.numeric(DateTime_Winter)]
  events_dt[, Time_floored := floor(Time_numeric / 600) * 600]
  events_dt[, Time := format(as.POSIXct(Time_floored, origin = "1970-01-01",
                                       tz = "Europe/Berlin"), "%H:%M:%S")]

  # Clean up temporary columns
  events_dt[, c("DateTime_Berlin", "is_dst", "DateTime_Winter", "Time_numeric", "Time_floored") := NULL]

  events_dt
}

# Create heatmap data - HEAVILY OPTIMIZED
create_heatmap_data_optimized <- function(events_dt) {
  # Ensure Value is numeric using data.table syntax
  events_dt[, Value := as.numeric(Value)]

  # Use data.table's extremely fast dcast with optimizations
  heatmap_dt <- dcast(
    events_dt,
    Time ~ Date,
    value.var = "Value",
    fill = 0,
    fun.aggregate = function(x) x[1]  # Take first value if multiple
  )

  # Convert date columns to numeric efficiently
  date_cols <- names(heatmap_dt)[-1]  # All columns except Time
  heatmap_dt[, (date_cols) := lapply(.SD, as.numeric), .SDcols = date_cols]

  # Return as data.frame for compatibility
  as.data.frame(heatmap_dt)
}

# Legacy wrapper functions for backward compatibility
create_inference_lookup <- function(inference_results) {
  # This is now handled in process_query_results for better performance
  warning("create_inference_lookup is deprecated - use optimized process_query_results instead")
  if (is.null(inference_results) || length(inference_results) == 0) {
    return(numeric(0))
  }
  if (is.data.frame(inference_results)) {
    setNames(inference_results$confidence, as.character(inference_results$record_id))
  } else if (is.list(inference_results)) {
    record_ids <- sapply(inference_results, function(x) as.character(x$record_id))
    confidences <- sapply(inference_results, function(x) x$confidence)
    setNames(confidences, record_ids)
  } else {
    numeric(0)
  }
}

create_events_dataframe <- function(all_records, inference_lookup) {
  # This is now handled in process_query_results for better performance
  warning("create_events_dataframe is deprecated - use optimized process_query_results instead")
  tryCatch({
    if (is.data.frame(all_records)) {
      record_ids <- as.character(all_records$id)
      confidence_values <- inference_lookup[record_ids]
      confidence_values[is.na(confidence_values)] <- 0
      data.frame(
        DateTime = all_records$record_datetime,
        Value = confidence_values,
        stringsAsFactors = FALSE
      )
    } else if (is.list(all_records)) {
      record_ids <- sapply(all_records, function(x) as.character(x$id))
      record_datetimes <- sapply(all_records, function(x) x$record_datetime)
      confidence_values <- inference_lookup[record_ids]
      confidence_values[is.na(confidence_values)] <- 0
      data.frame(
        DateTime = record_datetimes,
        Value = confidence_values,
        stringsAsFactors = FALSE
      )
    } else {
      stop("all_records must be either a data.frame or a list")
    }
  }, error = function(e) {
    stop("Error processing results: ", e$message)
  })
}

convert_to_wintertime <- function(events_df) {
  # Legacy wrapper - convert to data.table and use optimized function
  warning("convert_to_wintertime is deprecated - use convert_to_wintertime_optimized instead")
  if (!is.data.table(events_df)) {
    events_df <- setDT(events_df)
  }
  convert_to_wintertime_optimized(events_df)
}

create_heatmap_data <- function(events_df) {
  # Legacy wrapper - convert to data.table and use optimized function
  warning("create_heatmap_data is deprecated - use create_heatmap_data_optimized instead")
  if (!is.data.table(events_df)) {
    events_df <- setDT(events_df)
  }
  create_heatmap_data_optimized(events_df)
}