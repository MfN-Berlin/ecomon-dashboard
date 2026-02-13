# data_processing.R
# This file contains functions for loading, processing, and transforming data
# for use in the heatmap visualization. It includes functions for querying
# data from Hasura, processing the results, and preparing the data for plotting.

# Required libraries
library(data.table)
library(httr)
library(jsonlite)

# Function to load and process data
load_and_process_data <- function(species_id, model_id, site_id, year, threshold) {
  # Log the start of processing with parameters
  message("Starting data processing for species: ", species_id,
          ", model: ", model_id,
          ", site: ", site_id,
          ", year: ", year,
          ", threshold: ", threshold)

  tryCatch({
    # Execute queries and log
    message("Executing Hasura queries...")
    query_results <- execute_hasura_queries(species_id, model_id, site_id, year, threshold)

    # Log the raw data received
    message("Received ", length(query_results$all_records), " records and ",
            length(query_results$inference_results), " inference results")

    # Process results and log
    message("Processing query results...")
    events_df <- process_query_results(query_results$all_records, query_results$inference_results, threshold)

    # Log the processed events
    message("Processed ", nrow(events_df), " events with value range: ",
            min(events_df$Value, na.rm = TRUE), "-",
            max(events_df$Value, na.rm = TRUE))

    # Transform to heatmap and log
    message("Transforming to heatmap format...")
    final_data <- transform_to_heatmap(events_df)

    # Log the final output
    message("Data processing completed successfully")
    message("Final heatmap dimensions: ", nrow(final_data), " rows × ", ncol(final_data), " columns")

    # Log threshold application in final data
    above_threshold <- final_data[, -1] > threshold
    count_above <- sum(above_threshold, na.rm = TRUE)
    message("- Cells above threshold (", threshold, "): ", count_above)

    # Log the specific values above threshold
#    if (count_above > 0) {
#      message("Values above threshold:")
#      # Convert to data.table for easier manipulation
#      dt <- as.data.table(final_data)
#      # Get all cells above threshold
#      above_cells <- dt[, .SD > threshold, .SDcols = -1]
#      # Get the row and column names for above threshold cells
#      row_names <- dt$Time
#      col_names <- names(dt)[-1]

#      # Log each value above threshold with its position
#      for (col in col_names) {
#        col_values <- dt[[col]]
#        above_in_col <- col_values > threshold
#        if (any(above_in_col)) {
#          for (i in which(above_in_col)) {
#            message("  - ", row_names[i], " on ", col, ": ", col_values[i])
#          }
#        }
#      }
#    }

    final_data
  }, error = function(e) {
    # Log any errors that occur
    message("ERROR in data processing: ", e$message)
    stop(e)
  })
}

# Execute Hasura queries
execute_hasura_queries <- function(species_id, model_id, site_id, year, threshold) {
  queries <- build_queries(species_id, model_id, site_id, year, threshold)

  # Log the queries being executed
  message("Executing Hasura queries with threshold: ", threshold)
  message("- All records query: ", gsub("\\s+", " ", queries$all_records))
  message("- Inference query: ", gsub("\\s+", " ", queries$inference))

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

  # Log response status
  message("Response status:")
  message("- All records: ", http_status(all_records_response)$message)
  message("- Inference: ", http_status(inference_response)$message)

  validate_responses(all_records_response, inference_response)

  all_records_data <- content(all_records_response, "parsed", simplifyVector = TRUE)
  inference_data <- content(inference_response, "parsed", simplifyVector = TRUE)

  # Log basic response info
  message("Response data structure:")
  message("- All records: ", ifelse(is.null(all_records_data$data$records),
                                   "NULL", paste(length(all_records_data$data$records), "records")))
  message("- Inference: ", ifelse(is.null(inference_data$data$model_inference_results),
                                 "NULL", paste(length(inference_data$data$model_inference_results), "results")))

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
process_query_results <- function(all_records, inference_results, threshold) {
  # Log input data structure and counts
  message("Processing query results with threshold: ", threshold)
  message("- Input records: ", length(all_records))
  message("- Input inference results: ", length(inference_results))

  # Convert to data.table immediately for better performance
  if (is.data.frame(all_records)) {
    all_records_dt <- setDT(all_records)
    message("- Records data.frame converted to data.table with ", nrow(all_records_dt), " rows")
  } else {
    # Convert list to data.table directly
    all_records_dt <- rbindlist(all_records)
    message("- Records list converted to data.table with ", nrow(all_records_dt), " rows")
  }

  # Log record_datetime information
  if (nrow(all_records_dt) > 0) {
    unique_datetimes <- unique(all_records_dt$record_datetime)
    message("- Unique record_datetime values: ", length(unique_datetimes))
    message("- Date range: ", min(unique_datetimes), " to ", max(unique_datetimes))

    # Check if confidence exists in the data
    if ("confidence" %in% names(all_records_dt)) {
      above_threshold <- sum(all_records_dt$confidence > threshold, na.rm = TRUE)
      message("- Records above threshold (", threshold, "): ", above_threshold)
    }
  }

  # Create inference lookup as data.table for faster joins
  if (!is.null(inference_results) && length(inference_results) > 0) {
    if (is.data.frame(inference_results)) {
      inference_dt <- setDT(inference_results)
      message("- Inference data.frame converted to data.table with ", nrow(inference_dt), " rows")
    } else {
      inference_dt <- rbindlist(inference_results)
      message("- Inference list converted to data.table with ", nrow(inference_dt), " rows")
    }

    # Log inference data details
    if (nrow(inference_dt) > 0) {
      message("- Inference confidence range: ",
              min(inference_dt$confidence, na.rm = TRUE), "-",
              max(inference_dt$confidence, na.rm = TRUE))
      above_threshold <- sum(inference_dt$confidence > threshold, na.rm = TRUE)
      message("- Inference results above threshold (", threshold, "): ", above_threshold)
    }

    # Set key for faster joining
    setkey(inference_dt, record_id)
    setkey(all_records_dt, id)

    # Fast join instead of lookup
    events_dt <- inference_dt[all_records_dt, on = .(record_id = id)]
    events_dt[is.na(confidence), confidence := 0]

    # Log join results
    message("- After join: ", nrow(events_dt), " rows")
    message("- After join confidence range: ",
            min(events_dt$confidence, na.rm = TRUE), "-",
            max(events_dt$confidence, na.rm = TRUE))

    # Log threshold filtering after join
    above_threshold <- sum(events_dt$confidence > threshold, na.rm = TRUE)
    message("- Records above threshold (", threshold, ") after join: ", above_threshold)

    # Select and rename columns
    events_dt <- events_dt[, .(DateTime = record_datetime, Value = confidence)]
  } else {
    # No inference results, set all values to 0
    message("No inference results provided - setting all values to 0")
    events_dt <- all_records_dt[, .(DateTime = record_datetime, Value = 0)]
  }

  # Final logging before return
  if (nrow(events_dt) > 0) {
    unique_datetimes <- unique(events_dt$DateTime)
    message("- Final unique DateTime values: ", length(unique_datetimes))
    message("- Final value range: ",
            min(events_dt$Value, na.rm = TRUE), "-",
            max(events_dt$Value, na.rm = TRUE))

    above_threshold <- sum(events_dt$Value > threshold, na.rm = TRUE)
    message("- Final records above threshold (", threshold, "): ", above_threshold)
  }

  events_dt
}

# Transform events dataframe to heatmap data - OPTIMIZED
transform_to_heatmap <- function(events_dt) {
  message("Transforming to heatmap data - input: ", nrow(events_dt), " rows")

  # Keep as data.table throughout the pipeline
  events_dt <- convert_to_wintertime_optimized(events_dt)
  message("After wintertime conversion: ", nrow(events_dt), " rows")

  heatmap_data <- create_heatmap_data_optimized(events_dt)
  message("Heatmap data created with dimensions: ",
          nrow(heatmap_data), " rows × ", ncol(heatmap_data), " columns")

  # Log column names for reference
#  message("Heatmap columns: ", paste(names(heatmap_data), collapse = ", "))

  heatmap_data
}

# Convert to winter time function - HEAVILY OPTIMIZED
convert_to_wintertime_optimized <- function(events_dt) {
  # Parse DateTime column as-is (it has ISO format with T separator)
  events_dt[, DateTime := as.POSIXct(DateTime, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")]

  # Optimized time flooring - divide by 600 seconds (10 minutes), floor, multiply back
  events_dt[, Time_numeric := as.numeric(DateTime)]
  events_dt[, Time_floored := floor(Time_numeric / 600) * 600]

  # Extract date and time from the floored timestamp
  events_dt[, DateTime_Floored := as.POSIXct(Time_floored, origin = "1970-01-01", tz = "UTC")]
  events_dt[, Date := as.Date(DateTime_Floored)]
  events_dt[, Time := format(DateTime_Floored, "%H:%M:%S")]

  # Clean up temporary columns
  events_dt[, c("Time_numeric", "Time_floored", "DateTime_Floored") := NULL]

  events_dt
}

# Create heatmap data - HEAVILY OPTIMIZED
create_heatmap_data_optimized <- function(events_dt) {
  message("Creating heatmap data from ", nrow(events_dt), " events")

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

  # Log aggregation results
  message("- Heatmap matrix created with ", nrow(heatmap_dt), " time slots and ",
          ncol(heatmap_dt) - 1, " dates")
  # FIXED: Corrected data.table syntax for counting non-zero cells
  message("- Non-zero cells: ", sum(heatmap_dt[, .SD > 0, .SDcols = -1]))

  # Convert date columns to numeric efficiently
  date_cols <- names(heatmap_dt)[-1]  # All columns except Time
  heatmap_dt[, (date_cols) := lapply(.SD, as.numeric), .SDcols = date_cols]

  # Return as data.frame for compatibility
  as.data.frame(heatmap_dt)
}

# Legacy wrapper functions for backward compatibility
load_and_process_data_BACKUP <- function(species_id, model_id, site_id, year, threshold) {
  query_results <- execute_hasura_queries(species_id, model_id, site_id, year, threshold)
  events_df <- process_query_results(query_results$all_records, query_results$inference_results, threshold)
  final_data <- transform_to_heatmap(events_df)
  final_data
}

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