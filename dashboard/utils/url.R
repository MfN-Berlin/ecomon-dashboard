# -----------------------------------------------------------------------------
# url.R
# This file contains utility functions for parsing URL parameters and updating
# the Shiny app's reactive values and inputs based on the parsed parameters.
#
# Functions:
# 1. `parse_url_parameters`: Parses query parameters from the URL and updates
#    reactive values and UI inputs accordingly.
#
# Purpose:
# - This functionality allows the app to dynamically adjust its state based on
#   URL parameters, enabling deep linking and sharing of specific app states.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Function: parse_url_parameters
# Description:
#   Parses query parameters from the URL and updates reactive values and UI
#   inputs in the Shiny app. This function enables the app to dynamically
#   adjust its state based on the URL, supporting features like deep linking.
#
# Parameters:
#   - session (Shiny session): The Shiny session object.
#   - url_threshold (reactiveVal): Reactive value for the confidence threshold.
#   - url_site_ids (reactiveVal): Reactive value for the site IDs.
#   - url_species_id (reactiveVal): Reactive value for the species ID.
#   - url_species_name (reactiveVal): Reactive value for the species name.
#   - url_model_id (reactiveVal): Reactive value for the model ID.
#   - url_year (reactiveVal): Reactive value for the year.
#
# Returns:
#   - None. Updates reactive values and UI inputs directly.
# -----------------------------------------------------------------------------
parse_url_parameters <- function(
  session,
  url_site_ids, url_species_id, url_model_id, url_year, url_threshold
) {
  # Access the URL search string
  url_search <- session$clientData$url_search
  if (!is.null(url_search) && url_search != "") {
    # Parse query parameters
    query <- parseQueryString(url_search)

    # Model
    if (!is.null(query$model)) {
      model_id <- as.numeric(query$model)
      url_model_id(model_id)

      # Fetch model name from Hasura and update UI
      tryCatch({
        model_info <- get_model_info(model_id)
        updateSelectInput(session, "canvas_classifier", selected = model_info$name)
      }, error = function(e) {
        cat("Error fetching model info:", e$message, "\n")
      })
    }

    # Site parameters - handle comma-separated list or single ID
    if (!is.null(query$siteId)) {
      # Split by comma and convert to numeric vector
      site_ids <- as.numeric(strsplit(query$siteId, ",")[[1]])
      # Remove any NA values that might result from invalid conversions
      site_ids <- site_ids[!is.na(site_ids)]
      # Set the reactive value with the list of site IDs
      url_site_ids(site_ids)
    }

    # Year
    if (!is.null(query$year)) {
      updateTextInput(session, "canvas_year", value = query$year)
      url_year(as.numeric(query$year))
    }

    # Species
    if (!is.null(query$species)) {
      species_id <- as.numeric(query$species)
      url_species_id(species_id)
    }

    # Threshold
    if (!is.null(query$threshold)) {
      threshold_val <- as.numeric(query$threshold)
      if (!is.na(threshold_val) && threshold_val >= 0.01 && threshold_val <= 1.0) {
        url_threshold(threshold_val)
      }
    }
  }
}
