parse_url_parameters <- function(
  session,
  url_threshold, url_site_ids, url_site_name, url_species_id, url_species_name, url_model_id, url_lat, url_lon, url_year
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

      if (!is.null(query$modelName)) {
        updateSelectInput(session, "canvas_classifier", selected = query$modelName)
      }
    }

    # Site parameters
    if (!is.null(query$siteId)) {
      site_id <- as.numeric(query$siteId)
      url_site_ids(c(site_id))
    }
    if (!is.null(query$siteName)) {
      site_name <- URLdecode(query$siteName)
      updateTextInput(session, "canvas_site", value = site_name)
      url_site_name(site_name)
    }
    if (!is.null(query$lat) && !is.null(query$lon)) {
      lat <- as.numeric(query$lat)
      lon <- as.numeric(query$lon)
      url_lat(lat)
      url_lon(lon)
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
    if (!is.null(query$speciesName)) {
      species_name <- URLdecode(query$speciesName)
      updateTextInput(session, "canvas_species", value = species_name)
      url_species_name(species_name)
    }

    # Threshold
    if (!is.null(query$threshold)) {
      threshold <- as.numeric(query$threshold)
      updateTextInput(session, "canvas_threshold", value = threshold)
      url_threshold(threshold)
    }
  }
}