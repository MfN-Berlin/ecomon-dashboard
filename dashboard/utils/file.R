# Generate filename (with or without extension) based on site, model, year, species and threshold
# e.g. HM_BBNSF_1205H_2024_birdnetplus-v3_0_euna_1k_preview2_Turdus_philomelos_th0750
get_filename <- function(
  prefix = NULL,
  site_name = NULL, model_name = NULL, year = NULL, species_name = NULL, threshold = NULL,
  extension = NULL
) {
  # Sanitize function for filenames
  sanitize_filename <- function(filename) {
    gsub("[^a-zA-Z0-9_-]", "_", filename)
  }

  # Function to format threshold for filename
  format_threshold <- function(threshold) {
    if (is.null(threshold)) return(NULL)

    # Multiply by 1000 and round to nearest integer
    threshold_int <- round(threshold * 1000)

    # Format with leading zeros to make 4 digits
    sprintf("%04d", threshold_int)
  }

  # Create filename components
  filename_parts <- list()

  filename_parts$prefix <- prefix

  if (!is.null(site_name)) {
    code <- site_name
    # Split by comma and take the second part if available
    parts <- strsplit(site_name, ",")[[1]]
    if (length(parts) > 1) {
      # Trim whitespace and return the part after comma
      code <- trimws(parts[2])
    }
    filename_parts$site <- sanitize_filename(code)
  }

  filename_parts$year <- year

  if (!is.null(model_name)) {
    filename_parts$model <- sanitize_filename(model_name)
  }

  if (!is.null(species_name)) {
    filename_parts$species <- sanitize_filename(species_name)
  }

  if (!is.null(threshold)) {
    filename_parts$threshold <- paste0("th", format_threshold(threshold))
  }

  # Generate filename
  filename <- paste(unlist(filename_parts), collapse = "_")

  if (!is.null(extension)) {
    filename <- paste0(filename, ".", extension)
  }

  message("Generated filename: ", filename)
  return(filename)
}