# Helper function to compute sunrise and sunset times for each date in heatmap_long
# Returns: sunrise, sunset, dawn and dusk
# Requires the 'suncalc' package: install.packages("suncalc")
compute_sun_times <- function(heatmap_long, lat, lon) {
  # Extract unique dates from heatmap_long
  unique_dates <- unique(heatmap_long$Date)
  # Compute sunlight times for all dates
  sun_times <- getSunlightTimes(
    date = unique_dates, lat = lat, lon = lon, keep = c("sunrise", "sunset", "dawn", "dusk")
  )
  sun_times
}

# Helper function to floor POSIXct times to the nearest 10 minutes and return as "HH:MM:SS"
floor_time_to_10min <- function(times) {
  # times: a vector or list of POSIXct times
  floored <- lapply(times, function(t) {
    if (is.na(t)) return(NA_character_)
    # Floor to nearest 10 minutes
    t_floor <- as.POSIXct(floor(as.numeric(t) / 600) * 600, origin = "1970-01-01", tz = "Etc/GMT-1")
    format(t_floor, "%H:%M:%S")
  })
  unlist(floored)
}