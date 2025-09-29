# -----------------------------------------------------------------------------
# ephem.R
# This file contains helper functions for computing and processing astronomical
# data, such as sunrise, sunset, dawn, and dusk times, as well as time
# formatting. These functions are used to enhance the heatmap visualization
# with additional temporal context.
#
# Functions:
# 1. `compute_sun_times`: Computes sunrise, sunset, dawn, and dusk times for
#    each date in the heatmap data using the 'suncalc' package.
# 2. `floor_time_to_10min`: Floors POSIXct times to the nearest 10 minutes and
#    formats them as "HH:MM:SS".
#
# Dependencies:
# - Requires the 'suncalc' package for astronomical calculations.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Function: compute_sun_times
# Description:
#   Computes sunrise, sunset, dawn, and dusk times for each unique date in the
#   heatmap data.
#
# Parameters:
#   - heatmap_long (data.frame): A data frame containing a 'Date' column with
#     unique dates for which sunlight times will be computed.
#   - lat (numeric): The latitude of the location.
#   - lon (numeric): The longitude of the location.
#
# Returns:
#   - A data.frame containing the computed sunlight times (sunrise, sunset,
#     dawn, and dusk) for each date.
#
# Dependencies:
#   - Requires the 'suncalc' package. Install it with:
#     install.packages("suncalc")
# -----------------------------------------------------------------------------
compute_sun_times <- function(heatmap_long, lat, lon) {
  # Extract unique dates from heatmap_long
  unique_dates <- unique(heatmap_long$Date)
  # Compute sunlight times for all dates
  sun_times <- getSunlightTimes(
    date = unique_dates,
    lat = lat, lon = lon,
    keep = c("sunrise", "sunset", "dawn", "dusk", "nauticalDawn", "nauticalDusk", "nightEnd", "night")
  )
  sun_times
}

# -----------------------------------------------------------------------------
# Function: floor_time_to_10min
# Description:
#   Floors POSIXct times to the nearest 10 minutes and formats them as
#   "HH:MM:SS".
#
# Parameters:
#   - times (vector or list): A vector or list of POSIXct times to be floored.
#
# Returns:
#   - A character vector of times floored to the nearest 10 minutes, formatted
#     as "HH:MM:SS".
# -----------------------------------------------------------------------------
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

# Helper function to floor time to the nearest minute
floor_time_to_1min <- function(times) {
  t_floor <- as.POSIXct(floor(as.numeric(times) / 60) * 60, origin = "1970-01-01", tz = "Etc/GMT-1")
  format(t_floor, "%H:%M:00")
}
