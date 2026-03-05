# Function to create an acoustic activity plot (returns plotly object)
# Parameters:
#   heatmap_result (data.frame): Raw heatmap data with columns "Time" and dates as columns.
#   threshold_val (numeric): Threshold value for filtering data.
#   year (numeric): The year for which the acoustic activity is being generated.
#   interval (character): The interval for aggregating data (month, 10-day, 5-day, daily).
#   site_name (character, optional): Name of the site.
#   model_name (character, optional): Name of the model.
#   species_name (character, optional): Name of the species.
# Returns:
#   plotly object: An interactive acoustic activity plot.
render_acoustic_activity_plot <- function(
  heatmap_result, threshold_val, year, interval = "month",
  site_name = NULL, model_name = NULL, species_name = NULL
) {
  if (is.null(heatmap_result)) {
    return(plotly::plot_ly() %>%
      plotly::layout(
        title = "Loading data...",
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE)
      ))
  }

  # Prepare data
  heatmap_long <- prepare_heatmap_data(heatmap_result, threshold_val, year)

  # Filter out values that are below threshold (marked as -1)
  acoustic_data <- heatmap_long[heatmap_long$Value >= 0, ]

  if (nrow(acoustic_data) == 0) {
    return(plotly::plot_ly() %>%
      plotly::layout(
        title = "No data available for acoustic activity",
        xaxis = list(title = "Interval"),
        yaxis = list(title = "Minutes Above Threshold")
      ))
  }

  # Aggregate data based on the selected interval
  acoustic_data$Date <- as.Date(acoustic_data$Date)
  if (interval == "month") {
    acoustic_data$Interval <- format(acoustic_data$Date, "%Y-%m")
  } else if (interval == "10-day") {
    acoustic_data$Interval <- paste0(format(acoustic_data$Date, "%Y-%m-"),
                                    floor((as.numeric(format(acoustic_data$Date, "%d")) - 1) / 10) * 10 + 1)
  } else if (interval == "5-day") {
    acoustic_data$Interval <- paste0(format(acoustic_data$Date, "%Y-%m-"),
                                    floor((as.numeric(format(acoustic_data$Date, "%d")) - 1) / 5) * 5 + 1)
  } else if (interval == "daily") {
    acoustic_data$Interval <- format(acoustic_data$Date, "%Y-%m-%d")
  }

  aggregated_data <- aggregate(Value ~ Interval, data = acoustic_data, FUN = function(x) sum(x >= threshold_val))
  colnames(aggregated_data)[2] <- "MinutesAboveThreshold"

  # For daily interval, convert Interval to Date for proper scaling
  if (interval == "daily") {
    aggregated_data$Date <- as.Date(aggregated_data$Interval)
  }

  # Create base plot using ggplot2
  if (interval == "daily") {
    p <- ggplot(aggregated_data, aes(x = Date, y = MinutesAboveThreshold, group = 1)) +
      geom_line() +
      geom_point() +
      labs(
        x = paste("Interval (", interval, ")", sep = ""),
        y = "Minutes Above Threshold",
        title = paste("Acoustic Activity Over Time")
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 12),
        panel.border = element_rect(color = "black", fill = NA),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank()
      ) +
      scale_x_date(date_breaks = "10 days", date_labels = "%Y-%m-%d")
  } else {
    p <- ggplot(aggregated_data, aes(x = Interval, y = MinutesAboveThreshold, group = 1)) +
      geom_line() +
      geom_point() +
      labs(
        x = paste("Interval (", interval, ")", sep = ""),
        y = "Minutes Above Threshold",
        title = paste("Acoustic Activity Over Time")
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 12),
        panel.border = element_rect(color = "black", fill = NA),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank()
      )
  }

  # Convert to plotly
  plt <- plotly::ggplotly(p, tooltip = c("x", "y", "color"))

  # Configure plotly
  plotly::config(
    plt,
    displayModeBar = FALSE,
    displaylogo = FALSE
  )
}

get_min_max_dates <- function(diel_data) {
  # Get the first and last date in the data
  first_date <- min(as.Date(diel_data$Date))
  last_date <- max(as.Date(diel_data$Date))
  middle_date <- as.Date(first_date + (last_date - first_date) / 2)

  # Log the dates
  message("First date: ", first_date)
  message("Last date: ", last_date)
  message("Middle date: ", middle_date)

  return(list(first_date = first_date, last_date = last_date, middle_date = middle_date))
}

calc_sunrise_sunset <- function(date_info, lat, lon) {
  # Calculate sunrise and sunset for the middle date
  sun_times <- compute_sun_times(data.frame(Date = date_info$middle_date), lat, lon)
  sunrise_time <- as.POSIXct(sun_times$sunrise[1]) + 3600
  sunset_time  <- as.POSIXct(sun_times$sunset[1]) + 3600

  # Log the sunrise and sunset times
  message("Sunrise time (+1h): ", sunrise_time)
  message("Sunset time (+1h): ", sunset_time)

  return(list(sunrise_time = sunrise_time, sunset_time = sunset_time))
}

calc_all_sun_times <- function(diel_data, lat, lon) {
  # Get all unique dates from the data
  all_dates <- sort(unique(as.Date(diel_data$Date)))
  # Compute sunrise and sunset times for all dates on the plot
  all_sun_times <- compute_sun_times(data.frame(Date = all_dates), lat, lon)
  all_sunrise <- as.POSIXct(all_sun_times$sunrise) + 3600
  all_sunset <- as.POSIXct(all_sun_times$sunset) + 3600
  
  # Extract time-of-day as "HH:MM" for both sunrise and sunset
  all_sunrise_times <- format(all_sunrise, "%H:%M")
  all_sunset_times <- format(all_sunset, "%H:%M")
  
  min_sunrise <- min(all_sunrise_times)
  max_sunrise <- max(all_sunrise_times)
  min_sunset <- min(all_sunset_times)
  max_sunset <- max(all_sunset_times)
  
  message("Earliest sunrise (+1h): ", min_sunrise)
  message("Latest sunrise (+1h): ", max_sunrise)
  message("Earliest sunset (+1h): ", min_sunset)
  message("Latest sunset (+1h): ", max_sunset)

  return(list(all_sun_times = all_sun_times, 
              min_sunrise = min_sunrise, max_sunrise = max_sunrise,
              min_sunset = min_sunset, max_sunset = max_sunset))
}

render_sunrise_sunset <- function(diel_data, aggregated_data, lat, lon, p) {
  # Get the first and last date in the data
  date_info <- get_min_max_dates(diel_data)
  # Calculate sunrise and sunset for the middle date
  if (!is.null(lat) && !is.null(lon)) {
    sunrise_sunset_info <- calc_sunrise_sunset(date_info, lat, lon)
    all_sun_times_info <- calc_all_sun_times(diel_data, lat, lon)
  } else {
    message("Latitude or longitude not provided. Cannot compute sunrise and sunset times.")
  }

  # Add vertical lines for sunrise and sunset if lat and lon are provided
  if (!is.null(lat) && !is.null(lon)) {
    # Format sunrise and sunset times to match the TimeOfDay format
    sunrise_time_formatted <- format(as.POSIXct(sunrise_sunset_info$sunrise), "%H:%M")
    sunset_time_formatted <- format(as.POSIXct(sunrise_sunset_info$sunset), "%H:%M")
    # Log the formatted times and factor levels for debugging
    message("Formatted sunrise time: ", sunrise_time_formatted)
    message("Formatted sunset time: ", sunset_time_formatted)
    message("Sample factor levels: ", head(levels(aggregated_data$TimeOfDay), 10))

    # Find and log the index of sunrise and sunset in factor levels
    idx_sunrise <- which(levels(aggregated_data$TimeOfDay) == sunrise_time_formatted)
    idx_sunset  <- which(levels(aggregated_data$TimeOfDay) == sunset_time_formatted)
    message("Index of sunrise (", sunrise_time_formatted, "): ", idx_sunrise)
    message("Index of sunset (", sunset_time_formatted, "): ", idx_sunset)

    # Add vertical lines at exact sunrise and sunset indices (numeric x)
    p <- p +
      geom_vline(xintercept = idx_sunrise, color = "#FDB813", linewidth = 0.5) +
      geom_vline(xintercept = idx_sunset, color = "#FDB813", linewidth = 0.5)

    # Draw uncertainty band for sunrise
    idx_earliest_sunrise <- which(levels(aggregated_data$TimeOfDay) == all_sun_times_info$min_sunrise)  # nolint: line_length_linter.
    idx_latest_sunrise   <- which(levels(aggregated_data$TimeOfDay) == all_sun_times_info$max_sunrise)  # nolint: line_length_linter.
    message("Index of earliest sunrise (", all_sun_times_info$min_sunrise, "): ", idx_earliest_sunrise)  # nolint: line_length_linter.
    message("Index of latest sunrise (", all_sun_times_info$max_sunrise, "): ", idx_latest_sunrise)
    y_max <- max(aggregated_data$MinutesAboveThreshold, na.rm = TRUE)

    p <- p +
      geom_rect(
        aes(
          xmin = idx_earliest_sunrise,
          xmax = idx_latest_sunrise,
          ymin = 0,
          ymax = y_max
        ),
        fill = "#FDB813",
        alpha = 0.33,
        inherit.aes = FALSE  # Prevents inheriting global aesthetics
      )
    
    # Draw uncertainty band for sunset
    idx_earliest_sunset <- which(levels(aggregated_data$TimeOfDay) == all_sun_times_info$min_sunset)  # nolint: line_length_linter.
    idx_latest_sunset   <- which(levels(aggregated_data$TimeOfDay) == all_sun_times_info$max_sunset)  # nolint: line_length_linter.
    message("Index of earliest sunset (", all_sun_times_info$min_sunset, ": ", idx_earliest_sunset)  # nolint: line_length_linter.
    message("Index of latest sunset (", all_sun_times_info$max_sunset, ": ", idx_latest_sunset)
    
    p <- p +
      geom_rect(
        aes(
          xmin = idx_earliest_sunset,
          xmax = idx_latest_sunset,
          ymin = 0,
          ymax = y_max
        ),
        fill = "#FDB813",
        alpha = 0.33,
        inherit.aes = FALSE  # Prevents inheriting global aesthetics
      ) +
      # Remove padding at the top and bottom of the plot
      scale_y_continuous(limits = c(0, y_max), expand = c(0, 0))
  }
}


# Function to create a diel acoustic activity plot (returns plotly object)
# Parameters:
#   heatmap_result (data.frame): Raw heatmap data with columns "Time" and dates as columns.
#   threshold_val (numeric): Threshold value for filtering data.
#   year (numeric): The year for which the diel acoustic activity is being generated.
#   site_name (character, optional): Name of the site.
#   model_name (character, optional): Name of the model.
#   species_name (character, optional): Name of the species.
# Returns:
#   plotly object: An interactive diel acoustic activity plot.
render_diel_acoustic_activity_plot <- function(
  heatmap_result, threshold_val, year,
  site_name = NULL, model_name = NULL, species_name = NULL, lat = NULL, lon = NULL
) {
  if (is.null(heatmap_result)) {
    return(plotly::plot_ly() %>%
      plotly::layout(
        title = "Loading data...",
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE)
      ))
  }

  # Prepare data
  heatmap_long <- prepare_heatmap_data(heatmap_result, threshold_val, year)

  # Filter out values that are below threshold (marked as -1)
  diel_data <- heatmap_long[heatmap_long$Value >= 0, ]

  if (nrow(diel_data) == 0) {
    return(plotly::plot_ly() %>%
      plotly::layout(
        title = "No data available for diel acoustic activity",
        xaxis = list(title = "Time of Day"),
        yaxis = list(title = "Minutes Above Threshold")
      ))
  }

  # Extract time of day from the Time column
  diel_data$TimeOfDay <- format(as.POSIXct(diel_data$Time, format = "%H:%M:%S"), "%H:%M")

  # Aggregate data by time of day, considering 30 minutes before and after each hour
  aggregated_data <- data.frame(TimeOfDay = character(), MinutesAboveThreshold = numeric(), stringsAsFactors = FALSE)
  for (hour in 0:23) {
    start_time <- sprintf("%02d:30", ifelse(hour == 0, 23, hour - 1))
    end_time <- sprintf("%02d:30", hour)

    # Filter data for the current hour window
    subset_data <- diel_data[
      (diel_data$TimeOfDay >= start_time & diel_data$TimeOfDay < end_time) |
        (diel_data$TimeOfDay >= sprintf("%02d:00", hour) & diel_data$TimeOfDay < sprintf("%02d:00", ifelse(hour == 23, 0, hour + 1))),
      ]

    # Calculate the sum of minutes above threshold for this window
    minutes_above_threshold <- sum(subset_data$Value >= threshold_val, na.rm = TRUE)

    # Add to aggregated data
    aggregated_data <- rbind(aggregated_data,
                            data.frame(TimeOfDay = sprintf("%02d:00", hour),
                                      MinutesAboveThreshold = minutes_above_threshold,
                                      stringsAsFactors = FALSE))
  }

  # Convert TimeOfDay to a factor with ordered levels to ensure proper plotting
  # Include all possible times (every minute) to allow exact sunrise/sunset lines
  all_times <- sprintf("%02d:%02d", rep(0:23, each = 60), rep(0:59, 24))
  aggregated_data$TimeOfDay <- factor(aggregated_data$TimeOfDay, levels = all_times)
  aggregated_data$TimeIdx <- as.numeric(aggregated_data$TimeOfDay)

  # Create base plot using ggplot2
  p <- ggplot(aggregated_data, aes(x = TimeIdx, y = MinutesAboveThreshold, group = 1)) +
    geom_line() +
    geom_point(aes(text = TimeOfDay)) +
    labs(
      x = "Time of Day",
      y = "Minutes Above Threshold",
      title = paste("Diel Acoustic Activity")
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 11),
      plot.title = element_text(hjust = 0.5, size = 12),
      panel.border = element_rect(color = "black", fill = NA),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank()
    ) +
    scale_x_continuous(
      breaks = seq(1, length(all_times), by = 60),
      labels = all_times[seq(1, length(all_times), by = 60)],
      expand = c(0, 0)
    )

  p <- render_sunrise_sunset(diel_data, aggregated_data, lat, lon, p)

  # Convert to plotly
  plt <- plotly::ggplotly(p, tooltip = c("y", "text"))

  # Configure plotly
  plotly::config(
    plt,
    displayModeBar = FALSE,
    displaylogo = FALSE
  )
}
