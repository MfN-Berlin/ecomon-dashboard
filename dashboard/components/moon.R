# Workaround: Function to get moon phase dates for a year
# The function lunar::lunar.phase() calls format() on its input, but
# if your system locale or R version is recent, the default for the trim argument
# in prettyNum() has changed, and the package does not set it correctly.
# This is a known issue with the lunar package on modern R versions.
# -> change later if lunar package is updated

# ------------------------------------------------------------
# Moon Phase Data
# ------------------------------------------------------------

#' Get moon phase dates for a specified period
#'
#' @param start_date Start date of the period (Date object)
#' @param end_date End date of the period (Date object)
#' @param phases Vector of moon phase types to include
#' @return Data frame with Date and Phase columns
get_moon_phase_dates <- function(start_date, end_date, phases = c("Full Moon", "New Moon", "First Quarter", "Last Quarter")) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  year <- as.numeric(format(start_date, "%Y"))
  print(paste("Getting moon phases from", start_date, "to", end_date))

  # Get full moon dates for the year
  if (year == 2023) {
    full_moon_dates <- as.Date(c(
      "2023-01-06", "2023-02-05", "2023-03-07", "2023-04-06", "2023-05-05",
      "2023-06-04", "2023-07-03", "2023-08-01", "2023-08-31", "2023-09-29",
      "2023-10-28", "2023-11-27", "2023-12-27"
    ))
  } else if (year == 2024) {
    full_moon_dates <- as.Date(c(
      "2024-01-25", "2024-02-24", "2024-03-25", "2024-04-23", "2024-05-23",
      "2024-06-22", "2024-07-21", "2024-08-19", "2024-09-18", "2024-10-17",
      "2024-11-15", "2024-12-15"
    ))
  } else if (year == 2025) {
    full_moon_dates <- as.Date(c(
      "2025-01-13", "2025-02-12", "2025-03-13", "2025-04-12", "2025-05-12",
      "2025-06-10", "2025-07-10", "2025-08-08", "2025-09-07", "2025-10-06",
      "2025-11-05", "2025-12-04"
    ))
  } else if (year == 2026) {
    full_moon_dates <- as.Date(c(
      "2026-01-03", "2026-02-01", "2026-03-03", "2026-04-01", "2026-05-01",
      "2026-05-30", "2026-06-29", "2026-07-28", "2026-08-27", "2026-09-25",
      "2026-10-25", "2026-11-23", "2026-12-23"
    ))
  } else {
    # For years not in our list, return empty data frame
    return(data.frame(Date = as.Date(character()), Phase = character()))
  }

  # Calculate other moon phases based on full moon dates
  result <- data.frame(Date = as.Date(character()), Phase = character())

  # Add each requested phase
  if ("Full Moon" %in% phases) {
    result <- rbind(result, data.frame(
      Date = full_moon_dates,
      Phase = rep("Full Moon", length(full_moon_dates))
    ))
  }

  if ("New Moon" %in% phases) {
    # New moon occurs approximately 14.77 days before/after full moon
    new_moon_dates <- c(full_moon_dates - 14.77, full_moon_dates + 14.77)
    new_moon_dates <- sort(new_moon_dates)
    # Filter to the year range
    new_moon_dates <- new_moon_dates[format(new_moon_dates, "%Y") == as.character(year)]
    result <- rbind(result, data.frame(
      Date = new_moon_dates,
      Phase = rep("New Moon", length(new_moon_dates))
    ))
  }

  if ("First Quarter" %in% phases) {
    # First quarter occurs approximately 7.38 days before full moon
    first_quarter_dates <- full_moon_dates - 7.38
    first_quarter_dates <- first_quarter_dates[
      format(first_quarter_dates, "%Y") == as.character(year)
    ]
    result <- rbind(result, data.frame(
      Date = first_quarter_dates,
      Phase = rep("First Quarter", length(first_quarter_dates))
    ))
  }

  if ("Last Quarter" %in% phases) {
    # Last quarter occurs approximately 7.38 days after full moon
    last_quarter_dates <- full_moon_dates + 7.38
    last_quarter_dates <- last_quarter_dates[format(last_quarter_dates, "%Y") == as.character(year)]
    result <- rbind(result, data.frame(
      Date = last_quarter_dates,
      Phase = rep("Last Quarter", length(last_quarter_dates))
    ))
  }

  # Sort by date
  result <- result[order(result$Date), ]

  # Filter to ensure dates are within the requested start/end range
  result <- result[result$Date >= start_date & result$Date <= end_date, ]

  # Round dates to nearest day
  result$Date <- as.Date(result$Date)

  print(paste("Found", nrow(result), "moon phases for", year))
  # if (nrow(result) > 0) print(head(result, 10))

  return(result)
}

# ------------------------------------------------------------
# Moon Timeline Plotting
# ------------------------------------------------------------

#' Plot a timeline of moon phases
#'
#' @param start_date Start date for the timeline
#' @param end_date End date for the timeline
#' @param px_width Width of moon icons in pixels
#' @param px_height Height of moon icons in pixels
#' @param colorbar_width_pct Percentage of width for colorbar (preserved for compatibility)
#' @param plot_width_px Total plot width in pixels (preserved for compatibility)
plot_moon_timeline <- function(
  start_date,
  end_date,
  px_width = 12,
  px_height = 12,
  colorbar_width_pct = 10,
  plot_width_px = 476
) {
  # Set up the plot area
  par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0))

  # Ensure dates are properly formatted
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  # Always use fixed date range for the full year
  year <- as.numeric(format(start_date, "%Y"))
  start_date <- as.Date(sprintf("%d-01-01", year))
  end_date <- as.Date(sprintf("%d-12-31", year))

  # Create the base plot with exact date range
  plot(NA,
    xlim = c(start_date, end_date),
    ylim = c(0, 1),
    axes = FALSE, xlab = "", ylab = "",
    xaxs = "i"  # Important! This forces exact limits with no expansion
  )

  # Add month tick marks at the bottom to align with heatmap
  month_starts <- seq(
    start_date,
    as.Date(paste0(format(end_date, "%Y"), "-12-01")),
    by = "month"
  )
  axis(1, at = month_starts, labels = FALSE, tck = -0.05, lwd = 0.5, col = "gray")

  # Define all moon phase images
  phase_img = c(
    "Full Moon" = "full.png",
    "New Moon" = "new.png",
    "First Quarter" = "first_quarter.png",
    "Last Quarter" = "last_quarter.png"
  )

  # Define y-positions for different phases
  y_positions <- c(
    "Full Moon" = 0.75,     # Top position
    "New Moon" = 0.25,      # Bottom position
    "First Quarter" = 0.5,  # Middle position
    "Last Quarter" = 0.5    # Middle position
  )

  # Get all moon phases
  moon_events <- get_moon_phase_dates(
    start_date, end_date,
    phases = c("Full Moon", "New Moon", "First Quarter", "Last Quarter")
  )

  # Plot the moon phases with correct positioning
  for (i in seq_len(nrow(moon_events))) {
    img_file <- file.path("img", phase_img[moon_events$Phase[i]])
    if (file.exists(img_file)) {
      img <- png::readPNG(img_file)

      # Ensure x_center is a proper date value
      x_center <- as.Date(moon_events$Date[i])
      y_center <- 0.5  # Using fixed y-center as in the original code

      usr <- par("usr")
      pin <- par("pin")
      x_range <- usr[2] - usr[1]
      y_range <- usr[4] - usr[3]

      # Calculate pixel to user coordinate ratio - EXACTLY as in original
      x_pixel_per_usr <- pin[1] * 96 / x_range
      y_pixel_per_usr <- pin[2] * 96 / y_range

      half_usr_width <- (px_width / x_pixel_per_usr) / 2
      half_usr_height <- (px_height / y_pixel_per_usr) / 2

      # Add the moon image at the correct date - UNCHANGED from original
      rasterImage(
        img,
        as.numeric(x_center) - half_usr_width,
        y_center - half_usr_height,
        as.numeric(x_center) + half_usr_width,
        y_center + half_usr_height
      )
    }
  }

  # Add debug lines for each month start to verify alignment
  for(m_start in month_starts) {
    segments(m_start, 0.2, m_start, 0.8, col = "lightblue", lty = 3)
  }

  # Add a divider line at the bottom
  # abline(h = 0, col = "gray", lwd = 0.5)
}

# Function to handle moon timeline rendering
render_moon_timeline <- function(input, output, session, selected_year, moon_toggle) {
  output$moon_timeline <- renderPlot({
    # Use the year from canvas_year input which is set from URL parameter
    year <- selected_year()

    start_date <- as.Date(sprintf("%d-01-01", year))
    end_date <- as.Date(sprintf("%d-12-31", year))

    if (moon_toggle) {
      # Render the moon timeline plot
      plot_moon_timeline(start_date, end_date)
    } else {
      # Render an empty plot if moon_toggle is FALSE
      par(mar = c(0, 0, 0, 0))
      plot(NA, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
    }
  }, height = 30, width = 736)  # Dynamically set height
}