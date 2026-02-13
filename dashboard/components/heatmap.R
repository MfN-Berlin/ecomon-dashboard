# heatmap.R
# This file contains helper functions for preparing and visualizing heatmap data in a Shiny application.
# Functions include data preparation, axis scaling, color scale addition, and plotting enhancements.

# Helper function to prepare data for the heatmap
# Parameters:
#   heatmap_data (data.frame): Input data with columns "Time" and dates as columns.
#   threshold (numeric): Threshold value; values below this will be set to -1.
#   year (numeric): The year for which the heatmap is being generated.
# Returns:
#   data.frame: A long-format data frame with columns "Time", "Date", and "Value".
prepare_heatmap_data <- function(heatmap_data, threshold, year) {
  # Ensure year is treated as a number
  year <- as.numeric(year)

  # Convert heatmap_data to long format for ggplot using melt
  heatmap_long <- melt(
    heatmap_data,
    id.vars = "Time",
    variable.name = "Date",
    value.name = "Value"
  )
  heatmap_long$Date <- as.Date(as.character(heatmap_long$Date))

  # Create a complete grid of all possible times and dates
  all_times <- sprintf(
    "%02d:%02d:00",
    rep(0:23, each = 60),
    rep(0:59, 24)
  )

  # Create sequence of all dates in the year
  all_dates <- seq.Date(
    from = as.Date(sprintf("%d-01-01", year)),
    to = as.Date(sprintf("%d-12-31", year)),
    by = "day"
  )

  # Apply threshold - values below threshold become -1 (background color)
  heatmap_long$Value[!is.na(heatmap_long$Value) & heatmap_long$Value < threshold] <- -1

  # Remove rows where Value is NA (no actual data)
  heatmap_long <- heatmap_long[!is.na(heatmap_long$Value), ]

  # Ensure Time is properly ordered
  heatmap_long$Time <- factor(heatmap_long$Time, levels = all_times)

  # Store the full ranges as attributes for axis scaling
  attr(heatmap_long, "all_times") <- all_times
  attr(heatmap_long, "all_dates") <- all_dates

  heatmap_long
}

# Helper function to add a color scale to the heatmap
# Parameters:
#   plot (ggplot object): The ggplot object to which the color scale will be added.
#   colormap (character): The name of the colormap to use (e.g., "viridis", "plasma").
#   threshold (numeric): The threshold value to highlight on the color scale.
# Returns:
#   ggplot object: The ggplot object with the color scale added.
# Helper function to add color scale
add_color_scale <- function(plot, colormap, threshold) {
  message("Adding color scale with colormap: ", colormap, " and threshold: ", threshold)
  # Ensure threshold is numeric and has a valid value
  if (is.null(threshold) || length(threshold) == 0) {
    threshold <- 0.5
    warning("Using default threshold of 0.5 because provided threshold was NULL or empty")
  }

  # Convert to numeric if it's not already
  threshold <- as.numeric(threshold)

  # Regular breaks
  regular_breaks <- seq(0, 1, by = 0.1)
  # All breaks: regular + threshold
  breaks <- sort(unique(c(regular_breaks, threshold)))
  # Labels: arrow for threshold, value for regular breaks
  labels <- sapply(
    breaks,
    function(x) {
      if (abs(x - threshold) < 1e-8) {
        if (any(abs(x - regular_breaks) < 1e-8)) {
          paste0("\u25C0 ", sprintf("%.2f", x))  # Arrow and value
        } else {
          "\u25C0"  # Only arrow
        }
      } else {
        paste0("\u2007 ", sprintf("%.2f", x))  # Figure space for alignment
      }
    }
  )

  # Set the height of the color scale
  barheight <- unit(500, "pt")  # Fixed height for the color scale

  if (colormap == "grey") {
    plot + scale_fill_gradient(
      low = "white",
      high = "black",
      name = "Confidence",
      na.value = "transparent",
      limits = c(0, 1),
      oob = scales::squish,  # Use squish directly without custom function
      breaks = breaks,
      labels = labels,
      guide = guide_colorbar(
        barheight = barheight,       # Set the height of the color scale
        barwidth = unit(8, "pt"),   # Fixed width
        label.hjust = 0,             # Align labels to left
        label.vjust = 0.5,           # Center labels vertically
        title.position = "top",      # Position title at top
        title.hjust = 0.5,           # Center title
        frame.colour = "black",      # Add border around legend
        ticks.colour = "black"       # Add tick marks
      )
    ) + theme(
      legend.key.height = barheight  # Match the height of the labels to the color scale
    )
  } else {
    # Check if colormap is valid, default if not
    valid_colormaps <- c("rdbu", "plasma", "turbo", "viridis", "magma", "inferno", "cividis")
    if (!(colormap %in% valid_colormaps)) {
      warning(paste("Invalid colormap:", colormap, "- using 'RdBu' instead"))
      colormap <- "rdbu"
    }
    if (colormap == "rdbu") {
      message("Using RdBu colormap")
      plot + scale_fill_gradientn(
        name = "Confidence",
        colors = rdbu,
        na.value = "transparent",
        limits = c(0, 1),
        oob = scales::squish,  # Use squish directly without custom function
        breaks = breaks,
        labels = labels,
        guide = guide_colorbar(
          barheight = barheight,       # Set the height of the color scale
          barwidth = unit(8, "pt"),   # Fixed width
          label.hjust = 0,             # Align labels to left
          label.vjust = 0.5,           # Center labels vertically
          title.position = "top",      # Position title at top
          title.hjust = 0.5,           # Center title
          frame.colour = "black",      # Add border around legend
          ticks.colour = "black"       # Add tick marks
        )
      ) + theme(
        legend.key.height = barheight  # Match the height of the labels to the color scale
      )
    } else {
      plot + scale_fill_viridis_c(
        name = "Confidence",
        option = colormap,
        na.value = "transparent",
        limits = c(0, 1),
        oob = scales::squish,  # Use squish directly without custom function
        breaks = breaks,
        labels = labels,
        guide = guide_colorbar(
          barheight = barheight,       # Set the height of the color scale
          barwidth = unit(8, "pt"),   # Fixed width
          label.hjust = 0,             # Align labels to left
          label.vjust = 0.5,           # Center labels vertically
          title.position = "top",      # Position title at top
          title.hjust = 0.5,           # Center title
          frame.colour = "black",      # Add border around legend
          ticks.colour = "black"       # Add tick marks
        )
      ) + theme(
        legend.key.height = barheight  # Match the height of the labels to the color scale
      )
    }
  }
}

# Helper function to create axis scales for the heatmap
# Parameters:
#   year (numeric): The year for which the heatmap is being generated.
#   heatmap_long (data.frame, optional): The long-format heatmap data.
# Returns:
#   list: A list containing x and y axis scales for ggplot.
create_axis_scales <- function(year, heatmap_long = NULL) {
  # Create hours and minute sequences for a full day
  hours <- 0:23

  # Create breaks for hourly intervals (00:00, 01:00, etc.)
  hourly_breaks <- sprintf("%02d:00:00", 0:23)
  hourly_labels <- sprintf("%02d:00", 0:23)

  # Create all 10-minute intervals for the full range (but only show hourly ticks)
  minutes_seq <- seq(0, 23 * 60 + 59, by = 1)  # 0 to 1439 minutes in 1-min steps
  hours_all <- minutes_seq %/% 60
  minutes_all <- minutes_seq %% 60
  all_breaks <- sprintf("%02d:%02d:00", hours_all, minutes_all)

  # Set x-axis limits from January 1 to December 31 of the data's year
  x_limits <- as.Date(c(sprintf("%d-01-01", year), sprintf("%d-12-31", year)))

  # Get the full time range (either from attributes or generate it)
  if (!is.null(heatmap_long) && !is.null(attr(heatmap_long, "all_times"))) {
    y_limits <- attr(heatmap_long, "all_times")
  } else {
    y_limits <- all_breaks
  }

  list(
    x_scale = scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %Y",
      expand = c(0, 0),
      limits = x_limits
    ),
    y_scale = scale_y_discrete(
      breaks = hourly_breaks,     # Only show hourly ticks
      labels = hourly_labels,     # Only show hourly labels
      expand = c(0, 0),
      limits = y_limits          # Full range but only actual data plotted
    )
  )
}

# Helper function to create a custom ggplot theme
# Returns:
#   ggplot theme: A ggplot theme object for consistent styling.
create_plot_theme <- function() {
  theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(size = 8),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.2, "cm"),
      plot.title = element_text(hjust = 0.5),
      panel.border = element_rect(color = "black", fill = NA),
      legend.key.height = unit(35, "pt"),
      legend.key.width = unit(10, "pt"),
      legend.title = element_text(size = 9, margin = margin(b = 10))
    )
}

# Helper function to compute sunrise and sunset lines for the heatmap
# Parameters:
#   heatmap_long (data.frame): The long-format heatmap data.
#   lat (numeric): Latitude of the location.
#   lon (numeric): Longitude of the location.
# Returns:
#   data.frame: A data frame with sunrise and sunset times for each date.
get_sunrise_sunset_lines <- function(heatmap_long, lat, lon) {
  sun_times <- compute_sun_times(heatmap_long, lat, lon)
  sunrise <- floor_time_to_1min(sun_times$sunrise)
  sunset  <- floor_time_to_1min(sun_times$sunset)
  unique_dates <- unique(heatmap_long$Date)
  data.frame(
    Date = rep(unique_dates, 2),
    Time = c(sunrise, sunset),
    Type = rep(c("Sunrise", "Sunset"), each = length(unique_dates))
  )
}

# Helper function to compute dawn and dusk lines for the heatmap
# Parameters:
#   heatmap_long (data.frame): The long-format heatmap data.
#   lat (numeric): Latitude of the location.
#   lon (numeric): Longitude of the location.
# Returns:
#   data.frame: A data frame with dawn and dusk times for each date.
get_dawn_dusk_lines <- function(heatmap_long, lat, lon, twilight_type) {
  sun_times <- compute_sun_times(heatmap_long, lat, lon)
  message(paste("Computing dawn/dusk lines with twilight type:", twilight_type))
  if (twilight_type == "civil") {
    dawn <- sun_times$dawn
    dusk <- sun_times$dusk
  } else if (twilight_type == "nautical") {
    dawn <- sun_times$nauticalDawn
    dusk <- sun_times$nauticalDusk
  } else if (twilight_type == "astronomical") {
    dawn <- sun_times$nightEnd
    dusk <- sun_times$night
  }
  dawn <- floor_time_to_1min(dawn)
  dusk <- floor_time_to_1min(dusk)
  unique_dates <- unique(heatmap_long$Date)
  message(head(data.frame(Date = rep(unique_dates, 2),
             Time = c(dawn, dusk),
             Type = rep(c("Dawn", "Dusk"), each = length(unique_dates))), 3))
  data.frame(
    Date = rep(unique_dates, 2),
    Time = c(dawn, dusk),
    Type = rep(c("Dawn", "Dusk"), each = length(unique_dates))
  )
}

# Function to create a single heatmap plot (returns plotly object)
render_heatmap_plot <- function(
  heatmap_result, year, threshold_val, sun_toggle, twilight_toggle,
  lat, lon, site_name = NULL, model_name = NULL, species_name = NULL, colormap = "rdbu", twilight_type = "civil"
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

  # tooltip text
  heatmap_long$text <- paste0(
    "Date: ", heatmap_long$Date, "\n",
    "Time: ", heatmap_long$Time, "\n",
    "Value: ", ifelse(heatmap_long$Value == -1, 0, heatmap_long$Value)
  )

  # Create base plot
  p <- ggplot(heatmap_long, aes(x = Date, y = Time, fill = Value, text = text)) +
    geom_raster(interpolate = FALSE)

  # Add sunrise/sunset lines if sun_toggle is ON
  if (sun_toggle) {
    sun_df <- get_sunrise_sunset_lines(heatmap_long, lat, lon)
    p <- p +
      geom_line(
        data = sun_df,
        aes(x = Date, y = Time, group = Type),
        color = "#FDB813",
        size = 0.5,
        inherit.aes = FALSE,
        show.legend = FALSE
      )
  }

  # Add dawn/dusk lines if twilight_toggle is ON
  if (twilight_toggle) {
    if (is.null(twilight_type) || twilight_type == "") twilight_type <- "civil"
    twilight_df <- get_dawn_dusk_lines(heatmap_long, lat, lon, twilight_type)
    p <- p +
      geom_line(
        data = twilight_df,
        aes(x = Date, y = Time, group = Type),
        color = "#4FC3F7", # light blue
        size = 0.5,
        inherit.aes = FALSE,
        show.legend = FALSE
      )
  }

  # Add color scale
  p <- add_color_scale(p, colormap, threshold_val)

  # Add scales
  scales <- create_axis_scales(year)
  p <- p + scales$x_scale + scales$y_scale

  # Add labels and theme
  p <- p +
    labs(
      x = "Date",
      y = "CET (Winter Time)"
    ) +
    create_plot_theme()

  plt <- plotly::ggplotly(p, tooltip = "text")

  # Tweak the plotly object for better appearance
  for (i in seq_along(plt$x$data)) {
    # Make lines transparent to hover
    if (plt$x$data[[i]]$type == "scatter" && plt$x$data[[i]]$mode == "lines") {
      plt$x$data[[i]]$hoverinfo <- "skip"
    }
  }


  # DOWNLOAD SVG BUTTON
  plotly::config(
    plt,
    toImageButtonOptions = list(
      format = "svg",  # Default format for download
      filename = get_filename("HM", site_name, model_name, year, species_name, threshold_val),
      height = 400,
      width = 900,
      scale = 2  # Higher scale for better quality
    ),
    modeBarButtonsToRemove = c(
      "autoScale2d",
      "select2d", "lasso2d",
      "hoverClosestCartesian", "hoverCompareCartesian",
      "toggleSpikelines"
    ),
    # note that the toImage button is hidden using CSS, and triggered using a separate actionButton
    # defined in activity_controls.R
    modeBarButtonsToAdd = c("toImage", "zoom2d", "zoomIn2d", "zoomOut2d", "pan2d", "resetScale2d"),
    displaylogo = FALSE
  )
}

# Generate filename (without extension) based on site, model, year, species and threshold
# e.g. HM_BBNSF_1205H_2024_birdnetplus-v3_0_euna_1k_preview2_Turdus_philomelos_th0750
get_filename <- function(
  prefix = NULL,
  site_name = NULL, model_name = NULL, year = NULL, species_name = NULL, threshold = NULL
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

  message("Generated filename: ", filename)
  return(filename)
}

# Function to handle heatmap rendering (wrapper for backward compatibility)
render_heatmap <- function(
  input, output, session, heatmap_data, selected_year, threshold_reactive,
  sun_toggle, twilight_toggle, lat, lon
) {
  output$heatmap <- plotly::renderPlotly({
    # Force evaluation of heatmap_data
    heatmap_result <- heatmap_data()  # This evaluates the reactive

    colormap <- input$colormap
    if (is.null(colormap) || colormap == "") colormap <- "rdbu"

    # Get threshold with fallback
    threshold_val <- threshold_reactive()

    # Get the current year from URL/input
    year <- tryCatch({
      as.numeric(selected_year())
    }, error = function(e) {
      print("Error getting selected_year:")
      print(e)
      as.numeric(format(Sys.Date(), "%Y"))  # Default to current year
    })

    twilight_type <- input$twilight_type
    if (is.null(twilight_type) || twilight_type == "") twilight_type <- "civil"

    # Call the new render function
    render_heatmap_plot(
      heatmap_result, year, threshold_val,
      sun_toggle, twilight_toggle,
      lat, lon,
      site_name = NULL,
      model_name = NULL,
      colormap = colormap,
      twilight_type = twilight_type
    )
  })
}