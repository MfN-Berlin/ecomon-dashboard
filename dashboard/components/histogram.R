# Function to create a histogram plot (returns plotly object)
# Parameters:
#   heatmap_result (data.frame): Raw heatmap data with columns "Time" and dates as columns.
#   threshold_val (numeric): Threshold value for filtering data.
#   year (numeric): The year for which the histogram is being generated.
#   site_name (character, optional): Name of the site.
#   model_name (character, optional): Name of the model.
#   species_name (character, optional): Name of the species.
#   bin_size (numeric, optional): Size of bins for the histogram.
#   distribution_type (character, optional): Type of distribution - "all" or "activity".
# Returns:
#   plotly object: An interactive histogram.
render_histogram_plot <- function(
  heatmap_result, threshold_val, year,
  site_name = NULL, model_name = NULL, species_name = NULL, bin_size = 0.01, distribution_type = "activity"
) {
  if (is.null(heatmap_result)) {
    return(plotly::plot_ly() %>%
      plotly::layout(
        title = "Loading data...",
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE)
      ))
  }
  # Log the distribution type
  message("Distribution type: ", distribution_type)

  # Prepare data
  heatmap_long <- prepare_heatmap_data(heatmap_result, threshold_val, year)

  # Filter out values that are below threshold (marked as -1)
  histogram_data <- heatmap_long$Value[heatmap_long$Value >= 0]

  if (length(histogram_data) == 0) {
    return(plotly::plot_ly() %>%
      plotly::layout(
        title = "No data available for histogram",
        xaxis = list(title = "Confidence"),
        yaxis = list(title = "Frequency")
      ))
  }

  # Create base histogram using ggplot2
  p <- ggplot(data.frame(Value = histogram_data), aes(x = Value)) +
    geom_histogram(
      bins = 30,
      binwidth = bin_size,
      fill = "#1f77b4",
      # color = "black",
      alpha = 0.7
    ) +
    labs(
      x = "Confidence",
      y = "Frequency"
    ) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 11),
      plot.title = element_text(hjust = 0.5, size = 12),
      panel.border = element_rect(color = "black", fill = NA),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank()
    )

  # Convert to plotly
  plt <- plotly::ggplotly(p, tooltip = "y")

  # Configure plotly
  plotly::config(
    plt,
    displayModeBar = FALSE,
    displaylogo = FALSE
  )
}