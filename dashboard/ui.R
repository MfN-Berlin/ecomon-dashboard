# -----------------------------------------------------------------------------
# ui.R
# This file defines the user interface (UI) for the Shiny application. It
# organizes the layout, components, and visual elements of the app, including
# heatmaps, moon timelines, and activity controls.
#
# Key Components:
# 1. **Bootstrap Integration**:
#    - Includes Bootstrap Icons and custom CSS/JS for styling and interactivity.
#    - Uses the `bslib` package for Bootstrap 5 theming.
# 2. **Main Layout**:
#    - A fluid page layout with a container for the main panel.
#    - Includes tabs for heatmaps, histograms, and acoustic activity.
# 3. **Custom Components**:
#    - `canvas_controls`: Provides controls for filtering and interacting with
#      the heatmap.
#    - `appearance_controls`: Allows users to customize the heatmap's appearance.
#    - `activity_controls`: Displays activity-related data and download options.
#
# Purpose:
# - This file ties together the visual components of the app, ensuring a
#   responsive and user-friendly interface.
# -----------------------------------------------------------------------------

source("components/page_controls.R")
source("components/canvas_controls.R")
source("components/activity_controls.R")

ui <- fluidPage(
  # Add Bootstrap Icons CDN for bi-palette icon support
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$link(
      rel = "stylesheet",
      href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.1/font/bootstrap-icons.css"
    ),
    tags$script(src = "custom.js")
  ),
  # Use Bootstrap 5 theming via bslib
  theme = bs_theme(),

  # Main content without sidebar
  div(
    class = "container-fluid",
    page_controls,
    div(
      class = "main-panel",
      tags$div(
        class = "card shadow rounded p-3 mb-4", # Card styling
        canvas_controls,
        tabsetPanel(
          # First tab: Heatmap
          tabPanel(
            title = "Heatmap",
            tags$div(
              class = "tab-pane-content",
              # Wrapper div for both plots to maintain alignment
              tags$div(
                class = "plot-container",
                # plot moon
                tags$div(
                  class = "moon-timeline-container",
                  plotOutput("moon_timeline", height = "30px", width = "810px")
                ),
                # plot heatmap
                tags$div(
                  class = "heatmap-plot",
                  plotlyOutput(
                    "heatmap",
                    width = "900px",
                    height = "400px"
                  )
                ),
              )
            )
          ),
          # Second tab: Histogram
          tabPanel(
            title = "Histogram",
            tags$div(
              class = "tab-pane-content",
              tags$div(
                class = "plot-container",
                tags$h3("To do"),
                # plot events
                tags$div(
                  class = "hist-plot",
                  plotOutput("hist", height = "410px")
                )
              )
            )
          ),
          # Third tab: Acoustic activity
          tabPanel(
            title = "Acoustic activity",
            tags$div(
              class = "tab-pane-content",
              tags$div(
                class = "plot-container",
                tags$h3("To do"),
                # plot events
                tags$div(
                  class = "events-plot",
                  plotOutput("events", height = "410px")
                )
              )
            )
          )
        ),
        # No. of minutes, Download, Voucher buttons
        activity_controls
      )
    )
  )
)