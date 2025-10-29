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
    # Dynamic main panels - one for each site
    uiOutput("site_panels")
  )
)