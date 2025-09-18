source("components/canvas_controls.R")
source("components/appearance_controls.R")
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
                  plotOutput("heatmap", height = "440px")
                ),
                # heatmap options bar
                tags$div(
                  class = "options-bar d-flex align-items-center justify-content-between mb-3",
                  appearance_controls,
                  palette
                )
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