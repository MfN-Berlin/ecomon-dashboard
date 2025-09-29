# -----------------------------------------------------------------------------
# appearance_controls.R
# This file defines the UI components for controlling the appearance of the
# heatmap visualization. It includes:
#
# 1. A set of toggle buttons for enabling/disabling additional visual elements:
#    - Sunset & Sunrise lines
#    - Twilight lines
#    - Moonphase visualization
#
# 2. A dropdown menu for selecting the colormap used in the heatmap.
#    - Available colormaps: Viridis, Plasma, Cividis, Inferno, Magma, Grey
#
# These controls allow users to customize the visual representation of the
# heatmap to suit their preferences or analysis needs.
# -----------------------------------------------------------------------------


# Named vector of available colormaps
colormaps <- c(
  "RdBu" = "rdbu",
  "Plasma" = "plasma",
  "Turbo" = "turbo",
  "Viridis" = "viridis",
  "Cividis" = "cividis",
  "Inferno" = "inferno",
  "Magma" = "magma",
  "Grey" = "grey"
)
rdbu <- c(
  "#0d11a0",  # Deep blue (low)
  "#2166ac",
  "#4393c3",
  "#92c5de",
  "#d1e5f0",
  "#ffffff",  # White (mid)
  "#fddbc7",
  "#f4a582",
  "#d6604d",
  "#b2182b",
  "#b30c1d"   # Deep red (high)
)

# Read default colormap from environment, fallback to "rdbu"
default_colormap <- Sys.getenv("DEFAULT_COLORMAP", unset = "rdbu")
if (!(tolower(default_colormap) %in% colormaps)) {
  default_colormap <- "rdbu"
}
# Appearance controls UI component
appearance_controls <- fluidRow(
  column(
    width = 12,
    # Row for toggle buttons (sun, twilight, moonphase) side by side
    fluidRow(
      column(
        width = 12,
        div(
          style = "display: flex; gap: 0.5em;",
          actionButton(
            inputId = "sun_toggle",
            label = tagList(
              tags$span(
                class = "bi bi-sun sun-toggle-icon"
              ),
              "Sunset & Sunrise"
            ),
            class = "btn btn-outline-secondary sun-toggle-btn",
            value = 0
          ),
#          actionButton(
#            inputId = "twilight_toggle",
#            label = tagList(
#              tags$span(
#                class = "bi bi-cloud-moon twilight-toggle-icon"
#              ),
#              "Twilight"
#            ),
#            class = "btn btn-outline-secondary twilight-toggle-btn"
#          ),
          div(
            style = "display: flex; gap: 0.5em; align-items: center;",
            actionButton(
              inputId = "twilight_toggle",
              label = tagList(
                tags$span(class = "bi bi-cloud-moon twilight-toggle-icon"),
                "Twilight"
              ),
              class = "btn btn-outline-secondary twilight-toggle-btn"
            ),
            selectInput(
              inputId = "twilight_type",
              label = NULL,
              choices = c(
                "Civil" = "civil",
                "Nautical" = "nautical",
                "Astronomical" = "astronomical"
              ),
              selected = "civil",
              width = "120px"
            )
          ),
          actionButton(
            inputId = "moonphase_toggle",
            label = tagList(
              tags$span(
                class = "bi bi-moon-stars moonphase-toggle-icon"
              ),
              "Moonphase"
            ),
            class = "btn btn-outline-secondary moonphase-toggle-btn",
            value = 1
          )
        )
      )
    )
  )
)

# Palette dropdown
palette <- tags$div(
  class = "dropdown",
  tags$button(
    class = "btn btn-light dropdown-toggle palette-dropdown-btn",
    type = "button",
    id = "colormapMenuButton2",
    `data-bs-toggle` = "dropdown",
    `aria-expanded` = "false",
    tags$span(class = "bi bi-palette"),
    "Color map"
  ),
  tags$ul(
    class = "dropdown-menu",
    style = "font-size: 1em;",
    `aria-labelledby` = "colormapMenuButton2",
    lapply(
      names(colormaps),
      function(nm) {
        tags$li(
          tags$a(
            class = "dropdown-item colormap-option",
            href = "#",
            `data-value` = colormaps[[nm]],
            nm
          )
        )
      }
    )
  ),
  tags$input(id = "colormap", type = "hidden", value = default_colormap)
)
