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
  "Viridis" = "viridis",
  "Plasma" = "plasma",
  "Cividis" = "cividis",
  "Inferno" = "inferno",
  "Magma" = "magma",
  "Grey" = "grey"
)

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
            class = "btn btn-outline-secondary sun-toggle-btn"
          ),
          actionButton(
            inputId = "twilight_toggle",
            label = tagList(
              tags$span(
                class = "bi bi-cloud-sun twilight-toggle-icon"
              ),
              "Twilight"
            ),
            class = "btn btn-outline-secondary twilight-toggle-btn"
          ),
          actionButton(
            inputId = "moonphase_toggle",
            label = tagList(
              tags$span(
                class = "bi bi-moon-stars moonphase-toggle-icon"
              ),
              "Moonphase"
            ),
            class = "btn btn-outline-secondary moonphase-toggle-btn"
          ),
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
    id = "colormapMenuButton",
    `data-bs-toggle` = "dropdown",
    `aria-expanded` = "false",
    tags$span(
      class = "bi bi-palette"
    ),
    " Color Map"
  ),
  tags$ul(
    class = "dropdown-menu",
    style = "font-size: 1em;",
    `aria-labelledby` = "colormapMenuButton",
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
  tags$input(id = "colormap", type = "hidden", value = "plasma")
)