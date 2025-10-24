source("components/appearance_controls.R")

# Threshold input
threshold_input <- tags$div(
  id = "threshold_container",
  style = "position: absolute; top: 1em; gap: 0.5em;",
  tags$label("Threshold", `for` = "threshold", style = "margin-top: -1em; white-space: nowrap; margin-right: 0.5em; font-size: small"),
  numericInput(
    inputId = "threshold",
    label = NULL,
    value = 0.5,
    min = 0.1,
    max = 1,
    step = 0.001,
    width = "7em"
  ),
  tags$small(
    "Range: 0.01 to 1.00",
    style = "margin-top: -1em; color: #6c757d; white-space: nowrap; font-size: 0.75em;" # Hint styling
  )
)

# Settings button
settings_button <- tags$div(
  style = "display: flex; align-items: center; margin-left: auto;", # Push cog button to the right
  actionButton(
    inputId = "settings_btn",
    label = NULL,
    icon = icon("cog"),
    class = "btn btn-outline-secondary",
    style = "border: none; background: none; color: #6c757d; font-size: 1.2em;",
    title = "Settings",
    `data-bs-toggle` = "modal",
    `data-bs-target` = "#settingsModal"
  )
)

# Settings modal dialog
settings_modal <- tags$div(
  class = "modal fade",
  id = "settingsModal",
  tabindex = "-1",
  `aria-labelledby` = "settingsModalLabel",
  `aria-hidden` = "true",
  tags$div(
    class = "modal-dialog",
    tags$div(
      class = "modal-content",
      tags$div(
        class = "modal-header",
        tags$h5(class = "modal-title", id = "settingsModalLabel", "Settings"),
        tags$button(
          type = "button",
          class = "btn-close",
          `data-bs-dismiss` = "modal",
          `aria-label` = "Close"
        )
      ),
      tags$div(
        class = "modal-body",
        tabsetPanel(
          id = "settings_tabs", # ID for the tab panel
          type = "tabs",        # Use tabs style
          tabPanel(
            title = "Heatmap",  # Tab title
            appearance_controls,
            palette
          )
        )
      ),
      tags$div(
        class = "modal-footer",
        # Add a row with Close and Reset buttons
        tags$div(
          style = "display: flex; justify-content: flex-end; width: 100%;",
          tags$button(
            type = "button",
            class = "btn btn-secondary",
            `data-bs-dismiss` = "modal",
            "Close"
          ),
        )
      )
    )
  )
)

# Add horizontal bar above the main panel
page_controls <- tagList(
  tags$div(
    class = "card shadow rounded p-3 mb-4", # Styling for the container
    style = "display: flex; justify-content: space-between;", # Flexbox layout
    # Threshold input
    threshold_input,
    settings_button,
    settings_modal
  )
)