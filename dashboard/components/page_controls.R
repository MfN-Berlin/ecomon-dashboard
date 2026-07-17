source("components/appearance_controls.R")

# Read default threshold from environment, fallback to 0.5
default_threshold <- as.numeric(Sys.getenv("DEFAULT_THRESHOLD", unset = "0.5"))
if (is.na(default_threshold) || default_threshold < 0.1 || default_threshold > 1.0) {
  default_threshold <- 0.5
}

# Menu for threshold controls
threshold_menu <- tags$div(
  style = "display: flex; flex-direction: column; margin-right: 1em;",
  tags$div(
    class = "dropdown",
    style = "background: #f8f9fa; border: 1px solid #dee2e6; border-radius: 0.25rem; padding: 0.5rem; margin-bottom: 0.5em;margin-top: 0em;",
    tags$button(
      class = "btn btn-link dropdown-toggle",
      type = "button",
      id = "thresholdMenuButton",
      `data-bs-toggle` = "dropdown",
      `aria-expanded` = "false",
      style = "background: none; border: none; padding: 0; color: #495057; text-decoration: none; font-size: 0.85em; display: flex; align-items: center;",
      tags$div(
        style = "display: flex; flex-direction: column; justify-content: space-between; height: 1.5em; width: 1.5em; margin-right: 0.5em;",
        tags$div(style = "height: 0.2em; background-color: #495057; border-radius: 0.1em;"),
        tags$div(style = "height: 0.2em; background-color: #495057; border-radius: 0.1em;"),
        tags$div(style = "height: 0.2em; background-color: #495057; border-radius: 0.1em;")
      ),
    ),
    tags$ul(
      class = "dropdown-menu",
      `aria-labelledby` = "thresholdMenuButton",
      uiOutput("set_preliminary_threshold_menu_item"),
      uiOutput("reset_threshold_menu_item"),
      # Menu item rendered from server so it can be enabled/disabled based on state
      uiOutput("set_final_threshold_menu_item")
    )
  )
)

# Threshold input
threshold_input <- tags$div(
  id = "threshold_container",
  style = "position: absolute; top: 1em; gap: 0.5em; display: flex; align-items: center;",
  threshold_menu,
  tags$label("Threshold", `for` = "threshold", style = "margin-top: 0em; white-space: nowrap; margin-right: 0.5em; font-size: small"),
  uiOutput("threshold_input"),
  tags$small(
    "Range: 0.01 to 1.00",
    style = "margin-top: 0em; color: #6c757d; white-space: nowrap; font-size: 0.75em; margin-right: 0.5em;" # Hint styling
  ),
  uiOutput("threshold_status")
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
    style = "display: flex; justify-content: space-between;margin-top: 0.1em;", # Flexbox layout
    # Threshold input
    threshold_input,
    settings_button,
    settings_modal
  )
)