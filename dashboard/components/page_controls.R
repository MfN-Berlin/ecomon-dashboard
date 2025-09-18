# ------------------------------------------------------------------------------
# page_controls.R
#
# This file defines the 'page controls' UI component.
#
# Usage:
#   - Source this file in your ui.R with:
#       source("page_controls.R", local = TRUE)
#   - Make sure the colormaps vector is not redefined elsewhere in your app.
#   - Ensure the Bootstrap Icons CDN is loaded in your UI (see ui.R).
# ------------------------------------------------------------------------------

# Read the list of models from data/models.R
# make this dynamic / read from API
source("data/models.R", local = TRUE)
# Assumes data/models.R defines a variable named model_df (or similar) with your models


# UI for the two lists container of sites
site_lists_container <- tags$div(
  class = "control-container",
  tags$div(
    class = "site-lists-container",
    style = "display: flex; gap: 2em; margin-bottom: 1em;",
    # Left list
    tags$div(
      style = "flex: 1 1 0;",
      tags$label("Sites"),
      tags$div(
        class = "site-list-scroll",
        style = "height: 25em; overflow-y: auto;",
        uiOutput("site_list_ui")
      )
    ),
    # Right list
    tags$div(
      style = "flex: 1 1 0;",
      tags$label("Selected"),
      tags$div(
        class = "site-list-scroll",
        style = "height: 25em; overflow-y: auto;",
        uiOutput("selected_site_ui")
      )
    )
  )
)

# UI for the model dropdown
model_dropdown <- tags$div(
  class = "control-row",
  tags$label("Classifier", `for` = "model_select"),
  selectInput(
    inputId = "model_select",
    label = NULL,
    choices = setNames(model_df$id, model_df$title),
    selected = if (!is.null(model_df$id) && length(model_df$id) > 0) model_df$id[1] else NULL,
    width = "15em"
  )
)

# Time period selector UI function
time_period_selector <- tags$div(
  class = "control-row",
  tags$label("Year", `for` = "time_period"),
  selectInput(
    inputId = "time_period",
    label = NULL,
    choices = c("2023", "2024", "2025"),
    selected = "2025",
    width = "10em"
  )
)

# Species selector UI component
species_selector <- tags$div(
  class = "control-row",
  tags$label("Species", `for` = "species_select"),
  selectInput(
    inputId = "species_select",
    label = NULL,
    choices = c("Strix aluco"),
    selected = "Strix aluco",
    width = "15em"
  )
)

# Page controls UI component
page_controls <- tagList(
  tags$div(
    class = "control-container",
    model_dropdown,
    site_lists_container,
    time_period_selector,
    species_selector
  )
)