# UI controls for the canvas area (classifier, threshold, site, year, species)
canvas_controls <- tagList(
  # First row: input fields for classifier, threshold, site, year, species
  fluidRow(
    id = "canvas_controls",
    class = "g-2",  # Add Bootstrap gutter class for spacing
    column(3, textInput(
      "canvas_classifier", "Classifier", value = "", width = "100%", placeholder = "Classifier"
    ) %>% tagAppendAttributes(disabled = "disabled")),
    column(1, numericInput(
      "canvas_threshold", "Threshold", value = NA, min = 0, max = 1, step = 0.001, width = "100%"
    ) %>% tagAppendAttributes(disabled = "disabled")),
    column(4, textInput(
      "canvas_site", "Site", value = "", width = "100%", placeholder = "Site"
    ) %>% tagAppendAttributes(disabled = "disabled")),
    column(1, textInput(
      "canvas_year", "Year", value = "", width = "100%", placeholder = "Year"
    ) %>% tagAppendAttributes(disabled = "disabled")),
    column(3, textInput(
      "canvas_species", "Species", value = "", width = "100%", placeholder = "Species"
    ) %>% tagAppendAttributes(disabled = "disabled"))
  )
)