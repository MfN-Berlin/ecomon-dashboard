# -----------------------------------------------------------------------------
# activity_controls.R
# This file defines the UI components for controlling and displaying activity
# information in the dashboard. It includes:
#
# 1. A dynamic text output (`acoustic_activity_text`) for displaying information
#    about acoustic activity.
#
# 2. A download button for exporting data in CSV format.
#    - The button includes an icon and tooltip for better user experience.
#
# These controls allow users to view and download activity-related data
# directly from the dashboard.
# -----------------------------------------------------------------------------

# Function to create activity controls for a specific site
create_activity_controls <- function(site_id) {
  tags$div(
    class = "d-flex justify-content-between align-items-center mt-3",
    uiOutput(paste0("acoustic_activity_text_", site_id)),
    tags$div(
      class = "d-flex",
      downloadButton(
        paste0("download_", site_id),
        label = NULL,
        icon = icon("download", class = "bi bi-download"),
        title = "Download data (CSV)",
        class = "btn btn-primary me-2"
      ),
      actionButton(
        paste0("download_image_", site_id),
        label = NULL,
        icon = icon("image", class = "bi bi-image"),
        title = "Download publication-quality image",
        class = "btn btn-outline-secondary"
      )
    )
  )
}