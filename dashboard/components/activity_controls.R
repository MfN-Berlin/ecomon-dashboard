activity_controls <- tags$div(
  class = "mt-4", # Add margin-top for spacing
  id = "canvas_controls_activity_row",
  fluidRow(
    column(10, div(
      uiOutput("acoustic_activity_text")  # Placeholder for dynamic text
    )),
    column(2, div(
      class = "button-container d-flex justify-content-end",
      downloadButton(
        outputId = "download_data",  # This must match the output ID in the server logic
        label = NULL,
        icon = icon("card-heading", class = "bi bi-card-heading"),
        title = "Download data (CSV)",
        class = "btn btn-primary me-2"
      ),
#      actionButton(
#        inputId = "create_voucher",
#        label = NULL,
#        icon = icon("download", class = "bi bi-download"),
#        title = "Create voucher (CSV and audio)",
#        class = "btn btn-secondary"
#      )
    ))
  )
)
