# -----------------------------------------------------------------------------
# app.R
# This is the main entry point for the Shiny application. It initializes the
# app by loading required libraries, sourcing external files, and defining the
# server logic. The app dynamically adjusts its state based on URL parameters
# and provides interactive visualizations, including heatmaps and moon timelines.
#
# Purpose:
# - This file ties together the UI and server logic to create an interactive
#   dashboard for visualizing and analyzing acoustic activity data.
#
# IMPORTANT note:
# Do not add a server.R file to this app, as the Docker base image used expects
# an app.R and optionally a ui.R, but will not find the app if there is an
# additional server.R. So keep the main logic of the app here in app.R.
# -----------------------------------------------------------------------------

# Load required libraries
library(shiny)
library(bslib)
library(httpuv)
library(ggplot2)
library(viridis)
library(reshape2)
library(grid)
library(png)
library(httr)
library(jsonlite)
library(ini)
library(data.table)
library(suncalc)
library(plotly)

# Source external files
source("ui.R")
source("utils/ephem.R")
source("components/heatmap.R")
source("utils/data_processing.R")
source("utils/url.R")
source("utils/hasura.R")
source("utils/download_data.R")
source("components/canvas_controls.R")
source("components/moon.R")

# -----------------------------------------------------------------------------
# Function: render_site_list_ui
# Description:
#   Dynamically renders the list of available sites based on the provided
#   site data frame and user input.
#
# Parameters:
#   - site_df (data.frame): Data frame containing site information.
#   - input (Shiny input): Shiny input object.
#
# Returns:
#   - A Shiny UI element (HTML list) for the site list.
# -----------------------------------------------------------------------------
render_site_list_ui <- function(site_df, input) {
  renderUI({
    selected <- input$selected_sites
    tags$ul(
      lapply(seq_len(nrow(site_df)), function(i) {
        if (is.null(selected) || !(site_df$id[i] %in% selected)) {
          tags$li(
            site_df$title[i],
            id = paste0("site_", site_df$id[i]),
            onclick = sprintf(
              "var sel = Shiny.shinyapp.$inputValues.selected_sites || [];
               if(sel.indexOf(%d) === -1) { sel.push(%d); }
               Shiny.setInputValue('selected_sites', sel, {priority: 'event'});",
              site_df$id[i], site_df$id[i]
            ),
            class = "site-list-item"
          )
        }
      })
    )
  })
}

# -----------------------------------------------------------------------------
# Function: render_selected_site_ui
# Description:
#   Dynamically renders the list of selected sites based on user input.
#
# Parameters:
#   - site_df (data.frame): Data frame containing site information.
#   - input (Shiny input): Shiny input object.
#
# Returns:
#   - A Shiny UI element (HTML list) for the selected site list.
# -----------------------------------------------------------------------------
render_selected_site_ui <- function(site_df, input) {
  renderUI({
    sel <- if (!is.null(input$selected_sites))
      which(site_df$id %in% input$selected_sites) else integer(0)
    tags$ul(
      lapply(sel, function(i) {
        tags$li(
          site_df$title[i],
          id = paste0("selected_site_", site_df$id[i]),
          onclick = sprintf(
            "var sel = Shiny.shinyapp.$inputValues.selected_sites || [];
             var idx = sel.indexOf(%d);
             if(idx !== -1) { sel.splice(idx,1); }
             Shiny.setInputValue('selected_sites', sel, {priority: 'event'});",
            site_df$id[i]
          ),
          class = "site-list-item"
        )
      })
    )
  })
}

# -----------------------------------------------------------------------------
# Function: register_download_handler
# Description:
#   Registers a download handler for exporting data as a CSV file. The handler
#   dynamically generates the filename and fetches the data for download.
#
# Parameters:
#   - output (Shiny output): Shiny output object.
#   - species_id, species_name, model_id, site_id, site_name, year, threshold:
#     Parameters used to fetch and filter the data for download.
#
# Returns:
#   - None. Registers the download handler directly.
# -----------------------------------------------------------------------------
register_download_handler <- function(
    output, species_id, species_name, model_id, site_id, site_name, year, threshold) {

  sanitize = function(filename) {
    # Sanitize the filename to remove any unwanted characters
    gsub("[^a-zA-Z0-9_-]", "_", filename)
  }

  output$download_data <- downloadHandler(
    filename = function() {
      message("Generating filename for download...\n")
      filename <- sprintf("%s_%s_%s", site_name, year, species_name)
      sanitized_filename <- sanitize(filename)
      paste0(sanitized_filename, ".csv")
    },
    content = function(file) {
      message("Fetching download data...\n")
      download_data <- get_download_data(
        species_id = species_id,
        model_id = model_id,
        site_id = site_id,
        year = year,
        threshold = threshold
      )

      if (is.null(download_data)) {
        message("No data returned by get_download_data.\n")
        stop("No data available for download.")
      }

      message("Writing data to CSV file...\n")
      write.csv(download_data, file, row.names = TRUE)
      message("Download data written to file:", file, "\n")
    }
  )
}

# -----------------------------------------------------------------------------
# Main Server Function
# Description:
#   Defines the server logic for the Shiny app, including reactive values,
#   observers, and rendering of UI components.
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  # Reactive values for URL parameters
  url_threshold <- reactiveVal(NULL)
  url_site_ids <- reactiveVal(NULL)
  url_species_id <- reactiveVal(NULL)
  url_model_id <- reactiveVal(NULL)
  url_year <- reactiveVal(NULL)

  # site info from Hasura
  site_info <- reactiveVal(NULL)

  # model info from Hasura
  model_info <- reactiveVal(NULL)

  # model info from Hasura
  species_info <- reactiveVal(NULL)

  # Parse URL query parameters
  observe({
    parse_url_parameters(
      session,
      url_threshold, url_site_ids, url_species_id,
      url_model_id, url_year
    )
  })

  # Fetch species information when species_id changes
  observe({
    if (!is.null(url_species_id())) {
      tryCatch({
        species_data <- get_species_info(url_species_id())
        species_info(species_data)
        # Update the UI input with the species name
        updateTextInput(session, "canvas_species", value = species_data$name)
      }, error = function(e) {
        cat("Error loading species info:", e$message, "\n")
        species_info(NULL)
      })
    }
  })

  # Fetch model information when model_id changes
  observe({
    if (!is.null(url_model_id())) {
      tryCatch({
        model_data <- get_model_info(url_model_id())
        model_info(model_data)
        # Update the UI input with the model name
        updateTextInput(session, "canvas_classifier", value = model_data$name)
      }, error = function(e) {
        cat("Error loading model info:", e$message, "\n")
        model_info(NULL)
      })
    }
  })

  # Fetch site information when site_ids change
  observe({
    if (!is.null(url_site_ids()) && length(url_site_ids()) > 0) {
      tryCatch({
        site_data <- get_site_info(url_site_ids()[1])
        site_info(site_data)
        # Update the UI input with the site name
        updateTextInput(session, "canvas_site", value = site_data$name)
      }, error = function(e) {
        message("Error loading site info:", e$message, "\n")
        site_info(NULL)
      })
    }
  })

  # Register the download handler (use site name from Hasura)
  observe({
    if (!is.null(url_species_id()) && !is.null(url_model_id()) &&
          !is.null(url_site_ids()) && !is.null(url_year()) &&
          !is.null(url_threshold()) && !is.null(site_info()) &&
          !is.null(species_info())) {
      register_download_handler(
        output, url_species_id(), species_info()$name, url_model_id(),
        url_site_ids()[1],  # Use the first site from the list
        site_info()$name,  # Use site name from Hasura
        url_year(), url_threshold()
      )
    } else {
      message("Parameters are not set. Download handler not registered.\n")
    }
  })

  # Load and process data when app starts
  heatmap_data <- reactive({
    if (is.null(url_species_id()) || is.null(url_model_id()) ||
          is.null(url_site_ids()) || is.null(url_year()) ||
          is.null(site_info())) {
      return(NULL)
    }

    tryCatch({
      load_and_process_data(
        species_id = url_species_id(),
        model_id = url_model_id(),
        site_id = url_site_ids()[1],
        year = url_year(),
        threshold = url_threshold()
      )
    }, error = function(e) {
      message(paste("Error in load_and_process_data:", e$message))
      return(NULL)
    })
  })

  # Heatmap rendering
  observe({
    if (!is.null(site_info())) {
      sun_toggle <- is.null(input$sun_toggle) || input$sun_toggle %% 2 == 1
      twilight_toggle <- !is.null(input$twilight_toggle) && input$twilight_toggle %% 2 == 1

      render_heatmap(
        input, output, session, heatmap_data, url_year, url_threshold,
        sun_toggle, twilight_toggle, site_info()$latitude, site_info()$longitude
      )
    }
  })

  # Acoustic activity text
  output$acoustic_activity_text <- renderUI({
    threshold <- url_threshold()
    count_above_threshold <- get_count_above_threshold(
      species_id = url_species_id(),
      model_id = url_model_id(),
      site_id = url_site_ids()[1],
      year = url_year(),
      threshold = threshold
    )

    tags$span(paste("No. of minutes with acoustic activity:", count_above_threshold))
  })

  # Moon timeline plot
  observe({
    if (is.null(input$moonphase_toggle)) {
      updateActionButton(session, "moonphase_toggle", value = 0)
    }
  })
  observe({
    moon_toggle <- is.null(input$moonphase_toggle) || input$moonphase_toggle %% 2 == 1
    render_moon_timeline(input, output, session, url_year, moon_toggle)
  })
  render_moon_timeline(input, output, session, url_year, TRUE)
}

# Run the Shiny app
shinyApp(ui = ui, server = server)