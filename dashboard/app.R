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

# Sites list UI component
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

# Helper function for the download handler
register_download_handler <- function(
    output, species_id, species_name, model_id, site_id, site_name, year, threshold) {

  sanitize = function(filename) {
    # Sanitize the filename to remove any unwanted characters
    gsub("[^a-zA-Z0-9_-]", "_", filename)
  }

  output$download_data <- downloadHandler(
    filename = function() {
      cat("Generating filename for download...\n")
      filename <- sprintf("%s_%s_%s", site_name, year, species_name)
      sanitized_filename <- sanitize(filename)
      paste0(sanitized_filename, ".csv")
    },
    content = function(file) {
      cat("Fetching download data...\n")
      download_data <- get_download_data(
        species_id = species_id,
        model_id = model_id,
        site_id = site_id,
        year = year,
        threshold = threshold
      )

      if (is.null(download_data)) {
        cat("No data returned by get_download_data.\n")
        stop("No data available for download.")
      }

      cat("Writing data to CSV file...\n")
      write.csv(download_data, file, row.names = TRUE)
      cat("Download data written to file:", file, "\n")
    }
  )
}

# Main server function
server <- function(input, output, session) {
  # Add reactive values
  url_threshold <- reactiveVal(NULL)
  url_site_ids <- reactiveVal(NULL)
  url_site_name <- reactiveVal(NULL)
  url_species_id <- reactiveVal(NULL)
  url_species_name <- reactiveVal(NULL)
  url_model_id <- reactiveVal(NULL)
  url_lat <- reactiveVal(NULL)
  url_lon <- reactiveVal(NULL)
  url_year <- reactiveVal(NULL)

  # Parse URL query parameters
  observe({
    parse_url_parameters(
      session,
      url_threshold, url_site_ids, url_site_name, url_species_id, url_species_name,
      url_model_id, url_lat, url_lon, url_year
    )
  })

  # Register the download handler
  observe({
    if (!is.null(url_species_id()) && !is.null(url_model_id()) &&
          !is.null(url_site_ids()) && !is.null(url_year())) {
      register_download_handler(
        output, url_species_id(), url_species_name(), url_model_id(),
        url_site_ids()[1],  # Use the first site from the list
        url_site_name(),
        url_year(), url_threshold()
      )
    } else {
      cat("Parameters are not set. Download handler not registered.\n")
    }
  })

  # Load and process data when app starts
  heatmap_data <- reactive({
    # Check if all required parameters are available
    if (is.null(url_species_id()) || is.null(url_model_id()) ||
          is.null(url_site_ids()) || is.null(url_year()) ||
          is.null(url_lat()) || is.null(url_lon())) {
      return(NULL)  # Return NULL instead of triggering req() which would show error
    }

    tryCatch({
      load_and_process_data(  # defined in utils/data_processing.R
        species_id = url_species_id(),
        model_id = url_model_id(),     # Use the numeric model ID from URL
        site_id = url_site_ids()[1],  # Use first site from URL
        year = url_year(),
        threshold = url_threshold()
      )
    }, error = function(e) {
      print(paste("Error in load_and_process_data:", e$message))
      return(NULL)  # Return NULL on error instead of stopping
    })
  })

  # Heatmap
  observe({
    sun_toggle <- !is.null(input$sun_toggle) && input$sun_toggle %% 2 == 1
    twilight_toggle <- !is.null(input$twilight_toggle) && input$twilight_toggle %% 2 == 1

    # Call the render_heatmap function with the evaluated toggles
    render_heatmap(  # defined in components/heatmap.R
      input, output, session, heatmap_data, url_year, url_threshold,
      sun_toggle, twilight_toggle, url_lat(), url_lon()
    )
  })

  # Minutes with acoustic activity
  output$acoustic_activity_text <- renderUI({
    # Get the threshold value
    threshold <- url_threshold()

    # Get count directly from database using the new function
    count_above_threshold <- get_count_above_threshold(
      species_id = url_species_id(),
      model_id = url_model_id(),
      site_id = url_site_ids()[1],
      year = url_year(),
      threshold = threshold
    )

    cat(count_above_threshold, "minutes with acoustic activity above threshold", threshold, "\n")
    tags$span(paste("No. of minutes with acoustic activity:", count_above_threshold))
  })

  # Moon timeline plot: only show if moonphase_toggle is ON
  observe({
    moon_toggle <- !is.null(input$moonphase_toggle) && input$moonphase_toggle %% 2 == 1
    # defined in components/moon.R
    render_moon_timeline(input, output, session, url_year, moon_toggle)
  })
}

shinyApp(ui = ui, server = server)
