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
  url_site_ids <- reactiveVal(NULL)
  url_species_id <- reactiveVal(NULL)
  url_model_id <- reactiveVal(NULL)
  url_year <- reactiveVal(NULL)
  url_threshold <- reactiveVal(NULL)

  # Reactive value for threshold
  threshold <- reactiveVal(0.5)

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
      url_site_ids, url_species_id,
      url_model_id, url_year, url_threshold
    )
  })

  # Initialize threshold from URL parameter or use default
  observe({
    if (!is.null(url_threshold())) {
      threshold(url_threshold())
      # Update the UI inputs to reflect the URL threshold
      updateNumericInput(session, "threshold", value = url_threshold())
      updateNumericInput(session, "canvas_threshold", value = url_threshold())
    }
  })

  # Debounced threshold input (waits 500ms after user stops typing)
  threshold_debounced <- debounce(reactive(input$threshold), 500)

  # Update threshold reactive value when debounced input changes
  observeEvent(threshold_debounced(), {
    if (!is.null(threshold_debounced()) && !is.na(threshold_debounced())) {
      threshold(threshold_debounced())
      # Update canvas_threshold to keep them in sync
      updateNumericInput(session, "canvas_threshold", value = threshold_debounced())
    }
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
          !is.null(site_info()) &&
          !is.null(species_info())) {
      register_download_handler(
        output, url_species_id(), species_info()$name, url_model_id(),
        url_site_ids()[1],  # Use the first site from the list
        site_info()$name,  # Use site name from Hasura
        url_year(), threshold()
      )
    } else {
      message("Parameters are not set. Download handler not registered.\n")
    }
  })

  # Load and process data for all sites when app starts
  heatmap_data_list <- reactive({
    if (is.null(url_species_id()) || is.null(url_model_id()) ||
          is.null(url_site_ids()) || is.null(url_year())) {
      return(NULL)
    }

    message("Executing heatmap_data_list")

    # Load data for each site
    site_data_list <- lapply(url_site_ids(), function(site_id) {
      tryCatch({
        site_info <- get_site_info(site_id)
        data <- load_and_process_data(
          species_id = url_species_id(),
          model_id = url_model_id(),
          site_id = site_id,
          year = url_year(),
          threshold = threshold()
        )
        list(
          site_id = site_id,
          site_info = site_info,
          data = data
        )
      }, error = function(e) {
        message(paste("Error in load_and_process_data:", e$message))
        return(NULL)
      })
    })
    # Remove NULL entries
    site_data_list[!sapply(site_data_list, is.null)]
  })

  # Render dynamic UI for multiple heatmaps
  output$site_panels <- renderUI({
    site_data_list <- heatmap_data_list()

    if (is.null(site_data_list) || length(site_data_list) == 0) {
      return(tags$div(
        class = "main-panel",
        tags$div(
          class = "card shadow rounded p-3 mb-4",
          tags$p("No data available")
        )
      ))
    }

    # Create a main panel for each site
    tagList(
      lapply(site_data_list, function(site_data) {
        site_id <- site_data$site_id

        tags$div(
          class = "main-panel",
          tags$div(
            class = "card shadow rounded p-3 mb-4",
            # Canvas controls for this site
            fluidRow(
              id = paste0("canvas_controls_", site_id),
              class = "g-2 canvas-controls-row",
              column(3, textInput(
                paste0("canvas_classifier_", site_id),
                "Classifier",
                value = model_info()$name %||% "",
                width = "100%",
                placeholder = "Classifier"
              ) %>% tagAppendAttributes(disabled = "disabled")),
              column(1, numericInput(
                paste0("canvas_threshold_", site_id),
                "Threshold",
                value = threshold(),
                min = 0,
                max = 1,
                step = 0.001,
                width = "100%"
              ) %>% tagAppendAttributes(disabled = "disabled")),
              column(4, textInput(
                paste0("canvas_site_", site_id),
                "Site",
                value = site_data$site_info$name,
                width = "100%",
                placeholder = "Site"
              ) %>% tagAppendAttributes(disabled = "disabled")),
              column(1, textInput(
                paste0("canvas_year_", site_id),
                "Year",
                value = url_year(),
                width = "100%",
                placeholder = "Year"
              ) %>% tagAppendAttributes(disabled = "disabled")),
              column(3, textInput(
                paste0("canvas_species_", site_id),
                "Species",
                value = species_info()$name %||% "",
                width = "100%",
                placeholder = "Species"
              ) %>% tagAppendAttributes(disabled = "disabled"))
            ),
            # Tabs for this site
            tabsetPanel(
              # Heatmap tab
              tabPanel(
                title = "Heatmap",
                tags$div(
                  class = "tab-pane-content",
                  tags$div(
                    class = "plot-container",
                    # plot moon
                    tags$div(
                      class = "moon-timeline-container",
                      plotOutput(paste0("moon_timeline_", site_id),
                                height = "30px", width = "810px")
                    ),
                    # plot heatmap
                    tags$div(
                      class = "heatmap-plot",
                      plotlyOutput(
                        paste0("heatmap_", site_id),
                        width = "900px",
                        height = "400px"
                      )
                    )
                  )
                )
              ),
              # Histogram tab
              tabPanel(
                title = "Histogram",
                tags$div(
                  class = "tab-pane-content",
                  tags$div(
                    class = "plot-container",
                    tags$h3("To do"),
                    tags$div(
                      class = "hist-plot",
                      plotOutput(paste0("hist_", site_id), height = "410px")
                    )
                  )
                )
              ),
              # Acoustic activity tab
              tabPanel(
                title = "Acoustic activity",
                tags$div(
                  class = "tab-pane-content",
                  tags$div(
                    class = "plot-container",
                    tags$h3("To do"),
                    tags$div(
                      class = "events-plot",
                      plotOutput(paste0("events_", site_id), height = "410px")
                    )
                  )
                )
              )
            ),
            # Activity controls for this site
            create_activity_controls(site_id)
          )
        )
      })
    )
  })

  # Render heatmaps for all sites
  observe({
    site_data_list <- heatmap_data_list()

    if (!is.null(site_data_list) && length(site_data_list) > 0) {
      sun_toggle <- is.null(input$sun_toggle) || input$sun_toggle %% 2 == 1
      twilight_toggle <- !is.null(input$twilight_toggle) && input$twilight_toggle %% 2 == 1

      colormap <- input$colormap
      if (is.null(colormap) || colormap == "") colormap <- "rdbu"

      twilight_type <- input$twilight_type
      if (is.null(twilight_type) || twilight_type == "") twilight_type <- "civil"

      # Render a heatmap for each site
      lapply(seq_along(site_data_list), function(i) {
        local({
          site_idx <- i
          site_data <- site_data_list[[site_idx]]
          site_id <- site_data$site_id

          heatmap_output_id <- paste0("heatmap_", site_data$site_id)
          moon_output_id <- paste0("moon_timeline_", site_data$site_id)

          # Render heatmap
          output[[heatmap_output_id]] <- renderPlotly({
            render_heatmap_plot(
              site_data$data,
              url_year(),
              threshold(),
              sun_toggle,
              twilight_toggle,
              site_data$site_info$latitude,
              site_data$site_info$longitude,
              site_data$site_info$name,
              colormap,
              twilight_type
            )
          })

          # Render moon timeline
#          output[[moon_output_id]] <- renderPlot({
#            render_moon_timeline(url_year())
#          })

          # Render moon timeline directly
          output[[moon_output_id]] <- renderPlot({
            year <- url_year()
            moon_toggle <- !is.null(input$moonphase_toggle) && input$moonphase_toggle %% 2 == 1

            start_date <- as.Date(sprintf("%d-01-01", year))
            end_date <- as.Date(sprintf("%d-12-31", year))

            if (moon_toggle) {
              plot_moon_timeline(start_date, end_date)
            } else {
              par(mar = c(0, 0, 0, 0))
              plot(NA, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
            }
          }, height = 30, width = 736)

          # Render acoustic activity text
          output[[paste0("acoustic_activity_text_", site_id)]] <- renderUI({
            count_above_threshold <- get_count_above_threshold(
              species_id = url_species_id(),
              model_id = url_model_id(),
              site_id = site_id,
              year = url_year(),
              threshold = threshold()
            )

            tags$span(paste("No. of minutes with acoustic activity:", count_above_threshold))
          })

          # Register download handler for this site
          output[[paste0("download_", site_id)]] <- downloadHandler(
            filename = function() {
              filename <- sprintf("%s_%s_%s",
                                site_data$site_info$name,
                                url_year(),
                                species_info()$name)
              sanitized_filename <- gsub("[^a-zA-Z0-9_-]", "_", filename)
              paste0(sanitized_filename, ".csv")
            },
            content = function(file) {
              download_data <- get_download_data(
                species_id = url_species_id(),
                model_id = url_model_id(),
                site_id = site_id,
                year = url_year(),
                threshold = threshold()
              )
              write.csv(download_data, file, row.names = TRUE)
            }
          )
        })
      })
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)