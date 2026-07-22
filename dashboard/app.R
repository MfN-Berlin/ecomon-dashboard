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
#library(data.table)
library(suncalc)
library(plotly)


# Source external files
source("ui.R")
source("utils/ephem.R")
source("components/heatmap.R")
source("components/histogram.R")
source("utils/data_processing.R")
source("components/acoustic_activity.R")
source("utils/url.R")
source("utils/hasura.R")
source("utils/download_data.R")
source("components/canvas_controls.R")
source("components/moon.R")
source("utils/file.R")

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

  # Reactive value for threshold
  default_threshold <- as.numeric(Sys.getenv("DEFAULT_THRESHOLD", unset = "0.5"))
  if (is.na(default_threshold) || default_threshold < 0.1 || default_threshold > 1.0) {
    default_threshold <- 0.5
  }
  threshold <- reactiveVal(default_threshold)

  # Track whether a threshold is present and its value
  experimental_set <- reactiveVal(FALSE)
  experimental_threshold_value <- reactiveVal(NULL)
  preliminary_set <- reactiveVal(FALSE)
  preliminary_threshold_value <- reactiveVal(NULL)
  final_set <- reactiveVal(FALSE)
  
  # Track if a final threshold exists in database (can be set externally)
  final_threshold_exists_db <- reactiveVal(FALSE)

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
      url_model_id, url_year
    )
  })

  # Debounced threshold input (waits 500ms after user stops typing)
  threshold_debounced <- debounce(reactive(input$threshold), 1000)

  output$threshold_input <- renderUI({
    if (isTRUE(final_set())) {
      # Render a disabled native number input only when final to ensure it's non-editable
      return(tags$input(
        id = "threshold",
        type = "number",
        value = format(threshold(), nsmall = 2),
        min = "0.01",
        max = "1",
        step = "0.01",
        class = "form-control",
        style = "width: 7em;",
        disabled = "disabled"
      ))
    }

    numericInput(
      inputId = "threshold",
      label = NULL,
      value = threshold(),
      min = 0.01,
      max = 1,
      step = 0.01,
      width = "7em"
    )
  })

  # Update threshold reactive value when debounced input changes
  observeEvent(threshold_debounced(), {
    if (!is.null(threshold_debounced()) && !is.na(threshold_debounced())) {
      new_val <- as.numeric(threshold_debounced())
      threshold(new_val)
      # Update canvas_threshold to keep them in sync
      updateNumericInput(session, "canvas_threshold", value = new_val)

      # If a preliminary threshold is currently displayed and user changed the
      # input to a different value, show the override prompt message.
      prev_val <- preliminary_threshold_value()
      exp_val <- experimental_threshold_value()
      # Compare with a small tolerance to avoid floating-point equality issues
      tol <- 1e-9
      if (isTRUE(preliminary_set()) && !is.null(prev_val) && !is.na(prev_val) && !is.na(new_val)) {
        if (abs(new_val - as.numeric(prev_val)) > tol) {
          # Value differs from preliminary -> show override prompt
          output$threshold_status <- renderUI({
            tags$div(
              HTML('Preliminary threshold has been overriden. Would you like to <a href="#" onclick="Shiny.setInputValue(\'set_preliminary_threshold_btn\', Math.random()); return false;" style="color: #0d6efd; cursor: pointer; text-decoration: underline;">set it to the current threshold value</a>?'),
              style = "margin-top: -1em; color: #6c757d; white-space: nowrap; font-size: 0.75em; margin-right: 0.5em;"
            )
          })
        } else {
          # Value equals preliminary -> restore the preliminary message
          output$threshold_status <- renderUI({
            tags$div(
              "This is the preliminary threshold for this species and model.",
              style = "margin-top: 0em; color: #6c757d; white-space: nowrap; font-size: 0.75em; margin-right: 0.5em;"
            )
          })
        }
      } else if (isTRUE(experimental_set()) && !is.null(exp_val) && !is.na(exp_val) && !is.na(new_val)) {
        if (abs(new_val - as.numeric(exp_val)) > tol) {
          # Value differs from experimental -> show override prompt
          output$threshold_status <- renderUI({
            tags$div(
              HTML('Experimental threshold has been overriden. Would you like to <a href="#" onclick="Shiny.setInputValue(\'set_experimental_threshold_btn\', Math.random()); return false;" style="color: #0d6efd; cursor: pointer; text-decoration: underline;">set it to the current threshold value</a>?'),
              style = "margin-top: -1em; color: #6c757d; white-space: nowrap; font-size: 0.75em; margin-right: 0.5em;"
            )
          })
        } else {
          # Value equals experimental -> restore the experimental message
          output$threshold_status <- renderUI({
            tags$div(
              "This is the experimental threshold for this species and model.",
              style = "margin-top: 0em; color: #6c757d; white-space: nowrap; font-size: 0.75em; margin-right: 0.5em;"
            )
          })
        }
      }
    }
  })

  # Reset threshold to system default
  observeEvent(input$reset_threshold_btn, {
    current_label_id <- url_species_id()
    current_model_id <- url_model_id()
    
    # Check if a final threshold exists in the database
    if (!is.null(current_label_id) && !is.null(current_model_id) && 
        final_threshold_exists(current_label_id, current_model_id)) {
      # A final threshold was set externally - refresh the UI to show it as final
      final_record <- get_latest_final_threshold(current_label_id, current_model_id)
      if (!is.null(final_record) && !is.null(final_record$threshold)) {
        updateNumericInput(session, "threshold", value = final_record$threshold)
        output$threshold_status <- renderUI({
          tags$div(
            "This threshold is set as final for this species and model.",
            style = "margin-top: 0em; color: #6c757d; white-space: nowrap; font-size: 0.75em; margin-right: 0.5em;"
          )
        })
        experimental_set(FALSE)
        experimental_threshold_value(NULL)
        preliminary_set(FALSE)
        preliminary_threshold_value(NULL)
        final_set(TRUE)
        final_threshold_exists_db(TRUE)
      }
      # Show error message
      showNotification(
        "Cannot reset threshold: a final threshold has been set for this species and model.",
        type = "error"
      )
      return()
    }
    
    default_threshold <- as.numeric(Sys.getenv("DEFAULT_THRESHOLD", unset = "0.5"))
    if (is.na(default_threshold) || default_threshold < 0.1 || default_threshold > 1.0) {
      default_threshold <- 0.5
    }
    updateNumericInput(session, "threshold", value = default_threshold)
    
    # Update database threshold to default
    current_label_id <- url_species_id()
    current_model_id <- url_model_id()
    if (!is.null(current_label_id) && !is.null(current_model_id)) {
      reset_threshold_to_default(current_label_id, current_model_id, default_threshold)
    }
    
    # Check if there was a preliminary or experimental threshold and if it differs from default
    prev_val <- preliminary_threshold_value()
    exp_val <- experimental_threshold_value()
    tol <- 1e-9

    if (isTRUE(preliminary_set()) && !is.null(prev_val) && !is.na(prev_val) &&
        abs(as.numeric(default_threshold) - as.numeric(prev_val)) > tol) {
      # Default differs from preliminary -> show override message
      output$threshold_status <- renderUI({
        tags$div(
          HTML('Preliminary threshold has been overriden. Would you like to <a href="#" onclick="Shiny.setInputValue(\'set_preliminary_threshold_btn\', Math.random()); return false;" style="color: #0d6efd; cursor: pointer; text-decoration: underline;">update it</a>?'),
          style = "margin-top: 0em; color: #6c757d; white-space: nowrap; font-size: 0.75em; margin-right: 0.5em;"
        )
      })
      # Reset flags to remove checkmarks
      preliminary_set(FALSE)
      preliminary_threshold_value(NULL)
      experimental_set(FALSE)
      experimental_threshold_value(NULL)
      final_set(FALSE)
    } else if (isTRUE(experimental_set()) && !is.null(exp_val) && !is.na(exp_val) &&
               abs(as.numeric(default_threshold) - as.numeric(exp_val)) > tol) {
      # Default differs from experimental -> show override message
      output$threshold_status <- renderUI({
        tags$div(
          HTML('Experimental threshold has been overriden. Would you like to <a href="#" onclick="Shiny.setInputValue(\'set_experimental_threshold_btn\', Math.random()); return false;" style="color: #0d6efd; cursor: pointer; text-decoration: underline;">update it</a>?'),
          style = "margin-top: 0em; color: #6c757d; white-space: nowrap; font-size: 0.75em; margin-right: 0.5em;"
        )
      })
      # Reset flags to remove checkmarks
      experimental_set(FALSE)
      experimental_threshold_value(NULL)
      preliminary_set(FALSE)
      preliminary_threshold_value(NULL)
      final_set(FALSE)
    } else {
      # Clear any threshold status message and flags
      output$threshold_status <- renderUI({ NULL })
      experimental_set(FALSE)
      experimental_threshold_value(NULL)
      preliminary_set(FALSE)
      preliminary_threshold_value(NULL)
      final_set(FALSE)
    }
  })

  # Store the current threshold as experimental
  observeEvent(input$set_experimental_threshold_btn, {
    current_threshold <- input$threshold
    current_label_id <- url_species_id()
    current_model_id <- url_model_id()

    # Check if a final threshold exists in the database
    if (!is.null(current_label_id) && !is.null(current_model_id) && 
        final_threshold_exists(current_label_id, current_model_id)) {
      # A final threshold was set externally - refresh the UI to show it as final
      final_record <- get_latest_final_threshold(current_label_id, current_model_id)
      if (!is.null(final_record) && !is.null(final_record$threshold)) {
        updateNumericInput(session, "threshold", value = final_record$threshold)
        output$threshold_status <- renderUI({
          tags$div(
            "This threshold is set as final for this species and model.",
            style = "margin-top: 0em; color: #6c757d; white-space: nowrap; font-size: 0.75em; margin-right: 0.5em;"
          )
        })
        experimental_set(FALSE)
        experimental_threshold_value(NULL)
        preliminary_set(FALSE)
        preliminary_threshold_value(NULL)
        final_set(TRUE)
        final_threshold_exists_db(TRUE)
      }
      # Show error message
      showNotification(
        "Cannot set experimental threshold: a final threshold has been set for this species and model.",
        type = "error"
      )
      return()
    }

    if (!is.null(current_label_id) && !is.null(current_model_id)) {
      store_threshold(current_label_id, current_model_id, current_threshold, "experimental")
      # Update the threshold status text
      output$threshold_status <- renderUI({
        tags$div(
          "This is the experimental threshold for this species and model.",
          style = "margin-top: 0em; color: #6c757d; white-space: nowrap; font-size: 0.75em; margin-right: 0.5em;"
        )
      })
      # Mark experimental threshold state and save the value
      experimental_set(TRUE)
      experimental_threshold_value(as.numeric(current_threshold))
      preliminary_set(FALSE)
      preliminary_threshold_value(NULL)
      final_set(FALSE)
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

  # Store the current threshold as preliminary
  observeEvent(input$set_preliminary_threshold_btn, {
    current_threshold <- input$threshold
    current_label_id <- url_species_id()
    current_model_id <- url_model_id()

    # Check if a final threshold exists in the database
    if (!is.null(current_label_id) && !is.null(current_model_id) && 
        final_threshold_exists(current_label_id, current_model_id)) {
      # A final threshold was set externally - refresh the UI to show it as final
      final_record <- get_latest_final_threshold(current_label_id, current_model_id)
      if (!is.null(final_record) && !is.null(final_record$threshold)) {
        updateNumericInput(session, "threshold", value = final_record$threshold)
        output$threshold_status <- renderUI({
          tags$div(
            "This threshold is set as final for this species and model.",
            style = "margin-top: 0em; color: #6c757d; white-space: nowrap; font-size: 0.75em; margin-right: 0.5em;"
          )
        })
        experimental_set(FALSE)
        experimental_threshold_value(NULL)
        preliminary_set(FALSE)
        preliminary_threshold_value(NULL)
        final_set(TRUE)
        final_threshold_exists_db(TRUE)
      }
      # Show error message
      showNotification(
        "Cannot set preliminary threshold: a final threshold has been set for this species and model.",
        type = "error"
      )
      return()
    }

    if (!is.null(current_label_id) && !is.null(current_model_id)) {
      store_preliminary_threshold(current_label_id, current_model_id, current_threshold)
      # Update the threshold status text
      output$threshold_status <- renderUI({
        tags$div(
          "This is the preliminary threshold for this species and model.",
          style = "margin-top: 0em; color: #6c757d; white-space: nowrap; font-size: 0.75em; margin-right: 0.5em;"
        )
      })
      # Mark preliminary threshold state and save the value
      experimental_set(FALSE)
      experimental_threshold_value(NULL)
      preliminary_set(TRUE)
      final_set(FALSE)
      preliminary_threshold_value(as.numeric(current_threshold))
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

  # Render the 'Set current threshold as experimental' menu item; disable only when final is set
  output$set_experimental_threshold_menu_item <- renderUI({
    if (isTRUE(final_set()) || isTRUE(final_threshold_exists_db())) {
      tags$li(
        tags$a(
          class = "dropdown-item disabled",
          href = "#",
          tabindex = "-1",
          `aria-disabled` = "true",
          onclick = "return false;",
          "Set current threshold as experimental"
        )
      )
    } else {
      # Add checkmark when experimental is currently set
      if (isTRUE(experimental_set())) {
        tags$li(
          tags$a(
            class = "dropdown-item",
            href = "#",
            onclick = "Shiny.setInputValue('set_experimental_threshold_btn', Math.random()); return false;",
            HTML("&#10003; Set current threshold as experimental")
          )
        )
      } else {
        tags$li(
          tags$a(
            class = "dropdown-item",
            href = "#",
            onclick = "Shiny.setInputValue('set_experimental_threshold_btn', Math.random()); return false;",
            "Set current threshold as experimental"
          )
        )
      }
    }
  })

  # Render the 'Set current threshold as preliminary' menu item; disable only when final is set
  output$set_preliminary_threshold_menu_item <- renderUI({
    if (isTRUE(final_set()) || isTRUE(final_threshold_exists_db())) {
      tags$li(
        tags$a(
          class = "dropdown-item disabled",
          href = "#",
          tabindex = "-1",
          `aria-disabled` = "true",
          onclick = "return false;",
          "Set current threshold as preliminary"
        )
      )
    } else {
      # Add checkmark when preliminary is currently set
      if (isTRUE(preliminary_set())) {
        tags$li(
          tags$a(
            class = "dropdown-item",
            href = "#",
            onclick = "Shiny.setInputValue('set_preliminary_threshold_btn', Math.random()); return false;",
            HTML("&#10003; Set current threshold as preliminary")
          )
        )
      } else {
        tags$li(
          tags$a(
            class = "dropdown-item",
            href = "#",
            onclick = "Shiny.setInputValue('set_preliminary_threshold_btn', Math.random()); return false;",
            "Set current threshold as preliminary"
          )
        )
      }
    }
  })

  # Store the current threshold as final
  observeEvent(input$set_final_threshold_btn, {
    current_threshold <- input$threshold
    current_label_id <- url_species_id()
    current_model_id <- url_model_id()

    if (!is.null(current_label_id) && !is.null(current_model_id)) {
      store_final_threshold(current_label_id, current_model_id, current_threshold)
      output$threshold_status <- renderUI({
        tags$div(
          "This threshold has been saved as final for this species and model.",
          style = "margin-top: 0em; color: #6c757d; white-space: nowrap; font-size: 0.75em; margin-right: 0.5em;"
        )
      })
      experimental_set(FALSE)
      experimental_threshold_value(NULL)
      preliminary_set(FALSE)
      preliminary_threshold_value(NULL)
      final_set(TRUE)
    }
  })

  # Reset to last value stored in database
  observeEvent(input$reset_to_last_db_value_btn, {
    current_label_id <- url_species_id()
    current_model_id <- url_model_id()

    # Check if a final threshold exists in the database
    if (!is.null(current_label_id) && !is.null(current_model_id) && 
        final_threshold_exists(current_label_id, current_model_id)) {
      # A final threshold was set externally - cannot reset
      final_record <- get_latest_final_threshold(current_label_id, current_model_id)
      if (!is.null(final_record) && !is.null(final_record$threshold)) {
        updateNumericInput(session, "threshold", value = final_record$threshold)
        output$threshold_status <- renderUI({
          tags$div(
            "This threshold is set as final for this species and model.",
            style = "margin-top: 0em; color: #6c757d; white-space: nowrap; font-size: 0.75em; margin-right: 0.5em;"
          )
        })
        experimental_set(FALSE)
        experimental_threshold_value(NULL)
        preliminary_set(FALSE)
        preliminary_threshold_value(NULL)
        final_set(TRUE)
        final_threshold_exists_db(TRUE)
      }
      showNotification(
        "Cannot reset threshold: a final threshold has been set for this species and model.",
        type = "error"
      )
      return()
    }

    if (!is.null(current_label_id) && !is.null(current_model_id)) {
      # Fetch the latest threshold from the database
      latest_threshold <- get_latest_threshold(current_label_id, current_model_id)
      
      if (!is.null(latest_threshold) && !is.null(latest_threshold$threshold)) {
        new_threshold <- as.numeric(latest_threshold$threshold)
        updateNumericInput(session, "threshold", value = new_threshold)
        threshold(new_threshold)
        
        # Check the threshold type and display appropriate message
        threshold_type <- trimws(latest_threshold$threshold_type)
        
        if (tolower(threshold_type) == "preliminary") {
          output$threshold_status <- renderUI({
            tags$div(
              "This is the preliminary threshold for this species and model.",
              style = "margin-top: 0em; color: #6c757d; white-space: nowrap; font-size: 0.75em; margin-right: 0.5em;"
            )
          })
          experimental_set(FALSE)
          experimental_threshold_value(NULL)
          preliminary_set(TRUE)
          preliminary_threshold_value(new_threshold)
          final_set(FALSE)
        } else if (tolower(threshold_type) == "experimental") {
          output$threshold_status <- renderUI({
            tags$div(
              "This is the experimental threshold for this species and model.",
              style = "margin-top: 0em; color: #6c757d; white-space: nowrap; font-size: 0.75em; margin-right: 0.5em;"
            )
          })
          experimental_set(TRUE)
          experimental_threshold_value(new_threshold)
          preliminary_set(FALSE)
          preliminary_threshold_value(NULL)
          final_set(FALSE)
        } else {
          # Unknown type, treat as preliminary
          output$threshold_status <- renderUI({
            tags$div(
              "This is the preliminary threshold for this species and model.",
              style = "margin-top: 0em; color: #6c757d; white-space: nowrap; font-size: 0.75em; margin-right: 0.5em;"
            )
          })
          experimental_set(FALSE)
          experimental_threshold_value(NULL)
          preliminary_set(TRUE)
          preliminary_threshold_value(new_threshold)
          final_set(FALSE)
        }
      } else {
        # No threshold found in database
        showNotification(
          "No threshold found in database for this species and model.",
          type = "message"
        )
      }
    }
  })

  # Render the reset menu item; disable it only when a final threshold is set
  output$reset_threshold_menu_item <- renderUI({
    if (isTRUE(final_set()) || isTRUE(final_threshold_exists_db())) {
      tags$li(
        tags$a(
          class = "dropdown-item disabled",
          href = "#",
          tabindex = "-1",
          `aria-disabled` = "true",
          onclick = "return false;",
          "Reset to System Default"
        )
      )
    } else {
      tags$li(
        tags$a(
          class = "dropdown-item",
          href = "#",
          onclick = "Shiny.setInputValue('reset_threshold_btn', Math.random()); return false;",
          "Reset to System Default"
        )
      )
    }
  })

  # Render the 'Reset to last value stored in database' menu item; disable only when final is set
  output$reset_to_last_db_value_menu_item <- renderUI({
    if (isTRUE(final_set())) {
      tags$li(
        tags$a(
          class = "dropdown-item disabled",
          href = "#",
          tabindex = "-1",
          `aria-disabled` = "true",
          onclick = "return false;",
          "Reset to last value stored in database"
        )
      )
    } else {
      tags$li(
        tags$a(
          class = "dropdown-item",
          href = "#",
          onclick = "Shiny.setInputValue('reset_to_last_db_value_btn', Math.random()); return false;",
          "Reset to last value stored in database"
        )
      )
    }
  })

  # Render the 'Set threshold as final' menu item; enabled only when a preliminary threshold exists
  output$set_final_threshold_menu_item <- renderUI({
    # Check if preliminary threshold is set AND current threshold equals the preliminary value
    current_val <- input$threshold
    prev_val <- preliminary_threshold_value()
    tol <- 1e-9

    is_enabled <- isTRUE(preliminary_set()) &&
            !is.null(prev_val) &&
            !is.null(current_val) &&
            !is.na(prev_val) &&
            !is.na(current_val) &&
            abs(as.numeric(current_val) - as.numeric(prev_val)) <= tol

    if (is_enabled) {
      tags$li(
        tags$a(
          class = "dropdown-item",
          href = "#",
          onclick = "Shiny.setInputValue('set_final_threshold_btn', Math.random()); return false;",
          "Set threshold as final"
        )
      )
    } else {
      tags$li(
        tags$a(
          class = "dropdown-item disabled",
          href = "#",
          tabindex = "-1",
          `aria-disabled` = "true",
          onclick = "return false;",
          "Set threshold as final"
        )
      )
    }
  })


  # Fetch the latest threshold when species_id and model_id are available
  observe({
    if (!is.null(url_species_id()) && !is.null(url_model_id())) {
      # First, check if there's a final threshold
      final_record <- get_latest_final_threshold(url_species_id(), url_model_id())
      if (!is.null(final_record) && !is.null(final_record$threshold)) {
        # A final threshold exists -> set value and mark final
        updateNumericInput(session, "threshold", value = final_record$threshold)
        output$threshold_status <- renderUI({
          tags$div(
            "This threshold is set as final for this species and model.",
            style = "margin-top: 0em; color: #6c757d; white-space: nowrap; font-size: 0.75em; margin-right: 0.5em;"
          )
        })
        experimental_set(FALSE)
        experimental_threshold_value(NULL)
        preliminary_set(FALSE)
        preliminary_threshold_value(NULL)
        final_set(TRUE)
        final_threshold_exists_db(TRUE)
      } else {
        # No final threshold found; fall back to latest (preliminary or experimental) threshold
        final_threshold_exists_db(FALSE)
        latest_threshold <- get_latest_threshold(url_species_id(), url_model_id())
        if (!is.null(latest_threshold) && !is.null(latest_threshold$threshold)) {
          updateNumericInput(session, "threshold", value = latest_threshold$threshold)
          
          # Check the threshold type and display appropriate message
          threshold_type <- trimws(latest_threshold$threshold_type)
          
          if (tolower(threshold_type) == "preliminary") {
            # Show the preliminary message
            output$threshold_status <- renderUI({
              tags$div(
                "This is the preliminary threshold for this species and model.",
                style = "margin-top: 0em; color: #6c757d; white-space: nowrap; font-size: 0.75em; margin-right: 0.5em;"
              )
            })
            # Store preliminary flag and value
            experimental_set(FALSE)
            experimental_threshold_value(NULL)
            preliminary_set(TRUE)
            preliminary_threshold_value(as.numeric(latest_threshold$threshold))
            final_set(FALSE)
          } else if (tolower(threshold_type) == "experimental") {
            # Show the experimental message
            output$threshold_status <- renderUI({
              tags$div(
                "This is the experimental threshold for this species and model.",
                style = "margin-top: 0em; color: #6c757d; white-space: nowrap; font-size: 0.75em; margin-right: 0.5em;"
              )
            })
            # Store experimental flag and value
            experimental_set(TRUE)
            experimental_threshold_value(as.numeric(latest_threshold$threshold))
            preliminary_set(FALSE)
            preliminary_threshold_value(NULL)
            final_set(FALSE)
          } else {
            # Unknown type, treat as preliminary for backwards compatibility
            output$threshold_status <- renderUI({
              tags$div(
                "This is the preliminary threshold for this species and model.",
                style = "margin-top: 0em; color: #6c757d; white-space: nowrap; font-size: 0.75em; margin-right: 0.5em;"
              )
            })
            experimental_set(FALSE)
            experimental_threshold_value(NULL)
            preliminary_set(TRUE)
            preliminary_threshold_value(as.numeric(latest_threshold$threshold))
            final_set(FALSE)
          }
        } else {
          # Clear the threshold status when no threshold exists
          output$threshold_status <- renderUI({ NULL })
          experimental_set(FALSE)
          experimental_threshold_value(NULL)
          preliminary_set(FALSE)
          preliminary_threshold_value(NULL)
          final_set(FALSE)
        }
      }
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

  # Get site information only (without loading data - so it doesn't re-render when threshold changes)
  site_list <- reactive({
    if (is.null(url_species_id()) || is.null(url_model_id()) ||
          is.null(url_site_ids()) || is.null(url_year())) {
      return(NULL)
    }

    message("Executing site_list")

    # Get site info only (no data processing)
    site_data_list <- lapply(url_site_ids(), function(site_id) {
      tryCatch({
        site_info <- get_site_info(site_id)
        list(
          site_id = site_id,
          site_info = site_info
        )
      }, error = function(e) {
        message(paste("Error in get_site_info:", e$message))
        return(NULL)
      })
    })
    # Remove NULL entries
    site_data_list[!sapply(site_data_list, is.null)]
  })

  # Load and process data for all sites (this will still be used for plots)
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

  # No tab tracking needed - Shiny will preserve tab state naturally
  # The key is to NOT use renderUI for parts that contain tabsetPanel

  # Render dynamic UI for multiple heatmaps
  output$site_panels <- renderUI({
    site_data_list <- site_list()  # Use site_list instead of heatmap_data_list

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
              column(1, uiOutput(paste0("canvas_threshold_ui_", site_id))),
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
              id = paste0("tabs_", site_id),
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
              # Bin tab
              tabPanel(
                title = "Bin size",
                tags$div(
                  class = "tab-pane-content",
                  tags$div(
                    class = "plot-container",
                    tags$div(
                      class = "bin-size-menu",
                      style = "display: flex; align-items: center; gap: 10px; margin-bottom: 10px;",
                      tags$span("Bin Size:", style = "font-weight: bold;"),
                      numericInput(
                        inputId = paste0("bin_size_", site_id),
                        label = NULL,
                        value = 0.01,
                        min = 0.001,
                        max = 1,
                        step = 0.001
                      )
                    ),
                    tags$div(
                      class = "distribution-selector",
                      style = "display: flex; gap: 10px; margin-bottom: 10px;",
                      tags$span("Distribution of", style = "font-weight: bold;"),
                      radioButtons(
                        inputId = paste0("distribution_type_", site_id),
                        label = NULL,
                        choices = list(
                          "All values" = "all",
                          "Minutes with activity" = "activity"
                        ),
                        selected = "activity",
                        inline = TRUE
                      ),
                      tags$script(
                        HTML(
                          sprintf(
                            "$(document).ready(function() {
                              $('input[name=\"distribution_type_%s\"][value=\"all\"]').prop('disabled', true);
                              $('input[name=\"distribution_type_%s\"][value=\"all\"]').parent().css({'color': 'gray', 'cursor': 'not-allowed'});
                            });",
                            site_id, site_id
                          )
                        )
                      )
                    ),
                    tags$div(
                      class = "hist-plot",
                      plotlyOutput(paste0("hist_", site_id), width = "900px", height = "400px")
                    )
                  )
                )
              ),
              # Acoustic activity tab
              tabPanel(
                title = "Acoustic activity",
                value = paste0("acoustic_", site_id),
                tags$div(
                  class = "tab-pane-content",
                  tags$div(
                    class = "plot-container",
                    tags$div(
                      class = "interval-menu",
                      style = "display: flex; align-items: center; gap: 10px;",
                      tags$span("Interval:", style = "font-weight: bold;"),
                      selectInput(
                        inputId = paste0("interval_", site_id),
                        label = NULL,
                        choices = list(
                          "month" = "month",
                          "10-day" = "10-day",
                          "5-day" = "5-day",
                          "daily" = "daily"
                        ),
                        selected = "daily"
                      )
                    ),
                    tags$div(
                      class = "events-plot",
                      plotlyOutput(paste0("events_", site_id), width = "900px", height = "400px")
                    )
                  )
                )
              ),
              # Diel acoustic activity tab
              tabPanel(
                title = "Diel acoustic activity",
                value = paste0("diel_", site_id),
                tags$div(
                  class = "tab-pane-content",
                  tags$div(
                    class = "plot-container",
                    tags$div(
                      class = "date-range-info",
                      style = "display: flex; align-items: center; gap: 10px; margin-bottom: 10px;",
                      tags$span("Date Range:", style = "font-weight: bold;"),
                      textOutput(paste0("date_range_", site_id), inline = TRUE)
                    ),
                    tags$div(
                      class = "diel-plot",
                      plotlyOutput(paste0("diel_", site_id), width = "900px", height = "400px")
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

  # Render threshold inputs separately to avoid re-rendering entire UI
  observe({
    site_data_list <- heatmap_data_list()

    if (!is.null(site_data_list) && length(site_data_list) > 0) {
      lapply(site_data_list, function(site_data) {
        site_id <- site_data$site_id
        output[[paste0("canvas_threshold_ui_", site_id)]] <- renderUI({
          numericInput(
            paste0("canvas_threshold_", site_id),
            "Threshold",
            value = threshold(),
            min = 0,
            max = 1,
            step = 0.001,
            width = "100%"
          ) %>% tagAppendAttributes(disabled = "disabled")
        })
      })
    }
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

          # Log site_data contents
          message("site_data contents for site ", site_data$site_id, ":\n")
          # Print each field and its value
          for(field_name in names(site_data$site_info)) {
            message(field_name, ": ", site_data$site_info[[field_name]])
          }

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
              model_info()$name,
              species_info()$name,
              colormap,
              twilight_type
            )
          })

          # Render histogram
          histogram_output_id <- paste0("hist_", site_id)
          bin_size_input_id <- paste0("bin_size_", site_id)
          distribution_input_id <- paste0("distribution_type_", site_id)
          output[[histogram_output_id]] <- renderPlotly({
            render_histogram_plot(
              site_data$data,
              threshold(),
              url_year(),
              site_data$site_info$name,
              model_info()$name,
              species_info()$name,
              bin_size = input[[bin_size_input_id]],
              distribution_type = input[[distribution_input_id]]
            )
          })

          # Render acoustic activity
          events_output_id <- paste0("events_", site_id)
          interval_input_id <- paste0("interval_", site_id)
          output[[events_output_id]] <- renderPlotly({
            render_acoustic_activity_plot(
              site_data$data,
              threshold(),
              url_year(),
              input[[interval_input_id]],
              site_data$site_info$name,
              model_info()$name,
              species_info()$name
            )
          })

          # Render diel acoustic activity
          diel_output_id <- paste0("diel_", site_id)
          output[[diel_output_id]] <- renderPlotly({
            render_diel_acoustic_activity_plot(
              site_data$data,
              threshold(),
              url_year(),
              site_data$site_info$name,
              model_info()$name,
              species_info()$name,
              lat = site_data$site_info$latitude,
              lon = site_data$site_info$longitude
            )
          })

          # Render date range placeholder
          date_range_output_id <- paste0("date_range_", site_id)
          output[[date_range_output_id]] <- renderText({
            "YYYY-MM-DD to YYYY-MM-DD"
          })

          # Render moon timeline
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

          # Register download CSV handler for this site
          output[[paste0("download_", site_id)]] <- downloadHandler(
            filename = function() {
              get_filename(
                "DT",
                site_data$site_info$name,
                model_info()$name,
                url_year(),
                species_info()$name,
                threshold(),
                "csv"
              )
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
          # Register download Excel handler for this site
          output[[paste0("download_", site_id, "_xlsx")]] <- downloadHandler(
            filename = function() {
              get_filename(
                "DT",
                site_data$site_info$name,
                model_info()$name,
                url_year(),
                species_info()$name,
                threshold(),
                "xlsx"
              )
            },
            content = function(file) {
              # Load required package
              require(openxlsx)

              # Get your data (replace with your actual data)
              data <- get_download_data(
                species_id = url_species_id(),
                model_id = url_model_id(),
                site_id = site_id,
                year = url_year(),
                threshold = threshold()
              )

              # Create workbook and write data
              wb <- createWorkbook()
              addWorksheet(wb, "Activity Data")
              writeData(wb, "Activity Data", data)

              # Save the file
              saveWorkbook(wb, file, overwrite = TRUE)
            }
          )
        })
      })
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
