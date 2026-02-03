# R/server_taxonomy.R
# Server-side taxonomy filtering functions for Track G

#' Server Cube Metadata Updates
#'
#' @description
#' Updates cell size and date range when data cube loads.
#' Extracted from observeEvent(input$dataCube, ...) in app.R.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param r Reactive values containing dataCube and dataCube1
#'
#' @return NULL (side effects only)
#'
#' @importFrom shiny observeEvent req updateNumericInput updateSliderInput
#' @importFrom stringr str_extract
#'
#' @keywords internal
server_cube_metadata_updates <- function(input, output, session, r) {
  observeEvent(input$dataCube, {
    req(r$dataCube)

    # Extract resolution and determine units and size
    units <- stringr::str_extract(
      r$dataCube$resolutions,
      "(?<=[0-9,.]{1,6})[a-z]*$"
    )

    if (units == "degrees") {
      res_size <- as.numeric(stringr::str_extract(
        r$dataCube$resolutions,
        "[0-9,.]*(?=degrees)"
      ))
      defaultres <- ifelse(res_size > 0.25, res_size, 0.25)
      maxres <- 10
    } else if (units == "km") {
      res_size <- as.numeric(stringr::str_extract(
        r$dataCube$resolutions,
        "[0-9]*(?=km)"
      ))
      defaultres <- ifelse(res_size > 10, res_size, 10)
      maxres <- 100
    } else {
      # Default fallback if units not recognized
      res_size <- 1
      defaultres <- 1
      maxres <- 10
    }

    # Update cell size input
    tryCatch({
      updateNumericInput(
        session,
        inputId = "cellsize",
        min = res_size,
        max = maxres,
        value = defaultres,
        step = res_size
      )
    }, error = function(e) {
      warning("Failed to update cell size input: ", e$message)
    })

    # Update date range slider using dataCube1
    daterangemin <- r$dataCube1$first_year
    daterangemax <- r$dataCube1$last_year
    value <- c(daterangemin, daterangemax)

    tryCatch({
      updateSliderInput(
        session,
        inputId = "daterange",
        min = daterangemin,
        max = daterangemax,
        value = value
      )
    }, error = function(e) {
      warning("Failed to update date range slider: ", e$message)
    })
  })
}


#' Server Taxonomy Filtering
#'
#' @description
#' Handles family/species cascading dropdowns and data filtering.
#' Creates reactive expressions for available families and species,
#' updates dropdowns based on selections, and filters dataCube1.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param r Reactive values containing dataCube and dataCube1
#'
#' @return NULL (side effects only)
#'
#' @importFrom shiny observeEvent req reactive updateSelectInput
#' @importFrom dplyr filter pull
#'
#' @keywords internal
server_taxonomy_filtering <- function(input, output, session, r) {

  # Get all families in the data cube
  all_families <- reactive({
    req(r$dataCube)
    tryCatch({
      sort(unique(r$dataCube$data$family))
    }, error = function(e) {
      warning("Error extracting families: ", e$message)
      character(0)
    })
  })

  # Get all species in the data cube
  all_species <- reactive({
    req(r$dataCube)
    tryCatch({
      sort(unique(r$dataCube$data$scientificName))
    }, error = function(e) {
      warning("Error extracting species: ", e$message)
      character(0)
    })
  })

  # Update family selection dropdown when families change
  observeEvent(all_families(), {
    tryCatch({
      updateSelectInput(
        session,
        inputId = "family",
        choices = all_families(),
        selected = input$family
      )
    }, error = function(e) {
      warning("Failed to update family dropdown: ", e$message)
    })
  })

  # Update species selection based on family selection
  observeEvent(input$family, {
    available_species <- all_species()

    if (is.null(input$family) || length(input$family) == 0) {
      # No family selected - show all species and reset data
      tryCatch({
        updateSelectInput(
          session,
          inputId = "species",
          choices = available_species,
          selected = NULL
        )
        r$dataCube1$data <- r$dataCube$data
        message("Taxonomy filter reset. Data rows: ", nrow(r$dataCube1$data))
      }, error = function(e) {
        warning("Failed to reset species dropdown: ", e$message)
      })
    } else {
      # Family selected - filter species to those in selected families
      tryCatch({
        available_species <- r$dataCube$data %>%
          dplyr::filter(family %in% input$family) %>%
          dplyr::pull(scientificName) %>%
          unique() %>%
          sort()

        updateSelectInput(
          session,
          inputId = "species",
          choices = available_species,
          selected = intersect(input$species, available_species)
        )
      }, error = function(e) {
        warning("Failed to update species dropdown: ", e$message)
      })
    }
  })

  # Filter data based on selected families and species
  observeEvent(list(input$family, input$species), {
    req(r$dataCube)

    # Log current filter state
    if (length(input$family) == 0 && length(input$species) == 0) {
      message("Taxonomy filters: Both families and species deselected.")
    } else {
      family_text <- ifelse(
        length(input$family) == 0,
        "None",
        paste(input$family, collapse = ", ")
      )
      species_text <- ifelse(
        length(input$species) == 0,
        "None",
        paste(input$species, collapse = ", ")
      )
      message("Taxonomy filters - Families: ", family_text,
              " | Species: ", species_text)
    }

    # Apply filtering using taxonomy helper function
    tryCatch({
      filtered_result <- filter_by_taxonomy(
        cube = r$dataCube,
        families = input$family,
        species = input$species,
        preserve_cube = TRUE
      )

      r$dataCube1$data <- filtered_result$data
      message("Filtered data rows: ", nrow(r$dataCube1$data))

    }, error = function(e) {
      warning("Failed to apply taxonomy filter: ", e$message)
      # Reset to original data on error
      r$dataCube1$data <- r$dataCube$data
    })
  })

  # Handle species deselection with family filter maintained
  observeEvent(input$species, {
    if (is.null(input$species) || length(input$species) == 0) {
      tryCatch({
        if (is.null(input$family) || length(input$family) == 0) {
          # Both deselected - reset to full data
          r$dataCube1$data <- r$dataCube$data
        } else {
          # Only species deselected - maintain family filter
          filtered_result <- filter_by_taxonomy(
            cube = r$dataCube,
            families = input$family,
            species = NULL,
            preserve_cube = TRUE
          )
          r$dataCube1$data <- filtered_result$data
        }
        message("Species filter cleared. Data rows: ", nrow(r$dataCube1$data))
      }, error = function(e) {
        warning("Failed to update data after species deselection: ", e$message)
      })
    }
  })
}
