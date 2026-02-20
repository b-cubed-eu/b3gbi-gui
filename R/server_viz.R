# R/server_viz.R
# Server-side visualization functions for Track G
# Handles UI dependencies, input parsing, and plot generation

#' Server Visualization UI Dependencies
#'
#' @description
#' Handles all UI input resets when checkboxes are unchecked.
#' Includes observeEvent handlers for ts_expand, custom_map_axes,
#' legend_limits, custom_bg, and custom_land_fill.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#'
#' @return None (called for side effects)
#'
#' @export
server_viz_ui_dependencies <- function(input, output, session) {

  # Update time series axes expansion when custom axes checkbox is deselected
  observeEvent(input$ts_expand, {
    if (input$ts_expand == FALSE) {
      updateNumericInput(
        session,
        inputId = "ts_x_expand_left",
        value = 0
      )
      updateNumericInput(
        session,
        inputId = "ts_x_expand_right",
        value = 0
      )
      updateNumericInput(
        session,
        inputId = "ts_y_expand_top",
        value = 0
      )
      updateNumericInput(
        session,
        inputId = "ts_y_expand_bottom",
        value = 0
      )
    }
  })

  # Update map axes when custom map axes checkbox is deselected
  observeEvent(input$custom_map_axes, {
    if (input$custom_map_axes == FALSE) {
      updateNumericInput(
        session,
        inputId = "xcoord_min",
        value = ""
      )
      updateNumericInput(
        session,
        inputId = "xcoord_max",
        value = ""
      )
      updateNumericInput(
        session,
        inputId = "ycoord_min",
        value = ""
      )
      updateNumericInput(
        session,
        inputId = "ycoord_max",
        value = ""
      )
    }
  })

  # Update legend limits when custom legend checkbox is deselected
  observeEvent(input$legend_limits, {
    if (input$legend_limits == FALSE) {
      updateTextInput(
        session,
        inputId = "legend_min",
        value = ""
      )
      updateTextInput(
        session,
        inputId = "legend_max",
        value = ""
      )
    }
  })

  # Update panel background (ocean) colour when custom_bg checkbox is deselected
  observeEvent(input$custom_bg, {
    if (input$custom_bg == FALSE) {
      updateTextInput(
        session,
        inputId = "ocean_fill_colour",
        value = "#92c5f0"
      )
    }
  })

  # Update land fill colour when custom_land_fill checkbox is deselected
  observeEvent(input$custom_land_fill, {
    if (input$custom_land_fill == FALSE) {
      updateTextInput(
        session,
        inputId = "land_fill_colour",
        value = "grey85"
      )
    }
  })

}

#' Server Parsed Inputs
#'
#' @description
#' Creates reactive expressions for parsing comma-separated string inputs
#' into numeric or character vectors. Includes parsed_breaks, parsed_labels,
#' parsed_xbreaks, and parsed_ybreaks.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#'
#' @return List of reactive expressions:
#'   - parsed_breaks: numeric vector from input$breaks
#'   - parsed_labels: character vector from input$labels
#'   - parsed_xbreaks: numeric vector from input$xbreaks
#'   - parsed_ybreaks: numeric vector from input$ybreaks
#'
#' @export
server_parsed_inputs <- function(input, output, session) {

  # Convert text input for legend breaks into a numeric vector
  parsed_breaks <- reactive({
    inputText <- input$breaks
    # Remove spaces and split by comma
    breakpointVector <- unlist(strsplit(inputText, ",\\s*"))
    # Convert to numeric, suppress any conversion warnings of invalid entries
    as.numeric(breakpointVector)
  })

  # Convert text input for legend break labels into a character vector
  parsed_labels <- reactive({
    inputLabels <- input$labels
    if (inputLabels == "") {
      return(NULL)
    }
    labelVector <- unlist(strsplit(inputLabels, ",\\s*"))
    labelVector
  })

  # Convert text input for x-axis breaks into a numeric vector
  parsed_xbreaks <- reactive({
    inputText <- input$xbreaks
    # Remove spaces and split by comma
    breakpointVector <- unlist(strsplit(inputText, ",\\s*"))
    # Convert to numeric, suppress any conversion warnings of invalid entries
    as.numeric(breakpointVector)
  })

  # Convert text input for y-axis breaks into a numeric vector
  parsed_ybreaks <- reactive({
    inputText <- input$ybreaks
    # Remove spaces and split by comma
    breakpointVector <- unlist(strsplit(inputText, ",\\s*"))
    # Convert to numeric, suppress any conversion warnings of invalid entries
    as.numeric(breakpointVector)
  })

  # Return list of reactives
  list(
    parsed_breaks = parsed_breaks,
    parsed_labels = parsed_labels,
    parsed_xbreaks = parsed_xbreaks,
    parsed_ybreaks = parsed_ybreaks
  )

}

#' Server Map Plot
#'
#' @description
#' Handles map plot generation including input validation, parameter processing,
#' and plot rendering. Creates plot_to_print_map reactive and output$plot_map renderPlot.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param r Reactive values object containing dataCube1 and other shared state
#' @param parsed_inputs List of parsed input reactives from server_parsed_inputs()
#' @param ivplot Input validation object for plot inputs
#' @param plot_to_render_map Reactive containing the map indicator data
#'
#' @return List containing:
#'   - plot_to_print_map: reactive expression returning the ggplot map object
#'   - render_map: function to render the map output
#'
#' @export
server_map_plot <- function(input, output, session, r, parsed_inputs) {

  # Use eventReactive to create plot when button is clicked
  plot_to_print_map <- eventReactive(input$plot_map_bt, {
    req(r$dataCube1)
    req(input$indicatorsToAnalyse)

    # Convert mapres from numeric to expected values
    mapres_converted <- switch(input$mapres,
                               "110" = "small",
                               "50" = "medium",
                               "10" = "large",
                               input$mapres)

    # Calculate the indicator with error handling
    indicator_result <- tryCatch({
      calc_indicator_map(
        data = r$dataCube1,
        indicator = input$indicatorsToAnalyse,
        cell_size = input$cellsize,
        spatiallevel = input$spatiallevel,
        first_year = input$daterange[1],
        last_year = input$daterange[2],
        countrytype = input$countrytype,
        mapres = mapres_converted
      )
    }, error = function(e) {
      message("ERROR in calc_indicator_map: ", e$message)
      showNotification(paste("Error calculating map:", e$message), type = "error")
      NULL
    })

    req(indicator_result)

    # Check if custom coordinates are provided and format appropriately
    if (input$xcoord_min == "" && input$xcoord_max == "") {
      xlims <- NULL
    } else if (input$xcoord_min == "" || input$xcoord_max == "") {
      stop("To plot custom coordinates you must provide both min and max.")
    } else {
      xlims <- c(as.numeric(input$xcoord_min), as.numeric(input$xcoord_max))
    }
    if (input$ycoord_min == "" && input$ycoord_max == "") {
      ylims <- NULL
    } else if (input$ycoord_min == "" || input$ycoord_max == "") {
      stop("To plot custom coordinates you must provide both min and max.")
    } else {
      ylims <- c(as.numeric(input$ycoord_min), as.numeric(input$ycoord_max))
    }

    # Get parsed breaks and labels
    breaks <- parsed_inputs$parsed_breaks()
    labels <- parsed_inputs$parsed_labels()
    xbreaks <- parsed_inputs$parsed_xbreaks()
    ybreaks <- parsed_inputs$parsed_ybreaks()

    # Check if breaks are valid
    if (is.null(breaks) ||
        input$breaks == "" ||
        any(is.na(breaks))) {
      breaks <- NULL
    }

    # Check if labels are valid
    if (is.null(labels) ||
        input$labels == "" ||
        length(labels) != length(breaks)) {
      labels <- NULL
    }

    # Check if x-axis breaks are valid
    if (is.null(xbreaks) ||
        input$xbreaks == "" ||
        any(is.na(xbreaks))) {
      xbreaks <- NULL
    }

    # Check if y-axis breaks are valid
    if (is.null(ybreaks) ||
        input$ybreaks == "" ||
        any(is.na(ybreaks))) {
      ybreaks <- NULL
    }

    # Check if custom legend limits are provided and format appropriately
    if (input$legend_min == "" && input$legend_max == "") {
      legend_limits <- NULL
    } else if (input$legend_min == "" || input$legend_max == "") {
      stop("To plot custom legend limits you must provide both min and max.")
    } else {
      legend_limits <- c(as.numeric(input$legend_min),
                         as.numeric(input$legend_max))
    }

    # Check if title is provided
    if (input$title == "") {
      title <- "auto"
    } else {
      title <- input$title
    }

    # Check if legend title is provided
    if (input$legend_title == "") {
      legend_title <- NULL
    } else {
      legend_title <- input$legend_title
    }

    # Check if transformation is provided
    if (input$trans_yesno == FALSE ||
        is.null(input$trans_yesno) ||
        is.null(input$trans)) {
      trans <- NULL
    } else {
      trans <- input$trans
    }

    # Check if bcpower is provided
    if (is.null(input$bcpower) || input$bcpower == "") {
      bcpower <- NULL
    } else {
      bcpower <- input$bcpower
    }

    # Check if land fill colour is provided
    if (input$land_fill_colour == "") {
      land_fill_colour <- NULL
    } else {
      land_fill_colour <- input$land_fill_colour
    }

    # Check if panel background colour is provided
    if (input$ocean_fill_colour == "" || is.null(input$ocean_fill_colour)) {
      ocean_fill_colour <- NULL
    } else {
      ocean_fill_colour <- input$ocean_fill_colour
    }

    # Check if subtitle is provided
    if (input$subtitle == "") {
      subtitle <- NULL
    } else {
      subtitle <- input$subtitle
    }

    # Check if caption is provided
    if (input$caption == "") {
      caption <- NULL
    } else {
      caption <- input$caption
    }

    # Check that title, subtitle and caption font size inputs are ok
    if (input$title_size < 8 ||
        input$title_size > 40 ||
        is.na(input$title_size)
    ) {
      showNotification(
        paste0("Title font size is outside reasonable boundaries.",
               "Resetting to default."),
        type = "error")
      title_size <- NULL
    } else {
      title_size <- input$title_size
    }

    if (input$subtitle_size < 6 ||
        input$subtitle_size > 30 ||
        is.na(input$subtitle_size)
    ) {
      showNotification(
        paste0("Subtitle font size is outside reasonable boundaries.",
               "Resetting to default."),
        type = "error")
      subtitle_size <- NULL
    } else {
      subtitle_size <- input$subtitle_size
    }

    if (input$caption_size < 4 ||
        input$caption_size > 20 ||
        is.na(input$caption_size)
    ) {
      showNotification(
        paste0("Caption font size is outside reasonable boundaries. ",
               "Resetting to default."),
        type = "error")
      caption_size <- NULL
    } else {
      caption_size <- input$caption_size
    }

    if (input$title_wrap_length < 20 ||
        input$title_wrap_length > 200 ||
        is.na(input$title_wrap_length)
    ) {
      showNotification(
        paste0("Title wrap length is outside reasonable boundaries. ",
               "Resetting to default."),
        type = "error")
      title_wrap_length <- NULL
    } else {
      title_wrap_length <- input$title_wrap_length
    }

    if (input$legend_title_wrap_length < 10 ||
        input$legend_title_wrap_length > 100 ||
        is.na(input$legend_title_wrap_length)
    ) {
      showNotification(
        paste0("Title wrap length is outside reasonable boundaries. ",
               "Resetting to default."),
        type = "error")
      legend_title_wrap_length <- NULL
    } else {
      legend_title_wrap_length <- input$legend_title_wrap_length
    }

    if (input$xaxis_fontsize < 0 ||
        input$xaxis_fontsize > 30 ||
        is.na(input$xaxis_fontsize)
    ) {
      showNotification(
        paste0("X-Axis label font size is outside reasonable boundaries. ",
               "Resetting to default."),
        type = "error")
      xaxis_fontsize <- NULL
    } else {
      xaxis_fontsize <- input$xaxis_fontsize
    }

    if (input$yaxis_fontsize < 0 ||
        input$yaxis_fontsize > 30 ||
        is.na(input$yaxis_fontsize)
    ) {
      showNotification(
        paste0("Y-Axis label font size is outside reasonable boundaries. ",
               "Resetting to default."),
        type = "error")
      yaxis_fontsize <- NULL
    } else {
      yaxis_fontsize <- input$yaxis_fontsize
    }

    mapres <- switch(input$mapres,
                     "110" = "small",
                     "50" = "medium",
                     "10" = "large"
    )

    if ((is.null(input$species) || length(input$species) == 0) &&
        (input$indicatorsToAnalyse == "Species Occurrences" ||
         input$indicatorsToAnalyse == "Species Range")) {
      showNotification(
        paste0("Please select a single species using the 'Subset by family'",
         " and 'Subset by species' filters."), type = "error")
    }

    # Prepare parameters for plot
    params <- list(
      x = indicator_result,
      title = title,
      title_wrap_length = title_wrap_length,
      xlims = xlims,
      ylims = ylims,
      trans = trans,
      bcpower = bcpower,
      breaks = breaks,
      labels = labels,
      output_crs = NULL,
      crop_to_grid = input$crop_to_grid,
      crop_by_region = input$crop_by_region,
      ocean_fill_colour = ocean_fill_colour,
      land_fill_colour = land_fill_colour,
      grid_fill_colour = NULL,
      grid_line_colour = NULL,
      grid_outline_colour = NULL,
      grid_line_width = NULL,
      grid_outline_width = NULL,
      grid_fill_transparency = NULL,
      grid_line_transparency = NULL,
      legend_title = legend_title,
      legend_limits = legend_limits,
      legend_title_wrap_length = legend_title_wrap_length,
      visible_gridlines = input$gridlines,
      visible_grid_outline = input$grid_outline,
      visible_panel_gridlines = input$panel_gridlines,
      map_expansion_factor = 0.5,
      layers = NULL,
      layer_colours = NULL,
      layer_fill_colours = NULL,
      scale = mapres
    )

    if ((input$indicatorsToAnalyse == "Species Occurrences" ||
         input$indicatorsToAnalyse == "Species Range") &&
        length(input$species) > 0) {
      if (length(input$species) > 1) {
        showNotification(
          paste0(
            "Visualization options for this indicator currently only work ",
            "properly if you select a single species. Hopefully it will be ",
            "fixed soon. For now, please select only one species."
          ), type = "error"
        )
        return(NULL)
      }

      params$species <- c(input$species)

    }

    # Create plot using params which already contains x = indicator_result
    map_plot <- do.call(plot, params)

    # Add other options to plot
    map_plot <- map_plot +
      labs(subtitle = subtitle,
           caption = caption) +
      theme(
        axis.text.x = element_text(size = xaxis_fontsize),
        axis.text.y = element_text(size = yaxis_fontsize),
        plot.title = element_text(color = input$title_color,
                                  size = title_size,
                                  face = "bold"),
        plot.subtitle = element_text(color = input$subtitle_color,
                                     size = subtitle_size),
        plot.caption = element_text(color = input$caption_color,
                                     size = caption_size,
                                     face = "italic")
      )

    if (!is.null(xbreaks)) {
      map_plot <- map_plot + scale_x_continuous(breaks = xbreaks)
    }

    if (!is.null(ybreaks)) {
      map_plot <- map_plot + scale_y_continuous(breaks = ybreaks)
    }

    if (input$add_scalebar == TRUE) {

      if (input$scalebar_height > 0.1 ||
          input$scalebar_height < 0.01 ||
          is.na(input$scalebar_height)
      ) {
        showNotification(
          paste0("Scale bar height is outside reasonable boundaries. ",
                 "Resetting to default."),
          type = "error")
        scalebar_height <- NULL
      } else {
        scalebar_height <- input$scalebar_height
      }

      if (input$scalebar_size > 1 ||
          input$scalebar_size < 0.01 ||
          is.na(input$scalebar_size)
      ) {
        showNotification(
          paste0("Scale bar size is outside reasonable boundaries. ",
                 "Resetting to default."),
          type = "error")
        scalebar_size <- 0.2
      } else {
        scalebar_size <- input$scalebar_size
      }

      if (input$scalebar_tickheight > 2 ||
          input$scalebar_tickheight < 0 ||
          is.na(input$scalebar_tickheight)
      ) {
        showNotification(
          paste0("Scale bar tick height is outside reasonable boundaries. ",
                 "Resetting to default."),
          type = "error")
        scalebar_tickheight <- 0.6
      } else {
        scalebar_tickheight <- input$scalebar_tickheight
      }

      if (input$scalebar_padx > 0.5 ||
          input$scalebar_padx < 0 ||
          is.na(input$scalebar_padx)
      ) {
        showNotification(
          paste0("Scale bar x-axis padding is outside reasonable boundaries. ",
                 "Resetting to default."),
          type = "error")
        scalebar_padx <- 0.01
      } else {
        scalebar_padx <- input$scalebar_padx
      }

      if (input$scalebar_pady > 0.5 ||
          input$scalebar_pady < 0 ||
          is.na(input$scalebar_pady)
      ) {
        showNotification(
          paste0("Scale bar y-axis padding is outside reasonable boundaries. ",
                 "Resetting to default."),
          type = "error")
        scalebar_pady <- 0.02
      } else {
        scalebar_pady <- input$scalebar_pady
      }

      if (input$scalebar_fontsize > 5 ||
          input$scalebar_fontsize < 0.1 ||
          is.na(input$scalebar_fontsize)
      ) {
        showNotification(
          paste0("Scale bar font size is outside reasonable boundaries. ",
                 "Resetting to default."),
          type = "error")
        scalebar_fontsize <- NULL
      } else {
        scalebar_fontsize <- input$scalebar_fontsize
      }

      map_plot <- map_plot +
        ggspatial::annotation_scale(
          location = input$scalebar_location,
          width_hint = scalebar_size,
          height = unit(scalebar_height, "npc"),
          pad_x = unit(scalebar_padx, "npc"),
          pad_y = unit(scalebar_pady, "npc"),
          bar_cols = c(input$scalebar_colour1, input$scalebar_colour2),
          text_cex = scalebar_fontsize,
          text_face = "bold",
          text_col = input$scalebar_fontcolour,
          style = input$scalebar_style,
          tick_height = scalebar_tickheight
        )
    }

    if (input$add_northarrow == TRUE) {

      # Define north arrow style function
      north_arrow_style_func <- switch(
        input$northarrow_style,
        "nautical" = ggspatial::north_arrow_nautical,
        "minimal" = ggspatial::north_arrow_minimal,
        "orienteering" = ggspatial::north_arrow_orienteering,
        "fancy_orienteering" = ggspatial::north_arrow_fancy_orienteering
      )

      if (input$northarrow_style == "minimal") {
        north_arrow_fill <- input$northarrow_fillcolour1
      } else {
        north_arrow_fill <- c(input$northarrow_fillcolour1,
                              input$northarrow_fillcolour2)
      }

      if (input$northarrow_height > 1 ||
          input$northarrow_height < 0.01 ||
          is.na(input$northarrow_height)
      ) {
        showNotification(
          paste0("North arrow height is outside reasonable boundaries. ",
                 "Resetting to default."),
          type = "error")
        northarrow_height <- 0.1
      } else {
        northarrow_height <- input$northarrow_height
      }

      if (input$northarrow_width > 1 ||
          input$northarrow_width < 0.01 ||
          is.na(input$northarrow_width)
      ) {
        showNotification(
          paste0("North arrow width is outside reasonable boundaries. ",
                 "Resetting to default."),
          type = "error")
        northarrow_width <- 0.1
      } else {
        northarrow_width <- input$northarrow_width
      }

      if (input$northarrow_textsize > 100 ||
          input$northarrow_textsize < 0 ||
          is.na(input$northarrow_textsize)
      ) {
        showNotification(
          paste0("North arrow text size is outside reasonable boundaries. ",
                 "Resetting to default."),
          type = "error")
        northarrow_textsize <- 10
      } else {
        northarrow_textsize <- input$northarrow_textsize
      }

      if (input$northarrow_padx > 0.5 ||
          input$northarrow_padx < 0 ||
          is.na(input$northarrow_padx)
      ) {
        showNotification(
          paste0("Scale bar x-axis padding is outside reasonable boundaries. ",
                 "Resetting to default."),
          type = "error")
        northarrow_padx <- 0
      } else {
        northarrow_padx <- input$northarrow_padx
      }

      if (input$northarrow_pady > 0.5 ||
          input$northarrow_pady < 0 ||
          is.na(input$northarrow_pady)
      ) {
        showNotification(
          paste0("Scale bar y-axis padding is outside reasonable boundaries. ",
                 "Resetting to default."),
          type = "error")
        northarrow_pady <- 0.02
      } else {
        northarrow_pady <- input$northarrow_pady
      }

      map_plot <- map_plot +
        ggspatial::annotation_north_arrow(
          location = input$northarrow_location,
          which_north = "true",
          pad_x = unit(northarrow_padx, "npc"),
          pad_y = unit(northarrow_pady, "npc"),
          height = unit(northarrow_height, "npc"),
          width = unit(northarrow_width, "npc"),
          style = north_arrow_style_func(
             fill = north_arrow_fill,
             line_col = input$northarrow_linecolour,
             text_col = input$northarrow_textcolour,
             text_size = northarrow_textsize
          )
        )
    }

    map_plot
  })

  # Render function for map plot
  render_map <- function() {
    output$plot_map_container <- renderUI({
      plotOutput("plot_map", height = "600px")
    })
    output$plot_map <- renderPlot({
      req(plot_to_print_map())
      plot_to_print_map()
    })
  }

  # Return list of reactives and functions
  list(
    plot_to_print_map = plot_to_print_map,
    render_map = render_map
  )

}

#' Server Time Series Plot
#'
#' @description
#' Handles time series plot generation including input validation, parameter
#' processing, and plot rendering. Creates plot_to_print_ts reactive and
#' output$plot_ts renderPlot.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param r Reactive values object containing dataCube1 and other shared state
#' @param plot_to_render_ts Reactive containing the time series indicator data
#'
#' @return List containing:
#'   - plot_to_print_ts: reactive expression returning the ggplot time series object
#'   - render_ts: function to render the time series output
#'
#' @export
server_timeseries_plot <- function(input, output, session, r, parsed_inputs) {

  # Calculate indicator when plot button is clicked
  plot_to_print_ts <- eventReactive(input$plot_ts_bt, {
    req(r$dataCube1)
    req(input$indicatorsToAnalyse)

    message("DEBUG: Plot time series button clicked, starting calculation...")

    # Calculate the indicator with error handling
    indicator_result <- tryCatch({
      calc_indicator_ts(
        data = r$dataCube1,
        indicator = input$indicatorsToAnalyse,
        cell_size = input$cellsize,
        spatiallevel = input$spatiallevel,
        first_year = input$daterange[1],
        last_year = input$daterange[2],
        countrytype = input$countrytype
      )
    }, error = function(e) {
      message("ERROR in calc_indicator_ts: ", e$message)
      showNotification(paste("Error calculating time series:", e$message), type = "error")
      NULL
    })

    ribboncolour <- if (input$ci_vis_type == "None") NA else input$ribboncolour

    gridlines <- if (input$ts_gridlines == "TRUE") FALSE else TRUE

    if (
      input$title == ""
    ) {
      title <- NULL
    } else {
      title <- input$title
    }

    if (
      is.null(input$ts_x_label) ||
      length(input$ts_x_label) == 0 ||
      input$ts_x_label == ""
    ) {
      xlabel <- NULL
    } else {
      xlabel <- input$ts_x_label
    }

    if (
      is.null(input$ts_y_label) ||
      length(input$ts_y_label) == 0 ||
      input$ts_y_label == ""
    ) {
      ylabel <- NULL
    } else {
      ylabel <- input$ts_y_label
    }

    # Check if subtitle is provided
    if (input$subtitle == "") {
      subtitle <- NULL
    } else {
      subtitle <- input$subtitle
    }

    # Check if caption is provided
    if (input$caption == "") {
      caption <- NULL
    } else {
      caption <- input$caption
    }

    # Check that title, subtitle and caption font size inputs are ok
    if (input$title_size < 8 ||
        input$title_size > 40 ||
        is.na(input$title_size)
    ) {
      showNotification(
        paste0("Title font size is outside reasonable boundaries.",
               "Resetting to default."),
        type = "error")
      title_size <- NULL
    } else {
      title_size <- input$title_size
    }

    if (input$subtitle_size < 6 ||
        input$subtitle_size > 30 ||
        is.na(input$subtitle_size)
    ) {
      showNotification(
        paste0("Subtitle font size is outside reasonable boundaries.",
               "Resetting to default."),
        type = "error")
      subtitle_size <- NULL
    } else {
      subtitle_size <- input$subtitle_size
    }

    if (input$caption_size < 4 ||
        input$caption_size > 20 ||
        is.na(input$caption_size)
    ) {
      showNotification(
        paste0("Caption font size is outside reasonable boundaries. ",
               "Resetting to default."),
        type = "error")
      caption_size <- NULL
    } else {
      caption_size <- input$caption_size
    }

      if (input$title_wrap_length < 20 ||
          input$title_wrap_length > 200 ||
          is.na(input$title_wrap_length)
      ) {
        showNotification(
          paste0("Title wrap length is outside reasonable boundaries. ",
                 "Resetting to default."),
          type = "error")
        title_wrap_length <- NULL
      } else {
        title_wrap_length <- input$title_wrap_length
      }

    if (input$suppress_y == TRUE) {
      y_axis_text <- element_blank()
    } else {
      y_axis_text <- element_text()
    }

    if (input$suppress_yt == TRUE) {
      y_axis_title <- element_blank()
    } else {
      y_axis_title <- element_text()
    }

    if (input$suppress_x == TRUE) {
      x_axis_text <- element_blank()
    } else {
      x_axis_text <- element_text()
    }

    if (input$suppress_xt == TRUE) {
      x_axis_title <- element_blank()
    } else {
      x_axis_title <- element_text()
    }

    if (input$ts_x_breaks > 100 ||
        input$ts_x_breaks < 2 ||
        is.na(input$ts_x_breaks)
    ) {
      showNotification(
        paste0("Number of X axis breaks outside of reasonable range. ",
               "Resetting to default."),
        type = "error"
      )
      ts_x_breaks <- 10
    } else {
      ts_x_breaks <- input$ts_x_breaks
    }

    if (input$ts_y_breaks > 100 ||
        input$ts_y_breaks < 2 ||
        is.na(input$ts_y_breaks)
    ) {
      showNotification(
        paste0("Number of X axis breaks outside of reasonable range. ",
               "Resetting to default."),
        type = "error"
      )
      ts_y_breaks <- 6
    } else {
      ts_y_breaks <- input$ts_y_breaks
    }

    # Confidence intervals for indicator lines or points
    if (input$ci_vis_type == "Ribbon") {
      vis_type <- "ribbon"
    } else if (input$ci_vis_type == "Error Bars") {
      vis_type <- "error_bars"
    } else {
      vis_type <- "error_bars"
      error_alpha <- 0
    }

    error_alpha <- input$error_alpha

    if (input$error_thickness <= 0 ||
        input$error_thickness > 10 ||
        is.na(input$error_thickness)
    ) {
      showNotification(
        paste0("Error bar thickness outside of reasonable range.",
               "Resetting to default."),
        type = "error"
      )
      error_thickness <- 1
    } else {
      error_thickness <- input$error_thickness
    }

    if (input$error_width <= 0 ||
        input$error_width > 10 ||
        is.na(input$error_width)) {
      showNotification(
        paste0("Error bar width outside of reasonable range.",
               "Resetting to default."),
        type = "error"
      )
      error_width <- 1
    } else {
      error_width <- input$error_width
    }

    if (input$linewidth <= 0 ||
        input$linewidth > 10 ||
        is.na(input$linewidth)
    ) {
      showNotification(
        paste0("Indicator line width outside of reasonable range.",
               "Resetting to default."),
        type = "error"
      )
      linewidth <- 1
    } else {
      linewidth <- input$linewidth
    }

    if (input$smooth_linewidth <= 0 ||
        input$smooth_linewidth > 10 ||
        is.na(input$smooth_linewidth)
    ) {
      showNotification(
        paste0("Trend line width outside of reasonable range.",
               "Resetting to default."),
        type = "error"
      )
      smooth_linewidth <- 1
    } else {
      smooth_linewidth <- input$smooth_linewidth
    }

    if (input$smooth_cilinewidth <= 0 ||
        input$smooth_cilinewidth > 10 ||
        is.na(input$smooth_cilinewidth)) {
      showNotification(
        paste0("Trend envelope edge width outside of reasonable range.",
               "Resetting to default."),
        type = "error"
      )
      smooth_cilinewidth <- 1
    } else {
      smooth_cilinewidth <- input$smooth_cilinewidth
    }

    if (input$indicatorsToAnalyse == "Cumulative Species Richness") {
      smoothed_trend <- FALSE
    } else {
      smoothed_trend <- input$smoothed_trend
    }

    params <- list(
      x = indicator_result,
      title = title,
      suppress_y = FALSE,
      smoothed_trend = input$smoothed_trend,
      x_label = xlabel,
      y_label = ylabel,
      x_expand = c(input$ts_x_expand_left, input$ts_x_expand_right),
      y_expand = c(input$ts_y_expand_bottom, input$ts_y_expand_top),
      x_breaks = ts_x_breaks,
      y_breaks = ts_y_breaks,
      gridoff = gridlines,
      ci_type = vis_type,
      point_line = input$point_line,
      pointsize = input$pointsize,
      linewidth = linewidth,
      linecolour = input$linecolour,
      linealpha = input$linealpha,
      error_width = error_width,
      error_thickness = error_thickness,
      error_alpha = error_alpha,
      ribboncolour = ribboncolour,
      ribbonalpha = input$ribbonalpha,
      smooth_linetype = input$smooth_linetype,
      smooth_linewidth = smooth_linewidth,
      trendlinecolour = input$trendlinecolour,
      trendlinealpha = input$trendlinealpha,
      smooth_cilinewidth = smooth_cilinewidth,
      envelopecolour = input$envelopecolour,
      envelopealpha = input$envelopealpha,
      smooth_cialpha = input$smooth_cialpha
    )

    if ((input$indicatorsToAnalyse == "Species Occurrences" ||
         input$indicatorsToAnalyse == "Species Range") &&
        length(input$species) > 0) {
      if (length(input$species) > 1) {
        showNotification(
          paste0(
            "Visualization options for this indicator currently only work ",
            "properly if you select a single species. Hopefully it will be ",
            "fixed soon. For now, please select only one species."
          ), type = "error"
        )
        return(NULL)
      }
      params$species <- c(input$species)
    }

    # Plot diversity metric
    ts_plot <- do.call(plot, params)

    # Add other options to plot
    ts_plot <- ts_plot +
      labs(title = title,
           subtitle = subtitle,
           caption = caption) +
      theme(
        axis.title.x = x_axis_title,
        axis.text.x = x_axis_text,
        axis.title.y = y_axis_title,
        axis.text.y = y_axis_text,
        plot.title = element_text(color = input$title_color,
                                  size = title_size,
                                  face = "bold",
                                  hjust = 0),
        plot.subtitle = element_text(color = input$subtitle_color,
                                     size = subtitle_size),
        plot.caption = element_text(color = input$caption_color,
                                    size = caption_size,
                                    face = "italic")
      )

    ts_plot
  })

  # Render function for time series plot
  render_ts <- function() {
    output$plot_ts_container <- renderUI({
      plotOutput("plot_ts", height = "500px")
    })
    output$plot_ts <- renderPlot({
      req(plot_to_print_ts())
      plot_to_print_ts()
    })
  }

  # Return list of reactives and functions
  list(
    plot_to_print_ts = plot_to_print_ts,
    render_ts = render_ts
  )

}
