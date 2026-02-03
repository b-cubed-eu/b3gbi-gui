# UI Outputs Module
# 
# This file contains all main panel output UI components extracted from app.R
# Functions are designed to be called from the main UI definition
#

#' Create Summary Tab Output UI
#' 
#' @return UI elements for summary tab
summary_tab_output_ui <- function() {
  tagList(
    div(class = "scrollable-tab",
          HTML("<br>"),
          HTML(
            "Here you can view the metadata summarising your data cube."
          ),
          HTML("<br>"),
          HTML("<br>"),
          verbatimTextOutput("metadata"),
          HTML("<br>"),
          HTML("<br>")
    )
  )
}

#' Create Map Tab Output UI
#' 
#' @return UI elements for map tab
map_tab_output_ui <- function() {
  tagList(
    div(class = "scrollable-tab",
          HTML("<br>"),
          HTML(
            "Here you can view your selected biodiversity indicator ",
            "projected onto a map. Use the input filters to select the ",
            "indicator, taxa, geographical area, and temporal window of ",
            "interest, and use the visualization options to change how the ",
            "map looks."
          ),
          HTML("<br>"),
          HTML("<br>"),
          em(
            "(Loading the plot could take a few minutes, depending on the ",
            "options you have selected.)"
          ),
          HTML("<br>"),
          HTML("<br>"),
          actionButton("plot_map_bt", "Plot Map"),
          HTML("<br>"),
          HTML("<br>"),
          uiOutput("plot_map_container"),
          HTML("<br>"),
          p(strong("What am I looking at?")),
          textOutput("figure_legend_map_text"),
          HTML("<br>")
    )
  )
}

#' Create Time Series Tab Output UI
#' 
#' @return UI elements for time series tab
timeseries_tab_output_ui <- function() {
  tagList(
    div(class = "scrollable-tab",
          HTML("<br>"),
          HTML(
            "Here you can view your selected biodiversity indicator ",
            "projected over time. Use the input filters to select the ",
            "indicator, taxa, geographical area, and temporal window of ",
            "interest, and use the visualization options to change how the ",
            "time series looks."
          ),
          HTML("<br>"),
          HTML("<br>"),
          em(
            "(Loading the plot could take a few minutes, depending on the ",
            "options you have selected.)"
          ),
          HTML("<br>"),
          HTML("<br>"),
          actionButton("plot_ts_bt", "Plot Time Series"),
          HTML("<br>"),
          HTML("<br>"),
          uiOutput("plot_ts_container"),
          HTML("<br>"),
          p(strong("What am I looking at?")),
          textOutput("figure_legend_ts_text"),
          HTML("<br>")
    )
  )
}

#' Create Data Table Tab Output UI
#' 
#' @return UI elements for data table tab
datatable_tab_output_ui <- function() {
  tagList(
    div(class = "scrollable-tab",
          HTML("<br>"),
          HTML(
            "Here you can view the data table of your selected indicator."
          ),
          HTML("<br>"),
          HTML("<br>"),
          DTOutput("table")
    )
  )
}
