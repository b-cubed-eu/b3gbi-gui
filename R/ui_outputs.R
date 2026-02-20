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
          style = "color: #333;",
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
          style = "color: #333;",
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
          HTML("<br>"),
          p(strong("Background information on this indicator:")),
          uiOutput("indicator_background_text_map"),
          HTML("<br>"),
          fluidRow(
            column(
              selectInput(
                inputId = "downloadOptions_map",
                label = "Download Formats",
                choices = c(
                  "JPEG",
                  "PDF",
                  "PNG",
                  "SVG",
                  "TIFF"
                )
              ),
              width = 6
            ),
            column(
              downloadButton("downloadGo_map"),
              width = 4,
              style = "padding:18px;"
            )
          ),
          fluidRow(
            column(
              numericInput(
                "dlmap_width",
                "Map Width (in pixels)",
                min = 100,
                max = 10000,
                step = 100,
                value = 2000
              ),
              width = 3
            ),
            column(
              numericInput(
                "dlmap_height",
                "Map Height (in pixels)",
                min = 100,
                max = 10000,
                step = 100,
                value = 2000
              ),
              width = 3
            ),
            column(
              numericInput(
                "dlmap_scaling",
                "Scaling Factor",
                min = 0.1,
                max = 5,
                step = 0.1,
                value = 1
              ),
              width = 2
            )
          ),
          em(
            "Note: if there is extra white space in the downloaded image, try ",
            "adjusting the width or height. If the text and legend are too ",
            "large or too small, try adjusting the scaling factor (only ",
            "valid for JPG, PNG and TIFF, as other formats do not use the ",
            "scaling parameter)."
          )
    )
  )
}

#' Create Time Series Tab Output UI
#' 
#' @return UI elements for time series tab
timeseries_tab_output_ui <- function() {
  tagList(
    div(class = "scrollable-tab",
          style = "color: #333;",
          HTML("<br>"),
          HTML(
            "Here you can view a time-series plot of your selected ",
            "biodiversity indicator. Use the input filters to select the ",
            "indicator, taxa, geographical area, and temporal window of ",
            "interest, and use the visualization options to control how the plot ",
            "looks."
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
          HTML("<br>"),
          p(strong("Background information on this indicator:")),
          uiOutput("indicator_background_text_ts"),
          HTML("<br>"),
          fluidRow(
            column(
              selectInput("downloadOptions_ts",
                          "Download Formats",
                          choices = c(
                            "JPEG",
                            "PDF",
                            "PNG",
                            "SVG",
                            "TIFF"
                          )
              ),
              width = 6
            ),
            column(
              downloadButton("downloadGo_ts"),
              width = 4,
              style = "padding:18px;"
            )
          ),
          fluidRow(
            column(
              numericInput(
                "dlts_width",
                "Plot Width (in pixels)",
                min = 100,
                max = 10000,
                step = 100,
                value = 2000
              ),
              width = 3
            ),
            column(
              numericInput(
                "dlts_height",
                "Plot Height (in pixels)",
                min = 100,
                max = 10000,
                step = 100,
                value = 2000
              ),
              width = 3
            ),
            column(
              numericInput(
                "dlts_scaling",
                "Scaling Factor",
                min = 0.1,
                max = 5,
                step = 0.1,
                value = 1
              ),
              width = 2
            )
          ),
          em(
            "Note: If the text and legend are too large or too small, try ",
            "adjusting the scaling factor (only valid for JPG, PNG and TIFF, ",
            "as other formats do not use the scaling parameter)."
          )
    )
  )
}

#' Create Data Table Tab Output UI
#' 
#' @return UI elements for data table tab
datatable_tab_output_ui <- function() {
  tagList(
    div(class = "scrollable-tab",
          style = "color: #333;",
          HTML("<br>"),
          HTML("Here you can view your processed data cube as a table."),
          HTML("<br>"),
          HTML("<br>"),
          em(
            "(Note that the various export options will only copy the ",
            "data from the current page of the table. To export ",
            "everything, first select 'all' in the 'Show' dropdown menu.)"
          ),
          HTML("<br>"),
          HTML("<br>"),
          div(
            style = "overflow-x: auto;",
            dataTableOutput("table")
          )
    )
  )
}

#' Create Export Tab Output UI
#' 
#' @return UI elements for export tab
export_tab_output_ui <- function() {
  tagList(
    div(class = "scrollable-tab",
          style = "color: #333;",
          HTML("<br>"),
          HTML("<div>Download your processed data in .csv format.</div>"),
          HTML("<br>"),
          downloadButton("downloadProcessedCube",
                         label = "Processed Cube"
          ),
          downloadButton("downloadMappedCube",
                         label = "Mapped Cube"
          ),
          downloadButton("downloadTimeSeriesData",
                         label = "Time Series Data"
          )
    )
  )
}

#' Create Background Tab Output UI
#' 
#' @return UI elements for background tab
background_tab_output_ui <- function() {
  tagList(
    div(class = "scrollable-tab",
          style = "color: #333;",
          HTML("<br>"),
          HTML(
            "Here you can view technical information on the available ",
            "biodiversity indicators."),
          h3("Biodiversity Indicators"),
          HTML("<br>"),
          em("Occurrences"),
          p(strong("Total Occurrences")),
          p(
            "The total number of occurrences is calculated by summing the ",
            "occurrences of all species observed for each cell or year. This ",
            "variable provides an overview of the comprehensiveness and ",
            "distribution of data in the cube being analyzed, and may be ",
            "helpful, or even vital, for interpreting the results of ",
            "calculated indicators."
          ),
          p(strong("Density of Occurrences")),
          p(
            "Density is calculated by summing the total number of occurrences ",
            "per square km for each cell or year. This provides similar ",
            "information to total occurrences, but is adjusted for cell area."
          ),
          HTML("<br>"),
          em("Species Richness"),
          p(
            "Species richness is the total number of species present in a ",
            "sample (Magurran, 1988). It is a fundamental and commonly used ",
            "measure of biodiversity, providing a simple and intuitive ",
            "overview of the status of biodiversity. However, richness is not ",
            "well suited to measuring biodiversity change over time, as it ",
            "only decreases when local extinctions occur and thus lags behind ",
            "abundance for negative trends. While it may act as a leading ",
            "indicator of alien species invasions, it will not indicate ",
            "establishment because it ignores abundance. Nor will it ",
            "necessarily indicate changes in local species composition, which ",
            "can occur without any change in richness. Although richness is ",
            "conceptually simple, it can be measured in different ways."
          ),
          p(strong("Observed Richness")),
          p(
            "Observed richness is calculated by summing the number of unique ",
            "species observed for each year or each cell. Observed richness is ",
            "highly dependent on the comprehensiveness of the dataset it is being ",
            "applied to. If some regions are more intensively, carefully, or ",
            "systematically sampled than others, this will likely result in higher ",
            "observed richness. Observed richness also depends on the relative ",
            "abundance and spatial aggregation of each species, with less abundant ",
            "and less aggregated species less likely to be discovered during surveys ",
            "(Hillebrand et al., 2018), as well as the detectability of each species."
          ),
          p(strong("Cumulative Richness")),
          p(
            "Cumulative richness is calculated by adding the newly observed ",
            "unique species each year to a cumulative sum. This indicator ",
            "provides an estimation of whether and how many new species are ",
            "still being discovered in a region. While an influx of alien ",
            "species could cause an increase in cumulative richness, a fast-",
            "rising trend is likely an indication that the ",
            "dataset is not comprehensive and therefore observed richness ",
            "will provide an underestimate of species richness."
          ),
          HTML("<br>"),
          em("Evenness"),
          p(
            "Species evenness is a commonly used indicator that measures how ",
            "uniformly individuals are distributed across species in a region ",
            "or over time. It provides a complement to richness by taking ",
            "relative abundance into account. Although GBIF provides ",
            "information about abundances as individual counts, the majority ",
            "of entries lack this information. Hence, evenness can only be ",
            "calculated using the proportions of observations rather than ",
            "proportions of individuals. Strictly speaking, the evenness ",
            "measures therefore indicate how uniformly species are ",
            "represented in the respective data set rather than the true ",
            "evenness of the ecological community."
          ),
          p(strong("Pielou's Evenness")),
          p(
            "Pielou's evenness (1966) is a well-known and commonly ",
            "used evenness measure. It is calculated as: E = -sum(p_i * ln(p_i)) / ln(S), ",
            "where S is the number of species and pi is the proportion of occurrences ",
            "represented by species i."
          ),
          p(strong("Williams' Evenness")),
          p(
            "An analysis of evenness properties by Kvalseth (2015) showed ",
            "that an evenness index introduced by Williams in 1977 in an ",
            "unpublished manuscript has two important properties which ",
            "Pielou's does not. The properties in question are complex ",
            "mathematical properties known as the Schur-Concavity and ",
            "value validity, but we attempt to describe them here more ",
            "simply. If a measure of evenness is Schur-concave, it means ",
            "that when the distribution of individuals becomes more evenly ",
            "spread across species, the measure of evenness will stay the ",
            "same or increase, but never decrease. Value validity means ",
            "that an evenness index should provide sensible and meaningful ",
            "values across its range for any given distribution of species ",
            "abundances. Kvalseth referred to this evenness measure as E9 ",
            "but we refer to it as Williams' evenness."
          ),
          HTML("<br>"),
          em("Hill Diversity"),
          p(
            "Hill (1973) introduced the concept of Hill diversity, which assumes ",
            "that the number and relative abundance of species are inseparable ",
            "components of diversity. Hill diversity uses a single equation to ",
            "calculate multiple measures of diversity by varying a single ",
            "parameter, which changes the emphasis on rare vs common species ",
            "(Roswell et al., 2019). It represents the mean rarity of sampled ",
            "species. While the parameter can theoretically take almost any value, ",
            "three common measures of diversity are special cases: species richness, ",
            "and modified versions of the Shannon and Simpson diversity indices. ",
            "These three measures occur when the parameter takes the value of 1, ",
            "0 (or near-zero, as it cannot actually take the value of 0), or -1, ",
            "respectively. Richness uses an arithmetic scale (the arithmetic mean), ",
            "thus giving rare species a lot of leverage. By contrast, Hill-Shannon diversity ",
            "uses a logarithmic scale (the geometric mean), treating common and ",
            "rare species equally, and Hill-Simpson diversity uses a reciprocal ",
            "scale (the harmonic mean), giving common species higher leverage."
          ),
          p(strong("Species Richness (Hill Number 0)")),
          p(
            "Using the Hill diversity equation, richness becomes simply S, ",
            "the number of species, and is thus identical to richness ",
            "calculated without Hill diversity."
          ),
          p(strong("Hill-Shannon Diversity (Hill Number 1)")),
          p(
            "Hill-Shannon diversity is actually e (base of the natural log) raised ",
            "to the power of the Shannon index. It is estimated for each year or ",
            "cell count using the iNEXT package, standardized by coverage."
          ),
          p(strong("Hill-Simpson Diversity (Hill Number 2)")),
          p(
            "Hill-Simpson diversity is the inverse of the Simpson index. ",
            "It is estimated using the iNEXT package for each year or cell, ",
            "standardized by coverage. Both Hill-Simpson and Hill-Shannon diversity ",
            "describe a combination of richness and evenness that reduce the ",
            "inadequacies of either measure alone."
          ),
          HTML("<br>"),
          em("Rarity"),
          p(
            "Rarity is the scarcity or infrequency of a particular species in ",
            "an area. A rare species might have a small population size, a ",
            "limited distribution, or a unique ecological niche (Maciel, ",
            "2021; Rabinowitz, 1981). Rarity can also be a biodiversity ",
            "indicator when summed over multiple species in an area, and may ",
            "provide important insight for determining conservation ",
            "priorities. When measured over time, rarity may indicate ",
            "potential threats or changes in the environment."
          ),
          p(strong("Abundance-Based Rarity")),
          p(
            "Abundance-based rarity is the inverse of the proportion of total occurrences ",
            "represented by a particular species. The total summed rarity for each grid ",
            "cell or year is calculated (sum the rarity values of each species present ",
            "there). It is calculated as: sum(1/p_i) where S is the number of species ",
            "and pi is the proportion of occurrences represented by species i."
          ),
          p(strong("Area-Based Rarity")),
          p(
            "Area-based rarity is the inverse of occupancy frequency (proportion of grid ",
            "cells occupied) for a particular species. The total summed rarity for each ",
            "grid cell or year is calculated (sum the rarity values of each species ",
            "present there). It is calculated as: sum(N/n_i) where S is the number of species, ",
            "N is the total number of occupied grid cells, and ni is the number of grid ",
            "cells occupied by species i."
          ),
          HTML("<br>"),
          p(strong("Mean Year of Occurrence")),
          p(
            "The mean year of occurrence is calculated per cell, giving an ",
            "indication of how recent the data is for each cell. A recent ",
            "mean year is not necessarily an indication of quality, as some ",
            "countries or regions have been conducting comprehensive ",
            "biodiversity monitoring for many years and will therefore ",
            "reflect an older mean year of occurrence, while others may show ",
            "a recent mean year due to e.g., the sudden availability of large ",
            "amounts of citizen science data."
          ),
          HTML("<br>"),
          p(strong("Taxonomic Distinctness")),
          p(
            "Taxonomic distinctness measures the taxonomic relatedness between ",
            "species, providing a measure of biodiversity that accounts for ",
            "evolutionary relationships. A distance matrix based on pairwise ",
            "taxonomic relationships is calculated for each cell using the taxize ",
            "package (Chamberlain & Szocs, 2013; Chamberlain et al., 2020), then ",
            "taxonomic distinctness is calculated as the Taxonomic Distinctness ",
            "Index (TDI; Clarke & Warwick, 1999)."
          ),
          HTML("<br>"),
          p(strong("Species Occurrences")),
          p(
            "Species occurrences are considered an essential biodiversity variable ",
            "(EBV). They are mapped by calculating the total number of occurrences ",
            "of a given species for each cell. This represents the occurrence ",
            "frequency distribution, and also indicates the observed species ",
            "distribution. The number of occurrences can act as a proxy for relative ",
            "abundance of species with a similar detectability, which is an ",
            "important aspect of biodiversity although not an indicator when ",
            "calculated in isolation."
          ),
          HTML("<br>"),
          p(strong("Species Range")),
          p(
            "Species range is the area over which a species is found.",
            "Plotting it on a map results in coloured cells where the ",
            "species was observed. As a time series it is calculated by summing the ",
            "number of cells in which a species was observed each year. Species ",
            "range is an important indicator of species distribution and can ",
            "provide insights into the spatial extent of species occurrences. It ",
            "can be used to assess the geographic distribution of species and ",
            "evaluate the effectiveness of conservation strategies."
          ),
          HTML("<br>"),
          p(strong("Occupancy Turnover")),
          p(
            "Occupancy turnover measures the change in species composition over ",
            "time, reflecting the rate at which species appear or disappear from a ",
            "given area. It provides insights into the dynamic nature of ",
            "ecological communities, highlighting shifts in species distributions ",
            "and potential environmental changes. High turnover rates may indicate ",
            "rapid community restructuring, potentially driven by factors such as ",
            "habitat alteration, climate change, or invasive species. Analyzing ",
            "occupancy turnover can be crucial for understanding ecosystem ",
            "stability, identifying areas of conservation concern, and assessing ",
            "the effectiveness of management strategies."
          )
    )
  )
}

#' Create References Tab Output UI
#' 
#' @return UI elements for references tab
references_tab_output_ui <- function() {
  tagList(
    div(class = "scrollable-tab",
          style = "color: #333;",
          "References used in this app:",
          HTML(
            "<br><br>",
            "<p>Chamberlain, S., & Szocs, E. (2013). taxize: taxonomic ",
            "search and retrieval in R. <i>F1000Research</i>, 2. ",
            "<a href = 'https://doi.org/10.12688/f1000research.2-191.v2'",
            "target = 'new'>https://doi.org/10.12688/f1000research.2-191",
            ".v2</a>",
            "<p>Chamberlain, S., Szocs, E., Foster, Z., Boettiger, C., ",
            "Ram, K., Bartomeus, I., Baumgartner, J., O'Donnell, J., ",
            "Oksanen, J., Tzovaras, B.G., Marchand, P., Tran, V., Salmon",
            ", M., Li, G., & Grenie, M. (2020). taxize: Taxonomic ",
            "Information from Around the Web. R package version 0.9.98. ",
            "<a href = 'https://github.com/ropensci/taxize' target = ",
            "'new'>https://github.com/ropensci/taxize</a></p>",
            "<p>Clarke, K.R., & Warwick, R.M. (1999). The taxonomic ",
            "distinctness measure of biodiversity: weighting of step ",
            "lengths between hierarchical levels. Marine Ecology ",
            "Progress Series, 184, 21-29.</p>",
            "<p>Hill, M.O. (1973). Diversity and evenness: a unifying ",
            "notation and its consequences. <i>Ecology, 54</i>(2), ",
            "427-432.</p>",
            "<p>Hillebrand, H., Blasius, B., Borer, E.T., Chase, J.M., ",
            "Downing, J.A., Eriksson, B.K., Filstrup, C.T., Harpole, ",
            "W.S., Hodapp, D., Larsen, S., Lewandowska, A.M., Seabloom, ",
            "E.W., Van de Waal, D.B., & Ryabov, A.B. (2018). ",
            "Biodiversity change is uncoupled from species richness ",
            "trends: consequences for conservation and monitoring. ",
            "<i>Journal of Applied Ecology, 55</i>(1), 169-184.</p>",
            "<p>Jaccard, P. (1901). Etude Comparative de la ",
            "distribution florale dans une portion des Alpes et des ",
            "Jura. <i>Bulletin de la Societe Vaudoise des Sciences ",
            "Naturelles</i>, 37, 547-579.</p>",
            "<p>Kvalseth, T.O. (2015). Evenness indices once again: ",
            "critical analysis of properties. <i>SpringerPlus, 4</i>(1)",
            ", 1-12.</p>",
            "<p>Maciel, E.A. (2021). An index for assessing the rare ",
            "species of a community. <i>Ecological Indicators</i>, 124, ",
            "107424.</p>",
            "<p>Magurran, A.E. (1988). <i>Ecological diversity and its ",
            "measurement.</i> Princeton University Press.</p>",
            "<p>Pielou, E.C. (1966). The measurement of diversity in ",
            "different types of biological collections. <i>Journal of ",
            "Theoretical Biology</i>, 13, 131-144.</p>",
            "<p>Rabinowitz, D. (1981). Seven forms of rarity. In: ",
            "Synge, H. (Ed.). <i>The biological aspects of rare plant ",
            "conservation.</i> Chichester: John Wiley & Sons, 205-217.",
            "</p>",
            "<p>Roswell, M., Dushoff, J., & Winfree, R. (2021). A ",
            "conceptual guide to measuring species diversity. <i>Oikos, ",
            "130</i>(3), 321-338.</p><br>"
          )
    )
  )
}

#' Create About Tab Output UI
#' 
#' @return UI elements for about tab
about_tab_output_ui <- function() {
  tagList(
    div(class = "scrollable-tab",
          style = "color: #333;",
          HTML("<br>"),
          HTML(
            paste0(
              "This Shiny app was developed by: <br><br>",
              "Shawn Dove <br>",
              "Yanina Sica <br>",
              "Lissa Breugelmans <br>",
              "Melanie De Nolf <br>",
              "Arvin C. Diesmos <br>",
              "Mathias Dillen <br>",
              "Fabio Matos <br>",
              "Arman Pili <br>",
              "<br>",
              "The app is a graphical front end for the b3gbi R package, ",
              "an output of the B-cubed project.",
              "<br><br>",
              "<div>For more information about the b3gbi R package ",
              "please visit the ",
              "<a href='https://github.com/b-cubed-eu/b3gbi/' ",
              "style='color: blue; text-decoration: none;'> ",
              "GitHub page</a> or the ",
              "<a href='https://b-cubed-eu.r-universe.dev/b3gbi' ",
              "style='color: blue; text-decoration: none;'> ",
              "R Universe page </a>",
              "or the <a href='https://ec.europa.eu/info/funding-tenders/",
              "opportunities/portal/screen/opportunities/horizon-results-",
              "platform/83298' style='color: blue; text-decoration: ",
              "none;'> EU Horizon Results Platform page</a>.",
              "<br><br>",
              "For more information about the B-Cubed project ",
              "please visit the <a href='https://b-cubed.eu/' ",
              "style='color: blue; text-decoration: none;'>B-Cubed ",
              "website</a>.</div>",
              "<br>",
              "To cite this app:<br><br>",
              "Dove S, Breugelmans L, De Nolf M, Diesmos A, ",
              "Dillen M, Matos F, Pili A & Sica Y (2025). b3gbi Shiny GUI: ",
              "A graphical user interface for calculating general ",
              "biodiversity indicators on GBIF data cubes. [R Shiny ",
              "Application] ",
              "https://bcc9vd-shawn-dove.shinyapps.io/b3gbi-gui/ (Original ",
              "work published 2025)",
              "<br><br>",
              "B-Cubed (Biodiversity Building Blocks for Policy) receives ",
              "funding from the European Union's Horizon Europe Research ",
              "and Innovation Programme (ID No 101059592).",
              "<br><br>",
              "This app is licensed under the MIT License.</div>"
            )
          ),
          HTML("<br>")
    )
  )
}
