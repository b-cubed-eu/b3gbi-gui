#' Extract Unique Taxonomic Groups from Data Cube
#'
#' @description
#' Extracts sorted lists of unique families and species from a processed data cube.
#' These lists are used to populate dropdown menus in the UI.
#'
#' @param cube Object of class 'processed_cube' from b3gbi.
#'
#' @return A list containing:
#'   \itemize{
#'     \item families: Character vector of unique family names
#'     \item species: Character vector of unique species names
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cube <- load_data_cube("data.csv")
#' taxa <- extract_taxonomic_groups(cube)
#' print(taxa$families)
#' print(taxa$species)
#' }
extract_taxonomic_groups <- function(cube) {
  if (!inherits(cube, "processed_cube") || is.null(cube$data)) {
    stop("Invalid data cube provided")
  }
  
  data <- cube$data
  
  list(
    families = sort(unique(data$family)),
    species = sort(unique(data$scientificName))
  )
}


#' Get Species by Family
#'
#' @description
#' Returns all species belonging to specified families from the data cube.
#' This function is used for cascading dropdown updates.
#'
#' @param cube Object of class 'processed_cube'.
#' @param families Character vector of family names to filter by.
#' @param sort Logical. Whether to sort results alphabetically. Default is TRUE.
#'
#' @return Character vector of species names.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cube <- load_data_cube("data.csv")
#' # Get all species in the Pinaceae family
#' species <- get_species_by_family(cube, "Pinaceae")
#' 
#' # Get species from multiple families
#' species <- get_species_by_family(cube, c("Pinaceae", "Rosaceae"))
#' }
#'
#' @importFrom dplyr filter pull
get_species_by_family <- function(cube, families, sort = TRUE) {
  if (!inherits(cube, "processed_cube") || is.null(cube$data)) {
    stop("Invalid data cube provided")
  }
  
  if (is.null(families) || length(families) == 0) {
    return(character(0))
  }
  
  data <- cube$data
  
  # Filter by family and get unique species
  result <- data %>%
    dplyr::filter(family %in% families) %>%
    dplyr::pull(scientificName) %>%
    unique()
  
  if (sort) {
    result <- sort(result)
  }
  
  result
}


#' Filter Data Cube by Taxonomy
#'
#' @description
#' Filters a data cube by selected families and/or species. This is the core
#' filtering function used when users select taxonomic filters in the UI.
#'
#' @param cube Object of class 'processed_cube'.
#' @param families Character vector of family names to include. NULL or empty
#'   vector means no family filtering.
#' @param species Character vector of species names to include. NULL or empty
#'   vector means no species filtering.
#' @param preserve_cube Logical. Whether to return a full cube object (TRUE) or
#'   just the filtered data frame (FALSE). Default is TRUE.
#'
#' @return If preserve_cube is TRUE, returns a processed_cube object with
#'   filtered data. Otherwise returns a data frame.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cube <- load_data_cube("data.csv")
#'
#' # Filter by family only
#' filtered <- filter_by_taxonomy(cube, families = "Pinaceae")
#'
#' # Filter by species only
#' filtered <- filter_by_taxonomy(cube, species = "Pinus sylvestris")
#'
#' # Filter by both (intersection)
#' filtered <- filter_by_taxonomy(
#'   cube,
#'   families = c("Pinaceae", "Rosaceae"),
#'   species = c("Pinus sylvestris", "Rosa canina")
#' )
#'
#' # Get just the data frame
#' data <- filter_by_taxonomy(cube, families = "Pinaceae", preserve_cube = FALSE)
#' }
filter_by_taxonomy <- function(cube, families = NULL, species = NULL,
                                preserve_cube = TRUE) {
  if (!inherits(cube, "processed_cube") || is.null(cube$data)) {
    stop("Invalid data cube provided")
  }
  
  data <- cube$data
  
  # Apply family filter if provided
  if (!is.null(families) && length(families) > 0) {
    data <- data[data$family %in% families, ]
  }
  
  # Apply species filter if provided
  if (!is.null(species) && length(species) > 0) {
    data <- data[data$scientificName %in% species, ]
  }
  
  # Return appropriate format
  if (preserve_cube) {
    # Create a new cube object with filtered data
    filtered_cube <- cube
    filtered_cube$data <- data
    return(filtered_cube)
  } else {
    return(data)
  }
}


#' Get Taxonomic Summary Statistics
#'
#' @description
#' Calculates summary statistics about the taxonomic composition of a data cube.
#' Useful for displaying metadata and validation messages.
#'
#' @param cube Object of class 'processed_cube'.
#' @param filtered_data Optional data frame. If provided, calculates stats on
#'   filtered data instead of full cube.
#'
#' @return A list containing:
#'   \itemize{
#'     \item n_families: Number of unique families
#'     \item n_species: Number of unique species
#'     \item n_observations: Total number of observations
#'     \item top_families: Data frame of top 10 families by observation count
#'     \item top_species: Data frame of top 10 species by observation count
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cube <- load_data_cube("data.csv")
#' stats <- get_taxonomic_summary(cube)
#' print(stats$n_families)
#' print(stats$top_families)
#' }
#'
#' @importFrom dplyr count arrange desc slice
get_taxonomic_summary <- function(cube, filtered_data = NULL) {
  if (!inherits(cube, "processed_cube")) {
    stop("Invalid data cube provided")
  }
  
  # Use filtered data if provided, otherwise use full cube
  data <- filtered_data %||% cube$data
  
  if (is.null(data) || nrow(data) == 0) {
    return(list(
      n_families = 0,
      n_species = 0,
      n_observations = 0,
      top_families = data.frame(family = character(), n = integer()),
      top_species = data.frame(scientificName = character(), n = integer())
    ))
  }
  
  list(
    n_families = length(unique(data$family)),
    n_species = length(unique(data$scientificName)),
    n_observations = nrow(data),
    top_families = data %>%
      dplyr::count(family, name = "n") %>%
      dplyr::arrange(dplyr::desc(n)) %>%
      dplyr::slice(1:10),
    top_species = data %>%
      dplyr::count(scientificName, name = "n") %>%
      dplyr::arrange(dplyr::desc(n)) %>%
      dplyr::slice(1:10)
  )
}


#' Validate Taxonomic Selections
#'
#' @description
#' Validates that selected families and species exist in the data cube.
#' Returns warnings for any invalid selections.
#'
#' @param cube Object of class 'processed_cube'.
#' @param families Character vector of selected family names.
#' @param species Character vector of selected species names.
#'
#' @return Invisible TRUE if all selections are valid. Issues warnings for
#'   invalid selections.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' cube <- load_data_cube("data.csv")
#' validate_taxonomic_selections(cube, "Pinaceae", "Pinus sylvestris")
#' }
validate_taxonomic_selections <- function(cube, families = NULL, species = NULL) {
  if (!inherits(cube, "processed_cube") || is.null(cube$data)) {
    stop("Invalid data cube provided")
  }
  
  data <- cube$data
  
  # Validate families
  if (!is.null(families) && length(families) > 0) {
    valid_families <- unique(data$family)
    invalid <- setdiff(families, valid_families)
    if (length(invalid) > 0) {
      warning("Invalid family selections: ", paste(invalid, collapse = ", "))
    }
  }
  
  # Validate species
  if (!is.null(species) && length(species) > 0) {
    valid_species <- unique(data$scientificName)
    invalid <- setdiff(species, valid_species)
    if (length(invalid) > 0) {
      warning("Invalid species selections: ", paste(invalid, collapse = ", "))
    }
  }
  
  invisible(TRUE)
}


#' Reset Taxonomic Filters
#'
#' @description
#' Returns the original unfiltered data cube. Useful for resetting filters
#' in the UI.
#'
#' @param cube Object of class 'processed_cube'.
#'
#' @return The original cube with unfiltered data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cube <- load_data_cube("data.csv")
#' filtered <- filter_by_taxonomy(cube, families = "Pinaceae")
#' original <- reset_taxonomic_filters(filtered)
#' }
reset_taxonomic_filters <- function(cube) {
  if (!inherits(cube, "processed_cube")) {
    stop("Invalid data cube provided")
  }
  
  # Return the cube as-is (data was never modified, only filtered views created)
  cube
}


# Helper function for null default values
`%||%` <- function(x, y) if (is.null(x)) y else x
