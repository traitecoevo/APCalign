#' Process geographic data and return state level species origin and diversity counts
#'
#' This function processes the geographic data available in the current or any version of the Australian Plant Census and returns state level diversity for native, introduced and more complicated species origins.
#'
#'
#' @family diversity methods
#' @param resources the taxonomic resources required to make the summary statistics.  Loading this can be slow, so call load_taxonomic_resources separately to greatly speed this function up and pass the resources in.
#'
#' @return A data frame with columns representing each state and rows representing each species. The values in each cell represent the origin of the species in that state.
#'
#' @import dplyr
#' @import stringr
#' @export
#'
#' @seealso \code{\link{load_taxonomic_resources}}
#'
#' @examples
#' create_species_state_origin_matrix()
#'
#'
#'
create_species_state_origin_matrix <- function(resources = load_taxonomic_resources()) {
  apc_species <- filter_data_to_accepted_species(resources)
  sep_state_data <- separate_states(apc_species)
  apc_places <- identify_places(sep_state_data)
  species_df <- create_species_df(apc_places, apc_species)
  result_df <- parse_states(species_df, apc_places, apc_species)
  return(result_df)
}

#' @noRd
filter_data_to_accepted_species <- function(resources) {
  dplyr::filter(resources$APC,
                taxonRank == "Species" &
                  taxonomicStatus == "accepted")
}

#' @noRd
separate_states <- function(data) {
  stringr::str_split(unique(data$taxonDistribution), ",")
}

#' @noRd
identify_places <- function(sep_state_data) {
  all_codes <- unique(stringr::str_trim(unlist(sep_state_data)))
  unique(stringr::word(all_codes[!is.na(all_codes)], 1, 1))
}

#' @noRd
create_species_df <- function(apc_places, apc_species) {
  species_df <- dplyr::tibble(species = apc_species$scientificName)
  for (i in 1:length(apc_places)) {
    species_df <- dplyr::bind_cols(species_df, NA, .name_repair = "minimal")
  }
  names(species_df) <- c("species", apc_places)
  return(species_df)
}

#' @noRd
state_parse_and_add_column <- function(species_df, state, apc_species) {
  species_df[, state] <- dplyr::case_when(
    grepl(paste0("\\b", state, " \\(uncertain origin\\)"), apc_species$taxonDistribution) ~ "uncertain origin",
    grepl(paste0("\\b", state, " \\(naturalised\\)"), apc_species$taxonDistribution) ~ "naturalised",
    grepl(paste0("\\b", state, " \\(doubtfully naturalised\\)"), apc_species$taxonDistribution) ~ "doubtfully naturalised",
    grepl(paste0("\\b", state, " \\(native and naturalised\\)"), apc_species$taxonDistribution) ~ "native and naturalised",
    grepl(paste0("\\b", state, " \\(formerly naturalised\\)"), apc_species$taxonDistribution) ~ "formerly naturalised",
    grepl(paste0("\\b", state, " \\(presumed extinct\\)"), apc_species$taxonDistribution) ~ "presumed extinct",
    grepl(paste0("\\b", state, " \\(native and doubtfully naturalised\\)"), apc_species$taxonDistribution) ~ "native and doubtfully naturalised",
    grepl(paste0("\\b", state, " \\(native and uncertain origin\\)"), apc_species$taxonDistribution) ~ "native and uncertain origin",
    grepl(paste0("\\b", state), apc_species$taxonDistribution) ~ "native",
    TRUE ~ "not present"
  )
  return(species_df)
}

#' @noRd
parse_states <- function(species_df, apc_places, apc_species) {
  for (i in 1:length(apc_places)) {
    species_df <- state_parse_and_add_column(species_df, apc_places[i], apc_species)
  }
  return(species_df)
}