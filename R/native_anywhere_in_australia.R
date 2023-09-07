#' Check if a vector of species are native anywhere in Australia
#'
#' This function checks if the given species is native anywhere in Australia according to the loaded version of the Australian Plant Census (APC).
#' It creates a lookup table from taxonomic resources, and checks if the species
#' is listed as native in that table. Note that this will not detect within Australia invasions,
#' e.g. if a species is from Western Australia and is invasive on the east coast.  And recent invasions are unlikely to be documented yet in APC. 
#' For the complete matrix of species by states that also represents within-Australia invasions,
#' use \link{create_species_state_origin_matrix}.  For spelling checks and taxonomy updates please see \link{create_taxonomic_update_lookup}.
#'
#' @family diversity methods
#' @param species A character string typically representing the binomial for the species.  
#' @param resources An optional list of taxonomic resources to use for the lookup.
#'        If not provided, the function will load default taxonomic resources using the `load_taxonomic_resources()` function.
#' @return A tibble with two columns: `species`, which is the same as the unique values of the input `species`,
#'         and `native_anywhere_in_aus`, a vector indicating whether each species is native anywhere in Australia, introduced by humans from elsewhere, or unknown with respect to the APC resource.
#' @export
#' @examples
#' \donttest{native_anywhere_in_australia(c("Eucalyptus globulus","Pinus radiata","Banksis notaspecies"))}

native_anywhere_in_australia <- function(species, resources = load_taxonomic_resources()) {
  
  # Remove duplicates and replace hyphens
  species <- unique(species)
  species_modified <- gsub('-', ' ', species)
  
  # Count words and issue warnings for non-binomial names
  word_counts <- stringi::stri_count_words(species_modified)
  if (any(word_counts != 2)) {
    warning("All input species not binomials; \n this function is designed to work primarily with species-level binomials.")
  }
  
  # Create lookup tables
  full_lookup <- create_species_state_origin_matrix(resources = resources)
  
  # Filter for native species
  full_lookup$native_anywhere <-
    apply(full_lookup, 1, function(x)
      any(grepl("native", x)))
  native_only<-dplyr::filter(full_lookup,native_anywhere)
  
  # Get the first two words (assuming these are the binomial names)
  natives_binomials <- stringr::word(native_only$species, 1, 2)
  full_binomials <- stringr::word(full_lookup$species, 1, 2)
  species_binomials <- stringr::word(species, 1, 2)
  
  # Check membership
  natives <- species_binomials %in% natives_binomials
  fulllist <- species_binomials %in% full_binomials
  
  # Create output tibble
  result <- tibble(
    species = species,
    native_anywhere_in_aus = dplyr::case_when(
      natives & fulllist ~ "native",
      fulllist ~ "introduced",
      TRUE ~ "unknown"
    )
  )
  
  return(result)
}

