#' Check if a vector of species are native anywhere in Australia
#'
#' This function checks if the given species is native anywhere in Australia.
#' It creates a lookup table from taxonomic resources, and checks if the species
#' is listed as native in that table. Note that this will not detect within Australia invasives,
#' e.g. if a species is from Western Australia and is invasive on the east coast.
#' For the complete matrix of species by states that also represents within-Australia invasions,
#' use \link{create_species_state_origin_matrix}.
#'
#' @family diversity methods
#' @param species A character string representing the binomial or common name of the species.
#' @param resources An optional list of taxonomic resources to use for the lookup.
#'        If not provided, the function will load default taxonomic resources using the `load_taxonomic_resources()` function.
#' @return A tibble with two columns: `species`, which is the same as the input `species`,
#'         and `native_anywhere_in_aus`, a logical vector indicating whether each species is native anywhere in Australia.
#' @export
#' @examples
#' 
#' native_anywhere_in_australia(c("Eucalyptus globulus","Pinus radiata"), resources=resources)

native_anywhere_in_australia <-
  function(species, resources = load_taxonomic_resources()) {
    native_lookup <-
      create_species_state_origin_matrix(resources = resources)
    native_lookup$native_anywhere <-
      apply(native_lookup, 1, function(x)
        any(grepl("native", x)))
    native_only <- filter(native_lookup, native_anywhere == TRUE) %>%
      mutate(binomial = word(species, 1, 2))
    return(
      tibble(
        species = species,
        native_anywhere_in_aus = species %in% native_only$binomial |
          species %in% native_only$species
      )
    )
  }
