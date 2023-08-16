#' Create a lookup table to help fix the taxonomy for a list of Australian plant species
#'
#' This function takes a list of Australian plant species that needs to be reconciled with current taxonomy and generates a lookup table to help fix the taxonomy. The lookup table contains the original species names, the aligned species names, and additional taxonomic information such as taxon IDs and genera.
#'
#' @family taxonomic alignment functions
#'
#' @param taxa A list of Australian plant species that needs to be reconciled with current taxonomy.
#' @param stable_or_current_data either "stable" for a consistent version, or "current" for the leading edge version.
#' @param version The version number of the dataset to use.
#' @param one_to_many How to handle one_to_many taxonomic matches.  Default is "return_all".  The other options are "collapse_to_higher_taxon" and "most_likely_species". most_likely_species defaults to the original_name if that name is accepted by the APC; this will be right for certain species subsets, but make errors in other cases, use with caution.
#' @param full logical for whether the full lookup table is returned or just the two key columns
#' @param resources These are the taxonomic resources used for cleaning, this will default to loading them from a local place on your computer.  If this is to be called repeatedly, it's much faster to load the resources using \code{\link{load_taxonomic_resources}} separately and pass the data in.
#' @param APNI_matches Name matches to the APNI (Australian Plant Names Index) are turned off as a default. 
#' @param imprecise_fuzzy_matches Imprecise fuzzy matches are turned off as a default.
#' @param identifier A dataset, location or other identifier, which defaults to NA.
#' @param output file path to save the intermediate output to
#' @return A lookup table containing the original species names, the aligned species names, and additional taxonomic information such as taxon IDs and genera.
#' @details
#' `updated_reason` represents the taxonomic status of the aligned name
#' @export
#'
#' @seealso \code{\link{load_taxonomic_resources}}
#' @examples
#' \donttest{resources <- load_taxonomic_resources()
#' create_taxonomic_update_lookup(c("Eucalyptus regnans",
#'                                  "Acacia melanoxylon",
#'                                  "Banksia integrifolia",
#'                                  "Not a species"),
#'                                  resources=resources)
#'}
create_taxonomic_update_lookup <- function(taxa,
                                           stable_or_current_data = "stable",
                                           version = default_version(),
                                           one_to_many = "return_all",
                                           full = FALSE,
                                           APNI_matches = TRUE, 
                                           imprecise_fuzzy_matches = FALSE, 
                                           identifier = NA_character_,
                                           resources = load_taxonomic_resources(stable_or_current_data =
                                                                                  stable_or_current_data,
                                                                                version = version),
                                           output = NULL) {

  validate_one_to_many_input(one_to_many)

  aligned_data <- 
    align_taxa(taxa, resources = resources, 
               APNI_matches = APNI_matches, 
               identifier = identifier, 
               imprecise_fuzzy_matches = imprecise_fuzzy_matches)

  updated_data <- 
    update_taxonomy(aligned_data$aligned_name, resources = resources, output = output)

  if(one_to_many == "most_likely_species") {
    updated_data <-  
      updated_data %>%
      dplyr::group_by(aligned_name) %>%
      # todo: should this be for all outputs? Move to update_taxonomy
      # take first species, this is most likely, based on ordering determined in update_taxonomy
      dplyr::mutate(
        possible_matches = sprintf("%s (%s)", canonical_name, taxonomic_status_clean) %>% paste(collapse = "; ")
      ) %>%
      # take first record, this is most likely as we've set a preferred order above
      dplyr::slice(1) %>%
      dplyr::ungroup()
  }
  # browser()

  updated_data <-
    # merge with original data on alignment to preserve order
    dplyr::left_join(
      by = "aligned_name",
      aligned_data %>%
        dplyr::select(original_name, aligned_name, aligned_reason, known),
      updated_data %>% filter(!is.na(aligned_name)) %>% distinct()
    ) %>%
    dplyr::mutate(
      # todo - why isn't this source APNI? XXX Lizzy agrees
      taxonomic_reference = ifelse(known & is.na(taxonomic_reference), "known_name_but_not_apc_accepted", taxonomic_reference),
      # todo - do we want to keep this?
      taxonomic_status_clean = ifelse(known & is.na(taxonomic_status_clean), "known_name_but_not_apc_accepted", taxonomic_status_clean)
    ) %>%
    dplyr::select(-known)
  
  # todo - should this be an option here, or an extra function operating on outputs?
  if (one_to_many == "collapse_to_higher_taxon") {
    return(collapse_to_higher_taxon(updated_data, resources))
  }
  
  if (!full) {
    updated_data <-
      updated_data %>%
      dplyr::select(
        original_name,
        aligned_name,
        # todo - why are we renaming these?
        accepted_name = canonical_name,
        taxon_rank,
        author = scientific_name_authorship,
        aligned_reason,
        updated_reason = taxonomic_status_clean
      )
  }

  # todo - should we add file caching here? Or is it enough to have in component functions
  #  - however, results will be incomplete 

  return(updated_data)
}

#' @noRd
validate_one_to_many_input <- function(one_to_many) {
  valid_inputs <-
    c("return_all",
      "collapse_to_higher_taxon",
      "most_likely_species")
  if (!one_to_many %in% valid_inputs)
    stop(
      paste(
        "Invalid input:",
        one_to_many,
        ". Valid inputs are 'return_all', 'collapse_to_higher_taxon', or 'most_likely_species'."
      )
    )
}

#' Currently only collapses to genus or all the way to plants
#' @noRd
collapse_to_higher_taxon <-
  function(aligned_species_list, resources) {
    out <- 
      aligned_species_list %>%
      dplyr::group_by(original_name, aligned_name) %>%
      dplyr::summarise(
        apc_names = find_mrct(canonical_name, resources = resources),
        aligned_reason = paste(unique(aligned_reason), collapse = " and "),
        taxonomic_status = paste(unique(taxonomic_status_clean), collapse = " and "),
        taxonomic_reference = paste(unique(taxonomic_reference), collapse = " and "),
        number_of_collapsed_taxa = n()
      )

    # order same as inputs
    aligned_species_list %>%
      dplyr::select(original_name, aligned_name) %>%
      dplyr::distinct() %>%
      dplyr::left_join(
        by = c("original_name", "aligned_name"),
        out
      )
  }

#' @noRd
find_mrct <- function(taxa,
                      stable_or_current_data = "stable",
                      version = default_version(),
                      resources = load_taxonomic_resources(stable_or_current_data =
                                                             stable_or_current_data,
                                                           version = version)) {
  # Filter the resources data to only include the taxa of interest
  relevant_taxa <-
    dplyr::filter(resources$APC, resources$APC$canonical_name %in% taxa)
  
  # Check different scenarios to find the most recent common taxon
  unique_canonical_names <- unique(relevant_taxa$canonical_name)
  unique_genus_species <-
    unique(stringr::word(unique_canonical_names, 1, 2))
  unique_genus <-
    unique(stringr::word(unique_canonical_names, 1, 1))
  unique_family <- unique(relevant_taxa$family)
  
  if (length(unique_canonical_names) == 1) {
    # All taxa are the same
    return(unique_canonical_names[1])
  } else if (length(unique_genus_species) == 1) {
    # All species are the same, but different subspecific taxa
    return(stringr::word(unique_canonical_names[1], 1, 2))
  } else if (length(unique_genus) == 1) {
    # All genera are the same, but different species
    return(paste0(unique_genus, " sp."))
  } else if (length(unique_family) == 1) {
    # All families are the same, but different genera
    return(unique_family[1])
  } else {
    # Return "plants" for other cases
    return("plants")
  }
}
