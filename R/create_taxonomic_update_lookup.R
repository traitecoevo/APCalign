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
                                           resources = load_taxonomic_resources(stable_or_current_data =
                                                                                  stable_or_current_data,
                                                                                version = version),
                                           output = NULL) {
  validate_one_to_many_input(one_to_many)
  aligned_data <- align_taxa(taxa, resources = resources)
  updated_species_list <-
    get_updated_species_list(aligned_data, resources, one_to_many, output)
  
  if (one_to_many == "collapse_to_higher_taxon") {
    return(collapse_to_higher_taxon(updated_species_list, resources))
  }
  
  if (full == TRUE) {
    return(updated_species_list)
  } else {
    return(
      dplyr::select(
        updated_species_list,
        original_name,
        aligned_name,
        accepted_name = canonical_name,
        taxon_rank = taxonRank,
        author = scientificNameAuthorship,
        aligned_reason,
        updated_reason = taxonomicStatusClean
      ) %>%
        distinct()
    )
  }
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


#' Wrapper for update_taxonomy that either summarizes to one species or returns all matches
#' @noRd
get_updated_species_list <-
  function(aligned_data,
           resources,
           one_to_many,
           output= NULL) {
    aligned_species_list_tmp <-
      aligned_data$aligned_name %>% update_taxonomy(resources = resources, output = output)
    
    if (one_to_many %in% c("return_all", "collapse_to_higher_taxon")) {
      multiple = "all"
    } else {
      multiple = "first"
    }
    
    aligned_data %>%
      dplyr::select(original_name, aligned_name, aligned_reason) %>%
      dplyr::left_join(aligned_species_list_tmp,
                       by = c("aligned_name"),
                       multiple = multiple) %>%
      dplyr::filter(!is.na(taxonIDClean)) %>%
      dplyr::mutate(genus = stringr::word(canonicalName, 1, 1)) %>%
      dplyr::rename(canonical_name = canonicalName) -> updated_df
    
    failed_to_update <-
      filter(
        aligned_data,
        !aligned_name %in% updated_df$aligned_name &
          known == TRUE & checked == TRUE
      )
    
    if (nrow(failed_to_update) == 0)
      return(updated_df)
    
    # this could be improved with some thought, but may need to modify align_taxa to get more information out at this stage
    failed_to_update_ss <-
      select(failed_to_update,
             original_name,
             aligned_name,
             aligned_reason) %>%
      mutate(
        source = "known_name_but_not_apc_accepted",
        taxonIDClean = NA,
        taxonomicStatusClean = "known_name_but_not_apc_accepted",
        alternativeTaxonomicStatusClean = NA,
        acceptedNameUsageID = NA,
        canonical_name = NA,
        scientificNameAuthorship = NA,
        taxonRank = NA,
        #to do
        taxonomicStatus = NA,
        family = NA,
        subclass = NA,
        taxonDistribution = NA,
        ccAttributionIRI = NA,
        genus = NA
      )
    if (nrow(updated_df) == 0) {
      return(failed_to_update_ss)
    }
    return(bind_rows(updated_df, failed_to_update_ss))
  }


#' Currently only collapses to genus or all the way to plants
#' @noRd
collapse_to_higher_taxon <-
  function(aligned_species_list, resources) {
    aligned_species_list %>%
      group_by(original_name, aligned_name) %>%
      summarise(
        apc_names = find_mrct(canonical_name, resources = resources),
        aligned_reason = paste(unique(aligned_reason), collapse = " and "),
        taxonomicStatus = paste(unique(taxonomicStatusClean), collapse = " and "),
        source = paste(unique(source), collapse = " and "),
        number_of_collapsed_taxa = n()
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
    dplyr::filter(resources$APC, resources$APC$canonicalName %in% taxa)
  
  # Check different scenarios to find the most recent common taxon
  unique_canonical_names <- unique(relevant_taxa$canonicalName)
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
