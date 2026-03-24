#' @title Synonyms for Currently Accepted Names
#'
#' @description
#' This function generates lists a string of synonyms for currently accepted names to facilitate working out past names of a taxon
#' when the current name is known
#' 
#' @param accepted_names A character vector of currently accepted taxon names to look up synonyms for.
#' @param collapse Offering the option to return a long data table with each synonym in its own row,
#' versus collapsed into a vector for each accepted name
#' @param resources Taxonomic resources loaded via [load_taxonomic_resources()].
#'
#' @returns A table with the currently accepted name and columns documenting all synonyms and all synonyms with taxonomic status.
#' @export
#'
#' @examples
#' synonyms_for_accepted_names(
#'   accepted_names = c("Justicia tenella", "Acacia aneura"),
#'   collapse = TRUE
#' )
#' 
synonyms_for_accepted_names <- function(accepted_names, collapse = TRUE, resources = load_taxonomic_resources()) {

  if(is.null(resources)){
    message("Not finding taxonomic resources; check internet connection?")
    return(NULL)
  }
  
  # generate list of accepted_name_usage_ID's for accepted species
  accepted_names_with_usageID <- resources$APC_accepted |>
    dplyr::select(accepted_name_usage_ID, accepted_name = canonical_name) |>
    dplyr::filter(accepted_name %in% accepted_names)
  
  # preferred order of taxonomic updates (function from `update_taxonomy.R`)
  relevel_taxonomic_status_preferred_order <- function(taxonomic_status) {
    
    preferred_order <-
      c(
        "accepted",
        "taxonomic synonym",
        "basionym",
        "nomenclatural synonym",
        "isonym",
        "orthographic variant",
        "common name",
        "doubtful taxonomic synonym",
        "replaced synonym",
        "doubtful pro parte taxonomic synonym",
        "pro parte nomenclatural synonym",
        "pro parte taxonomic synonym",
        "pro parte misapplied",
        "misapplied",
        "unplaced", 
        "excluded",
        "doubtful misapplied",
        "doubtful pro parte misapplied",
        "included"
      )
    
    factor(taxonomic_status, levels =
             subset(
               preferred_order, 
               preferred_order %in% taxonomic_status
             )
    )
  }
  
  # generate list of accepted_name_usage_ID's for accepted species
  APC_synonyms_tmp <- resources$APC |> 
    dplyr::filter(taxon_rank %in% c("species", "variety", "form", "subspecies")) |>
    # merge currently accepted names for each taxon onto all the synonyms
    dplyr::right_join(accepted_names_with_usageID, by = "accepted_name_usage_ID") |>
    dplyr::select(canonical_name, taxonomic_status, accepted_name, accepted_name_usage_ID) |>
    # remove the accepted names themselves
    dplyr::filter(taxonomic_status != "accepted") |>
    dplyr::mutate(
      taxonomic_status = (relevel_taxonomic_status_preferred_order(taxonomic_status)),
    ) |>
    dplyr::distinct(accepted_name, canonical_name, .keep_all = TRUE) |>
    dplyr::arrange(accepted_name, taxonomic_status, taxonomic_status)
  
  
  if(collapse == TRUE) {
    # Generate list of delimited synonyms and their taxonomic status
    APC_synonyms <- APC_synonyms_tmp |>
      dplyr::group_by(accepted_name, accepted_name_usage_ID) |>
      dplyr::mutate(
        name_with_status = paste0(canonical_name, " (", taxonomic_status, ")"),
        synonyms = paste0(name_with_status, collapse = "; ")
        ) |>
      dplyr::ungroup() |>
      dplyr::distinct(accepted_name_usage_ID, synonyms)

    accepted_names_with_synonyms <- resources$APC |> 
      dplyr::select(canonical_name, taxon_rank, name_type, genus, family, scientific_name, accepted_name_usage_ID) |>
      dplyr::filter(canonical_name %in% accepted_names_with_usageID$accepted_name & accepted_name_usage_ID %in% accepted_names_with_usageID$accepted_name_usage_ID) |>
      dplyr::distinct(canonical_name, .keep_all = T) |>
      dplyr::left_join(APC_synonyms, by = "accepted_name_usage_ID") |>
      dplyr::rename(taxon_name = canonical_name) |>
      dplyr::arrange(family, taxon_name)
  
  } else {
    
    # Create a long list if collapse = F, with one row per synonym
    accepted_names_with_synonyms <- resources$APC |> 
      dplyr::select(canonical_name, taxon_rank, name_type, genus, family, scientific_name, accepted_name_usage_ID) |>
      dplyr::filter(canonical_name %in% accepted_names_with_usageID$accepted_name & accepted_name_usage_ID %in% accepted_names_with_usageID$accepted_name_usage_ID) |>
      dplyr::distinct(canonical_name, .keep_all = T) |>
      dplyr::select(-canonical_name) |>
      dplyr::left_join(APC_synonyms_tmp, by = "accepted_name_usage_ID") |>
      dplyr::arrange(family, accepted_name)
  }
  
  accepted_names_with_synonyms
  
}
