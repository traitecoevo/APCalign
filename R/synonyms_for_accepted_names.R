#' @title Synonyms for Currently Accepted Names
#'
#' @description
#' This function generates lists a string of synonyms for currently accepted names.
#' 
#' @param resources 
#'
#' @returns A table with the currently accepted name and columns documenting all synonyms and all synonyms with taxonomic styatus.
#' @export
#'
#' @examples
#' synonyms_for_accepted_names(resources = resources, accepted_names = c("Justicia tenella", "Acacia aneura"))
#' synonyms_for_accepted_names()
#' 
synonyms_for_accepted_names <- function(resources = resources, accepted_names = resources$`APC list (accepted`$canonical_name) {

  accepted_names_with_usageID <- resources$`APC list (accepted` |>
    select(accepted_name_usage_ID, accepted_name = canonical_name) |>
    filter(accepted_name %in% accepted_names)
  
  known_synonyms <- resources$APC |> 
    right_join(accepted_names_with_usageID) |>
    filter(taxon_rank %in% c("species", "variety", "form", "subspecies")) |>
    select(canonical_name, taxonomic_status, accepted_name, accepted_name_usage_ID, family) |>
    mutate(
      name_with_status = paste0(canonical_name, " (", taxonomic_status, ")")
    ) |>
    group_by(accepted_name, accepted_name_usage_ID) |>
      mutate(
        synonyms = paste0(canonical_name, collapse = "; "),
        synonyms_with_status = paste0(name_with_status, collapse = "; "),
        family = paste0(unique(family), collapse = "; ")
        ) |>
    ungroup() |>
    distinct(accepted_name, family, synonyms, synonyms_with_status, accepted_name_usage_ID) |>
    left_join(resources$`APC list (accepted` |> select(taxon_rank, name_type, genus, scientific_name, accepted_name_usage_ID)) |>
    arrange(family, accepted_name)
  
  known_synonyms
  
}
