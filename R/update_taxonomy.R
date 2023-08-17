#' Use APC and APNI to update taxonomy, replacing synonyms to current taxa where relevant
#'
#' This function uses the Australia's Virtual Herbarium's taxonomic resources, specifically the Australian Plant
#' Census (APC) and the Australian Plant Name Index (APNI), to update taxonomy of plant species, replacing any synonyms
#' to their current accepted name.
#'
#' @family taxonomic alignment functions
#'
#' @param aligned_names A character vector of plant names to update. These names must be in the format of the
#' scientific name, with genus and species, and may contain additional qualifiers such as subspecies or varieties.
#' The names are case insensitive.
#'
#' @param output (optional) Name of the file where results are saved. The default is NULL and no file is created.
#' If specified, the output will be saved in a CSV file with the given name.
#'
#' @param resources the taxonomic resources required to make the summary statistics.  Loading this can be slow, so call load_taxonomic_resources separately to greatly speed this function up and pass the resources in.
#'
#'
#' @return A tibble with updated taxonomy for the specified plant names. The tibble contains the following columns:
#' \itemize{
#'   \item \code{aligned_name}: the input plant name.
#'   \item \code{taxonomic_reference}: the source of the updated taxonomic information (APC or APNI).
#'   \item \code{taxon_ID_clean}: the unique identifier for the updated taxon.
#'   \item \code{taxonomic_status_clean}: the taxonomic status of the updated taxon.
#'   \item \code{alternative_taxonomic_status_clean}: the alternative taxonomic status for the input name, if any.
#'   \item \code{accepted_name_usage_ID}: the unique identifier for the accepted name of the input name.
#'   \item \code{canonical_name}: the accepted (or known) scientific name for the input name.
#'   \item \code{scientific_name_authorship}: the authorship information for the accepted (or known) name.
#'   \item \code{taxon_rank}: the taxonomic rank of the accepted (or known) name.
#'   \item \code{taxonomic_status}: the taxonomic status of the accepted (or known) name.
#'   \item \code{genus}: the genus of the accepted (or known) name.
#'   \item \code{family}: the family of the accepted (or known) name.
#'   \item \code{subclass}: the subclass of the accepted name.
#'   \item \code{taxon_distribution}: the distribution of the accepted name.
#'   \item \code{taxon_ID}: an identifier for a specific taxon concept.
#'   \item \code{scientific_name_ID}: an identifier for the nomenclatural (not taxonomic) details of a scientific name.
#'   \item \code{ccAttributionIRI}: the Creative Commons Attribution International Rights URI of the accepted name.
#' }
#'
#' @seealso load_taxonomic_resources
#'
#' @export
#'
#' @examples
#' # Update taxonomy for two plant names and print the result
#' \donttest{update_taxonomy(c("Eucalyptus pauciflora", "Acacia victoriae"))}

update_taxonomy <- function(aligned_data,
                            output = NULL,
                            resources = load_taxonomic_resources()) {
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
      "misapplied",
      "doubtful pro parte taxonomic synonym",
      "pro parte nomenclatural synonym",
      "pro parte taxonomic synonym",
      "pro parte misapplied",
      "excluded",
      "doubtful misapplied",
      "doubtful pro parte misapplied"
    )
  
  aligned_data <- 
    aligned_data %>%
    dplyr::select(aligned_name, taxon_rank, taxonomic_reference) %>%
    dplyr::mutate(genus = stringr::word(aligned_name, 1))
  
  species_and_infraspecific <- c("Species", "Forma", "Varietas", "Subspecies")
  
  taxa_out <-
    aligned_data %>%
    dplyr::left_join(
      by = "genus",
      resources$genera_all %>%
        dplyr::select(
          genus = canonical_name,
          taxonomic_dataset_genus = taxonomic_reference,
          accepted_name_usage_ID_genus = accepted_name_usage_ID,
          taxonomic_status_genus = taxonomic_status,
        )
    ) %>%
    # to do currently not documenting alternate taxonomic status for genera
    dplyr::mutate(my_order =  forcats::fct_relevel(
      taxonomic_status_genus,
      subset(preferred_order, preferred_order %in%  taxonomic_status_genus)
    )) %>%
    dplyr::arrange(aligned_name, my_order) %>%
    dplyr::mutate(
      genus_accepted = resources$genera_all$canonical_name[match(accepted_name_usage_ID_genus, resources$genera_all$taxon_ID)],
      taxonomic_dataset_genus = resources$genera_all$taxonomic_reference[match(accepted_name_usage_ID_genus, resources$genera_all$taxon_ID)],
      taxonomic_dataset_genus = ifelse(is.na(accepted_name_usage_ID_genus), taxonomic_reference, taxonomic_dataset_genus),
      taxonomic_status_genus = resources$genera_all$taxonomic_status[match(accepted_name_usage_ID_genus, resources$genera_all$taxon_ID)],
      taxonomic_status_genus = ifelse(is.na(accepted_name_usage_ID_genus), my_order, taxonomic_status_genus),
      genus_accepted = ifelse(is.na(taxonomic_status_genus)|taxonomic_status_genus == "unplaced", NA, genus_accepted),
      aligned_minus_genus = ifelse(is.na(genus_accepted), NA, stringr::str_replace(aligned_name, stringr::word(aligned_name, 1),"")),
      suggested_name = ifelse(taxon_rank == "genus" & taxonomic_status_genus == "accepted", paste0(genus_accepted, aligned_minus_genus), NA),
      suggested_name = ifelse(taxon_rank == "genus" & taxonomic_status_genus != "accepted", aligned_name, suggested_name),
      genus_update_reason = my_order,
      taxonomic_dataset_tmp = stringr::word(taxonomic_reference, 1)
      ) %>%
    split(paste(.$taxonomic_dataset_tmp))
  
  taxa_out[["APC"]] <- taxa_out[["APC"]] %>%
  dplyr::left_join(
    by = "aligned_name",
    resources$APC %>%
      dplyr::filter(taxon_rank %in% species_and_infraspecific) %>%
      dplyr::distinct(canonical_name, .keep_all = TRUE) %>%
      dplyr::select(
        aligned_name = canonical_name,
        taxon_ID_clean = taxon_ID,
        taxonomic_status_clean = taxonomic_status,
        accepted_name_usage_ID,
        scientific_name_ID
      )
  ) %>%
    # Now find accepted names for each name in the species (and infraspecific taxon) list (sometimes they are the same)
    dplyr::left_join(
      by = "accepted_name_usage_ID",
      resources$APC %>%
        dplyr::filter(taxonomic_status == "accepted") %>%
        dplyr::select(
          accepted_name_usage_ID,
          accepted_name = canonical_name,
          taxonomic_status,
          scientific_name_authorship,
          family,
          subclass,
          taxon_distribution,
          ccAttributionIRI
        )
    ) %>%
    # Some species have multiple matches. We will prefer the accepted usage, but record others if they exists
    # To do this we define the order we want variables to sort by, with accepted at the top
    dplyr::mutate(my_order =  forcats::fct_relevel(
      taxonomic_status_clean,
      subset(preferred_order, preferred_order %in%  taxonomic_status_clean)
    )) %>%
    dplyr::arrange(aligned_name, my_order) %>%
    # For each species, keep the first record (accepted if present) and
    # record any alternative status to indicate where there was ambiguity
    dplyr::group_by(aligned_name) %>%
    dplyr::mutate(
      # todo: move this outside function to higher level
      alternative_taxonomic_status_clean = ifelse(
        taxonomic_status_clean[1] == "accepted",
        taxonomic_status_clean %>% unique() %>%  subset(. , . != "accepted") %>% paste0(collapse = " | ") %>% dplyr::na_if(""),
        NA
      ),
      suggested_name = ifelse(
        taxon_rank %in% species_and_infraspecific, 
        accepted_name,
        suggested_name
      )
    )
  
  taxa_out[["APNI"]] <- taxa_out[["APNI"]] %>%
  dplyr::left_join(
    by = "aligned_name",
    resources$APNI %>%
      dplyr::filter(taxon_rank %in% species_and_infraspecific) %>%
      dplyr::distinct(canonical_name, .keep_all = TRUE) %>%
      dplyr::select(
        aligned_name = canonical_name,
        canonical_name,
        scientific_name_authorship,
        scientific_name_ID,
        taxonomic_status,
        family,
        ccAttributionIRI
      )
  ) %>%
    dplyr::mutate(
      taxon_ID_clean = NA_character_,
      family = ifelse(dplyr::n_distinct(family) > 1, NA, family[1])
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      taxonomic_reference = ifelse(is.na(scientific_name_ID), NA, "APNI"),
      ## taxa without a `scientific_name_ID` are also not in APNI
      canonical_name = ifelse(is.na(scientific_name_ID), NA, aligned_name),
      taxonomic_status_clean = ifelse(is.na(taxon_ID_clean), "unknown", "unplaced"),
      taxonomic_status = taxonomic_status_clean,
      accepted_name = NA_character_,
      suggested_name = ifelse(
        taxon_rank %in% species_and_infraspecific, 
        aligned_name,
        suggested_name
      )    
    ) 
  
  taxa_out <- 
    dplyr::bind_rows(taxa_out[["APC"]], taxa_out[["APNI"]], taxa_out[["NA"]]) %>%
    mutate(
      taxonomic_reference = ifelse(taxon_rank %in% species_and_infraspecific, taxonomic_reference, taxonomic_dataset_genus),
      taxonomic_status = ifelse(taxon_rank %in% species_and_infraspecific, taxonomic_status, taxonomic_status_genus),
      update_reason = ifelse(taxon_rank %in% species_and_infraspecific, taxonomic_status_clean, genus_update_reason)
    ) %>%
    select(
      original_name,
      aligned_name,
      accepted_name,
      suggested_name,
      genus,
      family,
      taxon_rank,
      taxonomic_reference,
      taxonomic_status,
      update_reason,
      subclass,
      taxon_distribution,
      scientific_name_authorship,
      taxon_ID = taxon_ID_clean,
      scientific_name_ID,
    )
  
  # if matches in APC and APNI, combine these and return
  #if (nrow(taxa_APNI) > 0 & nrow(taxa_APC) > 0) {
  #  taxa_out <-
  #    dplyr::bind_rows(taxa_APC,
  #                     taxa_APNI)
  #} else {
  #  taxa_out <- taxa_APC
  #}
  
  # Assemble output in the order of the input `aligned_names`
  
  ## XXX code exists because NA's in alignments were breaking code
  taxa_out <-
    taxa_out %>%
    dplyr::distinct() %>%
    # Bring in any missing taxa (without alignments) so output list is complete
    dplyr::bind_rows(
      aligned_data %>%
        dplyr::filter(!aligned_name %in% taxa_out$aligned_name)
    ) %>%
    # As we may have multiple matches per species and want to maintain order within taxa,
    # we'll do this by nesting data before joining into original list
    tidyr::nest(.by = "aligned_name", .key = "data")
  
  taxa_out <- 
    aligned_data %>%
    select(aligned_name) %>% 
    # join into original list
    dplyr::left_join(by = "aligned_name", taxa_out) %>%
    # Now unnest
    tidyr::unnest("data") #%>%
  # some extra useful info
  # dplyr::mutate(genus = stringr::word(canonical_name, 1, 1))
  
  if (!is.null(output)) {
    readr::write_csv(taxa_out, output)
    message("  - output saved in file: ", output)
  }
  
  taxa_out
}
