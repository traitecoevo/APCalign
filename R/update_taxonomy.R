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
#'   \item \code{source}: the source of the updated taxonomic information (APC or APNI).
#'   \item \code{taxonIDClean}: the unique identifier for the updated taxon.
#'   \item \code{taxonomic_status_clean}: the taxonomic status of the updated taxon.
#'   \item \code{alternative_taxonomic_status_clean}: the alternative taxonomic status for the input name, if any.
#'   \item \code{accepted_name_usage_ID}: the unique identifier for the accepted name of the input name.
#'   \item \code{canonical_name}: the accepted scientific name for the input name.
#'   \item \code{scientific_name_authorship}: the authorship information for the accepted name.
#'   \item \code{taxon_rank}: the taxonomic rank of the accepted name.
#'   \item \code{taxonomic_status}: the taxonomic status of the accepted name.
#'   \item \code{family}: the family of the accepted name.
#'   \item \code{subclass}: the subclass of the accepted name.
#'   \item \code{taxonDistribution}: the distribution of the accepted name.
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

update_taxonomy <- function(aligned_names,
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
  
  taxa_out <-
    tibble::tibble(aligned_name = aligned_names %>% unique %>% sort) %>%
    # match names against names in APC list
    dplyr::left_join(
      by = "aligned_name",
      resources$APC %>% dplyr::filter(!grepl("sp\\.$", canonical_name)) %>%
        dplyr::select(
          aligned_name = canonical_name,
          taxonIDClean = taxonID,
          taxonomic_status_clean = taxonomic_status,
          accepted_name_usage_ID
        )
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(source = ifelse(!is.na(taxonIDClean), "APC", NA)) %>%
    # Now find accepted names for each name in the list (sometimes they are the same)
    dplyr::left_join(
      by = "accepted_name_usage_ID",
      resources$APC %>%
        dplyr::filter(taxonomic_status == "accepted") %>%
        dplyr::select(
          accepted_name_usage_ID,
          canonical_name,
          taxonomic_status,
          scientific_name_authorship,
          family,
          subclass,
          taxonDistribution,
          taxon_rank,
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
      )
    ) %>%
    #dplyr::slice(1:5) %>%
    dplyr::filter(taxonomic_status_clean != "misapplied") %>%
    dplyr::ungroup() %>%
    dplyr::select(
      aligned_name,
      source,
      taxonIDClean,
      taxonomic_status_clean,
      alternative_taxonomic_status_clean,
      accepted_name_usage_ID,
      canonical_name,
      scientific_name_authorship,
      taxon_rank,
      taxonomic_status,
      family,
      subclass,
      taxonDistribution,
      ccAttributionIRI
    ) %>%
    distinct()
  
  taxa_APC <-
    taxa_out %>% dplyr::filter(!is.na(taxonIDClean)) %>%
    dplyr::distinct()
  
  # Now check against APNI for any species not found in APC
  taxa_APNI <-
    taxa_out %>%
    dplyr::filter(is.na(canonical_name))  %>%
    dplyr::select(aligned_name) %>%
    dplyr::left_join(
      by = "aligned_name",
      resources$APNI %>% dplyr::filter(nameElement != "sp.") %>%
        dplyr::select(
          aligned_name = canonical_name,
          scientific_name_ID,
          family,
          taxon_rank
        )
    ) %>%
    dplyr::group_by(aligned_name) %>%
    dplyr::mutate(
      taxonIDClean = paste(taxonIDClean, collapse = " ") %>% dplyr::na_if("NA"),
      family = ifelse(dplyr::n_distinct(family) > 1, NA, family[1])
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      source = ifelse(is.na(taxonIDClean), NA, "APNI"),
      canonical_name = ifelse(is.na(taxonIDClean), NA, aligned_name),
      taxonomic_status_clean = ifelse(is.na(taxonIDClean), "unknown", "unplaced"),
      taxonomic_status = taxonomic_status_clean
    ) %>%
    dplyr::filter(!is.na(taxonIDClean))

  # if matches in APC and APNI, combine these and return
  if (nrow(taxa_APNI) > 0) {
    taxa_out <-
      dplyr::bind_rows(taxa_APC,
                       taxa_APNI)
  } else {
    taxa_out <- taxa_APC
  }
  
  # Assemble output in the order of the input `aligned_names`
  taxa_out2 <-
    taxa_out %>%
    dplyr::distinct() %>%
    # Bring in any missing taxa (without alignments) so output list is complete
    dplyr::bind_rows(
      dplyr::tibble(aligned_name = unique(aligned_names)) %>%
        dplyr::filter(!aligned_name %in% taxa_out$aligned_name)
    ) %>%
    # As we may have multiple matches per species and want to maintain order within taxa,
    # we'll do this by nesting data before joining into original list
    tidyr::nest(.by = "aligned_name", .key = "data")
  
  taxa_out2 <- 
    dplyr::tibble(aligned_name = aligned_names) %>% 
    # join into original list
    dplyr::left_join(by = "aligned_name", taxa_out2) %>%
    # Now unnest
    tidyr::unnest("data") %>%
    # some extra useful info
    dplyr::mutate(genus = stringr::word(canonical_name, 1, 1))
  
  if (!is.null(output)) {
    readr::write_csv(taxa_out2, output)
    message("  - output saved in file: ", output)
  }
  
  taxa_out2
}
