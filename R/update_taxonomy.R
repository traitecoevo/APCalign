#' Use APC and APNI to update taxonomy, replacing synonyms to current taxa where relevant
#'
#' This function uses the Australia's Virtual Herbarium's taxonomic resources, specifically the Australian Plant
#' Census (APC) and the Australian Plant Name Index (APNI), to update taxonomy of plant species, replacing any synonyms
#' to their current accepted name.
#'
#' @family taxonomic alignment functions
#'
#' @param aligned_data A tibble of plant names to update. This table must include 5 columns, original_name, aligned_name, taxon_rank, taxonomic_reference, and aligned_reason.
#' These columns are created by the function `align_taxa`. 
#' The columns `original_name` and `aligned_name` must be in the format of the scientific name, with genus and species, 
#' and may contain additional qualifiers such as subspecies or varieties. The names are case insensitive.
#'
#' @param output (optional) Name of the file where results are saved. The default is NULL and no file is created.
#' If specified, the output will be saved in a CSV file with the given name.
#'
#' @param resources the taxonomic resources required to make the summary statistics.  Loading this can be slow, so call load_taxonomic_resources separately to greatly speed this function up and pass the resources in.
#'
#'
#' @return A tibble with updated taxonomy for the specified plant names. The tibble contains the following columns:
#' \itemize{
#'   \item \code{original_name}: the original plant name.
#'   \item \code{aligned_name}: the input plant name.
#'   \item \code{accepted_name}: the APC-accepted plant name, when available.
#'   \item \code{suggested_name}: the suggested plant name to use. Identical to the accepted_name, when an accepted_name exists.
#'   \item \code{taxonomic_reference}: the source of the updated taxonomic information (APC or APNI).
#'   \item \code{taxon_ID_clean}: the unique identifier for the updated taxon.
#'   \item \code{taxonomic_status_clean}: the taxonomic status of the updated taxon.
#'   \item \code{alternative_taxonomic_status_clean}: the alternative taxonomic status for the input name, if any.
#'   \item \code{accepted_name_usage_ID}: the unique identifier for the accepted name of the input name.
#'   \item \code{canonical_name}: the accepted (or known) scientific name for the input name.
#'   \item \code{scientific_name_authorship}: the authorship information for the accepted (or known) name.
#'   \item \code{taxon_rank}: the taxonomic rank of the accepted (or known) name.
#'   \item \code{taxonomic_status}: the taxonomic status of the accepted (or known) name.
#'   \item \code{taxonomic_status_with_splits}: for taxa where there is ambiguity due to a taxon split, the taxonomic status of each possible match.
#'   \item \code{aligned_reason}: the explanation of a specific taxon name alignment (from an original name to an aligned name).
#'   \item \code{update_reason}: the explanation of a specific taxon name update (from an aligned name to an accepted name).
#'   \item \code{genus}: the genus of the accepted (or known) name.
#'   \item \code{family}: the family of the accepted (or known) name.
#'   \item \code{subclass}: the subclass of the accepted name.
#'   \item \code{taxon_distribution}: the distribution of the accepted name.
#'   \item \code{taxon_ID}: an identifier for a specific taxon concept.
#'   \item \code{taxon_ID_genus}: an identifier for the genus.
#'   \item \code{scientific_name_ID}: an identifier for the nomenclatural (not taxonomic) details of a scientific name.
#'   \item \code{row_number}: the row number of a specific original_name in the input.
#' }
#' 
#'   
#'
#' @seealso load_taxonomic_resources
#'
#' @export
#'
#' @examples
#' # Update taxonomy for two plant names and print the result
#' update_taxonomy(
#'  tibble::tibble(
#'    original_name = c("Dryandra preissii", "Banksia acuminata"),
#'    aligned_name = c("Dryandra preissii", "Banksia acuminata"),
#'    taxon_rank = c("Species", "Species"),
#'    taxonomic_reference = c("APC", "APC"),
#'    aligned_reason = NA_character_
#'  )
#' )

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
    dplyr::select(original_name, aligned_name, taxon_rank, taxonomic_reference, aligned_reason) %>%
    dplyr::mutate(genus = stringr::word(aligned_name, 1))
  
  species_and_infraspecific <- c("Species", "Forma", "Varietas", "Subspecies")
  
  taxa_out <-
    aligned_data %>%
      mutate(
        taxonomic_dataset_tmp = stringr::word(taxonomic_reference, 1),
        taxonomic_rank_tmp = ifelse(taxon_rank %in% species_and_infraspecific, "Species_x", taxon_rank),
        row_number = dplyr::row_number()
      ) %>%
    split(paste(.$taxonomic_dataset_tmp, .$taxonomic_rank_tmp))

  if (!is.null(taxa_out[["APC genus"]])) {
    taxa_out[["APC genus"]] <- taxa_out[["APC genus"]] %>%
      dplyr::left_join(
        by = "genus",
        resources$genera_all %>%
          dplyr::filter(stringr::str_detect(taxonomic_reference, "APC")) %>%
          dplyr::arrange(canonical_name, taxonomic_status) %>% ### how do I specify that I want to arrange by `preferred order`
          dplyr::distinct(canonical_name, .keep_all = TRUE) %>%
          dplyr::mutate(
            genus = canonical_name,
            taxonomic_reference_genus = taxonomic_reference) %>%
                    
          dplyr::select(
            genus, 
            taxonomic_reference_genus,
            accepted_name_usage_ID,
            taxonomic_status
          )
      ) %>%
      # todo maybe: currently not documenting alternate taxonomic status for genera
      dplyr::mutate(my_order =  forcats::fct_relevel(
        taxonomic_status,
        subset(preferred_order, preferred_order %in%  taxonomic_status)
      )) %>%
      dplyr::arrange(aligned_name, my_order) %>%
      dplyr::mutate(
        genus_accepted = resources$genera_all$canonical_name[match(accepted_name_usage_ID, resources$genera_all$taxon_ID)],
        taxonomic_reference_genus = resources$genera_all$taxonomic_reference[match(accepted_name_usage_ID, resources$genera_all$taxon_ID)],
        taxonomic_reference = ifelse(is.na(accepted_name_usage_ID), taxonomic_reference, taxonomic_reference_genus),
        taxonomic_status_genus = resources$genera_all$taxonomic_status[match(accepted_name_usage_ID, resources$genera_all$taxon_ID)],
        taxonomic_status = ifelse(is.na(accepted_name_usage_ID), as.character(my_order), taxonomic_status_genus),
        taxon_ID_genus = resources$genera_all$taxon_ID[match(accepted_name_usage_ID, resources$genera_all$accepted_name_usage_ID)],
        aligned_minus_genus = ifelse(is.na(genus_accepted), NA, stringr::str_replace(aligned_name, stringr::word(aligned_name, 1),"")),
        suggested_name = ifelse(taxonomic_status == "accepted", paste0(genus_accepted, aligned_minus_genus), NA),
        suggested_name = ifelse(taxonomic_status != "accepted", aligned_name, suggested_name),
        genus_update_reason = as.character(my_order),
        genus = genus_accepted,
        taxonomic_reference = "APC"
      ) %>%    
      dplyr::left_join(
        by = "genus",
        resources$APC %>%
          dplyr::filter(family %in% resources$family_accepted$family) %>%
          dplyr::select(
            genus,
            family
          ) %>%
          dplyr::distinct(genus, .keep_all = TRUE)
      ) %>%
      dplyr::select(-taxonomic_reference_genus, -taxonomic_status_genus, -aligned_minus_genus, -my_order, -genus_accepted, -accepted_name_usage_ID)
  }
  
  if (!is.null(taxa_out[["APNI genus"]])) {
  # todo Should we have any identifier for APNI genus-rank names?
    taxa_out[["APNI genus"]] <- taxa_out[["APNI genus"]] %>%
      dplyr::mutate(genus = stringr::word(aligned_name,1)) %>%
      dplyr::left_join(
        by = "genus",
        resources$APNI %>%
          dplyr::filter(family %in% resources$family_accepted$family) %>%
          dplyr::select(
            genus,
            family
          ) %>%
          dplyr::distinct(genus, .keep_all = TRUE)
      ) %>%
      dplyr::mutate(
        genus = NA_character_, #todo confirm genera only in APNI don't appear in this column
        accepted_name = NA_character_,
        suggested_name = aligned_name,
        taxonomic_status_genus = "unplaced"
      )
  }
  
  if (!is.null(taxa_out[["APC family"]])) {
    taxa_out[["APC family"]] <- taxa_out[["APC family"]] %>%
      dplyr::mutate(
        suggested_name = aligned_name,
        accepted_name = NA_character_,
        family = genus,
        genus = NA_character_,
        taxonomic_status_genus = NA_character_,
        taxonomic_reference = "APC"
      )    
  }
  
  if (!is.null(taxa_out[["APC Species_x"]])) {
    taxa_out[["APC Species_x"]] <- taxa_out[["APC Species_x"]] %>%
      ## First propagate extra entries for taxa that have been split, based on the aligned names 
      dplyr::left_join(
        by = "aligned_name",
        resources$APC %>%
          dplyr::filter(
            taxon_rank %in% species_and_infraspecific &
              taxonomic_status != "misapplied"
          ) %>%
          dplyr::mutate(my_order = forcats::fct_relevel(
            taxonomic_status,
            subset(preferred_order, preferred_order %in%  taxonomic_status)
          )) %>%
          dplyr::arrange(canonical_name, my_order) %>%
          dplyr::mutate(
            aligned_name = canonical_name,
            taxonomic_status_with_splits = taxonomic_status,
            taxon_ID_with_splits = taxon_ID
          ) %>%
          dplyr::select(
            aligned_name,
            taxonomic_status_with_splits,
            accepted_name_usage_ID,
            taxon_ID_with_splits,
            scientific_name_ID
          )
      ) %>%
      # Second find accepted names for each name in the species (and infraspecific taxon) list (sometimes they are the same)
      dplyr::left_join(
        by = "accepted_name_usage_ID",
        resources$APC %>%
          dplyr::filter(taxonomic_status == "accepted") %>%
          dplyr::mutate(
            accepted_name = canonical_name,
            taxonomic_status_clean = taxonomic_status
          ) %>%
          dplyr::select(
            accepted_name_usage_ID,
            accepted_name,
            taxonomic_status_clean,
            scientific_name_authorship,
            family,
            subclass,
            taxon_distribution,
            ccAttributionIRI
          )
      ) %>%
      # Some species have multiple matches. We will prefer the accepted usage, but record others if they exist
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
      ungroup() %>%
      dplyr::mutate(
        suggested_name = accepted_name,
        genus_accepted = stringr::word(suggested_name, 1),
        taxonomic_status_clean = taxonomic_status_with_splits,
        taxonomic_status = "accepted" ,
        taxon_ID_genus = resources$genera_all$taxon_ID[match(genus_accepted, resources$genera_all$canonical_name)],
        genus = ifelse(is.na(genus_accepted), genus, genus_accepted),
        update_reason = taxonomic_status_clean,
        taxonomic_reference = "APC",
        suggested_name = ifelse(is.na(accepted_name), aligned_name, accepted_name),
        family = ifelse(is.na(family), resources$APC$family[match(stringr::word(suggested_name, 1), resources$APC$genus)], family),
        taxon_ID_clean = taxon_ID_with_splits
      ) %>% 
      dplyr::select(-my_order, -genus_accepted)
  }
  
  if (!is.null(taxa_out[["APNI Species_x"]])) {
    taxa_out[["APNI Species_x"]] <- taxa_out[["APNI Species_x"]] %>%
    dplyr::left_join(
      by = "aligned_name",
      resources$APNI %>%
        dplyr::filter(taxon_rank %in% species_and_infraspecific) %>%
        dplyr::distinct(canonical_name, .keep_all = TRUE) %>%
        dplyr::mutate(
          aligned_name = canonical_name
        ) %>%
        dplyr::select(
          aligned_name,
          canonical_name,
          scientific_name_authorship,
          scientific_name_ID,
          taxonomic_status,
          family,
          ccAttributionIRI
        )
    ) %>%
      dplyr::mutate(
        canonical_name = ifelse(is.na(scientific_name_ID), NA, aligned_name),
        accepted_name = NA_character_,
        taxon_ID_clean = NA_character_,
        suggested_name = ifelse(
          taxon_rank %in% species_and_infraspecific, 
          aligned_name,
          suggested_name
        ),
        genus = stringr::word(suggested_name, 1)
      ) %>%
      dplyr::left_join(
        by = "genus",
        resources$genera_all %>%
          dplyr::arrange(canonical_name, taxonomic_status) %>% ### how do I specify that I want to arrange by `preferred order`
          distinct(canonical_name, .keep_all = TRUE) %>%
          dplyr::mutate(
            genus = canonical_name,
            accepted_name_usage_ID_genus = accepted_name_usage_ID,
            taxonomic_status_genus = taxonomic_status,
            taxonomic_reference_genus = taxonomic_reference
          ) %>%
          dplyr::select(
            genus, 
            accepted_name_usage_ID_genus, 
            taxonomic_status_genus, 
            taxonomic_reference_genus
          )
      ) %>%
      dplyr::mutate(
        genus_accepted = ifelse(is.na(accepted_name_usage_ID_genus), NA_character_, resources$genera_all$canonical_name[match(accepted_name_usage_ID_genus, resources$genera_all$taxon_ID)]),
        taxon_ID_genus = resources$genera_all$taxon_ID[match(genus_accepted, resources$genera_all$canonical_name)],
        genus = ifelse(is.na(genus_accepted), genus, genus_accepted),
        taxonomic_reference_genus = ifelse(stringr::str_detect(taxonomic_reference_genus, "APC"), "APC", taxonomic_reference_genus)
      ) %>%
      dplyr::select(-accepted_name_usage_ID_genus)
  }
  
  taxa_blank <-
      tibble::tibble(
        original_name = character(0L),
        aligned_name = character(0L),
        accepted_name = character(0L),
        suggested_name = character(0L),
        genus = character(0L),
        family = character(0L),
        taxon_rank = character(0L),
        taxonomic_reference = character(0L),
        taxonomic_status = character(0L),
        aligned_reason = character(0L),
        update_reason = character(0L),
        subclass = character(0L),
        taxon_distribution = character(0L),
        scientific_name_authorship = character(0L),
        taxon_ID = character(0L),
        taxon_ID_genus = character(0L),
        scientific_name_ID = character(0L),
        canonical_name = character(0L),
        taxonomic_status_clean = character(0L),
        taxonomic_status_with_splits = character(0L),
        row_number = numeric(0L)
      )

  taxa_out <-
    dplyr::bind_rows(taxa_out) %>%
    dplyr::bind_rows(taxa_blank) %>%
    dplyr::mutate(
      suggested_name = ifelse(is.na(suggested_name), aligned_name, suggested_name),
      suggested_name = ifelse(is.na(suggested_name), original_name, suggested_name),
      update_reason = ifelse(taxonomic_status_clean == "accepted", "aligned name accepted by APC", update_reason),
      taxonomic_status = ifelse(is.na(taxonomic_status), "unknown", taxonomic_status),
      taxonomic_reference = ifelse(stringr::str_detect(taxonomic_reference, "APC"), "APC", taxonomic_reference),
      genus = ifelse(taxonomic_status == "unknown", NA_character_, genus),
      taxon_rank = ifelse(taxonomic_status == "unknown", NA_character_, taxon_rank),
      taxon_rank = stringr::str_to_lower(taxon_rank),
      canonical_name = suggested_name,
      taxonomic_status_clean = ifelse(is.na(taxonomic_status_clean), NA_character_, taxonomic_status_clean),
      taxon_ID = taxon_ID_clean
    ) %>%
    dplyr::select(
      original_name,
      aligned_name,
      accepted_name,
      suggested_name,
      genus,
      family,
      taxon_rank,
      taxonomic_reference,
      taxonomic_status,
      aligned_reason,
      update_reason,
      subclass,
      taxon_distribution,
      scientific_name_authorship,
      taxon_ID,
      taxon_ID_genus,
      scientific_name_ID,
      canonical_name,
      taxonomic_status_clean,
      taxonomic_status_with_splits,
      row_number
    )

  # Assemble output in the order of the input `aligned_names`
  
  ## XXX code exists because NA's in alignments were breaking code
  # taxa_out <-
  #   taxa_out %>%
  #   dplyr::distinct() %>%
  #   # Bring in any missing taxa (without alignments) so output list is complete
  #   dplyr::bind_rows(
  #     aligned_data %>%
  #       dplyr::filter(!aligned_name %in% taxa_out$aligned_name)
  #   ) %>%
  #   # As we may have multiple matches per species and want to maintain order within taxa,
  #   # we'll do this by nesting data before joining into original list
  #   tidyr::nest(.by = "aligned_name", .key = "data")
  # 
  # taxa_out <- 
  #   aligned_data %>%
  #   select(aligned_name) %>% 
  #   # join into original list
  #   dplyr::left_join(by = "aligned_name", taxa_out) %>%
  #   # Now unnest
  #   tidyr::unnest("data") #%>%
  # # some extra useful info
  # # dplyr::mutate(genus = stringr::word(canonical_name, 1, 1))
  
  if (!is.null(output)) {
    readr::write_csv(taxa_out, output)
    message("  - output saved in file: ", output)
  }
  
  taxa_out
}
