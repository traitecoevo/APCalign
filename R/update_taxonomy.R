#' Use APC and APNI to update taxonomy, replacing synonyms to current taxa where relevant
#'
#' This function uses the Australia's Virtual Herbarium's taxonomic resources, specifically the Australian Plant
#' Census (APC) and the Australian Plant Name Index (APNI), to update taxonomy of plant species, replacing any synonyms
#' to their current accepted name.
#'
#' @family taxonomic alignment functions
#'
#' @param aligned_data A tibble of plant names to update. This table must include 5 columns, original_name, aligned_name, taxon_rank, taxonomic_dataset, and aligned_reason.
#' These columns are created by the function `align_taxa`. 
#' The columns `original_name` and `aligned_name` must be in the format of the scientific name, with genus and species, 
#' and may contain additional qualifiers such as subspecies or varieties. The names are case insensitive.
#'
#' @param taxonomic_splits XXXXX
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
#'   \item \code{taxonomic_dataset}: the source of the updated taxonomic information (APC or APNI).
#'   \item \code{accepted_name_usage_ID}: the unique identifier for the accepted name of the input name.
#'   \item \code{canonical_name}: the accepted (or known) scientific name for the input name.
#'   \item \code{scientific_name_authorship}: the authorship information for the accepted (or known) name.
#'   \item \code{taxon_rank}: the taxonomic rank of the accepted (or known) name.
#'   \item \code{taxonomic_status}: the taxonomic status of the accepted (or known) name.
#'   \item \code{ taxonomic_status_aligned}: for taxa where there is ambiguity due to a taxon split, the taxonomic status of each possible match.
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
#'    taxonomic_dataset = c("APC", "APC"),
#'    aligned_reason = NA_character_
#'  )
#' )

update_taxonomy <- function(aligned_data,
                            taxonomic_splits = "most_likely_species",
                            output = NULL,
                            resources = load_taxonomic_resources()) {
    
  aligned_data <- 
    aligned_data %>%
    dplyr::select(original_name, aligned_name, taxon_rank, taxonomic_dataset, aligned_reason) %>%
    dplyr::mutate(
      genus = stringr::word(aligned_name, 1),
      row_number = dplyr::row_number()
    )

  ## split tibble of aligned names based on 
  ## - the taxonomic dataset (APC, APNI), 
  ## - the taxon rank (family, genus, species/infraspecific) assigned during taxon alignments
  taxa_out <-
    split(aligned_data,
      paste(
        # Taxonomic reference
        aligned_data$taxonomic_dataset,
        # Taxon rank
        ifelse(species_and_infraspecific(aligned_data$taxon_rank), "species_and_infraspecific_taxa", aligned_data$taxon_rank)
      )
    )

  ## taxa whose aligned_names are taxon_rank = genus and taxonomic_dataset = APC
  taxa_out[["APC genus"]] <- taxa_out[["APC genus"]] %>%
    update_taxonomy_APC_genus(resources)
    
  ## taxa whose aligned_names are taxon_rank = genus and taxonomic_dataset = APNI
  ## these are genera that are not recorded in the APC and therefore there are no identifiers for these taxa
  ## the only information that can be added is, when available, the family name
  taxa_out[["APNI genus"]] <- taxa_out[["APNI genus"]] %>%
    update_taxonomy_APNI_genus(resources)
    
  ## taxa whose aligned_names are taxon_rank = family and taxonomic_dataset = APC
  ## these are taxa for which the `genus` and `accepted_name` fields will be NA's and all identifiers will be blank
  taxa_out[["APC family"]] <- taxa_out[["APC family"]] %>%
    update_taxonomy_APC_family(resources)
    
  ## taxa whose aligned_names are taxon_rank = species/infraspecific and taxonomic_dataset = APC
  ## these are the subset of names in `taxa_out` that *should* have an APC-accepted name
  taxa_out[["APC species_and_infraspecific_taxa"]] <- taxa_out[["APC species_and_infraspecific_taxa"]] %>%
    update_taxonomy_APC_species_and_infraspecific_taxa(resources)
  
  ## taxa whose aligned_names are taxon_rank = species/infraspecific and taxonomic_dataset = APNI
  taxa_out[["APNI species_and_infraspecific_taxa"]] <- taxa_out[["APNI species_and_infraspecific_taxa"]] %>%
    update_taxonomy_APNI_species_and_infraspecific_taxa(resources)
  
  ## create a blank tibble with all columns, for taxon lists where some columns aren't created in any of the individual tibbles
  # todo can we remove?
  taxa_blank <-
      tibble::tibble(
        original_name = character(0L),
        aligned_name = character(0L),
        accepted_name = character(0L),
        suggested_name = character(0L),
        genus = character(0L),
        family = character(0L),
        taxon_rank = character(0L),
        taxonomic_dataset = character(0L),
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
        taxonomic_status_aligned = character(0L),
        row_number = numeric(0L)
      )

  ## bind taxa_out back together.
  ## there are some tibbles that can be created when taxa_out is split that will not have been manipulated, namely those without alignments and that
  ## therefore are NA for either `taxonomic_dataset` or `taxon_rank`; the mutate steps below are primarily to ensure the final `suggested_name`
  ## `taxon_rank`, `taxonomic_status`, `taxonomic_dataset`, `genus` are properly filled in for those rows of data
  taxa_out <-
    dplyr::bind_rows(taxa_out) %>%
    dplyr::bind_rows(taxa_blank) %>%
    dplyr::mutate(
      suggested_name = ifelse(is.na(suggested_name), aligned_name, suggested_name),
      suggested_name = ifelse(is.na(suggested_name), original_name, suggested_name),
      update_reason = ifelse(taxonomic_status_aligned == "accepted", "aligned name accepted by APC", update_reason),
      taxonomic_status = ifelse(is.na(taxonomic_status), "unknown", taxonomic_status),
      taxonomic_dataset = ifelse(stringr::str_detect(taxonomic_dataset, "APC"), "APC", taxonomic_dataset),
      ## `genus` was the first word of the `aligned_name` in the input table; now needs to be set to NA for unknown taxa
      genus = ifelse(taxonomic_status == "unknown", NA_character_, genus),
      taxon_rank = ifelse(taxonomic_status == "unknown", NA_character_, taxon_rank),
      # the next line makes everythign incosistent. If we want low, should do on loading APC
      taxon_rank = stringr::str_to_lower(taxon_rank),
      canonical_name = suggested_name,
      taxonomic_status_aligned = ifelse(is.na(taxonomic_status_aligned), NA_character_, taxonomic_status_aligned)
    ) %>%
    dplyr::select(
      original_name,
      aligned_name,
      accepted_name,
      suggested_name,
      genus,
      family,
      taxon_rank,
      taxonomic_dataset,
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
      taxonomic_status_aligned,
      row_number
    )

  
  # Implement decisions about how to handle splits
  if (taxonomic_splits == "most_likely_species") {
    taxa_out <-
      taxa_out %>%
      dplyr::group_by(row_number) %>%
      # todo move ordering to loading taxonomic resources?
      dplyr::mutate(
        my_order = relevel_taxonomic_status_preferred_order(taxonomic_status_aligned)
      ) %>%
      dplyr::arrange(row_number, my_order) %>%
      dplyr::mutate(
        possible_matches = sprintf("%s (%s)", suggested_name, taxonomic_status_aligned) %>% paste(collapse = "; ")
      ) %>%
      # take first record, this is most likely as we've set a preferred order above
      dplyr::slice(1) %>%
      dplyr::ungroup()
  }

  # todo - should this be an option here, or an extra function operating on outputs?
  if (taxonomic_splits == "collapse_to_higher_taxon") {
    taxa_out <- collapse_to_higher_taxon(taxa_out, resources)
  }
  
  # Assemble output in the order of the input `aligned_names`
  # sort so that has same order as input
  taxa_out <- taxa_out  %>% dplyr::arrange(row_number)

  if (!is.null(output)) {
    readr::write_csv(taxa_out, output)
    message("  - output saved in file: ", output)
  }
  
  taxa_out
}

# Logical based on rank of a taxon
species_and_infraspecific <- function(taxon_rank) {
  taxon_rank %in% c("Species", "Forma", "Varietas", "Subspecies")
}

# preferred order of taxonomic updates
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
      "misapplied",
      "doubtful pro parte taxonomic synonym",
      "pro parte nomenclatural synonym",
      "pro parte taxonomic synonym",
      "pro parte misapplied",
      "unplaced", 
      "excluded",
      "doubtful misapplied",
      "doubtful pro parte misapplied",
      "included"
    )
  
  forcats::fct_relevel(
    taxonomic_status,
    subset(
      preferred_order, 
      preferred_order %in% taxonomic_status
      )
  )
}
        

# Function to update names of taxa whose aligned_names are 
# taxon_rank = genus and taxonomic_dataset = APC
update_taxonomy_APC_genus <- function(data, resources) {
  
  if(is.null(data)) return(NULL)

  data %>% 
  # merge in columns from APC, at the genus-level
  dplyr::left_join(
    by = "genus",
    resources$genera_all %>%
      dplyr::filter(stringr::str_detect(taxonomic_dataset, "APC")) %>%
      dplyr::arrange(canonical_name, taxonomic_status) %>% ### how do I specify that I want to arrange by `preferred order`
      dplyr::distinct(canonical_name, .keep_all = TRUE) %>%
      dplyr::mutate(
        genus = canonical_name,
        taxonomic_dataset_genus = taxonomic_dataset
      ) %>%
      dplyr::select(
        genus,
        taxonomic_dataset_genus,
        accepted_name_usage_ID,
        taxonomic_status
      )
  ) %>%
    # todo maybe: currently not documenting alternate taxonomic status for genera
    dplyr::mutate(my_order = relevel_taxonomic_status_preferred_order(taxonomic_status)) %>%
    dplyr::arrange(aligned_name, my_order) %>%
    dplyr::mutate(
      # if required, update the genus name in the `aligned_name` to the currently APC-accepted genus
      genus_accepted = resources$genera_all$canonical_name[match(accepted_name_usage_ID, resources$genera_all$taxon_ID)],
      # XXX next 2 lines overwritten later and variables never used, probably can go
      # taxonomic_dataset_genus = resources$genera_all$taxonomic_dataset[match(accepted_name_usage_ID, resources$genera_all$taxon_ID)],
      # taxonomic_dataset = ifelse(is.na(accepted_name_usage_ID), taxonomic_dataset, taxonomic_dataset_genus),
      # add variables specifying the taxonomic_status and taxon_ID of the genus, since these will be APC-accepted taxon names, even though the
      # `accepted_name` and `taxon_ID` at the species-level will be blank
      taxonomic_status_genus = resources$genera_all$taxonomic_status[match(accepted_name_usage_ID, resources$genera_all$taxon_ID)],
      taxonomic_status = ifelse(is.na(accepted_name_usage_ID), as.character(my_order), paste("genus", taxonomic_status_genus)),
      taxon_ID_genus = resources$genera_all$taxon_ID[match(accepted_name_usage_ID, resources$genera_all$accepted_name_usage_ID)],
      # genus names in `aligned_name` that are not APC-accepted need to be updated to their current name in `suggested_name`
      aligned_minus_genus = ifelse(is.na(genus_accepted), NA, stringr::str_replace(aligned_name, stringr::word(aligned_name, 1), "")),
      suggested_name = ifelse(taxonomic_status == "accepted", paste0(genus_accepted, aligned_minus_genus), NA),
      suggested_name = ifelse(taxonomic_status != "accepted", aligned_name, suggested_name),
      # indicate taxonomic_status of the genus name in `aligned_name` and why it needed to be updated for the `suggested_name`
      genus_update_reason = as.character(my_order),
      genus = genus_accepted,
      taxonomic_dataset = "APC"
    ) %>%
    # add family alignments in
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
    dplyr::select(-taxonomic_dataset_genus, -taxonomic_status_genus, -aligned_minus_genus, -my_order, -genus_accepted, -accepted_name_usage_ID)
}

# Function to update names of taxa whose aligned_names are
#  taxon_rank = genus and taxonomic_dataset = APNI
update_taxonomy_APNI_genus <- function(data, resources) {

  if(is.null(data)) return(NULL)

  data %>%
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
    # the `suggested_name` is set to the aligned_name and other columns are set to NA
    dplyr::mutate(
      genus = NA_character_, # todo confirm genera only in APNI don't appear in this column
      accepted_name = NA_character_,
      suggested_name = aligned_name,
      taxonomic_status_genus = "unplaced"
    )
}

# Function to update names of taxa whose aligned_names are
# taxon_rank = family and taxonomic_dataset = APC
update_taxonomy_APC_family <- function(data, resources) {

  if(is.null(data)) return(NULL)

  data %>%
    dplyr::mutate(
      suggested_name = aligned_name,
      accepted_name = NA_character_,
      family = genus,
      genus = NA_character_,
      taxonomic_status_genus = NA_character_,
      taxonomic_status = "family accepted",
      taxonomic_dataset = "APC"
    )
}

# Function to update names of taxa whose aligned_names are
# taxon_rank = species/infraspecific and taxonomic_dataset = APC
update_taxonomy_APC_species_and_infraspecific_taxa <- function(data, resources) {

  if(is.null(data)) return(NULL)

  data %>%
    ## First propagate extra entries for taxa that have been split, based on the aligned names
    ## `misapplied` names need to be filtered out, as these are names that should not be include in the output when `taxonomic_splits = "return_all"`
    ## for taxa where there is ambiguity due to a taxon split, there can be multiple `canonical_name` (per APC) matches to a single `aligned_name`
    ## however the APC-column `taxon_ID` will be unique for each row of data
    ## the `accepted_named_usage_ID` column will provide the link to the possibly-accepted name
    dplyr::left_join(
      relationship = "many-to-many",
      by = "aligned_name",
      resources$APC %>%
        dplyr::filter(
          species_and_infraspecific(taxon_rank) &
            taxonomic_status != "misapplied"
        ) %>%
        dplyr::mutate(
          my_order = relevel_taxonomic_status_preferred_order(taxonomic_status)
        ) %>%
        dplyr::arrange(canonical_name, my_order) %>%
        dplyr::mutate(
          aligned_name = canonical_name,
          taxonomic_status_aligned = taxonomic_status,
        ) %>%
        dplyr::select(
          aligned_name,
          taxonomic_status_aligned,
          accepted_name_usage_ID,
          scientific_name_ID
        )
    ) %>%
    ## Second, find accepted names for each name in the species (and infraspecific taxon) list (sometimes they are the same)
    ## `accepted_name_usage_ID` provides the code to update the names. A given accepted_name has a single `accepted_name_usage_ID`
    dplyr::left_join(
      by = "accepted_name_usage_ID",
      resources$APC %>%
        dplyr::filter(taxonomic_status == "accepted") %>%
        dplyr::mutate(
          ## the canonical_name for an APC-accepted species becomes the `accepted_name` in the output
          accepted_name = canonical_name
        ) %>%
        dplyr::select(
          accepted_name_usage_ID,
          accepted_name,
          taxonomic_status,
          scientific_name_authorship,
          family,
          subclass,
          taxon_distribution
        )
    ) %>%
    dplyr::mutate(
      # for APC-accepted species, the `suggested_name` is the `accepted_name`
      suggested_name = accepted_name,
      taxonomic_status = ifelse(is.na(accepted_name),  taxonomic_status_aligned, "accepted"),
      # for APC-accepted species, the `genus` is the first word of the `accepted_name`
      genus_accepted = stringr::word(suggested_name, 1),
      taxon_ID_genus = resources$genera_all$taxon_ID[match(genus_accepted, resources$genera_all$canonical_name)],
      update_reason =  taxonomic_status_aligned,
      taxonomic_dataset = "APC",
      ## there are rare cases of names within the APC that do not align to an accepted name.
      ## For these taxa, the `suggested_name` is the `aligned_name` and the family name must be added
      genus = ifelse(is.na(genus_accepted), genus, genus_accepted),
      suggested_name = ifelse(is.na(accepted_name), aligned_name, accepted_name),
      family = ifelse(is.na(family), resources$APC$family[match(stringr::word(suggested_name, 1), resources$APC$genus)], family)
    ) %>%
    dplyr::select(-genus_accepted)
}

# Function to update names of taxa whose aligned_names are
# taxon_rank = species/infraspecific and taxonomic_dataset = APNI
update_taxonomy_APNI_species_and_infraspecific_taxa <- function(data, resources) {
  
  if(is.null(data)) return(NULL)

  data %>%
    dplyr::left_join(
      by = "aligned_name",
      resources$APNI %>%
        dplyr::filter(species_and_infraspecific(taxon_rank)) %>%
        dplyr::distinct(canonical_name, .keep_all = TRUE) %>%
        dplyr::mutate(
          aligned_name = canonical_name
        ) %>%
        # APNI names do not have a taxon_ID; therefore the only identifier is `scientific_name_ID`
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
    ## These taxa will not have an `accepted_name` and therefore the `suggested_name` is the `aligned_name`
    dplyr::mutate(
      canonical_name = ifelse(is.na(scientific_name_ID), NA, aligned_name),
      accepted_name = NA_character_,
      suggested_name = ifelse(
        species_and_infraspecific(taxon_rank),
        aligned_name,
        suggested_name
      ),
      genus = stringr::word(suggested_name, 1)
    ) %>%
    # when possible the genus of APNI names is matched to an APC-accepted genus and the appropriate genus-level taxon_ID is added
    dplyr::left_join(
      by = "genus",
      resources$genera_all %>%
        dplyr::arrange(canonical_name, taxonomic_status) %>% ### TODO - add function to create my_order and arrange based on it
        distinct(canonical_name, .keep_all = TRUE) %>%
        dplyr::mutate(
          genus = canonical_name,
          accepted_name_usage_ID_genus = accepted_name_usage_ID,
          taxonomic_status_genus = taxonomic_status,
          taxonomic_dataset_genus = taxonomic_dataset
        ) %>%
        dplyr::select(
          genus,
          accepted_name_usage_ID_genus,
          taxonomic_status_genus,
          taxonomic_dataset_genus
        )
    ) %>%
    ## final manipulations to ensure APNI names that align with APC-accepted genus
    ## have proper columns filled in (`genus` & `taxonomic_ID_genus` & `taxonomic_dataset_genus`),
    ## while APNI names that do not align with an APC-accepted genus have these columns set to NA
    dplyr::mutate(
      genus = ifelse(is.na(accepted_name_usage_ID_genus), NA_character_, resources$genera_all$canonical_name[match(accepted_name_usage_ID_genus, resources$genera_all$taxon_ID)]),
      taxon_ID_genus = resources$genera_all$taxon_ID[match(genus, resources$genera_all$canonical_name)],
      taxonomic_dataset_genus = ifelse(stringr::str_detect(taxonomic_dataset_genus, "APC"), "APC", taxonomic_dataset_genus)
    ) %>%
    dplyr::select(-accepted_name_usage_ID_genus)
}
