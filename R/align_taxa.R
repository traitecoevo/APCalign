#' Find taxonomic alignments for a list of names to a version of the Australian Plant Census (APC) through standardizing formatting and checking for spelling issues
#'
#' This function uses Australian Plant Census (APC) & the Australian Plant Name Index (APNI) to find taxonomic alignments for a list of names.
#'
#' @param original_name A list of names to query for taxonomic alignments.
#' @param output (optional) The name of the file to save the results to.
#' @param resources the taxonomic resources used to align the taxa names. Loading this can be slow,
#' so call \code{\link{load_taxonomic_resources}} separately to greatly speed this function up and pass the resources in.
#' @param fuzzy_abs_dist The number of characters allowed to be different for a fuzzy match.
#' @param fuzzy_rel_dist The proportion of characters allowed to be different for a fuzzy match. 
#' @param fuzzy_matches Fuzzy matches are turned on as a default. The relative and absolute distances allowed for fuzzy matches to species and infraspecific taxon names are defined by the parameters `fuzzy_abs_dist` and `fuzzy_rel_dist`
#' @param imprecise_fuzzy_matches Imprecise fuzzy matches are turned off as a default.
#' @param APNI_matches Name matches to the APNI (Australian Plant Names Index) are turned off as a default. 
#' @param dataset_id Identifier for dataset, location, or list of names.
#'
#' @return A tibble with columns: original_name, cleaned_name, aligned_name, source, known, and checked.
#' @export
#'
#' @examples
#' \donttest{align_taxa(c("Poa annua", "Abies alba"))}
#'
#' @importFrom readr read_csv cols col_logical col_character
#' @importFrom tibble tibble
#'
#'
#' @seealso
#' \code{\link{load_taxonomic_resources}}
#'
#' @family taxonomic alignment functions
#'
#' @rdname align_taxa
#'
#' 
align_taxa <- function(original_name,
                       output = NULL,
                       resources = load_taxonomic_resources(),
                       fuzzy_abs_dist = 3, 
                       fuzzy_rel_dist = 0.2, 
                       fuzzy_matches = TRUE, 
                       imprecise_fuzzy_matches = FALSE, 
                       APNI_matches = FALSE,
                       dataset_id = "XYZ") {
  original_name <- unique(original_name[!is.na(original_name)])
  
  message("Checking alignments of ", length(original_name), " taxa\n")
  
  if (!is.null(output) && file.exists(output)) {
    message("  - reading existing data from ", output)
    
    taxa_raw <-
      readr::read_csv(
        output,
        col_types = readr::cols(
          checked = readr::col_logical(),
          known = readr::col_logical(),
          .default = readr::col_character()
        )
      )
    
    # TODO: check taxa_ raw has correct columns
  }
  else {
    taxa_raw <-
      tibble::tibble(
        original_name = character(0L),
        cleaned_name = character(0L),
        aligned_name = character(0L),
        source = character(0L),
        known = logical(0L),
        checked = logical(0L)
      )
  }
  
  # create list, will have two elements: tocheck, checked
  taxa <- list()
  
  taxa[["tocheck"]] <-
    dplyr::bind_rows(
      taxa_raw,
      tibble::tibble(
        original_name = subset(original_name, !original_name %in% taxa_raw$original_name) %>% unique(),
        cleaned_name = NA_character_,
        stripped_name = NA_character_,
        stripped_name2 = NA_character_,
        trinomial = NA_character_,
        binomial = NA_character_,
        genus = NA_character_,
        aligned_name = NA_character_,
        aligned_reason = NA_character_,
        fuzzy_match_genus = NA_character_,
        fuzzy_match_genus_known = NA_character_,
        fuzzy_match_genus_APNI = NA_character_,
        fuzzy_match_binomial = NA_character_,
        fuzzy_match_binomial_APC_known = NA_character_,
        fuzzy_match_trinomial = NA_character_,
        fuzzy_match_trinomial_known = NA_character_,
        fuzzy_match_cleaned_APC = NA_character_,
        fuzzy_match_cleaned_APC_known = NA_character_,
        fuzzy_match_cleaned_APNI = NA_character_,
        fuzzy_match_cleaned_APC_imprecise = NA_character_,
        fuzzy_match_cleaned_APC_known_imprecise = NA_character_,
        fuzzy_match_cleaned_APNI_imprecise = NA_character_,
        taxonomic_ref = NA_character_,
        taxonomic_resolution = NA_character_,
        alignment_code = NA_character_,
        checked = FALSE,
        known = FALSE
      )
    )
  
  if (all(taxa$tocheck$checked)) {
    message("  - all taxa are already checked, yay!")
    return(invisible(taxa$tocheck))
  }
  
  # move all checked taxa to "checked"
  taxa <- redistribute(taxa)
  
  # check unknown taxa
  message(
    "  -> ",
    crayon::blue(sum(taxa$tocheck$known, na.rm = T)),
    " names already matched; ",
    crayon::blue(sum(
      taxa$tocheck$checked &
        !taxa$tocheck$known,
      na.rm = T
    )),
    " names checked but without a match; ",
    crayon::blue(sum(!taxa$tocheck$checked)),
    " taxa yet to be checked"
  )
  
  # do the actual matching
  taxa <- match_taxa(taxa, resources, fuzzy_abs_dist, fuzzy_rel_dist, fuzzy_matches, imprecise_fuzzy_matches, APNI_matches = FALSE, dataset_id)
  
  taxa_out <- dplyr::bind_rows(taxa) %>%
    dplyr::mutate(known = !is.na(aligned_name))
  
  if (!is.null(output)) {
    dir.create(dirname(output), FALSE, TRUE)
    readr::write_csv(taxa_out, output)
    message("  - output saved in file: ", output)
  }
  taxa_out
}

# function moves taxa from tocheck to checked
redistribute <- function(data) {
  data[["checked"]] <- dplyr::bind_rows(data[["checked"]],
                                        data[["tocheck"]] %>% dplyr::filter(checked))
  
  data[["tocheck"]] <-
    data[["tocheck"]] %>% dplyr::filter(!checked)
  data
}

