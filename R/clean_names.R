




#' Find taxonomic alignments for a list of names to a version of the Australian Plant Census (APC) through standardizing formatting and checking for spelling issues
#'
#' This function uses Australian Plant Census (APC) & the Australian Plant Name Index (APNI) to find taxonomic alignments for a list of names.
#'
#' @param original_name A list of names to query for taxonomic alignments.
#' @param output (optional) The name of the file to save the results to.
#' @param max_distance_abs The absolute distance in substitution space for the fuzzy matching.
#' @param max_distance_rel The relative distance in substitution space for the fuzzy matching.
#' @param resources the taxonomic resources used to align the taxa names. Loading this can be slow,
#' so call \code{\link{load_taxonomic_resources}} separately to greatly speed this function up and pass the resources in.
#'
#' @return A tibble with columns: original_name, cleaned_name, aligned_name, source, known, and checked.
#' @export
#'
#' @examples
#' align_taxa(c("Poa annua", "Abies alba"))
#'
#' @importFrom readr read_csv cols col_logical col_character
#' @importFrom tibble tibble
#'
#' @keywords taxonomic alignments, APC, APNI, flora resource
#'
#' @seealso
#' \code{\link{load_taxonomic_resources}}
#'
#' @family taxonomic alignment functions
#'
#' @rdname align_taxa
#'
align_taxa <- function(original_name,
                       output = NULL,
                       max_distance_abs = 3,
                       max_distance_rel = 0.2,
                       resources = load_taxonomic_resources()) {
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
        still_to_match = "needs_match",
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
  taxa <- match_taxa(taxa, resources)
  
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
#'   \item \code{taxonomicStatusClean}: the taxonomic status of the updated taxon.
#'   \item \code{alternativeTaxonomicStatusClean}: the alternative taxonomic status for the input name, if any.
#'   \item \code{acceptedNameUsageID}: the unique identifier for the accepted name of the input name.
#'   \item \code{canonicalName}: the accepted scientific name for the input name.
#'   \item \code{scientificNameAuthorship}: the authorship information for the accepted name.
#'   \item \code{taxonRank}: the taxonomic rank of the accepted name.
#'   \item \code{taxonomicStatus}: the taxonomic status of the accepted name.
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
#' update_taxonomy(c("Eucalyptus pauciflora", "Acacia victoriae"))
#'
#' # Update taxonomy for two plant names and save the result to a CSV file
#' update_taxonomy(c("Eucalyptus pauciflora", "Acacia victoriae"), output = "updated_taxonomy.csv")
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
    tibble::tibble(aligned_name = aligned_names) %>%
    # match names against names in APC list
    dplyr::left_join(
      by = "aligned_name",
      resources$APC %>% dplyr::filter(!grepl("sp\\.$", canonicalName)) %>%
        dplyr::select(
          aligned_name = canonicalName,
          taxonIDClean = taxonID,
          taxonomicStatusClean = taxonomicStatus,
          acceptedNameUsageID
        )
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(source = ifelse(!is.na(taxonIDClean), "APC", NA)) %>%
    # Now find accepted names for each name in the list (sometimes they are the same)
    dplyr::left_join(
      by = "acceptedNameUsageID",
      resources$APC %>%
        dplyr::filter(taxonomicStatus == "accepted") %>%
        dplyr::select(
          acceptedNameUsageID,
          canonicalName,
          taxonomicStatus,
          scientificNameAuthorship,
          family,
          subclass,
          taxonDistribution,
          taxonRank,
          ccAttributionIRI
        )
    ) %>%
    # Some species have multiple matches. We will prefer the accepted usage, but record others if they exists
    # To do this we define the order we want variables to sort by,m with accepted at the top
    dplyr::mutate(my_order =  forcats::fct_relevel(
      taxonomicStatusClean,
      subset(preferred_order, preferred_order %in%  taxonomicStatusClean)
    )) %>%
    dplyr::arrange(aligned_name, my_order) %>%
    # For each species, keep the first record (accepted if present) and
    # record any alternative status to indicate where there was ambiguity
    dplyr::group_by(aligned_name) %>%
    dplyr::mutate(
      alternativeTaxonomicStatusClean = ifelse(
        taxonomicStatusClean[1] == "accepted",
        taxonomicStatusClean %>% unique() %>%  subset(. , . != "accepted") %>% paste0(collapse = " | ") %>% dplyr::na_if(""),
        NA
      )
    ) %>%
    #dplyr::slice(1:5) %>%
    dplyr::filter(taxonomicStatusClean != "misapplied") %>%
    dplyr::ungroup() %>%
    dplyr::select(
      aligned_name,
      source,
      taxonIDClean,
      taxonomicStatusClean,
      alternativeTaxonomicStatusClean,
      acceptedNameUsageID,
      canonicalName,
      scientificNameAuthorship,
      taxonRank,
      taxonomicStatus,
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
    dplyr::filter(is.na(canonicalName))  %>%
    dplyr::select(aligned_name) %>%
    dplyr::left_join(
      by = "aligned_name",
      resources$APNI %>% dplyr::filter(nameElement != "sp.") %>%
        dplyr::select(
          aligned_name = canonicalName,
          taxonIDClean = scientificNameID,
          family,
          taxonRank
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
      canonicalName = ifelse(is.na(taxonIDClean), NA, aligned_name),
      taxonomicStatusClean = ifelse(is.na(taxonIDClean), "unknown", "unplaced"),
      taxonomicStatus = taxonomicStatusClean
    ) %>%
    dplyr::filter(!is.na(taxonIDClean))
  
  # if matches in APC and APNI, combine these and return
  if (nrow(taxa_APNI) > 0) {
    taxa_out <-
      dplyr::bind_rows(taxa_APC,
                       taxa_APNI) %>%
      dplyr::arrange(aligned_name) %>%
      dplyr::distinct()
  }
  # if matches only in APC, just return this
  else {
    taxa_out <- taxa_APC %>%
      dplyr::arrange(aligned_name) %>%
      dplyr::distinct()
  }
  
  if (!is.null(output)) {
    readr::write_csv(taxa_out, output)
    message("  - output saved in file: ", output)
  }
  taxa_out
}


#' Strip taxonomic names of subtaxa designations and special characters
#'
#' Given a vector of taxonomic names, this function removes subtaxa designations (e.g., "subsp."),
#' special characters (e.g., "-", ".", "(", ")", "?"), and extra whitespace. The resulting vector
#' of names is also converted to lowercase.
#'
#' @param taxon_names A character vector of taxonomic names to be stripped.
#'
#' @return A character vector of stripped taxonomic names, with subtaxa designations, special
#' characters, and extra whitespace removed, and all letters converted to lowercase.
#'
#'
#'
#' @examples
#' strip_names(c("Abies lasiocarpa subsp. lasiocarpa",
#'               "Quercus kelloggii",
#'               "Pinus contorta var. latifolia"))
#'
#' @noRd
strip_names <- function(taxon_names) {
  taxon_names %>%
    stringr::str_replace_all("[:punct:]", " ") %>%
    stringr::str_replace_all(" subsp ", " ") %>%
    stringr::str_replace_all(" var |var$", " ") %>%
    stringr::str_replace_all(" ser ", " ") %>%
    stringr::str_replace_all(" f ", " ") %>%
    stringr::str_replace_all(" s l ", " ") %>%
    stringr::str_replace_all(" s s ", " ") %>%
    stringr::str_replace_all("\\=", " ") %>%
    stringr::str_replace_all("  ", " ") %>%
    stringr::str_squish() %>%
    tolower()
}

#' @noRd
strip_names_2 <- function(x) {
  x %>%
    stringr::str_replace_all("[:punct:]", " ") %>%
    stringr::str_replace_all(" subsp ", " ") %>%
    stringr::str_replace_all(" var | var$", " ") %>%
    stringr::str_replace_all(" ser ", " ") %>%
    stringr::str_replace_all(" f ", " ") %>%
    stringr::str_replace_all(" forma ", " ") %>%
    stringr::str_replace_all(" species ", " ") %>%
    stringr::str_replace_all(" s l ", " ") %>%
    stringr::str_replace_all(" s s ", " ") %>%
    stringr::str_replace_all(" ss ", " ") %>%
    stringr::str_replace_all(" x ", " ") %>%
    stringr::str_replace_all(" sp |sp $", " ") %>%
    stringr::str_replace_all(" sp1", " 1") %>%
    stringr::str_replace_all(" sp2", " 2") %>%
    stringr::str_replace_all(" ssp |ssp $", " ") %>%
    stringr::str_replace_all(" cf | cf$", " ") %>%
    stringr::str_replace_all("\\=", " ") %>%
    stringr::str_replace_all("  ", " ") %>%
    stringr::str_squish() %>%
    tolower()
}



#' Standardise Taxon Names
#'
#' This function standardises taxon names by performing a series of text substitutions to remove common inconsistencies in taxonomic nomenclature. The function takes a character vector of taxon names as input and returns a character vector of standardised taxon names as output.
#'
#' @param taxon_names A character vector of taxon names that need to be standardised.
#'
#' @return A character vector of standardised taxon names.
#'
#'
#' @examples
#' standardise_names(c("Abies alba Mill.",
#'                     "Quercus suber",
#'                     "Eucalyptus sp.",
#'                     "Agave americana var. marginata"))
#' @noRd
standardise_names <- function(taxon_names) {
  f <- function(x, find, replace) {
    gsub(find, replace, x, perl = TRUE)
  }
  
  taxon_names %>%
    ## for hybrid markers
    stringi::stri_trans_general("Any-Latin; Latin-ASCII") %>%
    f("\\*", "x") %>%
    ## Weird formatting
    f("[\\n\\t]", " ") %>%
    
    ## Capitalise first letter
    f("^([a-z])", "\\U\\1") %>%
    
    ## sp. not sp or spp
    f("\\ssp(\\s|$)", " sp.\\1") %>%
    f("\\sspp.(\\s|$)", " sp.\\1") %>%
    f("\\sspp(\\s|$)", " sp.\\1") %>%
    f("\\sspp(\\s|$)", " sp.\\1") %>%
    
    ## subsp. not ssp, ssp., subsp or sub sp.
    f("\\sssp(\\s|$)", " subsp.\\1") %>%
    f("\\sssp.(\\s|$)", " subsp.\\1") %>%
    f("\\ssubsp(\\s|$)", " subsp.\\1") %>%
    f("\\ssub sp.(\\s|$)", " subsp.\\1") %>%
    
    ## var. not var
    f("\\svar(\\s|$)", " var.\\1") %>%
    
    ## aff. not affin, aff, affn
    f("\\saffin(\\s|$)", " aff.\\1") %>%
    f("\\saff(\\s|$)", " aff.\\1") %>%
    f("\\saffn(\\s|$)", " aff.\\1") %>%
    
    ## f. not forma
    f("\\sforma(\\s|$)", " f.\\1") %>%
    
    ## remove " ms" if present
    f("\\sms(\\s|$)", "\\1") %>%
    
    ## remove " s.l" or " s.s." if present
    f("\\ssl(\\s|$)", "\\1") %>%
    f("\\ss\\.l\\.(\\s|$)", "\\1") %>%
    f("\\sss(\\s|$)", "") %>%
    f("\\ss\\.s\\.(\\s|$)", "\\1") %>%
    
    ## clean white space
    stringr::str_squish()
}


#' Create a lookup table to help fix the taxonomy for a list of Australian plant species
#'
#' This function takes a list of Australian plant species that needs to be reconciled with current taxonomy and generates a lookup table to help fix the taxonomy. The lookup table contains the original species names, the aligned species names, and additional taxonomic information such as taxon IDs and genera.
#'
#' @family taxonomic alignment functions
#'
#' @param taxa A list of Australian plant species that needs to be reconciled with current taxonomy.
#' @param stable_or_current_data either "stable" for a consistent version, or "current" for the leading edge version.
#' @param version The version number of the dataset to use. Default is \code{"0.0.1.9000"}.
#' @param full logical for whether the full lookup table is returned or just the two key columns
#' @param resources These are the taxonomic resources used for cleaning, this will default to loading them from a local place on your computer.  If this is to be called repeatedly, it's much faster to load the resources using \code{\link{load_taxonomic_resources}} seperately and pass the data in.
#' @return A lookup table containing the original species names, the aligned species names, and additional taxonomic information such as taxon IDs and genera.
#' @export
#'
#' @seealso \code{\link{load_taxonomic_resources}}
#' @examples
#' resources <- load_taxonomic_resources()
#' create_taxonomic_update_lookup(c("Eucalyptus regnans",
#'                                  "Acacia melanoxylon",
#'                                  "Banksia integrifolia",
#'                                  "Not a species"),
#'                                  resources=resources)
#'
create_taxonomic_update_lookup <-
  function(taxa,
           stable_or_current_data = "stable",
           version = default_version(),
           full = FALSE,
           resources = load_taxonomic_resources(stable_or_current_data =
                                                  stable_or_current_data, version = version)) {
    aligned_data <-
      unique(taxa) %>%
      align_taxa(resources = resources)
    
    aligned_species_list_tmp <-
      aligned_data$aligned_name %>% update_taxonomy(resources = resources)
    
    aligned_species_list <-
      aligned_data %>% dplyr::select(original_name, aligned_name, aligned_reason) %>%
      dplyr::left_join(aligned_species_list_tmp,
                       by = c("aligned_name"),
                       multiple = "all") %>% # todo: consider implications
      dplyr::filter(!is.na(taxonIDClean)) %>%
      dplyr::mutate(genus = stringr::word(canonicalName, 1, 1)) %>%
      dplyr::rename(canonical_name = canonicalName)
    
    if (full == TRUE) {
      return(aligned_species_list)
    }
    if (full == FALSE) {
      return(
        dplyr::select(
          aligned_species_list,
          original_name,
          aligned_name,
          apc_name = canonical_name,
          aligned_reason,
          taxonomic_status_of_aligned_name = taxonomicStatusClean
        ) %>%
          distinct()
      )
    }
  }


#not working yet
find_mrct <- function(taxa,
                      stable_or_current_data = "stable",
                      version = default_version(),
                      resources = load_taxonomic_resources(stable_or_current_data =
                                                             stable_or_current_data, version = version)) {
  only_taxa_of_interest <-
    dplyr::filter(resources$APC, resources$APC$canonicalName %in% taxa)
  if (length(unique(only_taxa_of_interest$canonicalName)) == 1)
    return(only_taxa_of_interest$canonicalName[1])
  if (length(unique(stringr::word(
    only_taxa_of_interest$canonicalName, 1, 2
  ))) == 1)
    return(stringr::word(only_taxa_of_interest$canonicalName, 1, 2))
  if (length(unique(stringr::word(
    only_taxa_of_interest$canonicalName, 1, 1
  ))) == 1)
    return(paste0(
      stringr::word(only_taxa_of_interest$canonicalName[1], 1, 1),
      " sp."
    ))
  if (length(unique(only_taxa_of_interest$family)) == 1)
    return(only_taxa_of_interest$family[1])
  return("plants")
}
