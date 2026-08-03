#' @title Match taxonomic names to names in the APC/APNI
#'
#' @description
#' This function attempts to match input strings to Australia's reference lists
#' for vascular plants, the APC and APNI. It attempts:
#' 1. perfect matches and fuzzy matches
#' 2. matches to infraspecies, species, genus, and family names
#' 3. matches to the entire input string and subsets there-of
#' 4. searches for string patterns that suggest a specific taxon rank
#'
#' @details
#' - It cycles through more than 20 different string patterns, sequentially
#'  searching for additional match patterns.
#' - It identifies string patterns in input names that suggest a name can only be
#'  aligned to a genus (hybrids that are not accepted names; graded species;
#'  taxa not identified to species).
#' - It prioritises matches that do not require fuzzy matching (i.e. synonyms,
#'  orthographic variants) over those that do.
#' - If prioritises matches to taxa in the APC over names in the APNI.
#'
#' Each match step has the same shape: build a logical index `i` of the rows
#' it can resolve, hand those rows to [apply_match()] to be stamped with the
#' alignment and moved out of `tocheck`, then stop early if nothing is left.
#' The order of the match steps *is* the matching algorithm, so they must not
#' be reordered without re-checking the alignment benchmarks.
#'
#' @param taxa The list of taxa requiring checking
#
#' @param resources The list(s) of accepted names to check against,
#'  loaded through the function `load_taxonomic_resources()`
#' @param fuzzy_abs_dist The number of characters allowed to be different
#'  for a fuzzy match.
#' @param fuzzy_rel_dist The proportion of characters allowed to be different
#'  for a fuzzy match.
#' @param fuzzy_matches Fuzzy matches are turned on as a default. The relative
#'  and absolute distances allowed for fuzzy matches to species and
#'  infraspecific taxon names are defined by the parameters
#' `fuzzy_abs_dist` and `fuzzy_rel_dist`
#' @param imprecise_fuzzy_matches Imprecise fuzzy matches uses the fuzzy
#'  matching function with lenient levels set (absolute distance of
#'  5 characters; relative distance = 0.25).
#'  It offers a way to get a wider range of possible names, possibly
#'  corresponding to very distant spelling mistakes. This is FALSE as default
#'  and all outputs should be checked as it often makes erroneous matches.
#' @param APNI_matches Name matches to the APNI (Australian Plant Names Index)
#'  are turned off as a default.
#' @param identifier A dataset, location or other identifier,
#'  which defaults to NA.
#'
#' @noRd
match_taxa <- function(
    taxa,
    resources,
    fuzzy_abs_dist = 3,
    fuzzy_rel_dist = 0.2,
    fuzzy_matches = TRUE,
    imprecise_fuzzy_matches = FALSE,
    APNI_matches = TRUE,
    identifier = NA_character_
) {

  if(is.null(resources)){
    message("Not finding taxonomic resources; check internet connection?")
    return(NULL)
  }

  update_na_with <- function(current, new) {
    ifelse(is.na(current), new, current)
  }


  ## A function that specifies particular fuzzy matching conditions (for the
  ## function fuzzy_match) when matching is being done at the genus level.
  if (fuzzy_matches == TRUE) {
    fuzzy_match_genera <- function(x, y) {
      purrr::map_chr(x, ~ fuzzy_match(.x, y, 2, 0.35, n_allowed = 1))
    }
  } else {
    fuzzy_match_genera <- function(x, y) {
      purrr::map_chr(x, ~ fuzzy_match(.x, y, 0, 0.0, n_allowed = 1))
    }
  }

  ## set default imprecise fuzzy matching parameters
  imprecise_fuzzy_abs_dist <- 5
  imprecise_fuzzy_rel_dist <- 0.25

  ## override all fuzzy matching parameters with absolute and
  ## relative distances of 0 if fuzzy matching is turned off
  if (fuzzy_matches == FALSE) {
    fuzzy_abs_dist <- 0
    fuzzy_rel_dist <- 0
    imprecise_fuzzy_abs_dist <- 0
    imprecise_fuzzy_rel_dist <- 0
  }

  ## remove APNI-listed genera from resources if APNI matches are turned off
  ##(the default)
  if (APNI_matches == TRUE) {
    resources$genera_all2 <- resources$genera_all
  } else {
    resources$genera_all2 <- resources$genera_all %>% dplyr::filter(taxonomic_dataset != "APNI")
  }

  ## String patterns that mark a name as resolvable only to genus, however well
  ## the rest of the name matches. Applied to `cleaned_name`, and re-evaluated
  ## by each match step because `tocheck` shrinks as matches are found.
  is_genus_sp <- function(x) {
    stringr::str_detect(x, "[:space:]sp\\.$") & word(x, 2) %in% "sp."
  }

  is_intergrade <- function(x) stringr::str_detect(x, "\\ -- |\\--")

  is_hybrid <- function(x) stringr::str_detect(x, " [xX] ")

  is_indecision <- function(x) {
    (
      stringr::str_detect(x, "[:alpha:]\\/") |
        stringr::str_detect(x, "\\s\\/")
    ) &
      !stringr::str_detect(x, "[:digit:]") &
      !stringr::str_detect(x, "\\(") &
      !stringr::str_detect(x, "\\'")
  }

  ## `cf.` is only recognised by the exact-genus step (match_06a); the fuzzy
  ## fall-backs below it look for `aff.`/`affinis` alone.
  ##
  ## A bare `affinis` at the end of a name, or one qualified by a rank marker,
  ## is the species epithet `affinis` rather than an affinity qualifier, so
  ## neither counts as affinity here. `standardise_names()` leaves both alone
  ## for the same reason; see `not_before_rank_marker`.
  affinis_qualifier <- paste0(" affinis", not_before_rank_marker, "\\s")

  has_affinity_or_cf <- function(x) {
    stringr::str_detect(x, "[Aa]ff[\\.\\s]") |
      stringr::str_detect(x, affinis_qualifier) |
      stringr::str_detect(x, " cf[\\.\\s]")
  }

  has_affinity <- function(x) {
    stringr::str_detect(x, "[Aa]ff[\\.\\s]") |
      stringr::str_detect(x, affinis_qualifier)
  }

  ## Explanations shared between the exact and fuzzy variants of each
  ## genus-only pattern, spelled out once so the wording cannot drift.
  intergrade_note <- "Taxon name includes '--' (double dash) indicating an intergrade between two taxa and taxon can only be aligned to genus-rank"
  indecision_note <- "Taxon name includes '/' (slash) indicating an uncertain species identification but an accepted genus and taxon can only be aligned to genus-rank"
  affinity_note   <- "Taxon name includes 'affinis' or 'aff' indicating an unknown taxon that bears an affinity to a different taxon in the same genus and taxon can only be aligned to genus-rank"
  hybrid_note     <- "Taxon name includes ' x ' indicating a hybrid taxon and taxon can only be aligned to genus-rank"

  ## Repeatedly used identifier strings are created.
  ## These identifier strings are added to the aligned names of taxa that do
  ## not match to an APC or APNI species or infra-specific level name.
  taxa$tocheck <- taxa$tocheck %>%
    dplyr::mutate(
      identifier_string = ifelse(is.na(identifier), NA_character_, paste0(" [", identifier, "]")),
      identifier_string2 = ifelse(is.na(identifier), NA_character_, paste0("; ", identifier))
    )

  ## In the tocheck dataframe, add columns with manipulated versions of the string to match
  ## Various stripped versions of the string to match, versions with 1, 2 and 3 words (genus, binomial, trinomial), and fuzzy-matched genera are propagated.
  taxa$tocheck <- taxa$tocheck %>%
    dplyr::mutate(
      cleaned_name = cleaned_name %>%
        update_na_with(standardise_names(original_name)),
      stripped_name = stripped_name %>%
        update_na_with(strip_names(cleaned_name)),
      stripped_name2 = stripped_name2 %>%
        update_na_with(strip_names_extra(stripped_name)),
      trinomial = word(stripped_name2, start = 1, end = 3),
      binomial = word(stripped_name2, start = 1, end = 2),
      genus = extract_genus(original_name)
    )

  ## Taxa that have been checked are moved from `taxa$tocheck` to `taxa$checked`
  ## by `apply_match()`. After each match step, stop as soon as nothing is left
  ## to check; `drop_scratch()` removes the columns used only while matching.

  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # START MATCHES
  # match_01a: Scientific name matches
  # Taxon names that are an accepted scientific name, with authorship.

  taxa <- match_reference_name(
    taxa, resources$APC_accepted,
    key = "original_name", name_type = "scientific_name", taxonomic_dataset = "APC",
    aligned_reason = "Exact match of taxon name to an APC-accepted scientific name (including authorship)",
    alignment_code = "match_01a_accepted_scientific_name_with_authorship"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_01b: Scientific name matches
  # Taxon names that are an APC-known scientific name, with authorship.

  taxa <- match_reference_name(
    taxa, resources$APC_synonyms,
    key = "original_name", name_type = "scientific_name", taxonomic_dataset = "APC",
    aligned_reason = "Exact match of taxon name to an APC-known scientific name (including authorship)",
    alignment_code = "match_01b_synonym_scientific_name_with_authorship"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_01c: APC-accepted canonical name
  # Taxon names that are exact matches to APC-accepted canonical names, once filler words and punctuation are removed.

  taxa <- match_reference_name(
    taxa, resources$APC_accepted,
    key = "cleaned_name", name_type = "canonical_name", taxonomic_dataset = "APC",
    aligned_reason = "Exact match of taxon name to an APC-accepted canonical name once punctuation and filler words are removed",
    alignment_code = "match_01c_accepted_canonical_name"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_01d: APC-known canonical name
  # Taxon names that are exact matches to APC-known canonical names, once filler words and punctuation are removed.

  taxa <- match_reference_name(
    taxa, resources$APC_synonyms,
    key = "cleaned_name", name_type = "canonical_name", taxonomic_dataset = "APC",
    aligned_reason = "Exact match of taxon name to an APC-known canonical name once punctuation and filler words are removed",
    alignment_code = "match_01d_synonym_canonical_name"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_02a: Genus-level resolution
  # Exact matches of APC-accepted or APC-known genus for names where the final "word" is `sp` or `spp`
  # Aligned name includes identifier to indicate `genus sp.` refers to a specific species (or infra-specific taxon), associated with a specific dataset/location.

  i <-
    is_genus_sp(taxa$tocheck$cleaned_name) &
    taxa$tocheck$genus %in% resources$genera_all2$genus

  ii <- match(taxa$tocheck$genus[i], resources$genera_all2$genus)
  matched_dataset <- resources$genera_all2$taxonomic_dataset[ii]

  taxa <- apply_match(
    taxa, i,
    taxonomic_dataset = matched_dataset,
    taxon_rank = "genus",
    aligned_name = genus_sp_name(taxa, i, resources$genera_all2$genus[ii]),
    aligned_reason = paste0("Exact match of taxon name ending with `sp.` to an ", matched_dataset, " genus"),
    alignment_code = "match_02a_exact_genus_accepted_or_synonym"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # Add some extra columns - checking for fuzzy matches in genus and family
  # Not including this above, as fuzzy matching is slow
  taxa$tocheck <- taxa$tocheck %>%
    dplyr::mutate(
      fuzzy_match_genus =
        fuzzy_match_genera(genus, resources$genera_accepted$genus),
      fuzzy_match_genus_synonym =
        fuzzy_match_genera(genus, resources$genera_synonym$genus),
      fuzzy_match_genus_APNI =
        fuzzy_match_genera(genus, resources$genera_APNI$genus)
    )

  # match_02b: Genus-level resolution
  # Fuzzy matches of APC accepted genera for names where the final "word" is `sp` or `spp` and
  # there isn't an exact match to an APC accepted genus name
  # Aligned name includes identifier to indicate `genus sp.` refers to a specific species (or infra-specific taxon), associated with a specific dataset/location.

  i <-
    is_genus_sp(taxa$tocheck$cleaned_name) &
    taxa$tocheck$fuzzy_match_genus %in% resources$genera_accepted$genus

  ii <- match(taxa$tocheck$fuzzy_match_genus[i], resources$genera_accepted$genus)

  taxa <- apply_match(
    taxa, i,
    taxonomic_dataset = resources$genera_accepted$taxonomic_dataset[ii],
    taxon_rank = "genus",
    aligned_name = genus_sp_name(taxa, i, resources$genera_accepted$genus[ii]),
    aligned_reason = "Fuzzy match of taxon name ending with `sp.` to an APC-accepted genus",
    alignment_code = "match_02b_fuzzy_genus_accepted"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_02c: Genus-level resolution
  # Fuzzy matches of APC synonymous genera for names where the final "word" is `sp` or `spp` and
  # there isn't an exact match to an APC synonymous genus name.
  # Aligned name includes identifier to indicate `genus sp.` refers to a specific species (or infra-specific taxon), associated with a specific dataset/location.

  i <-
    is_genus_sp(taxa$tocheck$cleaned_name) &
    taxa$tocheck$fuzzy_match_genus_synonym %in% resources$genera_synonym$genus

  ii <- match(taxa$tocheck$fuzzy_match_genus_synonym[i], resources$genera_synonym$genus)

  taxa <- apply_match(
    taxa, i,
    taxonomic_dataset = resources$genera_synonym$taxonomic_dataset[ii],
    taxon_rank = "genus",
    aligned_name = genus_sp_name(taxa, i, resources$genera_synonym$genus[ii]),
    aligned_reason = "Fuzzy match of taxon name ending with `sp.` to an APC-known genus",
    alignment_code = "match_02c_fuzzy_genus_synonym"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_02d: Family-level resolution
  # Exact matches of APC-accepted family for names where the final "word" is `sp` or `spp`.
  # Aligned name includes identifier to indicate `family sp.` refers to a specific species (or infra-specific taxon), associated with a specific dataset/location.

  i <-
    is_genus_sp(taxa$tocheck$cleaned_name) &
    taxa$tocheck$genus %in% resources$family_accepted$canonical_name

  taxa <- apply_match(
    taxa, i,
    taxonomic_dataset = "APC",
    taxon_rank = "family",
    aligned_name = genus_sp_name(taxa, i, taxa$tocheck$genus[i]),
    aligned_reason = "Exact match of taxon name ending with `sp.` to an APC-accepted family",
    alignment_code = "match_02d_exact_family_accepted"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_03a: Intergrade taxon
  # Exact match to APC-accepted or APNI-listed genus for taxon names where a double hyphen indicates the plant is an intergrade.
  # For taxon names the fitting pattern, `genus species_A -- species_B` (intergrade) automatically align to genus,
  # since this is the highest taxon rank that can be attached to the plant name

  i <-
    is_intergrade(taxa$tocheck$cleaned_name) &
    taxa$tocheck$genus %in% resources$genera_all2$genus

  ii <- match(taxa$tocheck$genus[i], resources$genera_all2$genus)
  matched_dataset <- resources$genera_all2$taxonomic_dataset[ii]

  taxa <- apply_match(
    taxa, i,
    taxonomic_dataset = matched_dataset,
    taxon_rank = "genus",
    aligned_name = higher_rank_name(taxa, i, resources$genera_all2$genus[ii]),
    aligned_reason = paste0("Exact match to ", matched_dataset, " genus. ", intergrade_note),
    alignment_code = "match_03a_intergrade_accepted_or_synonym_genus"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_03b: Intergrade taxon, APC-accepted fuzzy
  # Fuzzy match to APC-accepted genus for taxon names where a double hyphen indicates the plant is an intergrade.

  i <-
    is_intergrade(taxa$tocheck$cleaned_name) &
    taxa$tocheck$fuzzy_match_genus %in% resources$genera_accepted$genus

  taxa <- apply_match(
    taxa, i,
    taxonomic_dataset = "APC",
    taxon_rank = "genus",
    aligned_name = higher_rank_name(taxa, i, taxa$tocheck$fuzzy_match_genus[i]),
    aligned_reason = paste0("Fuzzy match to APC-accepted genus. ", intergrade_note),
    alignment_code = "match_03b_intergrade_fuzzy_accepted_genus"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_03c: Intergrade matches, APC-known fuzzy
  # Fuzzy match to APC-known genus for taxon names where a double hyphen indicates the plant is an intergrade.

  i <-
    is_intergrade(taxa$tocheck$cleaned_name) &
    taxa$tocheck$fuzzy_match_genus_synonym %in% resources$genera_synonym$genus

  taxa <- apply_match(
    taxa, i,
    taxonomic_dataset = "APC",
    taxon_rank = "genus",
    aligned_name = higher_rank_name(taxa, i, taxa$tocheck$fuzzy_match_genus_synonym[i]),
    aligned_reason = paste0("Fuzzy match to APC-known genus. ", intergrade_note),
    alignment_code = "match_03c_intergrade_fuzzy_synonym_genus"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_03d: Intergrade matches, APNI-listed fuzzy
  # Fuzzy match to APNI-listed genus for taxon names where a double hyphen indicates the plant is an intergrade.
  if (APNI_matches == TRUE) {
    i <-
      is_intergrade(taxa$tocheck$cleaned_name) &
      taxa$tocheck$fuzzy_match_genus_APNI %in% resources$genera_APNI$genus

    taxa <- apply_match(
      taxa, i,
      taxonomic_dataset = "APNI",
      taxon_rank = "genus",
      aligned_name = higher_rank_name(taxa, i, taxa$tocheck$fuzzy_match_genus_APNI[i]),
      aligned_reason = paste0("Fuzzy match to APNI-listed genus. ", intergrade_note),
      alignment_code = "match_03d_intergrade_fuzzy_APNI_genus"
    )
    if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))
  }

  # match_03e: Intergrade with unknown genus
  # Neither perfect nor fuzzy matches identify the genus.

  i <-
    is_intergrade(taxa$tocheck$cleaned_name) &
    !taxa$tocheck$fuzzy_match_genus %in% resources$genera_all2$genus

  taxa <- apply_match(
    taxa, i,
    taxonomic_dataset = NA_character_,
    taxon_rank = NA_character_,
    aligned_name = NA_character_,
    aligned_reason = "Taxon name includes '--' (double dash) indicating an intergrade between two taxa, but exact and fuzzy matches fail to align to a genus in the APC or APNI",
    alignment_code = "match_03e_intergrade_unknown_genus"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_04a: Genus species_A / species_B
  # Exact match to APC-accepted or APNI-listed genus for taxon names where a slash ("/") indicates the author is uncertain of the proper taxon name
  # and can only identify the taxon to genus.

  i <-
    is_indecision(taxa$tocheck$cleaned_name) &
    taxa$tocheck$genus %in% resources$genera_all2$genus

  ii <- match(taxa$tocheck$genus[i], resources$genera_all2$genus)
  matched_dataset <- resources$genera_all2$taxonomic_dataset[ii]

  taxa <- apply_match(
    taxa, i,
    taxonomic_dataset = matched_dataset,
    taxon_rank = "genus",
    aligned_name = higher_rank_name(taxa, i, resources$genera_all2$genus[ii]),
    aligned_reason = paste0("Exact match to ", matched_dataset, " genus. ", indecision_note),
    alignment_code = "match_04a_indecision_accepted_or_synonym_genus"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_04b: Genus species_A / species_B
  # Fuzzy match to APC-accepted genus.

  i <-
    is_indecision(taxa$tocheck$cleaned_name) &
    taxa$tocheck$fuzzy_match_genus %in% resources$genera_accepted$genus

  taxa <- apply_match(
    taxa, i,
    taxonomic_dataset = "APC",
    taxon_rank = "genus",
    aligned_name = higher_rank_name(taxa, i, taxa$tocheck$fuzzy_match_genus[i]),
    aligned_reason = paste0("Fuzzy match to APC-accepted genus. ", indecision_note),
    alignment_code = "match_04b_indecision_fuzzy_accepted_genus"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_04c: Genus species_A / species_B
  # Fuzzy match to APC-known genus.

  i <-
    is_indecision(taxa$tocheck$cleaned_name) &
    taxa$tocheck$fuzzy_match_genus_synonym %in% resources$genera_synonym$genus

  taxa <- apply_match(
    taxa, i,
    taxonomic_dataset = "APC",
    taxon_rank = "genus",
    aligned_name = higher_rank_name(taxa, i, taxa$tocheck$fuzzy_match_genus_synonym[i]),
    aligned_reason = paste0("Fuzzy match to APC-known genus. ", indecision_note),
    alignment_code = "match_04c_indecision_fuzzy_synonym_genus"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_04d: Genus species_A / species_B
  # Fuzzy match to APNI-listed genus.
  if (APNI_matches == TRUE) {
    i <-
      is_indecision(taxa$tocheck$cleaned_name) &
      taxa$tocheck$fuzzy_match_genus_APNI %in% resources$genera_APNI$genus

    taxa <- apply_match(
      taxa, i,
      taxonomic_dataset = "APNI",
      taxon_rank = "genus",
      aligned_name = higher_rank_name(taxa, i, taxa$tocheck$fuzzy_match_genus_APNI[i]),
      aligned_reason = paste0("Fuzzy match to APNI-listed genus. ", indecision_note),
      alignment_code = "match_04d_indecision_fuzzy_APNI_genus"
    )
    if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))
  }

  # match_04e: Genus species_A / species_B
  # Neither perfect nor fuzzy matches identify the genus.

  i <-
    is_indecision(taxa$tocheck$cleaned_name) &
    !taxa$tocheck$fuzzy_match_genus %in% resources$genera_all2$genus

  taxa <- apply_match(
    taxa, i,
    taxonomic_dataset = NA_character_,
    taxon_rank = NA_character_,
    aligned_name = NA_character_,
    aligned_reason = "Taxon name includes '/' (slash) indicating an uncertain species identification  but exact and fuzzy matches fail to align to a genus in the APC or APNI",
    alignment_code = "match_04e_indecision_unknown_genus"
  )

  # Note:  -- Finished with checking genus sp. above, now continue with full species

  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_05a: fuzzy match to APC-accepted canonical name
  # Fuzzy match of taxon name to an APC-accepted canonical name, once filler words and punctuation are removed.

  taxa$tocheck$fuzzy_match_cleaned_APC <- fuzzy_match_column(
    taxa$tocheck$stripped_name, resources$APC_accepted$stripped_canonical,
    fuzzy_abs_dist, fuzzy_rel_dist
  )

  taxa <- match_reference_name(
    taxa, resources$APC_accepted,
    key = "fuzzy_match_cleaned_APC", name_type = "stripped_canonical", taxonomic_dataset = "APC",
    aligned_reason = "Fuzzy match of taxon name to an APC-accepted canonical name once punctuation and filler words are removed",
    alignment_code = "match_05a_fuzzy_accepted_canonical_name"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_05b: fuzzy match to APC-known canonical name
  # Fuzzy match of taxon name to an APC-known canonical name, once filler words and punctuation are removed.

  taxa$tocheck$fuzzy_match_cleaned_APC_synonym <- fuzzy_match_column(
    taxa$tocheck$stripped_name, resources$APC_synonyms$stripped_canonical,
    fuzzy_abs_dist, fuzzy_rel_dist
  )

  taxa <- match_reference_name(
    taxa, resources$APC_synonyms,
    key = "fuzzy_match_cleaned_APC_synonym", name_type = "stripped_canonical", taxonomic_dataset = "APC",
    aligned_reason = "Fuzzy match of taxon name to an APC-known canonical name once punctuation and filler words are removed",
    alignment_code = "match_05b_fuzzy_synonym_canonical_name"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_05c: APNI-listed canonical name
  # Taxon names that are exact matches to APNI-listed canonical names, once filler words and punctuation are removed.
  if (APNI_matches == TRUE) {
    taxa <- match_reference_name(
      taxa, resources$APNI_names,
      key = "cleaned_name", name_type = "canonical_name", taxonomic_dataset = "APNI",
      aligned_reason = "Exact match of taxon name to an APNI-listed canonical name once punctuation and filler words are removed",
      alignment_code = "match_05c_APNI_canonical_name"
    )
    if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))
  }

  # match_06a: `genus aff. species` and `genus cf. species`taxa
  # Exact match to APC-accepted or APC-known genus for names where "aff" indicates the taxon has an affinity to another taxon, but isn't the other taxon.
  # Similarly, "cf" indicates that a comparison should be made between the specific taxon and another taxon, but again, isn't the other taxon.
  # This alignment can only be made after exact matches of complete taxon names to APC/APNI + fuzzy matches to APC are complete,
  # because there are APC/APNI phrase names that include "sp. aff.".

  i <-
    has_affinity_or_cf(taxa$tocheck$cleaned_name) &
    taxa$tocheck$genus %in% resources$genera_all2$genus

  ii <- match(taxa$tocheck$genus[i], resources$genera_all2$genus)
  matched_dataset <- resources$genera_all2$taxonomic_dataset[ii]

  taxa <- apply_match(
    taxa, i,
    taxonomic_dataset = matched_dataset,
    taxon_rank = "genus",
    aligned_name = higher_rank_name(taxa, i, resources$genera_all2$genus[ii]),
    aligned_reason = paste0("Exact match to ", matched_dataset, " genus. ", affinity_note),
    alignment_code = "match_06a_species_affinis_APC_exact"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_06b: `genus aff. species` taxa
  # Fuzzy match to APC-accepted genus.

  i <-
    has_affinity(taxa$tocheck$cleaned_name) &
    taxa$tocheck$fuzzy_match_genus %in% resources$genera_accepted$genus

  taxa <- apply_match(
    taxa, i,
    taxonomic_dataset = "APC",
    taxon_rank = "genus",
    aligned_name = higher_rank_name(taxa, i, taxa$tocheck$fuzzy_match_genus[i]),
    aligned_reason = paste0("Fuzzy match to APC-accepted genus. ", affinity_note),
    alignment_code = "match_06b_species_affinis_APC_accepted_fuzzy"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_06c: `genus aff. species` taxa
  # Fuzzy match to APC-known genus.

  i <-
    has_affinity(taxa$tocheck$cleaned_name) &
    taxa$tocheck$fuzzy_match_genus_synonym %in% resources$genera_synonym$genus

  taxa <- apply_match(
    taxa, i,
    taxonomic_dataset = "APC",
    taxon_rank = "genus",
    aligned_name = higher_rank_name(taxa, i, taxa$tocheck$fuzzy_match_genus_synonym[i]),
    aligned_reason = paste0("Fuzzy match to APC-known genus. ", affinity_note),
    alignment_code = "match_06c_species_affinis_APC_synonym_fuzzy"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_06d: `genus aff. species` taxa
  # Fuzzy match to APNI-listed genus.
  if (APNI_matches == TRUE) {
    i <-
      has_affinity(taxa$tocheck$cleaned_name) &
      taxa$tocheck$fuzzy_match_genus_APNI %in% resources$genera_APNI$genus

    taxa <- apply_match(
      taxa, i,
      taxonomic_dataset = "APNI",
      taxon_rank = "genus",
      aligned_name = higher_rank_name(taxa, i, taxa$tocheck$fuzzy_match_genus_APNI[i]),
      aligned_reason = paste0("Fuzzy match to APNI-listed genus. ", affinity_note),
      alignment_code = "match_06d_species_affinis_APNI_fuzzy"
    )
    if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))
  }

  # match_06e: `genus aff. species` taxa
  # An exact or fuzzy genus-level match to APC & APNI genera cannot be made.

  i <-
    has_affinity(taxa$tocheck$cleaned_name) &
    !taxa$tocheck$fuzzy_match_genus %in% resources$genera_all2$genus

  taxa <- apply_match(
    taxa, i,
    taxonomic_dataset = NA_character_,
    taxon_rank = NA_character_,
    aligned_name = NA_character_,
    aligned_reason = "Taxon name includes 'affinis' or 'aff' indicating an unknown taxon that bears an affinity to a different taxon in the same genus,  but exact and fuzzy matches fail to align to a genus in the APC or APNI",
    alignment_code = "match_06e_species_affinis_unknown_genus"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_07a: imprecise fuzzy match
  # Imprecise fuzzy match of taxon name to an APC-accepted canonical name, once filler words and punctuation are removed.
  # For imprecise fuzzy matches, the taxon name can differ from the `APC-accepted` names by 5 characters & up to 25% of the string length.
  # These matches require individual review and are turned off as a default.
  if (imprecise_fuzzy_matches == TRUE) {
    taxa$tocheck$fuzzy_match_cleaned_APC_imprecise <- fuzzy_match_column(
      taxa$tocheck$stripped_name, resources$APC_accepted$stripped_canonical,
      imprecise_fuzzy_abs_dist, imprecise_fuzzy_rel_dist, epithet_letters = 2
    )

    taxa <- match_reference_name(
      taxa, resources$APC_accepted,
      key = "fuzzy_match_cleaned_APC_imprecise", name_type = "stripped_canonical", taxonomic_dataset = "APC",
      aligned_reason = "Imprecise fuzzy match of taxon name to an APC-accepted canonical name once punctuation and filler words are removed",
      alignment_code = "match_07a_imprecise_fuzzy_accepted_canonical_name"
    )
    if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))
  }

  # match_07b: imprecise fuzzy match
  # Imprecise fuzzy match of taxon name to an APC-known canonical name, once filler words and punctuation are removed.
  if (imprecise_fuzzy_matches == TRUE) {
    taxa$tocheck$fuzzy_match_cleaned_APC_synonym_imprecise <- fuzzy_match_column(
      taxa$tocheck$stripped_name, resources$APC_synonyms$stripped_canonical,
      imprecise_fuzzy_abs_dist, imprecise_fuzzy_rel_dist, epithet_letters = 2
    )

    taxa <- match_reference_name(
      taxa, resources$APC_synonyms,
      key = "fuzzy_match_cleaned_APC_synonym_imprecise", name_type = "stripped_canonical", taxonomic_dataset = "APC",
      aligned_reason = "Imprecise fuzzy match of taxon name to an APC-known canonical name once punctuation and filler words are removed",
      alignment_code = "match_07b_imprecise_fuzzy_synonym_canonical_name"
    )
    if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))
  }

  # match_08a: hybrid taxa
  # Exact match to APC-accepted, APC-known, or APNI-listed genus for names where " x " indicates taxon is a hybrid.
  # This alignment can only be made after exact matches of complete taxon names to APC/APNI + fuzzy matches to APC are complete,
  # because there are hybrid taxa listed in both APC & APNI.

  i <-
    is_hybrid(taxa$tocheck$cleaned_name) &
    taxa$tocheck$genus %in% resources$genera_all2$genus

  ii <- match(taxa$tocheck$genus[i], resources$genera_all2$genus)
  matched_dataset <- resources$genera_all2$taxonomic_dataset[ii]

  taxa <- apply_match(
    taxa, i,
    taxonomic_dataset = matched_dataset,
    taxon_rank = "genus",
    aligned_name = higher_rank_name(taxa, i, resources$genera_all2$genus[ii], marker = " x"),
    aligned_reason = paste0("Exact match to ", matched_dataset, " genus. ", hybrid_note),
    alignment_code = "match_08a_hybrid_taxon_exact"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_08b: hybrid taxa
  # Fuzzy match to APC-accepted genus.

  i <-
    is_hybrid(taxa$tocheck$cleaned_name) &
    taxa$tocheck$fuzzy_match_genus %in% resources$genera_accepted$genus

  taxa <- apply_match(
    taxa, i,
    taxonomic_dataset = "APC",
    taxon_rank = "genus",
    aligned_name = higher_rank_name(taxa, i, taxa$tocheck$fuzzy_match_genus[i], marker = " x"),
    aligned_reason = paste0("Fuzzy match to APC-accepted genus. ", hybrid_note),
    alignment_code = "match_08b_hybrid_taxon_accepted_fuzzy"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_08c: hybrid taxa
  # Fuzzy match to APC-known genus.

  i <-
    is_hybrid(taxa$tocheck$cleaned_name) &
    taxa$tocheck$fuzzy_match_genus_synonym %in% resources$genera_synonym$genus

  taxa <- apply_match(
    taxa, i,
    taxonomic_dataset = "APC",
    taxon_rank = "genus",
    aligned_name = higher_rank_name(taxa, i, taxa$tocheck$fuzzy_match_genus_synonym[i], marker = " x"),
    aligned_reason = paste0("Fuzzy match to APC-known genus. ", hybrid_note),
    alignment_code = "match_08c_hybrid_taxon_synonym_fuzzy"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_08d: hybrid taxa
  # Fuzzy match to APNI-listed genus.
  if (APNI_matches == TRUE) {
    i <-
      is_hybrid(taxa$tocheck$cleaned_name) &
      taxa$tocheck$fuzzy_match_genus_APNI %in% resources$genera_APNI$genus

    taxa <- apply_match(
      taxa, i,
      taxonomic_dataset = "APNI",
      taxon_rank = "genus",
      aligned_name = higher_rank_name(taxa, i, taxa$tocheck$fuzzy_match_genus_APNI[i], marker = " x"),
      aligned_reason = paste0("Fuzzy match to APNI-listed genus. ", hybrid_note),
      alignment_code = "match_08d_hybrid_taxon_APNI_fuzzy"
    )
    if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))
  }

  # match_08e: hybrid taxa
  # An exact or fuzzy genus-level match to APC & APNI genera cannot be made.

  i <-
    is_hybrid(taxa$tocheck$cleaned_name) &
    !taxa$tocheck$fuzzy_match_genus %in% resources$genera_all2$genus

  taxa <- apply_match(
    taxa, i,
    taxonomic_dataset = NA_character_,
    taxon_rank = NA_character_,
    aligned_name = NA_character_,
    aligned_reason = "Taxon name includes ' x ' indicating a hybrid,  but exact and fuzzy matches fail to align to a genus in the APC or APNI",
    alignment_code = "match_08e_hybrid_taxon_unknown"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_09a: exact trinomial matches, APC
  # Exact match of first three words of taxon name ("trinomial") to APC-accepted canonical name.
  # The purpose of matching only the first three words only to APC-accepted names is that
  # sometimes the submitted taxon name is a valid trinomial + notes and
  # such names will only be aligned by matches considering only the first three words of the stripped name.
  # This match also does a good job aligning and correcting syntax of phrase names.

  taxa <- match_reference_name(
    taxa, resources$APC_accepted,
    key = "trinomial", name_type = "trinomial", taxonomic_dataset = "APC",
    aligned_reason = "Exact match of the first three words of the taxon name to an APC-accepted canonical name",
    alignment_code = "match_09a_trinomial_exact_accepted"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_09b: exact trinomial matches, APC
  # Exact match of first three words of taxon name ("trinomial") to APC-known canonical name.

  taxa <- match_reference_name(
    taxa, resources$APC_synonyms,
    key = "trinomial", name_type = "trinomial", taxonomic_dataset = "APC",
    aligned_reason = "Exact match of the first three words of the taxon name to an APC-known canonical name",
    alignment_code = "match_09b_trinomial_exact_synonym"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_09c: fuzzy trinomial matches, APC
  # Fuzzy match of first three words of taxon name ("trinomial") to APC-accepted canonical name.

  taxa$tocheck$fuzzy_match_trinomial <- fuzzy_match_column(
    taxa$tocheck$trinomial, resources$APC_accepted$trinomial,
    fuzzy_abs_dist, fuzzy_rel_dist
  )

  taxa <- match_reference_name(
    taxa, resources$APC_accepted,
    key = "fuzzy_match_trinomial", name_type = "trinomial", taxonomic_dataset = "APC",
    aligned_reason = "Fuzzy match of the first three words of the taxon name to an APC-accepted canonical name",
    alignment_code = "match_09c_trinomial_fuzzy_accepted"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_09d: fuzzy trinomial matches, APC
  # Fuzzy match of first three words of taxon name ("trinomial") to APC-known canonical name.

  taxa$tocheck$fuzzy_match_trinomial_synonym <- fuzzy_match_column(
    taxa$tocheck$trinomial, resources$APC_synonyms$trinomial,
    fuzzy_abs_dist, fuzzy_rel_dist
  )

  taxa <- match_reference_name(
    taxa, resources$APC_synonyms,
    key = "fuzzy_match_trinomial_synonym", name_type = "trinomial", taxonomic_dataset = "APC",
    aligned_reason = "Fuzzy match of the first three words of the taxon name to an APC-known canonical name",
    alignment_code = "match_09d_trinomial_fuzzy_synonym"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_10a: exact binomial matches, APC
  # Exact match of first two words of taxon name ("binomial") to APC-accepted canonical name.
  # The purpose of matching only the first two words only to APC-accepted names is that
  # sometimes the submitted taxon name is a valid binomial + notes
  # or a valid binomial + invalid infraspecific epithet.
  # Such names will only be aligned by matches considering only the first two words of the stripped name.
  # This match also does a good job aligning and correcting syntax of phrase names.

  taxa <- match_reference_name(
    taxa, resources$APC_accepted,
    key = "binomial", name_type = "binomial", taxonomic_dataset = "APC",
    aligned_reason = "Exact match of the first two words of the taxon name to an APC-accepted canonical name",
    alignment_code = "match_10a_binomial_exact_accepted"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_10b: exact binomial matches, APC
  # Exact match of first two words of taxon name ("binomial") to APC-known canonical name.

  taxa <- match_reference_name(
    taxa, resources$APC_synonyms,
    key = "binomial", name_type = "binomial", taxonomic_dataset = "APC",
    aligned_reason = "Exact match of the first two words of the taxon name to an APC-known canonical name",
    alignment_code = "match_10b_binomial_exact_synonym"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_10c: fuzzy binomial matches, APC
  # Fuzzy match of first two words of taxon name ("binomial") to APC-accepted canonical name.

  taxa$tocheck$fuzzy_match_binomial <- fuzzy_match_column(
    taxa$tocheck$binomial, resources$APC_accepted$binomial,
    fuzzy_abs_dist, fuzzy_rel_dist, epithet_letters = 2
  )

  taxa <- match_reference_name(
    taxa, resources$APC_accepted,
    key = "fuzzy_match_binomial", name_type = "binomial", taxonomic_dataset = "APC",
    aligned_reason = "Fuzzy match of the first two words of the taxon name to an APC-accepted canonical name",
    alignment_code = "match_10c_binomial_fuzzy_accepted"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_10d: fuzzy binomial matches, APC
  # Fuzzy match of first two words of taxon name ("binomial") to APC-known canonical name.

  taxa$tocheck$fuzzy_match_binomial_APC_synonym <- fuzzy_match_column(
    taxa$tocheck$binomial, resources$APC_synonyms$binomial,
    fuzzy_abs_dist, fuzzy_rel_dist, epithet_letters = 2
  )

  taxa <- match_reference_name(
    taxa, resources$APC_synonyms,
    key = "fuzzy_match_binomial_APC_synonym", name_type = "binomial", taxonomic_dataset = "APC",
    aligned_reason = "Fuzzy match of the first two words of the taxon name to an APC-known canonical name",
    alignment_code = "match_10d_binomial_fuzzy_synonym"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_11a: fuzzy match to APNI-listed canonical name
  # Fuzzy match of taxon name to an APNI-listed canonical name, once filler words and punctuation are removed.
  # Fuzzy matches to APNI names occur toward the end of the alignment function,
  # because names exclusively in the APNI are often misspellings of APC accepted/known taxa and
  # many different string searches need to be completed on the APC-accepted and APC-known taxa (e.g. trinomial, binomial, less precise matches) first
  # to avoid incorrectly aligning an APC accepted/known taxa to an APNI name.
  # This is especially true to accurately align phrase names.
  if (APNI_matches == TRUE) {
    taxa$tocheck$fuzzy_match_cleaned_APNI <- fuzzy_match_column(
      taxa$tocheck$stripped_name, resources$APNI_names$stripped_canonical,
      fuzzy_abs_dist, fuzzy_rel_dist, epithet_letters = 2
    )

    taxa <- match_reference_name(
      taxa, resources$APNI_names,
      key = "fuzzy_match_cleaned_APNI", name_type = "stripped_canonical", taxonomic_dataset = "APNI",
      aligned_reason = "Fuzzy match of taxon name to an APNI-listed canonical name once punctuation and filler words are removed",
      alignment_code = "match_11a_fuzzy_APNI_canonical"
    )
    if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))
  }

  # match_11b: imprecise fuzzy APNI match
  # Imprecise fuzzy match of taxon name to an APNI-listed canonical name, once filler words and punctuation are removed.
  # For imprecise fuzzy matches, the taxon name can differ from the `APNI-listed` names by 5 characters & up to 25% of the string length.
  # These matches require individual review and are turned off as a default.
  if (APNI_matches == TRUE & imprecise_fuzzy_matches == TRUE) {
    taxa$tocheck$fuzzy_match_cleaned_APNI_imprecise <- fuzzy_match_column(
      taxa$tocheck$cleaned_name, resources$APNI_names$canonical_name,
      imprecise_fuzzy_abs_dist, imprecise_fuzzy_rel_dist, epithet_letters = 2
    )

    taxa <- match_reference_name(
      taxa, resources$APNI_names,
      key = "fuzzy_match_cleaned_APNI_imprecise", name_type = "canonical_name", taxonomic_dataset = "APNI",
      aligned_reason = "Imprecise fuzzy match of taxon name to an APNI-listed canonical name once punctuation and filler words are removed",
      alignment_code = "match_11b_imprecise_fuzzy_APNI_canonical_name"
    )
    if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))
  }

  # match_11c: exact trinomial matches, APNI
  # Exact match of first three words of taxon name ("trinomial") to APNI-listed canonical name.
  if (APNI_matches == TRUE) {
    taxa <- match_reference_name(
      taxa, resources$APNI_names,
      key = "trinomial", name_type = "trinomial", taxonomic_dataset = "APNI",
      aligned_reason = "Exact match of the first three words of the taxon name to an APNI-listed canonical name",
      alignment_code = "match_11c_trinomial_exact_APNI"
    )
    if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))
  }

  # match_11d: exact binomial matches, APNI
  # Exact match of first two words of taxon name ("binomial") to APNI-listed canonical name.
  if (APNI_matches == TRUE) {
    taxa <- match_reference_name(
      taxa, resources$APNI_names,
      key = "binomial", name_type = "binomial", taxonomic_dataset = "APNI",
      aligned_reason = "Exact match of the first two words of the taxon name to an APNI-listed canonical name",
      alignment_code = "match_11d_binomial_exact_APNI"
    )
    if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))
  }

  # match_12a: genus-level alignment
  # Toward the end of the alignment function, see if first word of unmatched taxa is an APC-accepted genus.
  # The 'taxon name' is then reformatted  as `genus sp.` with the original name in square brackets.

  i <- taxa$tocheck$genus %in% resources$genera_accepted$genus
  ii <- match(taxa$tocheck$genus[i], resources$genera_accepted$genus)

  taxa <- apply_match(
    taxa, i,
    taxonomic_dataset = "APC",
    taxon_rank = "genus",
    aligned_name = higher_rank_name(taxa, i, resources$genera_accepted$genus[ii]),
    aligned_reason = "Exact match of the first word of the taxon name to an APC-accepted genus",
    alignment_code = "match_12a_genus_exact_accepted"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_12b: genus-level alignment
  # Toward the end of the alignment function, see if first word of unmatched taxa is an APC-known genus.

  i <- taxa$tocheck$genus %in% resources$genera_synonym$genus
  ii <- match(taxa$tocheck$genus[i], resources$genera_synonym$genus)

  taxa <- apply_match(
    taxa, i,
    taxonomic_dataset = "APC",
    taxon_rank = "genus",
    aligned_name = higher_rank_name(taxa, i, resources$genera_synonym$genus[ii]),
    aligned_reason = "Exact match of the first word of the taxon name to an APC-known genus",
    alignment_code = "match_12b_genus_exact_synonym"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_12c: genus-level alignment
  # Toward the end of the alignment function, see if first word of unmatched taxa is an APNI-listed genus.
  if (APNI_matches == TRUE) {
    i <- taxa$tocheck$genus %in% resources$genera_APNI$genus
    ii <- match(taxa$tocheck$genus[i], resources$genera_APNI$genus)

    taxa <- apply_match(
      taxa, i,
      taxonomic_dataset = "APNI",
      taxon_rank = "genus",
      aligned_name = higher_rank_name(taxa, i, resources$genera_APNI$genus[ii]),
      aligned_reason = "Exact match of the first word of the taxon name to an APNI-listed genus",
      alignment_code = "match_12c_genus_exact_APNI"
    )
    if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))
  }

  # match_12d: family-level alignment
  # Toward the end of the alignment function, see if first word of unmatched taxa is an APC-accepted family.
  # The 'taxon name' is then reformatted  as `family sp.` with the original name in square brackets.

  i <-
    stringr::str_detect(word(taxa$tocheck$cleaned_name, 1), "aceae$") &
    taxa$tocheck$genus %in% resources$family_accepted$canonical_name

  taxa <- apply_match(
    taxa, i,
    taxonomic_dataset = "APC",
    taxon_rank = "family",
    aligned_name = higher_rank_name(taxa, i, taxa$tocheck$genus[i]),
    aligned_reason = "Exact match of the first word of the taxon name to an APC-accepted family",
    alignment_code = "match_12d_family_exact_accepted"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_12e: family-level synonym alignment
  # Toward the end of the alignment function, see if first word of unmatched taxa is an APC-known family.

  i <-
    stringr::str_detect(word(taxa$tocheck$cleaned_name, 1), "ae$") &
    taxa$tocheck$genus %in% resources$family_synonym$canonical_name

  taxa <- apply_match(
    taxa, i,
    taxonomic_dataset = "APC",
    taxon_rank = "family",
    aligned_name = higher_rank_name(taxa, i, taxa$tocheck$genus[i]),
    aligned_reason = "Exact match of the first word of the taxon name to an APC-synonymous family",
    alignment_code = "match_12e_family_exact_synonym"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_12f: genus-level fuzzy alignment
  # See if a fuzzy match can be made for the first word of unmatched taxa to an APC-accepted genus.

  i <- taxa$tocheck$fuzzy_match_genus %in% resources$genera_accepted$genus

  taxa <- apply_match(
    taxa, i,
    taxonomic_dataset = "APC",
    taxon_rank = "genus",
    aligned_name = higher_rank_name(taxa, i, taxa$tocheck$fuzzy_match_genus[i]),
    aligned_reason = "Fuzzy match of the first word of the taxon name to an APC-accepted genus",
    alignment_code = "match_12f_genus_fuzzy_accepted"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_12g: genus-level fuzzy alignment of synonyms
  # See if a fuzzy match can be made for the first word of unmatched taxa to an APC-known genus.

  i <- taxa$tocheck$fuzzy_match_genus_synonym %in% resources$genera_synonym$genus

  taxa <- apply_match(
    taxa, i,
    taxonomic_dataset = "APC",
    taxon_rank = "genus",
    aligned_name = higher_rank_name(taxa, i, taxa$tocheck$fuzzy_match_genus_synonym[i]),
    aligned_reason = "Fuzzy match of the first word of the taxon name to an APC-known genus",
    alignment_code = "match_12g_genus_fuzzy_synonym"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_12h: family-level fuzzy alignment
  # See if a fuzzy match can be made for the first word of unmatched taxa to an APC-accepted family.

  # Add some extra columns - checking for fuzzy matches in family
  # Not including this above, as fuzzy matching is slow
  taxa$tocheck <- taxa$tocheck %>%
    dplyr::mutate(
      fuzzy_match_family =
        fuzzy_match_genera(genus, resources$family_accepted$canonical_name),
      fuzzy_match_family_synonym =
        fuzzy_match_genera(genus, resources$family_synonym$canonical_name)
    )

  i <- taxa$tocheck$fuzzy_match_family %in% resources$family_accepted$canonical_name

  taxa <- apply_match(
    taxa, i,
    taxonomic_dataset = "APC",
    taxon_rank = "family",
    aligned_name = higher_rank_name(taxa, i, taxa$tocheck$fuzzy_match_family[i]),
    aligned_reason = "Fuzzy match of the first word of the taxon name to an APC-accepted family",
    alignment_code = "match_12h_family_fuzzy_accepted"
  )
  if (nrow(taxa$tocheck) == 0) return(drop_scratch(taxa))

  # match_12i: family-level fuzzy alignment for synonyms
  # The final alignment step is to see if a fuzzy match can be made for the first word of unmatched taxa to an APC-synonymous family.

  i <- taxa$tocheck$fuzzy_match_family_synonym %in% resources$family_synonym$canonical_name

  taxa <- apply_match(
    taxa, i,
    taxonomic_dataset = "APC",
    taxon_rank = "family",
    aligned_name = higher_rank_name(taxa, i, taxa$tocheck$fuzzy_match_family_synonym[i]),
    aligned_reason = "Fuzzy match of the first word of the taxon name to an APC-synonymous family",
    alignment_code = "match_12i_family_fuzzy_synonym"
  )

  return(drop_scratch(taxa))
}


# Record an alignment against the rows of `taxa$tocheck` selected by the logical
# index `i`, then move those rows into `taxa$checked`.
#
# The arguments are named for the columns they populate. `taxonomic_dataset`,
# `taxon_rank`, `aligned_name`, `aligned_reason` and `alignment_code` are each
# either a single value or a vector with one element per selected row.
#
# `aligned_reason` is the explanation *without* the trailing run date. Appending
# the date here means the ` (date)` separator is written once, rather than being
# repeated in every match step where it can be (and has been) mistyped.
apply_match <- function(taxa, i, taxonomic_dataset, taxon_rank, aligned_name,
                        aligned_reason, alignment_code) {
  i[is.na(i)] <- FALSE
  if (!any(i)) return(taxa)

  taxa$tocheck$taxonomic_dataset[i] <- taxonomic_dataset
  taxa$tocheck$taxon_rank[i]        <- taxon_rank
  taxa$tocheck$aligned_name[i]      <- aligned_name
  taxa$tocheck$aligned_reason[i]    <- paste0(aligned_reason, " (", Sys.Date(), ")")
  taxa$tocheck$alignment_code[i]    <- alignment_code
  taxa$tocheck$known[i]             <- TRUE
  taxa$tocheck$checked[i]           <- TRUE

  redistribute(taxa)
}


# The commonest match step: rows whose `key` column of `taxa$tocheck` (e.g.
# `original_name`, `cleaned_name`) exactly match the `name_type` column of a
# reference table (e.g. `scientific_name`, `canonical_name` of
# `resources$APC_accepted` and friends) take that reference row's canonical name
# and taxon rank.
match_reference_name <- function(taxa, table, key, name_type, taxonomic_dataset,
                                 aligned_reason, alignment_code) {
  lookup <- table[[name_type]]
  i <- taxa$tocheck[[key]] %in% lookup
  ii <- match(taxa$tocheck[[key]][i], lookup)

  apply_match(
    taxa, i,
    taxonomic_dataset = taxonomic_dataset,
    taxon_rank = table$taxon_rank[ii],
    aligned_name = table$canonical_name[ii],
    aligned_reason = aligned_reason,
    alignment_code = alignment_code
  )
}


# `Acacia sp.`, or `Acacia sp. [Royal NP]` when an identifier was supplied.
# Used where the input name already ended in `sp.`, so there is no detail below
# the aligned rank left to carry through.
genus_sp_name <- function(taxa, i, stem) {
  identifier <- taxa$tocheck$identifier_string[i]
  paste0(stem, " sp.", ifelse(is.na(identifier), "", identifier))
}


# `Acacia sp. [acacia aff. dealbata; Royal NP]`, or with `marker = " x"`,
# `Acacia x [acacia dealbata x mearnsii; Royal NP]`. Used where the input named
# something below the rank it could be aligned to, which is kept in brackets.
higher_rank_name <- function(taxa, i, stem, marker = " sp.") {
  cleaned_name <- taxa$tocheck$cleaned_name[i]
  identifier <- taxa$tocheck$identifier_string2[i]
  paste0(stem, marker, " [", cleaned_name,
         ifelse(is.na(identifier), "", identifier), "]")
}


# Fuzzy match a whole column in one pass. `fuzzy_match()` cannot handle a
# missing input (`trinomial` and `binomial` are NA for short names), so those
# rows are left unmatched.
fuzzy_match_column <- function(x, accepted_list, max_distance_abs,
                               max_distance_rel, epithet_letters = 1) {
  purrr::map_chr(x, function(txt) {
    if (is.na(txt)) return(NA_character_)
    fuzzy_match(
      txt = txt,
      accepted_list = accepted_list,
      max_distance_abs = max_distance_abs,
      max_distance_rel = max_distance_rel,
      n_allowed = 1,
      epithet_letters = epithet_letters
    )
  })
}


# Remove the columns that only exist while matching, so callers always see the
# documented column set regardless of which match step finished the job.
drop_scratch <- function(taxa) {
  scratch <- c("identifier_string", "identifier_string2")
  taxa$tocheck <- taxa$tocheck %>% dplyr::select(-dplyr::any_of(scratch))
  taxa$checked <- taxa$checked %>% dplyr::select(-dplyr::any_of(scratch))
  taxa
}
