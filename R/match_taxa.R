# do the actual matching
#' @noRd
match_taxa <- function(taxa, resources, dataset_id = "XXXX") {

  ## replace NA's with a new string
  update_na_with <- function(current, new) {
    ifelse(is.na(current), new, current)
  }
  
  fuzzy_match_genera <- function(x, y) {
    purrr::map_chr(x, ~ fuzzy_match(.x, y, 2, 0.35, n_allowed = 1))
  }
  
  taxa$tocheck <- taxa$tocheck %>%
    dplyr::mutate(
      cleaned_name = cleaned_name %>%
        update_na_with(standardise_names(original_name)),
      stripped_name = stripped_name %>%
        update_na_with(strip_names(original_name)),
      stripped_name2 = stripped_name2 %>%
        update_na_with(strip_names_2(original_name)),
      trinomial = stringr::word(stripped_name2, start = 1, end = 3),
      binomial = stringr::word(stripped_name2, start = 1, end = 2),
      genus = stringr::word(original_name, start = 1, end = 1),
      fuzzy_match_genus =
        fuzzy_match_genera(genus, resources$genera_accepted$canonicalName),
      fuzzy_match_genus_known =
        fuzzy_match_genera(genus, resources$genera_known$canonicalName),
      fuzzy_match_genus_APNI =
        fuzzy_match_genera(genus, resources$genera_APNI$canonicalName)
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_01, `genus sp.` matches, across all lists simultaneously
  # for names where the final "word" is `sp` or `spp`, immediately indicate it can only be aligned to `taxonomic_resolution = "genus"` (or family)
  # first match against known genera in any resource
  
  i <-
    (
      stringr::str_detect(taxa$tocheck$stripped_name, "[:space:]sp$") |
        stringr::str_detect(taxa$tocheck$stripped_name, "[:space:]spp$")
    ) &
    taxa$tocheck$genus %in% resources$genera_all$canonicalName &
    stringr::word(taxa$tocheck$stripped_name, 2) %in% c("sp", "spp")
  
  ii <-
    match(taxa$tocheck[i,]$genus, resources$genera_all$canonicalName)
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_ref = resources$genera_all$taxonomic_ref[ii],
      taxonomic_resolution = "genus",
      aligned_name = paste0(resources$genera_all$cleaned_name[ii], " sp. [", dataset_id, "]"),
      aligned_reason = paste0(
        "match_01. Rewording taxon with ending with `sp.` to indicate a genus-level alignment with `",
        taxonomic_ref,
        "` name (",
        Sys.Date(),
        ")"
      ),
      checked = TRUE,
      known = TRUE,
      still_to_match = "match_01"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_01_fuzzy_accepted, `genus sp.` matches, across all lists simultaneously
  # for names where the final "word" is `sp` or `spp`, if it doesn't exactly match a genus name, try fuzzy matches with APC accepted genera
  i <-
    (
      stringr::str_detect(taxa$tocheck$stripped_name, "[:space:]sp$") |
        stringr::str_detect(taxa$tocheck$stripped_name, "[:space:]spp$")
    ) &
    taxa$tocheck$fuzzy_match_genus %in% resources$genera_all$canonicalName &
    stringr::word(taxa$tocheck$stripped_name, 2) %in% c("sp", "spp")
  
  ii <-
    match(taxa$tocheck[i,]$fuzzy_match_genus,
          resources$genera_all$canonicalName)
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_ref = resources$genera_all$taxonomic_ref[ii],
      taxonomic_resolution = "genus",
      aligned_name = paste0(resources$genera_all$cleaned_name[ii], " sp. [", dataset_id, "]"),
      aligned_reason = paste0(
        "match_01_fuzzy. Fuzzy match of name ending with `sp.` to an APC accepted genus (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_01_fuzzy_accepted"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_01_fuzzy_known, `genus sp.` matches, across all lists simultaneously
  # for names where the final "word" is `sp` or `spp`, if it doesn't exactly match a genus name, try fuzzy matches with APC known genera
  i <-
    (
      stringr::str_detect(taxa$tocheck$stripped_name, "[:space:]sp$") |
        stringr::str_detect(taxa$tocheck$stripped_name, "[:space:]spp$")
    ) &
    taxa$tocheck$fuzzy_match_genus_known %in% resources$genera_all$canonicalName &
    stringr::word(taxa$tocheck$stripped_name, 2) %in% c("sp", "spp")
  
  ii <-
    match(taxa$tocheck[i,]$fuzzy_match_genus_known,
          resources$genera_known$canonicalName)
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_ref = resources$genera_all$taxonomic_ref[ii],
      taxonomic_resolution = "genus",
      aligned_name = paste0(
        word(resources$genera_known$acceptedNameUsage[ii], 1),
        " sp. [",
        dataset_id,
        "]"
      ),
      aligned_reason = paste0(
        "match_01_fuzzy. Fuzzy match of name ending with `sp.` to an APC known genus (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_01_fuzzy_known"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_02, `family sp.` matches
  # for names where the final "word" is `sp` or `spp`, that do not match to a genus, see if they instead match to a family
  i <-
    (
      stringr::str_detect(taxa$tocheck$stripped_name, "[:space:]sp$") |
        stringr::str_detect(taxa$tocheck$stripped_name, "[:space:]spp$")
    ) &
    taxa$tocheck$genus %in% resources$family_accepted$canonicalName &
    stringr::word(taxa$tocheck$stripped_name, 2) %in% c("sp", "spp")
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = "family",
      aligned_name = paste0(genus, " sp. [", dataset_id, "]"),
      aligned_reason = paste0(
        "match_02. Rewording taxon with ending with `sp.` to indicate a family-level alignment with `APC accepted` name (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_02a_exact_family_accepted"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_03a: Intergrade taxon
  # Exact match to APC-accepted or APNI-listed genus for taxon names where a double hyphen indicates the plant is an intergrade.
  # For taxon names the fitting pattern, `genus species_A -- species_B` (intergrade) automatically align to genus,
  # since this is the highest taxon rank that can be attached to the plant name
  # first consider perfect matches within either APC or APNI
  i <-
    stringr::str_detect(taxa$tocheck$cleaned_name, "\\ -- |\\--") &
    taxa$tocheck$genus %in% resources$genera_all$canonicalName
  
  ii <-
    match(taxa$tocheck[i,]$genus, resources$genera_all$canonicalName)
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_ref = resources$genera_all$taxonomic_ref[ii],
      taxonomic_resolution = "genus",
      aligned_name = paste0(
        resources$genera_all$cleaned_name[ii],
        " sp. [",
        cleaned_name,
        "; ",
        dataset_id,
        "]"
      ),
      aligned_reason = paste0(
        "match_03. Rewording taxon that are intergrades between two taxa and genus aligns with `",
        taxonomic_ref,
        "` genus (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_03a_intergrade_accepted_or_known_genus"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_03_fuzzy_accepted, `genus species_A -- species_B` (intergrade) matches with fuzzy-matched genus (APC accepted)
  # next consider fuzzy matches to `APC accepted` genera
  i <-
    stringr::str_detect(taxa$tocheck$cleaned_name, "\\ -- ") &
    taxa$tocheck$fuzzy_match_genus %in% resources$genera_accepted$canonicalName
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = "genus",
      aligned_name = paste0(fuzzy_match_genus, " sp. [", cleaned_name, "; ", dataset_id, "]"),
      aligned_reason = paste0(
        "match_03_fuzzy. Rewording taxon that are intergrades between two taxa to indicate a genus-level alignment with APC accepted genus via fuzzy match (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_03_fuzzy_accepted"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_03_fuzzy_known, `genus species_A -- species_B` (intergrade) matches with fuzzy-matched genus (APC known)
  # next consider fuzzy matches to `APC known` genera
  i <-
    stringr::str_detect(taxa$tocheck$cleaned_name, "\\ -- ") &
    taxa$tocheck$fuzzy_match_genus_known %in% resources$genera_accepted$canonicalName
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = "genus",
      aligned_name = paste0(
        fuzzy_match_genus_known,
        " sp. [",
        cleaned_name,
        "; ",
        dataset_id,
        "]"
      ),
      aligned_reason = paste0(
        "match_03_fuzzy. Rewording taxon that are intergrades between two taxa to indicate a genus-level alignment with APC known genus via fuzzy match (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_03_fuzzy_known"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_03_fuzzy_APNI, `genus species_A -- species_B` (intergrade) matches with fuzzy-matched genus (APNI)
  # next consider fuzzy matches to `APNI` genera
  i <-
    stringr::str_detect(taxa$tocheck$cleaned_name, "\\ -- ") &
    taxa$tocheck$fuzzy_match_genus_APNI %in% resources$genera_accepted$canonicalName
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = "genus",
      aligned_name = paste0(
        fuzzy_match_genus_APNI,
        " sp. [",
        cleaned_name,
        "; ",
        dataset_id,
        "]"
      ),
      aligned_reason = paste0(
        "match_03_fuzzy. Rewording taxon that are intergrades between two taxa to indicate a genus-level alignment with genus in APNI via fuzzy match (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_03_fuzzy_APNI"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_03_x, `genus species_A -- species_B` (intergrade) matches with unmatched genus
  # for intergrades where neither perfect nor fuzzy matches "capture" the genus,
  # simply reformat the name and indicate the `taxon_rank = genus`
  i <-
    stringr::str_detect(taxa$tocheck$cleaned_name, "\\ -- ") &
    !taxa$tocheck$fuzzy_match_genus %in% resources$genera_accepted$canonicalName
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = "genus",
      aligned_name = paste0(genus, " sp. [", cleaned_name, "; ", dataset_id, "]"),
      aligned_reason = paste0(
        "match_03_x. Rewording taxon that are intergrades between two taxa to indicate a genus-level alignment, but genus doesn't align to `",
        taxonomic_ref,
        "` genus via fuzzy match (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_03_x"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_04, `genus species_A / species_B` ("indecision") matches with accepted genus
  # for names where a slash ("/") indicates the author is uncertain of the proper taxon name, automatically align to genus
  # since this is the highest taxon rank that can be attached to the plant name
  # first consider perfect matches within either APC or APNI
  i <-
    (
      stringr::str_detect(taxa$tocheck$cleaned_name, "[:alpha:]\\/") |
        stringr::str_detect(taxa$tocheck$cleaned_name, "\\s\\/")
    ) &
    taxa$tocheck$genus %in% resources$genera_all$canonicalName
  
  ii <-
    match(taxa$tocheck[i,]$genus, resources$genera_all$canonicalName)
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_ref = resources$genera_all$taxonomic_ref[ii],
      taxonomic_resolution = "genus",
      aligned_name = paste0(
        resources$genera_all$cleaned_name[ii],
        " sp. [",
        cleaned_name,
        "; ",
        dataset_id,
        "]"
      ),
      aligned_reason = paste0(
        "match_04. Rewording taxon where `/` indicates uncertain species identification to align with `",
        taxonomic_ref,
        "` genus (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_04"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_04_fuzzy_accepted, `genus species_A / species_B` ("indecision") matches with fuzzy-matched genus
  # next consider fuzzy matches to `APC accepted` genera
  i <-
    (
      stringr::str_detect(taxa$tocheck$cleaned_name, "[:alpha:]\\/") |
        stringr::str_detect(taxa$tocheck$cleaned_name, "\\s\\/")
    ) &
    taxa$tocheck$fuzzy_match_genus %in% resources$genera_accepted$canonicalName
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = "genus",
      aligned_name = paste0(fuzzy_match_genus, " sp. [", cleaned_name, "; ", dataset_id, "]"),
      aligned_reason = paste0(
        "match_04_fuzzy. Rewording taxon where `/` indicates uncertain species identification & genus aligned to APC accepted genus via fuzzy match (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_04_fuzzy_accepted"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_04_fuzzy_known, `genus species_A / species_B` ("indecision") matches with fuzzy-matched genus (APC known)
  # next consider fuzzy matches to `APC known` genera
  i <-
    (
      stringr::str_detect(taxa$tocheck$cleaned_name, "[:alpha:]\\/") |
        stringr::str_detect(taxa$tocheck$cleaned_name, "\\s\\/")
    ) &
    taxa$tocheck$fuzzy_match_genus_known %in% resources$genera_accepted$canonicalName
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = "genus",
      aligned_name = paste0(
        fuzzy_match_genus_known,
        " sp. [",
        cleaned_name,
        "; ",
        dataset_id,
        "]"
      ),
      aligned_reason = paste0(
        "match_04_fuzzy. Rewording taxon where `/` indicates uncertain species identification & genus aligned to APC known genus via fuzzy match (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_04_fuzzy_known"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_04_fuzzy_APNI, `genus species_A / species_B` ("indecision") matches with fuzzy-matched genus (APNI)
  # next consider fuzzy matches to `APNI` genera
  i <-
    (
      stringr::str_detect(taxa$tocheck$cleaned_name, "[:alpha:]\\/") |
        stringr::str_detect(taxa$tocheck$cleaned_name, "\\s\\/")
    ) &
    taxa$tocheck$fuzzy_match_genus_APNI %in% resources$genera_accepted$canonicalName
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = "genus",
      aligned_name = paste0(
        fuzzy_match_genus_APNI,
        " sp. [",
        cleaned_name,
        "; ",
        dataset_id,
        "]"
      ),
      aligned_reason = paste0(
        "match_04_fuzzy. Rewording taxon where `/` indicates uncertain species identification & genus aligned to a genus in APNI via fuzzy match (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_04_fuzzy_APNI"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_04_x, `genus species_A / species_B` ("indecision") matches with unmatched genus
  # for uncertain species where neither perfect nor fuzzy matches "capture" the genus,
  # simply reformat the name and indicate the `taxon_rank = genus`
  # **this is a match to check afterwards, because sometimes indicates two possible genus names, instead of indecision over species names
  i <-
    (
      stringr::str_detect(taxa$tocheck$cleaned_name, "[:alpha:]\\/") |
        stringr::str_detect(taxa$tocheck$cleaned_name, "\\s\\/")
    ) &
    !taxa$tocheck$fuzzy_match_genus %in% resources$genera_accepted$canonicalName
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = "genus",
      aligned_name = paste0(genus, " sp. [", cleaned_name, "; ", dataset_id, "]"),
      aligned_reason = paste0(
        "match_04_x. Rewording taxon where `/` indicates uncertain species identification, but genus doesn't align to APC accepted genus via fuzzy match (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_04_x"
    )
  
  # Note:  -- Finished with checking genus sp. above, now continue with full species
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_05_accepted, `scientific name` matches
  # see if the author has submitted a scientific name, with authorship
  i <-
    taxa$tocheck$original_name %in% resources$`APC list (accepted)`$scientificName
  
  ii <-
    match(taxa$tocheck[i,]$original_name,
          resources$`APC list (accepted)`$scientificName)
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = resources$`APC list (accepted)`$taxonRank[ii],
      aligned_name = resources$`APC list (accepted)`$canonicalName[ii],
      aligned_reason = paste0(
        "match_05. Automatic alignment with scientific name in APC accepted list (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_05_accepted"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_06_accepted, `APC accepted` synonyms
  # see if the name matches an APC accepted taxon name once filler words and punctuation are removed
  i <-
    taxa$tocheck$stripped_name %in% resources$`APC list (accepted)`$stripped_canonical
  
  ii <-
    match(
      taxa$tocheck[i,]$stripped_name,
      resources$`APC list (accepted)`$stripped_canonical
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    
    mutate(
      taxonomic_resolution = resources$`APC list (accepted)`$taxonRank[ii],
      aligned_name = resources$`APC list (accepted)`$canonicalName[ii],
      aligned_reason = paste0(
        "match_06. Automatic alignment with accepted canonical names in APC (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_06_accepted"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_06_known, `APC known names`, synonyms
  # see if the name matches an APC known taxon name once filler words and punctuation are removed
  i <-
    taxa$tocheck$stripped_name %in% resources$`APC list (known names)`$stripped_canonical
  
  ii <-
    match(
      taxa$tocheck[i,]$stripped_name,
      resources$`APC list (known names)`$stripped_canonical
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = resources$`APC list (known names)`$taxonRank[ii],
      aligned_name = resources$`APC list (known names)`$canonicalName[ii],
      aligned_reason = paste0(
        "match_06. Automatic alignment with synonymous term among known canonical names APC (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_06_known"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_07_fuzzy_accepted, `APC accepted` fuzzy match
  # see if a fuzzy match to an APC accepted taxon name is possible
  # the default is set to only allow changes of up to 3 characters & no more than 20% of the total string length
  # the first letter of each word (up to 3 words) must match
  for (i in 1:nrow(taxa$tocheck)) {
    taxa$tocheck$stripped_name[i] <-
      standardise_names(taxa$tocheck$stripped_name[i]) %>% tolower()
    taxa$tocheck$fuzzy_match_cleaned_APC[i] <-
      fuzzy_match(
        taxa$tocheck$stripped_name[i],
        resources$`APC list (accepted)`$stripped_canonical,
        3,
        0.2,
        n_allowed = 1
      )
  }
  
  i <-
    taxa$tocheck$fuzzy_match_cleaned_APC %in% resources$`APC list (accepted)`$stripped_canonical
  
  ii <-
    match(
      taxa$tocheck[i,]$fuzzy_match_cleaned_APC,
      resources$`APC list (accepted)`$stripped_canonical
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = resources$`APC list (accepted)`$taxonRank[ii],
      aligned_name = resources$`APC list (accepted)`$canonicalName[ii],
      aligned_reason = paste0(
        "match_07_fuzzy. Fuzzy alignment with accepted canonical name in APC (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_07_fuzzy_accepted"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_07_fuzzy_known, `APC known names`, fuzzy matches
  # see if a fuzzy match to an APC known taxon name is possible
  # the default is set to only allow changes of up to 3 characters & no more than 20% of the total string length
  # the first letter of each word (up to 3 words) must match
  for (i in 1:nrow(taxa$tocheck)) {
    taxa$tocheck$stripped_name[i] <-
      standardise_names(taxa$tocheck$stripped_name[i]) %>% tolower()
    
    taxa$tocheck$fuzzy_match_cleaned_APC_known[i] <-
      fuzzy_match(
        taxa$tocheck$stripped_name[i],
        resources$`APC list (known names)`$stripped_canonical,
        3,
        0.2,
        n_allowed = 1
      )
  }
  
  i <-
    taxa$tocheck$fuzzy_match_cleaned_APC_known %in% resources$`APC list (known names)`$stripped_canonical
  
  ii <-
    match(
      taxa$tocheck[i,]$fuzzy_match_cleaned_APC_known,
      resources$`APC list (known names)`$stripped_canonical
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = resources$`APC list (known names)`$taxonRank[ii],
      aligned_name = resources$`APC list (known names)`$canonicalName[ii],
      aligned_reason = paste0(
        "match_07_fuzzy. Fuzzy alignment with known canonical name in APC (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_07_fuzzy_known"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_08_APNI, `APNI names`, synonyms
  # see if the name matches a taxon name listed in the APNI once filler words and punctuation are removed
  i <-
    taxa$tocheck$stripped_name2 %in% resources$`APNI names`$stripped_canonical2
  
  ii <-
    match(taxa$tocheck[i,]$stripped_name2,
          resources$`APNI names`$stripped_canonical2)
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = resources$`APNI names`$taxonRank[ii],
      aligned_name = resources$`APNI names`$canonicalName[ii],
      aligned_reason = paste0(
        "match_08. Automatic alignment with synonymous name in APNI (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_08_APNI"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_09, `genus aff. species` matches with accepted genus
  # for names where "aff" indicates the taxon has an affinity to another taxon, but isn't the other taxon, automatically align to genus
  # since this is the highest taxon rank that can be attached to the plant name
  # first consider perfect matches within either APC or APNI
  # **note this reformatting is only done after perfect matches to APC/APNI are considered + initial fuzzy matches to APC,
  # **because there are phrase names that include "sp. aff." and these will now have been picked up
  i <-
    (
      stringr::str_detect(taxa$tocheck$cleaned_name, "[Aa]ff[\\.\\s]") |
        stringr::str_detect(taxa$tocheck$cleaned_name, " affinis ")
    ) &
    taxa$tocheck$genus %in% resources$genera_all$canonicalName
  
  ii <-
    match(taxa$tocheck[i,]$genus, resources$genera_all$canonicalName)
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_ref = resources$genera_all$taxonomic_ref[ii],
      taxonomic_resolution = "genus",
      aligned_name = paste0(
        resources$genera_all$cleaned_name[ii],
        " sp. [",
        cleaned_name,
        "; ",
        dataset_id,
        "]"
      ),
      aligned_reason = paste0(
        "match_09. Rewording taxon with term `affinis` preceding species epithet to indicate a genus-level alignment with `",
        taxonomic_ref,
        "` genus (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_09"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_09_fuzzy_accepted, `genus aff. species` matches with fuzzy-matched genus
  # next consider fuzzy matches to `APC accepted` genera
  i <-
    (
      stringr::str_detect(taxa$tocheck$cleaned_name, "[Aa]ff[\\.\\s]") |
        stringr::str_detect(taxa$tocheck$cleaned_name, " affinis ")
    ) &
    taxa$tocheck$fuzzy_match_genus %in% resources$genera_accepted$canonicalName
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = "genus",
      aligned_name = paste0(fuzzy_match_genus, " sp. [", cleaned_name, "; ", dataset_id, "]"),
      aligned_reason = paste0(
        "match_09_fuzzy. Rewording taxon with term `affinis` preceding species epithet to indicate a genus-level alignment & genus aligned with APC accepted genus via fuzzy match (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_09_fuzzy_accepted"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_09_fuzzy_known, `genus aff. species` matches with fuzzy-matched genus (APC known)
  # next consider fuzzy matches to `APC known` genera
  i <-
    (
      stringr::str_detect(taxa$tocheck$cleaned_name, "[Aa]ff[\\.\\s]") |
        stringr::str_detect(taxa$tocheck$cleaned_name, " affinis ")
    ) &
    taxa$tocheck$fuzzy_match_genus_known %in% resources$genera_known$canonicalName
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = "genus",
      aligned_name = paste0(
        fuzzy_match_genus_known,
        " sp. [",
        cleaned_name,
        "; ",
        dataset_id,
        "]"
      ),
      aligned_reason = paste0(
        "match_09_fuzzy. Rewording taxon with term `affinis` preceding species epithet to indicate a genus-level alignment & genus aligned with APC known genus via fuzzy match (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_09_fuzzy_known"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_09_fuzzy_APNI, `genus aff. species` matches with fuzzy-matched genus (APNI)
  # next consider fuzzy matches to `APNI` genera
  i <-
    (
      stringr::str_detect(taxa$tocheck$cleaned_name, "[Aa]ff[\\.\\s]") |
        stringr::str_detect(taxa$tocheck$cleaned_name, " affinis ")
    ) &
    taxa$tocheck$fuzzy_match_genus_APNI %in% resources$genera_known$canonicalName
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = "genus",
      aligned_name = paste0(
        fuzzy_match_genus_APNI,
        " sp. [",
        cleaned_name,
        "; ",
        dataset_id,
        "]"
      ),
      aligned_reason = paste0(
        "match_09_fuzzy. Rewording taxon with term `affinis` preceding species epithet to indicate a genus-level alignment & genus aligned with genus listed on the APNI via fuzzy match (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_09_fuzzy_APNI"
    )
  
  # match match_09_x, `genus aff. species` matches with unmatched genus
  # for `sp. aff.` species where neither perfect nor fuzzy matches "capture" the genus,
  # simply reformat the name and indicate the `taxon_rank = genus`
  i <-
    (
      stringr::str_detect(taxa$tocheck$cleaned_name, "[Aa]ff[\\.\\s]") |
        stringr::str_detect(taxa$tocheck$cleaned_name, " affinis ")
    ) &
    !taxa$tocheck$fuzzy_match_genus %in% resources$genera_accepted$canonicalName
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = "genus",
      aligned_name = paste0(genus, " sp. [", cleaned_name, "; ", dataset_id, "]"),
      aligned_reason = paste0(
        "match_09_x. Rewording taxon with term `affinis` preceding species epithet to indicate a genus-level alignment, but genus doesn't align to APC accepted genus via fuzzy match (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_09_x"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_10_fuzzy_accepted, `APC accepted` fuzzy match
  # now begin a second round of fuzzy matches, with less restrictive matching rules
  # the input taxon name is now allowed to differ by `APC accepted` names by 5 characters & up to 25% of the string length
  # it is important to separate the more constrained and imprecise fuzzy matches, because it is the imprecise matches that require careful review
  for (i in 1:nrow(taxa$tocheck)) {
    taxa$tocheck$stripped_name[i] <-
      standardise_names(taxa$tocheck$stripped_name[i]) %>% tolower()
    
    taxa$tocheck$fuzzy_match_cleaned_APC_imprecise[i] <-
      fuzzy_match(
        taxa$tocheck$stripped_name[i],
        resources$`APC list (accepted)`$stripped_canonical,
        5,
        0.25,
        n_allowed = 1
      )
  }
  
  i <-
    taxa$tocheck$fuzzy_match_cleaned_APC_imprecise %in% resources$`APC list (accepted)`$stripped_canonical
  
  ii <-
    match(
      taxa$tocheck[i,]$fuzzy_match_cleaned_APC_imprecise,
      resources$`APC list (accepted)`$stripped_canonical
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = resources$`APC list (accepted)`$taxonRank[ii],
      aligned_name = resources$`APC list (accepted)`$canonicalName[ii],
      aligned_reason = paste0(
        "match_10_fuzzy. Imprecise fuzzy alignment with accepted canonical name in APC (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_10_fuzzy_accepted"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_10_fuzzy_known, `APC known names`, imprecise fuzzy matches
  # the input taxon name is now allowed to differ by `APC known` names by 5 characters & up to 25% of the string length
  # it is important to separate the more constrained and imprecise fuzzy matches, because it is the imprecise matches that require careful review
  for (i in 1:nrow(taxa$tocheck)) {
    taxa$tocheck$stripped_name[i] <-
      standardise_names(taxa$tocheck$stripped_name[i]) %>% tolower()
    
    taxa$tocheck$fuzzy_match_cleaned_APC_known_imprecise[i] <-
      fuzzy_match(
        taxa$tocheck$stripped_name[i],
        resources$`APC list (known names)`$stripped_canonical,
        5,
        0.25,
        n_allowed = 1
      )
  }
  
  i <-
    taxa$tocheck$fuzzy_match_cleaned_APC_known_imprecise %in% resources$`APC list (known names)`$stripped_canonical
  
  ii <-
    match(
      taxa$tocheck[i,]$fuzzy_match_cleaned_APC_known_imprecise,
      resources$`APC list (known names)`$stripped_canonical
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = resources$`APC list (known names)`$taxonRank[ii],
      aligned_name = resources$`APC list (known names)`$canonicalName[ii],
      aligned_reason = paste0(
        "match_10_fuzzy. Imprecise fuzzy alignment with known canonical name in APC (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_10_fuzzy_known"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_11, detect `genus species X genus species`, indicating hybrid & match accepted genus
  # for names where a stand-alone x (" x ") indicates taxon is a hybrid, automatically align to genus
  # since this is the highest taxon rank that can be attached to the plant name
  # first consider perfect matches within either APC or APNI
  # **note this reformatting is only done after perfect matches to APC/APNI are considered + initial fuzzy matches to APC,
  # **because there are recognised hybrids on both the APC/APNI and these will now have been picked up
  i <-
    stringr::str_detect(taxa$tocheck$cleaned_name, " [xX] ") &
    taxa$tocheck$genus %in% resources$genera_accepted$canonicalName
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = "genus",
      aligned_name = paste0(genus, " x [", cleaned_name, "; ", dataset_id, "]"),
      aligned_reason = paste0(
        "match_11. Rewording hybrid species name not in APC or APNI to indicate a genus-level alignment with APC accepted genus (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_11"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_11_fuzzy_accepted, detect `genus species X genus species`, indicating hybrid & align with fuzzy-matched genus
  # next consider fuzzy matches to `APC accepted` genera
  i <-
    stringr::str_detect(taxa$tocheck$cleaned_name, " [xX] ") &
    taxa$tocheck$fuzzy_match_genus %in% resources$genera_accepted$canonicalName
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = "genus",
      aligned_name = paste0(fuzzy_match_genus, " x [", cleaned_name, "; ", dataset_id, "]"),
      aligned_reason = paste0(
        "match_11_fuzzy. Rewording hybrid species name not in APC or APNI to indicate a genus-level alignment & genus aligned with APC accepted genus via fuzzy match (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_11_fuzzy_accepted"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_11_fuzzy_known, detect `genus species X genus species`, indicating hybrid & align with fuzzy-matched genus
  # next consider fuzzy matches to `APC known` genera
  i <-
    stringr::str_detect(taxa$tocheck$cleaned_name, " [xX] ") &
    taxa$tocheck$fuzzy_match_genus_known %in% resources$genera_accepted$canonicalName
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = "genus",
      aligned_name = paste0(
        fuzzy_match_genus_known,
        " x [",
        cleaned_name,
        "; ",
        dataset_id,
        "]"
      ),
      aligned_reason = paste0(
        "match_11_fuzzy. Rewording hybrid species name not in APC or APNI to indicate a genus-level alignment & genus aligned with APC known genus via fuzzy match (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_11_fuzzy_known"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_11_fuzzy_APNI, detect `genus species X genus species`, indicating hybrid & align with fuzzy-matched genus
  # next consider fuzzy matches to `APNI` genera
  i <-
    stringr::str_detect(taxa$tocheck$cleaned_name, " [xX] ") &
    taxa$tocheck$fuzzy_match_genus_APNI %in% resources$genera_accepted$canonicalName
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = "genus",
      aligned_name = paste0(
        fuzzy_match_genus_APNI,
        " x [",
        cleaned_name,
        "; ",
        dataset_id,
        "]"
      ),
      aligned_reason = paste0(
        "match_11_fuzzy. Rewording hybrid species name not in APC or APNI to indicate a genus-level alignment & genus aligned with a genus list on the APNI via fuzzy match (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_11_fuzzy_APNI"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_11_x, detect `genus species X genus species`, indicating hybrid & does not align with genus
  # for hybrid species where neither perfect nor fuzzy matches "capture" the genus,
  # simply reformat the name and indicate the `taxon_rank = genus`
  i <-
    stringr::str_detect(taxa$tocheck$cleaned_name, " [xX] ") &
    !taxa$tocheck$fuzzy_match_genus %in% resources$genera_accepted$canonicalName
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = "genus",
      aligned_name = paste0(genus, " x [", cleaned_name, "; ", dataset_id, "]"),
      aligned_reason = paste0(
        "match_11_x. Rewording hybrid species name not in APC or APNI to indicate a genus-level alignment, but genus doesn't align to APC accepted genus via fuzzy match (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_11_x"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_12_accepted. Match first three words only to APC accepted names
  # sometimes the submitted name is a valid trinomial + notes
  # such names will only be picked up by matches considering only the first three words of the stripped name
  # this match also does a good job matching phrase names
  # first match to APC accepted taxon names
  i <-
    taxa$tocheck$trinomial %in% resources$`APC list (accepted)`$trinomial
  
  ii <-
    match(taxa$tocheck[i,]$trinomial,
          resources$`APC list (accepted)`$trinomial)
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = resources$`APC list (accepted)`$taxonRank[ii],
      aligned_name = resources$`APC list (accepted)`$canonicalName[ii],
      aligned_reason = paste0(
        "match_12. Automatic alignment with infraspecific canonical name in APC accepted when notes are ignored (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_12_accepted"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_12_known. Match first three words only to APC known names
  # sometimes the submitted name is a valid trinomial + notes
  # such names will only be picked up by matches considering only the first three words of the stripped name
  # this match also does a good job matching phrase names
  # next match to APC known taxon names
  i <-
    taxa$tocheck$trinomial %in% resources$`APC list (known names)`$trinomial
  
  ii <-
    match(taxa$tocheck[i,]$trinomial,
          resources$`APC list (known names)`$trinomial)
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = resources$`APC list (known names)`$taxonRank[ii],
      aligned_name = resources$`APC list (known names)`$canonicalName[ii],
      aligned_reason = paste0(
        "match_12. Automatic alignment with infraspecific canonical name in APC known names when notes are ignored (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_12_known"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_13_fuzzy_accepted. Fuzzy match first three words only to APC accepted names
  # sometimes the submitted name is a valid trinomial + notes
  # such names will only be picked up by matches considering only the first three words of the stripped name
  # this match also does a good job matching phrase names
  # if perfect matches don't work, fuzzy match to APC accepted taxon names
  for (i in 1:nrow(taxa$tocheck)) {
    if (!is.na(taxa$tocheck$trinomial[i])) {
      taxa$tocheck$fuzzy_match_trinomial[i] <-
        fuzzy_match(
          taxa$tocheck$trinomial[i],
          resources$`APC list (accepted)`$trinomial,
          3,
          0.2,
          n_allowed = 1
        )
    }
  }
  
  i <-
    taxa$tocheck$fuzzy_match_trinomial %in% resources$`APC list (accepted)`$trinomial
  
  ii <-
    match(taxa$tocheck[i,]$fuzzy_match_trinomial,
          resources$`APC list (accepted)`$trinomial)
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = resources$`APC list (accepted)`$taxonRank[ii],
      aligned_name = resources$`APC list (accepted)`$canonicalName[ii],
      aligned_reason = paste0(
        "match_13_fuzzy. Fuzzy match alignment with infraspecific canonical name in APC accepted when notes are ignored (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_13_fuzzy_accepted"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_13_fuzzy_known. Fuzzy match first three words only to APC known names
  # this match also does a good job matching phrase names
  # then match to APC known names
  for (i in 1:nrow(taxa$tocheck)) {
    if (!is.na(taxa$tocheck$trinomial[i])) {
      taxa$tocheck$fuzzy_match_trinomial_known[i] <-
        fuzzy_match(
          taxa$tocheck$trinomial[i],
          resources$`APC list (known names)`$trinomial,
          3,
          0.2,
          n_allowed = 1
        )
    }
  }
  
  i <-
    taxa$tocheck$fuzzy_match_trinomial_known %in% resources$`APC list (known names)`$trinomial
  
  ii <-
    match(
      taxa$tocheck[i,]$fuzzy_match_trinomial_known,
      resources$`APC list (known names)`$trinomial
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = resources$`APC list (known names)`$taxonRank[ii],
      aligned_name = resources$`APC list (known names)`$canonicalName[ii],
      aligned_reason = paste0(
        "match_13_fuzzy. Fuzzy match alignment with infraspecific canonical name in APC known when notes are ignored (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_13_fuzzy_known"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_14_accepted. Match first two words only to APC accepted names
  # sometimes the submitted name is a valid binomial + notes
  # or a valid binomial + invalid infraspecific epithet.
  # such names will only be picked up by matches considering only the first two words of the stripped name
  # this match also does a good job matching phrase names
  # first match to APC accepted taxon names
  
  i <-
    taxa$tocheck$binomial %in% resources$`APC list (accepted)`$binomial
  
  ii <-
    match(taxa$tocheck[i,]$binomial,
          resources$`APC list (accepted)`$binomial)
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = resources$`APC list (accepted)`$taxonRank[ii],
      aligned_name = resources$`APC list (accepted)`$canonicalName[ii],
      aligned_reason = paste0(
        "match_14. Automatic alignment with species-level canonical name in APC accepted when notes are ignored (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_14_accepted"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_14_known. Match first two words only to APC known names
  # sometimes the submitted name is a valid binomial + notes
  # or a valid binomial + invalid infraspecific epithet.
  # such names will only be picked up by matches considering only the first two words of the stripped name
  # this match also does a good job matching phrase names
  # next match to APC known taxon names
  i <-
    taxa$tocheck$binomial %in% resources$`APC list (known names)`$binomial
  
  ii <-
    match(taxa$tocheck[i,]$binomial,
          resources$`APC list (known names)`$binomial)
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = resources$`APC list (known names)`$taxonRank[ii],
      aligned_name = resources$`APC list (known names)`$canonicalName[ii],
      aligned_reason = paste0(
        "match_14. Automatic alignment with species-level name known by APC when notes are ignored (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_14_known"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_15_fuzzy_accepted. Fuzzy match first two words only to APC accepted name
  # sometimes the submitted name is a valid binomial + notes
  # or a valid binomial + invalid infraspecific epithet.
  # such names will only be picked up by matches considering only the first two words of the stripped name
  # this match also does a good job matching phrase names
  # if perfect matches don't work, fuzzy match to APC accepted taxon names
  for (i in 1:nrow(taxa$tocheck)) {
    if (!is.na(taxa$tocheck$binomial[i]) &
        is.na(taxa$tocheck$fuzzy_match_binomial[i])) {
      taxa$tocheck$fuzzy_match_binomial[i] <-
        fuzzy_match(
          taxa$tocheck$binomial[i],
          resources$`APC list (accepted)`$binomial,
          3,
          0.2,
          n_allowed = 1
        )
    }
  }
  
  i <-
    taxa$tocheck$fuzzy_match_binomial %in% resources$`APC list (accepted)`$binomial
  
  ii <-
    match(taxa$tocheck[i,]$fuzzy_match_binomial,
          resources$`APC list (accepted)`$binomial)
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = resources$`APC list (accepted)`$taxonRank[ii],
      aligned_name = resources$`APC list (accepted)`$canonicalName[ii],
      aligned_reason = paste0(
        "match_15_fuzzy. Fuzzy match alignment with species-level canonical name in `APC accepted` when everything except first 2 words ignored (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_15_fuzzy_accepted"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_15_fuzzy_known. Fuzzy match first two words only to APC known name
  # sometimes the submitted name is a valid binomial + notes
  # or a valid binomial + invalid infraspecific epithet.
  # if perfect matches don't work, fuzzy match to APC known taxon names
  for (i in 1:nrow(taxa$tocheck)) {
    if (!is.na(taxa$tocheck$binomial[i]) &
        is.na(taxa$tocheck$fuzzy_match_binomial_APC_known[i])) {
      taxa$tocheck$fuzzy_match_binomial_APC_known[i] <-
        fuzzy_match(
          taxa$tocheck$binomial[i],
          resources$`APC list (known names)`$binomial,
          3,
          0.2,
          n_allowed = 1
        )
    }
  }
  
  i <-
    taxa$tocheck$fuzzy_match_binomial_APC_known %in% resources$`APC list (known names)`$binomial
  
  ii <-
    match(
      taxa$tocheck[i,]$fuzzy_match_binomial_APC_known,
      resources$`APC list (known names)`$binomial
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = resources$`APC list (known names)`$taxonRank[ii],
      aligned_name = resources$`APC list (known names)`$canonicalName[ii],
      aligned_reason = paste0(
        "match_15_fuzzy. Fuzzy match alignment with species-level canonical name in `APC known` when everything except first 2 words ignored (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_15_fuzzy_known"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_16_fuzzy_APNI, `APNI names`, fuzzy matches
  # only now, come back to names listed in the APNI, attempting fuzzy matches
  # this is because names exclusively in the APNI are often misspellings of APC accepted/known taxa and
  # these alignments might actually align a taxon that is on the APC to an APNI name
  # this is especially true for phrase names with lots of syntax options
  for (i in 1:nrow(taxa$tocheck)) {
    taxa$tocheck$fuzzy_match_cleaned_APNI[i] <-
      fuzzy_match(
        taxa$tocheck$stripped_name[i],
        resources$`APNI names`$stripped_canonical,
        3,
        0.2,
        n_allowed = 1
      )
  }
  
  i <-
    taxa$tocheck$fuzzy_match_cleaned_APNI %in% resources$`APNI names`$stripped_canonical
  
  ii <-
    match(
      taxa$tocheck[i,]$fuzzy_match_cleaned_APNI,
      resources$`APNI names`$stripped_canonical
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = resources$`APNI names`$taxonRank[ii],
      aligned_name = resources$`APNI names`$canonicalName[ii],
      aligned_reason = paste0(
        "match_16_fuzzy. Fuzzy alignment with canonical name in APNI (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_16_fuzzy_APNI"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_17_fuzzy_APNI, `APNI names`, less precise fuzzy matches
  # follow the main APNI fuzzy match, by a second, less precise fuzzy match
  # it is important to separate them, because it is the imprecise matches that require careful review
  for (i in 1:nrow(taxa$tocheck)) {
    taxa$tocheck$fuzzy_match_cleaned_APNI_imprecise[i] <-
      fuzzy_match(
        taxa$tocheck$cleaned_name[i],
        resources$`APNI names`$canonicalName,
        5,
        0.25,
        n_allowed = 1
      )
  }
  
  i <-
    taxa$tocheck$fuzzy_match_cleaned_APNI_imprecise %in% resources$`APNI names`$canonicalName
  
  ii <-
    match(
      taxa$tocheck[i,]$fuzzy_match_cleaned_APNI_imprecise,
      resources$`APNI names`$canonicalName
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = resources$`APNI names`$taxonRank[ii],
      aligned_name = resources$`APNI names`$canonicalName[ii],
      aligned_reason = paste0(
        "match_17_fuzzy. Imprecise fuzzy alignment with canonical name in APNI (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_17_fuzzy_APNI"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_18_APNI, `APNI` trinomial matches
  # sometimes the submitted name is a valid trinomial + notes
  # such names will only be picked up by matches considering only the first three words of the stripped name
  # this match also does a good job matching phrase names
  # here we match to names listed on the APNI
  i <-
    taxa$tocheck$trinomial %in% resources$`APNI names`$trinomial
  
  ii <-
    match(taxa$tocheck[i,]$trinomial, resources$`APNI names`$trinomial)
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = resources$`APNI names`$taxonRank[ii],
      aligned_name = resources$`APNI names`$canonicalName[ii],
      aligned_reason = paste0(
        "match_18. Automatic alignment with infraspecific canonical name in APNI when notes are ignored (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_18_APNI"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_19_APNI, `APNI` binomial matches
  # sometimes the submitted name is a valid binomial + notes
  # or a valid binomial + invalid infra-specific epithet.
  # such names will only be picked up by matches considering only the first three words of the stripped name
  # this match also does a good job matching phrase names
  # here we match to names listed on the APNI
  i <-
    taxa$tocheck$binomial %in% resources$`APNI names`$binomial
  
  ii <-
    match(taxa$tocheck[i,]$binomial, resources$`APNI names`$binomial)
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = resources$`APNI names`$taxonRank[ii],
      aligned_name = resources$`APNI names`$canonicalName[ii],
      aligned_reason = paste0(
        "match_19. Automatic alignment with species-level canonical name in APNI when notes are ignored (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_19_APNI"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_20_accepted. For remaining taxa, see if first word is an `APC accepted` genus.
  # The `taxon name` itself is reformatted so the second word becomes `sp.` with the original name in brackets.
  i <-
    (taxa$tocheck$genus %in% resources$genera_accepted$canonicalName)
  
  ii <-
    match(taxa$tocheck[i,]$genus,
          resources$genera_accepted$canonicalName)
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = "genus",
      aligned_name = paste0(
        word(resources$genera_accepted$acceptedNameUsage[ii], 1),
        " sp. [",
        cleaned_name,
        "; ",
        dataset_id,
        "]"
      ),
      aligned_reason = paste0(
        "match_20. Rewording name to be recognised as genus rank, with genus accepted by APC (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_20_accepted"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_20_known For remaining taxa, see if first word is an `APC known` genus.
  # The `taxon name` itself is reformatted so the second word becomes `sp.` with the original name in brackets.
  i <-
    (taxa$tocheck$genus %in% resources$genera_known$canonicalName)
  
  ii <-
    match(taxa$tocheck[i,]$genus, resources$genera_known$canonicalName)
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = "genus",
      aligned_name = paste0(
        word(resources$genera_known$acceptedNameUsage[ii], 1),
        " sp. [",
        cleaned_name,
        "; ",
        dataset_id,
        "]"
      ),
      aligned_reason = paste0(
        "match_20. Rewording name to be recognised as genus rank, with genus known by APC (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_20_known"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_20_APNI, For remaining taxa, see if first word is a genus within `APNI`
  # The `taxon name` itself is reformatted so the second word becomes `sp.` with the original name in brackets.
  i <-
    (taxa$tocheck$genus %in% resources$genera_APNI$canonicalName)
  
  ii <-
    match(taxa$tocheck[i,]$genus, resources$genera_APNI$canonicalName)
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = "genus",
      aligned_name = paste0(
        word(resources$genera_APNI$canonicalName[ii], 1),
        " sp. [",
        cleaned_name,
        "; ",
        dataset_id,
        "]"
      ),
      aligned_reason = paste0(
        "match_20. Rewording name to be recognised as genus rank, with genus in APNI (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_20_APNI"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_21. Capture names that are identified as being at the family level by the presence of `sp.` - but now sp. anywhere in the name.
  # The `taxon name` itself is reformatted so the second word becomes `sp.` with the original name in brackets.
  i <-
    stringr::str_detect(word(taxa$tocheck$cleaned_name, 1), "aceae$") &
    taxa$tocheck$genus %in% resources$family_accepted$canonicalName
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = "family",
      aligned_name = paste0(
        word(cleaned_name, 1),
        " sp. [",
        cleaned_name,
        "; ",
        dataset_id,
        "]"
      ),
      aligned_reason = paste0(
        "match_21. Rewording name to be recognised as family rank, with family accepted by APC (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_21"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_22_fuzzy. Capture genus-level alignment
  # Taxa where the entire taxon name string, the first three words, the first two words, and the first word all fail to match
  # should now be fuzzy matches to genus or family.
  # The `taxon name` itself is reformatted so the second word becomes `sp.` with the original name in brackets.
  i <-
    taxa$tocheck$fuzzy_match_genus %in% resources$genera_accepted$canonicalName
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_resolution = "genus",
      aligned_name = paste0(fuzzy_match_genus, " sp. [", cleaned_name, "; ", dataset_id, "]"),
      aligned_reason = paste0(
        "match_22_fuzzy. Aligning name with fuzzy matches genus accepted by APC (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      still_to_match = "match_22_fuzzy"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  return(taxa)
}

#' Standardise Taxon Names
#'
#' This function standardises taxon names by performing a series of text 
#' substitutions to remove common inconsistencies in taxonomic nomenclature.
#' The function takes a character vector of taxon names as input and returns a
#' character vector of standardised taxon names as output.
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