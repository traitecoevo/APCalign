### Create table with all current updates

all_updates <- tibble()

for (i in 1:288) {
  path <- file.path("data", dataset_names$dataset_id[i], "metadata.yml")
  meta_tmp <- read_yaml(path)
  
  if(!is.na(meta_tmp$taxonomic_updates[1])) {
    substitutions <- 
      meta_tmp$taxonomic_updates %>% 
      util_list_to_df2() %>%
      mutate(dataset_id = dataset_names$dataset_id[i])
    
    all_updates <- all_updates %>% bind_rows(substitutions)
  }
  
  all_updates
}

### Load APC & APNI files (from taxonomy.R)
#source("taxon_matching_functions.R")
library(dplyr)
library(tidyr)
library(readr)

file_paths <- list(
  APC = "config/NSL/APC-taxon-2022-10-21-4554.csv",
  APNI = "config/NSL/APNI-names-2022-10-21-4546.csv"
)

taxonomic_resources <- list()
taxonomic_resources$APC <- read_csv_char(file_paths$APC)
taxonomic_resources$APNI <- read_csv_char(file_paths$APNI) %>% dplyr::distinct(.data$canonicalName, .keep_all = TRUE)
taxonomic_resources[["genera_accepted"]] <- taxonomic_resources$APC %>% dplyr::filter(taxonRank %in% c('Genus'), taxonomicStatus == "accepted")

to_check <- list()
to_review <- tibble::tibble(dataset_id = character(), taxon_name = character())

APC_tmp <- 
  taxonomic_resources$APC %>% 
  dplyr::filter(.data$taxonRank %in% c('Series', 'Subspecies', 'Species', 'Forma', 'Varietas')) %>% 
  dplyr::select(.data$canonicalName, .data$scientificName, .data$taxonomicStatus, ID = .data$taxonID, .data$nameType, .data$taxonRank) %>% 
  dplyr::mutate(
    stripped_canonical = strip_names(.data$canonicalName),
    stripped_canonical2 = strip_names_2(.data$canonicalName),
    stripped_scientific = strip_names(.data$scientificName),
    binomial = ifelse(taxonRank == "Species",stringr::word(.data$stripped_canonical2, start = 1, end = 2),"zzzz zzzz"),
    binomial = ifelse(is.na(binomial), "zzzz zzzz", binomial),
    binomial = base::replace(binomial, duplicated(binomial), "zzzz zzzz"),
    genus = stringr::word(.data$stripped_canonical, 1),
    trinomial = stringr::word(.data$stripped_canonical2, start = 1, end = 3),
    trinomial = ifelse(is.na(trinomial), "zzzz zzzz", trinomial),
    trinomial = base::replace(trinomial, duplicated(trinomial), "zzzz zzzz"),
  ) %>%
  dplyr::distinct()

to_check[["APC list (accepted)"]] <- APC_tmp %>% dplyr::filter(.data$taxonomicStatus == "accepted") %>% mutate(taxonomic_ref = "APC accepted")
to_check[["APC list (known names)"]] <- APC_tmp %>% dplyr::filter(.data$taxonomicStatus != "accepted") %>% mutate(taxonomic_ref = "APC known")

to_check[["APNI names"]] <- 
  taxonomic_resources$APNI %>% dplyr::filter(.data$nameElement != "sp.")  %>%
  dplyr::filter(!.data$canonicalName %in% APC_tmp$canonicalName) %>% 
  dplyr::select(.data$canonicalName, .data$scientificName, ID = .data$scientificNameID, .data$nameType, .data$taxonRank) %>% 
  dplyr::filter(.data$taxonRank %in% c('Series', 'Subspecies', 'Species', 'Forma', 'Varietas')) %>% 
  dplyr::mutate(
    taxonomicStatus = "unplaced for APC", 
    stripped_canonical = strip_names(.data$canonicalName),
    stripped_canonical2 = strip_names_2(.data$canonicalName),
    stripped_scientific = strip_names(.data$scientificName),
    binomial = ifelse(taxonRank == "Species",stringr::word(.data$stripped_canonical2, start = 1, end = 2),"zzzz zzzz"), 
    binomial = ifelse(is.na(binomial), "zzzz zzzz", binomial),
    trinomial = stringr::word(.data$stripped_canonical2, start = 1, end = 3),
    trinomial = ifelse(is.na(trinomial), "zzzz zzzz", trinomial),
    trinomial = base::replace(trinomial, duplicated(trinomial), "zzzz zzzz"),
    genus = stringr::word(.data$stripped_canonical, 1),
    taxonomic_ref = "APNI"
  ) %>%
  dplyr::distinct() %>% dplyr::arrange(.data$canonicalName)
# the `zzzz zzzz` is because the fuzzy matching algorithm can't handles NA's
# stripped_2 gets rid of `sp` and `spp` which is helpful for some matches

genera_accepted <-  
  taxonomic_resources$APC %>% 
    dplyr::select(.data$canonicalName, .data$acceptedNameUsage, .data$scientificName, .data$taxonomicStatus, ID = .data$taxonID, .data$nameType, .data$taxonRank) %>%
    dplyr::filter(.data$taxonRank %in% c('Genus'), .data$taxonomicStatus == "accepted") %>% 
    dplyr::mutate(taxonomic_ref = "APC accepted")
genera_known <-  
  taxonomic_resources$APC %>% 
  dplyr::select(.data$canonicalName, .data$acceptedNameUsage, .data$scientificName, .data$taxonomicStatus, ID = .data$taxonID, .data$nameType, .data$taxonRank) %>%
    dplyr::filter(.data$taxonRank %in% c('Genus')) %>% 
    dplyr::filter(!.data$canonicalName %in% genera_accepted$canonicalName) %>% 
    dplyr::mutate(taxonomic_ref = "APC known")
genera_APNI <- 
  taxonomic_resources$APNI %>% 
    dplyr::select(.data$canonicalName, .data$taxonomicStatus, .data$nameType, .data$taxonRank, .data$scientificName) %>%
    dplyr::filter(.data$taxonRank %in% c('Genus')) %>% 
    dplyr::filter(!.data$canonicalName %in% taxonomic_resources$APC$canonicalName) %>% 
    dplyr::mutate(taxonomic_ref = "APNI")
genera_all <- bind_rows(genera_accepted, genera_known, genera_APNI) %>%
    dplyr::mutate(
      cleaned_name = stringr::word(.data$acceptedNameUsage,1),
      cleaned_name = ifelse(is.na(cleaned_name), canonicalName, cleaned_name)
         ) %>%
    dplyr::distinct(.data$cleaned_name, .data$canonicalName, .data$scientificName, .keep_all = TRUE)
family_accepted <-  taxonomic_resources$APC %>% dplyr::filter(.data$taxonRank %in% c('Familia'), .data$taxonomicStatus == "accepted") 

### Start matching

updates <- update %>% 
  rename(find = taxon_name)%>%
  mutate(still_to_match = "needs_match") %>%
  select(-taxonomic_reference, -taxon_rank)

try_again %>% 
  rename(find = taxon_name) %>%
  mutate(still_to_match = "needs_match") %>%
  mutate(dataset_id = "Sweedman_2006") -> updates

updates %>% distinct() %>%
  dplyr::mutate(
    stripped_name = strip_names(find),
    stripped_name2 = strip_names_2(find),
    cleaned_name = process_standardise_names(find),
    trinomial = stringr::word(stripped_name2, start = 1, end = 3),
    binomial = stringr::word(stripped_name2, start = 1, end = 2),
    genus = stringr::word(find, start = 1, end = 1),
    replace_new = NA,
    reason_new = NA,
    fuzzy_match_genus = NA,
    fuzzy_match_genus_known = NA,
    fuzzy_match_genus_APNI = NA,
    fuzzy_match_binomial = NA,
    fuzzy_match_binomial_APC_known = NA,
    fuzzy_match_trinomial = NA,
    fuzzy_match_trinomial_known = NA,
    fuzzy_match_cleaned_APC = NA,
    fuzzy_match_cleaned_APC_known = NA,
    fuzzy_match_cleaned_APNI = NA,
    fuzzy_match_cleaned_APC_imprecise = NA,
    fuzzy_match_cleaned_APC_known_imprecise = NA,
    fuzzy_match_cleaned_APNI_imprecise = NA,
    taxonomic_ref = NA,
    taxonomic_resolution = NA
    ) -> updates2

#### Start matches

# fuzzy match genera
  # it is important for fuzzy matches to be done on somewhat limited lists of species 
  # therefore it is best to keep the two APC components and APNI separate
  # this ensures that there are more unique matches - and that the match for the prioritised list is the one that is output

for (i in 1:nrow(updates2)) {
  updates2$fuzzy_match_genus[[i]] <- fuzzy_match(updates2$genus[[i]], genera_accepted$canonicalName, 2, 0.35, n_allowed = 1)
}

for (i in 1:nrow(updates2)) {
  updates2$fuzzy_match_genus_known[[i]] <- fuzzy_match(updates2$genus[[i]], genera_known$canonicalName, 2, 0.35, n_allowed = 1)
}

for (i in 1:nrow(updates2)) {
  updates2$fuzzy_match_genus_APNI[[i]] <- fuzzy_match(updates2$genus[[i]], genera_APNI$canonicalName, 2, 0.35, n_allowed = 1)
}

# match_01, `genus sp.` matches, across all lists simultaneously
  # for names where the final "word" is `sp` or `spp`, immediately indicate it can only be aligned to `taxonomic_resolution = "genus"` (or family)
  # first match against known genera in any resource 
i <- updates2$still_to_match == "needs_match" &
  (stringr::str_detect(updates2$stripped_name,"[:space:]sp$")|stringr::str_detect(updates2$stripped_name,"[:space:]spp$")) &
  updates2$genus %in% genera_all$canonicalName &
  stringr::word(updates2$stripped_name, 2) %in% c("sp","spp")

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_ref = genera_all$taxonomic_ref[match(updates2[i,]$genus, genera_all$canonicalName)],
    taxonomic_resolution = "genus",
    replace_new = paste0(genera_all$cleaned_name[match(updates2[i,]$genus, genera_all$canonicalName)]," sp. [", dataset_id, "]"),
    reason_new = paste0("match_01. Rewording taxon with ending with `sp.` to indicate a genus-level alignment with `", taxonomic_ref ,"` name (", Sys.Date(),")"),
    still_to_match = "match_01"
  )

# match_01_fuzzy_accepted, `genus sp.` matches, across all lists simultaneously
  # for names where the final "word" is `sp` or `spp`, if it doesn't exactly match a genus name, try fuzzy matches with APC accepted genera 
i <- updates2$still_to_match == "needs_match" &
  (stringr::str_detect(updates2$stripped_name,"[:space:]sp$")|stringr::str_detect(updates2$stripped_name,"[:space:]spp$")) &
  updates2$fuzzy_match_genus %in% genera_all$canonicalName &
  stringr::word(updates2$stripped_name, 2) %in% c("sp","spp")

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_ref = genera_all$taxonomic_ref[match(updates2[i,]$fuzzy_match_genus, genera_all$canonicalName)],
    taxonomic_resolution = "genus",
    replace_new = paste0(genera_all$cleaned_name[match(updates2[i,]$fuzzy_match_genus, genera_all$canonicalName)]," sp. [", dataset_id, "]"),
    reason_new = paste0("match_01_fuzzy. Fuzzy match of name ending with `sp.` to an APC accepted genus (", Sys.Date(),")"),
    still_to_match = "match_01_fuzzy_accepted"
  )

# match_01_fuzzy_known, `genus sp.` matches, across all lists simultaneously
  # for names where the final "word" is `sp` or `spp`, if it doesn't exactly match a genus name, try fuzzy matches with APC known genera
i <- updates2$still_to_match == "needs_match" &
  (stringr::str_detect(updates2$stripped_name,"[:space:]sp$")|stringr::str_detect(updates2$stripped_name,"[:space:]spp$")) &
  updates2$fuzzy_match_genus_known %in% genera_all$canonicalName &
  stringr::word(updates2$stripped_name, 2) %in% c("sp","spp")

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_ref = genera_all$taxonomic_ref[match(updates2[i,]$fuzzy_match_genus_known, genera_all$canonicalName)],
    taxonomic_resolution = "genus",
    replace_new = paste0(word(genera_known$acceptedNameUsage[match(updates2[i,]$fuzzy_match_genus_known, genera_known$canonicalName)],1)," sp. [", dataset_id, "]"),
    reason_new = paste0("match_01_fuzzy. Fuzzy match of name ending with `sp.` to an APC known genus (", Sys.Date(),")"),
    still_to_match = "match_01_fuzzy_known"
  )

# match_02, `family sp.` matches
  # for names where the final "word" is `sp` or `spp`, that do not match to a genus, see if they instead match to a family
i <- updates2$still_to_match == "needs_match" &
  (stringr::str_detect(updates2$stripped_name,"[:space:]sp$")|stringr::str_detect(updates2$stripped_name,"[:space:]spp$")) &
  updates2$genus %in% family_accepted$canonicalName &
  stringr::word(updates2$stripped_name, 2) %in% c("sp","spp")

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = "family",
    replace_new = paste0(genus, " sp. [", dataset_id, "]"),
    reason_new = paste0("match_02. Rewording taxon with ending with `sp.` to indicate a family-level alignment with `APC accepted` name (", Sys.Date(),")"),
    still_to_match = "match_02"
  )

# match_03, `genus species_A -- species_B` (intergrade) matches with all genera (APC, APNI)
  # for names where a double hyphen indicates the plant is an intergrade, automatically align to genus
  # since this is the highest taxon rank that can be attached to the plant name
  # first consider perfect matches within either APC or APNI
i <- updates2$still_to_match == "needs_match" &
  stringr::str_detect(updates2$cleaned_name, "\\ -- ") &
  updates2$genus %in% genera_all$canonicalName

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_ref = genera_all$taxonomic_ref[match(updates2[i,]$genus, genera_all$canonicalName)],
    taxonomic_resolution = "genus",
    replace_new = paste0(genera_all$cleaned_name[match(updates2[i,]$genus, genera_all$canonicalName)], " sp. [", cleaned_name, "; ", dataset_id, "]"),
    reason_new = paste0("match_03. Rewording taxon that are intergrades between two taxa and genus aligns with `", taxonomic_ref ,"` genus (", Sys.Date(),")"),
    still_to_match = "match_03"
  )

# match_03_fuzzy_accepted, `genus species_A -- species_B` (intergrade) matches with fuzzy-matched genus (APC accepted)
  # next consider fuzzy matches to `APC accepted` genera
i <- updates2$still_to_match == "needs_match" &
  stringr::str_detect(updates2$cleaned_name, "\\ -- ") &
  updates2$fuzzy_match_genus %in% genera_accepted$canonicalName

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = "genus",
    replace_new = paste0(fuzzy_match_genus, " sp. [", cleaned_name, "; ", dataset_id, "]"),
    reason_new = paste0("match_03_fuzzy. Rewording taxon that are intergrades between two taxa to indicate a genus-level alignment with APC accepted genus via fuzzy match (", Sys.Date(),")"),
    still_to_match = "match_03_fuzzy_accepted"
  )

# match_03_fuzzy_known, `genus species_A -- species_B` (intergrade) matches with fuzzy-matched genus (APC known)
  # next consider fuzzy matches to `APC known` genera
i <- updates2$still_to_match == "needs_match" &
  stringr::str_detect(updates2$cleaned_name, "\\ -- ") &
  updates2$fuzzy_match_genus_known %in% genera_accepted$canonicalName

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = "genus",
    replace_new = paste0(fuzzy_match_genus_known, " sp. [", cleaned_name, "; ", dataset_id, "]"),
    reason_new = paste0("match_03_fuzzy. Rewording taxon that are intergrades between two taxa to indicate a genus-level alignment with APC known genus via fuzzy match (", Sys.Date(),")"),
    still_to_match = "match_03_fuzzy_known"
  )

# match_03_fuzzy_APNI, `genus species_A -- species_B` (intergrade) matches with fuzzy-matched genus (APNI)
  # next consider fuzzy matches to `APNI` genera
i <- updates2$still_to_match == "needs_match" &
  stringr::str_detect(updates2$cleaned_name, "\\ -- ") &
  updates2$fuzzy_match_genus_APNI %in% genera_accepted$canonicalName

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = "genus",
    replace_new = paste0(fuzzy_match_genus_APNI, " sp. [", cleaned_name, "; ", dataset_id, "]"),
    reason_new = paste0("match_03_fuzzy. Rewording taxon that are intergrades between two taxa to indicate a genus-level alignment with genus in APNI via fuzzy match (", Sys.Date(),")"),
    still_to_match = "match_03_fuzzy_APNI"
  )

# match_03_x, `genus species_A -- species_B` (intergrade) matches with unmatched genus
  # for intergrades where neither perfect nor fuzzy matches "capture" the genus, 
  # simply reformat the name and indicate the `taxon_rank = genus`
i <- updates2$still_to_match == "needs_match" &
  stringr::str_detect(updates2$cleaned_name, "\\ -- ") &
  !updates2$fuzzy_match_genus %in% genera_accepted$canonicalName

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = "genus",
    replace_new = paste0(genus, " sp. [", cleaned_name, "; ", dataset_id, "]"),
    reason_new = paste0("match_03_x. Rewording taxon that are intergrades between two taxa to indicate a genus-level alignment, but genus doesn't align to `", taxonomic_ref ,"` genus via fuzzy match (", Sys.Date(),")"),
    still_to_match = "match_03_x"
  )

# match_04, `genus species_A / species_B` ("indecision") matches with accepted genus
  # for names where a slash ("/") indicates the author is uncertain of the proper taxon name, automatically align to genus
  # since this is the highest taxon rank that can be attached to the plant name
  # first consider perfect matches within either APC or APNI
i <- updates2$still_to_match == "needs_match" &
  (stringr::str_detect(updates2$cleaned_name, "[:alpha:]\\/")|stringr::str_detect(updates2$cleaned_name, "\\s\\/")) &
  updates2$genus %in% genera_all$canonicalName

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_ref = genera_all$taxonomic_ref[match(updates2[i,]$genus, genera_all$canonicalName)],
    taxonomic_resolution = "genus",
    replace_new = paste0(genera_all$cleaned_name[match(updates2[i,]$genus, genera_all$canonicalName)], " sp. [", cleaned_name, "; ", dataset_id, "]"),
    reason_new = paste0("match_04. Rewording taxon where `/` indicates uncertain species identification to align with `", taxonomic_ref, "` genus (", Sys.Date(),")"),
    still_to_match = "match_04"
  )

# match_04_fuzzy_accepted, `genus species_A / species_B` ("indecision") matches with fuzzy-matched genus
  # next consider fuzzy matches to `APC accepted` genera
i <- updates2$still_to_match == "needs_match" &
  (stringr::str_detect(updates2$cleaned_name, "[:alpha:]\\/")|stringr::str_detect(updates2$cleaned_name, "\\s\\/")) &
  updates2$fuzzy_match_genus %in% genera_accepted$canonicalName

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = "genus",
    replace_new = paste0(fuzzy_match_genus, " sp. [", cleaned_name, "; ", dataset_id, "]"),
    reason_new = paste0("match_04_fuzzy. Rewording taxon where `/` indicates uncertain species identification & genus aligned to APC accepted genus via fuzzy match (", Sys.Date(),")"),
    still_to_match = "match_04_fuzzy_accepted"
  )

# match_04_fuzzy_known, `genus species_A / species_B` ("indecision") matches with fuzzy-matched genus (APC known)
  # next consider fuzzy matches to `APC known` genera
i <- updates2$still_to_match == "needs_match" &
  (stringr::str_detect(updates2$cleaned_name, "[:alpha:]\\/")|stringr::str_detect(updates2$cleaned_name, "\\s\\/")) &
  updates2$fuzzy_match_genus_known %in% genera_accepted$canonicalName

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = "genus",
    replace_new = paste0(fuzzy_match_genus_known, " sp. [", cleaned_name, "; ", dataset_id, "]"),
    reason_new = paste0("match_04_fuzzy. Rewording taxon where `/` indicates uncertain species identification & genus aligned to APC known genus via fuzzy match (", Sys.Date(),")"),
    still_to_match = "match_04_fuzzy_known"
  )

# match_04_fuzzy_APNI, `genus species_A / species_B` ("indecision") matches with fuzzy-matched genus (APNI)
# next consider fuzzy matches to `APNI` genera
i <- updates2$still_to_match == "needs_match" &
  (stringr::str_detect(updates2$cleaned_name, "[:alpha:]\\/")|stringr::str_detect(updates2$cleaned_name, "\\s\\/")) &
  updates2$fuzzy_match_genus_APNI %in% genera_accepted$canonicalName

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = "genus",
    replace_new = paste0(fuzzy_match_genus_APNI, " sp. [", cleaned_name, "; ", dataset_id, "]"),
    reason_new = paste0("match_04_fuzzy. Rewording taxon where `/` indicates uncertain species identification & genus aligned to a genus in APNI via fuzzy match (", Sys.Date(),")"),
    still_to_match = "match_04_fuzzy_APNI"
  )

# match_04_x, `genus species_A / species_B` ("indecision") matches with unmatched genus
  # for uncertain species where neither perfect nor fuzzy matches "capture" the genus, 
  # simply reformat the name and indicate the `taxon_rank = genus`
  # **this is a match to check afterwards, because sometimes indicates two possible genus names, instead of indecision over species names
i <- updates2$still_to_match == "needs_match" &
  (stringr::str_detect(updates2$cleaned_name, "[:alpha:]\\/")|stringr::str_detect(updates2$cleaned_name, "\\s\\/")) &
  !updates2$fuzzy_match_genus %in% genera_accepted$canonicalName

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = "genus",
    replace_new = paste0(genus, " sp. [", cleaned_name, "; ", dataset_id, "]"),
    reason_new = paste0("match_04_x. Rewording taxon where `/` indicates uncertain species identification, but genus doesn't align to APC accepted genus via fuzzy match (", Sys.Date(),")"),
    still_to_match = "match_04_x"
  )

# match_05_accepted, `scientific name` matches
  # see if the author has submitted a scientific name, with authorship
i <- updates2$still_to_match == "needs_match" &
  updates2$find %in% to_check$`APC list (accepted)`$scientificName

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = to_check$`APC list (accepted)`$taxonRank[match(updates2[i,]$find, to_check$`APC list (accepted)`$scientificName)],
    replace_new = to_check$`APC list (accepted)`$canonicalName[match(updates2[i,]$find, to_check$`APC list (accepted)`$scientificName)],
    reason_new = paste0("match_05. Automatic alignment with scientific name in APC accepted list (", Sys.Date(),")"),
    still_to_match = "match_05_accepted"
  )

# match_06_accepted, `APC accepted` synonyms
  # see if the name matches an APC accepted taxon name once filler words and punctuation are removed
i <- updates2$still_to_match == "needs_match" &
  updates2$stripped_name2 %in% to_check$`APC list (accepted)`$stripped_canonical

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = to_check$`APC list (accepted)`$taxonRank[match(updates2[i,]$stripped_name2, to_check$`APC list (accepted)`$stripped_canonical)],
    replace_new = to_check$`APC list (accepted)`$canonicalName[match(updates2[i,]$stripped_name2, to_check$`APC list (accepted)`$stripped_canonical)],
    reason_new = paste0("match_06. Automatic alignment with synonymous term among accepted canonical names in APC (", Sys.Date(),")"),
    still_to_match = "match_06_accepted"
  )

# match_06_known, `APC known names`, synonyms
  # see if the name matches an APC known taxon name once filler words and punctuation are removed
i <- updates2$still_to_match == "needs_match" &
  updates2$stripped_name2 %in% to_check$`APC list (known names)`$stripped_canonical


updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = to_check$`APC list (known names)`$taxonRank[match(updates2[i,]$stripped_name2, to_check$`APC list (known names)`$stripped_canonical)],
    replace_new = to_check$`APC list (known names)`$canonicalName[match(updates2[i,]$stripped_name2, to_check$`APC list (known names)`$stripped_canonical)],
    reason_new = paste0("match_06. Automatic alignment with synonymous term among known canonical names APC (", Sys.Date(),")"),
    still_to_match = "match_06_known"
  )

# match_07_fuzzy_accepted, `APC accepted` fuzzy match
  # see if a fuzzy match to an APC accepted taxon name is possible
  # the default is set to only allow changes of up to 3 characters & no more than 20% of the total string length
  # the first letter of each word (up to 3 words) must match
for (i in 1:nrow(updates2)) {
  if(updates2$still_to_match[[i]] == "needs_match") {
    updates2$fuzzy_match_cleaned_APC[[i]] <- fuzzy_match(updates2$stripped_name[[i]], to_check$`APC list (accepted)`$stripped_canonical, 3, 0.2, n_allowed = 1)
  }
}

i <- updates2$still_to_match == "needs_match" &
  updates2$fuzzy_match_cleaned_APC %in% to_check$`APC list (accepted)`$stripped_canonical

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = to_check$`APC list (accepted)`$taxonRank[match(updates2[i,]$fuzzy_match_cleaned_APC, to_check$`APC list (accepted)`$stripped_canonical)],
    replace_new = to_check$`APC list (accepted)`$canonicalName[match(updates2[i,]$fuzzy_match_cleaned_APC, to_check$`APC list (accepted)`$stripped_canonical)],
    reason_new = paste0("match_07_fuzzy. Fuzzy alignment with accepted canonical name in APC (", Sys.Date(),")"),
    still_to_match = "match_07_fuzzy_accepted"
  )

# match_07_fuzzy_known, `APC known names`, fuzzy matches
  # see if a fuzzy match to an APC known taxon name is possible
  # the default is set to only allow changes of up to 3 characters & no more than 20% of the total string length
  # the first letter of each word (up to 3 words) must match
for (i in 1:nrow(updates2)) {
  if(updates2$still_to_match[[i]] == "needs_match") {
    updates2$fuzzy_match_cleaned_APC_known[[i]] <- fuzzy_match(updates2$stripped_name[[i]], to_check$`APC list (known names)`$stripped_canonical, 3, 0.2, n_allowed = 1)
  }
}

i <- updates2$still_to_match == "needs_match" &
  updates2$fuzzy_match_cleaned_APC_known %in% to_check$`APC list (known names)`$stripped_canonical

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = to_check$`APC list (known names)`$taxonRank[match(updates2[i,]$fuzzy_match_cleaned_APC_known, to_check$`APC list (known names)`$stripped_canonical)],
    replace_new = to_check$`APC list (known names)`$canonicalName[match(updates2[i,]$fuzzy_match_cleaned_APC_known, to_check$`APC list (known names)`$stripped_canonical)],
    reason_new = paste0("match_07_fuzzy. Fuzzy alignment with known canonical name in APC (", Sys.Date(),")"),
    still_to_match = "match_07_fuzzy_known"
  )

# match_08_APNI, `APNI names`, synonyms
# see if the name matches a taxon name listed in the APNI once filler words and punctuation are removed
i <- updates2$still_to_match == "needs_match" &
  updates2$stripped_name2 %in% to_check$`APNI names`$stripped_canonical2

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = to_check$`APNI names`$taxonRank[match(updates2[i,]$stripped_name2, to_check$`APNI names`$stripped_canonical2)],
    replace_new = to_check$`APNI names`$canonicalName[match(updates2[i,]$stripped_name2, to_check$`APNI names`$stripped_canonical2)],
    reason_new = paste0("match_08. Automatic alignment with synonymous name in APNI (", Sys.Date(),")"),
    still_to_match = "match_08_APNI"
  )

# match_09, `genus aff. species` matches with accepted genus
  # for names where "aff" indicates the taxon has an affinity to another taxon, but isn't the other taxon, automatically align to genus
  # since this is the highest taxon rank that can be attached to the plant name
  # first consider perfect matches within either APC or APNI
  # **note this reformatting is only done after perfect matches to APC/APNI are considered + initial fuzzy matches to APC,
  # **because there are phrase names that include "sp. aff." and these will now have been picked up
i <- updates2$still_to_match == "needs_match" &
  (stringr::str_detect(updates2$cleaned_name, "[Aa]ff[\\.\\s]")|stringr::str_detect(updates2$cleaned_name, " affinis ")) &
  updates2$genus %in% genera_all$canonicalName

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_ref = genera_all$taxonomic_ref[match(updates2[i,]$genus, genera_all$canonicalName)],
    taxonomic_resolution = "genus",
    replace_new = paste0(genera_all$cleaned_name[match(updates2[i,]$genus, genera_all$canonicalName)], " sp. [", cleaned_name, "; ", dataset_id, "]"),
    reason_new = paste0("match_09. Rewording taxon with term `affinis` preceding species epithet to indicate a genus-level alignment with `", taxonomic_ref ,"` genus (", Sys.Date(),")"),
    still_to_match = "match_09"
  )

# match_09_fuzzy_accepted, `genus aff. species` matches with fuzzy-matched genus
  # next consider fuzzy matches to `APC accepted` genera
i <- updates2$still_to_match == "needs_match" &
  (stringr::str_detect(updates2$cleaned_name, "[Aa]ff[\\.\\s]")|stringr::str_detect(updates2$cleaned_name, " affinis ")) &
  updates2$fuzzy_match_genus %in% genera_accepted$canonicalName

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = "genus",
    replace_new = paste0(fuzzy_match_genus, " sp. [", cleaned_name, "; ", dataset_id, "]"),
    reason_new = paste0("match_09_fuzzy. Rewording taxon with term `affinis` preceding species epithet to indicate a genus-level alignment & genus aligned with APC accepted genus via fuzzy match (", Sys.Date(),")"),
    still_to_match = "match_09_fuzzy_accepted"
  )

# match_09_fuzzy_known, `genus aff. species` matches with fuzzy-matched genus (APC known)
  # next consider fuzzy matches to `APC known` genera
i <- updates2$still_to_match == "needs_match" &
  (stringr::str_detect(updates2$cleaned_name, "[Aa]ff[\\.\\s]")|stringr::str_detect(updates2$cleaned_name, " affinis ")) &
  updates2$fuzzy_match_genus_known %in% genera_known$canonicalName

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = "genus",
    replace_new = paste0(fuzzy_match_genus_known, " sp. [", cleaned_name, "; ", dataset_id, "]"),
    reason_new = paste0("match_09_fuzzy. Rewording taxon with term `affinis` preceding species epithet to indicate a genus-level alignment & genus aligned with APC known genus via fuzzy match (", Sys.Date(),")"),
    still_to_match = "match_09_fuzzy_known"
  )

# match_09_fuzzy_APNI, `genus aff. species` matches with fuzzy-matched genus (APNI)
  # next consider fuzzy matches to `APNI` genera
i <- updates2$still_to_match == "needs_match" &
  (stringr::str_detect(updates2$cleaned_name, "[Aa]ff[\\.\\s]")|stringr::str_detect(updates2$cleaned_name, " affinis ")) &
  updates2$fuzzy_match_genus_APNI %in% genera_known$canonicalName

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = "genus",
    replace_new = paste0(fuzzy_match_genus_APNI, " sp. [", cleaned_name, "; ", dataset_id, "]"),
    reason_new = paste0("match_09_fuzzy. Rewording taxon with term `affinis` preceding species epithet to indicate a genus-level alignment & genus aligned with genus listed on the APNI via fuzzy match (", Sys.Date(),")"),
    still_to_match = "match_09_fuzzy_APNI"
  )

# match match_09_x, `genus aff. species` matches with unmatched genus
  # for `sp. aff.` species where neither perfect nor fuzzy matches "capture" the genus, 
  # simply reformat the name and indicate the `taxon_rank = genus`
i <- updates2$still_to_match == "needs_match" &
  (stringr::str_detect(updates2$cleaned_name, "[Aa]ff[\\.\\s]")|stringr::str_detect(updates2$cleaned_name, " affinis ")) &
  !updates2$fuzzy_match_genus %in% genera_accepted$canonicalName

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = "genus",
    replace_new = paste0(genus, " sp. [", cleaned_name, "; ", dataset_id, "]"),
    reason_new = paste0("match_09_x. Rewording taxon with term `affinis` preceding species epithet to indicate a genus-level alignment, but genus doesn't align to APC accepted genus via fuzzy match (", Sys.Date(),")"),
    still_to_match = "match_09_x"
  )

# match_10_fuzzy_accepted, `APC accepted` fuzzy match
  # now begin a second round of fuzzy matches, with less restrictive matching rules
  # the input taxon name is now allowed to differ by `APC accepted` names by 5 characters & up to 25% of the string length  
  # it is important to separate the more constrained and imprecise fuzzy matches, because it is the imprecise matches that require careful review
for (i in 1:nrow(updates2)) {
  if(updates2$still_to_match[[i]] == "needs_match") {
  updates2$fuzzy_match_cleaned_APC_imprecise[[i]] <- fuzzy_match(updates2$stripped_name[[i]], to_check$`APC list (accepted)`$stripped_canonical, 5, 0.25, n_allowed = 1)
  }
}

i <- updates2$still_to_match == "needs_match" &
  updates2$fuzzy_match_cleaned_APC_imprecise %in% to_check$`APC list (accepted)`$stripped_canonical

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = to_check$`APC list (accepted)`$taxonRank[match(updates2[i,]$fuzzy_match_cleaned_APC_imprecise, to_check$`APC list (accepted)`$stripped_canonical)],
    replace_new = to_check$`APC list (accepted)`$canonicalName[match(updates2[i,]$fuzzy_match_cleaned_APC_imprecise, to_check$`APC list (accepted)`$stripped_canonical)],
    reason_new = paste0("match_10_fuzzy. Imprecise fuzzy alignment with accepted canonical name in APC (", Sys.Date(),")"),
    still_to_match = "match_10_fuzzy_accepted"
  )

# match_10_fuzzy_known, `APC known names`, imprecise fuzzy matches
  # the input taxon name is now allowed to differ by `APC known` names by 5 characters & up to 25% of the string length 
  # it is important to separate the more constrained and imprecise fuzzy matches, because it is the imprecise matches that require careful review
for (i in 1:nrow(updates2)) {
  if(updates2$still_to_match[[i]] == "needs_match") {
    updates2$fuzzy_match_cleaned_APC_known_imprecise[[i]] <- fuzzy_match(updates2$stripped_name[[i]], to_check$`APC list (known names)`$stripped_canonical, 5, 0.25, n_allowed = 1)
  }
}

i <- updates2$still_to_match == "needs_match" &
  updates2$fuzzy_match_cleaned_APC_known_imprecise %in% to_check$`APC list (known names)`$stripped_canonical

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = to_check$`APC list (known names)`$taxonRank[match(updates2[i,]$fuzzy_match_cleaned_APC_known_imprecise, to_check$`APC list (known names)`$stripped_canonical)],
    replace_new = to_check$`APC list (known names)`$canonicalName[match(updates2[i,]$fuzzy_match_cleaned_APC_known_imprecise, to_check$`APC list (known names)`$stripped_canonical)],
    reason_new = paste0("match_10_fuzzy. Imprecise fuzzy alignment with known canonical name in APC (", Sys.Date(),")"),
    still_to_match = "match_10_fuzzy_known"
  )

# match_11, detect `genus species X genus species`, indicating hybrid & match accepted genus
  # for names where a stand-alone x (" x ") indicates taxon is a hybrid, automatically align to genus
  # since this is the highest taxon rank that can be attached to the plant name
  # first consider perfect matches within either APC or APNI
  # **note this reformatting is only done after perfect matches to APC/APNI are considered + initial fuzzy matches to APC,
  # **because there are recognised hybrids on both the APC/APNI and these will now have been picked up
i <- updates2$still_to_match == "needs_match" &
  stringr::str_detect(updates2$cleaned_name," [xX] ") &
  updates2$genus %in% genera_accepted$canonicalName

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = "genus",
    replace_new = paste0(genus, " x [", cleaned_name, "; ", dataset_id, "]"),
    reason_new = paste0("match_11. Rewording hybrid species name not in APC or APNI to indicate a genus-level alignment with APC accepted genus (", Sys.Date(),")"),
    still_to_match = "match_11"
  )

# match_11_fuzzy_accepted, detect `genus species X genus species`, indicating hybrid & align with fuzzy-matched genus
  # next consider fuzzy matches to `APC accepted` genera
i <- updates2$still_to_match == "needs_match" &
  stringr::str_detect(updates2$cleaned_name," [xX] ")&
  updates2$fuzzy_match_genus %in% genera_accepted$canonicalName

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = "genus",
    replace_new = paste0(fuzzy_match_genus, " x [", cleaned_name, "; ", dataset_id, "]"),
    reason_new = paste0("match_11_fuzzy. Rewording hybrid species name not in APC or APNI to indicate a genus-level alignment & genus aligned with APC accepted genus via fuzzy match (", Sys.Date(),")"),
    still_to_match = "match_11_fuzzy_accepted"
  )

# match_11_fuzzy_known, detect `genus species X genus species`, indicating hybrid & align with fuzzy-matched genus
  # next consider fuzzy matches to `APC known` genera
i <- updates2$still_to_match == "needs_match" &
  stringr::str_detect(updates2$cleaned_name," [xX] ")&
  updates2$fuzzy_match_genus_known %in% genera_accepted$canonicalName

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = "genus",
    replace_new = paste0(fuzzy_match_genus_known, " x [", cleaned_name, "; ", dataset_id, "]"),
    reason_new = paste0("match_11_fuzzy. Rewording hybrid species name not in APC or APNI to indicate a genus-level alignment & genus aligned with APC known genus via fuzzy match (", Sys.Date(),")"),
    still_to_match = "match_11_fuzzy_known"
  )

# match_11_fuzzy_APNI, detect `genus species X genus species`, indicating hybrid & align with fuzzy-matched genus
  # next consider fuzzy matches to `APNI` genera
i <- updates2$still_to_match == "needs_match" &
  stringr::str_detect(updates2$cleaned_name," [xX] ")&
  updates2$fuzzy_match_genus_APNI %in% genera_accepted$canonicalName

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = "genus",
    replace_new = paste0(fuzzy_match_genus_APNI, " x [", cleaned_name, "; ", dataset_id, "]"),
    reason_new = paste0("match_11_fuzzy. Rewording hybrid species name not in APC or APNI to indicate a genus-level alignment & genus aligned with a genus list on the APNI via fuzzy match (", Sys.Date(),")"),
    still_to_match = "match_11_fuzzy_APNI"
  )

# match_11_x, detect `genus species X genus species`, indicating hybrid & does not align with genus
  # for hybrid species where neither perfect nor fuzzy matches "capture" the genus, 
  # simply reformat the name and indicate the `taxon_rank = genus`
i <- updates2$still_to_match == "needs_match" &
  stringr::str_detect(updates2$cleaned_name," [xX] ") &
  !updates2$fuzzy_match_genus %in% genera_accepted$canonicalName

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = "genus",
    replace_new = paste0(genus, " x [", cleaned_name, "; ", dataset_id, "]"),
    reason_new = paste0("match_11_x. Rewording hybrid species name not in APC or APNI to indicate a genus-level alignment, but genus doesn't align to APC accepted genus via fuzzy match (", Sys.Date(),")"),
    still_to_match = "match_11_x"
  )

# match_12_accepted. Match first three words only to APC accepted names
  # sometimes the submitted name is a valid trinomial + notes
  # such names will only be picked up by matches considering only the first three words of the stripped name
  # this match also does a good job matching phrase names
  # first match to APC accepted taxon names
i <- updates2$still_to_match == "needs_match" &
  updates2$trinomial %in% to_check$`APC list (accepted)`$trinomial

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = to_check$`APC list (accepted)`$taxonRank[match(updates2[i,]$trinomial, to_check$`APC list (accepted)`$trinomial)],
    replace_new = to_check$`APC list (accepted)`$canonicalName[match(updates2[i,]$trinomial, to_check$`APC list (accepted)`$trinomial)],
    reason_new = paste0("match_12. Automatic alignment with infraspecific canonical name in APC accepted when notes are ignored (", Sys.Date(),")"),
    still_to_match = "match_12_accepted"
  )

# match_12_known. Match first three words only to APC known names
  # sometimes the submitted name is a valid trinomial + notes
  # such names will only be picked up by matches considering only the first three words of the stripped name
  # this match also does a good job matching phrase names
  # next match to APC known taxon names
i <- updates2$still_to_match == "needs_match" &
  updates2$trinomial %in% to_check$`APC list (known names)`$trinomial

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = to_check$`APC list (known names)`$taxonRank[match(updates2[i,]$trinomial, to_check$`APC list (known names)`$trinomial)],
    replace_new = to_check$`APC list (known names)`$canonicalName[match(updates2[i,]$trinomial, to_check$`APC list (known names)`$trinomial)],
    reason_new = paste0("match_12. Automatic alignment with infraspecific canonical name in APC known names when notes are ignored (", Sys.Date(),")"),
    still_to_match = "match_12_known"
  )

# match_13_fuzzy_accepted. Fuzzy match first three words only to APC accepted names
  # sometimes the submitted name is a valid trinomial + notes
  # such names will only be picked up by matches considering only the first three words of the stripped name
  # this match also does a good job matching phrase names
  # if perfect matches don't work, fuzzy match to APC accepted taxon names
for (i in 1:nrow(updates2)) {
  if(!is.na(updates2$trinomial[[i]]) & (updates2$still_to_match[[i]] == "needs_match")){
    updates2$fuzzy_match_trinomial[[i]] <- fuzzy_match(updates2$trinomial[[i]], to_check$`APC list (accepted)`$trinomial, 3, 0.2, n_allowed = 1)
  }
}

i <- updates2$still_to_match == "needs_match" &
  updates2$fuzzy_match_trinomial %in% to_check$`APC list (accepted)`$trinomial

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = to_check$`APC list (accepted)`$taxonRank[match(updates2[i,]$fuzzy_match_trinomial, to_check$`APC list (accepted)`$trinomial)],
    replace_new = to_check$`APC list (accepted)`$canonicalName[match(updates2[i,]$fuzzy_match_trinomial, to_check$`APC list (accepted)`$trinomial)],
    reason_new = paste0("match_13_fuzzy. Fuzzy match alignment with infraspecific canonical name in APC accepted when notes are ignored (", Sys.Date(),")"),
    still_to_match = "match_13_fuzzy_accepted"
  )

# match_13_fuzzy_known. Fuzzy match first three words only to APC known names
  # this match also does a good job matching phrase names
  # then match to APC known names
  for (i in 1:nrow(updates2)) {
    if(!is.na(updates2$trinomial[[i]]) & (updates2$still_to_match[[i]] == "needs_match")){
      updates2$fuzzy_match_trinomial_known[[i]] <- fuzzy_match(updates2$trinomial[[i]], to_check$`APC list (known names)`$trinomial, 3, 0.2, n_allowed = 1)
    }
  }

i <- updates2$still_to_match == "needs_match" &
  updates2$fuzzy_match_trinomial_known %in% to_check$`APC list (known names)`$trinomial

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = to_check$`APC list (known names)`$taxonRank[match(updates2[i,]$fuzzy_match_trinomial_known, to_check$`APC list (known names)`$trinomial)],
    replace_new = to_check$`APC list (known names)`$canonicalName[match(updates2[i,]$fuzzy_match_trinomial_known, to_check$`APC list (known names)`$trinomial)],
    reason_new = paste0("match_13_fuzzy. Fuzzy match alignment with infraspecific canonical name in APC known when notes are ignored (", Sys.Date(),")"),
    still_to_match = "match_13_fuzzy_known"
    )  
  
# match_14_accepted. Match first two words only to APC accepted names
  # sometimes the submitted name is a valid binomial + notes 
  # or a valid binomial + invalid infraspecific epithet.
  # such names will only be picked up by matches considering only the first two words of the stripped name
  # this match also does a good job matching phrase names
  # first match to APC accepted taxon names

i <- updates2$still_to_match == "needs_match" &
  updates2$binomial %in% to_check$`APC list (accepted)`$binomial

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = to_check$`APC list (accepted)`$taxonRank[match(updates2[i,]$binomial, to_check$`APC list (accepted)`$binomial)],
    replace_new = to_check$`APC list (accepted)`$canonicalName[match(updates2[i,]$binomial, to_check$`APC list (accepted)`$binomial)],
    reason_new = paste0("match_14. Automatic alignment with species-level canonical name in APC accepted when notes are ignored (", Sys.Date(),")"),
    still_to_match = "match_14_accepted"
  )

# match_14_known. Match first two words only to APC known names
  # sometimes the submitted name is a valid binomial + notes 
  # or a valid binomial + invalid infraspecific epithet.
  # such names will only be picked up by matches considering only the first two words of the stripped name
  # this match also does a good job matching phrase names
  # next match to APC known taxon names
i <- updates2$still_to_match == "needs_match" &
  updates2$binomial %in% to_check$`APC list (known names)`$binomial

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = to_check$`APC list (known names)`$taxonRank[match(updates2[i,]$binomial, to_check$`APC list (known names)`$binomial)],
    replace_new = to_check$`APC list (known names)`$canonicalName[match(updates2[i,]$binomial, to_check$`APC list (known names)`$binomial)],
    reason_new = paste0("match_14. Automatic alignment with species-level name known by APC when notes are ignored (", Sys.Date(),")"),
    still_to_match = "match_14_known"
  )

# match_15_fuzzy_accepted. Fuzzy match first two words only to APC accepted name
  # sometimes the submitted name is a valid binomial + notes 
  # or a valid binomial + invalid infraspecific epithet.
  # such names will only be picked up by matches considering only the first two words of the stripped name
  # this match also does a good job matching phrase names
  # if perfect matches don't work, fuzzy match to APC accepted taxon names
for (i in 1:nrow(updates2)) {
  if(
    !is.na(updates2$binomial[[i]]) & 
    is.na(updates2$fuzzy_match_binomial[[i]]) &
     updates2$still_to_match[[i]] == "needs_match"
    ) {
    updates2$fuzzy_match_binomial[[i]] <- fuzzy_match(updates2$binomial[[i]], to_check$`APC list (accepted)`$binomial, 3, 0.2, n_allowed = 1)
  }
}

i <- updates2$still_to_match == "needs_match" &
  updates2$fuzzy_match_binomial %in% to_check$`APC list (accepted)`$binomial

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = to_check$`APC list (accepted)`$taxonRank[match(updates2[i,]$fuzzy_match_binomial, to_check$`APC list (accepted)`$binomial)],
    replace_new = to_check$`APC list (accepted)`$canonicalName[match(updates2[i,]$fuzzy_match_binomial, to_check$`APC list (accepted)`$binomial)],
    reason_new = paste0("match_15_fuzzy. Fuzzy match alignment with species-level canonical name in `APC accepted` when everything except first 2 words ignored (", Sys.Date(),")"),
    still_to_match = "match_15_fuzzy_accepted"
  )

# match_15_fuzzy_known. Fuzzy match first two words only to APC known name
  # sometimes the submitted name is a valid binomial + notes 
  # or a valid binomial + invalid infraspecific epithet.
  # if perfect matches don't work, fuzzy match to APC known taxon names
for (i in 1:nrow(updates2)) {
  if(
    !is.na(updates2$binomial[[i]]) & 
    is.na(updates2$fuzzy_match_binomial_APC_known[[i]]) &
    updates2$still_to_match[[i]] == "needs_match"
  ) {
    updates2$fuzzy_match_binomial_APC_known[[i]] <- fuzzy_match(updates2$binomial[[i]], to_check$`APC list (known names)`$binomial, 3, 0.2, n_allowed = 1)
  }
}

i <- updates2$still_to_match == "needs_match" &
  updates2$fuzzy_match_binomial_APC_known %in% to_check$`APC list (known names)`$binomial

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = to_check$`APC list (known names)`$taxonRank[match(updates2[i,]$fuzzy_match_binomial_APC_known, to_check$`APC list (known names)`$binomial)],
    replace_new = to_check$`APC list (known names)`$canonicalName[match(updates2[i,]$fuzzy_match_binomial_APC_known, to_check$`APC list (known names)`$binomial)],
    reason_new = paste0("match_15_fuzzy. Fuzzy match alignment with species-level canonical name in `APC known` when everything except first 2 words ignored (", Sys.Date(),")"),
    still_to_match = "match_15_fuzzy_known"
  )

# match_16_fuzzy_APNI, `APNI names`, fuzzy matches
  # only now, come back to names listed in the APNI, attempting fuzzy matches
  # this is because names exclusively in the APNI are often misspellings of APC accepted/known taxa and 
  # these alignments might actually align a taxon that is on the APC to an APNI name
  # this is especially true for phrase names with lots of syntax options
for (i in 1:nrow(updates2)) {
  if(updates2$still_to_match[[i]] == "needs_match") {
    updates2$fuzzy_match_cleaned_APNI[[i]] <- fuzzy_match(updates2$stripped_name[[i]], to_check$`APNI names`$stripped_canonical, 3, 0.2, n_allowed = 1)
  }
}

i <- updates2$still_to_match == "needs_match" &
  updates2$fuzzy_match_cleaned_APNI %in% to_check$`APNI names`$stripped_canonical

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = to_check$`APNI names`$taxonRank[match(updates2[i,]$fuzzy_match_cleaned_APNI, to_check$`APNI names`$stripped_canonical)],
    replace_new = to_check$`APNI names`$canonicalName[match(updates2[i,]$fuzzy_match_cleaned_APNI, to_check$`APNI`$stripped_canonical)],
    reason_new = paste0("match_16_fuzzy. Fuzzy alignment with canonical name in APNI (", Sys.Date(),")"),
    still_to_match = "match_16_fuzzy_APNI"
  )

# match_17_fuzzy_APNI, `APNI names`, less precise fuzzy matches
  # follow the main APNI fuzzy match, by a second, less precise fuzzy match
  # it is important to separate them, because it is the imprecise matches that require careful review
for (i in 1:nrow(updates2)) {
  if(updates2$still_to_match[[i]] == "needs_match") {
    updates2$fuzzy_match_cleaned_APNI_imprecise[[i]] <- fuzzy_match(updates2$cleaned_name[[i]], to_check$`APNI names`$canonicalName, 5, 0.25, n_allowed = 1)
  }
}

i <- updates2$still_to_match == "needs_match" &
  updates2$fuzzy_match_cleaned_APNI_imprecise %in% to_check$`APNI names`$canonicalName

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = to_check$`APNI names`$taxonRank[match(updates2[i,]$fuzzy_match_cleaned_APNI_imprecise, to_check$`APNI names`$canonicalName)],
    replace_new = to_check$`APNI names`$canonicalName[match(updates2[i,]$fuzzy_match_cleaned_APNI_imprecise, to_check$`APNI names`$canonicalName)],
    reason_new = paste0("match_17_fuzzy. Imprecise fuzzy alignment with canonical name in APNI (", Sys.Date(),")"),
    still_to_match = "match_17_fuzzy_APNI"
  )

# match_18_APNI, `APNI` trinomial matches
  # sometimes the submitted name is a valid trinomial + notes
  # such names will only be picked up by matches considering only the first three words of the stripped name
  # this match also does a good job matching phrase names
  # here we match to names listed on the APNI
i <- updates2$still_to_match == "needs_match" &
  updates2$trinomial %in% to_check$`APNI names`$trinomial

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = to_check$`APNI names`$taxonRank[match(updates2[i,]$trinomial, to_check$`APNI names`$trinomial)],
    replace_new = to_check$`APNI names`$canonicalName[match(updates2[i,]$trinomial, to_check$`APNI names`$trinomial)],
    reason_new = paste0("match_18. Automatic alignment with infraspecific canonical name in APNI when notes are ignored (", Sys.Date(),")"),
    still_to_match = "match_18_APNI"
  )

# match_19_APNI, `APNI` binomial matches
  # sometimes the submitted name is a valid binomial + notes
  # or a valid binomial + invalid infra-specific epithet.
  # such names will only be picked up by matches considering only the first three words of the stripped name
  # this match also does a good job matching phrase names
  # here we match to names listed on the APNI
i <- updates2$still_to_match == "needs_match" &
  updates2$binomial %in% to_check$`APNI names`$binomial

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = to_check$`APNI names`$taxonRank[match(updates2[i,]$binomial, to_check$`APNI names`$binomial)],
    replace_new = to_check$`APNI names`$canonicalName[match(updates2[i,]$binomial, to_check$`APNI names`$binomial)],
    reason_new = paste0("match_19. Automatic alignment with species-level canonical name in APNI when notes are ignored (", Sys.Date(),")"),
    still_to_match = "match_19_APNI"
  )

# match_20_accepted. For remaining taxa, see if first word is an `APC accepted` genus.
  # The `taxon name` itself is reformatted so the second word becomes `sp.` with the original name in brackets.
i <- updates2$still_to_match == "needs_match" &
      (updates2$genus %in% genera_accepted$canonicalName)

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = "genus",
    replace_new = paste0(word(genera_accepted$acceptedNameUsage[match(updates2[i,]$genus, genera_accepted$canonicalName)],1), " sp. [", cleaned_name, "; ", dataset_id, "]"),
    reason_new = paste0("match_20. Rewording name to be recognised as genus rank, with genus accepted by APC (", Sys.Date(),")"),
    still_to_match = "match_20_accepted"
  )

# match_20_known For remaining taxa, see if first word is an `APC known` genus.
  # The `taxon name` itself is reformatted so the second word becomes `sp.` with the original name in brackets.
i <- updates2$still_to_match == "needs_match" &
  (updates2$genus %in% genera_known$canonicalName)

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = "genus",
    replace_new = paste0(word(genera_known$acceptedNameUsage[match(updates2[i,]$genus, genera_known$canonicalName)],1), " sp. [", cleaned_name, "; ", dataset_id, "]"),
    reason_new = paste0("match_20. Rewording name to be recognised as genus rank, with genus known by APC (", Sys.Date(),")"),
    still_to_match = "match_20_known"
  )

# match_20_APNI, For remaining taxa, see if first word is a genus within `APNI`
  # The `taxon name` itself is reformatted so the second word becomes `sp.` with the original name in brackets.
i <- updates2$still_to_match == "needs_match" &
  (updates2$genus %in% genera_APNI$canonicalName)

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = "genus",
    replace_new = paste0(word(genera_APNI$canonicalName[match(updates2[i,]$genus, genera_APNI$canonicalName)],1), " sp. [", cleaned_name, "; ", dataset_id, "]"),
    reason_new = paste0("match_20. Rewording name to be recognised as genus rank, with genus in APNI (", Sys.Date(),")"),
    still_to_match = "match_20_APNI"
  )

# match_21. Capture names that are identified as being at the family level by the presence of `sp.` - but now sp. anywhere in the name.
  # The `taxon name` itself is reformatted so the second word becomes `sp.` with the original name in brackets.
i <- updates2$still_to_match == "needs_match" &
  stringr::str_detect(word(updates2$cleaned_name, 1), "aceae$") & 
  updates2$genus %in% family_accepted$canonicalName

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = "family",
    replace_new = paste0(word(cleaned_name,1), " sp. [", cleaned_name, "; ", dataset_id, "]"),
    reason_new = paste0("match_21. Rewording name to be recognised as family rank, with family accepted by APC (", Sys.Date(),")"),
    still_to_match = "match_21"
  )

# match_22_fuzzy. Capture genus-level alignment
  # Taxa where the entire taxon name string, the first three words, the first two words, and the first word all fail to match
  # should now be fuzzy matches to genus or family.
  # The `taxon name` itself is reformatted so the second word becomes `sp.` with the original name in brackets.
i <- updates2$still_to_match == "needs_match" &
      updates2$fuzzy_match_genus %in% genera_accepted$canonicalName

updates2[i,] <- updates2[i,] %>%
  mutate(
    taxonomic_resolution = "genus",
    replace_new = paste0(fuzzy_match_genus, " sp. [", cleaned_name, "; ", dataset_id, "]"),
    reason_new = paste0("match_22_fuzzy. Aligning name with fuzzy matches genus accepted by APC (", Sys.Date(),")"),
    still_to_match = "match_22_fuzzy"
  )

  