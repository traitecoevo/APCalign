#' Fuzzy match taxonomic names
#' 
#' This function attempts to match input strings to a list of allowable taxonomic names.
#' It requires that the first letter (or digit) of each word is identical between the input and output strings to avoid mis-matches
#' 
#' @param txt The string of text requiring a match
#' @param accepted_list The list of accepted names attempting to match to
#' @param max_distance_abs The maximum allowable number of characters differing between the input string and the match
#' @param max_distance_rel The maximum proportional difference between the input string and the match
#' @param n_allowed The number of allowable matches returned. Defaults to 1
#'
#' @return A text string that matches a recognised taxon name or scientific name
#' @export
#'
#' @examples
fuzzy_match <- function(txt, accepted_list, max_distance_abs, max_distance_rel, n_allowed = 1) {
  
  words_in_text <- 1 + stringr::str_count(txt," ")
  
  txt_word1_start <- stringr::str_extract(txt, "[:alpha:]")
  
  if(words_in_text > 1) {
    txt_word2_start <- stringr::str_extract(word(txt,2), "[:alpha:]|[:digit:]")
  }
  
  if(words_in_text > 2) {
    txt_word3_start <- stringr::str_extract(word(txt,3), "[:alpha:]|[:digit:]")
  }
  
  distance_c <- utils::adist(txt, accepted_list, fixed=TRUE)[1,]
  
  min_dist_abs_c <-  min(distance_c)
  min_dist_per_c <-  min(distance_c) / stringr::str_length(txt)
  
  i <- which(distance_c==min_dist_abs_c)
  
  if(
    ## Within allowable number of characters (absolute)
    min_dist_abs_c <= max_distance_abs &
    ## Within allowable number of characters (relative)
    min_dist_per_c <= max_distance_rel &
    ## Is a unique solution
    length(i)<=n_allowed
  ) {
    words_in_match <- 1 + stringr::str_count(accepted_list[i]," ")
    
    match_word1_start <- stringr::str_extract(accepted_list[i], "[:alpha:]")
    
    if(words_in_text > 1) {
      match_word2_start <- stringr::str_extract(word(accepted_list[i],2), "[:alpha:]|[:digit:]")
    }
    
    if(words_in_text > 2) {
      match_word3_start <- stringr::str_extract(word(accepted_list[i],3), "[:alpha:]|[:digit:]")
    }
    
    keep = FALSE
    
    if(words_in_text == 1) {
      if (txt_word1_start == match_word1_start) {
        keep = TRUE }
      
    } else if(words_in_text == 2) {
      if (txt_word1_start == match_word1_start & txt_word2_start == match_word2_start) {
        keep = TRUE }
      
    } else if(words_in_text > 2) {
      if (words_in_match > 2) {
        if (txt_word1_start == match_word1_start & txt_word2_start == match_word2_start & txt_word3_start == match_word3_start) {
          keep = TRUE }
      } else if (txt_word1_start == match_word1_start & txt_word2_start == match_word2_start) {
        keep = TRUE }
    }
    
    if(keep == TRUE) {
      
      return(accepted_list[i])
      
    }
    return(NA)
  }
  return(NA)
}

#' Strip names of punctuation and filler words
#'
#' @param x Text string, generally a taxonomic name
#'
#' @return Taxonomic name stripped of punctuation and filler words, excluding sp.
#' @export
#'
#' @examples
strip_names <- function(x) {
  x %>% 
    stringr::str_replace_all("[:punct:]", " ") %>%
    stringr::str_replace_all(" subsp ", " ") %>% 
    stringr::str_replace_all(" var |var$", " ") %>% 
    stringr::str_replace_all(" ser ", " ") %>% 
    stringr::str_replace_all(" f ", " ") %>%
    stringr::str_replace_all(" s l ", " ") %>% 
    stringr::str_replace_all(" s s ", " ") %>% 
    stringr::str_replace_all("\\=", " ") %>%
    stringr::str_replace_all("  ", " ") %>%
    stringr::str_squish() %>% tolower() 
}

#' Strip names of punctuation and filler words, including sp.
#'
#' @param x Text string, generally a taxonomic name
#'
#' @return Taxonomic name stripped of punctuation and filler words, including sp.
#' @export
#'
#' @examples
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
    stringr::str_squish() %>% tolower() 
}


#' Builds list of potential species from the Australian Plant Census (APC) and 
#' Australian Plant Names Index (APNI)
#' 
#' Compiled list is saved at "config/taxon_list.csv". While this list is 
#' only an intermediate structure constructed entirely from 
#' the downloaded files, it saves us keeping copies of the entire 
#' lists (~8 vs 230Mb)
#' 
#' @param austraits austraits data object
#' @param taxonomic_resources resources used for building taxon list
#' @importFrom rlang .data
#' @export
austraits_rebuild_taxon_list <- function(austraits, taxonomic_resources) {
  
  subset_accepted <- function(x) {
    x[x!= "accepted"]
  }

  # First align to APC where possible 
  
  taxa <- 
    # build list of observed taxon names
    austraits$traits %>% 
      dplyr::select(cleaned_name = .data$taxon_name) %>%
      dplyr::mutate(
        complete_name = cleaned_name,
        cleaned_name = stringr::str_split_fixed(.data$cleaned_name, "\\[",2)[,1] %>% str_trim(),
        cleaned_name = stringr::str_replace(.data$cleaned_name, " sp\\.$",""), 
        cleaned_name = stringr::str_replace(.data$cleaned_name, " x$","")
      ) %>% 
      dplyr::distinct() %>%
      # match our cleaned names against names in APC list
      dplyr::left_join(
        by = "cleaned_name", taxonomic_resources$APC %>% dplyr::arrange(.data$taxonomicStatus) %>% dplyr::distinct(.data$canonicalName, .keep_all = TRUE) %>%
        dplyr::select(cleaned_name = .data$canonicalName, cleaned_scientific_name_id = .data$scientificNameID, 
                      cleaned_name_taxonomic_status = .data$taxonomicStatus, accepted_name_usage_id = .data$acceptedNameUsageID)) %>%
      # Also add all accepted genera species, varieties etc from APC
      dplyr::bind_rows(
        taxonomic_resources$APC %>% 
          dplyr::filter(.data$taxonRank %in% c('Familia', 'Series', 'Genus', 'Species', 'Forma', 'Varietas', 'Subspecies'), 
                        .data$taxonomicStatus == "accepted") %>% 
          dplyr::select(cleaned_name = .data$canonicalName, complete_name = .data$canonicalName, cleaned_scientific_name_id = .data$scientificNameID, 
                        cleaned_name_taxonomic_status = .data$taxonomicStatus, accepted_name_usage_id = .data$acceptedNameUsageID)) %>%
      dplyr::arrange(.data$complete_name) %>%
      dplyr::distinct(.data$complete_name, .data$cleaned_name, .keep_all = TRUE) %>% 
      dplyr::mutate(taxonomic_reference = ifelse(!is.na(.data$cleaned_scientific_name_id), "APC", NA_character_)) %>%
    # Some values for `cleaned_name` will have multiple matches in the APC. 
    # We will prefer the accepted usage, but record others if they exist
    # To do this we define the order we want variables to sort in the order listed below with accepted at the top
    dplyr::mutate(my_order = .data$cleaned_name_taxonomic_status %>% 
                    forcats::fct_relevel(c("accepted", "taxonomic synonym", "nomenclatural synonym", 
                                           "doubtful taxonomic synonym", "basionym", "orthographic variant",
                                           "replaced synonym", "doubtful pro parte taxonomic synonym", "pro parte taxonomic synonym",
                                           "doubtful misapplied", "doubtful pro parte misapplied",
                                           "misapplied", "pro parte misapplied"))) %>%
    dplyr::arrange(.data$cleaned_name, .data$my_order) %>%
      # For all names that can be linked to a taxon `accepted` by APC, add in additional columns of data
      # These matches are done using `accepted_name_usage_id`, because this identifier is the same for all `known` names that link
        # to a single `accepted` name
      # Only `accepted` names are merged in right now, because the goal is to add in the `taxon_id` and `scientific_name_id` for
        # the `accepted` name only
    dplyr::left_join(
        by = "accepted_name_usage_id", taxonomic_resources$APC %>%
          dplyr::filter(.data$taxonomicStatus == "accepted") %>% 
          dplyr::select(accepted_name_usage_id = .data$acceptedNameUsageID,
                        taxon_id = .data$taxonID, taxon_name = .data$canonicalName, 
                        taxonomic_status = .data$taxonomicStatus,  
                        scientific_name = .data$scientificName, scientific_name_id = .data$scientificNameID, 
                        scientific_name_authorship = .data$scientificNameAuthorship, .data$family,
                        taxon_distribution = .data$taxonDistribution, taxon_rank = .data$taxonRank)) %>% 
      # For each species, keep the first record (accepted if present) and 
      # record any alternative status to indicate where there was ambiguity
    dplyr::group_by(.data$accepted_name_usage_id) %>%
      dplyr::mutate(
        cleaned_name_alternative_taxonomic_status = ifelse(.data$cleaned_name_taxonomic_status[1] == "accepted", 
                                                 .data$cleaned_name_taxonomic_status %>% 
            unique() %>% 
            subset_accepted() %>% 
            paste0(collapse = " | ") %>% 
            dplyr::na_if(""), NA_character_)) %>% 
      #dplyr::slice(1) %>%  
      dplyr::ungroup() %>%
      dplyr::select(-.data$my_order) %>%
      # Add in `establishment_means`, indicating if a taxon is native, naturalised or both
      # This code is based on the exact syntax for taxon_distribution in APC; 
        # the word `native` is used only if a taxon is both native and naturalised in a state
      dplyr::mutate(
        count_naturalised = stringr::str_count(.data$taxon_distribution, "naturalised"),
        count_n_and_n = stringr::str_count(.data$taxon_distribution, "native and naturalised"),
        count_states = stringr::str_count(.data$taxon_distribution, ",") + 1,
        establishment_means = ifelse(.data$count_naturalised > 0 & .data$count_n_and_n == 0, "naturalised", NA),
        establishment_means = ifelse(.data$count_n_and_n > 0 | (.data$count_naturalised > 0 & .data$count_states > .data$count_naturalised), "native and naturalised", .data$establishment_means),
        establishment_means = ifelse(.data$count_naturalised == 0 & .data$count_n_and_n == 0, "native", .data$establishment_means),
      ) %>%
      dplyr::select(.data$cleaned_name, .data$complete_name, .data$taxonomic_reference, .data$cleaned_scientific_name_id, .data$cleaned_name_taxonomic_status, 
                    .data$cleaned_name_alternative_taxonomic_status, 
                    .data$taxon_name, .data$taxon_id, .data$scientific_name_authorship, .data$taxon_rank, 
                    .data$taxonomic_status, .data$family, .data$taxon_distribution, .data$establishment_means,
                    .data$scientific_name, .data$scientific_name_id)
    
  # taxa 1 is the `cleaned names` that have been matched to an `accepted` or `known` name in APC and therefore now have a `cleaned_scientific_name_id` assigned
  
  taxa1 <- 
    taxa %>% 
      dplyr::filter(!is.na(.data$scientific_name_id)) %>%
      dplyr::mutate(
        cleaned_name = ifelse(.data$taxon_rank %in% c("Familia", "family", "Genus", "genus"), .data$complete_name, .data$cleaned_name),
        complete_name = ifelse(is.na(.data$complete_name), .data$cleaned_name, .data$complete_name)
        ) %>%
      dplyr::distinct(.data$complete_name, .data$cleaned_name, .data$taxon_name, .keep_all = TRUE)
  
  # Now check against APNI for any `cleaned names` not found in APC
  # Only keep those species with a match

  taxa2 <-
    taxa %>% 
      dplyr::filter(is.na(.data$scientific_name_id)) %>%
      dplyr::select(.data$cleaned_name, .data$complete_name) %>%
      dplyr::left_join(by = "cleaned_name", taxonomic_resources$APNI %>%
                         dplyr::select(cleaned_name = .data$canonicalName, cleaned_scientific_name_id = .data$scientificNameID, 
                                       .data$family, taxon_rank = .data$taxonRank, scientific_name = .data$scientificName)) %>%
      dplyr::group_by(.data$cleaned_name) %>%
        dplyr::mutate(
          cleaned_scientific_name_id = paste(.data$cleaned_scientific_name_id, collapse = " ") %>% 
            dplyr::na_if("NA"),
          family = ifelse(dplyr::n_distinct(.data$family) > 1, NA_character_, .data$family[1])) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        taxonomic_reference = as.character(ifelse(is.na(.data$cleaned_scientific_name_id), NA_character_, "APNI")),
        taxon_name = as.character(ifelse(is.na(.data$cleaned_scientific_name_id), NA_character_, .data$cleaned_name)),
        cleaned_name_taxonomic_status = as.character(ifelse(is.na(.data$cleaned_scientific_name_id), "unknown", "unplaced by APC")),
        taxonomic_status = as.character(.data$cleaned_name_taxonomic_status),
        scientific_name_id = .data$cleaned_scientific_name_id,
        cleaned_name = ifelse(.data$taxon_rank %in% c("Familia", "family", "Genus", "genus"), .data$complete_name, .data$cleaned_name)
        )

  taxa_all <- taxa1 %>% 
    dplyr::bind_rows(taxa2 %>% 
        dplyr::filter(!is.na(.data$cleaned_scientific_name_id))) %>% 
    dplyr::arrange(.data$cleaned_name)  %>%
    dplyr::select(-.data$complete_name) %>%
    dplyr::distinct(.data$cleaned_name, .keep_all = TRUE)
  
  taxa_all %>%
    readr::write_csv("config/taxon_list.csv")
} 

