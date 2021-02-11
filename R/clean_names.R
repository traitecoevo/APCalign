

#' Default version of data to use
#'
#' @return
#' @export
#'
#' @examples
default_version <- function() {
  "0.0.0.9000"
}


#' Find taxonomic alignments for a list of names
#'
#' Use APC & APNI to find taxonomic alignments for a list of names
#' 
#' @param original_name list of names to query
#' @param output (optional) name of file to save results
#' @param fuzzy_matching option to turn off fuzzy matching
#' @param max_distance_abs 
#' @param max_distance_rel 
#' @param ver 
#'
#' @return
#' @export
#'
#' @examples
align_taxa <- function(original_name,
                       output = NULL,
                       fuzzy_matching = TRUE,
                       max_distance_abs = 3,
                       max_distance_rel = 0.2,
                       ver = default_version()) {
  
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
  else
    taxa_raw <-
      tibble::tibble(
        original_name = character(0L),
        cleaned_name = character(0L),
        aligned_name = character(0L),
        source = character(0L),
        known = logical(0L),
        checked = logical(0L)
      )
  
  # create list, will have two elements: tocheck, checked
  taxa <- list()
  
  taxa[["tocheck"]] <-
    dplyr::bind_rows(
      taxa_raw,
      tibble::tibble(
        original_name = subset(original_name,!original_name %in% taxa_raw$original_name) %>% unique(),
        cleaned_name = NA_character_,
        stripped_name = NA_character_,
        aligned_name = NA_character_,
        source = NA_character_,
        checked = FALSE,
        known = FALSE
      )
    ) %>%
    dplyr::mutate(
      cleaned_name = ifelse(
        is.na(cleaned_name),
        standardise_names(original_name),
        cleaned_name
      ),
      stripped_name = ifelse(
        is.na(stripped_name),
        strip_names(cleaned_name),
        stripped_name
      )
    )
  
  if (all(taxa$tocheck$checked)) {
    message("  - all taxa are already checked, yay!")
    return(invisible(taxa$tocheck))
  }
  
  # check unknown taxa
  message("  -> ", crayon::blue(sum(taxa$tocheck$known, na.rm = T)), " names already matched; ", 
          crayon::blue(sum(taxa$tocheck$checked & !taxa$tocheck$known, na.rm = T)), " names checked but without a match; ",
          crayon::blue(sum(!taxa$tocheck$checked)), " taxa yet to be checked")
  
  redistribute <- function(data) {
    data[["checked"]] <- dplyr::bind_rows(data[["checked"]],
                               data[["tocheck"]] %>% dplyr::filter(checked))
    
    data[["tocheck"]] <- data[["tocheck"]] %>% dplyr::filter(!checked)
    data
  }
  
  taxa <- redistribute(taxa)
  
  # Not checking anything ending in `sp.`
  # Todo: Note, genus in APC?
  taxa$tocheck <- taxa$tocheck %>%
    dplyr::mutate(checked = ifelse(!checked &
                              grepl("sp\\.$", cleaned_name), TRUE, checked))
  
  taxa <- redistribute(taxa)
  
  message("  -> checking for exact matches for ", nrow(taxa$tocheck), " species")
  
  resources <- load_taxonomic_resources(ver = ver)
  
  for (v in c("APC list (accepted)", "APC list (known names)", "APNI names"))  {
    
    # Compare to canonical name
    i <- match(taxa$tocheck$original_name, resources[[v]]$canonicalName)
    taxa$tocheck$aligned_name <- resources[[v]]$canonicalName[i]
    taxa$tocheck$source[!is.na(i)] <- v
    taxa$tocheck$checked <- !is.na(i)
    
    taxa <- redistribute(taxa)
    
    # Compare to stripped canonical name
    i <- match(taxa$tocheck$stripped_name, resources[[v]]$stripped_canonical)
    taxa$tocheck$aligned_name <- resources[[v]]$canonicalName[i]
    taxa$tocheck$source[!is.na(i)] <- v
    taxa$tocheck$checked <- !is.na(i)
    
    taxa <- redistribute(taxa)
    
    # Compare to scientific name
    i <- match(taxa$tocheck$original_name, resources[[v]]$scientificName)
    taxa$tocheck$aligned_name <- resources[[v]]$canonicalName[i]
    taxa$tocheck$source[!is.na(i)] <- v
    taxa$tocheck$checked <- !is.na(i)
    taxa <- redistribute(taxa)
    
    # Compare to stripped scientific name
    i <-match(taxa$tocheck$stripped_name, resources[[v]]$stripped_scientific)
    
    taxa$tocheck$aligned_name <- resources[[v]]$canonicalName[i]
    taxa$tocheck$source[!is.na(i)] <- v
    taxa$tocheck$checked <- !is.na(i)
    taxa <- redistribute(taxa)
  }
  
  if (fuzzy_matching == TRUE){
    # For any remaining species, look for distance based estimates
    message("  -> checking for fuzzy matches for ", nrow(taxa$tocheck), " taxa")
    
    for (i in seq_len(nrow(taxa$tocheck))) {
      
      stripped_name <- taxa$tocheck$stripped_name[i]
      taxa$tocheck$checked[i] <- TRUE
      
      cat("\t", i, "\t", taxa$tocheck$original_name[i])
      for (v in c("APC list (accepted)", "APC list (known names)", "APNI names"))  {
        
        distance_c <-
          utils::adist(stripped_name, resources[[v]]$stripped_canonical, fixed = TRUE)[1, ]
        min_dist_abs_c <-  min(distance_c)
        min_dist_per_c <-  min(distance_c) / stringr::str_length(stripped_name)
        j <- which(distance_c == min_dist_abs_c)
        
        if (## Within allowable number of characters (absolute)
          min_dist_abs_c <= max_distance_abs &
          ## Within allowable number of characters (relative)
          min_dist_per_c <= max_distance_rel &
          ## Is a unique solution
          length(j) == 1) {
          taxa$tocheck$aligned_name[i] <- resources[[v]]$canonicalName[j]
          taxa$tocheck$source[i] <- v
          break
          
        }
        #Todo: suggestions when no match
        
        distance_s <-
          utils::adist(stripped_name, resources[[v]]$stripped_scientific, fixed = TRUE)[1, ]
        min_dist_abs_s <-  min(distance_s)
        min_dist_per_s <-  min(distance_s) / stringr::str_length(stripped_name)
        j <- which(distance_s == min_dist_abs_s)
        
        if (## Within allowable number of characters (absolute)
          min_dist_abs_s <= max_distance_abs &
          ## Within allowable number of characters (relative)
          min_dist_per_s <= max_distance_rel &
          ## Is a unique solution
          length(j) == 1) {
          taxa$tocheck$aligned_name[i] <- resources[[v]]$canonicalName[j]
          taxa$tocheck$source[i] <- v
          break
          
        }
        #Todo: suggestions when no match
        
      }
    }
    
    if (is.na(taxa$tocheck$aligned_name[i]))
      cat(crayon::blue("\tnot found\n"))
    else
      cat(crayon::green("\tfound:\t"), taxa$tocheck$aligned_name[i],
        "\t", taxa$tocheck$source[i], "\n")
  }
  
  taxa_out <- dplyr::bind_rows(taxa) %>%
    dplyr::mutate(known = !is.na(aligned_name))
  
  if (!is.null(output)) {
    readr::write_csv(taxa_out, output)
    message("  - output saved in file: ", output)
  }
  taxa_out
}


#' Use APC & APNI to update taxonomy, replacing synonyms to current taxa where relevant
#'
#' @param aligned_names
#' @param output (optional) Name of file where results are saved
#' @param ver
#'
#' @return
#' @export
#'
#' @examples
update_taxonomy <- function(aligned_names,
                            output = NULL,
                            ver = default_version()) {
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
  
  resources <- load_taxonomic_resources(ver = ver)
  
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
          taxonDistribution,
          taxonRank,
          ccAttributionIRI
        )
    ) %>%
    # Some species have multiple matches. We will prefer the accepted usage, but record others if they exists
    # To do this we define the order we want variables to sort by,m with accepted at the top
    dplyr::mutate(
      my_order =  forcats::fct_relevel( taxonomicStatusClean, subset(preferred_order, preferred_order %in%  taxonomicStatusClean))
      ) %>%
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
    dplyr::slice(1) %>%
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
    )
  
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
  
  taxa_out <-
    dplyr::bind_rows(taxa_APC,
              taxa_APNI) %>%
    dplyr::arrange(aligned_name) %>%
    dplyr::distinct()
  
  if (!is.null(output)) {
    readr::write_csv(taxa_out, output)
    message("  - output saved in file: ", output)
  }
  taxa_out
}

#' Title
#'
#' @param ver
#' @param reload
#'
#' @return
#' @export
#'
#' @examples

load_taxonomic_resources <-
  function(ver = default_version(), reload = FALSE) {
    # TODO: replace with latest version as default
    
    if (!exists("taxonomic_resources",  envir = .GlobalEnv)) {
      message(crayon::red(
        "loading object `taxonomic_resources` into global environment"
      ))
      taxonomic_resources <- apcnames::dataset_access_function(ver)
      names(taxonomic_resources) <- c("APC", "APNI")
      
      taxonomic_resources[["genera_accepted"]] <-
        taxonomic_resources$APC %>% dplyr::filter(taxonRank %in% c('Genus'), taxonomicStatus == "accepted")
      
      APC_tmp <-
        taxonomic_resources$APC %>%
        dplyr::filter(taxonRank %in% c('Series', 'Subspecies', 'Species', 'Forma', 'Varietas')) %>%
        dplyr::select(canonicalName, scientificName, taxonomicStatus, ID = taxonID) %>%
        dplyr::mutate(
          stripped_canonical = strip_names(canonicalName),
          stripped_scientific = strip_names(scientificName)
        ) %>%
        dplyr::distinct()
      
      taxonomic_resources[["APC list (accepted)"]] <-
        APC_tmp %>% dplyr::filter(taxonomicStatus == "accepted")
      taxonomic_resources[["APC list (known names)"]] <-
        APC_tmp %>% dplyr::filter(taxonomicStatus != "accepted")
      
      taxonomic_resources[["APNI names"]] <-
        taxonomic_resources$APNI %>% dplyr::filter(nameElement != "sp.") %>%
        dplyr::select(canonicalName, scientificName, ID = scientificNameID) %>%
        dplyr::mutate(
          taxonomicStatus = "unplaced",
          stripped_canonical = strip_names(canonicalName),
          stripped_scientific = strip_names(scientificName)
        ) %>%
        dplyr::distinct() %>% 
        dplyr::arrange(canonicalName)
      
      assign("taxonomic_resources", taxonomic_resources, envir = .GlobalEnv)
    }
    
    get0("taxonomic_resources", envir = .GlobalEnv)
  }


#' Title
#'
#' @param taxon_names a vector of names to strip
#'
#' @return a vector of stripped names
#' @export
#'
#' @examples
strip_names <- function(taxon_names) {
  taxon_names %>%
    stringr::str_remove_all(" subsp\\.") %>% stringr::str_remove_all(" aff\\.")  %>%
    stringr::str_remove_all(" var\\.") %>% stringr::str_remove_all(" ser\\.") %>% stringr::str_remove_all(" f\\.") %>%
    stringr::str_remove_all(" s\\.l\\.") %>% stringr::str_remove_all(" s\\.s\\.") %>%
    stringr::str_replace_all("[-._()?]", " ") %>%
    stringr::str_squish() %>%
    tolower()
}


#' Title
#'
#' @param taxon_names
#'
#' @return
#' @export
#'
#' @examples
standardise_names <- function(taxon_names) {
  f <- function(x, find, replace) {
    gsub(find, replace, x, perl = TRUE)
  }
  
  taxon_names %>%
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
