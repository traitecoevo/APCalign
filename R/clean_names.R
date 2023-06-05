#' Get the default version for stable data
#'
#' This function returns the default version for stable data, which is used when no
#' version is specified. The default version is "0.0.1.9000".
#'
#' @return A character string representing the default version for stable data.
#' @export
#'
#' @examples
#' default_version()
#'
#'
#' @seealso
#' align_taxa
#'
#'
default_version <- function() {
  "0.0.2.9000"
}



#' Find taxonomic alignments for a list of names
#'
#' This function uses APC & APNI to find taxonomic alignments for a list of names.
#'
#' @param original_name A list of names to query for taxonomic alignments.
#' @param output (optional) The name of the file to save the results to.
#' @param fuzzy_matching An option to turn off fuzzy matching.
#' @param max_distance_abs The absolute distance in substitution space.
#' @param max_distance_rel The relative distance in substitution space.
#' @param ver The version number for the flora resource.
#'
#' @return A tibble with columns: original_name, cleaned_name, aligned_name, source, known, and checked.
#' @export
#'
#' @examples
#' align_taxa(c("Poa annua", "Abies alba"), output = "taxa_alignments.csv")
#'
#' @importFrom readr read_csv cols col_logical col_character
#' @importFrom tibble tibble
#'
#' @keywords taxonomic alignments, APC, APNI, flora resource
#'
#' @seealso
#' \code{\link{default_version}} for the default value of the \code{ver} parameter.
#'
#' @family taxonomic alignment functions
#'
#' @rdname align_taxa
#'
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
        original_name = subset(original_name, !original_name %in% taxa_raw$original_name) %>% unique(),
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
  message(
    "  -> ",
    crayon::blue(sum(taxa$tocheck$known, na.rm = T)),
    " names already matched; ",
    crayon::blue(sum(
      taxa$tocheck$checked &
        !taxa$tocheck$known, na.rm = T
    )),
    " names checked but without a match; ",
    crayon::blue(sum(!taxa$tocheck$checked)),
    " taxa yet to be checked"
  )
  
  redistribute <- function(data) {
    data[["checked"]] <- dplyr::bind_rows(data[["checked"]],
                                          data[["tocheck"]] %>% dplyr::filter(checked))
    
    data[["tocheck"]] <-
      data[["tocheck"]] %>% dplyr::filter(!checked)
    data
  }
  
  taxa <- redistribute(taxa)
  
  # Not checking anything ending in `sp.`
  # Todo: Note, genus in APC?
  taxa$tocheck <- taxa$tocheck %>%
    dplyr::mutate(checked = ifelse(!checked &
                                     grepl("sp\\.$", cleaned_name), TRUE, checked))
  
  taxa <- redistribute(taxa)
  
  message("  -> checking for exact matches for ",
          nrow(taxa$tocheck),
          " species")
  
  resources <- load_taxonomic_resources(ver = ver)
  
  for (v in c("APC list (accepted)", "APC list (known names)", "APNI names"))  {
    # Compare to canonical name
    i <-
      match(taxa$tocheck$original_name, resources[[v]]$canonicalName)
    taxa$tocheck$aligned_name <- resources[[v]]$canonicalName[i]
    taxa$tocheck$source[!is.na(i)] <- v
    taxa$tocheck$checked <- !is.na(i)
    
    taxa <- redistribute(taxa)
    
    # Compare to stripped canonical name
    i <-
      match(taxa$tocheck$stripped_name,
            resources[[v]]$stripped_canonical)
    taxa$tocheck$aligned_name <- resources[[v]]$canonicalName[i]
    taxa$tocheck$source[!is.na(i)] <- v
    taxa$tocheck$checked <- !is.na(i)
    
    taxa <- redistribute(taxa)
    
    # Compare to scientific name
    i <-
      match(taxa$tocheck$original_name, resources[[v]]$scientificName)
    taxa$tocheck$aligned_name <- resources[[v]]$canonicalName[i]
    taxa$tocheck$source[!is.na(i)] <- v
    taxa$tocheck$checked <- !is.na(i)
    taxa <- redistribute(taxa)
    
    # Compare to stripped scientific name
    i <-
      match(taxa$tocheck$stripped_name,
            resources[[v]]$stripped_scientific)
    
    taxa$tocheck$aligned_name <- resources[[v]]$canonicalName[i]
    taxa$tocheck$source[!is.na(i)] <- v
    taxa$tocheck$checked <- !is.na(i)
    taxa <- redistribute(taxa)
  }
  
  if (fuzzy_matching == TRUE) {
    # For any remaining species, look for distance based estimates
    message("  -> checking for fuzzy matches for ",
            nrow(taxa$tocheck),
            " taxa")
    
    for (i in seq_len(nrow(taxa$tocheck))) {
      stripped_name <- taxa$tocheck$stripped_name[i]
      taxa$tocheck$checked[i] <- TRUE
      
      cat("\t", i, "\t", taxa$tocheck$original_name[i])
      for (v in c("APC list (accepted)",
                  "APC list (known names)",
                  "APNI names"))  {
        distance_c <-
          utils::adist(stripped_name, resources[[v]]$stripped_canonical, fixed = TRUE)[1,]
        min_dist_abs_c <-  min(distance_c)
        min_dist_per_c <-
          min(distance_c) / stringr::str_length(stripped_name)
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
          utils::adist(stripped_name,
                       resources[[v]]$stripped_scientific,
                       fixed = TRUE)[1,]
        min_dist_abs_s <-  min(distance_s)
        min_dist_per_s <-
          min(distance_s) / stringr::str_length(stripped_name)
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
    if (length(taxa$tocheck$aligned_name[i]) == 0)
      cat("nothing to fix")
    else{
      if (is.na(taxa$tocheck$aligned_name[i]))
        cat(crayon::blue("\tnot found\n"))
      else{
        cat(
          crayon::green("\tfound:\t"),
          taxa$tocheck$aligned_name[i],
          "\t",
          taxa$tocheck$source[i],
          "\n"
        )
      }
    }
  }
  
  taxa_out <- dplyr::bind_rows(taxa) %>%
    dplyr::mutate(known = !is.na(aligned_name))
  
  if (!is.null(output)) {
    readr::write_csv(taxa_out, output)
    message("  - output saved in file: ", output)
  }
  taxa_out
}


#' Use APC and APNI to update taxonomy, replacing synonyms to current taxa where relevant
#'
#' This function uses the Australia's Virtual Herbarium's taxonomic resources, specifically the Australian Plant
#' Census (APC) and the Australian Plant Name Index (APNI), to update taxonomy of plant species, replacing any synonyms
#' to their current accepted name.
#'
#' @param aligned_names A character vector of plant names to update. These names must be in the format of the
#' scientific name, with genus and species, and may contain additional qualifiers such as subspecies or varieties.
#' The names are case insensitive.
#'
#' @param output (optional) Name of the file where results are saved. The default is NULL and no file is created.
#' If specified, the output will be saved in a CSV file with the given name.
#'
#' @param ver The version of the taxonomic resources to use. Default is set to the latest version available at
#' the time of running the function.
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
#' @export
#'
#' @examples
#' # Update taxonomy for two plant names and print the result
#' update_taxonomy(c("Eucalyptus pauciflora", "Acacia victoriae"))
#'
#' # Update taxonomy for two plant names and save the result to a CSV file
#' update_taxonomy(c("Eucalyptus pauciflora", "Acacia victoriae"), "updated_taxonomy.csv")
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

#' Load taxonomic resources
#'
#' Loads taxonomic resources into the global environment. This function accesses taxonomic data from a dataset using the provided version number or the default version. The loaded data contains two lists: APC and APNI, which contain taxonomic information about plant species in Australia. The function creates several data frames by filtering and selecting data from the loaded lists.
#'
#' @param ver The version number of the dataset to use. Defaults to the default version.
#' @param reload A logical indicating whether to reload the dataset from the data source. Defaults to FALSE.
#' @param filetype type of file to download. parquet or csv
#'
#' @return The taxonomic resources data loaded into the global environment.
#' @export
#'
#' @examples
#' load_taxonomic_resources()
#'
#'
#' @importFrom dplyr filter select mutate distinct arrange
#' @importFrom crayon red

load_taxonomic_resources <-
  function(ver = default_version(),
           reload = FALSE,
           filetype = "parquet") {
    taxonomic_resources <-
      dataset_access_function(version = ver,
                              path = NULL,
                              type = "stable")
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
    
    return(taxonomic_resources)
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
#' @export
#'
#' @examples
#' strip_names(c("Abies lasiocarpa subsp. lasiocarpa", "Quercus kelloggii", "Pinus contorta var. latifolia"))
#'

strip_names <- function(taxon_names) {
  taxon_names %>%
    stringr::str_remove_all(" subsp\\.") %>% stringr::str_remove_all(" aff\\.")  %>%
    stringr::str_remove_all(" var\\.") %>% stringr::str_remove_all(" ser\\.") %>% stringr::str_remove_all(" f\\.") %>%
    stringr::str_remove_all(" s\\.l\\.") %>% stringr::str_remove_all(" s\\.s\\.") %>%
    stringr::str_replace_all("[-._()?]", " ") %>%
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
#' @export
#'
#' @examples
#' standardise_names(c("Abies alba Mill.", "Quercus suber", "Eucalyptus sp.", "Agave americana var. marginata"))
#'
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


#' Create a lookup table to help fix the taxonomy for a list of Australian plant species
#'
#' This function takes a list of Australian plant species that needs to be reconciled with current taxonomy and generates a lookup table to help fix the taxonomy. The lookup table contains the original species names, the aligned species names, and additional taxonomic information such as taxon IDs and genera.
#'
#'
#' @param species_list A list of Australian plant species that needs to be reconciled with current taxonomy.
#' @param fuzzy_matching A logical value indicating whether fuzzy matching should be used to align the species names. Default is \code{FALSE}.
#' @param version_number The version number of the dataset to use. Default is \code{"0.0.1.9000"}.
#' @param full logical for whether the full lookup table is returned or just the two key columns
#' @return A lookup table containing the original species names, the aligned species names, and additional taxonomic information such as taxon IDs and genera.
#' @export
#' @examples
#' create_taxonomic_update_lookup(c("Eucalyptus regnans", "Acacia melanoxylon",
#' "Banksia integrifolia","Not a species"))
#'
create_taxonomic_update_lookup <-
  function(species_list,
           fuzzy_matching = FALSE,
           version_number = default_version(),
           full = FALSE) {
    tmp <- dataset_access_function(ver = version_number)
    aligned_data <-
      unique(species_list) %>%
      align_taxa(fuzzy_matching = fuzzy_matching, ver = version_number)
    # it'd be nice to carry a column for TRUE/FALSE on the fuzzy fix back to the top level
    
    aligned_species_list_tmp <-
      aligned_data$aligned_name %>% update_taxonomy()
    
    aligned_species_list <-
      aligned_data %>% dplyr::select(original_name, aligned_name) %>%
      dplyr::left_join(aligned_species_list_tmp,
                       by = c("aligned_name"),
                       multiple = "first") %>%
      dplyr::filter(!is.na(taxonIDClean)) %>%
      dplyr::mutate(genus = stringr::word(canonicalName, 1, 1)) %>%
      dplyr::rename(canonical_name = canonicalName) 
    
    if (full == TRUE) {
      return(aligned_species_list)
    }
    if (full == FALSE) {
      return(dplyr::select(aligned_species_list, original_name, canonical_name))
    }
  }
