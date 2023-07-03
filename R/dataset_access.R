
##' Access Australian Plant Census Dataset
##'
##' This function provides access to the Australian Plant Census dataset containing information
##' about various species. The dataset can be loaded from a github for a stable file or from a URL for the most cutting-edge, but not stable version.
##'
##' @param version Version number. The default is NULL, which will load the most recent
##'   version of the dataset on your computer or the most recent version known
##'   to the package if you have never downloaded the data before. With 
##'   `plant_lookup_del`, specifying `version=NULL` will delete \emph{all} data sets.
##' @param path Path to store the data at. If not given, `datastorr` will use `rappdirs`
##'   to find the best place to put persistent application data on your system. You can
##'   delete the persistent data at any time by running `mydata_del(NULL)` (or 
##'   `mydata_del(NULL, path)` if you use a different path).
##' @param type Type of dataset to access. The default is "stable", which loads the 
##'   dataset from a github archived file. If set to "current", the dataset will be loaded from 
##'   a URL which is the cutting edge version, but this may change at any time without notice.
##' @export
##' @examples
##'
##' # Load the current version of the dataset direct from the APC website
##'  \dontrun{dataset_access_function(type = "current")}
##'
##' # Load the a stable version of the dataset 
##' dataset_access_function(version="0.0.2.9000",type = "stable")
##'
dataset_access_function <- function(version=default_version(), path=NULL, 
                                    type="stable") {
  if(type=="stable"){
    return(dataset_get(version, path))
  }
  if(type=="current"){
    APC <- readr::read_csv("https://biodiversity.org.au/nsl/services/export/taxonCsv",
                           col_types = 
                             readr::cols(
                               .default = readr::col_character(),
                               proParte = readr::col_logical(),
                               taxonRankSortOrder = readr::col_double(),
                               created = readr::col_datetime(format = ""),
                               modified = readr::col_datetime(format = "")))
    APNI <- readr::read_csv("https://biodiversity.org.au/nsl/services/export/namesCsv",
                            col_types = 
                              readr::cols(
                                .default = readr::col_character(),
                                autonym = readr::col_logical(),
                                hybrid = readr::col_logical(),
                                cultivar = readr::col_logical(),
                                formula = readr::col_logical(),
                                scientific = readr::col_logical(),
                                nomInval = readr::col_logical(),
                                nomIlleg = readr::col_logical(),
                                namePublishedInYear = readr::col_double(),
                                taxonRankSortOrder = readr::col_double(),
                                created = readr::col_datetime(format = ""),
                                modified = readr::col_datetime(format = "")))
    current_list <- list(APC, APNI)  
    names(current_list) <- c("APC", "APNI")
    return(current_list)
  }
}

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
#' @seealso
#' align_taxa
#'
#'
default_version <- function() {
  "0.0.2.9000"
}


dataset_get <- function(version = default_version(),
                        path = tools::R_user_dir("ausflora")) {
    #APC
    url <- paste0("https://github.com/traitecoevo/ausflora/releases/download/",
                  version,"/apc.parquet")
    apc_hash <- contentid::register(url)
    apc_file <- contentid::resolve(apc_hash, store=TRUE, path=path)
    APC <- arrow::read_parquet(apc_file)
    
    #APNI
    url <- paste0("https://github.com/traitecoevo/ausflora/releases/download/",
                  version,"/apni.parquet")
    apni_hash <- contentid::register(url)
    apni_file <- contentid::resolve(apni_hash,store=TRUE,path=path)
    APNI <- arrow::read_parquet(apni_file)
    
    #combine
    current_list <- list(APC, APNI)
    names(current_list) <- c("APC", "APNI")
    return(current_list)
}


#' Load taxonomic resources
#'
#' Loads taxonomic resources into the global environment. This function accesses taxonomic data from a dataset using the provided version number or the default version. The loaded data contains two lists: APC and APNI, which contain taxonomic information about plant species in Australia. The function creates several data frames by filtering and selecting data from the loaded lists.
#'
#' @param version The version number of the dataset to use. Defaults to the default version.
#' @param type XXXX
#' @param reload A logical indicating whether to reload the dataset from the data source. Defaults to FALSE.
#' @param filetype type of file to download. parquet or csv
#'
#' @return The taxonomic resources data loaded into the global environment.
#' @export
#'
#' @examples
#' load_taxonomic_resources()
#'
#' @importFrom dplyr filter select mutate distinct arrange
#' @importFrom crayon red

load_taxonomic_resources <-
  function(version = default_version(),
           type = "stable",
           reload = FALSE,
           filetype = "parquet") {

    message("Loading resources...", appendLF = FALSE)
    on.exit(message("...done"))

    taxonomic_resources <-
      dataset_access_function(
        version = version,
        path = NULL,
        type = "stable"
      )
    names(taxonomic_resources) <- c("APC", "APNI")

    ## todo :review this, why zzz
    ### Note: Use `zzzz zzzz` because the fuzzy matching algorithm can't handles NA's
    zzz <- "zzzz zzzz"
    
    APC_tmp <-
      taxonomic_resources$APC %>%
      dplyr::filter(taxonRank %in% c("Subspecies", "Species", "Forma", "Varietas")) %>%
      dplyr::select(.data$canonicalName, .data$scientificName, .data$taxonomicStatus, ID = .data$taxonID, .data$nameType, .data$taxonRank) %>%
      dplyr::mutate(
        stripped_canonical = strip_names(canonicalName),
        ## Todo: rename stripped_canonical2, state purpose
        # stripped_2 gets rid of `sp` and `spp` which is helpful for some matches
        stripped_canonical2 = strip_names_2(.data$canonicalName),
        stripped_scientific = strip_names(scientificName),
        binomial = ifelse(taxonRank == "Species", stringr::word(.data$stripped_canonical2, start = 1, end = 2), zzz),
        binomial = ifelse(is.na(binomial), zzz, binomial),
        binomial = base::replace(binomial, duplicated(binomial), zzz),
        genus = stringr::word(.data$stripped_canonical, 1),
        trinomial = stringr::word(.data$stripped_canonical2, start = 1, end = 3),
        trinomial = ifelse(is.na(trinomial), zzz, trinomial),
        trinomial = base::replace(trinomial, duplicated(trinomial), zzz), 
      ) %>%
      dplyr::distinct()

    taxonomic_resources[["APC list (accepted)"]] <-
      APC_tmp %>%
        dplyr::filter(taxonomicStatus == "accepted") %>%
        dplyr::mutate(taxonomic_ref = "APC accepted")
    taxonomic_resources[["APC list (known names)"]] <-
      APC_tmp %>%
        dplyr::filter(taxonomicStatus != "accepted") %>%
        dplyr::mutate(taxonomic_ref = "APC known")


    # Repeated from above - bionomial, tronomials etc
    taxonomic_resources[["APNI names"]] <-
      taxonomic_resources$APNI %>%
      dplyr::filter(.data$nameElement != "sp.") %>%
      dplyr::filter(!.data$canonicalName %in% APC_tmp$canonicalName) %>%
      dplyr::select(.data$canonicalName, .data$scientificName, ID = .data$scientificNameID, .data$nameType, .data$taxonRank) %>%
      dplyr::filter(.data$taxonRank %in% c("Series", "Subspecies", "Species", "Forma", "Varietas")) %>%
      dplyr::mutate(
        taxonomicStatus = "unplaced for APC",
        stripped_canonical = strip_names(.data$canonicalName),
        stripped_canonical2 = strip_names_2(.data$canonicalName),
        stripped_scientific = strip_names(.data$scientificName),
        binomial = ifelse(taxonRank == "Species", stringr::word(.data$stripped_canonical2, start = 1, end = 2), "zzzz zzzz"),
        binomial = ifelse(is.na(binomial), "zzzz zzzz", binomial),
        trinomial = stringr::word(.data$stripped_canonical2, start = 1, end = 3),
        trinomial = ifelse(is.na(trinomial), "zzzz zzzz", trinomial),
        trinomial = base::replace(trinomial, duplicated(trinomial), "zzzz zzzz"),
        genus = stringr::word(.data$stripped_canonical, 1),
        taxonomic_ref = "APNI"
      ) %>%
      dplyr::distinct() %>%
      dplyr::arrange(.data$canonicalName)

      ## Todo: do we need all this, or only genera_all

      taxonomic_resources[["genera_accepted"]] <-
        taxonomic_resources$APC %>%
        dplyr::select(.data$canonicalName, .data$acceptedNameUsage, .data$scientificName, .data$taxonomicStatus, ID = .data$taxonID, .data$nameType, .data$taxonRank) %>%
        dplyr::filter(.data$taxonRank %in% c("Genus"), .data$taxonomicStatus == "accepted") %>%
        dplyr::mutate(taxonomic_ref = "APC accepted")
      
      taxonomic_resources[["genera_known"]] <-
        taxonomic_resources$APC %>%
        dplyr::select(.data$canonicalName, .data$acceptedNameUsage, .data$scientificName, .data$taxonomicStatus, ID = .data$taxonID, .data$nameType, .data$taxonRank) %>%
        dplyr::filter(.data$taxonRank %in% c("Genus")) %>%
        dplyr::filter(!.data$canonicalName %in% taxonomic_resources$genera_accepted$canonicalName) %>%
        dplyr::mutate(taxonomic_ref = "APC known")
      
      taxonomic_resources[["genera_APNI"]] <-
        taxonomic_resources$APNI %>%
        dplyr::select(.data$canonicalName, .data$taxonomicStatus, .data$nameType, .data$taxonRank, .data$scientificName) %>%
        dplyr::filter(.data$taxonRank %in% c("Genus")) %>%
        dplyr::filter(!.data$canonicalName %in% taxonomic_resources$APC$canonicalName) %>%
        dplyr::mutate(taxonomic_ref = "APNI")
      
      taxonomic_resources[["genera_all"]] <- 
        dplyr::bind_rows(taxonomic_resources$genera_accepted, taxonomic_resources$genera_known, taxonomic_resources$genera_APNI) %>%
        dplyr::mutate(
          cleaned_name = stringr::word(.data$acceptedNameUsage, 1),
          cleaned_name = ifelse(is.na(cleaned_name), canonicalName, cleaned_name)
        ) %>%
        dplyr::distinct(.data$cleaned_name, .data$canonicalName, .data$scientificName, .keep_all = TRUE)
      
      taxonomic_resources[["family_accepted"]] <- 
        taxonomic_resources$APC %>% 
        dplyr::filter(.data$taxonRank %in% c("Familia"), .data$taxonomicStatus == "accepted")

    return(taxonomic_resources)
  }
