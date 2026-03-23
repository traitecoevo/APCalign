# Package-internal cache environment.
# This is NOT a global variable — it lives in the package namespace only.
# CRAN policy prohibits modifying .GlobalEnv; using a package-private
# environment is the standard CRAN-compliant pattern for session-level caching.
.pkg_cache <- new.env(parent = emptyenv())

#' @title Load taxonomic reference lists, APC & APNI
#' 
#' @description
#' This function loads two taxonomic datasets for Australia's vascular plants,
#' the APC and APNI. It creates several data frames by filtering and selecting
#' data from the loaded lists.
#' 
#' @details
#' - It accesses taxonomic data from a dataset using the provided version number
#' or the default version.
#' - The output is several dataframes that include subsets of the APC/APNI based
#' on taxon rank and taxonomic status.
#' - Results are cached in memory for the R session so that repeated calls with
#'   the same `version` and `stable_or_current_data` arguments return immediately
#'   without re-downloading or re-processing the data. Use
#'   [clear_cached_resources()] to force a reload.
#' - `"current"` data is not cached because it may change between calls.
#'
#' @param stable_or_current_data Type of dataset to access.
#' The default is "stable", which loads the dataset from a github archived file.
#' If set to "current", the dataset will be loaded from a URL which is the
#' cutting edge version, but this may change at any time without notice.
#' @param version The version number of the dataset to use.
#' Defaults to the default version.
#'
#' @param quiet A logical indicating whether to print status of loading to screen.
#'  Defaults to FALSE.
#'
#' @return A list of taxonomic resource data frames.
#' @export
#'
#' @examples
#' \donttest{
#' load_taxonomic_resources(stable_or_current_data="stable", 
#' version="2024-10-11")}
#'

load_taxonomic_resources <-
  function(stable_or_current_data = "stable",
           version = default_version(),
           quiet = FALSE) {

    # Session-level cache for stable data only.
    # "current" data is explicitly cutting-edge and may change between calls.
    # .pkg_cache is a package-private environment — NOT .GlobalEnv — so this
    # is CRAN-compliant.
    use_cache <- stable_or_current_data == "stable" && !is.null(version)
    cache_key <- if (use_cache) paste0("stable_", version) else NULL

    if (use_cache && !is.null(.pkg_cache[[cache_key]])) {
      if (!quiet) message("Using cached taxonomic resources.")
      return(.pkg_cache[[cache_key]])
    }

    if(is.null(version)){
      message("No internet connection, please retry with stable connection or specify a local version of the data")
      return(invisible(NULL))
    }
    
    taxonomic_resources <- dataset_access_function(
      version = version,
      path = tools::R_user_dir("APCalign"),
      type = stable_or_current_data
    )
    
    
    total_steps <- 3  # Define how many steps you expect in the function
    pb <- utils::txtProgressBar(min = 0, max = total_steps, style = 2)
    if(!quiet){
      message("Loading resources into memory...")
      utils::setTxtProgressBar(pb, 1)  
    }
    if(is.null(taxonomic_resources)) {
      return(NULL)
    }
    
    
    # Give list names
    names(taxonomic_resources) <- c("APC", "APNI")
    
    ## todo :review this, why zzz
    ### Note: Use `zzzz zzzz` because the fuzzy matching algorithm can't handles NA's
    zzz <- "zzzz zzzz"
    
    column_rename <- 
      c(
        taxon_ID = "taxonID",
        taxon_rank = "taxonRank",
        name_type = "nameType",
        taxonomic_status = "taxonomicStatus",
        pro_parte = "proParte",
        scientific_name = "scientificName",
        scientific_name_ID = "scientificNameID",
        accepted_name_usage_ID = "acceptedNameUsageID",
        accepted_name_usage = "acceptedNameUsage",
        canonical_name = "canonicalName",
        scientific_name_authorship = "scientificNameAuthorship",
        taxon_rank_sort_order = "taxonRankSortOrder",
        taxon_remarks = "taxonRemarks",
        taxon_distribution = "taxonDistribution",
        higher_classification = "higherClassification",
        nomenclatural_code = "nomenclaturalCode",
        dataset_name = "datasetName",
        name_element = "nameElement"
      )

    taxonomic_resources$APC <- taxonomic_resources$APC %>%
      dplyr::rename(dplyr::any_of(column_rename)) %>%
      dplyr::mutate(
        genus = extract_genus(canonical_name),
        taxon_rank = standardise_taxon_rank(taxon_rank)
      )
    
    taxonomic_resources$APNI <- taxonomic_resources$APNI %>%
      dplyr::rename(dplyr::any_of(column_rename)) %>%
      dplyr::mutate(
        genus = extract_genus(canonical_name),
        taxon_rank = standardise_taxon_rank(taxon_rank)
      )
    
    APC_tmp <-
      taxonomic_resources$APC %>%
      dplyr::arrange(taxonomic_status) %>%
      dplyr::filter(taxon_rank %in% c("subspecies", "species", "form", "variety")) %>%
      dplyr::filter(!stringr::str_detect(canonical_name, "[:space:]sp\\.$")) %>%
      dplyr::select(
        canonical_name,
        scientific_name,
        taxonomic_status,
        taxon_ID,
        scientific_name_ID,
        accepted_name_usage_ID,
        name_type,
        taxon_rank,
        family,
        genus
      ) %>%
      dplyr::mutate(
        ## strip_names removes punctuation and filler words associated with
        ## infraspecific taxa (subsp, var, f, ser)
        stripped_canonical = strip_names(canonical_name),
        ## strip_names_extra removes extra filler words associated with 
        ## species name cases (x, sp)
        ## strip_names_extra is essential for the matches involving 2 or 3 words,
        ## since you want those words to not count filler words
        stripped_canonical2 = strip_names_extra(stripped_canonical),
        stripped_scientific = strip_names(scientific_name),
        binomial = ifelse(
          taxon_rank == "species",
          word(stripped_canonical2, start = 1, end = 2),
          zzz
        ),
        binomial = ifelse(is.na(binomial), zzz, binomial),
        binomial = base::replace(binomial, duplicated(binomial), zzz),
        genus = extract_genus(stripped_canonical),
        trinomial = word(stripped_canonical2, start = 1, end = 3),
        trinomial = ifelse(is.na(trinomial), zzz, trinomial),
        trinomial = base::replace(trinomial, duplicated(trinomial), zzz),
      ) %>%
      dplyr::distinct()
    
    taxonomic_resources[["APC_accepted"]] <-
      APC_tmp %>%
      dplyr::filter(taxonomic_status == "accepted") %>%
      dplyr::mutate(taxonomic_dataset = "APC")
    
    taxonomic_resources[["APC_synonyms"]] <-
      APC_tmp %>%
      dplyr::filter(taxonomic_status != "accepted") %>%
      dplyr::mutate(taxonomic_dataset = "APC")
    
    
    if(!quiet) utils::setTxtProgressBar(pb, 2) 
    # Repeated from above - bionomial, tronomials etc
    taxonomic_resources[["APNI_names"]] <-
      taxonomic_resources$APNI %>%
      dplyr::filter(name_element != "sp.") %>%
      dplyr::filter(!canonical_name %in% APC_tmp$canonical_name) %>%
      dplyr::select(canonical_name,
                    scientific_name,
                    scientific_name_ID,
                    name_type,
                    taxon_rank) %>%
      dplyr::filter(taxon_rank %in% c("series", "subspecies", "species", "form", "variety")) %>%
      dplyr::mutate(
        taxonomic_status = "unplaced for APC",
        stripped_canonical = strip_names(canonical_name),
        stripped_canonical2 = strip_names_extra(stripped_canonical),
        stripped_scientific = strip_names(scientific_name),
        binomial = ifelse(
          taxon_rank == "species",
          word(stripped_canonical2, start = 1, end = 2),
          "zzzz zzzz"
        ),
        binomial = ifelse(is.na(binomial), "zzzz zzzz", binomial),
        trinomial = word(stripped_canonical2, start = 1, end = 3),
        trinomial = ifelse(is.na(trinomial), "zzzz zzzz", trinomial),
        trinomial = base::replace(trinomial, duplicated(trinomial), "zzzz zzzz"),
        genus = extract_genus(stripped_canonical),
        taxonomic_dataset = "APNI"
      ) %>%
      dplyr::distinct() %>%
      dplyr::arrange(canonical_name)
    
    taxonomic_resources[["genera_accepted"]] <-
      taxonomic_resources$APC %>%
      dplyr::select(
        canonical_name,
        accepted_name_usage,
        accepted_name_usage_ID,
        scientific_name,
        taxonomic_status,
        taxon_ID,
        scientific_name_ID,
        name_type,
        taxon_rank,
        genus
      ) %>%
      dplyr::filter(taxon_rank %in% c("genus"), taxonomic_status == "accepted") %>%
      dplyr::filter(!stringr::str_detect(genus, "aceae$")) %>%
      dplyr::mutate(taxonomic_dataset = "APC")
    
    taxonomic_resources[["genera_synonym"]] <-
      taxonomic_resources$APC %>%
      dplyr::select(
        canonical_name,
        accepted_name_usage,
        accepted_name_usage_ID,
        scientific_name,
        taxonomic_status,
        taxon_ID,
        scientific_name_ID,
        name_type,
        taxon_rank,
        genus
      ) %>%
      dplyr::filter(taxon_rank %in% c("genus")) %>%
      dplyr::filter(!canonical_name %in% taxonomic_resources$genera_accepted$canonical_name) %>%
      dplyr::filter(!stringr::str_detect(genus, "aceae$")) %>%
      dplyr::mutate(taxonomic_dataset = "APC") %>%
      dplyr::distinct(canonical_name, .keep_all = TRUE)
    
    if(!quiet) utils::setTxtProgressBar(pb, 3) 
    taxonomic_resources[["genera_APNI"]] <-
      taxonomic_resources$APNI %>%
      dplyr::select(
        canonical_name,
        scientific_name,
        taxonomic_status,
        scientific_name_ID,
        name_type,
        taxon_rank,
        genus
      ) %>%
      dplyr::filter(taxon_rank %in% c("genus")) %>%
      dplyr::filter(!canonical_name %in% taxonomic_resources$APC$canonical_name) %>%
      dplyr::filter(!stringr::str_detect(genus, "aceae$")) %>%
      dplyr::mutate(taxonomic_dataset = "APNI") %>%
      dplyr::distinct(canonical_name, .keep_all = TRUE)
    
    taxonomic_resources[["genera_all"]] <-
      dplyr::bind_rows(
        taxonomic_resources$genera_accepted,
        taxonomic_resources$genera_synonym,
        taxonomic_resources$genera_APNI
      ) %>%
      dplyr::mutate(
        cleaned_name = word(accepted_name_usage, 1),
        cleaned_name = ifelse(is.na(cleaned_name), canonical_name, cleaned_name)
      ) %>%
      dplyr::distinct(cleaned_name, canonical_name, scientific_name, .keep_all = TRUE)
    
    taxonomic_resources[["family_accepted"]] <-
      taxonomic_resources$APC %>%
      dplyr::filter(taxon_rank %in% c("family"), taxonomic_status == "accepted")

    taxonomic_resources[["family_synonym"]] <-
      taxonomic_resources$APC %>%
      dplyr::select(
        canonical_name,
        accepted_name_usage,
        accepted_name_usage_ID,
        scientific_name,
        taxonomic_status,
        taxon_ID,
        scientific_name_ID,
        name_type,
        taxon_rank,
        genus
      ) %>%
      dplyr::filter(taxon_rank %in% c("family"), taxonomic_status != "accepted") %>%
      dplyr::mutate(taxonomic_dataset = "APC") %>%
      dplyr::distinct(canonical_name, .keep_all = TRUE)
    
    close(pb)
    if(!quiet) message("...done")

    # Store in session cache for future calls
    if (use_cache) {
      .pkg_cache[[cache_key]] <- taxonomic_resources
    }

    return(taxonomic_resources)
  }

#' Clear cached taxonomic resources
#'
#' Removes any taxonomic resources that have been cached in memory during the
#' current R session. After calling this function, the next call to
#' [load_taxonomic_resources()] will re-download and re-process the data.
#'
#' This is useful if you want to force a reload of the resources, for example
#' after updating the package or switching to a different version.
#'
#' @return Invisibly returns `NULL`.
#' @export
#'
#' @examples
#' clear_cached_resources()
clear_cached_resources <- function() {
  rm(list = ls(.pkg_cache), envir = .pkg_cache)
  invisible(NULL)
}

##' Access Australian Plant Census Dataset
##'
##' This function provides access to the Australian Plant Census dataset
##' about various species. The dataset can be loaded from a github for a stable file or
##' from a URL for the most cutting-edge, but not stable version.
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
##'   dataset from a github archived file. If set to "current", the dataset will be
##' loaded from a URL which is the cutting edge version, but this may change at any time
##'  without notice.
##'
##' @examples
##'
##'
##' # Load the a stable version of the dataset
##' dataset_access_function(version="2024-10-11",type = "stable")
##'
##' @noRd
dataset_access_function <-
  function(version = default_version(),
           path = tools::R_user_dir("APCalign"),
           type = "stable") {
    
    # Check if there is internet connection
    ## Dummy variable to allow testing of network
    network <- as.logical(Sys.getenv("NETWORK_UP", unset = TRUE))
    offline <- !curl::has_internet() | !network

    # "current" always requires a live internet connection
    if (offline && type == "current") {
      message("No internet connection, please retry with stable connection (dataset_access_function)")
      return(invisible(NULL))
    }

    if (is.null(version)) {
      message("No internet connection, please retry with stable connection (dataset_access_function)")
      return(invisible(NULL))
    }

    # Download from Github Release (dataset_get handles offline fallback)
    if (type == "stable") {
      return(dataset_get(version, path))
    }
    
    # Download from NSL
    if (type == "current") {
      tryCatch({
        APC <- readr::read_csv(
          "https://biodiversity.org.au/nsl/services/export/taxonCsv",
          col_types =
            readr::cols(
              .default = readr::col_character(),
              proParte = readr::col_logical(),
              taxonRankSortOrder = readr::col_double(),
              created = readr::col_datetime(format = ""),
              modified = readr::col_datetime(format = "")
            )
        )
        
        APNI <-
          readr::read_csv(
            "https://biodiversity.org.au/nsl/services/export/namesCsv",
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
                modified = readr::col_datetime(format = "")
              )
          )
      }, error = function(e) rlang::abort("Taxonomic resources not currently available, try again later")
      )
    }
    
    # Put lists together
    current_list <- list(APC, APNI)
    names(current_list) <- c("APC", "APNI")
    return(current_list)
  }

#' Get the default version for stable data
#'
#' This function returns the default version for stable data, which is used when no
#' version is specified.
#'
#' @return A character string representing the default version for stable data.
#' @examples
#' default_version()
#'
#' @export

default_version <- function() {
  # Check if there is internet connection
  ## Dummy variable to allow testing of network
  network <- as.logical(Sys.getenv("NETWORK_UP", unset = TRUE))

  if (!curl::has_internet() | !network) { # Simulate if network is down
    # Fall back to the most recently downloaded local version, if any
    local_versions <- local_cached_versions()
    if (length(local_versions) > 0) {
      version <- sort(local_versions, decreasing = TRUE)[1]
      message("No internet connection; using most recent locally cached version: ", version)
      return(version)
    }
    message("No internet connection, please retry with stable connection (default_version)")
    return(invisible(NULL))
  } else {
    
    # Get all the releases
    url <-
      paste0(
        "https://api.github.com/repos/",
        "traitecoevo",
        "/",
        "APCalign",
        "/releases"
      )
    
    response <- httr::GET(url)
  
  if(httr::http_error(response) | !network){
    message("API currently down, try again later")
    return(invisible(NULL))
  } else
  release_data <- httr::content(response, "text") |> jsonlite::fromJSON()
  
  # Pull out versions
  versions <- unique(release_data$tag_name)
  
  # Exclude rough semantic versions
  cleaned_versions <- versions[!grepl("^\\d+\\.\\d+\\.\\d+(\\.\\d+)?$", versions)]
  
  # Verify only dates left
  if(! all(grepl("^\\d{4}-\\d{2}-\\d{2}$", cleaned_versions))) 
    rlang::abort("Inconsistent date formats detected in versions!")
    
  # Return latest version
    sort(cleaned_versions, decreasing = TRUE) |> #Sort from most recent to oldest
    dplyr::first() #  and take the first value after sorting
  }
}

# Returns a character vector of version strings (e.g. "2024-10-11") for which
# both APC and APNI parquet files exist in the local cache directory.
#' @noRd
local_cached_versions <- function(path = tools::R_user_dir("APCalign")) {
  if (!dir.exists(path)) return(character(0))
  apc_files <- list.files(path, pattern = "^apc\\d{4}-\\d{2}-\\d{2}\\.parquet$")
  versions <- gsub("^apc|\\.parquet$", "", apc_files)
  # Only return versions where the matching APNI file also exists
  has_apni <- file.exists(file.path(path, paste0("apni", versions, ".parquet")))
  versions[has_apni]
}

#' @noRd
dataset_get <- function(version = default_version(),
                        path = tools::R_user_dir("APCalign")) {

  # Check if there is internet connection
  ## Dummy variable to allow testing of network
  network <- as.logical(Sys.getenv("NETWORK_UP", unset = TRUE))
  offline <- !curl::has_internet() | !network

  if (is.null(version)) {
    message(
      "No internet connection and no locally cached version found (dataset_get)"
    )
    return(invisible(NULL))
  }

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  path_to_apc  <- file.path(path, paste0("apc",  version, ".parquet"))
  path_to_apni <- file.path(path, paste0("apni", version, ".parquet"))

  # If offline but both files are cached locally, read and return them
  if (offline) {
    if (file.exists(path_to_apc) && file.exists(path_to_apni)) {
      message(
        "No internet connection; loading locally cached version: ", version
      )
      APC  <- arrow::read_parquet(path_to_apc)
      APNI <- arrow::read_parquet(path_to_apni)
      current_list <- list(APC, APNI)
      names(current_list) <- c("APC", "APNI")
      return(current_list)
    } else {
      message(
        "No internet connection and no local data for version ", version,
        "; please connect and retry (dataset_get)"
      )
      return(invisible(NULL))
    }
  }

  # Online path — download if not already cached
  #APC
  apc.url <-
    paste0(
      "https://github.com/traitecoevo/APCalign/releases/download/",
      version,
      "/apc.parquet"
    )
  
  # APNI
  apni.url <-
    paste0(
      "https://github.com/traitecoevo/APCalign/releases/download/",
      version,
      "/apni.parquet"
    )
  
  
  download_and_read_parquet <- function(url, path_to_file) {
    tryCatch({
      utils::download.file(url, path_to_file, mode = "wb")
      message("File downloaded successfully.")
      return(arrow::read_parquet(path_to_file))
    }, error = function(e) {
      message(
        "Internet or server may be down; error in downloading or reading the file: ",
        e$message
      )
      return(NULL)
    })
  }

  APC <- if (!file.exists(path_to_apc)) {
    message("Downloading...")
    download_and_read_parquet(apc.url, path_to_apc)
  } else {
    arrow::read_parquet(path_to_apc)
  }

  APNI <- if (!file.exists(path_to_apni)) {
    download_and_read_parquet(apni.url, path_to_apni)
  } else {
    arrow::read_parquet(path_to_apni)
  }

  current_list <- list(APC, APNI)
  names(current_list) <- c("APC", "APNI")
  return(current_list)
}


#' Which versions of taxonomic resources are available?
#'
#' @return tibble of dates when APC/APNI resources were downloaded as a Github Release
#' @export
#'
#' @examples
#' get_versions()
get_versions <- function() {
  # Check if there is internet connection
  ## Dummy variable to allow testing of network
  network <- as.logical(Sys.getenv("NETWORK_UP", unset = TRUE)) 
  
  if (!curl::has_internet() | !network) { # Simulate if network is down
    message("No internet connection, please retry with stable connection (default_version)")
    return(invisible(NULL))
  } else {
    
    # Get all the releases
    url <-
      paste0(
        "https://api.github.com/repos/",
        "traitecoevo",
        "/",
        "APCalign",
        "/releases"
      )
    
    response <- httr::GET(url)
    
    if(httr::http_error(response) | !network){
      message("API currently down, try again later")
      return(invisible(NULL))
    } else
      release_data <- httr::content(response, "text") |> jsonlite::fromJSON()
    
    # Create table
    dplyr::tibble(versions = unique(release_data$tag_name) |> sort(decreasing = TRUE)) |> 
      dplyr::filter(!versions == "2020-05-14") #Excluding first ever release because it is not in parquet format
  }
}
