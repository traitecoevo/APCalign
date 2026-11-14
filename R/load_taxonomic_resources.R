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
#' - Stable versions are downloaded once and kept in
#'   `tools::R_user_dir("APCalign")`, so later sessions, and sessions without
#'   an internet connection, can load them without downloading again. Each
#'   version takes about 20 MB; delete the files there to remove versions you
#'   no longer need.
#'
#' @param stable_or_current_data Type of dataset to access.
#' The default is "stable", which loads the dataset from a github archived file.
#' If set to "current", the dataset will be loaded from a URL which is the
#' cutting edge version, but this may change at any time without notice.
#' Loading "current" data requires the 'readr' package.
#' @param version The version number of the dataset to use (see
#'   [get_versions()]). Defaults to [default_version()], the most recent
#'   release. Ignored for "current" data.
#'
#' @param quiet A logical indicating whether to print status of loading to screen.
#'  Defaults to FALSE.
#'
#' @return A list of taxonomic resource data frames. Its `"version"` attribute
#'   records which data were loaded: the `version` for stable data, or
#'   `"current, downloaded <date>"` for current data. [align_taxa()] writes it
#'   into each `aligned_reason`.
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

    if (stable_or_current_data == "stable" && is.null(version)) {
      message("No version of the taxonomic resources is available (see above); please retry later, or specify a version already downloaded")
      return(invisible(NULL))
    }

    taxonomic_resources <- dataset_access_function(
      version = version,
      path = tools::R_user_dir("APCalign"),
      type = stable_or_current_data
    )

    if(is.null(taxonomic_resources)) {
      return(NULL)
    }

    if (!quiet) {
      message("Loading resources into memory...")
      # on stderr, like the messages, so it is not mixed into printed output
      pb <- utils::txtProgressBar(min = 0, max = 3, style = 2, file = stderr())
      utils::setTxtProgressBar(pb, 1)
    }

    # Give list names
    names(taxonomic_resources) <- c("APC", "APNI")

    ## Placeholder for a `binomial` or `trinomial` that must never match: a
    ## name that has no such form (too few words, or a binomial of a
    ## non-species), or (except for APNI binomials) a duplicate of one already
    ## listed, so the lookup by first two or three words is unambiguous. A string rather than NA,
    ## because the fuzzy matching cannot handle NA.
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
        genus = extract_genus_clean(canonical_name),
        taxon_rank = standardise_taxon_rank(taxon_rank)
      )

    taxonomic_resources$APNI <- taxonomic_resources$APNI %>%
      dplyr::rename(dplyr::any_of(column_rename)) %>%
      dplyr::mutate(
        genus = extract_genus_clean(canonical_name),
        taxon_rank = standardise_taxon_rank(taxon_rank)
      )


    APC_tmp <-
      taxonomic_resources$APC %>%
      dplyr::filter(taxon_rank %in% c("subspecies", "species", "form", "variety")) %>%
      dplyr::filter(!re_detect(canonical_name, "[\\s\\p{Z}\\x85]sp\\.$")) %>%
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
      dplyr::arrange(relevel_taxonomic_status_preferred_order(taxonomic_status)) %>%
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
        genus = extract_genus_clean(stripped_canonical),
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
    # The same derived columns as for the APC names above
    taxonomic_resources[["APNI_names"]] <-
      taxonomic_resources$APNI %>%
      dplyr::filter(name_element != "sp.") %>%
      dplyr::filter(taxon_rank %in% c("series", "subspecies", "species", "form", "variety")) %>%
      dplyr::filter(!canonical_name %in% APC_tmp$canonical_name) %>%
      dplyr::select(canonical_name,
                    scientific_name,
                    scientific_name_ID,
                    name_type,
                    taxon_rank) %>%
      dplyr::mutate(
        taxonomic_status = "unplaced for APC",
        stripped_canonical = strip_names(canonical_name),
        stripped_canonical2 = strip_names_extra(stripped_canonical),
        stripped_scientific = strip_names(scientific_name),
        binomial = ifelse(
          taxon_rank == "species",
          word(stripped_canonical2, start = 1, end = 2),
          zzz
        ),
        binomial = ifelse(is.na(binomial), zzz, binomial),
        trinomial = word(stripped_canonical2, start = 1, end = 3),
        trinomial = ifelse(is.na(trinomial), zzz, trinomial),
        trinomial = base::replace(trinomial, duplicated(trinomial), zzz),
        genus = extract_genus_clean(stripped_canonical),
        taxonomic_dataset = "APNI"
      ) %>%
      dplyr::distinct() %>%
      dplyr::arrange(canonical_name)


    apc_genera <-
      taxonomic_resources$APC %>%
      dplyr::filter(taxon_rank == "genus") %>%
      dplyr::filter(!re_detect(genus, "aceae$")) %>%
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
      )

    taxonomic_resources[["genera_accepted"]] <-
      apc_genera %>%
      dplyr::filter(taxonomic_status == "accepted") %>%
      dplyr::mutate(taxonomic_dataset = "APC")

    taxonomic_resources[["genera_synonym"]] <-
      apc_genera %>%
      dplyr::filter(!canonical_name %in% taxonomic_resources$genera_accepted$canonical_name) %>%
      dplyr::arrange(relevel_taxonomic_status_preferred_order(taxonomic_status)) %>%
      dplyr::mutate(taxonomic_dataset = "APC") %>%
      dplyr::distinct(canonical_name, .keep_all = TRUE)


    if(!quiet) utils::setTxtProgressBar(pb, 3)
    # Family names, accepted or not, which a "genus" in APNI must not be: APNI
    # has a genus-rank record "Leguminosae (=RFK/2760)", which made the family
    # name Leguminosae align as an APNI genus.
    apc_family_names <-
      taxonomic_resources$APC$canonical_name[taxonomic_resources$APC$taxon_rank %in% "family"]

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
      dplyr::filter(!re_detect(genus, "aceae$")) %>%
      dplyr::filter(!genus %in% apc_family_names) %>%
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

    if (!quiet) {
      close(pb)
      message("...done")
    }

    # Record which APC/APNI data these tables were built from. `match_taxa()`
    # stamps it into every `aligned_reason`, in place of the run date (#281).
    attr(taxonomic_resources, "version") <-
      if (stable_or_current_data == "stable") {
        version
      } else {
        paste0("current, downloaded ", Sys.Date())
      }

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

##' Access the APC and APNI tables
##'
##' Returns the raw APC and APNI tables, as a list of two data frames. "stable"
##' data are a dated GitHub release, downloaded once and kept in `path`;
##' "current" data are read from the live NSL export, which may change at any
##' time without notice.
##'
##' @param version The dated release to load, for "stable" data.
##' @param path Directory where downloaded releases are kept.
##' @param type "stable" or "current".
##'
##' @examples
##' dataset_access_function(version = "2024-10-11", type = "stable")
##'
##' @noRd
dataset_access_function <-
  function(version = default_version(),
           path = tools::R_user_dir("APCalign"),
           type = "stable") {

    # "current" always requires a live internet connection
    if (type == "current" && is_offline()) {
      message("No internet connection, please retry with stable connection (dataset_access_function)")
      return(invisible(NULL))
    }

    # Download from Github Release (dataset_get handles offline fallback)
    if (type == "stable") {
      if (is.null(version)) {
        message("No version of the taxonomic resources available, please retry with stable connection (dataset_access_function)")
        return(invisible(NULL))
      }
      return(dataset_get(version, path))
    }

    # Download from NSL
    if (type == "current") {
      # readr is only in Suggests; check before the tryCatch below, which would
      # otherwise report a missing package as "resources not available".
      rlang::check_installed("readr", reason = "to download the current APC/APNI CSV exports.")
      return(tryCatch(
        list(APC = read_nsl_export("APC"), APNI = read_nsl_export("APNI")),
        error = function(e) rlang::abort("Taxonomic resources not currently available, try again later")
      ))
    }

    rlang::abort(paste0("`type` must be \"stable\" or \"current\", not \"", type, "\"."))
  }

# Read one of the live NSL CSV exports (the "current" data). Shared with
# download_taxonomic_resources_for_release(), which snapshots the same exports
# as a release, so both read them with the same column types.
#' @noRd
read_nsl_export <- function(dataset = c("APC", "APNI")) {
  dataset <- match.arg(dataset)
  rlang::check_installed("readr", reason = "to read the APC/APNI CSV exports.")

  timestamps <- list(
    created = readr::col_datetime(format = ""),
    modified = readr::col_datetime(format = "")
  )
  if (dataset == "APC") {
    url <- "https://biodiversity.org.au/nsl/services/export/taxonCsv"
    types <- c(list(
      .default = readr::col_character(),
      proParte = readr::col_logical(),
      taxonRankSortOrder = readr::col_double()
    ), timestamps)
  } else {
    url <- "https://biodiversity.org.au/nsl/services/export/namesCsv"
    types <- c(list(
      .default = readr::col_character(),
      autonym = readr::col_logical(),
      hybrid = readr::col_logical(),
      cultivar = readr::col_logical(),
      formula = readr::col_logical(),
      scientific = readr::col_logical(),
      nomInval = readr::col_logical(),
      nomIlleg = readr::col_logical(),
      namePublishedInYear = readr::col_double(),
      taxonRankSortOrder = readr::col_double()
    ), timestamps)
  }

  readr::read_csv(url, col_types = do.call(readr::cols, types))
}

#' Get the default version for stable data
#'
#' This function returns the default version for stable data, which is used when no
#' version is specified: the most recent dated release of the taxonomic
#' resources on GitHub.
#'
#' @details
#' The answer is looked up once per R session and then remembered (until
#' [clear_cached_resources()]). If GitHub cannot be reached -- no internet
#' connection, or the GitHub API is down or rate-limited (it allows 60
#' unauthenticated requests an hour) -- the most recent version already
#' downloaded to this computer is used instead, with a message. With neither,
#' it returns `NULL` with a message.
#'
#' @return A character string representing the default version for stable data.
#' @examples
#' \donttest{default_version()}
#'
#' @export

default_version <- function() {
  if (!is_offline()) {
    if (!is.null(.pkg_cache$default_version)) return(.pkg_cache$default_version)

    tags <- github_release_tags()
    if (!is.null(tags)) {
      # Data releases are tagged with their date; skip any other tags
      dates <- tags[grepl("^\\d{4}-\\d{2}-\\d{2}$", tags)]
      if (length(dates) > 0) {
        version <- sort(dates, decreasing = TRUE)[1]
        .pkg_cache$default_version <- version
        return(version)
      }
    }
    reason <- "Could not get the list of versions from GitHub"
  } else {
    reason <- "No internet connection"
  }

  # Fall back to the most recently downloaded local version, if any
  local_versions <- local_cached_versions()
  if (length(local_versions) > 0) {
    version <- sort(local_versions, decreasing = TRUE)[1]
    message(reason, "; using most recent locally cached version: ", version)
    return(version)
  }
  message(reason, " and no locally cached version found; please retry later (default_version)")
  invisible(NULL)
}

# Is the network unavailable? The `NETWORK_UP` environment variable lets tests
# simulate being offline.
#' @noRd
is_offline <- function() {
  network <- as.logical(Sys.getenv("NETWORK_UP", unset = TRUE))
  !isTRUE(network) || !curl::has_internet()
}

# Tag names of the releases of traitecoevo/APCalign that hold the parquet
# files `dataset_get()` downloads, or NULL (with a message) if the GitHub API
# can't be reached. The earliest releases hold only CSV files, so can't be
# loaded. Shared by default_version() and get_versions().
#' @noRd
github_release_tags <- function() {
  url <- "https://api.github.com/repos/traitecoevo/APCalign/releases?per_page=100"

  response <- tryCatch(
    httr::GET(url, httr::timeout(20)),
    error = function(e) {
      message("Could not reach the GitHub API: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(response)) return(NULL)

  if (httr::http_error(response)) {
    remaining <- httr::headers(response)[["x-ratelimit-remaining"]]
    if (identical(remaining, "0")) {
      message("GitHub API rate limit reached (60 requests an hour without authentication); try again later")
    } else {
      message("GitHub API currently unavailable (HTTP ", httr::status_code(response), "); try again later")
    }
    return(NULL)
  }

  release_data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  has_parquet <- vapply(release_data$assets,
                        function(a) "apc.parquet" %in% a$name, logical(1))
  unique(release_data$tag_name[has_parquet])
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

  if (is.null(version)) {
    message(
      "No internet connection and no locally cached version found (dataset_get)"
    )
    return(invisible(NULL))
  }

  path_to_apc  <- file.path(path, paste0("apc",  version, ".parquet"))
  path_to_apni <- file.path(path, paste0("apni", version, ".parquet"))
  have_local <- file.exists(path_to_apc) && file.exists(path_to_apni)

  # Offline, the files can only come from the local cache
  if (is_offline()) {
    if (!have_local) {
      message(
        "No internet connection and no local data for version ", version,
        "; please connect and retry (dataset_get)"
      )
      return(invisible(NULL))
    }
    message("No internet connection; loading locally cached version: ", version)
  }

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  release_url <- paste0(
    "https://github.com/traitecoevo/APCalign/releases/download/", version, "/"
  )
  APC  <- get_release_file(paste0(release_url, "apc.parquet"),  path_to_apc)
  APNI <- get_release_file(paste0(release_url, "apni.parquet"), path_to_apni)

  if (is.null(APC) || is.null(APNI)) return(invisible(NULL))
  list(APC = APC, APNI = APNI)
}

# Read a release file from the local cache, downloading it first if it isn't
# there. The download goes to a temporary file that is only moved into the
# cache once it has been read successfully, so a failed or partial download
# never leaves a file behind that would be read as the cached copy next time.
# Returns NULL, with a message, if the file can't be downloaded or read.
#' @noRd
get_release_file <- function(url, path_to_file) {
  if (file.exists(path_to_file)) {
    return(read_parquet_tbl(path_to_file))
  }

  message("Downloading ", basename(url), "...")
  tmp <- tempfile(fileext = ".parquet")
  on.exit(unlink(tmp), add = TRUE)
  tryCatch({
    utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
    data <- read_parquet_tbl(tmp)
    file.copy(tmp, path_to_file, overwrite = TRUE)
    message("File downloaded successfully.")
    data
  }, error = function(e) {
    message(
      "Internet or server may be down; error in downloading or reading the file: ",
      conditionMessage(e)
    )
    NULL
  })
}

# nanoparquet returns a data frame of class c("tbl", "data.frame"), not a
# tibble. Convert so that downstream code keeps tibble semantics (e.g.
# `df[, "col"]` stays a data frame rather than dropping to a vector).
#' @noRd
read_parquet_tbl <- function(path) {
  dplyr::as_tibble(nanoparquet::read_parquet(path))
}


#' Which versions of taxonomic resources are available?
#'
#' @return tibble of dates when APC/APNI resources were downloaded as a Github Release
#' @export
#'
#' @examples
#' \donttest{get_versions()}
get_versions <- function() {
  if (is_offline()) {
    message("No internet connection, please retry with stable connection (get_versions)")
    return(invisible(NULL))
  }

  tags <- github_release_tags()
  if (is.null(tags)) return(invisible(NULL))

  dplyr::tibble(versions = sort(tags, decreasing = TRUE))
}
