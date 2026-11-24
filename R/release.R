#' Download taxonomic resources for a GitHub release
#'
#' Snapshots the current APC and APNI exports as `apc.parquet` and
#' `apni.parquet`, the assets of a dated release that
#' `load_taxonomic_resources()` downloads. Run by the `apc-release` GitHub
#' Actions workflow (`.github/workflows/apc-release.yaml`), which checks the
#' files with `check_release_resources()` and `check_release_alignment()`
#' before releasing them.
#'
#' @param path directory to save the files in, to upload from
#' @return Invisibly, a summary of the files written: see `release_summary()`.
#' @keywords internal
#' @noRd
download_taxonomic_resources_for_release <- function(path = "ignore/") {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)

  APC <- read_nsl_export("APC")
  nanoparquet::write_parquet(APC, file.path(path, "apc.parquet"))

  # Exclude names that are in APC from APNI
  APNI_cleaned <- read_nsl_export("APNI") |>
    dplyr::filter(!canonicalName %in% APC$canonicalName)
  nanoparquet::write_parquet(APNI_cleaned, file.path(path, "apni.parquet"))

  invisible(release_summary(list(APC = APC, APNI = APNI_cleaned)))
}

# Read the `apc.parquet` and `apni.parquet` of a release from a directory.
#' @noRd
read_release_files <- function(path) {
  list(
    APC = read_parquet_tbl(file.path(path, "apc.parquet")),
    APNI = read_parquet_tbl(file.path(path, "apni.parquet"))
  )
}

# Row counts and the most recent `modified` time (as UTC text, so it compares
# and prints the same everywhere) of a release's APC and APNI tables. A release
# with the same summary as the previous one is taken to be unchanged.
#' @noRd
release_summary <- function(resources) {
  modified <- c(resources$APC$modified, resources$APNI$modified)
  latest <- if (all(is.na(modified))) {
    NA_character_
  } else {
    format(max(modified, na.rm = TRUE), "%Y-%m-%d %H:%M:%S", tz = "UTC")
  }
  list(
    apc_rows = nrow(resources$APC),
    apni_rows = nrow(resources$APNI),
    latest_modified = latest
  )
}

# Columns that must be filled in for the package to work.
release_key_columns <- list(
  APC = c("taxonID", "canonicalName", "scientificName", "taxonomicStatus",
          "taxonRank", "scientificNameID", "acceptedNameUsageID"),
  APNI = c("canonicalName", "scientificName", "taxonomicStatus", "taxonRank",
           "scientificNameID")
)

# Compare a new set of release files with the previous release, to catch a
# broken or partial NSL export before it reaches every user (#315). Both are
# lists of `APC` and `APNI` tables, as read by read_release_files().
#
# Returns a list with `status`, `problems` (a character vector explaining the
# status) and the new release_summary(). `status` is the first of these that
# applies:
# - "fail": don't release. A table is empty, its columns (names or types)
#   differ from the previous release, or a key column is entirely empty.
# - "unchanged": the row counts and latest `modified` time match the previous
#   release, so there is nothing new to release.
# - "review": release as a draft, for someone to check and publish by hand.
#   APC lost more than `max_apc_drop` of its rows or APNI more than
#   `max_apni_drop`, or there are more missing values in a key column, or more
#   duplicated APC taxonIDs, than in the previous release. Missing values and
#   duplicates count only when they increase, so a problem already in the
#   published release isn't flagged again by every later run.
# - "pass": release.
#
# APNI here holds only the names not in APC, so it shrinks a little as names
# move into APC: by 1.4% to 5.3% between the releases of 2024-10-11 and
# 2026-11-15, hence its looser limit.
#' @noRd
check_release_resources <- function(new, previous, max_apc_drop = 0.02,
                                    max_apni_drop = 0.10) {
  failures <- character(0)
  reviews <- character(0)
  max_drop <- c(APC = max_apc_drop, APNI = max_apni_drop)

  for (table in c("APC", "APNI")) {
    new_table <- new[[table]]
    previous_table <- previous[[table]]

    if (nrow(new_table) == 0) {
      failures <- c(failures, paste0(table, " has no rows."))
      next
    }

    new_types <- vapply(new_table, function(x) class(x)[1], character(1))
    previous_types <- vapply(previous_table, function(x) class(x)[1], character(1))
    added <- setdiff(names(new_types), names(previous_types))
    removed <- setdiff(names(previous_types), names(new_types))
    if (length(added) > 0) {
      failures <- c(failures, paste0(table, " has new columns: ", toString(added), "."))
    }
    if (length(removed) > 0) {
      failures <- c(failures, paste0(table, " is missing columns: ", toString(removed), "."))
    }
    shared <- intersect(names(new_types), names(previous_types))
    retyped <- shared[new_types[shared] != previous_types[shared]]
    for (column in retyped) {
      failures <- c(failures, paste0(
        table, " column ", column, " changed type from ",
        previous_types[[column]], " to ", new_types[[column]], "."
      ))
    }

    for (column in intersect(release_key_columns[[table]], names(new_table))) {
      missing <- count_missing(new_table[[column]])
      previous_missing <- count_missing(previous_table[[column]])
      if (missing == nrow(new_table)) {
        failures <- c(failures, paste0(table, " column ", column, " is empty."))
      } else if (missing > previous_missing) {
        reviews <- c(reviews, paste0(
          table, " column ", column, " has ", format_count(missing),
          " missing values (previous release: ", format_count(previous_missing), ")."
        ))
      }
    }

    # A partial download shows up as lost rows
    drop <- 1 - nrow(new_table) / nrow(previous_table)
    if (drop > max_drop[[table]]) {
      reviews <- c(reviews, sprintf(
        "%s has %s rows, %.1f%% fewer than the previous release (%s); more than the %.0f%% allowed.",
        table, format_count(nrow(new_table)), 100 * drop,
        format_count(nrow(previous_table)), 100 * max_drop[[table]]
      ))
    }
  }

  # A repeated download shows up as duplicated rows
  if (nrow(new$APC) > 0 && "taxonID" %in% names(new$APC)) {
    duplicated_ids <- sum(duplicated(new$APC$taxonID))
    previous_duplicated <- sum(duplicated(previous$APC$taxonID))
    if (duplicated_ids > previous_duplicated) {
      reviews <- c(reviews, paste0(
        "APC has ", format_count(duplicated_ids), " duplicated taxonID values",
        " (previous release: ", format_count(previous_duplicated), ")."
      ))
    }
  }

  summary <- release_summary(new)
  previous_summary <- release_summary(previous)
  unchanged <- identical(summary, previous_summary)

  status <- if (length(failures) > 0) {
    "fail"
  } else if (unchanged) {
    "unchanged"
  } else if (length(reviews) > 0) {
    "review"
  } else {
    "pass"
  }
  problems <- switch(status,
    fail = c(failures, reviews),
    unchanged = paste0(
      "Same row counts and latest modified time (", summary$latest_modified,
      ") as the previous release."
    ),
    review = reviews,
    pass = character(0)
  )

  list(status = status, problems = problems, summary = summary)
}

# Names that have aligned the same way in every release since the benchmark
# version (2024-10-11). A new release that aligns one differently needs review:
# it may be broken, or APC may have changed that name's taxonomy.
# The last two are only in APNI (so they have no accepted name), and check the
# APNI table too.
release_smoke_test_names <- data.frame(
  original_name = c(
    "Eucalyptus regnans", "Eucalyptus camaldulensis", "Acacia aneura",
    "Acacia dealbata", "Banksia integrifolia", "Grevillea robusta",
    "Triodia pungens",
    "Eucalyptus regnas", # misspelt, so it needs a fuzzy match
    "Hakea paludosa", "Viola linifolia"
  ),
  aligned_name = c(
    "Eucalyptus regnans", "Eucalyptus camaldulensis", "Acacia aneura",
    "Acacia dealbata", "Banksia integrifolia", "Grevillea robusta",
    "Triodia pungens", "Eucalyptus regnans",
    "Hakea paludosa", "Viola linifolia"
  ),
  accepted_name = c(
    "Eucalyptus regnans", "Eucalyptus camaldulensis", "Acacia aneura",
    "Acacia dealbata", "Banksia integrifolia", "Grevillea robusta",
    "Triodia pungens", "Eucalyptus regnans",
    NA, NA
  ),
  taxonomic_status = c(rep("accepted", 8), rep("unplaced", 2))
)

# Load the release files in `path` through load_taxonomic_resources(), as a
# user would, and check the smoke-test names above align and update as
# expected. Returns a list of problems (each a character vector, empty if
# none): `fail`, the files couldn't be loaded or the names couldn't be
# aligned at all, so they're broken; and `review`, names that align
# differently, which may be a real taxonomic change.
#' @noRd
check_release_alignment <- function(path, version = "release-candidate") {
  # load_taxonomic_resources() reads a version's files from
  # tools::R_user_dir("APCalign") when they are there, so point that at a
  # temporary directory holding these files under the given version name.
  # The session cache would return tables already loaded under this version
  # name instead of reading these files, so clear it before and after.
  cache_key <- paste0("stable_", version)
  clear_cache_key <- function() {
    if (exists(cache_key, envir = .pkg_cache, inherits = FALSE)) {
      rm(list = cache_key, envir = .pkg_cache)
    }
  }
  clear_cache_key()

  user_dir <- tempfile("apcalign-release-")
  old <- Sys.getenv("R_USER_DATA_DIR", unset = NA)
  Sys.setenv(R_USER_DATA_DIR = user_dir)
  on.exit({
    if (is.na(old)) Sys.unsetenv("R_USER_DATA_DIR") else Sys.setenv(R_USER_DATA_DIR = old)
    unlink(user_dir, recursive = TRUE)
    clear_cache_key()
  }, add = TRUE)

  cache_dir <- tools::R_user_dir("APCalign")
  dir.create(cache_dir, recursive = TRUE)
  copied <- file.copy(
    file.path(path, c("apc.parquet", "apni.parquet")),
    file.path(cache_dir, paste0(c("apc", "apni"), version, ".parquet"))
  )
  if (!all(copied)) {
    return(alignment_problems(fail = paste0(
      "Could not find the release files apc.parquet and apni.parquet in ", path, "."
    )))
  }

  result <- tryCatch({
    resources <- load_taxonomic_resources(version = version, quiet = TRUE)
    create_taxonomic_update_lookup(
      release_smoke_test_names$original_name,
      resources = resources,
      quiet = TRUE
    )
  }, error = function(e) e)

  if (inherits(result, "error")) {
    return(alignment_problems(fail = paste0("Aligning names failed: ", conditionMessage(result))))
  }

  # create_taxonomic_update_lookup() returns NULL, with a message, when it
  # couldn't load any resources; that must not count as a pass.
  if (!is.data.frame(result) || nrow(result) == 0) {
    return(alignment_problems(
      fail = "Aligning names failed: no results, so the release files could not be loaded."
    ))
  }

  # e.g. "aligned to Hakea paludosa, accepted name none, status unplaced"
  describe <- function(x) {
    sprintf("aligned to %s, accepted name %s, status %s",
            ifelse(is.na(x$aligned_name), "none", x$aligned_name),
            ifelse(is.na(x$accepted_name), "none", x$accepted_name),
            ifelse(is.na(x$taxonomic_status), "none", x$taxonomic_status))
  }
  expected <- release_smoke_test_names
  wanted <- describe(expected)
  found <- describe(result[match(expected$original_name, result$original_name), ])
  wrong <- found != wanted
  alignment_problems(review = sprintf(
    "%s: expected %s; got %s.",
    expected$original_name[wrong], wanted[wrong], found[wrong]
  ))
}

#' @noRd
alignment_problems <- function(fail = character(0), review = character(0)) {
  list(fail = fail, review = review)
}

# Number of missing (NA or empty) values in a column; 0 for a column that isn't
# there.
#' @noRd
count_missing <- function(values) {
  if (is.null(values)) return(0L)
  sum(is.na(values) | values == "")
}

# 114021 -> "114,021"
#' @noRd
format_count <- function(x) {
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
}
