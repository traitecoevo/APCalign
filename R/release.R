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
# status) and the new release_summary(). `status` is the most serious of:
# - "fail": don't release. A table is empty, its columns (names or types)
#   differ from the previous release, or a key column is entirely empty.
# - "review": release as a draft, for someone to check and publish by hand.
#   APC lost more than `max_apc_drop` of its rows, or a key column has some
#   values missing.
# - "unchanged": the row counts and latest `modified` time match the previous
#   release, so there is nothing new to release.
# - "pass": release.
#' @noRd
check_release_resources <- function(new, previous, max_apc_drop = 0.02) {
  failures <- character(0)
  reviews <- character(0)

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
      values <- new_table[[column]]
      missing <- sum(is.na(values) | values == "")
      if (missing == nrow(new_table)) {
        failures <- c(failures, paste0(table, " column ", column, " is empty."))
      } else if (missing > 0) {
        reviews <- c(reviews, paste0(
          table, " column ", column, " has ", format_count(missing),
          " missing values."
        ))
      }
    }
  }

  summary <- release_summary(new)
  previous_summary <- release_summary(previous)

  if (nrow(new$APC) > 0) {
    drop <- 1 - summary$apc_rows / previous_summary$apc_rows
    if (drop > max_apc_drop) {
      reviews <- c(reviews, sprintf(
        "APC has %s rows, %.1f%% fewer than the previous release (%s); more than the %.0f%% allowed.",
        format_count(summary$apc_rows), 100 * drop,
        format_count(previous_summary$apc_rows), 100 * max_apc_drop
      ))
    }
  }

  unchanged <- identical(summary, previous_summary)

  status <- if (length(failures) > 0) {
    "fail"
  } else if (length(reviews) > 0) {
    "review"
  } else if (unchanged) {
    "unchanged"
  } else {
    "pass"
  }
  problems <- switch(status,
    fail = c(failures, reviews),
    review = reviews,
    unchanged = paste0(
      "Same row counts and latest modified time (", summary$latest_modified,
      ") as the previous release."
    ),
    pass = character(0)
  )

  list(status = status, problems = problems, summary = summary)
}

# Names that have aligned the same way in every release since the benchmark
# version (2024-10-11). If a new release can't reproduce this, it is broken.
release_smoke_test_names <- data.frame(
  original_name = c(
    "Eucalyptus regnans", "Eucalyptus camaldulensis", "Acacia aneura",
    "Acacia dealbata", "Banksia integrifolia", "Grevillea robusta",
    "Triodia pungens",
    "Eucalyptus regnas" # misspelt, so it needs a fuzzy match
  ),
  accepted_name = c(
    "Eucalyptus regnans", "Eucalyptus camaldulensis", "Acacia aneura",
    "Acacia dealbata", "Banksia integrifolia", "Grevillea robusta",
    "Triodia pungens", "Eucalyptus regnans"
  )
)

# Load the release files in `path` through load_taxonomic_resources(), as a
# user would, and check the smoke-test names above align and update as
# expected. Returns a character vector of problems (empty if none).
#' @noRd
check_release_alignment <- function(path, version = "release-candidate") {
  # load_taxonomic_resources() reads a version's files from
  # tools::R_user_dir("APCalign") when they are there, so point that at a
  # temporary directory holding these files under the given version name.
  user_dir <- tempfile("apcalign-release-")
  old <- Sys.getenv("R_USER_DATA_DIR", unset = NA)
  Sys.setenv(R_USER_DATA_DIR = user_dir)
  on.exit({
    if (is.na(old)) Sys.unsetenv("R_USER_DATA_DIR") else Sys.setenv(R_USER_DATA_DIR = old)
    unlink(user_dir, recursive = TRUE)
    cache_key <- paste0("stable_", version)
    if (exists(cache_key, envir = .pkg_cache, inherits = FALSE)) {
      rm(list = cache_key, envir = .pkg_cache)
    }
  }, add = TRUE)

  cache_dir <- tools::R_user_dir("APCalign")
  dir.create(cache_dir, recursive = TRUE)
  file.copy(file.path(path, "apc.parquet"),
            file.path(cache_dir, paste0("apc", version, ".parquet")))
  file.copy(file.path(path, "apni.parquet"),
            file.path(cache_dir, paste0("apni", version, ".parquet")))

  result <- tryCatch({
    resources <- load_taxonomic_resources(version = version, quiet = TRUE)
    create_taxonomic_update_lookup(
      release_smoke_test_names$original_name,
      resources = resources,
      quiet = TRUE
    )
  }, error = function(e) e)

  if (inherits(result, "error")) {
    return(paste0("Aligning names failed: ", conditionMessage(result)))
  }

  expected <- release_smoke_test_names
  found <- result$accepted_name[match(expected$original_name, result$original_name)]
  wrong <- is.na(found) | found != expected$accepted_name
  sprintf(
    "%s: expected accepted name %s, got %s.",
    expected$original_name[wrong], expected$accepted_name[wrong],
    ifelse(is.na(found[wrong]), "none", found[wrong])
  )
}

# 114021 -> "114,021"
#' @noRd
format_count <- function(x) {
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
}
