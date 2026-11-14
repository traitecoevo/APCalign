#' Download taxonomic resources for a GitHub release
#'
#' Snapshots the current APC and APNI exports, for a maintainer to upload as
#' the assets of a new dated release (`version_name`, e.g. "2026-07-30"):
#' `apc.parquet` and `apni.parquet`, which `load_taxonomic_resources()`
#' downloads.
#'
#' @param version_name the release's tag, a date ("YYYY-MM-DD")
#' @param path directory to save the files in, to upload from
#' @keywords internal
#' @noRd
download_taxonomic_resources_for_release <- function(version_name = NULL,
                                                     path = "ignore/") {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)

  APC <- read_nsl_export("APC")
  nanoparquet::write_parquet(APC, file.path(path, "apc.parquet"))

  # Exclude names that are in APC from APNI
  APNI_cleaned <- read_nsl_export("APNI") |>
    dplyr::filter(!canonicalName %in% APC$canonicalName)
  nanoparquet::write_parquet(APNI_cleaned, file.path(path, "apni.parquet"))
}
