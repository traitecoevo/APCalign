# Run by .github/workflows/apc-release.yaml (#315): check the APC and APNI
# files the workflow has just downloaded from NSL against the previous
# release, and write the outcome for the workflow's release step.
#
# Usage: Rscript apc-release.R <tag> <new_dir> <previous_dir>
#
# Writes `status` (pass, review, unchanged or fail) to $GITHUB_OUTPUT, the
# release notes to <new_dir>/notes.md, and a report to $GITHUB_STEP_SUMMARY.
# Exits with an error on "fail", so nothing is released.

args <- commandArgs(trailingOnly = TRUE)
tag <- args[[1]]
new_dir <- args[[2]]
previous_dir <- args[[3]]

# helpers = FALSE: don't run tests/testthat/helper.R, which loads the test data
pkgload::load_all(helpers = FALSE, quiet = TRUE)

# Check the files as written, so they are compared like for like with the
# previous release's parquet files
new <- read_release_files(new_dir)
previous <- read_release_files(previous_dir)
result <- check_release_resources(new, previous)

# Files that can't be loaded fail; names that align differently need review,
# since APC may have changed their taxonomy
if (result$status %in% c("pass", "review")) {
  alignment <- check_release_alignment(new_dir, version = tag)
  if (length(alignment$fail) > 0) {
    result$status <- "fail"
    result$problems <- c(result$problems, alignment$fail)
  } else if (length(alignment$review) > 0) {
    result$status <- "review"
    result$problems <- c(result$problems, alignment$review)
  }
}

s <- result$summary
notes <- sprintf(
  "Download of taxonomic resources from APC and APNI as of %s (APC rows: %s; APNI rows not in APC: %s).",
  tag, format_count(s$apc_rows), format_count(s$apni_rows)
)
if (result$status == "review") {
  notes <- c(
    notes, "",
    "**Draft: needs review before publishing.** The release checks flagged:",
    paste0("- ", result$problems)
  )
}
writeLines(notes, file.path(new_dir, "notes.md"))

previous_summary <- release_summary(previous)
report <- c(
  paste0("## APC/APNI release ", tag, ": ", result$status), "",
  "| | Previous release | This download |",
  "|---|---:|---:|",
  sprintf("| APC rows | %s | %s |", format_count(previous_summary$apc_rows), format_count(s$apc_rows)),
  sprintf("| APNI rows not in APC | %s | %s |", format_count(previous_summary$apni_rows), format_count(s$apni_rows)),
  sprintf("| Latest modified (UTC) | %s | %s |", previous_summary$latest_modified, s$latest_modified),
  "",
  if (length(result$problems) > 0) paste0("- ", result$problems)
)
writeLines(report)
summary_file <- Sys.getenv("GITHUB_STEP_SUMMARY")
if (nzchar(summary_file)) write(report, summary_file, append = TRUE)

output_file <- Sys.getenv("GITHUB_OUTPUT")
if (nzchar(output_file)) write(paste0("status=", result$status), output_file, append = TRUE)

if (result$status == "fail") {
  stop("The new release files failed the checks, so nothing was released:\n",
       paste0("- ", result$problems, collapse = "\n"))
}
