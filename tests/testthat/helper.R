# The taxonomic resources most tests use, loaded once (later calls reuse the
# session cache). Without an internet connection or a locally cached copy this
# is NULL, and the tests that need it skip rather than fail.
resources <- load_taxonomic_resources(stable_or_current_data = "stable", version = "2024-10-11")

skip_if_no_resources <- function() {
  testthat::skip_if(is.null(resources), "Taxonomic resources could not be loaded")
}

# The taxon names listed in "Genus sp. [collapsed names: A (status) | B (status)]",
# without their statuses; one character vector per suggested_name.
collapsed_names <- function(suggested_name) {
  inside <- sub("^.*\\[collapsed names: (.*)\\]$", "\\1", suggested_name)
  lapply(strsplit(inside, " | ", fixed = TRUE), function(x) sub(" \\([^()]*\\)$", "", x))
}

# Checks the rows of `return_all` output against `collapse_to_higher_taxon`
# output for the same names: a name that is not collapsed has one row, and each
# name returned for a collapsed name is one of its collapsed names.
expect_return_all_matches_collapsed <- function(all_rows, collapsed) {
  is_collapsed <- !is.na(collapsed$number_of_collapsed_taxa)
  rows_per_name <- table(all_rows$original_name)
  testthat::expect_true(all(rows_per_name[collapsed$original_name[!is_collapsed]] == 1))
  listed <- collapsed_names(collapsed$suggested_name[is_collapsed])
  testthat::expect_equal(lengths(listed), collapsed$number_of_collapsed_taxa[is_collapsed])
  for (i in seq_along(listed)) {
    name <- collapsed$original_name[is_collapsed][i]
    testthat::expect_contains(listed[[i]], all_rows$suggested_name[all_rows$original_name == name])
  }
}
