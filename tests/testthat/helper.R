# The taxonomic resources most tests use, loaded once (later calls reuse the
# session cache). Without an internet connection or a locally cached copy this
# is NULL, and the tests that need it skip rather than fail.
resources <- load_taxonomic_resources(stable_or_current_data = "stable", version = "2024-10-11")

skip_if_no_resources <- function() {
  testthat::skip_if(is.null(resources), "Taxonomic resources could not be loaded")
}
