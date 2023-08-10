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
#'
#'
#' @examples
#' strip_names(c("Abies lasiocarpa subsp. lasiocarpa",
#'               "Quercus kelloggii",
#'               "Pinus contorta var. latifolia"))
#'
#' @noRd
strip_names <- function(taxon_names) {
  taxon_names %>%
    stringr::str_replace_all("\\.", "") %>%
    stringr::str_replace_all("[:punct:]", " ") %>%
    stringr::str_replace_all(" subsp ", " ") %>%
    stringr::str_replace_all(" ssp ", " ") %>%
    stringr::str_replace_all(" var |var$", " ") %>%
    stringr::str_replace_all(" ser ", " ") %>%
    stringr::str_replace_all(" f ", " ") %>%
    stringr::str_replace_all(" s l ", " ") %>%
    stringr::str_replace_all(" s s ", " ") %>%
    stringr::str_replace_all("\\=", " ") %>%
    stringr::str_replace_all("  ", " ") %>%
    stringr::str_squish() %>%
    tolower()
}

#' @noRd
strip_names_2 <- function(x) {
  x %>%
    stringr::str_replace_all("\\.", "") %>%
    stringr::str_replace_all("[:punct:]", " ") %>%
    stringr::str_replace_all(" subsp ", " ") %>%
    stringr::str_replace_all(" var | var$", " ") %>%
    stringr::str_replace_all(" ser ", " ") %>%
    stringr::str_replace_all(" f ", " ") %>%
    stringr::str_replace_all(" forma ", " ") %>%
    stringr::str_replace_all(" species ", " ") %>%
    stringr::str_replace_all(" s l ", " ") %>%
    stringr::str_replace_all(" s s ", " ") %>%
    stringr::str_replace_all(" ss ", " ") %>%
    stringr::str_replace_all(" x ", " ") %>%
    stringr::str_replace_all(" sp ", " ") %>%
    stringr::str_replace_all("sp$", " ") %>%
    stringr::str_replace_all(" sp1", " 1") %>%
    stringr::str_replace_all(" sp2", " 2") %>%
    stringr::str_replace_all(" ssp |ssp $", " ") %>%
    stringr::str_replace_all(" cf | cf$", " ") %>%
    stringr::str_replace_all("\\=", " ") %>%
    stringr::str_replace_all("  ", " ") %>%
    stringr::str_squish() %>%
    tolower()
}
