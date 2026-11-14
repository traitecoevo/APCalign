#' @title Strip taxon names
#' 
#' @description
#' Strip taxonomic names of taxon rank abbreviations and qualifiers 
#' and special characters
#'
#' @details
#' Given a vector of taxonomic names, this function removes:
#' -  subtaxa designations ("subsp.", "var.", "f.", and "ser")
#' -  special characters (e.g., "-", ".", "(", ")", "?")
#' -  extra whitespace
#' 
#'  The resulting vector of names is also converted to lowercase.
#'
#' @param taxon_names A character vector of taxonomic names to be stripped.
#'
#' @return A character vector of stripped taxonomic names,
#'  with subtaxa designations, special characters, and extra whitespace
#'  removed, and all letters converted to lowercase.
#'
#'
#' @examples
#' strip_names(c("Abies lasiocarpa subsp. lasiocarpa",
#'               "Quercus kelloggii",
#'               "Pinus contorta var. latifolia"))
#'
#' @export
strip_names <- function(taxon_names) {
  ## Steps run one after another, not as a single `%>%` chain (see gsub_perl_rules()).
  x <- gsub_perl_rules(taxon_names, list(
    c("\\.", ""),
    c("\\ \\)", ""),
    c("\\(\\ ", "")
  ))
  x <- re_replace_all(x, "\\p{P}", " ")
  x <- gsub("\u2215", " ", x, fixed = TRUE)  # division slash
  x <- gsub_perl_rules(x, list(
    c("\\,", ""),
    c("\\=", " "),
    c("  ", " "),
    c(" subsp ", " "),
    c(" var ", " "),
    c(" ser ", " "),
    c(" f ", " ")
  ))
  to_lower(squish(x))
}

#' @title Strip taxon names, extra
#' 
#' @description
#' Strip taxonomic names of `sp.` and hybrid symbols. This function assumes 
#' that a character function has already been run through `strip_names`.
#' 
#' @details
#' Given a vector of taxonomic names, this function removes additional filler
#' words (" x " for hybrid taxa, "sp.") not removed by the function 
#' `strip_names`
#'
#' @param taxon_names A character vector of taxonomic names to be stripped.
#'
#' @return A character vector of stripped taxonomic names, 
#'  with `sp.` and hybrid symbols removed.
#'
#'
#' @examples
#' strip_names_extra(c("Abies lasiocarpa subsp. lasiocarpa",
#'               "Quercus kelloggii",
#'               "Pinus contorta var. latifolia",
#'               "Acacia sp.",
#'               "Lepidium sp. Tanguin Hill (K.R.Newbey 10501)"))
#'
#' @export
strip_names_extra <- function(taxon_names) {
  x <- gsub_perl_rules(taxon_names, list(
    c(" species ", " "),
    c(" x ", " "),
    c(" sp ", " "),
    c(" sp1", " 1"),
    c(" sp2", " 2")
  ))
  squish(x)
}
