#' Perl-flavoured gsub with `x` first, so regex replacements chain in a `%>%` pipeline
#'
#' Shared by `standardise_names()`, `strip_names()` and `strip_names_extra()`.
#' Note the sibling `gsub_fixed()` in `standardise_names.R` — same shape, but
#' literal (`fixed = TRUE`) rather than regex matching.
#' @noRd
gsub_perl <- function(x, find, replace) {
  gsub(find, replace, x, perl = TRUE)
}
