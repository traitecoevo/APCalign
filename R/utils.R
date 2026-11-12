#' Perl-flavoured gsub with `x` first, so regex replacements chain in a `%>%` pipeline
#'
#' Shared by `standardise_names()`, `strip_names()` and `strip_names_extra()`.
#' Note the sibling `gsub_fixed()` in `standardise_names.R` — same shape, but
#' literal (`fixed = TRUE`) rather than regex matching.
#' @noRd
gsub_perl <- function(x, find, replace) {
  gsub(find, replace, x, perl = TRUE)
}

#' Apply a list of Perl-regex replacements to `x`, one after another
#'
#' `rules` is a list of `c(find, replace)` pairs, applied in order. Used instead
#' of a long `%>%` chain of `gsub_perl()` calls: magrittr's `%>%` (and the
#' native `|>`) run a chain as nested calls, so the evaluation depth grows with
#' the length of the chain. `standardise_names()` used to be a 46-step chain
#' that needed an evaluation depth of about 400, which overflows small stacks
#' (webR in Safari fails with "Maximum call stack size exceeded"). A loop has
#' constant depth.
#' @noRd
gsub_perl_rules <- function(x, rules) {
  for (rule in rules) x <- gsub(rule[[1]], rule[[2]], x, perl = TRUE)
  x
}
