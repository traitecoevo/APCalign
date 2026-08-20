#' Perl-flavoured gsub, used to chain regex replacements in a %>% pipeline
#' @noRd
f <- function(x, find, replace) {
  gsub(find, replace, x, perl = TRUE)
}
