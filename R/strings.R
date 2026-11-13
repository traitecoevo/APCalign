## Base-R string helpers, in place of stringr/stringi.
##
## APCalign used to import stringr (and through it stringi, whose ICU library
## is the largest download when APCalign runs in a web browser through webR).
## These helpers reproduce the stringr behaviour APCalign relies on, so results
## are unchanged:
##
## - Patterns are Perl regular expressions (PCRE), not ICU. ICU's bare POSIX
##   classes are written as Unicode properties: `[:alpha:]` -> `\\p{L}`,
##   `[:digit:]` -> `\\p{Nd}`, `[:punct:]` -> `\\p{P}`, `[:space:]` ->
##   `[\\s\\p{Z}]`. Note `\\p{P}` is NOT base R's `[[:punct:]]`: ICU's
##   punctuation excludes symbols such as `+ < = > ^ $ | ~`.
## - Missing values stay missing (`grepl()` would return FALSE for NA).
##
## All are internal.

#' @noRd
re_detect <- function(x, pattern) {
  out <- grepl(pattern, x, perl = TRUE)
  out[is.na(x)] <- NA
  out
}

## First match of `pattern` in each element, NA where there is none.
#' @noRd
re_extract <- function(x, pattern) {
  out <- rep(NA_character_, length(x))
  m <- regexpr(pattern, x, perl = TRUE)
  ok <- !is.na(m) & m > 0
  out[ok] <- substr(x[ok], m[ok], m[ok] + attr(m, "match.length")[ok] - 1L)
  out
}

## Replace the first match. Like stringr::str_replace(), `pattern` may be a
## vector the same length as `x` (one pattern per element). An NA pattern gives
## an NA result, where stringr::str_replace() stops with an error.
#' @noRd
re_replace <- function(x, pattern, replacement) {
  if (length(pattern) == 1L) {
    if (is.na(pattern)) return(rep(NA_character_, length(x)))
    return(sub(pattern, replacement, x, perl = TRUE))
  }
  out <- rep(NA_character_, length(x))
  ok <- !is.na(pattern)
  out[ok] <- mapply(function(s, p) sub(p, replacement, s, perl = TRUE),
                    x[ok], pattern[ok], USE.NAMES = FALSE)
  out
}

#' @noRd
re_replace_all <- function(x, pattern, replacement) {
  gsub(pattern, replacement, x, perl = TRUE)
}

## Number of (non-overlapping) matches of a fixed string.
#' @noRd
count_fixed <- function(x, pattern) {
  m <- gregexpr(pattern, x, fixed = TRUE)
  out <- vapply(m, function(p) sum(p > 0), integer(1))
  out[is.na(x)] <- NA_integer_
  out
}

## Split each element at `pattern` and return piece `i`, NA if there are fewer
## pieces. Matches stringr::str_split_i(): an empty string is one empty piece,
## and a separator at the end leaves an empty last piece (strsplit() drops
## both).
#' @noRd
split_i <- function(x, pattern, i) {
  pieces <- strsplit(x, pattern, perl = TRUE)
  out <- vapply(pieces, function(p) if (length(p) >= i) p[[i]] else NA_character_,
                character(1))
  n <- lengths(pieces) +
    grepl(paste0("(?:", pattern, ")$"), x, perl = TRUE) +
    (!is.na(x) & x == "")
  out[is.na(out) & n >= i] <- ""
  out[is.na(x)] <- NA_character_
  out
}

## Split each element at `pattern`, keeping empty pieces the way
## stringr::str_split() does.
#' @noRd
split_all <- function(x, pattern) {
  pieces <- strsplit(x, pattern, perl = TRUE)
  trailing <- grepl(paste0("(?:", pattern, ")$"), x, perl = TRUE)
  pieces[trailing] <- lapply(pieces[trailing], function(p) c(p, ""))
  pieces[!is.na(x) & x == ""] <- list("")
  pieces[is.na(x)] <- list(NA_character_)
  pieces
}

## Trim, and collapse runs of white space to one space (stringr::str_squish()).
#' @noRd
squish <- function(x) {
  x <- gsub("^[\\s\\p{Z}]+|[\\s\\p{Z}]+$", "", x, perl = TRUE)
  gsub("[\\s\\p{Z}]+", " ", x, perl = TRUE)
}

#' @noRd
trim <- function(x) {
  gsub("^[\\s\\p{Z}]+|[\\s\\p{Z}]+$", "", x, perl = TRUE)
}

## Lower case, as stringr::str_to_lower() (ICU) did. ICU differs from base R
## for a handful of characters, e.g. U+0130 (I with dot above), listed in
## `case_table` (R/sysdata.rda, built by data-raw/ascii_table.R).
#' @noRd
to_lower <- function(x) {
  todo <- !is.na(x) & grepl("[^\\x01-\\x7F]", x, perl = TRUE)
  for (k in seq_along(case_table$lower_from)) {
    x[todo] <- gsub(case_table$lower_from[k], case_table$lower_to[k], x[todo], fixed = TRUE)
  }
  tolower(x)
}

## Sentence case, as stringr::str_to_sentence() (ICU) did: lower case, then
## title-case the first character of each sentence if that character is a
## letter. ICU skips punctuation and spaces to find it, but stops at a number or
## symbol (so "(acacia" -> "(Acacia" but "5acacia" is unchanged). A sentence
## starts at the beginning, after a line break, after ! ? and the other
## characters in `case_table$sentence_end` (even with no space after them), and
## after a full stop followed by spaces and an upper-case letter.
#' @noRd
to_sentence <- function(x) {
  first <- "^([^\\p{L}\\p{N}\\p{S}\\p{Co}]*)(\\p{L})"
  ends <- paste0("[\n\r\u0085\u2028\u2029", gsub("([]\\\\^-])", "\\\\\\1",
                 paste(case_table$sentence_end, collapse = "")), "]")
  full_stop <- "\\.[\\p{Ps}\\p{Pe}\\p{Pi}\\p{Pf}\"']*[\\s\\p{Z}]+[\\p{Lu}\\p{Lt}]"
  lead <- re_extract(x, first)
  letter <- substring(lead, nchar(lead))
  ## common case: one sentence, whose first letter is title-cased by PCRE's \\U
  ## the way ICU does it
  slow <- !is.na(x) & ((!is.na(letter) & letter %in% case_table$title_from) |
    grepl(ends, x, perl = TRUE) | grepl(full_stop, x, perl = TRUE))
  out <- sub(first, "\\1\\U\\2", to_lower(x), perl = TRUE)
  ## otherwise, one character at a time
  out[slow] <- vapply(x[slow], function(s) {
    ch <- strsplit(s, "")[[1]]
    n <- length(ch)
    starts <- 1L
    brk <- which(grepl(ends, ch, perl = TRUE))
    starts <- c(starts, brk + 1L)
    for (m in gregexpr(full_stop, s, perl = TRUE)[[1]]) {
      if (m > 0) starts <- c(starts, m + attr(regexpr(full_stop, substring(s, m), perl = TRUE), "match.length") - 1L)
    }
    low <- to_lower(ch)
    stop_at <- grepl("[\\p{L}\\p{N}\\p{S}\\p{Co}]", ch, perl = TRUE)
    for (st in unique(starts[starts <= n])) {
      j <- st - 1L + match(TRUE, stop_at[st:n])
      if (is.na(j) || !grepl("\\p{L}", ch[j], perl = TRUE)) next
      k <- match(ch[j], case_table$title_from)
      low[j] <- if (is.na(k)) sub("^(.)", "\\U\\1", low[j], perl = TRUE) else case_table$title_to[k]
    }
    paste(low, collapse = "")
  }, character(1), USE.NAMES = FALSE)
  out
}

## Transliterate to ASCII, as stringi::stri_trans_general(x, "Any-Latin;
## Latin-ASCII") did, using a fixed table (`ascii_table` in R/sysdata.rda,
## built by data-raw/ascii_table.R). The table covers every character up to
## U+33FF and the presentation and fullwidth forms, each on its own and, for
## accented letters, also written as letter + combining marks. That is exact for
## Latin script. Other scripts (Greek, Cyrillic, kana, ...) are transliterated
## a character at a time, where ICU sometimes uses the neighbouring characters;
## characters not in the table (e.g. CJK ideographs) are left as they are.
#' @noRd
to_ascii <- function(x) {
  todo <- !is.na(x) & grepl("[^\\x01-\\x7F]", x, perl = TRUE)
  if (!any(todo)) return(x)
  y <- enc2utf8(x[todo])
  ## letter + combining marks: longest sequences first
  marked <- grepl("\\p{M}", y, perl = TRUE)
  if (any(marked)) {
    multi <- which(nchar(ascii_table$from) > 1L)
    multi <- multi[order(-nchar(ascii_table$from[multi]))]
    for (k in multi) {
      y[marked] <- gsub(ascii_table$from[k], ascii_table$to[k], y[marked], fixed = TRUE)
    }
  }
  ## any other combining marks after a Latin letter are dropped too (as ICU
  ## does for, e.g., "D" + acute, which has no precomposed form)
  y <- gsub("(\\p{Latin})\\p{M}+", "\\1", y, perl = TRUE)
  ## the table rows for characters that occur
  present <- unique(unlist(strsplit(y, ""), use.names = FALSE))
  k <- which(ascii_table$from %in% present)
  one <- k[nchar(ascii_table$to[k]) == 1L & ascii_table$to[k] != "-"]
  ## replacements that are several characters (or "-", which chartr() would
  ## read as a range) first, then the one-to-one ones in a single chartr()
  ## call, so each character is replaced once
  for (j in setdiff(k, one)) {
    y <- gsub(ascii_table$from[j], ascii_table$to[j], y, fixed = TRUE)
  }
  if (length(one)) {
    y <- chartr(paste(ascii_table$from[one], collapse = ""),
                paste(ascii_table$to[one], collapse = ""), y)
  }
  x[todo] <- y
  x
}

## Pattern matching `word` as a whole word. Spelled out with Unicode classes
## because PCRE's `\\b` only knows ASCII word characters, while the ICU `\\b`
## stringr used treats accented letters as word characters too.
#' @noRd
whole_word <- function(word) {
  w <- "[\\p{L}\\p{M}\\p{Nd}\\p{Nl}\\p{Pc}\u200C\u200D]"
  paste0("(?<!", w, ")", word, "(?!", w, ")")
}
