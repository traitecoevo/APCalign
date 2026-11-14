## Base-R string helpers, in place of stringr/stringi.
##
## APCalign used to import stringr (and through it stringi, whose ICU library
## is the largest download when APCalign runs in a web browser through webR).
## These helpers reproduce the stringr behaviour APCalign relies on, so results
## are unchanged. tests/testthat/test-functions-strings.R pins each behaviour
## below, including every gap found while writing them.
##
## - Patterns are Perl regular expressions (PCRE), not ICU. ICU's bare POSIX
##   classes are written as Unicode properties: `[:alpha:]` -> `\\p{L}` (close
##   enough here: ICU's also takes some marks and letter-like symbols, which
##   to_ascii() has removed or replaced before any such match), `[:digit:]` ->
##   `\\p{Nd}`, `[:punct:]` -> `\\p{P}`, and `[:space:]` or `\\s` ->
##   `[\\s\\p{Z}\\x85]`. `\\p{P}` is NOT base R's `[[:punct:]]`: ICU's
##   punctuation excludes symbols such as `+ < = > ^ $ | ~`. And ICU counts
##   U+0085 (next line) as white space, PCRE's `\\s` and `\\p{Z}` do not.
## - A character above U+00FF goes in the R string as a `\\u` escape, never in
##   the regex as `\\x{...}`: PCRE only runs in Unicode mode when the input or
##   pattern is non-ASCII, and rejects `\\x{...}` above 255 otherwise.
## - Missing values stay missing (`grepl()` would return FALSE for NA).
## - stringi drops a byte-order mark (U+FEFF) at the very start of a string in
##   some functions (squish, trim, lower and sentence case, splitting) but not
##   others (detect, replace, transliterate). The helpers copy that.
##
## All are internal.

## Remove a byte-order mark at the start of each string.
#' @noRd
drop_bom <- function(x) {
  if (!is.character(x)) x <- as.character(x)  # as stringr does for NA, factors
  b <- !is.na(x) & startsWith(x, "\uFEFF")
  if (any(b)) x[b] <- substring(x[b], 2L)
  x
}

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
## vector the same length as `x` (one pattern per element), and other lengths
## are an error. An NA pattern gives an NA result, where stringr::str_replace()
## stops with an error.
#' @noRd
re_replace <- function(x, pattern, replacement) {
  if (length(pattern) == 1L) {
    if (is.na(pattern)) return(rep(NA_character_, length(x)))
    return(sub(pattern, replacement, x, perl = TRUE))
  }
  if (length(pattern) != length(x)) {
    stop("`pattern` must have length 1 or the same length as `x`.", call. = FALSE)
  }
  out <- rep(NA_character_, length(x))
  ok <- !is.na(pattern)
  if (any(ok)) {
    out[ok] <- mapply(function(s, p) sub(p, replacement, s, perl = TRUE),
                      x[ok], pattern[ok], USE.NAMES = FALSE)
  }
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

## Does `pattern` match at the very end of each string? (`\\z`, not `$`, which
## also matches before a final newline.)
#' @noRd
ends_with_match <- function(x, pattern) {
  grepl(paste0("(?:", pattern, ")\\z"), x, perl = TRUE)
}

## Split each element at `pattern` and return piece `i`, NA if there are fewer
## pieces. Matches stringr::str_split_i(): an empty string is one empty piece,
## and a separator at the end leaves an empty last piece (strsplit() drops
## both).
#' @noRd
split_i <- function(x, pattern, i) {
  x <- drop_bom(x)
  pieces <- strsplit(x, pattern, perl = TRUE)
  out <- vapply(pieces, function(p) if (length(p) >= i) p[[i]] else NA_character_,
                character(1))
  n <- lengths(pieces) + ends_with_match(x, pattern) + (!is.na(x) & x == "")
  out[is.na(out) & n >= i] <- ""
  out[is.na(x)] <- NA_character_
  out
}

## Split each element at `pattern`, keeping empty pieces the way
## stringr::str_split() does.
#' @noRd
split_all <- function(x, pattern) {
  x <- drop_bom(x)
  pieces <- strsplit(x, pattern, perl = TRUE)
  trailing <- ends_with_match(x, pattern)
  pieces[trailing] <- lapply(pieces[trailing], function(p) c(p, ""))
  pieces[!is.na(x) & x == ""] <- list("")
  pieces[is.na(x)] <- list(NA_character_)
  pieces
}

## Trim, and collapse runs of white space to one space (stringr::str_squish()).
#' @noRd
squish <- function(x) {
  x <- trim(x)
  gsub("[\\s\\p{Z}\\x85]+", " ", x, perl = TRUE)
}

#' @noRd
trim <- function(x) {
  gsub("^[\\s\\p{Z}\\x85]+|[\\s\\p{Z}\\x85]+$", "", drop_bom(x), perl = TRUE)
}

## Lower case, as stringr::str_to_lower() (ICU) did. ICU differs from base R
## for a handful of characters, e.g. U+0130 (I with dot above), listed in
## `case_table` (R/sysdata.rda, built by data-raw/ascii_table.R).
#' @noRd
to_lower <- function(x) {
  x <- drop_bom(x)
  todo <- !is.na(x) & grepl("[^\\x01-\\x7F]", x, perl = TRUE)
  for (k in seq_along(case_table$lower_from)) {
    x[todo] <- gsub(case_table$lower_from[k], case_table$lower_to[k], x[todo], fixed = TRUE)
  }
  tolower(x)
}

## Sentence case, as stringr::str_to_sentence() (ICU) did: lower case, then
## title-case the first character of each sentence if that character is a
## letter. ICU skips punctuation and spaces to find it, but stops at a number or
## symbol (so "(acacia" -> "(Acacia" but "5acacia" is unchanged). Where a
## sentence ends follows ICU's sentence-break rules (sentence_starts()).
#' @noRd
to_sentence <- function(x) {
  x <- drop_bom(x)
  first <- "^([^\\p{L}\\p{N}\\p{S}\\p{Co}]*)(\\p{L})"
  lead <- re_extract(x, first)
  letter <- substring(lead, nchar(lead))
  ## a string can only hold a second sentence if it has a sentence terminal or
  ## a line break in it
  breaks <- case_table$sb_chars[case_table$sb_class %in% c("AT", "ST", "SE")]
  slow <- !is.na(x) & ((!is.na(letter) & letter %in% case_table$title_from) |
    grepl(paste0("[", gsub("([]\\\\^-])", "\\\\\\1", paste(breaks, collapse = "")), "]"),
          x, perl = TRUE))
  ## common case: one sentence, whose first letter PCRE's \\U title-cases the
  ## way ICU does
  out <- sub(first, "\\1\\U\\2", to_lower(x), perl = TRUE)
  ## otherwise, one character at a time
  out[slow] <- vapply(x[slow], function(s) {
    ch <- strsplit(s, "")[[1]]
    n <- length(ch)
    low <- to_lower(ch)
    stop_at <- grepl("[\\p{L}\\p{N}\\p{S}\\p{Co}]", ch, perl = TRUE)
    for (st in sentence_starts(ch)) {
      j <- st - 1L + match(TRUE, stop_at[st:n])
      if (is.na(j) || !grepl("\\p{L}", ch[j], perl = TRUE)) next
      k <- match(ch[j], case_table$title_from)
      low[j] <- if (is.na(k)) sub("^(.)", "\\U\\1", low[j], perl = TRUE) else case_table$title_to[k]
    }
    paste(low, collapse = "")
  }, character(1), USE.NAMES = FALSE)
  out
}

## Positions in the character vector `ch` where a sentence starts, following
## the Unicode sentence-break rules ICU uses (UAX #29, rules SB3-SB11), with
## each character's class from `case_table` (or from its general category for
## characters outside the table).
#' @noRd
sentence_starts <- function(ch) {
  n <- length(ch)
  cl <- case_table$sb_class[match(ch, case_table$sb_chars)]
  miss <- is.na(cl)
  cl[miss & grepl("^\\p{Lu}$", ch, perl = TRUE)] <- "UP"
  cl[miss & grepl("^\\p{Ll}$", ch, perl = TRUE)] <- "LO"
  cl[miss & is.na(cl) & grepl("^\\p{L}$", ch, perl = TRUE)] <- "LE"
  cl[miss & is.na(cl) & grepl("^\\p{Nd}$", ch, perl = TRUE)] <- "NU"
  cl[is.na(cl)] <- "XX"
  starts <- 1L
  for (i in seq_len(n)) {
    if (cl[i] == "SE") {
      ## SB3: CR LF stays together; SB4: break after any other line break
      if (!(ch[i] == "\r" && i < n && ch[i + 1L] == "\n")) starts <- c(starts, i + 1L)
      next
    }
    if (!cl[i] %in% c("AT", "ST")) next
    j <- i + 1L
    while (j <= n && cl[j] == "CL") j <- j + 1L
    no_close <- j == i + 1L
    while (j <= n && cl[j] == "SP") j <- j + 1L
    if (j > n) next
    ## SB11 with a line break after: the break falls after it (handled above)
    if (cl[j] == "SE") next
    if (cl[i] == "AT") {
      ## SB6: "3.4"
      if (j == i + 1L && cl[j] == "NU") next
      ## SB7: "U.S" (letter, full stop, upper case, nothing between)
      if (j == i + 1L && i > 1L && cl[i - 1L] %in% c("UP", "LO") && cl[j] == "UP") next
      ## SB8: no break if the next letter-like character is lower case
      k <- j
      while (k <= n && !cl[k] %in% c("LE", "UP", "LO", "SE", "AT", "ST")) k <- k + 1L
      if (k <= n && cl[k] == "LO") next
    }
    ## SB8a: no break before a continuation character or another terminal
    if (cl[j] %in% c("SC", "AT", "ST")) next
    ## SB11: break
    starts <- c(starts, j)
  }
  unique(starts[starts <= n])
}

## Transliterate to ASCII, as stringi::stri_trans_general(x, "Any-Latin;
## Latin-ASCII") did, using a fixed table (`ascii_table` in R/sysdata.rda,
## built by data-raw/ascii_table.R). The table covers every character up to
## U+33FF, Latin Extended-D and -E, and the presentation and fullwidth forms,
## each on its own and, for accented letters, also written as letter +
## combining marks. That is exact for Latin script in those blocks. Other
## scripts (Greek, Cyrillic, kana, ...) are transliterated a character at a
## time, where ICU sometimes uses the neighbouring characters; characters not
## in the table (e.g. CJK ideographs) are left as they are.
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
