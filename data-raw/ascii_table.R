## Builds R/sysdata.rda: the character tables that APCalign's base-R string
## helpers (R/strings.R) use to reproduce what stringi/ICU did, now that
## APCalign no longer imports stringi.
##
## - `ascii_table`: what stringi::stri_trans_general(x, "Any-Latin; Latin-ASCII")
##   turns each character into, for every character it changes in the blocks
##   below, plus the decomposed form (letter + combining marks) of each.
## - `case_table`: characters whose lower or title case in ICU differs from
##   base R's tolower()/toupper(), and ICU's sentence-terminal characters.
##
## Needs stringi and the taxonomic resources. Run from the package root:
##   Rscript data-raw/ascii_table.R

library(APCalign)
res <- load_taxonomic_resources(version = "2026-07-30", quiet = TRUE)

## Unicode blocks up to the CJK ideographs, plus the presentation forms and
## half/fullwidth forms, plus every non-ASCII character in the APC and APNI tables
in_data <- unlist(lapply(res[c("APC", "APNI")], function(d)
  unlist(lapply(d[vapply(d, is.character, TRUE)], function(v) utf8ToInt(paste(unique(v), collapse = ""))))))
codes <- sort(unique(c(0x00A0:0x33FF, 0xFB00:0xFFEF, in_data[in_data > 127])))
chars <- intToUtf8(codes, multiple = TRUE)
chars <- chars[!grepl("\\p{Cn}|\\p{Cs}|\\p{Co}", chars, perl = TRUE)]

translit <- function(s) stringi::stri_trans_general(s, "Any-Latin; Latin-ASCII")

## single characters the transform changes
to <- translit(chars)
keep <- to != chars
from <- chars[keep]; to <- to[keep]
## their decomposed forms, e.g. "e" + U+0301 for U+00E9
nfd <- stringi::stri_trans_nfd(from)
multi <- nchar(nfd) > 1 & nfd != from
from <- c(from, nfd[multi]); to <- c(to, translit(nfd[multi]))
## a decomposed form can be shared by two characters (e.g. compatibility forms)
dup <- duplicated(from)
from <- from[!dup]; to <- to[!dup]

## case: where ICU and base R disagree
lower_icu <- stringi::stri_trans_tolower(chars)
lower_diff <- lower_icu != tolower(chars)
## ICU title-cases the ORIGINAL character (so U+1E9E capital sharp s stays as
## it is, while lower-case sharp s becomes "Ss"). to_sentence() lower-cases
## first and then uses PCRE's \\U, so record every letter where that differs.
let <- chars[grepl("^\\p{L}$", chars, perl = TRUE)]
title_icu <- stringi::stri_trans_totitle(let)
title_diff <- title_icu != sub("^(.)", "\\U\\1", stringi::stri_trans_tolower(let), perl = TRUE)
## characters after which ICU starts a new sentence even with no space between:
## the sentence terminals, less the full stops (ICU's ATerm), which only end a
## sentence before a space
sterm <- chars[stringi::stri_detect_regex(chars, "^\\p{Sentence_Terminal}$")]
sterm <- setdiff(c("!", "?", sterm), c(".", "\u2024", "\uFE52", "\uFF0E"))

case_table <- list(
  lower_from = chars[lower_diff], lower_to = lower_icu[lower_diff],
  title_from = let[title_diff], title_to = title_icu[title_diff],
  sentence_end = sterm
)
ascii_table <- list(from = from, to = to)
attr(ascii_table, "source") <- attr(case_table, "source") <-
  paste0("stringi ", packageVersion("stringi"), " (ICU ", stringi::stri_info()$ICU.version, ")")
save(ascii_table, case_table, file = "R/sysdata.rda", compress = "xz")
cat("ascii_table:", length(from), "entries;", "lower exceptions:", sum(lower_diff),
    "; title exceptions:", sum(title_diff), "; sentence terminals:", length(sterm), "\n")
