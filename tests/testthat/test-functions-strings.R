# The base-R string helpers in R/strings.R replace stringr/stringi. Each test
# below pins a behaviour where base R and stringr differ, so that an edit to a
# helper cannot silently change APCalign's output. The expected values were
# produced by stringr 1.6.0 / stringi 1.8.7 (ICU 74); the last test compares
# with stringr/stringi directly when they are installed.

test_that("re_detect() keeps missing values missing", {
  expect_equal(APCalign:::re_detect(c("a", NA, "b"), "a"), c(TRUE, NA, FALSE))
})

test_that("[:punct:] is ICU punctuation (\\p{P}), not base R's [[:punct:]]", {
  # ICU punctuation excludes the symbols + < | ~
  expect_equal(APCalign:::re_replace_all("a+b<c-d.e|f~g'h", "\\p{P}", " "),
               "a+b<c d e|f~g h")
  expect_equal(strip_names("Acacia+aneura"), "acacia+aneura")
})

test_that("U+0085 (next line) counts as white space, as in ICU", {
  # Arrives from Windows-1252 text (an ellipsis) read as latin1. PCRE's \s and
  # \p{Z} do not include it.
  expect_equal(APCalign:::squish("Acacia\u0085dealbata\u0085"), "Acacia dealbata")
  expect_equal(standardise_names("Grevillea sp.\u0085"), "Grevillea sp.")
  expect_equal(extract_genus("Acacia\u0085dealbata"), "Acacia")
})

test_that("a byte-order mark at the start is dropped where stringi dropped it", {
  # dropped by squish, trim, lower and sentence case, and splitting ...
  expect_equal(APCalign:::squish("\uFEFFAcacia  dealbata"), "Acacia dealbata")
  expect_equal(APCalign:::to_lower("\uFEFFABC"), "abc")
  expect_equal(APCalign:::split_i("\uFEFFAb c", " ", 1), "Ab")
  expect_equal(standardise_names("\uFEFFAcacia dealbata"), "Acacia dealbata")
  # ... but kept anywhere else in the string, and by transliteration
  expect_equal(APCalign:::squish("A\uFEFFb"), "A\uFEFFb")
  expect_equal(APCalign:::to_lower("A\uFEFFB"), "a\uFEFFb")
  expect_equal(APCalign:::to_ascii("\uFEFFA"), "\uFEFFA")
  expect_equal(strip_names("Acacia dealbata\uFEFF"), "acacia dealbata\uFEFF")
})

test_that("split_i() and split_all() keep the empty pieces stringr keeps", {
  expect_equal(APCalign:::split_i("a ", " ", 2), "")      # separator at the end
  expect_equal(APCalign:::split_i(" a", " ", 1), "")      # separator at the start
  expect_equal(APCalign:::split_i("", " ", 1), "")        # empty string: one piece
  expect_equal(APCalign:::split_i("", " ", 2), NA_character_)
  expect_equal(APCalign:::split_i(NA, " ", 1), NA_character_)
  expect_equal(APCalign:::split_all("NSW,Qld,", ","), list(c("NSW", "Qld", "")))
  # a final newline is not a separator at the end (\z, not $)
  expect_equal(APCalign:::split_i("a \n", " ", 3), NA_character_)
  expect_equal(APCalign:::split_all("NSW, Qld,\n", ","), list(c("NSW", " Qld", "\n")))
})

test_that("re_replace() takes one pattern per element, like stringr", {
  expect_equal(APCalign:::re_replace(c("Acacia a", "Banksia b"), c("Acacia", "Banksia"), ""),
               c(" a", " b"))
  # an NA pattern gives NA (stringr stops with an error), and the result stays
  # a character vector even when every pattern is NA
  expect_identical(APCalign:::re_replace(c("a", "b"), c(NA_character_, NA_character_), ""),
                   c(NA_character_, NA_character_))
  expect_identical(APCalign:::re_replace(c("a", "b"), c("a", NA), "x"), c("x", NA))
  # patterns of another length are an error, not silently recycled
  expect_error(APCalign:::re_replace(c("a", "b", "c"), c("a", "b"), ""))
})

test_that("to_sentence() follows ICU's sentence case", {
  x <- c("acacia?-dealbata", "1.Acacia", "(acacia", "5acacia", "ab!cd",
         "\u01C6a", "\u1E9Ea", "a.B", "ab.cd", "a. B", "\u00A4wx", "ACACIA", "x", NA)
  expected <- c("Acacia?-dealbata", "1.Acacia", "(Acacia", "5acacia", "Ab!Cd",
                "\u01C5a", "\u1E9Ea", "A.b", "Ab.cd", "A. B", "\u00A4wx", "Acacia", "X", NA)
  expect_equal(APCalign:::to_sentence(x), expected)
})

test_that("to_lower() follows ICU where base R differs", {
  expect_equal(APCalign:::to_lower("\u0130"), "i\u0307")
  expect_equal(APCalign:::to_lower("\u00C9COLE"), "\u00E9cole")
})

test_that("to_ascii() transliterates as ICU's Any-Latin; Latin-ASCII did", {
  x <- c("\u00E9", "e\u0301", "D\u0301", "\u00DF", "\u00A0", "\u00D7", "\uA733",
         "!\u0308", "\u2215", "\u201CAcacia\u201D", "\u2013")
  expected <- c("e", "e", "D", "ss", " ", "*", "aa", "!\u0308", "/", "\"Acacia\"", "-")
  expect_equal(APCalign:::to_ascii(x), expected)
  expect_equal(standardise_names("Ac\u00E1cia longifolia"), "Acacia longifolia")
})

test_that("whole_word() treats accented letters as word characters, like ICU's \\b", {
  x <- c("a x b", "\u00E9x b", "x\u0301 b", "1x", "x")
  expect_equal(APCalign:::re_detect(x, APCalign:::whole_word("x")),
               c(TRUE, FALSE, FALSE, FALSE, TRUE))
})

test_that("patterns with characters above U+00FF work on all-ASCII input", {
  # PCRE rejects a \x{...} escape above 255 unless the input is non-ASCII, so
  # such characters must be written as \u escapes in the R string
  expect_no_error(APCalign:::to_sentence(c("acacia!xyz", "Acacia aneura")))
  expect_no_error(strip_names(c("Acacia aneura", "Banksia/serrata")))
  expect_no_error(APCalign:::re_detect("Acacia aneura", APCalign:::whole_word("aff")))
})

test_that("fuzzy_match()'s first-letter cache follows the reference list", {
  expect_equal(APCalign:::fuzzy_match("acacia anera", c("acacia aneura", "banksia serrata"), 2, 0.35),
               "acacia aneura")
  # a different list straight after must not reuse the previous first letters
  expect_true(is.na(APCalign:::fuzzy_match("acacia anera", c("banksia serrata"), 2, 0.35)))
  expect_equal(APCalign:::fuzzy_match("banksia serata", c("banksia serrata", "acacia aneura"), 2, 0.35),
               "banksia serrata")
})

test_that("odd white space and BOMs give the same alignment as clean input", {
  skip_if_no_resources()
  clean <- create_taxonomic_update_lookup(c("Grevillea sp.", "Acacia dealbata"), resources = resources)
  messy <- create_taxonomic_update_lookup(c("Grevillea sp.\u0085", "\uFEFFAcacia dealbata"), resources = resources)
  expect_equal(messy$aligned_name, clean$aligned_name)
  expect_equal(messy$taxon_rank, clean$taxon_rank)
})

test_that("helpers agree with stringr/stringi on awkward strings", {
  skip_if_not_installed("stringr")
  skip_if_not_installed("stringi")
  z <- c("", " ", "a ", " a", "a  b", "a/", "/a", NA, "a /b", "x", "a \n",
         "Acacia\u0085dealbata", "\uFEFFAcacia dealbata", "A\uFEFFb", "Ac\u00E1cia",
         "e\u0301x", "\u00A0Banksia\u3000serrata ", "(acacia", "acacia?-dealbata",
         "\u0130stanbul", "\u00DFa", "a+b<c|d~e", "Acacia\u2215B", "\u201CAcacia\u201D")
  expect_equal(APCalign:::squish(z), stringr::str_squish(z))
  expect_equal(APCalign:::trim(z), stringr::str_trim(z))
  expect_equal(APCalign:::to_lower(z), stringr::str_to_lower(z))
  expect_equal(APCalign:::to_ascii(z), stringi::stri_trans_general(z, "Any-Latin; Latin-ASCII"))
  first <- stringr::str_split_i(z, " ", 1)
  expect_equal(APCalign:::to_sentence(first), stringr::str_to_sentence(first))
  for (i in 1:3) {
    expect_equal(APCalign:::split_i(z, " |\\/", i), stringr::str_split_i(z, " |\\/", i))
  }
  expect_equal(APCalign:::split_all(z, ","), stringr::str_split(z, ","))
  expect_equal(APCalign:::re_extract(z, "\\p{L}|\\p{Nd}"), stringr::str_extract(z, "[:alpha:]|[:digit:]"))
  expect_equal(APCalign:::re_replace_all(z, "\\p{P}", " "), stringr::str_replace_all(z, "[:punct:]", " "))
  expect_equal(APCalign:::re_detect(z, "[\\s\\p{Z}\\x85]\\/"), stringr::str_detect(z, "\\s\\/"))
  expect_equal(APCalign:::count_fixed(z, " "), stringr::str_count(z, " "))
})
