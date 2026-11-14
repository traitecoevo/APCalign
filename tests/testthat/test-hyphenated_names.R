# Hyphenated genera (ICN Art. 20.3, e.g. Van-royena) and epithets
# (plantago-aquatica, undulata-elegans) are one word each. Split at the hyphen,
# every later word shifted along, so the first-two- and first-three-word keys
# matched the wrong names (#310).

test_that("strip_names() keeps a hyphen between two letters, and only there", {
  expect_equal(
    strip_names(c(
      "Van-royena",
      "Alisma plantago-aquatica",
      "Psychotria daphnoides f. 'small-leaved'",
      "Acacia dealbata--mearnsii",   # intergrade marker, not a hyphenated word
      "Acacia sp. (12-3)",           # collector number
      "Acacia - aneura",
      "Acacia -aneura",
      "Acacia sp.-Mt Magnet",        # a hyphen after a rank marker or qualifier
      "Acacia sp-Mt Magnet",         # separates two words
      "Acacia aneura var-intermedia",
      "Acacia aff-aneura",
      "Acacia x-aneura",
      "Van\u2010royena"               # the Unicode hyphen U+2010
    )),
    c(
      "van-royena",
      "alisma plantago-aquatica",
      "psychotria daphnoides small-leaved",
      "acacia dealbata mearnsii",
      "acacia sp 12 3",
      "acacia aneura",
      "acacia aneura",
      "acacia sp mt magnet",
      "acacia sp mt magnet",
      "acacia aneura intermedia",
      "acacia aff aneura",
      "acacia x aneura",
      "van-royena"
    )
  )
})

test_that("rehyphenate() joins only known hyphenated words", {
  words <- c("pes-caprae", "roseo-albus", "van-royena", "a-b-c")
  expect_equal(
    rehyphenate(
      c("ipomoea pes caprae brasiliensis", "ptilotus roseo albus", "van royena",
        "x a b c y", "acacia aneura", "pes", NA),
      words
    ),
    c("ipomoea pes-caprae brasiliensis", "ptilotus roseo-albus", "van-royena",
      "x a-b-c y", "acacia aneura", "pes", NA)
  )
  expect_equal(rehyphenate("ipomoea pes caprae", character(0)), "ipomoea pes caprae")
})

test_that("hyphenated genera and epithets align to the right names (#310)", {
  skip_if(is.null(resources), "Taxonomic resources could not be loaded")

  # the examples in #310; "fooensis" is a made-up infraspecific epithet, so each
  # name should fall back to its species, or to the genus
  out <- align_taxa(
    c("Van-royena",
      "Van-royena fooensis",
      "Alisma plantago-aquatica subsp. fooensis",
      "Acacia undulata-elegans var. fooensis",
      "Araucaria van-geertii subsp. fooensis"),
    resources = resources, quiet = TRUE
  )

  expect_equal(
    out$aligned_name,
    c("Van-royena sp. [Van-royena]",               # was the species Van-royena castanosperma
      "Van-royena sp. [Van-royena fooensis]",      # likewise
      "Alisma plantago-aquatica",
      "Acacia undulata-elegans",                   # was a different taxon, Acacia undulata var. elegans
      "Araucaria van-geertii")
  )
  expect_equal(out$taxon_rank, c("genus", "genus", "species", "species", "species"))
  expect_equal(
    out$alignment_code,
    c("match_12a_genus_exact_accepted",
      "match_12a_genus_exact_accepted",
      "match_10a_binomial_exact_accepted",
      "match_10b_binomial_exact_synonym",
      "match_10b_binomial_exact_synonym")
  )

  # the reference keys keep the hyphen too, so a hyphenated name has a binomial
  # of its own rather than sharing one with the unhyphenated name
  expect_true("alisma plantago-aquatica" %in% resources$APC_accepted$binomial)
  expect_true("van-royena castanosperma" %in% resources$APC_accepted$binomial)
  expect_false("van royena" %in% resources$APC_accepted$binomial)
})

test_that("exact hyphenated names still align to themselves", {
  skip_if(is.null(resources), "Taxonomic resources could not be loaded")

  x <- c("Van-royena castanosperma", "Duval-jouvea maritima", "Apera spica-venti",
         "Apera spica-ventii", "Acacia undulata-elegans", "Alisma plantago-aquatica")
  out <- align_taxa(x, resources = resources, quiet = TRUE)
  expect_equal(out$aligned_name, x)
  expect_equal(out$taxon_rank, rep("species", length(x)))
})

test_that("a hyphenated name written with spaces still aligns to it", {
  skip_if(is.null(resources), "Taxonomic resources could not be loaded")

  # Before #310 these matched the reference names because the hyphen was
  # stripped from both; the input now has its hyphen put back instead
  x <- c("Ipomoea pes caprae subsp. brasiliensis",
         "Van royena castanosperma",
         "Ptilotus roseo albus var. conglomeratus",  # not "Ptilotus roei"
         "Scandix pecten veneris subsp. pecten veneris",
         "Pyracantha crenato serrata",
         "Grevillea montis cole subsp. brevistyla",
         "Polyscias sp. Douglas Denison")
  out <- align_taxa(x, resources = resources, quiet = TRUE)
  expect_equal(
    out$aligned_name,
    c("Ipomoea pes-caprae subsp. brasiliensis",
      "Van-royena castanosperma",
      "Ptilotus roseo-albus var. conglomeratus",
      "Scandix pecten-veneris subsp. pecten-veneris",
      "Pyracantha crenato-serrata",
      "Grevillea montis-cole subsp. brevistyla",
      "Polyscias sp. Douglas-Denison (R.Schahinger HO526133)")
  )

  # the words of a hyphenated name are joined only where the reference names
  # never use them as two separate words
  expect_true("pes-caprae" %in% resources$hyphenated_words)
  expect_false(any(grepl(" ", resources$hyphenated_words)))
})

test_that("a hyphen after a rank marker or qualifier still separates words", {
  skip_if(is.null(resources), "Taxonomic resources could not be loaded")

  out <- align_taxa(c("Acacia sp.-Mt Magnet", "Acacia sp-Mt Magnet",
                      "Acacia aneura var.-intermedia", "Acacia aneura var-intermedia"),
                    resources = resources, quiet = TRUE)
  expect_equal(
    out$aligned_name,
    c("Acacia sp. Mt Magnet (T.McKenzie 5)", "Acacia sp. Mt Magnet (T.McKenzie 5)",
      "Acacia aneura var. intermedia", "Acacia aneura var. intermedia")
  )
})
