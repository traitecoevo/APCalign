skip_if_no_resources()

test_that("fuzzy_match() rejects an affinis/hybrid qualifier masquerading as an ordinary word", {

  # Regression test for #291/#294: "Acacia aff. aneura" ("resembles A. aneura, not
  # confidently identified") used to fuzzy-match the unrelated accepted species
  # "Acacia aptaneura", because check_match()'s word-position check only compared
  # as many words as the shorter of query/candidate had -- so "aff" (query word 2)
  # and "aptaneura" (candidate word 2) were compared by first letter alone ("a" ==
  # "a"), and the real epithet "aneura" (query word 3) was never checked at all.
  expect_equal(
    fuzzy_match(
      txt = "acacia aff aneura",
      accepted_list = resources$APC_accepted$stripped_canonical,
      max_distance_abs = 3, max_distance_rel = 0.2, n_allowed = 1
    ),
    NA
  )

  # Same shape of bug for the hybrid marker "x": "banksia x integrifolia" is only
  # 2 edits from the real accepted "banksia integrifolia" (delete the marker word,
  # no letter substitutions needed), and would confirm for any genus/epithet pair
  # where "x" happens to share a first letter with the epithet (real accepted
  # epithets starting with "x" do exist, e.g. "Boronia xanthastrum").
  expect_equal(
    fuzzy_match(
      txt = "banksia x integrifolia",
      accepted_list = resources$APC_accepted$stripped_canonical,
      max_distance_abs = 3, max_distance_rel = 0.2, n_allowed = 1
    ),
    NA
  )

  # A qualifier token must still confirm a match when the *candidate* carries it
  # too -- e.g. a typo'd version of the real APC-known synonym
  # "Isoetes sp. aff. muelleri" (an informally affinis-qualified name in its own
  # right, not just uncertain-ID notation on an otherwise plain species).
  expect_equal(
    fuzzy_match(
      txt = "isoetes sp aff muellerii",
      accepted_list = resources$APC_synonyms$stripped_canonical,
      max_distance_abs = 3, max_distance_rel = 0.2, n_allowed = 1
    ),
    "isoetes sp aff muelleri"
  )

  # Likewise for a typo'd real named hybrid ("x" is a hybrid marker embedded in a
  # genuinely accepted canonical name here, present on both sides of the match).
  expect_equal(
    fuzzy_match(
      txt = "persoonia x luciida",
      accepted_list = resources$APC_accepted$stripped_canonical,
      max_distance_abs = 3, max_distance_rel = 0.2, n_allowed = 1
    ),
    "persoonia x lucida"
  )
})

test_that("align_taxa() caps an affinis-qualified name at genus rank instead of a wrong species match", {

  # End-to-end version of the fuzzy_match() regression above: before the fix,
  # match_05a (species-level fuzzy match) claimed this row before match_06 (the
  # dedicated aff./cf. handling) ever got a chance to see it.
  out <- align_taxa("Acacia aff. aneura", resources = resources, quiet = TRUE, full = TRUE)

  expect_equal(out$aligned_name, "Acacia sp. [Acacia aff. aneura]")
  expect_equal(out$taxon_rank, "genus")
  expect_equal(out$alignment_code, "match_06a_species_affinis_APC_exact")
})

test_that("a name with no letter does not stop the lookup", {
  # One such name used to stop the whole batch with "missing value where
  # TRUE/FALSE needed" in fuzzy_match().
  expect_true(is.na(fuzzy_match("12 34", resources$genera_accepted$genus, 2, 0.35)))
  odd <- c("12 34", "1 1/2", "\u00bd", "\u00b1 Acacia")
  out <- expect_no_error(
    create_taxonomic_update_lookup(c(odd, "Acacia aneura"), resources = resources)
  )
  expect_equal(nrow(out), 5)
  expect_equal(out$original_name, c(odd, "Acacia aneura"))
  expect_equal(out$aligned_name[5], "Acacia aneura")
})

test_that("a word with no letter or digit after the genus does not stop the lookup", {
  # "+ ~ $ ^ |" are symbols, not punctuation, so strip_names() keeps them; the
  # word's first letter is NA, and comparing it used to stop the whole batch.
  expect_true(is.na(fuzzy_match("acacia --", "acacia ab", 5, 1)))
  expect_true(is.na(fuzzy_match("acacia b", "acacia", 2, 0.35)))
  odd <- c("Acacia + aneura", "Eucalyptus |", "Eucalyptus ~", "Acacia \u2297 aneura")
  out <- expect_no_error(
    create_taxonomic_update_lookup(c(odd, "Acacia aneura"), resources = resources)
  )
  expect_equal(out$original_name, c(odd, "Acacia aneura"))
  expect_equal(out$aligned_name[5], "Acacia aneura")
})
