# These tests pin the behaviour of the match_taxa() alignment branches so that
# internal refactoring of match_taxa() can be verified as behaviour-preserving.
#
# - The dark-branch tests assert the seven match branches that the main
#   benchmark (test_matches_alignments_updates.csv) historically did not reach.
# - The snapshot locks the full alignment contract (branch + result + reason)
#   across every input in the benchmark, which now covers all match branches.
#
# `resources` is supplied by helper.R.

# Inputs constructed to land on the branches that no other benchmark exercises.
dark_branch_inputs <- c(
  "match_03e_intergrade_unknown_genus"      = "Xqzztia aaa -- bbb",
  "match_04e_indecision_unknown_genus"      = "Xqzztia aaa / bbb",
  "match_06e_species_affinis_unknown_genus" = "Xqzztia aff. bbb",
  "match_08e_hybrid_taxon_unknown"          = "Xqzztia x bbb",
  "match_12g_genus_fuzzy_synonym"           = "Enceephalartos xqzztii",
  "match_12h_family_fuzzy_accepted"         = "Zamiaceaa",
  "match_12i_family_fuzzy_synonym"          = "Boweniaceaa"
)

test_that("previously-untested match branches resolve as expected", {

  out <- align_taxa(
    original_name = unname(dark_branch_inputs),
    resources = resources,
    full = TRUE,
    imprecise_fuzzy_matches = TRUE,
    APNI_matches = TRUE,
    fuzzy_matches = TRUE,
    identifier = "test_all_matches_TRUE",
    quiet = TRUE
  )

  # each input lands on its intended branch
  expect_equal(out$alignment_code, names(dark_branch_inputs))

  # the four "unknown genus" fall-throughs cannot be aligned
  unknown <- out$alignment_code %in% c(
    "match_03e_intergrade_unknown_genus",
    "match_04e_indecision_unknown_genus",
    "match_06e_species_affinis_unknown_genus",
    "match_08e_hybrid_taxon_unknown"
  )
  expect_true(all(is.na(out$aligned_name[unknown])))

  # the three fuzzy genus/family branches do produce an aligned name
  expect_false(any(is.na(out$aligned_name[!unknown])))
})

test_that("every aligned_reason is well-formed (ends with a parenthesised date)", {
  # Guards against the copy-paste class of bug where a branch's reason string
  # omits the ' (' before the appended Sys.Date(), e.g. '...genus-rank2026-01-01)'.
  benchmarks <- readr::read_csv(
    "benchmarks/test_matches_alignments_updates.csv", show_col_types = FALSE
  )

  out <- align_taxa(
    original_name = benchmarks$original_name,
    resources = resources,
    full = TRUE,
    imprecise_fuzzy_matches = TRUE,
    APNI_matches = TRUE,
    fuzzy_matches = TRUE,
    identifier = "test_all_matches_TRUE",
    quiet = TRUE
  )

  reasons <- out$aligned_reason[!is.na(out$aligned_reason)]
  malformed <- reasons[!stringr::str_detect(reasons, " \\([0-9]{4}-[0-9]{2}-[0-9]{2}\\)$")]
  expect_equal(malformed, character(0))
})

test_that("full alignment output is stable across all match branches (snapshot)", {
  benchmarks <- readr::read_csv(
    "benchmarks/test_matches_alignments_updates.csv", show_col_types = FALSE
  )

  out <- align_taxa(
    original_name = benchmarks$original_name,
    resources = resources,
    full = TRUE,
    fuzzy_abs_dist = 3,
    fuzzy_rel_dist = 0.2,
    imprecise_fuzzy_matches = TRUE,
    APNI_matches = TRUE,
    fuzzy_matches = TRUE,
    identifier = "test_all_matches_TRUE",
    quiet = TRUE
  )

  # Pin the alignment contract: which branch fired, the result, and the reason.
  # Normalise the embedded run-date so the snapshot is stable over time.
  contract <- out %>%
    dplyr::transmute(
      original_name,
      cleaned_name,
      aligned_name,
      taxonomic_dataset,
      taxon_rank,
      alignment_code,
      aligned_reason = stringr::str_replace(
        aligned_reason, "\\([0-9]{4}-[0-9]{2}-[0-9]{2}\\)$", "(DATE)"
      )
    ) %>%
    dplyr::arrange(original_name, aligned_name, alignment_code)

  expect_snapshot_value(contract, style = "json2")
})
