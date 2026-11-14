# The tests in this file are primarily designed to check that the results of the
# taxonomy updating give expected results. 
# Limited attention is given to checking all possible ways of running a function
# We have established spreadsheets with benchmarks of expected ouput

test_that("consistency with previous runs", {
  
  # Check that results are consistent through time

  taxa <-
    c(
      "Banksia integrifolia",
      "Acacia longifolia",
      "Commersonia rosea",
      "Thelymitra pauciflora",
      "Justicia procumbens",
      "Hibbertia stricta",
      "Rostellularia adscendens",
      "Hibbertia sericea",
      "Hibbertia sp.",
      "Athrotaxis laxiflolia",
      "Genoplesium insigne",
      "Polypogon viridis",
      "Acacia aneura",
      "Acacia paraneura",
      "Galactia striata"
    )
  
  output <-
    create_taxonomic_update_lookup(
      taxa,
      resources = resources,
      full = TRUE,
      taxonomic_splits = "return_all", 
      quiet = TRUE
    ) %>%
    dplyr::arrange(original_name, accepted_name)

  #readr::write_csv(output, "consistency_lookup.csv")

  past_result <-
    readr::read_csv("benchmarks/consistency_lookup.csv", show_col_types = FALSE) %>%
    dplyr::arrange(original_name, canonical_name) %>%
    dplyr::rename(accepted_name = canonical_name) %>%
    dplyr::distinct(aligned_name, accepted_name_usage_ID, .keep_all = TRUE)

  # tests the most important columns
  # other cols changed so we can't check other columns
  v <-c("original_name", "aligned_name", "accepted_name")
  expect_equal(past_result[,v], output[,v])
  })

test_that("taxon name splits and complex taxonomic status values work as expected", {
  # Compare results to a table of values that have been closely scrutinised
  benchmarks <- 
    readr::read_csv("benchmarks/test_splits_synonyms.csv", show_col_types = FALSE) %>%
    dplyr::arrange(original_name, accepted_name_usage_ID, taxonomic_status)
  
  out1 <-
    create_taxonomic_update_lookup(
      benchmarks$original_name,
      taxonomic_splits = "most_likely_species",
      resources = resources,
      full = TRUE, 
      quiet = TRUE
      ) %>%
      dplyr::arrange(original_name, taxon_ID, taxonomic_status)
  
  expect_equal(benchmarks$original_name, out1$original_name)
  #expect_equal(benchmarks$accepted_name_usage_ID, out1$taxon_ID)
  #todo: include test that confirms taxonomic_status in benchmarks is present (str_detect) in either out1$taxonomic_status or out1$alternative_taxonomic_status_aligned
  
  out2 <-
    create_taxonomic_update_lookup(
      benchmarks$original_name,
      taxonomic_splits = "return_all",
      resources = resources,
      full = TRUE, 
      quiet = TRUE
      ) %>%
      dplyr::arrange(original_name, taxon_ID, taxonomic_status)
  
  expect_gte(nrow(out2), 60)
  expect_contains(out2$original_name, benchmarks$original_name)
  expect_contains(out2$accepted_name, out1$accepted_name)
  
  out3 <-
    create_taxonomic_update_lookup(
      benchmarks$original_name,
      taxonomic_splits = "collapse_to_higher_taxon",
      resources = resources,
      full = TRUE, 
      quiet = TRUE) %>%
    dplyr::arrange(original_name, taxon_ID, taxonomic_status) %>%
    dplyr::mutate(number_of_collapsed_taxa = ifelse(is.na(number_of_collapsed_taxa), 1, number_of_collapsed_taxa))
  
  rows_gt_1 <- out3 %>% dplyr::filter(number_of_collapsed_taxa > 1)
  rows_end_sp <- out3 %>% dplyr::filter(grepl("sp.", suggested_name))
  rows_alt_names <- out3 %>% dplyr::filter(grepl("collapsed names:", suggested_name))
  
  
  expect_equal(nrow(out1), nrow(out3))
  #will be less (slightly) because `return_all` excludes misapplied and excluded taxa
  expect_equal(nrow(out2), sum(out3$number_of_collapsed_taxa)-2)
  expect_equal(nrow(rows_gt_1), nrow(rows_end_sp))
  expect_equal(nrow(rows_gt_1), nrow(rows_alt_names))
  
  out4 <-
    create_taxonomic_update_lookup(
      benchmarks$original_name,
      resources = resources,
      fuzzy_matches = FALSE,
      full = TRUE, 
      quiet = TRUE) %>%
    dplyr::arrange(original_name, taxon_ID, taxonomic_status)
  
  expect_equal(out1, out4)
  
  
  })

test_that("taxon name alignment matches and updates work as expected", {

  # Compare results to a table of values that have been closely scrutinised
  
  benchmarks <- 
    readr::read_csv("benchmarks/test_matches_alignments_updates.csv", show_col_types = FALSE) %>%
    dplyr::rename(
      alignment_code = alignment_code_all_matches_TRUE, 
      aligned_name = aligned_name_all_matches_TRUE,
      taxon_rank = taxon_rank_all_matches_TRUE,
      taxonomic_dataset = taxonomic_dataset_all_matches_TRUE,
    ) %>%
    dplyr::select(
      original_name, 
      alignment_code,
      aligned_name,
      taxon_rank,
      taxonomic_dataset,
      updated_name,
      updated_name_passes
    ) %>%
    dplyr::arrange(original_name, aligned_name)
      
  output_align <- 
    align_taxa(
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
  
  expect_equal(benchmarks$original_name,  output_align$original_name)
  expect_equal(benchmarks$aligned_name,   output_align$aligned_name)
  expect_equal(benchmarks$taxon_rank,     output_align$taxon_rank)
  expect_equal(benchmarks$taxonomic_dataset, output_align$taxonomic_dataset)
  # alignment_code pins which match branch each name resolves through;
  # the benchmark now covers all match branches, including the genus/family
  # fuzzy and "unknown genus" fall-throughs.
  expect_equal(benchmarks$alignment_code, output_align$alignment_code)

  # chars_changed: 0 for every exact match, NA when nothing was aligned, and a
  # count for every fuzzy step. Every fuzzy step in the benchmark must be listed
  # in `fuzzy_match_steps`, and everything listed there must be a fuzzy step.
  aligned <- !is.na(output_align$aligned_name)
  fuzzy <- grepl("fuzzy", output_align$alignment_code)
  expect_true(all(is.na(output_align$chars_changed[!aligned])))
  expect_true(all(output_align$chars_changed[aligned & !fuzzy] == 0L))
  expect_false(anyNA(output_align$chars_changed[aligned & fuzzy]))
  expect_setequal(
    names(fuzzy_match_steps),
    unique(substr(output_align$alignment_code[fuzzy], 1, 9))
  )

  output_updates <- 
    update_taxonomy(
      output_align, 
      resources = resources,
      taxonomic_splits = "most_likely_species"
    )
  
  output_updates <- 
    output_updates %>% 
    dplyr::left_join(by = "original_name",
      benchmarks %>% dplyr::select(original_name, updated_name, updated_name_passes), 
    ) %>% 
    # Make a logical to see if the suggested name matches the updated_name in the spreadsheet
    # We don't expect all of these to match perfectly. 
    # The column `updated_name_passes` has our expectation on whether the match works, 
    # and is used below for the test
    dplyr::mutate(
      test_column = ifelse(suggested_name == updated_name, TRUE, FALSE),
      test_column = ifelse(is.na(suggested_name) & is.na(updated_name), TRUE, test_column),
      test_column = ifelse(is.na(test_column), FALSE, test_column)
    )

  expect_equal(benchmarks$original_name, output_updates$original_name)
  # We expect 100% success in alignment
  expect_equal(benchmarks$aligned_name, output_updates$aligned_name)
  # for update_taxonomy, there are cases where the algorithm doesn't produce a desired result (suggested_name != updated_name)
  # these are known and expected failures.
  expect_equal(benchmarks$updated_name_passes, output_updates$test_column)
}
)
  
test_that("fuzzy_match works as expected when n_allowed > 1", {
    
  expect_length(
    fuzzy_match(
      txt = "Danksia",
      accepted_list = resources$genera_all$canonical_name,
      max_distance_abs = 4, 
      max_distance_rel = 0.4, 
      n_allowed = 4,
      epithet_letters = 1
    ),
    1
  )
  
  expect_length(
    fuzzy_match(
      txt = "Aucalyptus",
      accepted_list = resources$genera_all$canonical_name,
      max_distance_abs = 4, 
      max_distance_rel = 0.4, 
      n_allowed = 4,
      epithet_letters = 1
    ),
    2
  ) 
}
)

test_that("identifier column works when mismatch between unique taxa and unique identifiers", {
  taxa <-
    c(
      "Banksia integrifolia",
      "Acacia longifolia",
      "Commersonia rosea",
      "Thelymitra pauciflora",
      "Justicia procumbens",
      "Hibbertia stricta",
      "Rostellularia adscendens",
      "Hibbertia sericea",
      "Hibbertia sp.",
      "Athrotaxis laxiflolia",
      "Genoplesium insigne",
      "Polypogon viridis",
      "Acacia aneura",
      "Acacia paraneura",
      "Galactia striata",
      "Acacia sp.",
      "Acacia sp.",
      "Acacia sp.",
      "Acacia sp."
    )
  
  identifiers <-
    c(
      "message_01",
      "message_02",
      "message_03",
      "message_04",
      "message_05",
      "message_06",
      "message_07",
      "message_08",
      "message_09",
      "message_10",
      "message_11",
      "message_12",
      "message_13",
      "message_14",
      "message_15",
      "message_16",
      "message_17",
      "message_18",
      "message_19"
    )
  
  output <-
    align_taxa(
      original_name = taxa,
      identifier = identifiers,
      resources = resources,
      full = TRUE,
      quiet = TRUE
    )
  
  expect_length(
    output$aligned_name, 19
  )
}  
  
)

test_that("No warnings if trying to match input name to empty accepted name set.", {
  
 expect_equal(
    fuzzy_match(
      txt = "Kallstroemie",
      accepted_list = resources$family_synonym$canonical_name,
      max_distance_abs = 2, 
      max_distance_rel = 0.3, 
      n_allowed = 1,
      epithet_letters = 1
    ), NA)
}
)

test_that("synonyms_for_accepted_names outputs expected number of rows", {
  
  expect_silent(
    synonyms_T <- synonyms_for_accepted_names(
      accepted_names = c("Justicia tenella", "Acacia aneura"), 
      collapse = TRUE, resources = resources
    )
  )

  expect_silent(
    synonyms_F <- synonyms_for_accepted_names(
      accepted_names = c("Justicia tenella", "Acacia aneura", "Cardamine gunnii"), 
      collapse = FALSE, resources = resources
    )
  )
  
  expect_equal(
    nrow(synonyms_T),
    2
  )
  
  expect_silent(
    synonyms_F <- synonyms_for_accepted_names(
      accepted_names = c("Justicia tenella", "Acacia aneura", "Cardamine gunnii"), 
      collapse = FALSE, resources = resources
    )
  )
  
  # currently there are 15 rows, but this can increase with additional synonyms being added
  expect_gte(
    nrow(synonyms_F),
    14
  )
  
  expect_gte(
    nrow(synonyms_F),
    nrow(synonyms_T)
  )
  
  expect_contains(synonyms_F$synonym, "Racosperma aneurum")
  expect_contains(synonyms_F$synonym, "Cardamine heterophylla")
  expect_no_error(grepl("Racosperma aneurum", synonyms_T$synonyms))
  expect_no_error(grepl("Cardamine heterophylla", synonyms_T$synonyms))
  
  
  expect_equal(
    names(synonyms_T), 
    c("family", "accepted_name", "synonyms", "scientific_name", "accepted_name_usage_ID"))

  expect_equal(
    names(synonyms_F), 
    c("family", "accepted_name", "synonym", "taxonomic_status", "scientific_name", "accepted_name_usage_ID", "taxon_ID"))

  expect_equal(
    nrow(synonyms_F),
    length(unique(synonyms_F$taxon_ID))
  )
  
  expect_equal(
    length(unique(synonyms_F$synonym)),
    length(unique(synonyms_F$taxon_ID))
  )
  
  expect_equal(
    length(unique(synonyms_F$accepted_name)),
    length(unique(synonyms_F$accepted_name_usage_ID))
  )
  
}
)

test_that("chars_changed counts the characters a fuzzy match changed", {
  x <- c("Acacia aneura", "eucalyptus regnans", "Acacia aneuraa", "Acaica aneura",
         "Eucalytpus sp.", "Eucalyptus regnans subsp. regnanss", "Xyzzyplant foo")
  out <- align_taxa(x, resources = resources, quiet = TRUE)
  # exact (case is standardised before matching), one added letter, two
  # swapped letters (one change), a genus-only fuzzy match, a rank marker that
  # differs (removed before matching) plus one added letter, and no match
  expect_equal(out$chars_changed, c(0L, 0L, 1L, 1L, 1L, 1L, NA))

  lookup <- create_taxonomic_update_lookup(x, resources = resources, quiet = TRUE)
  expect_equal(lookup$chars_changed, out$chars_changed)
})

test_that("chars_changed survives a rerun from a cached output file", {
  # Rows read back from `output` carry no alignment_code; they must keep the
  # value they were saved with, not be counted as exact matches.
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f))
  x <- c("Banksia serrrata", "Acacia aneuraa", "Eucalyptus regnans")
  first <- create_taxonomic_update_lookup(x, resources = resources, quiet = TRUE, output = f)
  expect_equal(first$chars_changed, c(1L, 1L, 0L))

  # some names cached, one new
  second <- create_taxonomic_update_lookup(c(x, "Acacia dealbata"), resources = resources, quiet = TRUE, output = f)
  expect_equal(second$chars_changed, c(1L, 1L, 0L, 0L))

  # every name cached (align_taxa() returns early)
  third <- create_taxonomic_update_lookup(c(x, "Acacia dealbata"), resources = resources, quiet = TRUE, output = f)
  expect_identical(third$chars_changed, c(1L, 1L, 0L, 0L))
})
