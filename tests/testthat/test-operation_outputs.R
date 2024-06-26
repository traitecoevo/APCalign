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
  rows_end_sp <- out3 %>% dplyr::filter(stringr::str_detect(suggested_name, "sp."))
  rows_alt_names <- out3 %>% dplyr::filter(stringr::str_detect(suggested_name, "collapsed names:"))
  
  
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
