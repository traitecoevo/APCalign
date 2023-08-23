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
      full = TRUE
    ) %>%
    dplyr::arrange(original_name, canonical_name)

  #readr::write_csv(output, "consistency_lookup.csv")

  past_result <-
    readr::read_csv("benchmarks/consistency_lookup.csv", show_col_types = FALSE) %>%
    dplyr::arrange(original_name, canonical_name) %>%
    dplyr::rename(accepted_name = canonical_name)

  # tests the most important columns
  # other cols changed so we can't check other columns
  v <-c("original_name", "aligned_name", "accepted_name")
  expect_equal(past_result[,v], output[,v])
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
      identifier = "test_all_matches_TRUE"
    )
  
  expect_equal(benchmarks$original_name,  output_align$original_name)
  expect_equal(benchmarks$aligned_name,   output_align$aligned_name)
  expect_equal(benchmarks$taxon_rank,     output_align$taxon_rank)
  expect_equal(benchmarks$taxonomic_dataset, output_align$taxonomic_dataset)
  expect_equal(benchmarks$alignment_code, 
                stringr::str_extract(output_align$alignment_code, "match_[:digit:][:digit:][:alpha:]"))     


  output_updates <- 
    update_taxonomy(
      output_align, 
      resources = resources,
      taxonomic_splits = "most_likely_species"
    )
  
  output_updates <- 
    output_updates %>% 
    dplyr::left_join(by = "original_name",
      benchmarks %>% select(original_name, updated_name, updated_name_passes), 
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
  # for update_taxonomony, there are cases where the algorithm doesn't produce a desired result (suggested_name != updated_name)
  # these are known and expected failures.
  expect_equal(benchmarks$updated_name_passes, output_updates$test_column)
  })


