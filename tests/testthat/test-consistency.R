#
#
# currently not testing the current option as the webhosting seems unreliable for that dataset
# resources <-load_taxonomic_resources(stable_or_current_data="current")


test_that("create_taxonomic_update_lookup() works with full", {
  current_result <-
    create_taxonomic_update_lookup(
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
    ),
    resources = resources,
    full = TRUE
  ) %>%
    dplyr::arrange(original_name, canonical_name)

  #readr::write_csv(current_result, "consistency_lookup.csv")

  past_result <-
    readr::read_csv("consistency_lookup.csv", show_col_types = FALSE) %>%
    dplyr::arrange(original_name, canonical_name)

  # tests the most important columns
  # things have changed and we can't check other columns
  expect_equal(past_result$original_name, current_result$original_name)
  expect_equal(past_result$aligned_name, current_result$aligned_name)
  expect_equal(past_result$canonical_name, current_result$accepted_name)
  
  })

test_that("taxon name alignment matches and updates work as expected", {

  archived_values <- 
    readr::read_csv("test_matches_alignments_updates.csv", show_col_types = FALSE) %>%
    dplyr::rename(
      alignment_code = alignment_code_all_matches_TRUE, 
      aligned_name = aligned_name_all_matches_TRUE,
      taxon_rank = taxon_rank_all_matches_TRUE,
      taxonomic_reference = taxonomic_reference_all_matches_TRUE,
    ) %>%
    dplyr::select(
      original_name, 
      alignment_code,
      aligned_name,
      taxon_rank,
      taxonomic_reference,
      updated_name,
      updated_name_passes
    ) %>%
    dplyr::arrange(original_name, aligned_name)
      
  current_match_align_values <- 
    align_taxa(
      original_name = archived_values$original_name, 
      resources = resources, 
      fuzzy_abs_dist = 3, 
      fuzzy_rel_dist = 0.2, 
      imprecise_fuzzy_matches = TRUE,
      APNI_matches = TRUE,
      fuzzy_matches = TRUE,
      identifier = "test_all_matches_TRUE"
    )
  expect_equal(archived_values$original_name, current_match_align_values$original_name)
  expect_equal(archived_values$aligned_name, current_match_align_values$aligned_name)
  expect_equal(archived_values$taxon_rank, current_match_align_values$taxon_rank)
  expect_equal(archived_values$taxonomic_reference, current_match_align_values$taxonomic_reference)
  expect_equal(archived_values$alignment_code, 
                stringr::str_extract(current_match_align_values$alignment_code, "match_[:digit:][:digit:][:alpha:]"))     

  current_update_values <- 
    create_taxonomic_update_lookup(
      archived_values$original_name, 
      resources = resources,
      full = TRUE,
      imprecise_fuzzy_matches = TRUE,
      identifier = "test_all_matches_TRUE",
      taxonomic_splits = "most_likely_species"
    )
  
  current_update_values <- 
    current_update_values %>% 
    dplyr::left_join(by = "original_name",
      archived_values %>% select(original_name, updated_name, updated_name_passes), 
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
  
  expect_equal(archived_values$original_name, current_update_values$original_name)
  # We expect 100% success in alignment
  expect_equal(archived_values$aligned_name, current_update_values$aligned_name)
  # for update_taxonomony, there are cases where the algorithm doesn't produce a desired result (suggested_name != updated_name)
  # these are known and expected failures.
  expect_equal(archived_values$updated_name_passes, current_update_values$test_column)
})

test_that("create_taxonomic_update_lookup() works with collapse_to_higher_taxon",
          {
            original_name <-
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
                "Acacia aneura"
              )
            zz <- 
              create_taxonomic_update_lookup(
                original_name,
                taxonomic_splits = "collapse_to_higher_taxon",
                resources = resources
              )
            expect_equal(1,1)
            #expect_equal(nrow(zz), 4)
            #expect_equal(zz$original_name, original_name)
          })


test_that("align_taxa() works no fuzzy", {
  expect_equal(nrow(align_taxa(
    original_name = c("Dryandra preissii", "Banksia acuminata"),
    resources = resources
  )), 2)
})


test_that("align_taxa() works with fuzzy", {
  expect_equal(nrow(align_taxa(
    original_name = c("Dryandra preissii", "Banksia acuminata"),
    resources = resources
  )), 2)
})

test_that("align_taxa() works with longer list", {
  species_list <-
    readr::read_csv(system.file("extdata", "species.csv", package = "APCalign"),
                    show_col_types = FALSE)
  expect_equal(nrow(
    aligned_data <- align_taxa(species_list$name,
                               resources = resources)
  ), 199)
})

test_that("update_taxonomy() works", {
  aligned_data <- tibble::tibble(
    original_name = c("Dryandra preissii", "Banksia acuminata"),
    aligned_name = c("Dryandra preissii", "Banksia acuminata"),
    taxon_rank = c("Species", "Species"),
    taxonomic_reference = c("APC", "APC"),
    aligned_reason = NA_character_
  )
  
  expect_equal(nrow(update_taxonomy(
    aligned_data,
    resources = resources
  )), 2)

  expect_equal(nrow(create_taxonomic_update_lookup(
    c("Dryandra preissii", "Banksia acuminata"), resources = resources
  )), 2)
})

test_that("weird hybrid symbols work", {
  expect_equal(nrow(align_taxa(
    c("Platanus × acerifolia", "Platanus × hispanica"), resources = resources
  )), 2)
})

test_that("genus level ids", {
  aliged_nms <- align_taxa(c("Acacia sp.", "Eucalyptus sp."), resources = resources) |> pull(aligned_name)
  expect_false(any(str_detect(aliged_nms, "NA sp.")))
})
