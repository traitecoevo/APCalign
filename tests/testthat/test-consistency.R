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
#    dplyr::select(-aligned_reason) %>% #because this has a date in it
    dplyr::arrange(original_name, canonical_name)

  #readr::write_csv(current_result, "consistency_lookup.csv")

  past_result <-
    readr::read_csv("consistency_lookup.csv", show_col_types = FALSE) %>%
    dplyr::select(-aligned_reason) %>% #because this has a date in it
    dplyr::arrange(original_name, canonical_name)

  expect_equal(past_result$original_name, current_result$original_name)
  expect_equal(names(past_result), names(current_result))
  expect_equal(past_result, current_result)
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
                one_to_many = "collapse_to_higher_taxon",
                resources = resources
              )
            expect_equal(nrow(zz), 13)
            expect_equal(zz$original_name, original_name)
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
  expect_equal(nrow(update_taxonomy(
    aligned_names = c("Dryandra preissii", "Banksia acuminata"),
    resources = resources
  )), 2)
})

test_that("update_taxonomy() works", {
  expect_equal(nrow(create_taxonomic_update_lookup(
    c("Dryandra preissii", "Banksia acuminata"), resources = resources
  )), 2)
})

test_that("weird hybrid symbols work", {
  expect_equal(nrow(align_taxa(
    c("Platanus × acerifolia", "Platanus × hispanica"), resources = resources
  )), 2)
})

test_that("handles NAs", {
  expect_gte(nrow(align_taxa(c(
    "Acacia aneura", NA
  ), resources = resources)), 0)
  expect_gte(nrow(create_taxonomic_update_lookup(c(
    "Acacia aneura", NA
  ), resources = resources)), 0)
})

test_that("genus level ids", {
  aliged_nms <- align_taxa(c("Acacia sp.", "Eucalyptus sp."), resources = resources) |> pull(aligned_name)
  expect_false(any(str_detect(aliged_nms, "NA sp.")))
})
