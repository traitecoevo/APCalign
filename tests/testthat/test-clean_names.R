# currently not testing the current option as the webhosting seems unreliable for that dataset
# resources_current<-load_taxonomic_resources(stable_or_current_data="current")


test_that("create_taxonomic_update_lookup() works with full", {
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
  ) -> zz
  expect_gte(nrow(zz), 80)
})

test_that("create_taxonomic_update_lookup() works with collapse_to_higher_taxon",
          {
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
                "Acacia aneura"
              ),
              one_to_many = "collapse_to_higher_taxon",
              resources = resources
            ) -> zz
            expect_gte(nrow(zz), 11)
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


test_that("handles NAs", {
  expect_gte(nrow(align_taxa(c(
    "Acacia aneura", NA
  ), resources = resources)), 0)
  expect_gte(nrow(create_taxonomic_update_lookup(c(
    "Acacia aneura", NA
  ), resources = resources)), 0)
})
