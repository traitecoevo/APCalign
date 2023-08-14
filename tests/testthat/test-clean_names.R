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

test_that("handles weird strings", {
  test_strings <- c("", "''", "'", "          ", "\t", "\n", "stuff with      ",
                    "test'string'withquotes", 
                    "!@#$%^&*()_+", 
                    rep("abc", times=10000),
                    "print('whoops no cleaning')",
                    "Doesthislook likeaspeciesi",
                    "Doesn'tlook likeaspeciesi",
                    "Banksia  serrata"
  )
  expect_equal(nrow(create_taxonomic_update_lookup(test_strings,resources=resources)),1)
})


  test_that("handles APNI taxa and genus level IDs",{
    zz<-create_taxonomic_update_lookup(c("Acacia sp.", "Dendropanax amplifolius",
                                        "Acanthopanax divaricatum", "Eucalyptus sp."), resources=resources)
    expect_gte(nrow(zz), 2)
  })


test_that("returns same number of rows as input, even with duplicates", {
  
  original_name <-
    c("Dryandra preissii", "Banksia acuminata", 
      "Doesthislook likeaspeciesi", "Doesthislook likeaspeciesi", 
      "Banksia acuminata", "Banksia acuminata", "Hibbertia sericea")
  
  x1 <- align_taxa(
    original_name = original_name,
    resources = resources)

  x2 <-
    update_taxonomy(x1$aligned_name, resources = resources)

  # Nas missing in select_taxonomy_options
  x3 <-
    select_taxonomy_options(x1, resources = resources, one_to_many = "most_likely_species")

  x4 <- create_taxonomic_update_lookup(
    taxa = original_name,
    resources = resources, one_to_many = "most_likely_species")

  # output should be same order and length as input
  expect_equal(x1$original_name, original_name)
  # x3 should be in same order but may have more rows
  expect_equal(subset(x2$aligned_name, !duplicated(x2$aligned_name)), subset(x1$aligned_name, !duplicated(x1$aligned_name)))
  expect_gte(length(x2$aligned_name), length(x1$aligned_name))
  # output should be same order and length as input
  # Nas missing in select_taxonomy_options
  expect_equal(x3$aligned_name, x1$aligned_name)

  expect_equal(x4$original_name, original_name)
})
