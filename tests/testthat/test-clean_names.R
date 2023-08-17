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
              taxonomic_splits = "collapse_to_higher_taxon",
              resources = resources
            ) -> zz
            expect_gte(nrow(zz), 11)
          })


test_that("align_taxa() works - no/with fuzzy", {
  
  original_name = c("Dryandra preissii", "Banksia acuminata")
  
  out1 <- align_taxa(original_name, resources = resources, fuzzy_matches = TRUE)
  out2 <- align_taxa(original_name, resources = resources, fuzzy_matches = FALSE)

  expect_equal(original_name, out1$original_name)
  expect_equal(original_name, out2$original_name)
})


test_that("align_taxa() works with longer list", {
  species_list <-
    readr::read_csv(system.file("extdata", "species.csv", package = "APCalign"),
                    show_col_types = FALSE)
  aligned_data <- align_taxa(species_list$name, resources = resources)
  
  expect_equal(nrow(aligned_data), 199)
  expect_equal(species_list$name, aligned_data$original_name)
})

test_that("update_taxonomy() works", {
  expect_equal(nrow(update_taxonomy(
    aligned_names = c("Dryandra preissii", "Banksia acuminata"),
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

test_that("handles NAs", {
  original_name <- c("Acacia aneura", NA)

  out <- align_taxa(original_name, resources = resources)

  expect_equal(original_name, original_name)
  expect_gte(nrow(out), 0)

  out <- create_taxonomic_update_lookup(original_name, resources = resources)
  expect_gte(nrow(out), 0)
})


test_that("handles weird strings", {
  test_strings <- c("", "''", "'", "          ", "\t", "\n", "stuff with      ",
                    "test'string'withquotes", 
                    "!@#$%^&*()_+", 
                    rep("abc", times= 10),
                    "print('whoops no cleaning')",
                    "Doesthislook likeaspeciesi",
                    "Doesn'tlook likeaspeciesi",
                    "Banksia  serrata"
  )

  out <- align_taxa(test_strings, resources = resources)
  expect_equal(test_strings, out$original_name)

  out <- create_taxonomic_update_lookup(test_strings, taxonomic_splits = "most_likely_species",
    resources = resources)
  expect_equal(test_strings, out$original_name)
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

  x4 <- create_taxonomic_update_lookup(
    taxa = original_name,
    resources = resources, taxonomic_splits = "most_likely_species")

  # output should be same order and length as input
  expect_equal(x1$original_name, original_name)
  # x2 should be in same order but may have more rows
  expect_equal(subset(x2$aligned_name, !duplicated(x2$aligned_name)), subset(x1$aligned_name, !duplicated(x1$aligned_name)))
  expect_gte(length(x2$aligned_name), length(x1$aligned_name))

  expect_equal(x4$original_name, original_name)
})
