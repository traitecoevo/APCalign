# Tests for the structure (names and column names) of loaded taxonomic resources

test_that("taxonomic resources contains expected tables", {
  skip_on_cran()

  expect_setequal(
    names(resources),
    c("APC", "APNI", "APC_accepted", "APC_synonyms", "APNI_names",
      "genera_accepted", "genera_synonym", "genera_APNI", "genera_all",
      "family_accepted", "family_synonym")
  )
})

test_that("APC_accepted has expected columns", {
  skip_on_cran()

  expect_setequal(
    names(resources$APC_accepted),
    c("canonical_name", "scientific_name", "taxonomic_status", "taxon_ID",
      "scientific_name_ID", "accepted_name_usage_ID", "name_type", "taxon_rank",
      "family", "genus", "stripped_canonical", "stripped_canonical2",
      "stripped_scientific", "binomial", "trinomial", "taxonomic_dataset")
  )
})

test_that("APC_synonyms has expected columns", {
  skip_on_cran()

  expect_setequal(
    names(resources$APC_synonyms),
    names(resources$APC_accepted)
  )
})

test_that("APNI_names has expected columns", {
  skip_on_cran()

  expect_setequal(
    names(resources[["APNI_names"]]),
    c("canonical_name", "scientific_name", "scientific_name_ID", "name_type",
      "taxon_rank", "taxonomic_status", "stripped_canonical", "stripped_canonical2",
      "stripped_scientific", "binomial", "trinomial", "genus", "taxonomic_dataset")
  )
})

test_that("genera_accepted has expected columns", {
  skip_on_cran()

  expect_setequal(
    names(resources$genera_accepted),
    c("canonical_name", "accepted_name_usage", "accepted_name_usage_ID",
      "scientific_name", "taxonomic_status", "taxon_ID", "scientific_name_ID",
      "name_type", "taxon_rank", "genus", "taxonomic_dataset")
  )
})

test_that("genera_synonym has expected columns", {
  skip_on_cran()

  expect_setequal(
    names(resources$genera_synonym),
    names(resources$genera_accepted)
  )
})

test_that("genera_APNI has expected columns", {
  skip_on_cran()

  expect_setequal(
    names(resources$genera_APNI),
    c("canonical_name", "scientific_name", "taxonomic_status", "scientific_name_ID",
      "name_type", "taxon_rank", "genus", "taxonomic_dataset")
  )
})

test_that("genera_all has expected columns", {
  skip_on_cran()

  expect_setequal(
    names(resources$genera_all),
    c("canonical_name", "accepted_name_usage", "accepted_name_usage_ID",
      "scientific_name", "taxonomic_status", "taxon_ID", "scientific_name_ID",
      "name_type", "taxon_rank", "genus", "taxonomic_dataset", "cleaned_name")
  )
})

test_that("family_synonym has expected columns", {
  skip_on_cran()

  expect_setequal(
    names(resources$family_synonym),
    c("canonical_name", "accepted_name_usage", "accepted_name_usage_ID",
      "scientific_name", "taxonomic_status", "taxon_ID", "scientific_name_ID",
      "name_type", "taxon_rank", "genus", "taxonomic_dataset")
  )
})
