# ss<-create_species_state_origin_matrix(resources=resources)
#
# ss %>%
#   slice(sample(1:n(),1000) %>%sort()) %>%
#   write_csv("state_diversity.csv")

test_that("state_diversity() works", {
  nsw_species_counts <-
    state_diversity_counts(state = "NSW", resources = resources)
  expect_true(
    sum(nsw_species_counts$num_species) > 7000 &
      sum(nsw_species_counts$num_species) < 10000
  )
  expect_error(state_diversity_counts(state = "NOTASTATE", resources = resources))
  ss <- create_species_state_origin_matrix(resources = resources)
  sd <- read_csv("state_diversity.csv", show_col_types = FALSE)
  ss_subset <- filter(ss, ss$species %in% sd$species)
  expect_equal(ss_subset, sd)
})


test_that("native_anywhere_in_australia() works", {
  native_check <-
    native_anywhere_in_australia(
      c(
        "Eucalyptus globulus",
        "Pinus radiata",
        "Brassica rapa",
        "banksis notaspecies"
      ),
      resources = resources
    )
  #write_csv(native_check,"native_check.csv")
  previous_check <- read_csv("native_check.csv", show_col_types = FALSE)
  expect_equal(native_check, previous_check)
  expect_error(native_anywhere_in_australia(species = "NOTASPECIES", resources = resources))
})
