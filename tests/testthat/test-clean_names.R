

test_that("align_taxa() works no fuzzy", {
  expect_equal(nrow(align_taxa(original_name=c("Dryandra preissii","Banksia acuminata"),
                               fuzzy_matching = FALSE)),2)
})


test_that("align_taxa() works with fuzzy", {
  expect_equal(nrow(align_taxa(original_name=c("Dryandra preissii","Banksia acuminata"),
                               fuzzy_matching = TRUE)),2)
})

test_that("align_taxa() works with longer list", {
  species_list <- readr::read_csv(system.file("extdata", "species.csv", package = "ausflora"))
  expect_equal(nrow(aligned_data <- align_taxa(species_list$name,
                               fuzzy_matching = TRUE)),199)
})



test_that("update_taxonomy() works",{
  expect_equal(nrow(update_taxonomy(aligned_names=c("Dryandra preissii","Banksia acuminata")
                               )),2)
})

test_that("update_taxonomy() works",{
  expect_equal(nrow(create_taxonomic_update_lookup(c("Dryandra preissii","Banksia acuminata"))),2)
})


test_that("state_diversity() works",{
  nsw_species_counts<-state_diversity_counts(state = "NSW", type_of_data = "stable")
  expect_true(sum(nsw_species_counts$num_species)>7000 & sum(nsw_species_counts$num_species)<10000)
  nsw_species_counts_2<-state_diversity_counts(state = "NSW", type_of_data = "current")
  expect_true(sum(nsw_species_counts_2$num_species)>7000 & sum(nsw_species_counts_2$num_species)<10000)
})
