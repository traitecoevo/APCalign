resources_current<-load_taxonomic_resources(stable_or_current_data="current")
resources<-load_taxonomic_resources(version = "0.0.2.9000")

test_that("align_taxa() works no fuzzy", {
  expect_equal(nrow(align_taxa(original_name=c("Dryandra preissii","Banksia acuminata"),
                               fuzzy_matching = FALSE, resources = resources)),2)
})


test_that("align_taxa() works with fuzzy", {
  expect_equal(nrow(align_taxa(original_name=c("Dryandra preissii","Banksia acuminata"),
                               fuzzy_matching = TRUE, resources = resources)),2)
})

test_that("align_taxa() works with longer list", {
  species_list <- readr::read_csv(system.file("extdata", "species.csv", package = "ausflora"), show_col_types = FALSE)
  expect_equal(nrow(aligned_data <- align_taxa(species_list$name,
                               fuzzy_matching = FALSE, resources = resources)),199)
})



test_that("update_taxonomy() works",{
  expect_equal(nrow(update_taxonomy(aligned_names=c("Dryandra preissii","Banksia acuminata"), 
              resources = resources)),2)
})

test_that("update_taxonomy() works",{
  expect_equal(nrow(create_taxonomic_update_lookup(c("Dryandra preissii", "Banksia acuminata"), resources = resources)), 2)
})


test_that("state_diversity() works",{
  nsw_species_counts<-state_diversity_counts(state = "NSW", resources = resources)
  expect_true(sum(nsw_species_counts$num_species)>7000 & sum(nsw_species_counts$num_species)<10000)
})

test_that("weird hybrid symbols work",{
  expect_equal(nrow(align_taxa(c("Platanus × acerifolia", "Platanus × hispanica"), resources = resources)), 2)
})

