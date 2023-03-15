

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
  expect_equal(nrow(create_lookup(c("Dryandra preissii","Banksia acuminata")
  )),2)
})
