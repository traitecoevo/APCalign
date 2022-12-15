

test_that("align_taxa() works no fuzzy", {
  expect_equal(nrow(align_taxa(original_name=c("Dryandra preissii","Banksia acuminata"),
                               fuzzy_matching = FALSE)),2)
})


test_that("align_taxa() works with fuzzy", {
  expect_equal(nrow(align_taxa(original_name=c("Dryandra preissii","Banksia acuminata"),
                               fuzzy_matching = TRUE)),2)
})


test_that("update_taxonomy() works",{
  expect_equal(nrow(update_taxonomy(aligned_names=c("Dryandra preissii","Banksia acuminata")
                               )),2)
})
