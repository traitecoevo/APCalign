test_that("Retrieval is possible", {
  skip_on_ci()
  
  Sys.setenv("NETWORK_UP" = TRUE)
  versions <- get_versions()
  
  expect_visible(versions)
  expect_named(versions)
})

