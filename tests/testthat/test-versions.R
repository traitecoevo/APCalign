test_that("Retrieval is possible", {
  skip_on_ci()
  skip_on_cran()
  
  Sys.setenv("NETWORK_UP" = TRUE)
  versions <- get_versions()
  
  expect_visible(versions)
})

