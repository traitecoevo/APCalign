test_that("Retrieval is possible", {
  skip_if_offline(host = "api.github.com")
  
  Sys.setenv("NETWORK_UP" = TRUE)
  versions <- get_versions()
  
  expect_visible(versions)
  expect_named(versions)
})

