test_that("Complains when network is down", {
  skip_if_offline(host = "api.github.com")

  Sys.setenv("NETWORK_UP" = FALSE)
  expect_message(default_version())
  expect_message(dataset_access_function())
  expect_message(dataset_get())
  
  #commenting out for now to test in CI, see issue #235
  #Sys.setenv("NETWORK_UP" = TRUE)
  #expect_visible(default_version())
  #expect_visible(dataset_access_function())
  #expect_visible(dataset_get())
})


