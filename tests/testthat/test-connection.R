test_that("Complains when network is down", {
  Sys.setenv("NETWORK_UP" = FALSE)
  expect_message(default_version())
  
  Sys.setenv("NETWORK_UP" = TRUE)
  expect_visible(default_version())
})


