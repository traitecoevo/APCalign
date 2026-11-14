test_that("Retrieval is possible", {
  skip_on_ci()
  skip_on_cran()
  
  Sys.setenv("NETWORK_UP" = TRUE)
  versions <- get_versions()
  skip_if(is.null(versions), "GitHub API unavailable")

  expect_s3_class(versions, "tbl_df")
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", versions$versions)))
  expect_true("2024-10-11" %in% versions$versions)
  # releases without parquet files can't be loaded, so aren't listed
  expect_false("2020-05-14" %in% versions$versions)
})

