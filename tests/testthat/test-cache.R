test_that("resources are cached after first load", {
  skip_on_cran()

  # Ensure cache has the version loaded by helper.R
  cache_key <- "stable_2024-10-11"
  expect_false(is.null(.pkg_cache[[cache_key]]))
})

test_that("second call returns cached resources with message", {
  skip_on_cran()

  expect_message(
    load_taxonomic_resources(stable_or_current_data = "stable", version = "2024-10-11", quiet = FALSE),
    "Using cached taxonomic resources"
  )
})

test_that("cached result is identical to original", {
  skip_on_cran()

  r1 <- load_taxonomic_resources(stable_or_current_data = "stable", version = "2024-10-11", quiet = TRUE)
  r2 <- load_taxonomic_resources(stable_or_current_data = "stable", version = "2024-10-11", quiet = TRUE)
  expect_identical(r1, r2)
})

test_that("clear_cached_resources() empties the cache", {
  skip_on_cran()

  # Confirm something is cached first
  expect_gt(length(ls(.pkg_cache)), 0)

  clear_cached_resources()
  expect_equal(length(ls(.pkg_cache)), 0)

  # Reload for subsequent tests
  load_taxonomic_resources(stable_or_current_data = "stable", version = "2024-10-11", quiet = TRUE)
})

test_that("after clearing, resources are reloaded without cache message", {
  skip_on_cran()

  clear_cached_resources()

  expect_no_message(
    load_taxonomic_resources(stable_or_current_data = "stable", version = "2024-10-11", quiet = TRUE),
    message = "Using cached taxonomic resources"
  )
})

test_that("local_cached_versions() finds previously downloaded versions", {
  skip_on_cran()

  versions <- local_cached_versions()
  expect_type(versions, "character")
  # At least one version should be present (downloaded by helper.R)
  expect_gt(length(versions), 0)
  # All returned values should look like dates
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", versions)))
})

test_that("local_cached_versions() returns empty vector for empty directory", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  expect_equal(local_cached_versions(path = tmp), character(0))
})

test_that("local_cached_versions() ignores versions missing the APNI file", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  # Create only the APC file, not the APNI
  file.create(file.path(tmp, "apc2024-01-01.parquet"))
  expect_equal(local_cached_versions(path = tmp), character(0))
})

test_that("default_version() falls back to local cache when offline", {
  skip_on_cran()

  Sys.setenv("NETWORK_UP" = FALSE)
  on.exit(Sys.setenv("NETWORK_UP" = TRUE))

  version <- suppressMessages(default_version())
  # Should return a date-string from the local cache, not NULL
  expect_type(version, "character")
  expect_true(grepl("^\\d{4}-\\d{2}-\\d{2}$", version))
})

test_that("load_taxonomic_resources() works offline when files are cached locally", {
  skip_on_cran()

  clear_cached_resources()
  on.exit(Sys.setenv("NETWORK_UP" = TRUE))

  Sys.setenv("NETWORK_UP" = FALSE)
  result <- suppressMessages(
    load_taxonomic_resources(quiet = TRUE)
  )

  expect_type(result, "list")
  expect_true("APC" %in% names(result))
  expect_true("APNI" %in% names(result))
})

test_that("'current' type data is not cached", {
  skip_on_cran()
  skip_on_ci()  # avoid hitting live URL in CI

  Sys.setenv("NETWORK_UP" = TRUE)
  n_before <- length(ls(.pkg_cache))

  # Suppress output; we only care about cache side-effect
  suppressMessages(
    tryCatch(
      load_taxonomic_resources(stable_or_current_data = "current", quiet = TRUE),
      error = function(e) NULL  # tolerate network/server errors
    )
  )

  # Cache should not have grown with a "current" key
  current_keys <- grep("^current", ls(.pkg_cache), value = TRUE)
  expect_equal(length(current_keys), 0)
})
