# Tests for graceful degradation when network is unavailable.
# These tests simulate offline conditions using the NETWORK_UP environment
# variable and do NOT require an internet connection, so they run on CRAN.

# Helper: simulate offline, restore on exit
with_network_down <- function(code) {
  Sys.setenv("NETWORK_UP" = FALSE)
  on.exit(Sys.setenv("NETWORK_UP" = TRUE), add = TRUE)
  force(code)
}

test_that("default_version() messages and does not error when offline", {
  with_network_down({
    expect_message(result <- default_version())
    # Must be either NULL or a valid date-string from local cache — never an error
    expect_true(is.null(result) || grepl("^\\d{4}-\\d{2}-\\d{2}$", result))
  })
})

test_that("dataset_get() messages and returns NULL when offline with no local files", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  with_network_down({
    expect_message(result <- dataset_get(version = "2099-01-01", path = tmp))
    expect_null(result)
  })
})

test_that("dataset_get() messages and returns NULL when version is NULL", {
  with_network_down({
    expect_message(result <- dataset_get(version = NULL))
    expect_null(result)
  })
})

test_that("dataset_access_function() messages and returns NULL for 'current' when offline", {
  with_network_down({
    expect_message(
      result <- dataset_access_function(
        version = "2024-10-11", type = "current"
      )
    )
    expect_null(result)
  })
})

test_that("dataset_access_function() messages and returns NULL when version is NULL", {
  with_network_down({
    expect_message(
      result <- dataset_access_function(version = NULL, type = "stable")
    )
    expect_null(result)
  })
})

test_that("load_taxonomic_resources() messages and returns NULL when offline with no local files", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  with_network_down({
    # Provide a bogus version so it cannot fall back to local cache
    expect_message(
      result <- load_taxonomic_resources(
        stable_or_current_data = "stable",
        version = "2099-01-01",
        quiet = FALSE
      )
    )
    expect_null(result)
  })
})

test_that("get_versions() messages and returns NULL when offline", {
  with_network_down({
    expect_message(result <- get_versions())
    expect_null(result)
  })
})

test_that("functions return visibly when online", {
  skip_on_ci()
  skip_on_cran()

  Sys.setenv("NETWORK_UP" = TRUE)
  expect_visible(default_version())
  expect_visible(dataset_access_function())
  expect_visible(dataset_get())
})
