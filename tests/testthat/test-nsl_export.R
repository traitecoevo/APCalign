# read_nsl_export() must fail clearly when NSL sends back something other than
# a CSV export. These tests read local files in place of the NSL URLs, so they
# need no internet connection.

local_nsl_file <- function(lines, env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".csv", .local_envir = env)
  writeLines(lines, path)
  local_mocked_bindings(nsl_export_url = function(dataset) path, .env = env)
  path
}

test_that("read_nsl_export() errors clearly on an empty export", {
  skip_if_not_installed("readr")
  local_nsl_file(character(0))
  expect_error(read_nsl_export("APC"), "NSL returned an empty APC export")
})

test_that("read_nsl_export() errors clearly when it gets a web page", {
  skip_if_not_installed("readr")
  local_nsl_file(c("<html>", "<body>Checking your browser</body>", "</html>"))
  expect_error(
    expect_no_warning(read_nsl_export("APNI")),
    "NSL's APNI export is missing expected columns: autonym, hybrid"
  )
})

test_that("read_nsl_export() reads an export with the expected columns", {
  skip_if_not_installed("readr")
  local_nsl_file(c(
    "taxonID,canonicalName,proParte,taxonRankSortOrder,created,modified",
    "t1,Acacia dealbata,false,190,2024-01-01T00:00:00Z,2024-06-01T00:00:00Z"
  ))
  apc <- read_nsl_export("APC")
  expect_equal(nrow(apc), 1)
  expect_type(apc$proParte, "logical")
  expect_s3_class(apc$modified, "POSIXct")
})

test_that("an empty export gives the reason under the 'current' data error", {
  skip_if_not_installed("readr")
  local_mocked_bindings(is_offline = function() FALSE)
  local_nsl_file(character(0))
  expect_error(
    dataset_access_function(version = NULL, type = "current"),
    "not currently available"
  )
  err <- tryCatch(
    dataset_access_function(version = NULL, type = "current"),
    error = identity
  )
  expect_match(conditionMessage(err$parent), "empty APC export")
})
