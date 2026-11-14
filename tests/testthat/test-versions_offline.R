# default_version() and get_versions() when the GitHub API can or cannot be
# reached, with the API call mocked so these run without a network.

local_online_with_tags <- function(tags, env = parent.frame()) {
  local_mocked_bindings(
    is_offline = function() FALSE,
    github_release_tags = function() tags,
    .env = env
  )
  # forget any version looked up earlier this session, and again afterwards
  old <- .pkg_cache$default_version
  rm(list = intersect("default_version", ls(.pkg_cache)), envir = .pkg_cache)
  withr::defer({
    rm(list = intersect("default_version", ls(.pkg_cache)), envir = .pkg_cache)
    if (!is.null(old)) .pkg_cache$default_version <- old
  }, envir = env)
}

test_that("default_version() is the newest dated release, remembered for the session", {
  local_online_with_tags(c("2024-10-11", "2026-01-14", "2025-03-17", "1.0.0"))
  expect_equal(default_version(), "2026-01-14")
  expect_equal(.pkg_cache$default_version, "2026-01-14")
})

test_that("default_version() falls back to a local version when the API fails", {
  local_online_with_tags(NULL)
  local <- local_cached_versions()

  if (length(local) > 0) {
    expect_message(v <- default_version(), "using most recent locally cached version")
    expect_equal(v, max(local))
  } else {
    expect_message(v <- default_version(), "no locally cached version")
    expect_null(v)
  }
  # a failure is not remembered
  expect_null(.pkg_cache$default_version)
})

test_that("get_versions() lists the dated releases, newest first", {
  local_online_with_tags(c("2024-10-11", "2026-01-14"))
  expect_equal(get_versions()$versions, c("2026-01-14", "2024-10-11"))

  local_mocked_bindings(github_release_tags = function() NULL)
  expect_null(get_versions())
})

test_that("a failed download returns NULL and leaves no file behind", {
  tmp <- withr::local_tempdir()
  target <- file.path(tmp, "apc2099-01-01.parquet")
  expect_message(
    out <- get_release_file(paste0("file://", tmp, "/no-such-file.parquet"), target),
    "error in downloading or reading the file"
  )
  expect_null(out)
  expect_false(file.exists(target))
})

test_that("dataset_get() returns NULL if either file cannot be had", {
  local_mocked_bindings(
    is_offline = function() FALSE,
    get_release_file = function(url, path_to_file) {
      if (grepl("apni", url)) NULL else data.frame(x = 1)
    }
  )
  expect_null(dataset_get("2099-01-01", path = withr::local_tempdir()))
})
