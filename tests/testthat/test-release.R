# Checks the apc-release workflow runs on new release files before releasing
# them (#315).

release_tables <- function(apc_rows = 100, apni_rows = 50,
                           modified = "2026-11-01 10:00:00") {
  modified <- as.POSIXct(modified, tz = "UTC")
  list(
    APC = dplyr::tibble(
      taxonID = paste0("t", seq_len(apc_rows)),
      canonicalName = paste("Genus species", seq_len(apc_rows)),
      scientificName = paste("Genus species", seq_len(apc_rows), "Author"),
      taxonomicStatus = "accepted",
      taxonRank = "Species",
      scientificNameID = paste0("n", seq_len(apc_rows)),
      acceptedNameUsageID = paste0("t", seq_len(apc_rows)),
      proParte = FALSE,
      modified = rep(modified, apc_rows)
    ),
    APNI = dplyr::tibble(
      canonicalName = paste("Other species", seq_len(apni_rows)),
      scientificName = paste("Other species", seq_len(apni_rows), "Author"),
      taxonomicStatus = "unplaced",
      taxonRank = "Species",
      scientificNameID = paste0("m", seq_len(apni_rows)),
      modified = rep(modified, apni_rows)
    )
  )
}

test_that("a release with more data than the previous one passes", {
  previous <- release_tables()
  new <- release_tables(apc_rows = 101, modified = "2026-11-10 10:00:00")
  result <- check_release_resources(new, previous)

  expect_equal(result$status, "pass")
  expect_equal(result$problems, character(0))
  expect_equal(result$summary, list(
    apc_rows = 101L, apni_rows = 50L, latest_modified = "2026-11-10 10:00:00"
  ))
})

test_that("a release with the same counts and latest modified time is unchanged", {
  result <- check_release_resources(release_tables(), release_tables())
  expect_equal(result$status, "unchanged")
})

test_that("a small drop in APC rows passes, a drop of more than 2% needs review", {
  previous <- release_tables(apc_rows = 1000)
  later <- "2026-11-10 10:00:00"

  small <- check_release_resources(release_tables(apc_rows = 985, modified = later), previous)
  expect_equal(small$status, "pass")

  big <- check_release_resources(release_tables(apc_rows = 970, modified = later), previous)
  expect_equal(big$status, "review")
  expect_match(big$problems, "3.0% fewer", fixed = TRUE)
})

test_that("missing values in a key column need review; an empty column fails", {
  previous <- release_tables()

  some <- release_tables(modified = "2026-11-10 10:00:00")
  some$APC$taxonomicStatus[1:3] <- NA
  result <- check_release_resources(some, previous)
  expect_equal(result$status, "review")
  expect_match(result$problems, "taxonomicStatus has 3 missing values")

  empty <- release_tables()
  empty$APNI$canonicalName <- ""
  result <- check_release_resources(empty, previous)
  expect_equal(result$status, "fail")
  expect_match(result$problems, "APNI column canonicalName is empty")
})

test_that("changed columns or an empty table fail", {
  previous <- release_tables()

  added <- release_tables()
  added$APC$newColumn <- "x"
  removed <- release_tables()
  removed$APNI$scientificNameID <- NULL
  retyped <- release_tables()
  retyped$APC$proParte <- "FALSE"
  no_rows <- release_tables(apc_rows = 0)

  expect_match(check_release_resources(added, previous)$problems, "APC has new columns: newColumn")
  expect_match(check_release_resources(removed, previous)$problems, "APNI is missing columns: scientificNameID")
  expect_match(check_release_resources(retyped, previous)$problems, "proParte changed type from logical to character")
  expect_match(check_release_resources(no_rows, previous)$problems, "APC has no rows")

  for (new in list(added, removed, retyped, no_rows)) {
    expect_equal(check_release_resources(new, previous)$status, "fail")
  }
})

test_that("release files written to and read from parquet keep their summary", {
  path <- withr::local_tempdir()
  tables <- release_tables()
  nanoparquet::write_parquet(tables$APC, file.path(path, "apc.parquet"))
  nanoparquet::write_parquet(tables$APNI, file.path(path, "apni.parquet"))

  expect_equal(release_summary(read_release_files(path)), release_summary(tables))
})

test_that("the smoke-test names align as expected with real release files", {
  skip_if_no_resources()
  source_dir <- tools::R_user_dir("APCalign")
  skip_if_not(file.exists(file.path(source_dir, "apc2024-10-11.parquet")))

  path <- withr::local_tempdir()
  file.copy(file.path(source_dir, "apc2024-10-11.parquet"), file.path(path, "apc.parquet"))
  file.copy(file.path(source_dir, "apni2024-10-11.parquet"), file.path(path, "apni.parquet"))

  expect_equal(check_release_alignment(path), character(0))
  # it leaves no trace: the real data directory and session cache are untouched
  expect_equal(tools::R_user_dir("APCalign"), source_dir)
  expect_false(exists("stable_release-candidate", envir = .pkg_cache))
})

test_that("the smoke test reports names that don't align as expected", {
  skip_if_no_resources()
  source_dir <- tools::R_user_dir("APCalign")
  skip_if_not(file.exists(file.path(source_dir, "apc2024-10-11.parquet")))

  # an APC without Eucalyptus regnans
  path <- withr::local_tempdir()
  apc <- read_parquet_tbl(file.path(source_dir, "apc2024-10-11.parquet"))
  apc <- apc[!grepl("^Eucalyptus regnans", apc$canonicalName), ]
  nanoparquet::write_parquet(apc, file.path(path, "apc.parquet"))
  file.copy(file.path(source_dir, "apni2024-10-11.parquet"), file.path(path, "apni.parquet"))

  problems <- check_release_alignment(path)
  expect_true(any(grepl("^Eucalyptus regnans: expected accepted name Eucalyptus regnans", problems)))
})
