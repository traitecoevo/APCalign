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

test_that("missing values already in the previous release aren't flagged again", {
  previous <- release_tables()
  previous$APC$taxonomicStatus[1:3] <- NA

  # the same data, so nothing to release (not another draft for review)
  expect_equal(check_release_resources(previous, previous)$status, "unchanged")

  # newer data, with no more missing values than before
  same_missing <- release_tables(modified = "2026-11-10 10:00:00")
  same_missing$APC$taxonomicStatus[4:6] <- NA
  expect_equal(check_release_resources(same_missing, previous)$status, "pass")

  more_missing <- release_tables(modified = "2026-11-10 10:00:00")
  more_missing$APC$taxonomicStatus[1:4] <- NA
  result <- check_release_resources(more_missing, previous)
  expect_equal(result$status, "review")
  expect_match(result$problems, "4 missing values (previous release: 3)", fixed = TRUE)
})

test_that("APNI losing more than 10% of its rows needs review", {
  previous <- release_tables(apni_rows = 100)
  later <- "2026-11-10 10:00:00"

  small <- check_release_resources(release_tables(apni_rows = 92, modified = later), previous)
  expect_equal(small$status, "pass")

  big <- check_release_resources(release_tables(apni_rows = 85, modified = later), previous)
  expect_equal(big$status, "review")
  expect_match(big$problems, "APNI has 85 rows, 15.0% fewer", fixed = TRUE)
})

test_that("duplicated APC taxonIDs need review", {
  previous <- release_tables()
  doubled <- release_tables(modified = "2026-11-10 10:00:00")
  doubled$APC <- rbind(doubled$APC, doubled$APC)

  result <- check_release_resources(doubled, previous)
  expect_equal(result$status, "review")
  expect_match(result$problems, "APC has 100 duplicated taxonID values (previous release: 0)", fixed = TRUE)
})

test_that("the smoke test fails when the release files are missing or can't be loaded", {
  empty <- withr::local_tempdir()
  problems <- check_release_alignment(empty)
  expect_match(problems$fail, "Could not find the release files")
  expect_equal(problems$review, character(0))

  # load_taxonomic_resources() returns NULL, with a message, when it can't load
  # the files; the lookup then returns NULL too, which must not pass
  path <- withr::local_tempdir()
  tables <- release_tables()
  nanoparquet::write_parquet(tables$APC, file.path(path, "apc.parquet"))
  nanoparquet::write_parquet(tables$APNI, file.path(path, "apni.parquet"))
  local_mocked_bindings(load_taxonomic_resources = function(...) NULL)
  problems <- suppressMessages(check_release_alignment(path))
  expect_match(problems$fail, "no results")
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

  expect_equal(check_release_alignment(path), list(fail = character(0), review = character(0)))
  # it leaves no trace: the real data directory and session cache are untouched
  expect_equal(tools::R_user_dir("APCalign"), source_dir)
  expect_false(exists("stable_release-candidate", envir = .pkg_cache))
})

test_that("the smoke test reports names that don't align as expected, for review", {
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
  expect_equal(problems$fail, character(0))
  expect_true(any(startsWith(problems$review, "Eucalyptus regnans: expected aligned to Eucalyptus regnans, accepted name Eucalyptus regnans")))
})
