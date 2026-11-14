skip_if_no_resources()

# Edge-case inputs, the `output` file cache, and the `version` argument.

test_that("missing, blank and empty input give one row per input name", {
  inputs <- list(
    NA_character_,
    c(NA, NA),            # logical: what an empty CSV column reads in as
    c("", "   "),
    c(NA, "", "Acacia aneura"),
    character(0)
  )
  for (x in inputs) {
    aligned <- align_taxa(x, resources = resources, quiet = TRUE)
    expect_equal(nrow(aligned), length(x))
    expect_equal(aligned$original_name, as.character(x))
    expect_type(aligned$aligned_name, "character")

    lookup <- create_taxonomic_update_lookup(x, resources = resources, quiet = TRUE)
    expect_equal(nrow(lookup), length(x))
    expect_equal(lookup$original_name, as.character(x))
    expect_type(lookup$suggested_name, "character")
  }

  aligned <- align_taxa(c(NA, "Acacia aneura"), resources = resources, quiet = TRUE)
  expect_equal(aligned$aligned_name, c(NA, "Acacia aneura"))
})

test_that("factor input aligns like character input", {
  x <- c("Acacia aneura", "Banksia serrata")
  expect_equal(
    align_taxa(factor(x), resources = resources, quiet = TRUE),
    align_taxa(x, resources = resources, quiet = TRUE)
  )
})

test_that("identifier must be a single value or one per name", {
  expect_error(
    align_taxa(c("Acacia sp.", "Banksia sp.", "Eucalyptus sp."),
               identifier = c("a", "b"), resources = resources, quiet = TRUE),
    "`identifier` must be a single value"
  )

  out <- create_taxonomic_update_lookup(
    c("Acacia sp.", NA, "Acacia sp."),
    identifier = c("site 1", "site 2", "site 3"),
    resources = resources, quiet = TRUE
  )
  expect_equal(out$suggested_name, c("Acacia sp. [site 1]", NA, "Acacia sp. [site 3]"))
})

test_that("invalid taxonomic_splits is an error", {
  expect_error(
    create_taxonomic_update_lookup("Acacia aneura", taxonomic_splits = "all",
                                   resources = resources, quiet = TRUE),
    "Invalid input"
  )
})

test_that("align_taxa() reuses and extends its output file", {
  f <- withr::local_tempfile(fileext = ".csv")
  first <- c("Acacia aneura", "Banksia serrata", "Acacia sp.")
  second <- c("Eucalyptus regnans", "Banksia serrata", "Dryandra sp.", NA)

  out1 <- align_taxa(first, output = f, identifier = "Royal NP",
                     resources = resources, quiet = TRUE)
  expect_true(file.exists(f))
  expect_equal(out1, align_taxa(first, identifier = "Royal NP",
                                resources = resources, quiet = TRUE))

  # a second call reads the file, rather than stopping with an error
  out2 <- align_taxa(second, output = f, identifier = "Royal NP",
                     resources = resources, quiet = TRUE)
  expect_equal(out2, align_taxa(second, identifier = "Royal NP",
                                resources = resources, quiet = TRUE))

  # the file now holds every name aligned so far
  saved <- readr::read_csv(f, show_col_types = FALSE)
  expect_setequal(saved$original_name, union(first, na.omit(second)))

  # a name already in the file is read from it, not aligned again
  expect_message(
    align_taxa("Acacia aneura", output = f, identifier = "Royal NP",
               resources = resources),
    "1 names already aligned in the output file; 0 taxa yet to be checked"
  )

  # the same name with a different identifier is a different entry
  out3 <- align_taxa("Acacia sp.", output = f, identifier = "Kosciuszko",
                     resources = resources, quiet = TRUE)
  expect_equal(out3$aligned_name, "Acacia sp. [Kosciuszko]")
})

test_that("align_taxa() refuses an output file it did not write", {
  f <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("a,b", "1,2"), f)
  expect_error(
    align_taxa("Acacia aneura", output = f, resources = resources, quiet = TRUE),
    "was not written by this function"
  )
})

test_that("create_taxonomic_update_lookup() reuses and extends its output file", {
  f <- withr::local_tempfile(fileext = ".csv")
  first <- c("Acacia aneura", "Banksia serrata", "Dryandra sp.")
  second <- c("Eucalyptus regnans", "Acacia aneura", NA, "Acacia aneura",
              "Banksia serrata")

  for (splits in c("most_likely_species", "return_all")) {
    unlink(f)
    lookup <- function(x, ...) {
      create_taxonomic_update_lookup(x, taxonomic_splits = splits,
                                     resources = resources, quiet = TRUE, ...)
    }

    expect_equal(lookup(first, output = f), lookup(first))
    expect_equal(lookup(second, output = f), lookup(second))
    expect_equal(lookup(second, output = f, full = TRUE),
                 lookup(second, full = TRUE))

    saved <- readr::read_csv(f, show_col_types = FALSE)
    expect_setequal(saved$original_name, union(first, na.omit(second)))
  }

  # every name already looked up (the file was last written with
  # "return_all", so use that again): nothing new to align
  expect_message(
    out <- create_taxonomic_update_lookup(first, output = f,
                                          taxonomic_splits = "return_all",
                                          resources = resources),
    "3 names already in the output file; looking up the other 0"
  )
  expect_equal(unique(out$original_name), first)
  expect_equal(out, create_taxonomic_update_lookup(
    first, taxonomic_splits = "return_all", resources = resources, quiet = TRUE
  ))
})

test_that("create_taxonomic_update_lookup() passes `version` on to the resources", {
  out <- create_taxonomic_update_lookup("Acacia aneura", version = "2024-10-11",
                                        quiet = TRUE)
  expect_match(out$aligned_reason, "\\(APC/APNI version 2024-10-11\\)$")
})

test_that("APNI_matches = FALSE never aligns to the APNI", {
  benchmarks <- readr::read_csv("benchmarks/test_matches_alignments_updates.csv",
                                show_col_types = FALSE)
  out <- align_taxa(benchmarks$original_name, APNI_matches = FALSE,
                    resources = resources, quiet = TRUE)
  expect_false(any(out$taxonomic_dataset %in% "APNI"))
  expect_false(any(grepl("APNI", out$alignment_code)))

  expect_equal(
    align_taxa("Dendropanax amplifolius", resources = resources, quiet = TRUE)$taxonomic_dataset,
    "APNI"
  )
  expect_true(is.na(
    align_taxa("Dendropanax amplifolius", APNI_matches = FALSE,
               resources = resources, quiet = TRUE)$aligned_name
  ))
})

test_that("`cf.` names are aligned to genus", {
  out <- align_taxa(c("Acacia cf. aneura", "Acacia cf aneura"),
                    resources = resources, quiet = TRUE)
  expect_equal(out$alignment_code, rep("match_06a_species_affinis_APC_exact", 2))
  expect_equal(out$taxon_rank, rep("genus", 2))
})

test_that("family names that are not '-aceae' align as families, not APNI genera", {
  # APNI has a genus-rank record "Leguminosae (=RFK/2760)"
  out <- align_taxa(c("Leguminosae", "Leguminosae sp.", "Compositae"),
                    resources = resources, quiet = TRUE)
  expect_equal(out$taxon_rank, rep("family", 3))
  expect_equal(out$alignment_code, rep("match_12e_family_exact_synonym", 3))
  expect_false("Leguminosae" %in% resources$genera_APNI$genus)
})

test_that("state_diversity_counts() returns integer counts", {
  out <- state_diversity_counts("NSW", resources = resources)
  expect_type(out$num_species, "integer")
  expect_error(state_diversity_counts("Nsw", resources = resources), "Invalid state")
})

test_that("the output file cache is keyed exactly by name and identifier", {
  f <- withr::local_tempfile(fileext = ".csv")
  rows_in_file <- function() nrow(readr::read_csv(f, show_col_types = FALSE))

  # white space at the ends of a name, and an empty identifier, survive the
  # round trip through the file, so a repeated call finds them
  x <- c("Acacia aneura ", "Acacia aneura")
  for (i in 1:3) {
    out <- align_taxa(x, output = f, identifier = "", resources = resources, quiet = TRUE)
    expect_equal(nrow(out), 2)
    expect_equal(rows_in_file(), 2)
  }
  unlink(f)
  for (i in 1:3) {
    out <- create_taxonomic_update_lookup(x, output = f, identifier = "",
                                          resources = resources, quiet = TRUE)
    expect_equal(out$original_name, x)
  }
  expect_equal(rows_in_file(), 2)

  # pairs that would run together with a space separator are kept apart
  unlink(f)
  create_taxonomic_update_lookup("Eucalyptus sp. Royal", identifier = "NP",
                                 output = f, resources = resources, quiet = TRUE)
  out <- create_taxonomic_update_lookup("Eucalyptus sp.", identifier = "Royal NP",
                                        output = f, resources = resources, quiet = TRUE)
  expect_equal(out$suggested_name, "Eucalyptus sp. [Royal NP]")
})

test_that("align_taxa() returns the columns `full` asks for, whatever the file holds", {
  f <- withr::local_tempfile(fileext = ".csv")
  align_taxa("Acacia aneura", output = f, full = TRUE, resources = resources, quiet = TRUE)
  out <- align_taxa(c("Acacia aneura", "Banksia serrata"), output = f,
                    resources = resources, quiet = TRUE)
  expect_equal(out, align_taxa(c("Acacia aneura", "Banksia serrata"),
                               resources = resources, quiet = TRUE))
  out <- align_taxa(c("Acacia aneura", "Banksia serrata"), output = f, full = TRUE,
                    resources = resources, quiet = TRUE)
  expect_equal(out, align_taxa(c("Acacia aneura", "Banksia serrata"), full = TRUE,
                               resources = resources, quiet = TRUE))
})

test_that("an output file from an earlier version, with repeated names, is reused", {
  # APCalign 2.0.1 wrote one row per input row, including repeats and missing
  # names, with `row_number`, `checked` and `known` columns and no identifier
  f <- withr::local_tempfile(fileext = ".csv")
  x <- c("Acacia aneura", "Acacia sp.", "Acacia aneura", NA)
  old <- create_taxonomic_update_lookup(x, full = TRUE, resources = resources, quiet = TRUE)
  old$checked <- TRUE
  old$known <- !is.na(old$accepted_name)
  readr::write_csv(old, f)

  y <- c("Acacia aneura", "Eucalyptus regnans", NA)
  expect_equal(
    create_taxonomic_update_lookup(y, output = f, resources = resources, quiet = TRUE),
    create_taxonomic_update_lookup(y, resources = resources, quiet = TRUE)
  )
})

test_that("numbered names still align", {
  out <- align_taxa(c("1 Acacia sp.", "1. Acacia aneura", "3 Acacia bigredflowers"),
                    resources = resources, quiet = TRUE)
  expect_equal(out$aligned_name,
               c("Acacia sp.", "Acacia aneura", "Acacia sp. [Acacia bigredflowers]"))
})
