skip_if_no_resources()

# The closing summary printed by create_taxonomic_update_lookup() (#131)

# One name per summary line, plus a repeated name and an NA, which must not be
# counted twice or at all.
summary_names <- c(
  "Banksia serrata",                # APC species, accepted
  "Banksia serrata",                # repeat: counted once
  "Acalypha indica var. australis", # APC variety, synonym of Acalypha lanceolata
  "Dryandra sp.",                   # genus only, Dryandra -> Banksia
  "Asteraceae sp.",                 # family only
  "Acaena x ovina",                 # pro parte: could be three accepted taxa
  "Abcde fgh x ijk",                # cannot be aligned
  NA
)

lookup_summary <- function(taxonomic_splits) {
  aligned <- align_taxa(summary_names, resources = resources, quiet = TRUE)
  updated <- update_taxonomy(aligned, taxonomic_splits = taxonomic_splits,
                             resources = resources, quiet = TRUE)
  cli::ansi_strip(summarise_update_lookup(aligned, updated, taxonomic_splits))
}

test_that("summary counts each distinct name once, in every splits mode", {
  for (splits in c("most_likely_species", "collapse_to_higher_taxon",
                   "return_all")) {
    lines <- lookup_summary(splits)
    expect_equal(lines[1], "Summary of 6 names:", info = splits)
    expect_match(lines[2], "-> 3 aligned to a species or infraspecific taxon \\(3 in the APC, 0 in the APNI only\\)", info = splits)
    expect_match(lines[3], "-> 1 aligned only to a genus and 1 only to a family", info = splits)
    expect_match(lines[4], "-> 1 could not be aligned", info = splits)
    # Acalypha (synonym) and Dryandra sp. (genus) changed; Acaena x ovina is
    # reported as a split, not also as an update.
    expect_match(lines[5], "-> 2 aligned names were updated", info = splits)
    expect_match(lines[6], "-> 1 names could refer to more than one", info = splits)
    expect_match(lines[6], splits, fixed = TRUE, info = splits)
    expect_length(lines, 6)
  }
})

test_that("summary omits the splits line when there are no splits", {
  aligned <- align_taxa("Banksia serrata", resources = resources, quiet = TRUE)
  updated <- update_taxonomy(aligned, resources = resources, quiet = TRUE)
  lines <- summarise_update_lookup(aligned, updated, "most_likely_species")
  expect_length(lines, 5)
  expect_false(any(grepl("more than one", lines)))
})

test_that("create_taxonomic_update_lookup() prints the summary unless quiet", {
  expect_message(
    create_taxonomic_update_lookup("Banksia serrata", resources = resources),
    "Summary of 1 names"
  )
  expect_no_message(
    create_taxonomic_update_lookup("Banksia serrata", resources = resources,
                                   quiet = TRUE)
  )
})
