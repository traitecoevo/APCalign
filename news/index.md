# Changelog

## APCalign 2.0.0

CRAN release: 2026-03-27

- [`standardise_taxon_rank()`](https://traitecoevo.github.io/APCalign/reference/standardise_taxon_rank.md)
  no longer corrupts rank values that are already English, or that
  merely contain a Latin rank term as a substring. `sectio` is a literal
  prefix of its own translation (`section`) and `forma` sits inside
  `informal`, so replacing either anywhere in the input silently mangled
  `section`/`subsection`/`zoosection` into
  `sectionn`/`subsectionn`/`zoosectionn`, and
  `informal`/`informal group` into `informl`/`informl group`. Both terms
  are now matched only as the last word of the value, which still
  translates the prefixed ranks that need it (`subsectio`, `subforma`).
  APC/APNI rank values are unaffected — the fix changes output only for
  the zoological and DwC vocabularies, where it was found
  (traitecoevo/taxonAlign#11).
- [`standardise_names()`](https://traitecoevo.github.io/APCalign/reference/standardise_names.md)
  no longer rewrites `affinis` to `aff.` when it is the species epithet
  of an infraspecific name — that is, when the next word is a rank
  marker (`subsp.`, `var.`, `f.`, `ser.`, `cv.` and their unabbreviated
  or unpunctuated variants). Previously
  `Gomphrena affinis subsp. pilbarensis` became
  `Gomphrena aff. subsp. pilbarensis`, a name that exists nowhere, so it
  could only ever align to genus rank. Eight APC/APNI names were
  affected, three of them currently accepted; all now align at their own
  rank. A trailing `affinis` (`Acacia affinis`) was already left alone
  and still is. Two hybrid formulas of the form
  `Genus affinis x Genus epithet` are still rewritten, and are left for
  a follow-up.
- Fix malformed `aligned_reason` text for fuzzy genus-level
  `aff.`/`affinis` matches (`match_06b`/`06c`/`06d`), which previously
  appended the date without a separating `(`.
- `align_taxa(full = TRUE)` no longer leaks the internal
  `identifier_string`, `identifier_string2` and `aligned_name_tmp`
  columns when every name is aligned before the last match step runs.
  The output is now the documented set of columns in all cases.
- Internal refactor of `match_taxa()`: the ~54 match steps now share
  helper functions rather than repeating the same block of code.
  Alignment output is unchanged.
- [`native_anywhere_in_australia()`](https://traitecoevo.github.io/APCalign/reference/native_anywhere_in_australia.md)
  now checks for missing taxonomic resources before building the
  state-origin matrix, so an offline call reports the problem once
  instead of once per function that gives up. Its native/introduced test
  also now reads only the state columns, so a taxon whose name contains
  “native” (e.g. the `nativitatis` epithets) can no longer be
  misclassified.
- New function
  [`synonyms_for_accepted_names()`](https://traitecoevo.github.io/APCalign/reference/synonyms_for_accepted_names.md)
  to list synonyms for currently accepted taxon names.
- [`load_taxonomic_resources()`](https://traitecoevo.github.io/APCalign/reference/load_taxonomic_resources.md)
  now caches results in memory for the duration of the R session, so
  repeated calls with the same version return immediately without
  re-downloading or re-processing data.
- New function
  [`clear_cached_resources()`](https://traitecoevo.github.io/APCalign/reference/clear_cached_resources.md)
  to remove the session cache and force a reload.
- [`load_taxonomic_resources()`](https://traitecoevo.github.io/APCalign/reference/load_taxonomic_resources.md)
  now works offline when parquet files have been previously downloaded;
  [`default_version()`](https://traitecoevo.github.io/APCalign/reference/default_version.md)
  falls back to the most recently cached local version when no internet
  connection is available.
- Internal taxonomic resource tables renamed to snake_case; `family`
  column added to resource tables.
- Functions
  [`create_species_state_origin_matrix()`](https://traitecoevo.github.io/APCalign/reference/create_species_state_origin_matrix.md)
  and
  [`state_diversity_counts()`](https://traitecoevo.github.io/APCalign/reference/state_diversity_counts.md)
  now includes the parameter `include_infrataxa`, allowing users to
  select whether just species-rank taxa or species and infra-specific
  taxa are output in the table. When
  [`create_species_state_origin_matrix()`](https://traitecoevo.github.io/APCalign/reference/create_species_state_origin_matrix.md)
  is called by
  [`native_anywhere_in_australia()`](https://traitecoevo.github.io/APCalign/reference/native_anywhere_in_australia.md),
  `include_infrataxa = TRUE` is set as the default, so infrataxa can
  also be checked by this function.

## APCalign 1.1.6

- Fix issue [\#262](https://github.com/traitecoevo/APCalign/issues/262):
  filter to accepted species only in genus-family lookup
- Skip problematic tests on CRAN
- Update maintainer to Elizabeth Wenk, reflecting her leading role in
  writing the core of the package algorithms and maintaining it going
  forward.

## APCalign 1.1.3

CRAN release: 2025-02-11

- Failing gracefully for the edge case when the internet is up generally
  but github is down for a few seconds

## APCalign 1.1.2

CRAN release: 2025-01-28

- Added
  [`get_versions()`](https://traitecoevo.github.io/APCalign/reference/get_versions.md)

- Create a genus-\>family lookup from the specified APC release

## APCalign 1.0.2

CRAN release: 2024-08-17

Minor update to fix issues

- Deal with the vignette issues that emerged on CRAN
- Improve “graceful failing”, based on issues that have come up on
  github CI
- minor formatting

## APCalign 1.0.1

CRAN release: 2024-05-30

First major release of APCalign. A preprint is available at
<https://www.biorxiv.org/content/10.1101/2024.02.02.578715v1>. Article
has been accepted for publication at Australian journal of Botany.

Following review, a number of changes have been implemented. These have
sped & streamlined the package.

- Update function documentation
- Speed up `extract_genus`
- Write a replacement function for
  [`stringr::word`](https://stringr.tidyverse.org/reference/word.html)
  that is much faster.
- Additional speed up and accuracy of fuzzy_match function by
  - Restricting reference list to names with the same first letter as
    input string.
  - Switch from using
    [`utils::adist`](https://rdrr.io/r/utils/adist.html) to
    `stringdist::stringdist(method = "dl")`
- Rework `standardise_names` to remove punctuation from the start of the
  string
- Rework `strip_names_extra` (previously `strip_names_2`) to just
  perform additional functions to `strip_names`, rather than repeating
  those performed by `strip_names`.
- Avoid importing entire packages by using package::function format
  throughout and removing functions from
  [@import](https://github.com/import)
- Add fuzzy match arguments to `create_taxonomic_update_lookup`
- Add 3 additional family-level APC matches to `match_taxa`.
- Refine tests
- Make messages to console optional
- Fix issue with fails when github is down
  (<https://github.com/traitecoevo/APCalign/issues/205>)
- Update installation instructions
- Added how to cite and version APCalign as an article
- Exported `default_version`
- Add citing method for R package
- Update GitHub Actions
- Improved family alignments
- Added `standardise_taxon_rank`
- Improved messaging during alignment
