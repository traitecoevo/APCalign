# CRAN submission — APCalign 2.0.0

Date: 2026-03-25

## Summary of changes

This is a major version release. Key additions and changes since 1.1.6:

### New functions

- [`synonyms_for_accepted_names()`](https://traitecoevo.github.io/APCalign/reference/synonyms_for_accepted_names.md):
  returns synonyms for currently accepted taxon names.
- [`clear_cached_resources()`](https://traitecoevo.github.io/APCalign/reference/clear_cached_resources.md):
  clears the in-session cache set by
  [`load_taxonomic_resources()`](https://traitecoevo.github.io/APCalign/reference/load_taxonomic_resources.md).

### Improvements

- [`load_taxonomic_resources()`](https://traitecoevo.github.io/APCalign/reference/load_taxonomic_resources.md)
  caches results in memory for the R session; repeated calls with the
  same version return immediately without re-downloading.
- [`load_taxonomic_resources()`](https://traitecoevo.github.io/APCalign/reference/load_taxonomic_resources.md)
  works offline when parquet files are already cached locally;
  [`default_version()`](https://traitecoevo.github.io/APCalign/reference/default_version.md)
  falls back to the most recently cached local version when no internet
  connection is available.
- [`create_species_state_origin_matrix()`](https://traitecoevo.github.io/APCalign/reference/create_species_state_origin_matrix.md)
  and
  [`native_anywhere_in_australia()`](https://traitecoevo.github.io/APCalign/reference/native_anywhere_in_australia.md)
  gain an `include_infrataxa` parameter (addresses user requests, closes
  \#265).
- State/territory columns reordered (main states/territories
  alphabetically, then smaller territories); `family` and `taxon_id`
  columns added to output.
- Internal taxonomic resource tables renamed to snake_case; `family`
  column added to resource tables (breaking change for direct table
  access).
- [`standardise_names()`](https://traitecoevo.github.io/APCalign/reference/standardise_names.md)
  now strips leading `(` from genus names.

## Test environments

- Local: macOS 15 (aarch64), R 4.4.x
- GitHub Actions: ubuntu-latest (R release, R devel), macOS-latest,
  windows-latest

## R CMD check results

0 errors \| 0 warnings \| 0 notes

## Reverse dependencies

None on CRAN.
