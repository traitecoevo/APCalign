## R CMD check results

0 errors | 0 warnings | 0 notes

## Submission notes

* This is a new major version (2.0.0) submission.
* Version bump to 2.0.0 reflects breaking changes in internal table naming
  (snake_case) and new user-facing features.

## Changes since last CRAN release (1.1.6)

* New function `synonyms_for_accepted_names()` to list synonyms for currently
  accepted taxon names.
* New function `clear_cached_resources()` to remove the session cache.
* `load_taxonomic_resources()` now caches results in memory for the R session,
  so repeated calls with the same version return immediately.
* `load_taxonomic_resources()` now works offline when parquet files have been
  previously downloaded; `default_version()` falls back to the most recently
  cached local version when no internet connection is available.
* Internal taxonomic resource tables renamed to snake_case; `family` column
  added to resource tables.
* `create_species_state_origin_matrix()` and `native_anywhere_in_australia()`
  gain an `include_infrataxa` parameter.
* State/territory columns reordered; `family` and `taxon_id` added to output.
* `standardise_names()` now removes leading `(` from genus names (relevant for
  some hybrid name formats).
