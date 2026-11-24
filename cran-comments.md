## R CMD check results

0 errors | 0 warnings | 1 note

* Local: macOS (aarch64), R 4.6.1, `R CMD check --as-cran`, online and offline
  (no network and an empty data cache).
* win-builder: R-devel and R-release.
* R-hub: Linux, macOS (x86_64 and arm64) and Windows, R-devel: no notes.

The one NOTE, on win-builder, is about URLs:

    Found the following (possibly) invalid URLs:
      URL: https://biodiversity.org.au/nsl/services/search/names
        Status: 405
      URL: https://biodiversity.org.au/nsl/services/search/taxonomy
        Status: 405

These are the search pages of the Australian Plant Name Index and the
Australian Plant Census, the data this package works with. Their site now
answers automated requests with a bot check (HTTP 405 Method Not Allowed), but
the pages are valid and open normally in a browser.

## Reverse dependencies

There are no reverse dependencies.

## Submission notes

This is a minor release (2.0.1 -> 2.1.0). Nothing exported is removed; there
are new output columns and several bug fixes.

## Changes since last CRAN release (2.0.1)

* New `chars_changed` output column: for a fuzzy match, how many characters
  were changed to match the reference name.
* `create_taxonomic_update_lookup()` ends with a summary of what it did, unless
  `quiet = TRUE`.
* `aligned_reason` ends with the version of the APC/APNI data used instead of
  the run date, so output is reproducible.
* Hyphenated genus names and epithets are kept as one word.
* Names with a pro parte usage that leads to a single taxon are updated to
  that taxon, and taxa excluded from the APC are never returned when the name
  also leads to a taxon in Australia.
* Bug fixes: the `version` argument of `create_taxonomic_update_lookup()` was
  ignored; `default_version()` failed when the GitHub API rate limit was
  reached; the `output` file argument now works as documented; names whose
  first words have no letter no longer stop a lookup; failed downloads no
  longer leave a partial file in the cache.
* Lighter dependencies: `arrow` is replaced by `nanoparquet`; `stringr`,
  `stringi` and `purrr` are no longer imported; `readr` moves to Suggests;
  `crayon` is replaced by `cli`.
