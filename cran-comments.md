## R CMD check results

0 errors | 0 warnings | 0 notes

## Submission notes

* This is a patch release (2.0.0 -> 2.0.1): bug fixes and internal cleanups
  only, no breaking changes, no new exported functions.

## Changes since last CRAN release (2.0.0)

* `fuzzy_match()` no longer confirms a fuzzy match against a candidate that
  drops an `aff.`/`cf.`/`x` qualifier present in the query, which previously
  let affinis- and hybrid-qualified names resolve past their qualifier to an
  unrelated accepted species instead of being capped at genus rank.
* `standardise_taxon_rank()` no longer corrupts rank values that are already
  English, or that merely contain a Latin rank term as a substring (e.g.
  `section`/`subsection` were being mangled into `sectionn`/`subsectionn`).
* `standardise_names()` no longer rewrites `affinis` to `aff.` when it is the
  species epithet of an infraspecific name, which previously produced names
  that could only ever align to genus rank.
* Fixed malformed `aligned_reason` text for fuzzy genus-level `aff.`/`affinis`
  matches.
* `align_taxa(full = TRUE)` no longer leaks internal scratch columns when
  every name is aligned before the last match step runs.
* `native_anywhere_in_australia()` now checks for missing taxonomic resources
  once up front, and no longer misclassifies taxa whose name contains
  "native" (e.g. the `nativitatis` epithets).
* Internal refactor of `match_taxa()` and de-duplication of a repeated string
  helper; both are behaviour-preserving.
