# APCalign — agent guide

R package that resolves and updates Australian plant taxon names against the
Australian Plant Census (APC) and Australian Plant Name Index (APNI). Exported
functions align messy input names to accepted names, report taxonomic updates,
and supply native/introduced status by state.

## Cross-package context

APCalign is part of the **AusTraits family**. It is the source of truth for plant
**taxonomy alignment**, and publishes the `apc.*.parquet` / `apni.*.parquet` resources
(GitHub releases) that downstream repos (notably `austraits.build`) consume during
database builds. Family-wide concerns — pipeline order, dependency direction,
cross-boundary artifacts, source-of-truth rules, gotchas — are documented centrally in the
**[austraits-meta](https://github.com/traitecoevo/austraits-meta)** repo (don't restate
them here):

- **[`AGENTS.md`](https://github.com/traitecoevo/austraits-meta/blob/main/AGENTS.md)** —
  cross-package orientation.
- **[`dependencies.yml`](https://github.com/traitecoevo/austraits-meta/blob/main/dependencies.yml)** —
  package graph + artifacts.
- **[`governance/`](https://github.com/traitecoevo/austraits-meta/tree/main/governance)** —
  labels, board #9, release playbooks, triage.

> austraits-meta is hand-maintained — verify specifics against the actual repos.
> Everything below is APCalign-local detail and stays here.

## Architecture

The user-facing pipeline is **align → update**:

- `create_taxonomic_update_lookup()` — the main entry point; runs alignment then
  taxonomy updating end to end.
- `align_taxa()` ([R/align_taxa.R](../R/align_taxa.R)) — standardises input names
  and finds the best APC/APNI alignment. Builds a `taxa` list with `tocheck` and
  `checked` tibbles, then delegates to `match_taxa()`.
- `match_taxa()` ([R/match_taxa.R](../R/match_taxa.R)) — **the core matcher, ~2150
  lines.** It runs ~54 sequential match branches (`match_01a` … `match_12i`),
  each: compute a logical index `i`, `match()` against a resource table, `mutate()`
  the matched rows with `aligned_name`/`taxon_rank`/`taxonomic_dataset`/
  `aligned_reason`/`alignment_code`, then `redistribute()` checked rows out of
  `tocheck` and early-return when `tocheck` is empty. Branches are heavily
  copy-pasted — see "Known issues".
- `update_taxonomy()` ([R/update_taxonomy.R](../R/update_taxonomy.R)) — maps aligned
  names to currently accepted names, handling synonyms and taxonomic splits.
- `load_taxonomic_resources()` ([R/load_taxonomic_resources.R](../R/load_taxonomic_resources.R))
  — downloads/loads APC+APNI parquet files for a dated `version`, derives the
  filtered lookup tables (`APC_accepted`, `APC_synonyms`, `APNI_names`,
  `genera_accepted`, `genera_synonym`, `genera_APNI`, `family_accepted`,
  `family_synonym`, …). Session-cached in the package-private `.pkg_cache`
  environment (NOT `.GlobalEnv` — CRAN compliance). Clear with
  `clear_cached_resources()`.

Supporting helpers: `standardise_names()`/`strip_names()`/`strip_names_extra()`
(text normalisation), `fuzzy_match()` (Damerau–Levenshtein with first-letter-per-word
constraints), `word()` (fast `stringr::word` replacement), `extract_genus()`.

Diversity functions: `native_anywhere_in_australia()`,
`create_species_state_origin_matrix()`, `state_diversity_counts()`.

## Running tests

Tests need the taxonomic resources loaded. `tests/testthat/helper.R` reuses a
global `resources` if present, else loads **version `2024-10-11`** (the version the
benchmarks were built against — do not bump it casually; benchmark expected values
are tied to it). First load downloads parquet files from GitHub releases.

```r
# fast iteration in an R session — load once, reuse:
devtools::load_all()
resources <- load_taxonomic_resources(version = "2024-10-11", quiet = TRUE)
testthat::test_dir("tests/testthat", filter = "match_branches")  # one file

# full suite
devtools::test()
```

Gotchas:
- **Snapshot tests need `NOT_CRAN=true`.** `expect_snapshot_value()` defaults to
  skipping on CRAN, so `test_dir()` without `NOT_CRAN` set reports "Reason: On CRAN".
  `devtools::test()` sets it for you.
- Network/data-dependent tests (`test-cache`, `test-versions`) `skip_on_cran()`.
- `R CMD check`: `devtools::check()`.

## Test design (important for changing `match_taxa`)

Regression coverage of the matcher lives in:
- `benchmarks/test_matches_alignments_updates.csv` — curated inputs, one per match
  branch; covers **all 54 branches**. Asserted in
  `test-operation_outputs.R` for `aligned_name`, `taxon_rank`,
  `taxonomic_dataset`, **and `alignment_code`**.
- `test-match_branches.R` — dark-branch resolution, `aligned_reason`
  well-formedness, and a **full-output snapshot** (`_snaps/match_branches.md`,
  date-normalised) that pins the alignment contract across all branches.

This snapshot + assertions are the safety net for refactoring `match_taxa()`: a
behaviour-preserving change leaves them green. If you legitimately change matcher
output, regenerate with `testthat::snapshot_accept("match_branches")` and update
the CSV.

## Conventions

- Tidyverse style: `%>%`, `dplyr`/`stringr`/`purrr`, namespaced calls (`dplyr::`).
- roxygen2 with markdown; run `devtools::document()` after changing `@` docs — keep
  `man/*.Rd` and `NAMESPACE` in sync (do not hand-edit them).
- User-facing output text changes (e.g. `aligned_reason`) warrant a `NEWS.md` entry.
- `Sys.Date()` is embedded in every `aligned_reason`, so raw output is not
  reproducible day to day — normalise the date when snapshotting/diffing.

## Known issues / landmines

- **`match_taxa.R` duplication.** ~54 near-identical blocks. This has already bred
  bugs (a missing ` (` before the date in three `aff.` fuzzy branches). A helper
  (`apply_match(...)`) would collapse it dramatically — do it as its own PR backed
  by the snapshot above.
- **`native_anywhere_in_australia()`**: the `is.null(resources)` guard runs *after*
  `create_species_state_origin_matrix()` (so offline errors instead of failing
  gracefully), and its `apply(..., grepl("native", x))` greps across all columns
  including the species name.
- Tests assume resources are available; there is no `skip_if_offline()` guard in the
  operation tests.
