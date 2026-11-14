# APCalign (development version)

- `create_taxonomic_update_lookup()` now uses its `version` and `stable_or_current_data` arguments. They were documented but never passed on, so the resources loaded were always the default version: `version = "2024-10-11"` was silently ignored.
- `default_version()` no longer gives up when the GitHub API can't be reached while the internet can. The API allows 60 unauthenticated requests an hour, and once that was used up (easily, on a shared network or in CI) `default_version()` returned `NULL`, so every function loading resources by default returned `NULL` with "No internet connection", even with the data already downloaded. It now falls back to the most recent locally downloaded version, as it already did offline, and says why. It also asks the API only once per R session. `get_versions()` and `default_version()` list only releases that hold the parquet files `load_taxonomic_resources()` downloads (the 2020-05-14 and 2023-02-28 releases are CSV only).
- `load_taxonomic_resources()` no longer leaves a partly downloaded file in its cache when a download fails, which was then read as the cached copy on every later call. A failed download now returns `NULL` with a message, as documented, rather than an error. `stable_or_current_data = "current"` no longer needs the GitHub API to be reachable. The progress bar goes to stderr, with the messages, and is not shown when `quiet = TRUE`.
- The `output` argument of `align_taxa()` and `create_taxonomic_update_lookup()` now works as documented: a second call with the same file reads the names already there and adds the new ones. Before, `align_taxa()` stopped with "not in the right format" on its own file, and `create_taxonomic_update_lookup()` overwrote the file with just the current names. The file now also records each name's `identifier`, and is keyed by name and identifier, so the same name with another identifier is looked up afresh. Files written by earlier versions can be reused: their names are read as having no identifier. The other arguments are not recorded, so use the same settings with a given file. When every name is already in the file, the closing summary is not printed.
- `align_taxa()` and `create_taxonomic_update_lookup()` return one row per input name for any input, including input that is all missing or blank (which returned no rows), and accept a logical all-`NA` vector (what an empty spreadsheet column reads in as), which was an error. `identifier` must be a single value or one per name, with a clear error otherwise. Columns of `update_taxonomy()` output are always their documented type (an all-`NA` column was logical).
- `standardise_names()` no longer joins the words either side of `ss`, `s.lat.` or `s.str.` (`"Acacia dealbata s.lat. Royal NP"` became `"Acacia dealbataRoyal NP"`), and now also removes `s.lat.` and `s.str.` at the end of a name. (Leading digits are still removed, so numbered lists still align; they were removed only by accident before, through a character range, `+-=`, that also covered the digits.)
- Family names that do not end in "-aceae" (Leguminosae) no longer align as an APNI genus: APNI has a genus-rank record "Leguminosae (=RFK/2760)". They now align as APC family synonyms, like Compositae.
- `state_diversity_counts()` returns `num_species` as integers rather than a `table` column, and its error for an unknown state names the argument.
- Documentation corrections: the defaults of `taxonomic_splits` and `APNI_matches` in `create_taxonomic_update_lookup()`; the output columns of `align_taxa()`, `update_taxonomy()` and `create_taxonomic_update_lookup()` (`scientific_name`, not `scientific_name_authorship`); and the main vignette, rebuilt against the current code.
- `align_taxa()`, `update_taxonomy()` and `create_taxonomic_update_lookup()` gain a `chars_changed` column: for a fuzzy match, how many characters of the name were changed to match the reference name (the Damerau-Levenshtein distance that `fuzzy_match()` already uses to choose the match), `0` for an exact match, and `NA` when the name was not aligned. Only what the match step compared is counted: for a genus- or family-rank match, just the first word, and in most steps after case, punctuation, rank markers and filler words have been standardised or removed. It describes the size of the correction, not how reliable the match is (#307).
- `create_taxonomic_update_lookup()` and `align_taxa()` no longer stop with "missing value where TRUE/FALSE needed" when one of a name's first words has no letter or digit: a genus word like `"12 34"`, `"1 1/2"`, `"\u00bd"` or `"\u00b1 Acacia"` (which becomes `"+/- Acacia"`), or a later word that is only symbols, like `"Acacia + aneura"` or `"Eucalyptus |"`. One such name used to stop the whole lookup; it now goes unmatched where it cannot be aligned. `fuzzy_match()` returns `NA` for text whose first word has no letter, and no longer errors when a word's first letter is missing.
- `create_taxonomic_update_lookup()` now ends with a summary of what it did, unless `quiet = TRUE`: how many names aligned to a species or infraspecific taxon (in the APC, or the APNI only), to a genus or family only, or not at all; how many aligned names were updated to a different name; and how many could refer to more than one accepted taxon, with a note on how the chosen `taxonomic_splits` option reports them. Each distinct input name is counted once (#131).
- `aligned_reason` now ends with the version of the APC/APNI data used, e.g. `(APC/APNI version 2024-10-11)`, instead of the date the alignment was run. The same names aligned against the same data now give identical output on any day, so results can be diffed and cached. The run date was also the wrong provenance: what an alignment depends on is the data it was made against. `load_taxonomic_resources()` records this as a `"version"` attribute on the resources it returns. For `stable_or_current_data = "current"` it is `"current, downloaded <date>"`, since the live data has no version. Resources built without `load_taxonomic_resources()` have no such attribute, and their `aligned_reason` then carries no suffix (#281).
- APCalign no longer imports `stringr`, `stringi` or `purrr`. `stringr` and `stringi` move to `Suggests`, for the tests; `purrr` is no longer used. `stringi` bundles the ICU library and was the largest single download when APCalign runs in a web browser through webR (14 MB of about 51 MB). The string handling is now base R, through small internal helpers in `R/strings.R` that reproduce the `stringr` behaviour APCalign relied on: missing values stay missing, and `stringr`'s ICU regular expressions are written as Perl ones with Unicode properties (`[:alpha:]` becomes `\p{L}`, `[:punct:]` becomes `\p{P}`, which is not the same as base R's `[[:punct:]]`). Transliteration to ASCII and ICU's lower, title and sentence case use character tables generated once from `stringi` by `data-raw/ascii_table.R` and stored in `R/sysdata.rda`.
  Output is unchanged: `standardise_names()`, `strip_names()`, `strip_names_extra()`, `extract_genus()` and `standardise_taxon_rank()` give identical results on 353,776 names (every APC and APNI name, the benchmark names, and versions with random typos, accents, non-breaking spaces and changed case), in both `en_US.UTF-8` and `C.UTF-8` locales, and so do the resource tables, `create_taxonomic_update_lookup()` (all `taxonomic_splits` and `full` options), `native_anywhere_in_australia()` and `create_species_state_origin_matrix()`. The only known differences are for text in other scripts (Greek, Cyrillic, Chinese, ...), which ICU transliterates using neighbouring characters and the tables do a character at a time, and for strings of symbols and stacked combining marks that are not plausible names.
- `fuzzy_match()` keeps the lower-case first letter of each reference name between calls, instead of working it out for the whole reference list for every name. Results are unchanged; a lookup of 790 names with typos took 6.2 s instead of 11.0 s.
- `standardise_names()`, `strip_names()` and `strip_names_extra()` now apply their text substitutions in a loop instead of `%>%` chains. Output is unchanged (checked on every APC and APNI name). magrittr's `%>%` runs a chain as nested calls, and with the 46-step chain in `standardise_names()` a lookup of even a single name needed an R evaluation depth of about 430. That overflows small stacks: APCalign running in a web browser through webR failed in Safari with "Maximum call stack size exceeded". A full lookup now needs a depth of about 110.
- Lighter dependencies. The cached APC/APNI parquet files are now read with `nanoparquet` instead of `arrow`, a much smaller install that doesn't need the Arrow C++ library. The files and the loaded resources are unchanged. `readr` moves from `Imports` to `Suggests`: it is only needed for `load_taxonomic_resources(stable_or_current_data = "current")` and for the `output` argument of `align_taxa()`, `update_taxonomy()` and `create_taxonomic_update_lookup()`. Each of these now asks you to install it if it is missing. `crayon` is replaced by `cli`, which `dplyr` already installs.

# APCalign 2.0.1

- `fuzzy_match()` no longer confirms a fuzzy match against a candidate that drops an `aff.`/`cf.`/`x` qualifier present in the query. Its `check_match()` confirmation step only compared word positions up to the shorter of query/candidate, so `Acacia aff. aneura` ("resembles *Acacia aneura*, not confidently identified") could fuzzy-match past its qualifier to the unrelated accepted species `Acacia aptaneura` instead of being capped at genus rank. A query containing `aff`, `cf`, or `x` as a standalone word now requires the candidate to contain that same token, or the candidate is rejected outright before the coarser word-position check runs; genuine `aff.`/`cf.`-qualified synonyms and named hybrids that still need fuzzy matching to fix a typo are unaffected.
- `standardise_taxon_rank()` no longer corrupts rank values that are already English, or that merely contain a Latin rank term as a substring. `sectio` is a literal prefix of its own translation (`section`) and `forma` sits inside `informal`, so replacing either anywhere in the input silently mangled `section`/`subsection`/`zoosection` into `sectionn`/`subsectionn`/`zoosectionn`, and `informal`/`informal group` into `informl`/`informl group`. Both terms are now matched only as the last word of the value, which still translates the prefixed ranks that need it (`subsectio`, `subforma`). APC/APNI rank values are unaffected — the fix changes output only for the zoological and DwC vocabularies, where it was found (traitecoevo/taxonAlign#11).
- `standardise_names()` no longer rewrites `affinis` to `aff.` when it is the species epithet of an infraspecific name — that is, when the next word is a rank marker (`subsp.`, `var.`, `f.`, `ser.`, `cv.` and their unabbreviated or unpunctuated variants). Previously `Gomphrena affinis subsp. pilbarensis` became `Gomphrena aff. subsp. pilbarensis`, a name that exists nowhere, so it could only ever align to genus rank. Eight APC/APNI names were affected, three of them currently accepted; all now align at their own rank. A trailing `affinis` (`Acacia affinis`) was already left alone and still is. Two hybrid formulas of the form `Genus affinis x Genus epithet` are still rewritten, and are left for a follow-up.
- Fix malformed `aligned_reason` text for fuzzy genus-level `aff.`/`affinis` matches (`match_06b`/`06c`/`06d`), which previously appended the date without a separating ` (`.
- `align_taxa(full = TRUE)` no longer leaks the internal `identifier_string`, `identifier_string2` and `aligned_name_tmp` columns when every name is aligned before the last match step runs. The output is now the documented set of columns in all cases.
- `native_anywhere_in_australia()` now checks for missing taxonomic resources before building the state-origin matrix, so an offline call reports the problem once instead of once per function that gives up. Its native/introduced test also now reads only the state columns, so a taxon whose name contains "native" (e.g. the `nativitatis` epithets) can no longer be misclassified.
- Internal refactor of `match_taxa()`: the ~54 match steps now share helper functions rather than repeating the same block of code. Alignment output is unchanged.
- Internal refactor: the duplicated `gsub()`-wrapper helper in `standardise_names()`, `strip_names()` and `strip_names_extra()` is now a single shared `gsub_perl()`, and a redundant nested copy of `relevel_taxonomic_status_preferred_order()` inside `synonyms_for_accepted_names()` has been removed in favour of the existing top-level function. Behaviour is unchanged.

# APCalign 2.0.0

- New function `synonyms_for_accepted_names()` to list synonyms for currently accepted taxon names.
- `load_taxonomic_resources()` now caches results in memory for the duration of the R session, so repeated calls with the same version return immediately without re-downloading or re-processing data.
- New function `clear_cached_resources()` to remove the session cache and force a reload.
- `load_taxonomic_resources()` now works offline when parquet files have been previously downloaded; `default_version()` falls back to the most recently cached local version when no internet connection is available.
- Internal taxonomic resource tables renamed to snake_case; `family` column added to resource tables.
- Functions `create_species_state_origin_matrix()` and `state_diversity_counts()` now includes the parameter `include_infrataxa`, allowing users to select whether just species-rank taxa or species and infra-specific taxa are output in the table. When `create_species_state_origin_matrix()` is called by `native_anywhere_in_australia()`, `include_infrataxa = TRUE` is set as the default, so infrataxa can also be checked by this function.

# APCalign 1.1.6

- Fix issue #262: filter to accepted species only in genus-family lookup
- Skip problematic tests on CRAN
- Update maintainer to Elizabeth Wenk, reflecting her leading role in writing the core of the package algorithms and maintaining it going forward.

# APCalign 1.1.3

- Failing gracefully for the edge case when the internet is up generally but github is down for a few seconds

# APCalign 1.1.2

- Added `get_versions()`

- Create a genus->family lookup from the specified APC release 

# APCalign 1.0.2

Minor update to fix issues

- Deal with the vignette issues that emerged on CRAN 
- Improve "graceful failing", based on issues that have come up on github CI
- minor formatting

# APCalign 1.0.1

First major release of APCalign.  A preprint is available at 
https://www.biorxiv.org/content/10.1101/2024.02.02.578715v1. 
Article has been accepted for publication at Australian journal of Botany.

Following review, a number of changes have been implemented. These have sped & 
streamlined the package.

* Update function documentation
* Speed up `extract_genus`
* Write a replacement function for `stringr::word` that is much faster.
* Additional speed up and accuracy of fuzzy_match function by
  - Restricting reference list to names with the same first letter as input string.
  - Switch from using `utils::adist` to `stringdist::stringdist(method = "dl")`
* Rework `standardise_names` to remove punctuation from the start of the string
* Rework `strip_names_extra` (previously `strip_names_2`) to just perform 
additional functions to `strip_names`, rather than repeating those performed by `strip_names`.
* Avoid importing entire packages by using package::function format throughout 
and removing functions from @import
* Add fuzzy match arguments to `create_taxonomic_update_lookup`
* Add 3 additional family-level APC matches to `match_taxa`.
* Refine tests
* Make messages to console optional
* Fix issue with fails when github is down (https://github.com/traitecoevo/APCalign/issues/205)
* Update installation instructions 
* Added how to cite and version APCalign as an article
* Exported `default_version`
* Add citing method for R package
* Update GitHub Actions
* Improved family alignments 
* Added `standardise_taxon_rank`
* Improved messaging during alignment
