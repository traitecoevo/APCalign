# Synonyms for Currently Accepted Names

This function generates lists a string of synonyms for currently
accepted species and infra-species to facilitate working out past names
of a taxon when the current name is known.

## Usage

``` r
synonyms_for_accepted_names(
  accepted_names,
  collapse = TRUE,
  resources = load_taxonomic_resources()
)
```

## Arguments

- accepted_names:

  A character vector of currently accepted taxon names to look up
  synonyms for.

- collapse:

  Offering the option to return a long data table with each synonym in
  its own row, versus collapsed into a vector for each accepted name

- resources:

  Taxonomic resources loaded via
  [`load_taxonomic_resources()`](https://traitecoevo.github.io/APCalign/reference/load_taxonomic_resources.md).

## Value

A table with the currently accepted name and columns documenting all
synonyms and all synonyms with taxonomic status.

## Examples

``` r
# \donttest{
synonyms_for_accepted_names(
  accepted_names = c("Justicia tenella", "Acacia aneura"),
  collapse = TRUE
)
#> API currently down, try again later
#> No internet connection, please retry with stable connection or specify a local version of the data
#> Not finding taxonomic resources; check internet connection?
#> NULL
# }
```
