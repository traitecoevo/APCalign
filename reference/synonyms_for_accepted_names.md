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
#> Using cached taxonomic resources.
#> # A tibble: 2 × 5
#>   family      accepted_name    synonyms   scientific_name accepted_name_usage_ID
#>   <chr>       <chr>            <chr>      <chr>           <chr>                 
#> 1 Acanthaceae Justicia tenella Rostellul… Justicia tenel… https://id.biodiversi…
#> 2 Fabaceae    Acacia aneura    Acacia an… Acacia aneura … https://id.biodiversi…
# }
```
