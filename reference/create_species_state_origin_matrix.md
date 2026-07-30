# State level native and introduced origin status

This function uses the taxon distribution data from the APC to determine
state level native and introduced origin status.

This function processes the geographic data available in the APC and
returns state level native, introduced and more complicated origins
status for all taxa.

## Usage

``` r
create_species_state_origin_matrix(
  resources = load_taxonomic_resources(),
  include_infrataxa = FALSE
)
```

## Arguments

- resources:

  the taxonomic resources required to make the summary statistics.
  Loading this can be slow, so call load_taxonomic_resources separately
  to greatly speed this function up and pass the resources in.

- include_infrataxa:

  option to include subspecies, varieties and forms in the output. Set
  to false as the default, outputting results just for species-rank
  taxa.

## Value

A tibble with columns representing each state and rows representing each
species. The values in each cell represent the origin of the species in
that state.

## See also

[`load_taxonomic_resources`](https://traitecoevo.github.io/APCalign/reference/load_taxonomic_resources.md)

Other diversity methods:
[`native_anywhere_in_australia()`](https://traitecoevo.github.io/APCalign/reference/native_anywhere_in_australia.md),
[`state_diversity_counts()`](https://traitecoevo.github.io/APCalign/reference/state_diversity_counts.md)

## Examples

``` r
create_species_state_origin_matrix() 
#> API currently down, try again later
#> No internet connection, please retry with stable connection or specify a local version of the data
#> Not finding taxonomic resources; check internet connection?
#> NULL
create_species_state_origin_matrix(include_infrataxa = TRUE)
#> API currently down, try again later
#> No internet connection, please retry with stable connection or specify a local version of the data
#> Not finding taxonomic resources; check internet connection?
#> NULL


```
