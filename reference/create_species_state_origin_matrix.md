# State level native and introduced origin status

This function uses the taxon distribution data from the APC to determine
state level native and introduced origin status.

This function processes the geographic data available in the APC and
returns state level native, introduced and more complicated origins
status for all taxa.

## Usage

``` r
create_species_state_origin_matrix(resources = load_taxonomic_resources())
```

## Arguments

- resources:

  the taxonomic resources required to make the summary statistics.
  Loading this can be slow, so call load_taxonomic_resources separately
  to greatly speed this function up and pass the resources in.

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
#> 
#> Loading resources into memory...
#> ================================================================================================================================================================
#> ...done
#> # A tibble: 26,606 × 19
#>    species     WA    SA    Vic   NSW   Qld   LHI   NT    Tas   NI    ACT   MI   
#>    <chr>       <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
#>  1 Tribolium … natu… natu… natu… not … not … not … not … not … not … not … not …
#>  2 Acacia lox… nati… not … not … not … not … not … not … not … not … not … not …
#>  3 Schoenus s… nati… not … not … not … not … not … not … not … not … not … not …
#>  4 Olearia qu… not … not … not … nati… not … not … not … not … not … not … not …
#>  5 Thelymitra… not … nati… not … not … not … not … not … not … not … not … not …
#>  6 Pimelea br… nati… not … not … not … not … not … not … not … not … not … not …
#>  7 Boronia bi… not … not … not … not … nati… not … not … not … not … not … not …
#>  8 Asplenium … not … not … not … not … not … nati… not … not … not … not … not …
#>  9 Alectryon … nati… nati… nati… nati… nati… not … nati… not … not … not … not …
#> 10 Petrophile… nati… not … not … not … not … not … not … not … not … not … not …
#> # ℹ 26,596 more rows
#> # ℹ 7 more variables: CoI <chr>, ChI <chr>, CSI <chr>, AR <chr>, HI <chr>,
#> #   MDI <chr>, CaI <chr>


```
