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
#> 
#> Loading resources into memory...
#> ================================================================================================================================================================
#> ...done
#> # A tibble: 26,620 × 21
#>    family species taxon_ID ACT   NSW   NT    Qld   SA    Tas   Vic   WA    LHI  
#>    <chr>  <chr>   <chr>    <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
#>  1 Acant… Acanth… https:/… not … not … nati… nati… not … not … not … nati… not …
#>  2 Acant… Acanth… https:/… not … not … nati… nati… not … not … not … not … not …
#>  3 Acant… Acanth… https:/… not … natu… not … doub… natu… natu… natu… natu… not …
#>  4 Acant… Androg… https:/… not … not … natu… doub… not … not … not … not … not …
#>  5 Acant… Asysta… https:/… not … not … not … not … not … not … not … not … not …
#>  6 Acant… Asysta… https:/… not … not … not … nati… not … not … not … not … not …
#>  7 Acant… Asysta… https:/… not … natu… natu… natu… not … not … not … natu… not …
#>  8 Acant… Asysta… https:/… not … not … not … not … not … not … not … not … not …
#>  9 Acant… Asysta… https:/… not … not … not … nati… not … not … not … not … not …
#> 10 Acant… Asysta… https:/… not … not … not … not … not … not … not … not … not …
#> # ℹ 26,610 more rows
#> # ℹ 9 more variables: NI <chr>, AR <chr>, CaI <chr>, ChI <chr>, CoI <chr>,
#> #   CSI <chr>, HI <chr>, MDI <chr>, MI <chr>
create_species_state_origin_matrix(include_infrataxa = TRUE)
#> Using cached taxonomic resources.
#> # A tibble: 30,935 × 21
#>    family species taxon_ID ACT   NSW   NT    Qld   SA    Tas   Vic   WA    LHI  
#>    <chr>  <chr>   <chr>    <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
#>  1 Acant… Acanth… https:/… not … not … nati… nati… not … not … not … nati… not …
#>  2 Acant… Acanth… https:/… not … not … nati… nati… not … not … not … nati… not …
#>  3 Acant… Acanth… https:/… not … not … nati… nati… not … not … not … not … not …
#>  4 Acant… Acanth… https:/… not … natu… not … doub… natu… natu… natu… natu… not …
#>  5 Acant… Androg… https:/… not … not … natu… doub… not … not … not … not … not …
#>  6 Acant… Asysta… https:/… not … not … not … not … not … not … not … not … not …
#>  7 Acant… Asysta… https:/… not … not … not … nati… not … not … not … not … not …
#>  8 Acant… Asysta… https:/… not … natu… natu… natu… not … not … not … natu… not …
#>  9 Acant… Asysta… https:/… not … not … natu… natu… not … not … not … natu… not …
#> 10 Acant… Asysta… https:/… not … not … not … natu… not … not … not … not … not …
#> # ℹ 30,925 more rows
#> # ℹ 9 more variables: NI <chr>, AR <chr>, CaI <chr>, ChI <chr>, CoI <chr>,
#> #   CSI <chr>, HI <chr>, MDI <chr>, MI <chr>


```
