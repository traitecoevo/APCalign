# State- and territory-level diversity

For Australian states and territories, use geographic distribution data
from the APC to calculate state-level diversity for native, introduced,
and more complicated species origins

## Usage

``` r
state_diversity_counts(state, resources = load_taxonomic_resources())
```

## Arguments

- state:

  A character string indicating the Australian state or territory to
  calculate the diversity for. Possible values are "NSW", "NT", "Qld",
  "WA", "ChI", "SA", "Vic", "Tas", "ACT", "NI", "LHI", "MI", "HI",
  "MDI", "CoI", "CSI", and "AR".

- resources:

  the taxonomic resources required to make the summary statistics.
  loading this can be slow, so call load_taxonomic_resources separately
  to greatly speed this function up and pass the resources in.

## Value

A tibble of diversity counts for the specified state or territory,
including native, introduced, and more complicated species origins. The
tibble has three columns: "origin" indicating the origin of the species,
"state" indicating the Australian state or territory, and "num_species"
indicating the number of species for that origin and state.

## See also

[`load_taxonomic_resources`](https://traitecoevo.github.io/APCalign/reference/load_taxonomic_resources.md)

Other diversity methods:
[`create_species_state_origin_matrix()`](https://traitecoevo.github.io/APCalign/reference/create_species_state_origin_matrix.md),
[`native_anywhere_in_australia()`](https://traitecoevo.github.io/APCalign/reference/native_anywhere_in_australia.md)

## Examples

``` r
 state_diversity_counts(state = "NSW")
#> 
#> Loading resources into memory...
#> ================================================================================================================================================================
#> ...done
#> # A tibble: 7 × 3
#>   origin                            state num_species
#>   <chr>                             <chr> <table[1d]>
#> 1 doubtfully naturalised            NSW     94       
#> 2 formerly naturalised              NSW      8       
#> 3 native                            NSW   5975       
#> 4 native and doubtfully naturalised NSW      2       
#> 5 native and naturalised            NSW     34       
#> 6 naturalised                       NSW   1584       
#> 7 presumed extinct                  NSW      9       
```
