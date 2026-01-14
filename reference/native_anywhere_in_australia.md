# Native anywhere in Australia

This function checks which species from a list is thought to be native
anywhere in Australia according to the APC.

## Usage

``` r
native_anywhere_in_australia(species, resources = load_taxonomic_resources())
```

## Arguments

- species:

  A character string typically representing the binomial for the
  species.

- resources:

  An optional list of taxonomic resources to use for the lookup. If not
  provided, the function will load default taxonomic resources using the
  [`load_taxonomic_resources()`](https://traitecoevo.github.io/APCalign/reference/load_taxonomic_resources.md)
  function.

## Value

A tibble with two columns: `species`, which is the same as the unique
values of the input `species`, and `native_anywhere_in_aus`, a vector
indicating whether each species is native anywhere in Australia,
introduced by humans from elsewhere, or unknown with respect to the APC
resource.

## Details

Important caveats:

- This function will not detect within-Australia introductions, e.g. if
  a species is from Western Australia and is invasive on the east coast.

- Very recent invasions are unlikely to be documented yet in APC.

- Ideally check spelling and taxonomy updates first via
  [create_taxonomic_update_lookup](https://traitecoevo.github.io/APCalign/reference/create_taxonomic_update_lookup.md).

- For the complete matrix of species by states that also represents
  within-Australia invasions, use
  [create_species_state_origin_matrix](https://traitecoevo.github.io/APCalign/reference/create_species_state_origin_matrix.md).

## See also

Other diversity methods:
[`create_species_state_origin_matrix()`](https://traitecoevo.github.io/APCalign/reference/create_species_state_origin_matrix.md),
[`state_diversity_counts()`](https://traitecoevo.github.io/APCalign/reference/state_diversity_counts.md)

## Examples

``` r
native_anywhere_in_australia(c("Eucalyptus globulus","Pinus radiata","Banksis notaspecies"))
#> 
#> Loading resources into memory...
#> ================================================================================================================================================================
#> ...done
#> Warning: At least one input not found in APC; make sure inputs are at the species level and consider using `create_taxonomic_update_lookup` first.
#> # A tibble: 3 × 2
#>   species             native_anywhere_in_aus
#>   <chr>               <chr>                 
#> 1 Eucalyptus globulus native                
#> 2 Pinus radiata       introduced            
#> 3 Banksis notaspecies unknown               
```
