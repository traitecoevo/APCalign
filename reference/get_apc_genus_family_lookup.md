# Lookup Family by Genus from APC

Retrieve the family name for a given genus using taxonomic data from the
Australian Plant Census (APC).

## Usage

``` r
get_apc_genus_family_lookup(genus, resources = load_taxonomic_resources())
```

## Arguments

- genus:

  A character vector of genus names for which to retrieve the
  corresponding family names.

- resources:

  The taxonomic resources required to make the lookup. Loading this can
  be slow, so call
  [`load_taxonomic_resources`](https://traitecoevo.github.io/APCalign/reference/load_taxonomic_resources.md)
  separately to speed up this function and pass the resources in.

## Value

A data frame with two columns: "genus", indicating the genus name, and
"family", indicating the corresponding family name from the APC.

## See also

[`load_taxonomic_resources`](https://traitecoevo.github.io/APCalign/reference/load_taxonomic_resources.md)

## Examples

``` r
 get_apc_genus_family_lookup(genus = c("Acacia", "Eucalyptus"))
#> API currently down, try again later
#> No internet connection, please retry with stable connection or specify a local version of the data
#> Not finding taxonomic resources; check internet connection?
#> NULL
```
