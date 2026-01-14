# Standardise taxon ranks

Standardise taxon ranks from Latin into English.

## Usage

``` r
standardise_taxon_rank(taxon_rank)
```

## Arguments

- taxon_rank:

  A character vector of Latin taxon ranks.

## Value

A character vector of English taxon ranks.

## Details

The function takes a character vector of Latin taxon ranks as input and
returns a character vector of taxon ranks using standardised English
terms.

## Examples

``` r
standardise_taxon_rank(c("regnum", "kingdom", "classis", "class"))
#> [1] "kingdom" "kingdom" "class"   "class"  
```
