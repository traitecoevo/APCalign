# Standardise taxon names

Standardises taxon names by performing a series of text substitutions to
remove common inconsistencies in taxonomic nomenclature.

The function takes a character vector of taxon names as input and
returns a character vector of taxon names using standardised taxonomic
syntax as output.

## Usage

``` r
standardise_names(taxon_names)
```

## Arguments

- taxon_names:

  A character vector of taxon names that need to be standardised.

## Value

A character vector of standardised taxon names.

## Details

- It removes stray punctuation at the start and end of a character
  string.

- It standardises unusual characters and symbols to ASCII equivalents.

- It standardises taxon rank abbreviations and qualifiers (subsp., var.,
  f.), as people use many variants of these terms.

- It standardises or removes a few additional filler words used within
  taxon names (affinis becomes aff.; s.l. and s.s. are removed).

## Examples

``` r
standardise_names(c("Quercus suber",
                    "Eucalyptus sp.",
                    "Eucalyptus spp.",
                    "Agave americana var. marginata",
                    "Agave americana v marginata",
                    "Notelaea longifolia forma longifolia",
                    "Notelaea longifolia f longifolia"))
#> [1] "Quercus suber"                     "Eucalyptus sp."                   
#> [3] "Eucalyptus sp."                    "Agave americana var. marginata"   
#> [5] "Agave americana var. marginata"    "Notelaea longifolia f. longifolia"
#> [7] "Notelaea longifolia f. longifolia"
```
