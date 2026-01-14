# Strip taxon names, extra

Strip taxonomic names of `sp.` and hybrid symbols. This function assumes
that a character function has already been run through `strip_names`.

## Usage

``` r
strip_names_extra(taxon_names)
```

## Arguments

- taxon_names:

  A character vector of taxonomic names to be stripped.

## Value

A character vector of stripped taxonomic names, with `sp.` and hybrid
symbols removed.

## Details

Given a vector of taxonomic names, this function removes additional
filler words (" x " for hybrid taxa, "sp.") not removed by the function
`strip_names`

## Examples

``` r
strip_names_extra(c("Abies lasiocarpa subsp. lasiocarpa",
              "Quercus kelloggii",
              "Pinus contorta var. latifolia",
              "Acacia sp.",
              "Lepidium sp. Tanguin Hill (K.R.Newbey 10501)"))
#> [1] "Abies lasiocarpa subsp. lasiocarpa"          
#> [2] "Quercus kelloggii"                           
#> [3] "Pinus contorta var. latifolia"               
#> [4] "Acacia sp."                                  
#> [5] "Lepidium sp. Tanguin Hill (K.R.Newbey 10501)"
```
