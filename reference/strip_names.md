# Strip taxon names

Strip taxonomic names of taxon rank abbreviations and qualifiers and
special characters

## Usage

``` r
strip_names(taxon_names)
```

## Arguments

- taxon_names:

  A character vector of taxonomic names to be stripped.

## Value

A character vector of stripped taxonomic names, with subtaxa
designations, special characters, and extra whitespace removed, and all
letters converted to lowercase.

## Details

Given a vector of taxonomic names, this function removes:

- subtaxa designations ("subsp.", "var.", "f.", and "ser")

- special characters (e.g., "-", ".", "(", ")", "?"), except a hyphen
  between two letters, which is part of a hyphenated genus or epithet
  (`Van-royena`, `Alisma plantago-aquatica`)

- extra whitespace

The resulting vector of names is also converted to lowercase.

## Examples

``` r
strip_names(c("Abies lasiocarpa subsp. lasiocarpa",
              "Quercus kelloggii",
              "Pinus contorta var. latifolia",
              "Alisma plantago-aquatica"))
#> [1] "abies lasiocarpa lasiocarpa" "quercus kelloggii"          
#> [3] "pinus contorta latifolia"    "alisma plantago-aquatica"   
```
