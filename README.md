
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/traitecoevo/ausflora/branch/master/graph/badge.svg)](https://app.codecov.io/gh/traitecoevo/ausflora?branch=master)
[![R-CMD-check](https://github.com/traitecoevo/ausflora/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/traitecoevo/ausflora/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# ausflora <img src="inst/figures/ausflora_hex2.png" align="right" width="120"/>

A R package for aligning and updating taxonomic names from the
[Australian Plant Census
(APC)](https://biodiversity.org.au/nsl/services/search/taxonomy)

## Installation

``` r
# install.packages("remotes")
# remotes::install_github("traitecoevo/ausflora")

library(ausflora)
```

## A quick demo

Generating a look-up table can be done with just one function

``` r
create_taxonomic_update_lookup(c("Banksia integrifolia", "Acacia longifolia", "Commersonia rosea"))
#> # A tibble: 3 × 2
#>   original_name        canonical_name      
#>   <chr>                <chr>               
#> 1 Banksia integrifolia Banksia integrifolia
#> 2 Acacia longifolia    Acacia longifolia   
#> 3 Commersonia rosea    Androcalva rosea
```

`create_taxonomic_update_lookup` (1) provides updates where appropiate,
(2) returns same name where there is a match to an accepted name, and
(3) returns nothing for names the function doesn’t find a match. The
returned dataframe is designed to be set up to call `left_join` with
your dataset.

This is the core functionality. Now for some more features–to see the
full taxonomic information use `full=TRUE`

``` r
create_taxonomic_update_lookup(c("Banksia integrifolia", "Acacia longifolia", "Commersonia rosea"), full = TRUE)
#> # A tibble: 3 × 16
#>   original_name        aligned_name     source taxonIDClean taxonomicStatusClean
#>   <chr>                <chr>            <chr>  <chr>        <chr>               
#> 1 Banksia integrifolia Banksia integri… APC    https://id.… accepted            
#> 2 Acacia longifolia    Acacia longifol… APC    https://id.… accepted            
#> 3 Commersonia rosea    Commersonia ros… APC    https://id.… basionym            
#> # ℹ 11 more variables: alternativeTaxonomicStatusClean <chr>,
#> #   acceptedNameUsageID <chr>, canonical_name <chr>,
#> #   scientificNameAuthorship <chr>, taxonRank <chr>, taxonomicStatus <chr>,
#> #   family <chr>, subclass <chr>, taxonDistribution <chr>,
#> #   ccAttributionIRI <chr>, genus <chr>
```

To make a reproducible workflow, specify the version number in your
code. without this the underlying taxonomic data may change.

``` r
create_taxonomic_update_lookup(c("Banksia integrifolia", "Acacia longifolia", "Commersonia rosea"), version = "0.0.2.9000")
#> # A tibble: 3 × 2
#>   original_name        canonical_name      
#>   <chr>                <chr>               
#> 1 Banksia integrifolia Banksia integrifolia
#> 2 Acacia longifolia    Acacia longifolia   
#> 3 Commersonia rosea    Androcalva rosea
```

If you’ve got potential misspellings in your data, turn on fuzzy
matching, and a putative spelling fix will be returned.

``` r
create_taxonomic_update_lookup(c("Banksia integrifolia", "Acacia longifolia", "Commersonia rosea", "Baksia integrifolia"), fuzzy_matching = TRUE)
#>   1   Baksia integrifolia    found:   Banksia integrifolia    APC list (accepted)
#> # A tibble: 4 × 2
#>   original_name        canonical_name      
#>   <chr>                <chr>               
#> 1 Baksia integrifolia  Banksia integrifolia
#> 2 Banksia integrifolia Banksia integrifolia
#> 3 Acacia longifolia    Acacia longifolia   
#> 4 Commersonia rosea    Androcalva rosea
```
