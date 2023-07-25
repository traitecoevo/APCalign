
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
resources <- load_taxonomic_resources()

create_taxonomic_update_lookup(
  c(
    "Banksia integrifolia",
    "Acacia longifolia",
    "Commersonia rosea"
  ),
  resources = resources
)
#> # A tibble: 3 × 5
#>   original_name      aligned_name apc_name aligned_reason taxonomic_status_of_…¹
#>   <chr>              <chr>        <chr>    <chr>          <chr>                 
#> 1 Banksia integrifo… Banksia int… Banksia… match_06. Aut… accepted              
#> 2 Acacia longifolia  Acacia long… Acacia … match_06. Aut… accepted              
#> 3 Commersonia rosea  Commersonia… Androca… match_06. Aut… basionym              
#> # ℹ abbreviated name: ¹​taxonomic_status_of_aligned_name
```

`create_taxonomic_update_lookup` (1) provides updates where appropriate,
(2) returns same name where there is a match to an accepted name, and
(3) returns nothing for names the function doesn’t find a match. The
returned dataframe is designed to be set up to call `left_join` with
your dataset.

This is the core functionality. Now for some more features–to see the
full taxonomic information use `full=TRUE`

``` r
create_taxonomic_update_lookup(
  c(
    "Banksia integrifolia",
    "Acacia longifolia",
    "Commersonia rosea"
  ),
  full = TRUE,
  resources = resources
)
#> # A tibble: 3 × 17
#>   original_name        aligned_name         aligned_reason   source taxonIDClean
#>   <chr>                <chr>                <chr>            <chr>  <chr>       
#> 1 Banksia integrifolia Banksia integrifolia match_06. Autom… APC    https://id.…
#> 2 Acacia longifolia    Acacia longifolia    match_06. Autom… APC    https://id.…
#> 3 Commersonia rosea    Commersonia rosea    match_06. Autom… APC    https://id.…
#> # ℹ 12 more variables: taxonomicStatusClean <chr>,
#> #   alternativeTaxonomicStatusClean <chr>, acceptedNameUsageID <chr>,
#> #   canonical_name <chr>, scientificNameAuthorship <chr>, taxonRank <chr>,
#> #   taxonomicStatus <chr>, family <chr>, subclass <chr>,
#> #   taxonDistribution <chr>, ccAttributionIRI <chr>, genus <chr>
```

To make a reproducible workflow, specify the version number in your
code. without this the underlying taxonomic data may change.

``` r
resources_0029 <- load_taxonomic_resources(version = "0.0.2.9000")
create_taxonomic_update_lookup(
  c(
    "Banksia integrifolia",
    "Acacia longifolia",
    "Commersonia rosea"
  ),
  resources = resources_0029
)
#> # A tibble: 3 × 5
#>   original_name      aligned_name apc_name aligned_reason taxonomic_status_of_…¹
#>   <chr>              <chr>        <chr>    <chr>          <chr>                 
#> 1 Banksia integrifo… Banksia int… Banksia… match_06. Aut… accepted              
#> 2 Acacia longifolia  Acacia long… Acacia … match_06. Aut… accepted              
#> 3 Commersonia rosea  Commersonia… Androca… match_06. Aut… basionym              
#> # ℹ abbreviated name: ¹​taxonomic_status_of_aligned_name
```

If you’ve got potential misspellings in your data a putative spelling
fix will be returned, along with a column that explains the change:

``` r
create_taxonomic_update_lookup(
  c(
    "Banksia integrifolia",
    "Acacia longifolia",
    "Commersonia rosea",
    "Baksia integrifolia"
  ),
  resources = resources_0029
)
#> # A tibble: 4 × 5
#>   original_name      aligned_name apc_name aligned_reason taxonomic_status_of_…¹
#>   <chr>              <chr>        <chr>    <chr>          <chr>                 
#> 1 Banksia integrifo… Banksia int… Banksia… match_06. Aut… accepted              
#> 2 Acacia longifolia  Acacia long… Acacia … match_06. Aut… accepted              
#> 3 Commersonia rosea  Commersonia… Androca… match_06. Aut… basionym              
#> 4 Baksia integrifol… Banksia int… Banksia… match_15_fuzz… accepted              
#> # ℹ abbreviated name: ¹​taxonomic_status_of_aligned_name
```

### How to link

To link the taxonomic lookup back to the original data, use `left_join`
(or one of the other join functions):

``` r
library(tidyverse, quietly = TRUE)
raw_data <- tibble(
  original_name = c(
    "Banksia integrifolia",
    "Acacia longifolia",
    "Commersonia rosea",
    "Baksia integrifolia"
  ),
  other_data = NA
)

lookup <- create_taxonomic_update_lookup(raw_data$original_name,
  resources = resources_0029
)

dplyr::left_join(raw_data, lookup)
#> # A tibble: 4 × 6
#>   original_name        other_data aligned_name         apc_name   aligned_reason
#>   <chr>                <lgl>      <chr>                <chr>      <chr>         
#> 1 Banksia integrifolia NA         Banksia integrifolia Banksia i… match_06. Aut…
#> 2 Acacia longifolia    NA         Acacia longifolia    Acacia lo… match_06. Aut…
#> 3 Commersonia rosea    NA         Commersonia rosea    Androcalv… match_06. Aut…
#> 4 Baksia integrifolia  NA         Banksia integrifolia Banksia i… match_15_fuzz…
#> # ℹ 1 more variable: taxonomic_status_of_aligned_name <chr>
```

### More detailed queires

For more experienced users, run two steps in workflow…

1.  `align_taxa`: …find best alignment with name in APNI. Name may not
    be current. This does the best possible effort to match phrase names
    and subspecific taxa which often have alternative formatting. This
    function also searches for small spelling or gender mistakes.
2.  `update_taxonomy`: …update APNI taxa to specified (or current)
    version of APC. This will use the general synonymy published by the
    APC to attempt to sync the taxon names to a specific list.

This will do spelling and formatting fixes but not taxonomic updates:

``` r
aligned <- align_taxa(
  c(
    "Banksia integrifolia",
    "Acacia longifolia",
    "Commersonia rosea",
    "Baksia integrifolia"
  ),
  resources = resources
)
#> Checking alignments of 4 taxa
#>   -> 0 names already matched; 0 names checked but without a match; 4 taxa yet to be checked
```

This will do the taxonomic update:

``` r
update_taxonomy(aligned$aligned_name)
#> Loading resources......done
#> # A tibble: 3 × 14
#>   aligned_name   source taxonIDClean taxonomicStatusClean alternativeTaxonomic…¹
#>   <chr>          <chr>  <chr>        <chr>                <chr>                 
#> 1 Acacia longif… APC    https://id.… accepted             misapplied            
#> 2 Banksia integ… APC    https://id.… accepted             misapplied            
#> 3 Commersonia r… APC    https://id.… basionym             <NA>                  
#> # ℹ abbreviated name: ¹​alternativeTaxonomicStatusClean
#> # ℹ 9 more variables: acceptedNameUsageID <chr>, canonicalName <chr>,
#> #   scientificNameAuthorship <chr>, taxonRank <chr>, taxonomicStatus <chr>,
#> #   family <chr>, subclass <chr>, taxonDistribution <chr>,
#> #   ccAttributionIRI <chr>
```

Both functions are called by `create_taxonomic_update_lookup` so if you
want the combined functionality it’s faster/easier to use
`create_taxonomic_update_lookup`.

### Saving outputs

``` r
# add more here
```
