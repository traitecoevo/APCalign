
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/traitecoevo/APCalign/branch/master/graph/badge.svg)](https://app.codecov.io/gh/traitecoevo/APCalign?branch=master)
[![R-CMD-check](https://github.com/traitecoevo/APCalign/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/traitecoevo/APCalign/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# APCalign <img src="man/figures/APCalign_hex.png" align="right" width="120"/>

‘APCalign’ uses the [Australian Plant Census
(APC)](https://biodiversity.org.au/nsl/services/search/taxonomy) and
[Australian Plant Name
Index](https://biodiversity.org.au/nsl/services/search/names) to
standardise Australian plant taxon names. ‘APCalign’ also supplies
information about the established status of plant taxa across different
states/territories.

## Installation

‘APCalign’ is current not on CRAN. Install the currently development
version:

``` r
# install.packages("remotes")
# remotes::install_github("traitecoevo/APCalign")

library(APCalign)
```

## A quick demo

Generating a look-up table can be done with just one function

``` r
resources <- load_taxonomic_resources()

create_taxonomic_update_lookup( 
  taxa = c(
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

## Learn more

Highly recommend looking at our [Getting
Started](https://traitecoevo.github.io/APCalign/articles/APCalign.html)
vignette to learn about how to use ‘APCalign’. You can also learn more
about our [taxa matching
algorithim](https://traitecoevo.github.io/APCalign/articles/updating-taxon-names.html)
and how [APC/APNI data is
cached](https://traitecoevo.github.io/APCalign/articles/caching.html)
behind-the-scenes.

## Found a bug?

Did you come across an unexpected taxon name change? Elusive error you
can’t debug - [submit an
issue](https://github.com/traitecoevo/APCalign/issues) and we will try
our best to help

## Comments and contributions

We welcome any comments and contributions to the package, start by
[submit an issue](https://github.com/traitecoevo/APCalign/issues) and we
can take it from there!
