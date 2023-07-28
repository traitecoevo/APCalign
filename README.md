
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/traitecoevo/ausflora/branch/master/graph/badge.svg)](https://app.codecov.io/gh/traitecoevo/ausflora?branch=master)
[![R-CMD-check](https://github.com/traitecoevo/ausflora/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/traitecoevo/ausflora/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# ausflora <img src="man/figures/ausflora_hex2.png" align="right" width="120"/>

‘ausflora’ uses the [Australian Plant Census
(APC)](https://biodiversity.org.au/nsl/services/search/taxonomy) and
[Australian Plant Name
Index](https://biodiversity.org.au/nsl/services/search/names) to
standardise Australian plant taxon names. ‘ausflora’ also supplies
information about the established status of plant taxa across different
states/territories.

## Installation

‘ausflora’ is current not on CRAN. Install the current development
version:

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

## Further reading

Highly recommend looking at our [Getting Started]() vignette to learn
about how to use ‘ausflora’. You can also learn more about our [taxa
matching algorithim]() and how [APC/APNI data is cached]()
behind-the-scenes.

## Found a bug?

Did you come across an unexpected taxon name change? Elusive error you
can’t debug - [submit an
issue](https://github.com/traitecoevo/ausflora/issues) and we will try
our best to help

## Comments and contributions

We welcome any comments and contributions to the package, start by
[submit an issue](https://github.com/traitecoevo/ausflora/issues) and we
can take it from there!
