# APCalign ![](reference/figures/APCalign_hex_2.svg)

When working with biodiversity data, it is important to verify taxonomic
names with an authoritative list and correct any out-of-date names or
names with typos.

The ‘APCalign’ package simplifies this process by:

- Accessing up-to-date taxonomic information from the [Australian Plant
  Census](https://biodiversity.org.au/nsl/services/search/taxonomy) and
  the [Australia Plant Name
  Index](https://biodiversity.org.au/nsl/services/search/names).
- Aligning authoritative names to your taxonomic names using our [fuzzy
  matching
  algorithm](https://traitecoevo.github.io/APCalign/articles/updating-taxon-names.html)
- Updating your taxonomic names in a transparent, reproducible manner
- Because APCalign was developed explicitly for the Australian flora it
  handles phrase names and aligns disparate phrase name syntax
- Indicating when a split leads to uncertainty in a name alignment

‘APCalign’ also supplies information about the established status (i.e.,
native/introduced) of plant taxa within different states/territories as
compiled by the APC. It’s useful for updating species list and
intersecting them with the APC consensus for both taxonomy and
establishment status.

Read the [APCalign paper](https://doi.org/10.1071/BT24014) to learn more
about the motivations for this project and our fuzzy matching and
aligning algorithms.

## Installation 🛠️

From CRAN:

``` r
install.packages("APCalign")

library(APCalign)
```

OR for the GitHub version:

``` r
install.packages("remotes")
remotes::install_github("traitecoevo/APCalign")
```

Or for the ShinyApp head to
[unsw.shinyapps.io/APCalign-app](https://unsw.shinyapps.io/APCalign-app/)

## A quick demo

Generating a look-up table can be done with just one function:

``` r
create_taxonomic_update_lookup( 
  taxa = c(
    "Banksia integrifolia",
    "Acacia longifolia",
    "Commersonia rosea"
    )
)
```

``` R
#> ================================================================================================================================================================
#> # A tibble: 3 × 12
#>   original_name       aligned_name accepted_name suggested_name genus taxon_rank
#>   <chr>               <chr>        <chr>         <chr>          <chr> <chr>     
#> 1 Banksia integrifol… Banksia int… Banksia inte… Banksia integ… Bank… species   
#> 2 Acacia longifolia   Acacia long… Acacia longi… Acacia longif… Acac… species   
#> 3 Commersonia rosea   Commersonia… Androcalva r… Androcalva ro… Andr… species   
#> # ℹ 6 more variables: taxonomic_dataset <chr>, taxonomic_status <chr>,
#> #   scientific_name <chr>, aligned_reason <chr>, update_reason <chr>,
#> #   number_of_collapsed_taxa <dbl>
```

You can alternatively load the taxonomic resources into memory first:

``` r
tax_resources <- load_taxonomic_resources()

create_taxonomic_update_lookup( 
  taxa = c(
    "Banksia integrifolia",
    "Banksya integrifolla",
    "Banksya integriifolla",
    "Banksyya integriifolla",
    "Banksia red flowers",
    "Banksia sp.",
    "Banksia catoglypta",
    "Dryandra catoglypta",
    "Dryandra cataglypta",
    "Dryandra australis",
    "Acacia longifolia",
    "Commersonia rosea",
    "Panicum sp. Hairy glumes (C.R.Michell 4192)",
    "Panicum sp. Hairy glumes (Michell)",
    "Panicum sp. Hairy glumes",
    "not a species"
    ),
  resources = tax_resources
)
#> # A tibble: 16 × 12
#>    original_name      aligned_name accepted_name suggested_name genus taxon_rank
#>    <chr>              <chr>        <chr>         <chr>          <chr> <chr>     
#>  1 Banksia integrifo… Banksia int… Banksia inte… Banksia integ… Bank… species   
#>  2 Banksya integrifo… Banksia int… Banksia inte… Banksia integ… Bank… species   
#>  3 Banksya integriif… Banksia int… Banksia inte… Banksia integ… Bank… species   
#>  4 Banksyya integrii… Banksia sp.… <NA>          Banksia sp. [… Bank… genus     
#>  5 Banksia red flowe… Banksia sp.… <NA>          Banksia sp. [… Bank… genus     
#>  6 Banksia sp.        Banksia sp.  <NA>          Banksia sp.    Bank… genus     
#>  7 Banksia catoglypta Banksia cat… Banksia cato… Banksia catog… Bank… species   
#>  8 Dryandra catoglyp… Dryandra ca… Banksia cato… Banksia catog… Bank… species   
#>  9 Dryandra cataglyp… Dryandra ca… Banksia cato… Banksia catog… Bank… species   
#> 10 Dryandra australis Dryandra sp… <NA>          Dryandra sp. … Drya… genus     
#> 11 Acacia longifolia  Acacia long… Acacia longi… Acacia longif… Acac… species   
#> 12 Commersonia rosea  Commersonia… Androcalva r… Androcalva ro… Andr… species   
#> 13 Panicum sp. Hairy… Panicum sp.… Panicum sp. … Panicum sp. H… Pani… species   
#> 14 Panicum sp. Hairy… Panicum sp.… Panicum sp. … Panicum sp. H… Pani… species   
#> 15 Panicum sp. Hairy… Panicum sp.… Panicum sp. … Panicum sp. H… Pani… species   
#> 16 not a species      <NA>         <NA>          <NA>           <NA>  <NA>      
#> # ℹ 6 more variables: taxonomic_dataset <chr>, taxonomic_status <chr>,
#> #   scientific_name <chr>, aligned_reason <chr>, update_reason <chr>,
#> #   number_of_collapsed_taxa <dbl>
```

Checking for a list of species to see if they are classified as
Australian natives:

``` r
native_anywhere_in_australia(c("Eucalyptus globulus","Pinus radiata"), resources = tax_resources)
#> # A tibble: 2 × 2
#>   species             native_anywhere_in_aus
#>   <chr>               <chr>                 
#> 1 Eucalyptus globulus native                
#> 2 Pinus radiata       introduced
```

Determining the number of species present in NSW and their establishment
means:

``` r
state_diversity_counts("NSW", resources = tax_resources)
#> # A tibble: 7 × 3
#>   origin                            state num_species
#>   <chr>                             <chr> <table[1d]>
#> 1 doubtfully naturalised            NSW     94       
#> 2 formerly naturalised              NSW      8       
#> 3 native                            NSW   5980       
#> 4 native and doubtfully naturalised NSW      2       
#> 5 native and naturalised            NSW     34       
#> 6 naturalised                       NSW   1584       
#> 7 presumed extinct                  NSW      9
```

The related function
[`create_species_state_origin_matrix()`](https://traitecoevo.github.io/APCalign/reference/create_species_state_origin_matrix.md)
generates a table for all taxa in Australia, indicating their
distribution and establishment means, by state.

Getting a family lookup table for genera from the specified taxonomy:

``` r
get_apc_genus_family_lookup(c("Eucalyptus",
                              "Pinus",
                              "Actinotus",
                              "Banksia",
                              "Acacia",
                              "Triodia"), 
                            resources = tax_resources)
#> # A tibble: 6 × 2
#>   genus      family    
#>   <chr>      <chr>     
#> 1 Eucalyptus Myrtaceae 
#> 2 Pinus      Pinaceae  
#> 3 Actinotus  Apiaceae  
#> 4 Banksia    Proteaceae
#> 5 Acacia     Fabaceae  
#> 6 Triodia    Poaceae
```

Compiling a list of outdated synonyms for currently accepted names:

``` r
names_to_check <- c("Acacia aneura", "Banksia nivea", "Cardamine gunnii", "Stenocarpus sinuatus")
synonyms_for_accepted_names(resources = tax_resources, accepted_names = names_to_check, collapse = T)
#> # A tibble: 4 × 5
#>   family       accepted_name     synonyms scientific_name accepted_name_usage_ID
#>   <chr>        <chr>             <chr>    <chr>           <chr>                 
#> 1 Brassicaceae Cardamine gunnii  Cardami… Cardamine gunn… https://id.biodiversi…
#> 2 Fabaceae     Acacia aneura     Acacia … Acacia aneura … https://id.biodiversi…
#> 3 Proteaceae   Banksia nivea     Dryandr… Banksia nivea … https://id.biodiversi…
#> 4 Proteaceae   Stenocarpus sinu… Stenoca… Stenocarpus si… https://id.biodiversi…
```

## Cheatsheet

[![](reference/figures/APCalign-cheatsheet.png)](https://github.com/traitecoevo/APCalign/blob/master/inst/cheatsheet/APCalign-cheatsheet.pdf)

## Learn more 📚

Highly recommend looking at our [Getting
Started](https://traitecoevo.github.io/APCalign/articles/APCalign.html)
vignette to learn about how to use `APCalign`. You can also learn more
about our [taxa matching
algorithm](https://traitecoevo.github.io/APCalign/articles/updating-taxon-names.html).

## Show us support 💛

Please consider citing our work, we are really proud of it!

``` r
citation("APCalign")
#> To cite package 'APCalign' in publications use:
#> 
#>   Wenk E, Cornwell W, Fuchs A, Kar F, Monro A, Sauquet H, Stephens R,
#>   Falster D (2024). "APCalign: an R package workflow and app for
#>   aligning and updating flora names to the Australian Plant Census."
#>   _Australian Journal of Botany_, *72*(4). R package version: 1.1.4,
#>   <https://doi.org/10.1071/BT24014>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     title = {APCalign: an R package workflow and app for aligning and updating flora names to the Australian Plant Census},
#>     journal = {Australian Journal of Botany},
#>     author = {Elizabeth Wenk and Will Cornwell and Ann Fuchs and Fonti Kar and Anna Monro and Herve Sauquet and Ruby Stephens and Daniel Falster},
#>     volume = {72},
#>     number = {4},
#>     year = {2024},
#>     publisher = {CSIRO Publishing},
#>     note = {R package version: 1.1.4},
#>     url = {https://doi.org/10.1071/BT24014},
#>   }
```

## Found a bug? 🐛

Did you come across an unexpected taxon name change? Elusive error you
can’t debug - [submit an
issue](https://github.com/traitecoevo/APCalign/issues) and we will try
our best to help.

## Comments and contributions

We welcome any comments and contributions to the package, start by
[submit an issue](https://github.com/traitecoevo/APCalign/issues) and we
can take it from there!
