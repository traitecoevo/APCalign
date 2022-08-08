
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

# ausflora <img src="inst/figures/ausflora_hex2.png" align="right" width="120"/>

A R package for aligning and updating taxonomic names from the
[Australian Plant Census
(APC)](https://biodiversity.org.au/nsl/services/search/taxonomy)

## Installation

`ausflora` relies on another R package `datastorr`, both of which are
currently available from
[GitHub](https://github.com/traitecoevo/ausflora) only. You can install
both these packages with:

``` r
# install.packages("remotes")
remotes::install_github("traitecoevo/datastorr")
remotes::install_github("traitecoevo/ausflora")

library(ausflora)
```

## A quick demo

First, we will retrieve the current [release of APC
names](https://github.com/traitecoevo/ausflora/releases/tag/0.0.0.9000).
The `dataset_access_function` saves the download in a local, temporary
folder.

``` r
tmp <- dataset_access_function("0.0.0.9000")

tmp
#> $APC
#> # A tibble: 100,835 × 34
#>    taxonID       nameT…¹ accep…² accep…³ nomen…⁴ taxon…⁵ proPa…⁶ scien…⁷ scien…⁸
#>    <chr>         <chr>   <chr>   <chr>   <chr>   <chr>   <lgl>   <chr>   <chr>  
#>  1 https://id.b… scient… https:… Planta… <NA>    accept… FALSE   Planta… https:…
#>  2 https://id.b… scient… https:… Charop… <NA>    orthog… FALSE   Charac… https:…
#>  3 https://id.b… scient… https:… Charop… <NA>    accept… FALSE   Charop… https:…
#>  4 https://id.b… scient… https:… Equise… <NA>    orthog… FALSE   Equise… https:…
#>  5 https://id.b… scient… https:… Equise… <NA>    accept… FALSE   Equise… https:…
#>  6 https://id.b… scient… https:… Equise… <NA>    taxono… FALSE   Spheno… https:…
#>  7 https://id.b… scient… https:… Cycadi… <NA>    accept… FALSE   Cycadi… https:…
#>  8 https://id.b… scient… https:… Cycada… <NA>    taxono… FALSE   Zamiin… https:…
#>  9 https://id.b… scient… https:… Cycada… <NA>    accept… FALSE   Cycada… https:…
#> 10 https://id.b… scient… https:… Cycada… <NA>    taxono… FALSE   Stange… https:…
#> # … with 100,825 more rows, 25 more variables: canonicalName <chr>,
#> #   scientificNameAuthorship <chr>, parentNameUsageID <chr>, taxonRank <chr>,
#> #   taxonRankSortOrder <dbl>, kingdom <chr>, class <chr>, subclass <chr>,
#> #   family <chr>, created <dttm>, modified <dttm>, datasetName <chr>,
#> #   taxonConceptID <chr>, nameAccordingTo <chr>, nameAccordingToID <chr>,
#> #   taxonRemarks <chr>, taxonDistribution <chr>, higherClassification <chr>,
#> #   firstHybridParentName <chr>, firstHybridParentNameID <chr>, …
#> # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names
#> 
#> $APNI
#> # A tibble: 154,302 × 42
#>    scientificN…¹ scien…² canon…³ canon…⁴ nameE…⁵ scien…⁶ nameT…⁷ taxon…⁸ nomen…⁹
#>    <chr>         <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
#>  1 Aaron's Beard <commo… Aaron'… <commo… Aaron'… https:… common  unplac… [n/a]  
#>  2 Aaron's Rod   <commo… Aaron'… <commo… Aaron'… https:… common  unplac… [n/a]  
#>  3 Abacopteris … <scien… Abacop… <scien… Abacop… https:… scient… includ… <NA>   
#>  4 Abacopteris … <scien… Abacop… <scien… Abacop… https:… scient… unplac… nom. i…
#>  5 Abacopteris … <scien… Abacop… <scien… aspera  https:… scient… includ… <NA>   
#>  6 Abacopteris … <scien… Abacop… <scien… presli… https:… scient… unplac… <NA>   
#>  7 Abacopteris … <scien… Abacop… <scien… triphy… https:… scient… includ… <NA>   
#>  8 Abarema Pitt… <scien… Abarema <scien… Abarema https:… scient… unplac… <NA>   
#>  9 Abarema clyp… <scien… Abarem… <scien… praini… https:… scient… unplac… nom. i…
#> 10 Abarema gran… <scien… Abarem… <scien… grandi… https:… scient… includ… <NA>   
#> # … with 154,292 more rows, 33 more variables: scientificNameAuthorship <chr>,
#> #   cultivarEpithet <chr>, autonym <lgl>, hybrid <lgl>, cultivar <lgl>,
#> #   formula <lgl>, scientific <lgl>, nomInval <lgl>, nomIlleg <lgl>,
#> #   namePublishedIn <chr>, namePublishedInYear <dbl>, nameInstanceType <chr>,
#> #   originalNameUsage <chr>, originalNameUsageID <chr>, typeCitation <chr>,
#> #   kingdom <chr>, family <chr>, genericName <chr>, specificEpithet <chr>,
#> #   infraspecificEpithet <chr>, taxonRank <chr>, taxonRankSortOrder <dbl>, …
#> # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names
```

Now we will load a toy plant species list and try align it with the APC
species names.

``` r
#remotes::install_packages("readr")
species_list <- readr::read_csv(system.file("extdata", "species.csv", package = "ausflora"))

species_list
#> # A tibble: 199 × 1
#>    name                       
#>    <chr>                      
#>  1 Eucalyptus tectifica       
#>  2 Eulalia aurea              
#>  3 Triodia bitextura          
#>  4 Terminalia canescens       
#>  5 Annual forb                
#>  6 Erythrophleum chlorostachys
#>  7 Schizachyrium fragile      
#>  8 Acacia hemsleyi            
#>  9 Eriachne obtusa            
#> 10 Corchorus sidoides         
#> # … with 189 more rows
#> # ℹ Use `print(n = ...)` to see more rows
```

### Align away!

`align_taxa` will try match the species names in our list to those
listed in the APC. You can specify a file path and file name using the
argument `output` to save this aligned data

``` r
aligned_data <- align_taxa(species_list$name, output = "ignore/taxonomic_updates.csv")
#> Checking alignments of 199 taxa
#>   - reading existing data from ignore/taxonomic_updates.csv
#>   - all taxa are already checked, yay!

aligned_data
#> # A tibble: 199 × 7
#>    original_name                 cleaned_…¹ align…² source known checked strip…³
#>    <chr>                         <chr>      <chr>   <chr>  <lgl> <lgl>   <chr>  
#>  1 Annual forb                   Annual fo… <NA>    <NA>   FALSE TRUE    annual…
#>  2 Tephrosia sp. macarthur river Tephrosia… <NA>    <NA>   FALSE TRUE    tephro…
#>  3 No id                         No id      <NA>    <NA>   FALSE TRUE    no id  
#>  4 Sorghum sp.                   Sorghum s… <NA>    <NA>   FALSE TRUE    sorghu…
#>  5 Tephrosia sp.                 Tephrosia… <NA>    <NA>   FALSE TRUE    tephro…
#>  6 Corymbia sp.                  Corymbia … <NA>    <NA>   FALSE TRUE    corymb…
#>  7 Alphitonia sp.                Alphitoni… <NA>    <NA>   FALSE TRUE    alphit…
#>  8 Poaceae sp.                   Poaceae s… <NA>    <NA>   FALSE TRUE    poacea…
#>  9 Pultenaea sp.                 Pultenaea… <NA>    <NA>   FALSE TRUE    pulten…
#> 10 Eucalyptus tectifica          Eucalyptu… Eucaly… APC l… TRUE  TRUE    eucaly…
#> # … with 189 more rows, and abbreviated variable names ¹​cleaned_name,
#> #   ²​aligned_name, ³​stripped_name
#> # ℹ Use `print(n = ...)` to see more rows
```

### Update taxonomic names

Once alignment is complete we can update our species names, replacing
synonyms to current APC names. Note that this function drops the
observations where names cannot be aligned. Again, this updated species
list can be saved using the `output` argument.

``` r
aligned_species_list <- update_taxonomy(aligned_data$aligned_name)
#> loading object `taxonomic_resources` into global environment
```
