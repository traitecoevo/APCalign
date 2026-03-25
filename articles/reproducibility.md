# How to be more reproducible with APCalign

The article will show you how to use `APCalign` to update and align your
plant taxonomic names in a more reproducible manner. The tips offered
below will be particularly useful if you used our package and will share
your code and data in your research paper or report.

There are two components that we need to cited and their versions
determined:

- The `APCalign` package itself
- The taxonomic resources used by `APCalign` for aligning and updating
  your plant taxon names

Both of these components are updated for bug fixes, or to incorporate
new taxonomic information and decisions.

First let’s load `APCalign`

``` r
library(APCalign)
```

#### APCalign R package version

To determine the version of the `APCalign` package itself:

``` r
packageVersion("APCalign")
```

#### Taxonomic Resources

`APCalign` allows users to load static downloads of taxonomic resources
the APC and APNI or the latest version from the National Species List
website. This functionality is specified using the
`stable_or_current_data` argument of
[`load_taxonomic_resources()`](https://traitecoevo.github.io/APCalign/reference/load_taxonomic_resources.md).

If you want your taxonomic alignment and update to be reproducible, we
recommend to always use `stable_or_current_data = "stable"`. The default
value is `stable_or_current_data = "stable"`. These static downloads are
version controlled and stored in our repository as
[releases](https://github.com/traitecoevo/APCalign/releases).

``` r
load_taxonomic_resources(stable_or_current_data = "stable")
```

By default,
[`load_taxonomic_resources()`](https://traitecoevo.github.io/APCalign/reference/load_taxonomic_resources.md)
will load the latest version of the static downloads.

``` r
load_taxonomic_resources(
  stable_or_current_data = "stable",
  version = default_version()
)
```

In order to be more transparent, we recommend you to check what is the
latest `default_version` before each alignment

``` r
default_version()
#> [1] "2026-03-25"
```

Then copying and pasting the output into
[`load_taxonomic_resources()`](https://traitecoevo.github.io/APCalign/reference/load_taxonomic_resources.md)
directly. This way makes the version of taxonomic resources more
explicit in your code.

To ensure the specific version of taxonomic resources is available for
subsequent functions make sure to assign them to an object:

``` r
resources_0.0.4.9000 <- load_taxonomic_resources(
  stable_or_current_data = "stable",
  version = "0.0.4.9000"
)
```

Then during alignment and update, make sure you supply your version of
taxonomic resources using the `resources` argument:

``` r
# Align taxa
aligned_taxa <- align_taxa(gbif_lite$species, resources = resources_0.0.4.9000)

# Update taxonomy 
updated_taxa <- update_taxonomy(aligned_taxa, resources = resources_0.0.4.9000)

# Align and update all-in-one
aligned_updated_taxa <- create_taxonomic_update_lookup(gbif_lite$species, resources = resources_0.0.4.9000)
```

#### Citing the R package

For completion, you can also cite the R package by calling
[`citation()`](https://rdrr.io/r/utils/citation.html). We also have a
research article introducing the `APCalign`, we will share the details
of its citation when it is in press.

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
