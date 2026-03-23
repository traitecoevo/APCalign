# Clear cached taxonomic resources

Removes any taxonomic resources that have been cached in memory during
the current R session. After calling this function, the next call to
[`load_taxonomic_resources()`](https://traitecoevo.github.io/APCalign/reference/load_taxonomic_resources.md)
will re-download and re-process the data.

## Usage

``` r
clear_cached_resources()
```

## Value

Invisibly returns `NULL`.

## Details

This is useful if you want to force a reload of the resources, for
example after updating the package or switching to a different version.

## Examples

``` r
clear_cached_resources()
```
