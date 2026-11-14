# Get the default version for stable data

This function returns the default version for stable data, which is used
when no version is specified: the most recent dated release of the
taxonomic resources on GitHub.

## Usage

``` r
default_version()
```

## Value

A character string representing the default version for stable data.

## Details

The answer is looked up once per R session and then remembered (until
[`clear_cached_resources()`](https://traitecoevo.github.io/APCalign/reference/clear_cached_resources.md)).
If GitHub cannot be reached – no internet connection, or the GitHub API
is down or rate-limited (it allows 60 unauthenticated requests an hour)
– the most recent version already downloaded to this computer is used
instead, with a message. With neither, it returns `NULL` with a message.

## Examples

``` r
default_version()
#> [1] "2026-07-30"
```
