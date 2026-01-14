# Update to currently accepted APC name and add APC/APNI name metadata

For a list of taxon names aligned to the APC, update the name to an
accepted taxon concept per the APC and add scientific name and taxon
concept metadata to names aligned to either the APC or APNI.

## Usage

``` r
update_taxonomy(
  aligned_data,
  taxonomic_splits = "most_likely_species",
  quiet = TRUE,
  output = NULL,
  resources = load_taxonomic_resources()
)
```

## Arguments

- aligned_data:

  A tibble of plant names to update. This table must include 5 columns,
  original_name, aligned_name, taxon_rank, taxonomic_dataset, and
  aligned_reason. These columns are created by the function
  `align_taxa`. The columns `original_name` and `aligned_name` must be
  in the format of the scientific name, with genus and species, and may
  contain additional qualifiers such as subspecies or varieties. The
  names are case insensitive.

- taxonomic_splits:

  Variable that determines what protocol to use to update taxon names
  that are ambiguous due to taxonomic splits. The three options are:

  - `most_likely_species`, which returns the species name in use before
    the split; alternative names are returned in a separate column

  - `return_all`, which returns all possible names

  - `collapse_to_higher_taxon`, which declares that an ambiguous name
    cannot be aligned to an accepted species/infraspecific name and the
    name is demoted to genus rank

- quiet:

  Logical to indicate whether to display messages while updating taxa.

- output:

  (optional) Name of the file where results are saved. The default is
  NULL and no file is created. If specified, the output will be saved in
  a CSV file with the given name.

- resources:

  the taxonomic resources required to make the summary statistics.
  Loading this can be slow, so call load_taxonomic_resources separately
  to greatly speed this function up and pass the resources in.

## Value

A tibble with updated taxonomy for the specified plant names. The tibble
contains the following columns:

- original_name: the original plant name.

- aligned_name: the input plant name that has been aligned to a taxon
  name in the APC or APNI by the align_taxa function.

- accepted_name: the APC-accepted plant name, when available.

- suggested_name: the suggested plant name to use. Identical to the
  accepted_name, when an accepted_name exists; otherwise the the
  suggested_name is the aligned_name.

- genus: the genus of the accepted (or suggested) name; only
  APC-accepted genus names are filled in.

- family: the family of the accepted (or suggested) name; only
  APC-accepted family names are filled in.

- taxon_rank: the taxonomic rank of the suggested (and accepted) name.

- taxonomic_dataset: the source of the suggested (and accepted) names
  (APC or APNI).

- taxonomic_status: the taxonomic status of the suggested (and accepted)
  name.

- taxonomic_status_aligned: the taxonomic status of the aligned name,
  before any taxonomic updates have been applied.

- aligned_reason: the explanation of a specific taxon name alignment
  (from an original name to an aligned name).

- update_reason: the explanation of a specific taxon name update (from
  an aligned name to an accepted or suggested name).

- subclass: the subclass of the accepted name.

- taxon_distribution: the distribution of the accepted name; only filled
  in if an APC accepted_name is available.

- scientific_name_authorship: the authorship information for the
  accepted (or synonymous) name; available for both APC and APNI names.

- taxon_ID: the unique taxon concept identifier for the accepted_name;
  only filled in if an APC accepted_name is available.

- taxon_ID_genus: an identifier for the genus; only filled in if an
  APC-accepted genus name is available.

- scientific_name_ID: an identifier for the nomenclatural (not
  taxonomic) details of a scientific name; available for both APC and
  APNI names.

- row_number: the row number of a specific original_name in the input.

- number_of_collapsed_taxa: when taxonomic_splits ==
  "collapse_to_higher_taxon", the number of possible taxon names that
  have been collapsed.

## Details

- This function uses the APC to update the taxonomy of names aligned to
  a taxon concept listed in the APC to the currently accepted name for
  the taxon concept.

- The aligned_data data frame that is input must contain 5 columns,
  `original_name`, `aligned_name`, `taxon_rank`, `taxonomic_dataset`,
  and `aligned_reason`. (These are the columns output by the function
  `align_taxa`.)

- The aligned name is a plant name that has been aligned to a taxon name
  in the APC or APNI by the align_taxa function.

Notes:

- As the input for this function is a table with 5 columns (output by
  align_taxa), this function will only be used when you explicitly want
  to separate the alignment and updating components of APCalign. This
  function is the second half of create_taxonomic_update_lookup.

## See also

load_taxonomic_resources

Other taxonomic alignment functions:
[`align_taxa()`](https://traitecoevo.github.io/APCalign/reference/align_taxa.md),
[`create_taxonomic_update_lookup()`](https://traitecoevo.github.io/APCalign/reference/create_taxonomic_update_lookup.md)

## Examples

``` r
# Update taxonomy for two plant names and print the result
# \donttest{
resources <- load_taxonomic_resources()
#> 
#> Loading resources into memory...
#> ================================================================================================================================================================
#> ...done

update_taxonomy(
 dplyr::tibble(
   original_name = c("Dryandra preissii", "Banksia acuminata"),
   aligned_name = c("Dryandra preissii", "Banksia acuminata"),
   taxon_rank = c("species", "species"),
   taxonomic_dataset = c("APC", "APC"),
   aligned_reason = c(NA_character_,
   NA_character_)
 ),
 resources = resources
)
#> # A tibble: 2 × 21
#>   original_name     aligned_name      accepted_name  suggested_name genus family
#>   <chr>             <chr>             <chr>          <chr>          <chr> <chr> 
#> 1 Dryandra preissii Dryandra preissii Banksia acumi… Banksia acumi… Bank… Prote…
#> 2 Banksia acuminata Banksia acuminata Banksia acumi… Banksia acumi… Bank… Prote…
#> # ℹ 15 more variables: taxon_rank <chr>, taxonomic_dataset <chr>,
#> #   taxonomic_status <chr>, taxonomic_status_aligned <chr>,
#> #   aligned_reason <chr>, update_reason <chr>, subclass <chr>,
#> #   taxon_distribution <chr>, scientific_name <chr>, taxon_ID <chr>,
#> #   taxon_ID_genus <chr>, scientific_name_ID <chr>, canonical_name <chr>,
#> #   row_number <dbl>, number_of_collapsed_taxa <dbl>
# }
```
