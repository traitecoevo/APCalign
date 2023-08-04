#' @title Standardising Taxonomic Names in Australian Plants
#'
#' @description
#' The process of standardising taxon names is necessary when working with
#' biodiversity data. 'APCalign' uses the Australian Plant Name Index (APNI)
#' and the Australian Plant Census (APC) to align and update plant taxon names
#' to current, accepted standards. 'APCalign' can also supply information about
#' the established status of plant taxa across different states/territories.
#'
#' @name APCalign
#' @docType package
#' @references If you have any questions, comments or suggestions, please
#' submit an issue at our [GitHub repository](https://github.com/traitecoevo/APCalign/issues)
#' @keywords internal
#' @section Functions:
#' **Standarise taxon names**
#'
#' * [load_taxonomic_resources]
#' * [create_taxonomic_update_lookup]
#' * [align_taxa]
#' * [update_taxonomy]
#'
#' **Established status by region**
#'
#' * [state_diversity_counts]
#' * [create_species_state_origin_matrix]
#' * [native_anywhere_in_australia]
#'
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
utils::globalVariables(
  c(
    ".",
    "acceptedNameUsage",
    "acceptedNameUsageID",
    "aligned_name",
    "aligned_reason",
    "alternativeTaxonomicStatusClean",
    "binomial",
    "canonicalName",
    "canonical_name",
    "ccAttributionIRI",
    "checked",
    "cleaned_name",
    "family",
    "fuzzy_match_genus",
    "fuzzy_match_genus_APNI",
    "fuzzy_match_genus_known",
    "genus",
    "known",
    "my_order",
    "nameElement", 
    "nameType",
    "native_anywhere", 
    "original_name",
    "scientificName",
    "scientificNameAuthorship",
    "scientificNameID",
    "stripped_canonical",
    "stripped_canonical2",
    "stripped_name",
    "stripped_name2",
    "subclass",
    "taxonDistribution",
    "taxonID",
    "taxonIDClean",
    "taxonRank",
    "taxonomicStatus",
    "taxonomicStatusClean",
    "taxonomic_ref",
    "trinomial"
  )
)
