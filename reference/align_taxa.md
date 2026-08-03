# Align Australian plant scientific names to the APC or APNI

For a list of Australian plant names, find taxonomic or scientific name
alignments to the APC or APNI through standardizing formatting and
fixing spelling errors.

Usage case: Users will run this function if they wish to see the details
of the matching algorithms, the many output columns that the matching
function compares to as it seeks the best alignment. They may also
select this function if they want to adjust the “fuzziness” level for
fuzzy matches, options not allowed in create_taxonomic_update_lookup.
This function is the first half of create_taxonomic_update_lookup.

## Usage

``` r
align_taxa(
  original_name,
  output = NULL,
  full = FALSE,
  resources = load_taxonomic_resources(),
  quiet = FALSE,
  fuzzy_abs_dist = 3,
  fuzzy_rel_dist = 0.2,
  fuzzy_matches = TRUE,
  imprecise_fuzzy_matches = FALSE,
  APNI_matches = TRUE,
  identifier = NA_character_
)
```

## Arguments

- original_name:

  A list of names to query for taxonomic alignments.

- output:

  (optional) The name of the file to save the results to.

- full:

  Parameter to determine how many columns are output

- resources:

  the taxonomic resources used to align the taxa names. Loading this can
  be slow, so call
  [`load_taxonomic_resources`](https://traitecoevo.github.io/APCalign/reference/load_taxonomic_resources.md)
  separately to greatly speed this function up and pass the resources
  in.

- quiet:

  Logical to indicate whether to display messages while aligning taxa.

- fuzzy_abs_dist:

  The number of characters allowed to be different for a fuzzy match.

- fuzzy_rel_dist:

  The proportion of characters allowed to be different for a fuzzy
  match.

- fuzzy_matches:

  Fuzzy matches are turned on as a default. The relative and absolute
  distances allowed for fuzzy matches to species and infraspecific taxon
  names are defined by the parameters `fuzzy_abs_dist` and
  `fuzzy_rel_dist`

- imprecise_fuzzy_matches:

  Imprecise fuzzy matches uses the fuzzy matching function with lenient
  levels set (absolute distance of 5 characters; relative distance =
  0.25). It offers a way to get a wider range of possible names,
  possibly corresponding to very distant spelling mistakes. This is
  FALSE as default and all outputs should be checked as it often makes
  erroneous matches.

- APNI_matches:

  Name matches to the APNI (Australian Plant Names Index) are turned on
  as a default.

- identifier:

  A dataset, location or other identifier, which defaults to NA.

## Value

A tibble with columns that include original_name, aligned_name,
taxonomic_dataset, taxon_rank, aligned_reason, alignment_code.

- original_name: the original plant name input.

- aligned_name: the original plant name after the function
  standardise_names has standardised the syntax of infraspecific taxon
  designations.

- taxonomic_dataset: the source of the aligned names (APC or APNI).

- taxon_rank: the taxonomic rank of the aligned name.

- aligned_reason: the explanation of a specific taxon name alignment
  (from an original name to an aligned name).

- alignment_code: a code that accompanies the aligned_reason, indicating
  the relative sequence of the match during the alignment process.

- cleaned_name: original name with punctuation and infraspecific taxon
  designation terms standardised by the function standardise_names;
  streamlines exact matches.

- stripped_name: cleaned name with punctuation and infraspecific taxon
  designation terms removed by the function strip_names; improves fuzzy
  matches.

- stripped_name2: cleaned name with punctuation, infraspecific taxon
  designation terms, and other filler words removed by the function
  `strip_names_extra`; required for matches to `first two word` and
  `first three words`.

- trinomial: the first three words in `stripped_name2`, required for
  matches that ignore all other text in the original_name; improves
  phrase name matches.

- binomial: the first two words in `stripped_name2`, required for
  matches that ignore all other text in the original_name; improves
  phrase name matches.

- genus: the first two words in `cleaned_name`; required for genus-rank
  matches and reprocessing of genus-rank names.

- fuzzy_match_genus: fuzzy match of genus column to best match among
  APC-accepted names; required for fuzzy matches of genus-rank names.

- fuzzy_match_genus_synonym: fuzzy match of genus column to best match
  among APC-synonymous names, only considering different matches to
  those documented under APC-accepted genera; required for fuzzy matches
  of genus-rank names.

- fuzzy_match_genus_APNI: fuzzy match of genus column to best match
  among APNI names, only considering different matches to those
  documented under APC-accepted and APC-known genera; required for fuzzy
  matches of genus-rank names.

- fuzzy_match_family: fuzzy match of genus column to best match among
  APC-accepted family names; required for fuzzy matches of family-rank
  names.

- fuzzy_match_family_synonym: fuzzy match of genus column to best match
  among APC-synonymous family names; required for fuzzy matches of
  family-rank names.

- fuzzy_match_cleaned_APC: fuzzy match of stripped_name to APC-accepted
  names; created for yet-to-be-aligned names at the match step 05a in
  the function `match_taxa`.

- fuzzy_match_cleaned_APC_synonym: fuzzy match of stripped_name to
  APC-synonymous names; created for yet-to-be-aligned names at the match
  step 05b in the function `match_taxa`.

- fuzzy_match_cleaned_APC_imprecise: imprecise fuzzy match of
  stripped_name to APC-accepted names; created for yet-to-be-aligned
  names at the match step 07a in the function `match_taxa`.

- fuzzy_match_cleaned_APC_synonym_imprecise: imprecise fuzzy match of
  stripped_name to APC-accepted names; created for yet-to-be-aligned
  names at the match step 07b in the function `match_taxa`.

- fuzzy_match_binomial: fuzzy match of binomial column to best match
  among APC-accepted names; created for yet-to-be-aligned names at match
  step 10c in the function `match_taxa`.

- fuzzy_match_binomial_APC_synonym: fuzzy match of binomial column to
  best match among APC-synonymous names; created for yet-to-be-aligned
  names at match step 10d in the function `match_taxa`.

- fuzzy_match_trinomial: fuzzy match of trinomial column to best match
  among APC-accepted names; created for yet-to-be-aligned names at match
  step 09c in the function `match_taxa`.

- fuzzy_match_trinomial_synonym: fuzzy match of trinomial column to best
  match among APC-synonymous names; created for yet-to-be-aligned names
  at match step 09d in the function `match_taxa`.

- fuzzy_match_cleaned_APNI: fuzzy match of stripped_name to APNI names;
  created for yet-to-be-aligned names at the match step 11a in the
  function `match_taxa`.

- fuzzy_match_cleaned_APNI_imprecise: imprecise fuzzy match of
  stripped_name to APNI names; created for yet-to-be-aligned names at
  the match step 11b in the function `match_taxa`.

## Details

- This function finds taxonomic alignments in APC or scientific name
  alignments in APNI.

- It uses the internal function `match_taxa` to attempt to match input
  strings to taxon names in the APC/APNI.

- It sequentially searches for matches against more than 20 different
  string patterns, prioritising exact matches (to accepted names as well
  as synonyms, orthographic variants) over fuzzy matches.

- It prioritises matches to taxa in the APC over names in the APNI.

- It identifies string patterns in input names that suggest a name can
  only be aligned to a genus (hybrids that are not in the APC/APNI;
  graded species; taxa not identified to species), and indicates these
  names only have a genus-rank match.

Notes:

- If you will be running the function
  APCalign::create_taxonomic_update_lookup many times, it is best to
  load the taxonomic resources separately using resources \<-
  load_taxonomic_resources(), then add the argument resources =
  resources

- The name Banksia cerrata does not align as the fuzzy matching
  algorithm does not allow the first letter of the genus and species
  epithet to change.

- With this function you have the option of changing the fuzzy matching
  parameters. The defaults, with fuzzy matches only allowing changes of
  3 (or fewer) characters AND 20% (or less) of characters has been
  carefully calibrated to catch just about all typos, but very, very
  rarely mis-align a name. If you wish to introduce less conservative
  fuzzy matching it is recommended you manually check the aligned names.

- It is recommended that you begin with imprecise_fuzzy_matches = FALSE
  (the default), as quite a few of the less precise fuzzy matches are
  likely to be erroneous. This argument should be turned on only if you
  plan to check all alignments manually.

- The argument identifier allows you to add a fix text string to all
  genus- and family- level names, such as identifier = "Royal NP" would
  return "Acacia sp. \[Royal NP\]".

## See also

[`load_taxonomic_resources`](https://traitecoevo.github.io/APCalign/reference/load_taxonomic_resources.md)

Other taxonomic alignment functions:
[`create_taxonomic_update_lookup()`](https://traitecoevo.github.io/APCalign/reference/create_taxonomic_update_lookup.md),
[`update_taxonomy()`](https://traitecoevo.github.io/APCalign/reference/update_taxonomy.md)

## Examples

``` r
# \donttest{

resources <- load_taxonomic_resources()
#> Downloading...
#> File downloaded successfully.
#> File downloaded successfully.
#> 
#> Loading resources into memory...
#> ================================================================================================================================================================
#> ...done

# example 1
align_taxa(c("Poa annua", "Abies alba"), resources=resources)
#> Checking alignments of 2 taxa
#>   -> of these 1 names have a perfect match to an accepted scientific name in the APC, and 0 names have a perfect match to a synonym in the APC.
#>       Alignments being sought for remaining names.
#> # A tibble: 2 × 7
#>   original_name cleaned_name aligned_name taxonomic_dataset taxon_rank
#>   <chr>         <chr>        <chr>        <chr>             <chr>     
#> 1 Poa annua     Poa annua    Poa annua    APC               species   
#> 2 Abies alba    Abies alba   NA           NA                NA        
#> # ℹ 2 more variables: aligned_reason <chr>, alignment_code <chr>

# example 2
input <- c("Banksia serrata", "Banksia serrate", "Banksia cerrata", 
"Banksia serrrrata", "Dryandra sp.", "Banksia big red flowers")

aligned_taxa <-
  APCalign::align_taxa(
    original_name = input,
    identifier = "APCalign test",
    full = TRUE,
    resources=resources
  ) 
#> Checking alignments of 6 taxa
#>   -> of these 1 names have a perfect match to an accepted scientific name in the APC, and 0 names have a perfect match to a synonym in the APC.
#>       Alignments being sought for remaining names.
  
# }

```
