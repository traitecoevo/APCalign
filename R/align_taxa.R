#' @title Align Australian plant scientific names to the APC or APNI
#' 
#' @description
#' For a list of Australian plant names, find taxonomic or scientific name
#' alignments to the APC or APNI through standardizing formatting and fixing
#' spelling errors.
#' 
#' Usage case: Users will run this function if they wish to see the details
#'  of the matching algorithms, the many output columns that the matching
#'  function compares to as it seeks the best alignment. They may also select
#'  this function if they want to adjust the “fuzziness” level for fuzzy
#'  matches, options not allowed in create_taxonomic_update_lookup. This
#'  function is the first half of create_taxonomic_update_lookup.
#'
#' @details  
#' - This function finds taxonomic alignments in APC or scientific name 
#' alignments in APNI. 
#' - It uses the internal function `match_taxa` to attempt to match input
#'  strings to taxon names in the APC/APNI. 
#' - It sequentially searches for matches against 54 different string
#'  patterns, prioritising exact matches (to accepted names as well as
#'  synonyms, orthographic variants) over fuzzy matches.
#' - It prioritises matches to taxa in the APC over names in the APNI.
#' - It identifies string patterns in input names that suggest a name can only
#'  be aligned to a genus (hybrids that are not in the APC/APNI; graded species;
#'  taxa not identified to species), and indicates these names only have a
#'  genus-rank match.
#' 
#' Notes:
#' 
#' - If you will be running the function APCalign::create_taxonomic_update_lookup
#'  many times, it is best to load the taxonomic resources separately using
#'   resources <- load_taxonomic_resources(), then add the argument
#'    resources = resources
#' - The name Banksia cerrata only aligns to the genus (`Banksia sp.
#'  \[Banksia cerrata]`), as the fuzzy matching algorithm does not allow the
#'  first letter of the genus or species epithet to change.
#' - With this function you have the option of changing the fuzzy matching
#'  parameters. The defaults, with fuzzy matches only allowing changes of 3
#'   (or fewer) characters AND 20% (or less) of characters has been carefully
#'    calibrated to catch just about all typos, but very, very rarely mis-align
#'     a name. If you wish to introduce less conservative fuzzy matching it is
#'      recommended you manually check the aligned names.
#' - It is recommended that you begin with imprecise_fuzzy_matches = FALSE (the
#'  default), as quite a few of the less precise fuzzy matches are likely to be
#'   erroneous. This argument should be turned on only if you plan to check all
#'    alignments manually.
#' - The argument identifier allows you to add a fix text string to all genus-
#'  and family- level names, such as identifier = "Royal NP" would return "Acacia
#'   sp. \[Royal NP]".
#'
#' @param original_name A list of names to query for taxonomic alignments.
#' @param output (optional) A CSV file in which to keep the results, so that
#'  names are only aligned once. If the file already exists (written by an
#'  earlier call to `align_taxa()`), names already in it (for the same
#'  `identifier`) are read from it rather than aligned again, and the new names
#'  are added to it. The file does not record the other arguments, so use the
#'  same settings and `resources` with a given file. Requires the 'readr'
#'  package.
#' @param full Parameter to determine how many columns are output
#' @param resources the taxonomic resources used to align the taxa names.
#'  Loading this can be slow, so call \code{\link{load_taxonomic_resources}}
#'  separately to greatly speed this function up and pass the resources in.
#' @param quiet Logical to indicate whether to display messages while
#'  aligning taxa.
#' @param fuzzy_abs_dist The number of characters allowed to be different for a
#'  fuzzy match.
#' @param fuzzy_rel_dist The proportion of characters allowed to be different
#'  for a fuzzy match. 
#' @param fuzzy_matches Fuzzy matches are turned on as a default. 
#' The relative and absolute distances allowed for fuzzy matches to species and
#'  infraspecific taxon names are defined by the parameters `fuzzy_abs_dist`
#'  and `fuzzy_rel_dist`
#' @param imprecise_fuzzy_matches Imprecise fuzzy matches uses the
#'  fuzzy matching function with lenient levels set (absolute distance of
#'  5 characters; relative distance = 0.25). 
#'  It offers a way to get a wider range of possible names, possibly
#'  corresponding to very distant spelling mistakes. 
#' This is FALSE as default and all outputs should be checked as it often
#'  makes erroneous matches.
#' @param APNI_matches Name matches to the APNI (Australian Plant Names Index)
#'  are turned on as a default.
#' @param identifier A dataset, location or other identifier, added to the
#'  aligned names of taxa only aligned to a genus or family. Either a single
#'  value or one value per name; defaults to NA.
#'
#' @return A tibble with one row per input name, with columns original_name,
#'   cleaned_name, aligned_name, taxonomic_dataset, taxon_rank, aligned_reason
#'   and alignment_code; `full = TRUE` adds the others below.
#' - original_name: the original plant name input.
#' - aligned_name: the APC or APNI name the original name was aligned to; for
#'   names only aligned to a genus or family, that name followed by `sp.` (`x`
#'   for hybrids) and the original name (and identifier) in square brackets.
#' - taxonomic_dataset: the source of the aligned names (APC or APNI).
#' - taxon_rank: the taxonomic rank of the aligned name.
#' - aligned_reason: the explanation of a specific taxon name alignment
#'   (from an original name to an aligned name).
#' - alignment_code: a code that accompanies the aligned_reason, indicating the
#'   relative sequence of the match during the alignment process.
#' - cleaned_name: original name with punctuation and infraspecific taxon
#'   designation terms standardised by the function standardise_names;
#'   streamlines exact matches.
#' - stripped_name: cleaned name with punctuation and infraspecific taxon
#'   designation terms removed by the function strip_names;
#'   improves fuzzy matches. 
#' - stripped_name2: cleaned name with punctuation, infraspecific taxon
#'   designation terms, and other filler words removed by
#'   the function `strip_names_extra`;
#'   required for matches to `first two word` and `first three words`.
#' - trinomial: the first three words in `stripped_name2`, required for matches
#'   that ignore all other text in the original_name;
#'   improves phrase name matches.
#' - binomial: the first two words in `stripped_name2`, required for matches
#'   that ignore all other text in the original_name;
#'   improves phrase name matches.
#' - fuzzy_match_genus: fuzzy match of genus column to best match among
#'   APC-accepted names;
#'   required for fuzzy matches of genus-rank names.
#' - fuzzy_match_genus_synonym: fuzzy match of genus column to best match among
#'   APC-synonymous names, only considering different matches to those documented
#'   under APC-accepted genera; required for fuzzy matches of genus-rank names.
#' - fuzzy_match_genus_APNI: fuzzy match of genus column to best match among
#'   APNI names, only considering different matches to those documented under
#'   APC-accepted and APC-known genera; required for fuzzy matches of
#'   genus-rank names.
#' - fuzzy_match_family: fuzzy match of genus column to best match among
#'   APC-accepted family names; required for fuzzy matches of family-rank names.
#' - fuzzy_match_family_synonym: fuzzy match of genus column to best match
#'   among APC-synonymous family names; required for fuzzy matches of
#'   family-rank names.
#' - fuzzy_match_cleaned_APC: fuzzy match of stripped_name to APC-accepted
#'   names; created for yet-to-be-aligned names at the match step 05a
#'   in the function `match_taxa`.
#' - fuzzy_match_cleaned_APC_synonym: fuzzy match of stripped_name to
#'   APC-synonymous names; created for yet-to-be-aligned names at the
#'   match step 05b in the function `match_taxa`.
#' - fuzzy_match_cleaned_APC_imprecise: imprecise fuzzy match of stripped_name
#'   to APC-accepted names; created for yet-to-be-aligned names at the
#'   match step 07a in the function `match_taxa`.
#' - fuzzy_match_cleaned_APC_synonym_imprecise: imprecise fuzzy match of
#'   stripped_name to APC-synonymous names; created for yet-to-be-aligned names
#'   at the match step 07b in the function `match_taxa`.
#' - fuzzy_match_binomial: fuzzy match of binomial column to best match among
#'   APC-accepted names; created for yet-to-be-aligned names at
#'   match step 10c in the function `match_taxa`.
#' - fuzzy_match_binomial_APC_synonym: fuzzy match of binomial column to best
#'   match among APC-synonymous names; created for yet-to-be-aligned names at
#'   match step 10d in the function `match_taxa`.
#' - fuzzy_match_trinomial: fuzzy match of trinomial column to best match
#'   among APC-accepted names; created for yet-to-be-aligned names at
#'   match step 09c in the function `match_taxa`.
#' - fuzzy_match_trinomial_synonym: fuzzy match of trinomial column to best
#'   match among APC-synonymous names; created for yet-to-be-aligned names at
#'   match step 09d in the function `match_taxa`.
#' - fuzzy_match_cleaned_APNI: fuzzy match of stripped_name to APNI names;
#'   created for yet-to-be-aligned names at the match step 11a in the
#'   function `match_taxa`.
#' - fuzzy_match_cleaned_APNI_imprecise: imprecise fuzzy match of
#'   cleaned_name to APNI names; created for yet-to-be-aligned names
#'   at the match step 11b in the function `match_taxa`.
#' 
#' @export
#'
#' @examples
#' \donttest{
#' 
#' resources <- load_taxonomic_resources()
#' 
#' # example 1
#' align_taxa(c("Poa annua", "Abies alba"), resources=resources)
#' 
#' # example 2
#' input <- c("Banksia serrata", "Banksia serrate", "Banksia cerrata", 
#' "Banksia serrrrata", "Dryandra sp.", "Banksia big red flowers")
#' 
#' aligned_taxa <-
#'   APCalign::align_taxa(
#'     original_name = input,
#'     identifier = "APCalign test",
#'     full = TRUE,
#'     resources=resources
#'   ) 
#'   
#' }
#'
#'
#' @seealso
#' \code{\link{load_taxonomic_resources}}
#'
#' @family taxonomic alignment functions
#'
#' @rdname align_taxa
#'
#' 
align_taxa <- function(original_name,
                       output = NULL,
                       full = FALSE,
                       resources = load_taxonomic_resources(),
                       quiet = FALSE,
                       fuzzy_abs_dist = 3, 
                       fuzzy_rel_dist = 0.2, 
                       fuzzy_matches = TRUE, 
                       imprecise_fuzzy_matches = FALSE, 
                       APNI_matches = TRUE,
                       identifier = NA_character_) {
  
  if(is.null(resources)){
    message("Not finding taxonomic resources; check internet connection?")
    return(NULL)
  }

  # One row per input name, paired with its identifier. `as.character()`
  # accepts factors, and a column read in as all-NA logical.
  input <- name_identifier_pairs(original_name, identifier)

  if (!is.null(output))
    rlang::check_installed("readr", reason = "to read and write the `output` file.")

  if(!quiet)
    message("Checking alignments of ",
            dplyr::n_distinct(input$original_name, na.rm = TRUE),
            " taxa\n")

  # Alignments saved by an earlier run with the same `output` file
  cached <- read_output_file(output, align_taxa_cache_columns, quiet)

  # The names still to align: each distinct name/identifier pair once, leaving
  # out names that are missing, blank once standardised, or already cached
  new <- input %>%
    dplyr::filter(!is.na(original_name)) %>%
    dplyr::distinct() %>%
    dplyr::filter(standardise_names(original_name) != "") %>%
    dplyr::anti_join(cached, by = c("original_name", "identifier"))

  if (!is.null(output) && !quiet && nrow(cached) > 0) {
    message(
      "  -> ", cli::col_blue(nrow(input %>% dplyr::semi_join(cached, by = c("original_name", "identifier")) %>% dplyr::distinct())),
      " names already aligned in the output file; ",
      cli::col_blue(nrow(new)), " taxa yet to be checked"
    )
  }

  taxa <- align_new_taxa(new, resources, quiet, fuzzy_abs_dist, fuzzy_rel_dist,
                         fuzzy_matches, imprecise_fuzzy_matches, APNI_matches)

  if (full == TRUE) {
    taxa <-
      taxa %>%
      dplyr::select(-genus, -known, -checked) %>%
      dplyr::select(original_name, cleaned_name, aligned_name,
                    taxonomic_dataset, taxon_rank, aligned_reason,
                    alignment_code, dplyr::everything())
  } else {
     taxa <-
      taxa %>%
      dplyr::select(original_name, cleaned_name, aligned_name,
                    taxonomic_dataset, taxon_rank, aligned_reason,
                    alignment_code, identifier)
  }

  taxa <- dplyr::bind_rows(cached, taxa)

  ## save outputs to file, useful for caching results: every name aligned so
  ## far, not only this call's, so the file grows from run to run
  if (!is.null(output)) {
    dir.create(dirname(output), FALSE, TRUE)
    readr::write_csv(taxa, output, na = "")
    if(!quiet)
      message("  - output saved in file: ", output)
  }

  # Assemble output in the order of the input
  # by joining results into a tibble with inputs as column
  input %>%
    dplyr::left_join(by = c("original_name", "identifier"), taxa) %>%
    # can remove column identifier now that matches are complete
    dplyr::select(-identifier)
}

# Align the distinct name/identifier pairs in `new` (a tibble with columns
# `original_name` and `identifier`), returning one row for each in the wide
# working format of `match_taxa()`.
align_new_taxa <- function(new, resources, quiet, fuzzy_abs_dist,
                           fuzzy_rel_dist, fuzzy_matches,
                           imprecise_fuzzy_matches, APNI_matches) {
  # create list, will have two elements: tocheck, checked
  taxa <- list()

  taxa[["tocheck"]] <-
    dplyr::tibble(
      # columns in the order earlier versions gave them, which `full = TRUE`
      # output keeps
      original_name = new$original_name,
      cleaned_name = NA_character_,
      aligned_name = NA_character_,
      taxonomic_dataset = NA_character_,
      identifier = new$identifier,
      known = FALSE,
      checked = FALSE,
      stripped_name = NA_character_,
      stripped_name2 = NA_character_,
      trinomial = NA_character_,
      binomial = NA_character_,
      genus = NA_character_,
      aligned_reason = NA_character_,
      fuzzy_match_genus = NA_character_,
      fuzzy_match_genus_synonym = NA_character_,
      fuzzy_match_genus_APNI = NA_character_,
      fuzzy_match_family = NA_character_,
      fuzzy_match_family_synonym = NA_character_,
      fuzzy_match_binomial = NA_character_,
      fuzzy_match_binomial_APC_synonym = NA_character_,
      fuzzy_match_trinomial = NA_character_,
      fuzzy_match_trinomial_synonym = NA_character_,
      fuzzy_match_cleaned_APC = NA_character_,
      fuzzy_match_cleaned_APC_synonym = NA_character_,
      fuzzy_match_cleaned_APNI = NA_character_,
      fuzzy_match_cleaned_APC_imprecise = NA_character_,
      fuzzy_match_cleaned_APC_synonym_imprecise = NA_character_,
      fuzzy_match_cleaned_APNI_imprecise = NA_character_,
      taxon_rank = NA_character_,
      alignment_code = NA_character_
    )

  if (nrow(taxa$tocheck) == 0) {
    return(taxa$tocheck)
  }

  # a simple message that indicates the number of perfect matches
  if (!quiet) {
    perfect_matches <- taxa$tocheck %>%
      dplyr::filter(original_name %in% resources$APC_accepted$canonical_name) %>%
      dplyr::distinct(original_name) %>%
      nrow()

    synonym_matches <- taxa$tocheck %>%
      dplyr::filter(original_name %in% resources$APC_synonyms$canonical_name) %>%
      dplyr::filter(!original_name %in% resources$APC_accepted$canonical_name) %>%
      dplyr::distinct(original_name) %>%
      nrow()

    message(
      "  -> of these ",
      cli::col_blue(perfect_matches),
      " names have a perfect match to an accepted scientific name in the APC, and ",
      cli::col_blue(synonym_matches),
      " names have a perfect match to a synonym in the APC.
      Alignments being sought for remaining names."
    )
  }

  # do the actual matching
  match_taxa(taxa, resources, fuzzy_abs_dist, fuzzy_rel_dist, fuzzy_matches,
             imprecise_fuzzy_matches, APNI_matches) %>%
    # reassemble
    dplyr::bind_rows() %>%
    dplyr::mutate(known = !is.na(aligned_name))
}

# Pair each input name with its identifier, as a tibble with one row per
# input. `identifier` is either a single value or one per name.
name_identifier_pairs <- function(original_name, identifier) {
  original_name <- as.character(original_name)
  if (!length(identifier) %in% c(1L, length(original_name))) {
    stop(
      "`identifier` must be a single value or have one value per name (",
      length(original_name), "), not ", length(identifier), ".",
      call. = FALSE
    )
  }
  dplyr::tibble(
    original_name = original_name,
    identifier = rep_len(as.character(identifier), length(original_name))
  )
}

# The columns an `align_taxa()` output file must have to be reused.
align_taxa_cache_columns <- c(
  "original_name", "identifier", "aligned_name", "taxonomic_dataset",
  "taxon_rank", "aligned_reason", "alignment_code"
)

# Read the results an earlier run saved to `output`, to reuse them. Returns a
# tibble of character columns (except the numeric ones listed below), empty
# if there is no file yet. Files from APCalign 2.0.1 and earlier have no
# `identifier` column (their names are read as having none) and have the
# `checked` and `known` columns those versions wrote, which are dropped.
read_output_file <- function(output, required, quiet = FALSE) {
  if (is.null(output) || !file.exists(output)) {
    return(dplyr::tibble(original_name = character(0), identifier = character(0)))
  }

  rlang::check_installed("readr", reason = "to read the `output` file.")
  if(!quiet)
    message("  - reading existing data from ", output)

  cached <- readr::read_csv(
    output,
    col_types = readr::cols(.default = readr::col_character()),
    na = c("", "NA")
  )
  numeric_columns <- intersect(c("row_number", "number_of_collapsed_taxa"), names(cached))
  cached[numeric_columns] <- lapply(cached[numeric_columns], as.numeric)
  if (!"identifier" %in% names(cached)) cached$identifier <- NA_character_
  cached <- cached %>% dplyr::select(-dplyr::any_of(c("checked", "known")))

  missing <- setdiff(required, names(cached))
  if (length(missing) > 0) {
    stop(
      "The `output` file ", output, " already exists but was not written by ",
      "this function: it lacks the column(s) ", paste(missing, collapse = ", "),
      ". Choose another file name, or delete it.",
      call. = FALSE
    )
  }
  cached
}

# function moves taxa from tocheck to checked
redistribute <- function(data) {
  data[["checked"]] <- dplyr::bind_rows(data[["checked"]],
                                        data[["tocheck"]] %>% 
                                           dplyr::filter(checked))
  
  data[["tocheck"]] <-
    data[["tocheck"]] %>% dplyr::filter(!checked)
  data
}

