
## Negative lookahead for an infraspecific rank marker.
##
## `affinis` is both an affinity qualifier ("Acacia affinis dealbata" = a taxon
## resembling Acacia dealbata) and a legitimate species epithet. The two are
## told apart by what follows: an epithet can be qualified by a rank marker,
## an affinity qualifier cannot. So `Gomphrena affinis subsp. pilbarensis` --
## an accepted APC name -- must keep its `affinis`.
##
## The marker spellings listed here are the ones that can still be present at
## the point this is applied; `standardise_names()` normalises some of them
## (forma/form -> f.) only further down its pipeline. Shared with the affinity
## predicates in `match_taxa()` so the two cannot drift apart.
not_before_rank_marker <-
  "(?!\\s+(?:subsp|ssp|subvar|var|forma|form|ser|series|cv|f)\\.?(?:\\s|$))"

#' @title Standardise taxon names
#' 
#' @description
#' Standardises taxon names by performing a series of text substitutions to 
#' remove common inconsistencies in taxonomic nomenclature.
#' 
#' The function takes a character vector of taxon names as input and 
#' returns a character vector of taxon names using standardised taxonomic syntax
#' as output. 
#'
#' @details
#' -  It removes stray punctuation at the start and end of a character string.
#' -  It standardises unusual characters and symbols to ASCII equivalents.
#' -  It standardises taxon rank abbreviations and qualifiers (subsp., var., f.),
#'  as people use many variants of these terms. 
#' -  It standardises or removes a few additional filler words used within
#'  taxon names (affinis becomes aff.; s.l. and s.s. are removed).
#'
#'  `affinis` is only treated as an affinity qualifier where it cannot be a
#'  species epithet: it is left alone at the end of a name
#'  (`Acacia affinis`) and before a rank marker
#'  (`Gomphrena affinis subsp. pilbarensis`).
#'
#' @param taxon_names A character vector of taxon names that need to be standardised.
#'
#' @return A character vector of standardised taxon names.
#'
#' @examples
#' standardise_names(c("Quercus suber",
#'                     "Eucalyptus sp.",
#'                     "Eucalyptus spp.",
#'                     "Agave americana var. marginata",
#'                     "Agave americana v marginata",
#'                     "Notelaea longifolia forma longifolia",
#'                     "Notelaea longifolia f longifolia",
#'                     "Acacia affinis dealbata",
#'                     "Gomphrena affinis subsp. pilbarensis"))
#' @export
standardise_names <- function(taxon_names) {
  taxon_names %>%
    ## remove ? throughout
    gsub_perl("\\?", "") %>%

    ## remove all punct and symbols at start of string
    ## this combination should catch almost everything
    ## it is essential there are no stray characters at the start of strings
    ## for fuzzy-matching to work once the reference list is split by first-character
    stringr::str_replace("^[~!@#$%^&*()_+-=`;',./<>?:{}|]+", "") %>%
    stringr::str_replace("^[:punct:]+", "") %>%
    
    ## remove * at end of string
    gsub_perl("\\*$", "") %>%

    ## replace hybrid x marker with standard x 
    ## for certain hybrid x's that aren't dealt with below
    gsub_perl("\u00D7", "x") %>%

    ## hybrid markers and other non-standard characters used are replaced with 
    ## the standard equivalent (e.g. x, \)
    stringi::stri_trans_general("Any-Latin; Latin-ASCII") %>%

    ## add spaces between letters and /
    gsub_perl("([a-zA-Z])/([a-zA-Z])", "\\1 / \\2") %>%
  
    ## remove ".."
    gsub_perl("\\.\\.", "\\.") %>%

    ## Weird formatting
    gsub_perl("[\\n\\t]", " ") %>%
    gsub_perl("[\\n\\t]", " ") %>%
    
    ## Remove spaces before or after brackets
    gsub_perl("\\ \\)", "\\)") %>%
    gsub_perl("\\(\\ ", "\\(") %>%
    
    ## Capitalise first letter
    gsub_perl("^([a-z])", "\\U\\1") %>%
    
    ## sp. not sp or spp
    gsub_perl("\\ssp(\\s|$)",   " sp. ") %>%
    gsub_perl("\\sspp.(\\s|$)", " sp. ") %>%
    gsub_perl("\\sspp(\\s|$)",  " sp. ") %>%
    
    ## subsp. not ssp, ssp., subsp or sub sp.
    gsub_perl("\\sssp(\\s|$)",     " subsp. ") %>%
    gsub_perl("\\sssp.(\\s|$)",    " subsp. ") %>%
    gsub_perl("\\ssubsp(\\s|$)",   " subsp. ") %>%
    gsub_perl("\\ssub sp.(\\s|$)", " subsp. ") %>%
    
    ## var. not var or v or v.
    gsub_perl("\\svar(\\s|$)",   " var. ") %>%
    gsub_perl("\\sv(\\s|$|\\.)", " var. ") %>%
    
    ## aff. not affin, aff affn affinis
    ## `affinis` is also a legitimate species epithet, so it is only rewritten
    ## when it sits mid-name and is not the epithet of an infraspecific name:
    ## a trailing `affinis` is left alone (`Acacia affinis`), and so is one
    ## followed by a rank marker (`Gomphrena affinis subsp. pilbarensis`).
    gsub_perl("\\saffin(\\s|$)",    " aff. ") %>%
    gsub_perl("\\saff(\\s|$)",      " aff. ") %>%
    gsub_perl("\\saffn(\\s|$|\\.)", " aff. ") %>%
    gsub_perl(paste0("\\saffinis", not_before_rank_marker, "(\\s)"),  " aff. ") %>%
    
    ## f. not forma or form or form. or f
    gsub_perl("\\sforma(\\s|$)",       " f. ") %>%
    gsub_perl("\\sform(\\s|$|\\.\\s)", " f. ") %>%
    gsub_perl("\\sf(\\s|$)",           " f. ") %>%
    
    ## remove " ms" if present
    gsub_perl("\\sms(\\s|$|\\.\\s)", " ") %>%
    
    ## remove " s.l" or " s.s." or "s s " or " s l " if present
    gsub_perl("\\ssl(\\s|$)", " ") %>%
    gsub_perl("\\ss\\.l\\.(\\s|$)", " ") %>%
    gsub_perl("\\sss(\\s|$)", "") %>%
    gsub_perl("\\ss\\.s\\.(\\s|$)", " ") %>%
    gsub_perl("\\ss\\ss(\\s|$)", " ") %>%
    gsub_perl("\\ss\\sl(\\s|$)", " ") %>%
    gsub_perl("\\ss\\.\\ss(\\s|$|\\.\\s)", " ") %>%
    gsub_perl("\\ss\\.\\sl(\\s|$|\\.\\s)", " ") %>%
    gsub_perl("\\ss(\\.\\s|\\s)lat(\\s|$|\\.\\s)", " ") %>%
    gsub_perl("\\ssensu\\slato(\\s|$|\\.\\s)", " ") %>%
    gsub_perl("\\ssensu\\sstricto(\\s|$|\\.\\s)", " ") %>%
    gsub_perl("(\\s|\\()s\\.lat\\.(\\s|\\))", "") %>%
    gsub_perl("(\\s|\\()s\\.str\\.(\\s|\\))", "") %>%
    
    ## standardise "ser"
    gsub_perl("\\sser(\\s|\\.\\s)", " ser. ") %>%
    gsub_perl("\\sseries(\\s|\\.\\s)", " ser. ") %>%

    ## clean white space
    stringr::str_squish()
}

#' Extract Genus
#' 
#' This function extracts the genus component of a scientific name. 
#' It identifies if the genus is/is not a hybrid. For a hybrid genus,
#' the first two words of the taxon name are extracted (e.g. "x Cynochloris"),
#' while for a non-hybrid genus just the first word is extracted (e.g. "Banksia").
#'
#' @param taxon_name A character vector of scientific names.
#'
#' @return The genus for a scientific name.
#'
#' @examples
#' extract_genus(c("Banksia integrifolia", "Acacia longifolia"))
#' 
#' @keywords internal
#' @noRd
extract_genus <- function(taxon_name) {

  taxon_name <- standardise_names(taxon_name)
  extract_genus_clean(taxon_name)
}

# Fast genus extraction for already-clean canonical names (e.g. from APC/APNI).
# Skips standardise_names() since the input is known to be clean.
#' @noRd
extract_genus_clean <- function(taxon_name) {
  genus <- stringr::str_split_i(taxon_name, " |\\/", 1) %>% stringr::str_to_sentence()

  # Deal with names that begin with x,
  # e.g."x Taurodium x toveyanum" or "x Glossadenia tutelata"
  i <- !is.na(genus) & genus == "X"

  genus[i] <-
    stringr::str_split_i(taxon_name[i], " |\\/", 2) %>% stringr::str_to_sentence() %>% paste("x", .)

  genus
}


#' @title Standardise taxon ranks
#' 
#' @description
#' Standardise taxon ranks from Latin into English.
#'
#' @details
#' The function takes a character vector of Latin taxon ranks as input and 
#' returns a character vector of taxon ranks using standardised English terms.
#'
#' @param taxon_rank A character vector of Latin taxon ranks.
#'
#' @return A character vector of English taxon ranks.
#'
#' @examples
#' standardise_taxon_rank(c("regnum", "kingdom", "classis", "class"))
#' @export
standardise_taxon_rank <- function(taxon_rank) {
  gsub_fixed <- function(x, find, replace) {
    gsub(find, replace, x, fixed = TRUE)
  }

  # Last-word replacement, for the two terms that can. "sectio" is a literal
  # prefix of its own translation ("section"), and "forma" sits inside
  # "informal"; replacing either as a bare substring silently corrupts those
  # values ("section" -> "sectionn", "informal" -> "informl"). Matching only
  # when the term ends the string still catches the prefixed ranks that do need
  # translating ("subsectio", "subforma"), and the lookahead keeps any trailing
  # whitespace, since taxon_rank is not trimmed upstream.
  g <- function(x, find, replace) {
    stringr::str_replace(x, paste0(find, "(?=\\s*$)"), replace)
  }

  taxon_rank %>%
  stringr::str_to_lower() %>%
  gsub_fixed("regnum", "kingdom") %>%
  gsub_fixed("classis", "class") %>%
  gsub_fixed("ordo", "order") %>%
  gsub_fixed("familia", "family") %>%
  gsub_fixed("varietas", "variety") %>%
  gsub_fixed("forma", "form") %>%
  gsub_fixed("sectio", "section")
}
