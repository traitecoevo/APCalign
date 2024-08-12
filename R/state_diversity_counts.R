#' @title State- and territory-level diversity
#'
#' @description
#' For Australian states and territories, use geographic distribution data from
#' the APC to calculate state-level diversity for native, introduced,
#' and more complicated species origins
#'
#' @family diversity methods
#' @param state A character string indicating the Australian state or
#'  territory to calculate the diversity for. Possible values are "NSW", "NT",
#'  "Qld", "WA", "ChI", "SA", "Vic", "Tas", "ACT", "NI", "LHI", "MI", "HI",
#'  "MDI", "CoI", "CSI", and "AR".
#' @param resources the taxonomic resources required to make the summary
#'  statistics.  loading this can be slow, so call load_taxonomic_resources
#'  separately to greatly speed this function up and pass the resources in.
#'
#' @return A tibble of diversity counts for the specified state or territory,
#'  including native, introduced, and more complicated species origins.
#' The tibble has three columns: "origin" indicating the origin of the
#'  species, "state" indicating the Australian state or territory, and
#'  "num_species" indicating the number of species for that origin and state.
#'
#' @seealso \code{\link{load_taxonomic_resources}}
#'
#' @export
#'
#' @examples
#'  \donttest{state_diversity_counts(state = "NSW")}
state_diversity_counts <- function(state, resources = load_taxonomic_resources()) {
  if (is.null(resources)) {
    message("Not finding taxonomic resources; check internet connection?")
    return(NULL)
  }
  
  valid_inputs <- c(
    "NSW",
    "NT",
    "Qld",
    "WA",
    "ChI",
    "NSW",
    "SA",
    "Vic",
    "Tas",
    "ACT",
    "NI",
    "LHI",
    "MI",
    "HI",
    "MDI",
    "CoI",
    "CSI",
    "AR",
    "CaI"
  )
  if (!(state %in% valid_inputs)) {
    stop(paste(
      "Invalid str_input:",
      state,
      ". Expected one of:",
      paste(valid_inputs, collapse = ", ")
    ))
  }
  test <-
    create_species_state_origin_matrix(resources = resources)
  test2 <- test[test[[state]] != "not present", ]
  state_table <- table(test2[[state]])
  return(dplyr::tibble(
    origin = names(state_table),
    state = state,
    num_species = state_table
  ))
}



#' @noRd
create_apc_genus_family_lookup <-
  function(resources) {
    apc_s <- dplyr::filter(resources$APC, taxon_rank == "species")
    dplyr::tibble(genus = word(apc_s$accepted_name_usage, 1, 1),
                  family = apc_s$family) |>
      dplyr::distinct() -> lu
    return(lu)
  }

#' @title Lookup Family by Genus from APC
#'
#' @description
#' Retrieve the family name for a given genus using taxonomic data from the
#' Australian Plant Census (APC).
#'
#' @param genus A character vector of genus names for which to retrieve the
#'  corresponding family names.
#' @param resources The taxonomic resources required to make the lookup. 
#'  Loading this can be slow, so call \code{\link{load_taxonomic_resources}} 
#'  separately to speed up this function and pass the resources in.
#'
#' @return A data frame with two columns: "genus", indicating the genus name, 
#'  and "family", indicating the corresponding family name from the APC.
#'
#' @seealso \code{\link{load_taxonomic_resources}}, \code{\link{create_apc_genus_family_lookup}}
#'
#' @export
#'
#' @examples
#'  \donttest{get_apc_genus_family_lookup(genus = c("Acacia", "Eucalyptus"))}
get_apc_genus_family_lookup <-
  function(genus, resources = load_taxonomic_resources()) {
    if (is.null(resources)) {
      message("Not finding taxonomic resources; check internet connection?")
      return(NULL)
    }
    fam_lu <- create_apc_genus_family_lookup(resources = resources)
    lu <- dplyr::tibble(genus = genus) %>%
      dplyr::left_join(fam_lu, by = "genus")
    if (any(is.na(lu$family))) warning("some non-matches with the APC accepted genus list, check the formatting of your genus vector.")
    return(lu)
  }
