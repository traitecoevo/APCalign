




#' Process geographic data and return state level species origin and diversity counts
#'
#' This function processes the geographic data available in the current or any version of the Australian Plant Census and returns state level diversity for native, introduced and more complicated species origins.
#'
#'
#' @family diversity methods
#' @param resources the taxonomic resources required to make the summary statistics.  Loading this can be slow, so call load_taxonomic_resources separately to greatly speed this function up and pass the resources in.
#'
#' @return A data frame with columns representing each state and rows representing each species. The values in each cell represent the origin of the species in that state.
#'
#' @import dplyr
#' @import stringr
#'
#' @seealso \code{\link{load_taxonomic_resources}}
#'
#' @examples
#' create_species_state_origin_matrix()
#'
#' @export
create_species_state_origin_matrix <-
  function(resources = load_taxonomic_resources()) {
    apc_species <-
      dplyr::filter(resources$APC,
                    taxonRank == "Species" &
                      taxonomicStatus == "accepted")
    #seperate the states
    sep_state_data <-
      stringr::str_split(unique(apc_species$taxonDistribution), ",")
    
    #get unique places
    all_codes <- unique(stringr::str_trim(unlist(sep_state_data)))
    apc_places <-
      unique(stringr::word(all_codes[!is.na(all_codes)], 1, 1))
    
    #make a table to fill in
    data.frame(col.names = apc_places)
    species_df <-
      dplyr::tibble(species = apc_species$scientificName)
    for (i in 1:length(apc_places)) {
      species_df <-
        dplyr::bind_cols(species_df, NA, .name_repair = "minimal")
    }
    names(species_df) <- c("species", apc_places)
    
    #look for all possible entries after each state
    state_parse_and_add_column <-
      function(species_df, state, apc_species) {
        # print(all_codes[grepl(state,all_codes)]) # checking for weird ones
        species_df[, state] <- dplyr::case_when(
          grepl(
            paste0("\\b", state, " \\(uncertain origin\\)"),
            apc_species$taxonDistribution
          ) ~ "uncertain origin",
          grepl(
            paste0("\\b", state, " \\(naturalised\\)"),
            apc_species$taxonDistribution
          ) ~ "naturalised",
          grepl(
            paste0("\\b", state, " \\(doubtfully naturalised\\)"),
            apc_species$taxonDistribution
          ) ~ "doubtfully naturalised",
          grepl(
            paste0("\\b", state, " \\(native and naturalised\\)"),
            apc_species$taxonDistribution
          ) ~ "native and naturalised",
          grepl(
            paste0("\\b", state, " \\(formerly naturalised\\)"),
            apc_species$taxonDistribution
          ) ~ "formerly naturalised",
          grepl(
            paste0("\\b", state, " \\(presumed extinct\\)"),
            apc_species$taxonDistribution
          ) ~ "presumed extinct",
          grepl(
            paste0("\\b", state, " \\(native and doubtfully naturalised\\)"),
            apc_species$taxonDistribution
          ) ~ "native and doubtfully naturalised",
          grepl(
            paste0("\\b", state, " \\(native and uncertain origin\\)"),
            apc_species$taxonDistribution
          ) ~ "native and uncertain origin",
          grepl(paste0("\\b", state), apc_species$taxonDistribution) ~ "native",
          #no entry = native, it's important this is last in the list
          TRUE ~ "not present"
        )
        return(species_df)
      }
    
    #bug checking
    #species_df<-state_parse_and_add_column(species_df,"LHI",apc_species)
    #species_df<-state_parse_and_add_column(species_df,"HI",apc_species)
    
    #go through the states one by one
    for (i in 1:length(apc_places)) {
      species_df <-
        state_parse_and_add_column(species_df, apc_places[i], apc_species)
    }
    return(species_df)
  }




#' Calculate Australian plant state-level diversity for native, introduced, and more complicated species origins
#'
#' This function calculates state-level diversity for native, introduced, and more complicated species origins based on the geographic data available in the current Australian Plant Census.
#'
#' @family diversity methods
#' @param state A character string indicating the Australian state or territory to calculate the diversity for. Default is "NSW". Possible values are "NSW", "NT", "Qld", "WA", "ChI", "SA", "Vic", "Tas", "ACT", "NI", "LHI", "MI", "HI", "MDI", "CoI", "CSI", and "AR".
#' @param resources the taxonomic resources required to make the summary statistics.  loading this can be slow, so call load_taxonomic_resources separately to greatly speed this function up and pass the resources in.
#'
#' @return A tibble of diversity counts for the specified state or territory, including native, introduced, and more complicated species origins.
#' The tibble has three columns: "origin" indicating the origin of the species, "state" indicating the Australian state or territory, and "num_species" indicating the number of species for that origin and state.
#'
#' @seealso \code{\link{load_taxonomic_resources}}
#'
#' @export
#'
#' @examples
#'  \dontrun{state_diversity_counts(state = "NSW")}
state_diversity_counts <- function(state = c(
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
),
resources = load_taxonomic_resources(version = default_version())) {
  test <-
    create_species_state_origin_matrix(resources = resources)
  test2 <- test[test[[state]] != "not present",]
  state_table <- table(test2[[state]])
  return(tibble(
    origin = names(state_table),
    state = state,
    num_species = state_table
  ))
}



#' @noRd
get_apc_genus_family_lookup <-
  function(resources = load_taxonomic_resources()) {
    apc_s <- filter(resourcesAPC,
                    taxonRank == "Species")
    tibble(genus = word(apc_s$scientificName, 1, 1),
           family = apc_s$family) %>%
      distinct() -> lu
    return(lu)
  }


#' @noRd
native_anywhere_in_australia <-
  function(resources = load_taxonomic_resources()) {
    native_lookup <-
      create_species_state_origin_matrix(resources = resources)
    native_lookup$native_anywhere <-
      apply(native_lookup, 1, function(x)
        any(grepl("native", x)))
    return(native_lookup)
  }
