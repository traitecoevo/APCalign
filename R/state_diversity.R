

#' Process Geographic Data and Return State Level Species Origin and Diversity Counts
#'
#' This function processes the geographic data available in the current Australian Plant Census and returns state level diversity for native, introduced and more complicated species origins.
#'
#'
#' @param type_of_data A character string indicating the type of data to be used. The default is "stable".  "current" downloads the file from APC directly and remakes the calculations from the most recent version.
#'
#' @param ver Version of the database (for the stable option only)
#'
#' @return A data frame with columns representing each state and rows representing each species. The values in each cell represent the origin of the species in that state.
#'
#' @import dplyr
#' @import stringr
#'
#' @examples
#' create_species_state_origin_matrix()
#'
#' @export
create_species_state_origin_matrix <-
  function(type_of_data = "stable", ver = "0.0.1.9000") {
    apc <- dataset_access_function(ver = ver, type = type_of_data)
      apc_species <-
        dplyr::filter(apc$APC,
                      taxonRank == "Species" & taxonomicStatus == "accepted")
    #seperate the states
    sep_state_data <-
      stringr::str_split(unique(apc_species$taxonDistribution), ",")
    
    #get unique places
    all_codes <- unique(stringr::str_trim(unlist(sep_state_data)))
    apc_places <-
      unique(stringr::word(all_codes[!is.na(all_codes)], 1, 1))
    
    #make a table to fill in
    data.frame(col.names = apc_places)
    species_df <- dplyr::tibble(species = apc_species$scientificName)
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




#' Calculate state-level diversity for native, introduced, and more complicated species origins
#'
#' This function calculates state-level diversity for native, introduced, and more complicated species origins based on the geographic data available in the current Australian Plant Census.
#'
#' @param state A character string indicating the Australian state or territory to calculate the diversity for. Default is "NSW". Possible values are "NSW", "NT", "Qld", "WA", "ChI", "SA", "Vic", "Tas", "ACT", "NI", "LHI", "MI", "HI", "MDI", "CoI", "CSI", and "AR".
#' @param type_of_data A character string indicating the type of data to use for the calculation. Default is "stable". Possible values are "stable" and "current".
#' @param ver A character string indicating the version of the data to use for the calculation in the case of "stable" source. Default is "0.0.1.9000".
#'
#' @return A tibble of diversity counts for the specified state or territory, including native, introduced, and more complicated species origins.
#' The tibble has three columns: "origin" indicating the origin of the species, "state" indicating the Australian state or territory, and "num_species" indicating the number of species for that origin and state.
#'
#' @export
#'
#' @examples
#' state_diversity_counts(state = "NSW", type_of_data = "current", ver = "0.0.1.9000")
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
  type_of_data = "stable",
  ver = "0.0.1.9000") {
    test <-
      create_species_state_origin_matrix(ver = ver, type_of_data = type_of_data)
    test2 <- test[test[[state]] != "not present", ]
    state_table <- table(test2[[state]])
    return(tibble(origin=names(state_table), state=state, num_species=state_table))
  }





