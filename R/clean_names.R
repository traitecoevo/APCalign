
#' Title
#'
#' @return
#' @export
#'
#' @examples
default_version <- function() {"0.0.0.9000"}

#' Checks all taxa within against our list of known species
#' If not found, and update=TRUE, checks the unknown species against
#'
#' @param dataset_id 
#' @param update 
#' @param typos 
#' @param diffchar 
#'
#' @export
#'
#' @examples
metadata_check_taxa <- function(original_name, 
                                output = "taxonomic_updates.csv",
                                max_distance_abs = 3, max_distance_rel = 0.2,
                                ver = default_version()) {
  
  
  cat("Checking alignments of ", length(original_name), "taxa\n")

  if(file.exists(output))
    taxa_raw <- readr::read_csv(output, col_types = cols(checked=col_logical(), known=col_logical(), .default = col_character()))
  else
    taxa_raw <- tibble(original_name=character(0L), cleaned_name = character(0L), aligned_name = character(0L), 
                       source = character(0L), known=logical(0), checked=logical(0))
  # TODO: check taxa has correct columns
  
  # expand list
  taxa <- list()
  
  taxa[["tocheck"]] <- 
    bind_rows(
      taxa_raw, 
      tibble(original_name=subset(original_name, !original_name %in% taxa_raw$original_name) %>% unique(), 
             cleaned_name = NA_character_, stripped_name = NA_character_, aligned_name= NA_character_, source = NA_character_, 
             checked=FALSE, known=FALSE)
    ) %>% 
   mutate(
     cleaned_name = ifelse(is.na(cleaned_name), standardise_names(original_name), cleaned_name),
     stripped_name = ifelse(is.na(stripped_name), strip_names(cleaned_name), stripped_name)
     )

  if(all(taxa$tocheck$checked)){
    message(crayon::red("All taxa are already checked\n"))
    return(invisible(taxa$tocheck));   
  }
  
  # check unknown taxa
  cat(crayon::red(sum(taxa$tocheck$known, na.rm=T)), " names already matched; ")
  cat(crayon::red(sum(taxa$tocheck$checked & !taxa$tocheck$known, na.rm=T)), " names checked but without a match; ")
  cat(crayon::red(sum(!taxa$tocheck$checked)), " taxa  yet to be checked\n")
  
  rebalance <- function(data) {
    data[["checked"]] <- rbind(data[["checked"]], 
                               data[["tocheck"]] %>% filter(checked))

    data[["tocheck"]] <- data[["tocheck"]] %>% filter(!checked)
    data
  }
  
  taxa <- rebalance(taxa)
  
  # Not checking anything ending in `sp.` 
  # Todo: Note, genus in APC? 
  taxa$tocheck <- taxa$tocheck %>% 
    mutate(
      checked = ifelse(!checked & grepl("sp\\.$", cleaned_name), TRUE, checked)
    )
  
  taxa <- rebalance(taxa)
  
  cat("  -> checking for extact matches for ", nrow(taxa$tocheck), " species\n")
  
  resources <- load_taxonomic_resources(ver = ver)

  for(v in c("APC list (accepted)", "APC list (known names)", "APNI names"))  {
    
    # Compare to canonical name
    i <-  match(taxa$tocheck$original_name, resources[[v]]$canonicalName)
    taxa$tocheck$aligned_name <- resources[[v]]$canonicalName[i]
    taxa$tocheck$source[!is.na(i)] <- v
    taxa$tocheck$checked <- !is.na(i)
    
    taxa <- rebalance(taxa)
    
    # Compare to stripped canonical name
    i <-  match(taxa$tocheck$stripped_name, resources[[v]]$stripped_canonical)
    taxa$tocheck$aligned_name <- resources[[v]]$canonicalName[i]
    taxa$tocheck$source[!is.na(i)] <- v
    taxa$tocheck$checked <- !is.na(i)
    taxa <- rebalance(taxa)
    
    # Compare to scientific name
    i <-  match(taxa$tocheck$original_name, resources[[v]]$scientificName)
    taxa$tocheck$aligned_name <- resources[[v]]$canonicalName[i]
    taxa$tocheck$source[!is.na(i)] <- v
    taxa$tocheck$checked <- !is.na(i)
    taxa <- rebalance(taxa)
    
    # Compare to stripped scientific name
    i <-  match(taxa$tocheck$stripped_name, resources[[v]]$stripped_scientific)
    taxa$tocheck$aligned_name <- resources[[v]]$canonicalName[i]
    taxa$tocheck$source[!is.na(i)] <- v
    taxa$tocheck$checked <- !is.na(i)
    taxa <- rebalance(taxa)
  }
  
  # For any remaining species, look for distance based estimates
  cat("  -> checking for fuzzzy matches for ", nrow(taxa$tocheck), " species\n")

  for(i in seq_len(nrow(taxa$tocheck))) {
          
    stripped_name <- taxa$tocheck$stripped_name[i]
    taxa$tocheck$checked[i] <- TRUE
        
    cat("\t", i, "\t", taxa$tocheck$original_name[i])
    for(v in c("APC list (accepted)", "APC list (known names)", "APNI names"))  {
      distance_c <- adist(stripped_name, resources[[v]]$stripped_canonical, fixed=TRUE)[1,]
      min_dist_abs_c <-  min(distance_c)
      min_dist_per_c <-  min(distance_c) / str_length(stripped_name)
      j <- which(distance_c==min_dist_abs_c)
        
      if(
          ## Within allowable number of characters (absolute)
          min_dist_abs_c <= max_distance_abs & 
          ## Within allowable number of characters (relative) 
          min_dist_per_c <= max_distance_rel &
          ## Is a unique solution
          length(j)==1
          ) {
            taxa$tocheck$aligned_name[i] <- resources[[v]]$canonicalName[j]
            taxa$tocheck$source[i] <- v
            break;
        }
      #Todo: suggestions when no match
      
      distance_s <- adist(stripped_name, resources[[v]]$stripped_scientific, fixed=TRUE)[1,]
      min_dist_abs_s <-  min(distance_s)
      min_dist_per_s <-  min(distance_s) / str_length(stripped_name)
      j <- which(distance_s==min_dist_abs_s)
        
      if(
          ## Within allowable number of characters (absolute)
          min_dist_abs_s <= max_distance_abs & 
          ## Within allowable number of characters (relative) 
          min_dist_per_s <= max_distance_rel &
          ## Is a unique solution
          length(j)==1
          ) {
            taxa$tocheck$aligned_name[i] <- resources[[v]]$canonicalName[j]
            taxa$tocheck$source[i] <- v
            break;
      }
      #Todo: suggestions when no match
      
    }
    
    if(is.na( taxa$tocheck$aligned_name[i]))
      cat(crayon::red("\tnot found\n"))
    else
      cat(crayon::green("\tfound:\t"),taxa$tocheck$aligned_name[i],  "\tin ", taxa$tocheck$source[i], "\n")
    
  }
  
  taxa_out <- bind_rows(taxa) %>% 
    mutate(known=!is.na(aligned_name))
  
  write_csv(taxa_out, output)
  invisible(taxa_out)
}

  
#' Title
#'
#' @param aligned_names 
#' @param output 
#' @param ver 
#'
#' @return
#' @export
#'
#' @examples
update_taxonomy <- function(aligned_names,  
                            output = "taxon_list.csv",
                            ver = default_version()) {
  
  preferrred_oder <- c("accepted", "taxonomic synonym", "basionym", "nomenclatural synonym", "isonym", 
                       "orthographic variant", "common name", "doubtful taxonomic synonym", "replaced synonym", 
                       "misapplied", "doubtful pro parte taxonomic synonym", "pro parte nomenclatural synonym", 
                       "pro parte taxonomic synonym", "pro parte misapplied", "excluded", "doubtful misapplied", 
                       "doubtful pro parte misapplied")
  
  resources <- load_taxonomic_resources(ver = version)
  
  taxa_out <- 
    tibble(aligned_name = aligned_names) %>% 
    # match names against names in APC list
    left_join(by = "aligned_name",
      resources$APC %>% filter(!grepl("sp\\.$", canonicalName)) %>% 
              select(aligned_name = canonicalName, taxonIDClean = taxonID, 
                     taxonomicStatusClean = taxonomicStatus, acceptedNameUsageID)
    ) %>%
    distinct() %>%
    mutate(source = ifelse(!is.na(taxonIDClean), "APC", NA)) %>% 
    # Now find accepted names for each name in the list (sometimes they are the same)
    left_join(by = "acceptedNameUsageID",
              resources$APC %>% 
                filter(taxonomicStatus =="accepted") %>% 
                select(acceptedNameUsageID, canonicalName, taxonomicStatus, scientificNameAuthorship, 
                       family, taxonDistribution, taxonRank, ccAttributionIRI)
    ) %>%
    # Some species have multiple matches. We will prefer the accepted usage, but record others if they exists
    # To do this we define the order we want variables to sort by,m with accepted at the top
    mutate(my_order =  forcats::fct_relevel( taxonomicStatusClean, subset(preferrred_oder, preferrred_oder %in%  taxonomicStatusClean))) %>%
    arrange(aligned_name, my_order) %>%
    # For each species, keep the first record (accepted if present) and 
    # record any alternative status to indicate where there was ambiguity
    group_by(aligned_name) %>% 
    mutate(
      alternativeTaxonomicStatusClean = ifelse(taxonomicStatusClean[1] == "accepted", taxonomicStatusClean %>% unique() %>%  subset(. , . !="accepted") %>% paste0(collapse = " | ") %>% na_if(""), NA)
    ) %>%
    slice(1) %>%  
    ungroup() %>% 
    select(-my_order) %>% 
    select(aligned_name, source, taxonIDClean, taxonomicStatusClean, alternativeTaxonomicStatusClean,
           acceptedNameUsageID, canonicalName, scientificNameAuthorship, taxonRank, taxonomicStatus, family, 
           taxonDistribution, ccAttributionIRI)
  
  taxa1 <- 
    taxa_out %>% filter(!is.na(taxonIDClean)) %>% 
    distinct() 
  
  # Now check against APNI for any species not found in APC
  taxa2 <-
    taxa_out %>% filter(is.na(canonicalName))  %>%
    select(aligned_name) %>%
    left_join(by = "aligned_name",
              resources$APNI %>% filter(nameElement != "sp.") %>% select(aligned_name = canonicalName, taxonIDClean = scientificNameID, family, taxonRank)
    ) %>% group_by(aligned_name) %>%
    mutate(
      taxonIDClean = paste(taxonIDClean, collapse = " ") %>% na_if("NA"),
      family = ifelse(n_distinct(family) > 1, NA, family[1])
    ) %>%
    ungroup() %>%
    mutate(
      source =ifelse(is.na(taxonIDClean), NA, "APNI"),
      canonicalName = ifelse(is.na(taxonIDClean), NA, aligned_name),
      taxonomicStatusClean = ifelse(is.na(taxonIDClean), "unknown", "unplaced"),
      taxonomicStatus = taxonomicStatusClean
    )
  
  taxa_out <- 
    bind_rows(
      taxa1,
      taxa2 %>% filter(!is.na(taxonIDClean))
    ) %>%
    arrange(aligned_name) 
  
  write_csv(taxa_out, output)
  
  invisible(taxa_out)
}

#' Title
#'
#' @param ver 
#' @param reload 
#'
#' @return
#' @export
#'
#' @examples

load_taxonomic_resources <- function(ver = default_version(), reload=FALSE) {
  # TODO: replace with latest version as default
  
  if(!exists("taxonomic_resources",  envir = .GlobalEnv)) {
    message(crayon::red("loading object `taxonomic_resources` into global environment"))
    taxonomic_resources <- apcnames::dataset_access_function(ver)
    names(taxonomic_resources) <- c("APC", "APNI")
    
    taxonomic_resources[["genera_accepted"]] <-  
      taxonomic_resources$APC %>% filter(taxonRank %in% c('Genus'), taxonomicStatus == "accepted") 
    
    APC_tmp <- 
      taxonomic_resources$APC %>% 
      filter(taxonRank %in% c('Series', 'Subspecies', 'Species', 'Forma', 'Varietas')) %>% 
      select(canonicalName, scientificName, taxonomicStatus, ID = taxonID) %>% 
      mutate(
        stripped_canonical = strip_names(canonicalName),
        stripped_scientific = strip_names(scientificName)
      ) %>%
      distinct()
    
    taxonomic_resources[["APC list (accepted)"]] <- APC_tmp %>% filter(taxonomicStatus == "accepted")
    taxonomic_resources[["APC list (known names)"]] <- APC_tmp %>% filter(taxonomicStatus != "accepted")
    
    taxonomic_resources[["APNI names"]] <- 
      taxonomic_resources$APNI %>% filter(nameElement != "sp.") %>% 
      select(canonicalName, scientificName, ID = scientificNameID) %>% 
      mutate( taxonomicStatus = "unplaced", 
              stripped_canonical = strip_names(canonicalName),
              stripped_scientific = strip_names(scientificName)
      ) %>%
      distinct() %>% arrange(canonicalName)
    
    assign("taxonomic_resources", taxonomic_resources, envir = .GlobalEnv)
  } 
  
  get0("taxonomic_resources", envir = .GlobalEnv)
}


#' Title
#'
#' @param taxon_names a vector of names to strip
#'
#' @return a vector of stripped names
#' @export
#'
#' @examples
strip_names <- function(taxon_names) {
  taxon_names %>% 
    str_remove_all(" subsp\\.") %>% str_remove_all(" aff\\.")  %>% 
    str_remove_all(" var\\.") %>% str_remove_all(" ser\\.") %>% str_remove_all(" f\\.") %>%
    str_remove_all(" s\\.l\\.") %>% str_remove_all(" s\\.s\\.") %>%
    str_replace_all("[-._()?]", " ") %>% 
    str_squish() %>% 
    tolower() 
}


#' Title
#'
#' @param taxon_names 
#'
#' @return
#' @export
#'
#' @examples
standardise_names <- function(taxon_names) {
  
  f <- function(x, find, replace) {
    gsub(find, replace, x, perl=TRUE)
  }
  
  taxon_names %>%
    ## Weird formattring
    f("[\\n\\t]", " ") %>%

    ## Capitalise first letter
    f("^([a-z])", "\\U\\1") %>%
    
    ## sp. not sp or spp
    f("\\ssp(\\s|$)", " sp.\\1") %>%
    f("\\sspp.(\\s|$)", " sp.\\1") %>%
    f("\\sspp(\\s|$)", " sp.\\1") %>%
    f("\\sspp(\\s|$)", " sp.\\1") %>%
    
    ## subsp. not ssp, ssp., subsp or sub sp.
    f("\\sssp(\\s|$)", " subsp.\\1") %>%
    f("\\sssp.(\\s|$)", " subsp.\\1") %>%
    f("\\ssubsp(\\s|$)", " subsp.\\1") %>%
    f("\\ssub sp.(\\s|$)", " subsp.\\1") %>%
    
    ## var. not var
    f("\\svar(\\s|$)", " var.\\1") %>%
    
    ## aff. not affin, aff, affn
    f("\\saffin(\\s|$)", " aff.\\1") %>%
    f("\\saff(\\s|$)", " aff.\\1") %>%
    f("\\saffn(\\s|$)", " aff.\\1") %>%
    
    ## f. not forma
    f("\\sforma(\\s|$)", " f.\\1") %>%
    
    ## remove " ms" if present
    f("\\sms(\\s|$)", "\\1") %>%
    
    ## remove " s.l" or " s.s." if present
    f("\\ssl(\\s|$)", "\\1") %>%
    f("\\ss\\.l\\.(\\s|$)", "\\1") %>%
    f("\\sss(\\s|$)", "") %>%
    f("\\ss\\.s\\.(\\s|$)", "\\1") %>%
    
    ## clean white space
    str_squish()
}
