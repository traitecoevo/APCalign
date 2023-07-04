#' Fuzzy match taxonomic names
#' 
#' This function attempts to match input strings to a list of allowable taxonomic names.
#' It requires that the first letter (or digit) of each word is identical between the input and output strings to avoid mis-matches
#' 
#' @param txt The string of text requiring a match
#' @param accepted_list The list of accepted names attempting to match to
#' @param max_distance_abs The maximum allowable number of characters differing between the input string and the match
#' @param max_distance_rel The maximum proportional difference between the input string and the match
#' @param n_allowed The number of allowable matches returned. Defaults to 1
#'
#' @return A text string that matches a recognised taxon name or scientific name
#' @export
#'
#' @examples
#' fuzzy_match("Baksia serrata", c("Banksia serrata", 
#'                                  "Banksia integrifolia"), 
#'                                  max_distance_abs = 1, 
#'                                  max_distance_rel = 1)
#' 
fuzzy_match <- function(txt, accepted_list, max_distance_abs, max_distance_rel, n_allowed = 1) {
  
  words_in_text <- 1 + stringr::str_count(txt," ")
  
  txt_word1_start <- stringr::str_extract(txt, "[:alpha:]")
  
  if(words_in_text > 1) {
    txt_word2_start <- stringr::str_extract(word(txt,2), "[:alpha:]|[:digit:]")
  }
  
  if(words_in_text > 2) {
    txt_word3_start <- stringr::str_extract(word(txt,3), "[:alpha:]|[:digit:]")
  }
  
  distance_c <- utils::adist(txt, accepted_list, fixed=TRUE)[1,]
  
  min_dist_abs_c <-  min(distance_c)
  min_dist_per_c <-  min(distance_c) / stringr::str_length(txt)
  
  i <- which(distance_c==min_dist_abs_c)
  
  if(
    ## Within allowable number of characters (absolute)
    min_dist_abs_c <= max_distance_abs &
    ## Within allowable number of characters (relative)
    min_dist_per_c <= max_distance_rel &
    ## Is a unique solution
    length(i)<=n_allowed
  ) {
    words_in_match <- 1 + stringr::str_count(accepted_list[i]," ")
    
    match_word1_start <- stringr::str_extract(accepted_list[i], "[:alpha:]")
    
    if(words_in_text > 1) {
      match_word2_start <- stringr::str_extract(word(accepted_list[i],2), "[:alpha:]|[:digit:]")
    }
    
    if(words_in_text > 2) {
      match_word3_start <- stringr::str_extract(word(accepted_list[i],3), "[:alpha:]|[:digit:]")
    }
    
    keep = FALSE
    
    if(words_in_text == 1) {
      if (txt_word1_start == match_word1_start) {
        keep = TRUE }
      
    } else if(words_in_text == 2) {
      if (txt_word1_start == match_word1_start & txt_word2_start == match_word2_start) {
        keep = TRUE }
      
    } else if(words_in_text > 2) {
      if (words_in_match > 2) {
        if (txt_word1_start == match_word1_start & txt_word2_start == match_word2_start & txt_word3_start == match_word3_start) {
          keep = TRUE }
      } else if (txt_word1_start == match_word1_start & txt_word2_start == match_word2_start) {
        keep = TRUE }
    }
    
    if(keep == TRUE) {
      
      return(accepted_list[i])
      
    }
    return(NA)
  }
  return(NA)
}
