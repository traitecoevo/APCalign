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
#' @param epithet_letters A string specifying if 1 or 2 letters remain fixed at the start of the species epithet.
#'
#' @return A text string that matches a recognised taxon name or scientific name
#' 
#'
#' @examples
#' fuzzy_match("Baksia serrata", c("Banksia serrata", 
#'                                  "Banksia integrifolia"), 
#'                                  max_distance_abs = 1, 
#'                                  max_distance_rel = 1)
#' 
#' @noRd
fuzzy_match <- function(txt, accepted_list, max_distance_abs, max_distance_rel, n_allowed = 1, epithet_letters = 1) {
  
  if (!epithet_letters %in% c(1,2)) {
    stop("Epithet must be 1 or 2.")
    }
  ## identify number of words in the text to match
  words_in_text <- 1 + stringr::str_count(txt," ")
  
  ## extract first letter of first word
  txt_word1_start <- stringr::str_extract(txt, "[:alpha:]")
  
  ## for text matches with 2 or more words, extract the first letter/digit of the second word
  if(words_in_text > 1 & epithet_letters == 2) 
    {if(nchar(word(txt,2)) == 1) {
      txt_word2_start <- stringr::str_extract(word(txt,2), "[:alpha:]|[:digit:]")
    } else {
      txt_word2_start <- stringr::str_extract(word(txt,2), "[:alpha:][:alpha:]|[:digit:]")     
    }
  }

  if(words_in_text > 1 & epithet_letters == 1) {
    txt_word2_start <- stringr::str_extract(word(txt,2), "[:alpha:]|[:digit:]")
  }
    
  ## for text matches with 3 or more words, extract the first letter/digit of the third word
  if(words_in_text > 2) {
    txt_word3_start <- stringr::str_extract(word(txt,3), "[:alpha:]|[:digit:]")
  }
  
  ## identify the number of characters that must change for the text string to match each of the possible accepted names
  distance_c <- utils::adist(txt, accepted_list, fixed=TRUE)[1,]
  
  ## identify the minimum number of characters that must change for the text string to match a string in the list of accepted names
  min_dist_abs_c <-  min(distance_c)
  min_dist_per_c <-  min(distance_c) / stringr::str_length(txt)

  i <- which(distance_c==min_dist_abs_c)
  keep = FALSE
  
  if(
    ## Within allowable number of characters (absolute)
    min_dist_abs_c <= max_distance_abs &
    ## Within allowable number of characters (relative)
    min_dist_per_c <= max_distance_rel #&
    ## Is a unique solution
    #length(i) <= n_allowed
    ) {
    
    for (j in 1:length(i)) {
  
      if (keep == TRUE) {
        
        break()
        
      } else {
        ## identify number of words in the matched string
        words_in_match <- 1 + stringr::str_count(accepted_list[i][j]," ")
        
        ## identify the first letter of the first word in the matched string
        match_word1_start <- stringr::str_extract(accepted_list[i][j], "[:alpha:]")
        
        ## identify the first letter of the second word in the matched string (if the matched string includes 2+ words)
        if(words_in_text > 1 & epithet_letters == 2) {
          if(nchar(word(accepted_list[i][j],2)) == 1) {
            match_word2_start <- stringr::str_extract(word(accepted_list[i][j],2), "[:alpha:]|[:digit:]")
          } else {
            match_word2_start <- stringr::str_extract(word(accepted_list[i][j],2), "[:alpha:][:alpha:]|[:digit:]")
          }
        }
        
        if(words_in_text > 1 & epithet_letters == 1) {
            match_word2_start <- stringr::str_extract(word(accepted_list[i][j],2), "[:alpha:]|[:digit:]")
        }
    
        ## identify the first letter of the third word in the matched string (if the matched string includes 3+ words)
        if(words_in_text > 2) {
          match_word3_start <- stringr::str_extract(word(accepted_list[i][j],3), "[:alpha:]|[:digit:]")
        }
        
        ## keep match if the first letters of the first three words (or fewer if applicable) in the string to match 
        ## are identical to the first letters of the first three words in the matched string
    
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
          
          return(accepted_list[i][j])
          
        }
        
        return(NA)
      }

      return(NA)
    }
    
    return(NA)
  }
  
  return(NA)
}

