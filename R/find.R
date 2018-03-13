#' Given a document and a search trie finds occurrences of 
#' 
#'
#' @param document 
#' @param trie 
#'
#' @return a dataframe
#' @export
#'
#' @examples
trie_find <- function(document, trie, border_chars = c("!", ",", ".", "?", ";", ":")){
  
  characters <- purrr::flatten_chr(stringr::str_split(document, ""))
  
  whole_words <- trie$whole_words
  
  if(whole_words){
    characters <- c(" ", characters)
    characters <- purrr::map_chr(characters, .f = str_replace_border_chars, border_chars)
  }
  
  index <- seq_along(characters)
  
  result <- list()
  for(i in index){
    character <- characters[i]
    while(isRoot(trie) == F & !character %in% names(trie$children)){
      trie <- trie$fail
    }
    if(isRoot(trie)){
      if(character %in% names(trie$children)){
        trie <- trie[[character]]
        if(!is.null(trie$value)){
          result[[i]] <- list(
            value = get_attribute(trie, "value"),
            output = get_attribute(trie, "output"),
            index = i)
        } else {
          NULL
          }
        } else{
          NULL
        }
      } else {
        trie <- trie[[character]]
        if(!is.null(trie$value)){
          result[[i]] <- list(
            value = get_attribute(trie, "value"),
            output = get_attribute(trie, "output"),
            index = i)
        } else {
          NULL
        }
      }
    }
  result_to_df(result, whole_words = whole_words)
  }


result_to_df <- function(result, whole_words){
  
  compacted <- purrr::compact(result) 
  output <- lapply(compacted, function(x) x[[2]])
  lengths <-lengths(output) 
  output <- purrr::flatten_chr(output)
  output <- stringr::str_trim(output)
  
  value <- lapply(compacted, function(x) x[[1]])
  value <- purrr::flatten_chr(value)

  index <- purrr::map2(compacted, lengths, function(x, y) rep(x$index, y))
  index <- purrr::flatten_int(index)
  
  df <- dplyr::data_frame(
    output = output,
    value = value,
    index = index
  )
  
  num_chars <- nchar(output)
  
  df$start <- df$index - num_chars + 1
  
  if(whole_words){
    df$end <- df$start + num_chars - 3  
  } else {
    df$end <- df$start + num_chars - 1
  }
  
  unique(df)
  
}


str_replace_border_chars <- function(char, border_chars){
  if(char %in% border_chars){
    " "
  } else {
    char
  }
  
}


