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
trie_find <- function(document, trie){
  characters <- purrr::flatten_chr(stringr::str_split(document, ""))
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
          result[[i]] <- NULL
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
          result[[i]] <- NULL
        }
      }
    }
  result_to_df(result)
  }


result_to_df <- function(result){
  compact <- purrr::compact(result) 
  output <- purrr::map(compact, "output")
  value <- purrr::map2(compact, lengths(output), function(x, y) rep(x$value, y))
  index <- purrr::map2(compact, lengths(output), function(x, y) rep(x$index, y))
  
  output <- purrr::flatten_chr(output)
  value <- purrr::flatten_chr(value)
  index <- purrr::flatten_int(index)
  
  df <- dplyr::data_frame(
    output = output,
    value = value,
    index = index
  )
  
  num_chars <- nchar(df$output)
  df$start <- df$index - nchar(df$output) + 1
  df$end <- df$start + num_chars
  
  df
  
}


  