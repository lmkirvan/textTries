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
trie_find <- function(document, trie, border_chars = c(" ", "!", ",", ".", "?", ";", ":")){
  characters <- purrr::flatten_chr(stringr::str_split(document, ""))
  characters <- c(" ", characters)
  index <- seq_along(characters)
  result <- list()
  for(i in index){
    character <- str_replace_border_chars(characters[i], border_chars)
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
  result_to_df(result)
  }


result_to_df <- function(result){
  
  compacted <- purrr::compact(result) 
  output <- lapply(compacted, function(x) x[[2]])
  lengths <-lengths(output) 
  output <- purrr::flatten_chr(output)
  output <- stringr::str_trim(output)
  
  value <- lapply(compacted, function(x) x[[1]])
  value <- purrr::flatten_chr(value)

  index <- purrr::map2(compacted, lengths, function(x, y) rep(x$index, y))
  index <- purrr::reduce(index, c)
  
  df <- dplyr::data_frame(
    output = output,
    value = value,
    index = index
  )
  
  num_chars <- nchar(output)
  
  df$start <- df$index - num_chars + 1
  df$end <- df$start + num_chars - 3 
  
  unique(df)
  
}


str_replace_border_chars <- function(char, border_chars = c(" ", "!", ",", ".", "?", ";", ":")){
  if(char %in% border_chars){
    " "
  } else {
    char
  }
  
}
 


library(textTries)
keys <- quanteda::stopwords()
keys <- tolower(keys)
values <- rep(c("bing", "bang", "boom", "blart", "blat"), 175 /5)
keys <- stringr::str_c(" ", keys, " ")
text <- quanteda::data_corpus_inaugural$documents$texts[57]
text <- tolower(text)
text <- paste('i am a loser, i suck.', text)
example_trie <- trie_create(keys, values)
example_df <- trie_find(text, example_trie)

