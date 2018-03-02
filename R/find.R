trie_find <- function(document, trie){
  characters <- purrr::flatten_chr(str_split(document, ""))
  index <- seq_along(characters)
  result <- list()
  for(i in index){
    character <- characters[i]
    if(character %in% names(trie$children)){
      trie <- trie[[character]]
      result[[i]] <- list(
        value = trie$value,
        output = trie$output,
        index = i)
    } else {
      trie <- trie$fail
      if(character %in% names(trie$children)){
        trie <- trie[[character]]
        result[[i]] <- list(
          value = trie$value,
          output = trie$output,
          index = i)
      }
    }
  }
  result
}








