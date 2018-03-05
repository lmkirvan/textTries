trie_find <- function(document, trie){
  characters <- purrr::flatten_chr(stringr::str_split(document, ""))
  index <- seq_along(characters)
  result <- list()
  for(i in index){
    character <- characters[i]
    if(character %in% names(trie$children)){
      trie <- trie[[character]]
      result[[i]] <- list(
        outputs = get_attribute(trie, "output"),
        value = get_attribute(trie, "value"),
        index = i)
      } else { 
        while(!character %in% names(trie$children)){
         trie <- trie$fail}
        trie <- trie[[character]]
        result[[i]] <- list(
          outputs = get_attribute(trie, "output"),
          value = get_attribute(trie, "value"),
          index = i)
      } 
    }
  result
  }

