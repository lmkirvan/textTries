find_trie <- function(document, trie){
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
        result[[i]] <- list(
          value = get_attribute(trie, "value"),
          output = get_attribute(trie, "output"),
          index = i
        )} else{
          NULL
        }
      } else {
        trie <- trie[[character]]
        result[[i]] <- list(
          value = get_attribute(trie, "value"),
          output = get_attribute(trie, "output"),
          index = i
        )
      }
    }
  result
  }






