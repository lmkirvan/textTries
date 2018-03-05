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

#problem with getting stuck at rootnode and not progressing forward.
keys <- c("a", "ab", "bab", "bc", "bca", "c", "caa", "baby")
values <- c("foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar")
example_trie <- trie_create(keys = keys, value = values)
find_string <- "abccbabyab"
temp <- trie_find(find_string, example_trie)
