find_trie <- function(document, trie){
  characters <- purrr::flatten_chr(stringr::str_split(document, ""))
  index <- seq_along(characters)
  result <- list()
  for(i in index){
    character <- characters[i]
<<<<<<< HEAD
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
=======
    while(isRoot(trie)==F & !character %in% names(trie$children)){
      trie <- trie$fail
    }
    if(isRoot(trie)){
      NULL
>>>>>>> ea493652bcd1efae189ac7f8259e1f6ae8c7f7d1
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




<<<<<<< HEAD
=======
#problem with getting stuck at rootnode and not progressing forward.
keys <- c("a", "ab", "bab", "bc", "bca", "c", "caa", "baby")
values <- c("foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar")
example_trie <- trie_create(keys = keys, value = values)
find_string <- "sdfl;khasdfl;knabccbabysdflsdfalkjdfs"

temp <- find_trie(find_string, example_trie)



>>>>>>> ea493652bcd1efae189ac7f8259e1f6ae8c7f7d1


