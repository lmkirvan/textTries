suffix_func <-

key_node <- trie_find(key, trie)

reduce_key <- function(key, trie, key_node){
  if(cdr_char(key) == ""){
    return(key_node$Set(suffix_link = trie))
  } else if(!is.null(trie_find(cdr_char(key), trie = trie))){
    return(key_node$Set(suffix_link = trie_find(cdr_char(key), trie = trie)))
  } else {
    trie <- trie$root
    reduce_key(cdr_char(key), trie, key_node)
  }
}

reduce_key(key, trie, key_node)


}
