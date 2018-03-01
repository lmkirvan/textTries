trie_find <- function(document, trie){
  document <- purrr::flatten_chr(stringr::str_split(document, pattern = ""))
  index <- seq_along(document)
  match_list <- purrr::map2(document, index, process_character, trie)
  
  return(match_list)
}

#character -> index -> node -> maybe list(key, value, index) node 
process_character <- function(character, index, node){
  if(is.null(node$children)){
    NULL
  } else if(character %in% names(node$children)){
    node <- node$children[[character]]
    if(!is.null(node$output)){
      return(list(
        output = node$output,
        value = node$value,
        index = index))
    } else{
      NULL
    }
  } else {
    if(!is.null(node$fail)){
      node <- node$fail
      process_character(character, index, node)
    } else {
      NULL
    }
  }
}

example_trie[["s"]]

test_string <- "aabaacddcabcaa"

process_character("a",1, example_trie)


trie_find(test_string, example_trie)

example_trie$c$a$a$output
