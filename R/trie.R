#' Insert new key value pairs into a trie.
#'
#' This can be used to insert new key-value pairs into an existing trie. It's
#' also the engine used in trie creation
#'
#' @param trie a trie to update
#' @param key the replacement keyword
#' @param value the value that will be used to replace keywords when using
#' trie replace
#'
#' @return an updated trie
#' @export
#'
#' @examples
#'test <- c("a", "ab", "bab", "bc", "bca", "c", "caa", "abc")
#'values <- c("foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar")
#'example_trie <- trie_create(test, value = values)
#'trie_insert(example_trie, key = "dab", value = "foo")
#'#'trie_insert(example_trie, key = "cad", value = "bar")
trie_insert <- function(trie, key, value) {

  trie_build <- function(key, trie) {
    if (length(key) == 1) {
      trie$AddChild(key)
    } else {
      trie$AddChild(key[1])
      trie_build(cdr(key), trie$children[[key[1]]])
    }
  }

  chars <- split_flat(key)

  if (head(chars, 1) %in% names(trie$children)) {
    if (length(cdr(chars)) == 0) {
      trie
    } else {
      trie_insert(trie$children[[chars[1]]], cdr(chars), value = NULL)
    }
  } else {
    trie_build(chars, trie)
  }
  node <- key_traverse(key, trie)
  node$output <- key
  node$value <- value
  return(trie)
}

#' Traverse the trie to a particular node by full key name.
#'
#' Each node's formal name is a single character. You can traverse to the node
#' of a key in the trie by splitting a key into characters and then traverseing
#' from parent to child node. This is really just a helper function.
#'
#' @param key the key to search for
#' @param trie the trie to search for
#'
#' @return the trie node that corresponds to the key
#' @export
#'
#' @examples
#'test <- c("a", "ab", "bab", "bc", "bca", "c", "caa", "abc")
#'values <- c("foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar")
#'example_trie <- trie_create(test, value = values)
#'key_traverse("c", example_trie)
#'key_traverse("ca", example_trie)
#'key_traverse("caa", example_trie)
key_traverse <- function(key, trie){
  key <- purrr::flatten_chr(stringr::str_split(key, pattern = ""))
  for(i in seq_along(key)){
    if(key[i] %in% names(trie$children)) {
      trie <- trie$children[[key[i]]]
      } else {
      return(NULL)
      }
    }
  trie
}

#'Create the finite state automaton.
#'
#'AKA the trie with failure attributes built in. This will need to be run one
#'time and then can be used for both find and replace functions.
#'
#'@export
#'@param keys these are the words that you want to search for in the trie
#'@param values these are optional, but if you will need to supply them if you
#'  want to use the replace method.
#'@param to_lower are your keys case sensitive? If so, this should be false.
#'@param whole_words_only do you want to pad your keys with whitespace to find
#'  only those that are whole words? If so select this should be true
#'@return a reference class object of class node from the data.tree package.
#'@examples
#'keys <- c("a", "ab", "bab", "bc", "bca", "c", "caa", "abc")
#'values <- c("foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar")
#'example_trie <- trie_create(keys, value = values)
#'example_trie
#'example_trie$a$b$fail
trie_create <- function(keys, values, to_lower = TRUE, whole_words_only = TRUE){
  if(length(keys) != length(values)){
    stop("Your key and values should be the same length")
  }
  
  if(to_lower){
    keys <- tolower(keys)
  }
  
  if(whole_words_only){
    keys <- stringr::str_c(" ", keys, " ")
  }
  
  
  trie <- Node$new("trie_root")
  purrr::map2(
    .x = keys,
    .y = values,
    .f = function(x, y) trie_insert(trie, key = x, value = y))
  add_fails(trie)

  if(whole_words_only == T){
    trie$whole_words <- TRUE
  } else {
    trie$whole_words <- FALSE
  }
  
  trie
    
  
}



