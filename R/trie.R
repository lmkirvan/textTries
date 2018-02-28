trie_insert <- function(trie, key, value) {
  trie_build <- function(key, trie) {
    if (length(key) == 1) {
      trie$AddChild(key)
    } else {
      trie$AddChild(key[1])
      trie_build(cdr(key), trie$children[[key[1]]])
    }
  }
  
  char_key <- purrr::flatten_chr(str_split(key, pattern = ""))
  
  if (head(char_key, 1) %in% names(trie$children)) {
    if (length(cdr(char_key)) == 0) {
      trie
    } else {
      trie_insert(trie$children[[char_key[1]]], cdr(char_key), value = NULL)
    }
  } else {
    trie_build(char_key, trie)
  }
  node <- key_find(key, trie)
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
#'test <- c("a", "ab", "bab", "bc", "bca", "c", "caa")
#'example_trie <- trie_create(test, value = rep("foo", length(test)))
#'key_find("c", test_trie)
#'key_find("ca", test_trie)
#'key_find("caa", test_trie)
key_find <- function(key, trie){
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
#'@return a reference class object of class node from the data.tree package.
#'@examples
#'test <- c("a", "ab", "bab", "bc", "bca", "c", "caa")
#'example_trie <- trie_create(test, value = rep("foo", length(test)))
#'example_trie
#'example_trie$a$b$fail
trie_create <- function(keys, values){
  
  if(length(keys) != length(values)){
    stop("Your key and values should be the same length")
  }

  trie <- Node$new("trie_root")
  purrr::map2(
    .x = keys,
    .y = values,
    .f = function(x, y) trie_insert(trie, key = x, value = y)
    )

  add_fails(trie)
  trie
}

test_trie <- Node$new("trie_root")
test_trie <- trie_insert(trie = test_trie, key = "a", value = "TEST")
test_trie <- trie_insert(trie = test_trie, key = "ab", value = "test")
test_trie <- trie_insert(trie = test_trie, key = "bab", value = "test")
test_trie <- trie_insert(trie = test_trie, key = "bc", value = "test")
test_trie <- trie_insert(trie = test_trie, key = "bca", value = "test")
test_trie <- trie_insert(trie = test_trie, key = "c", value = "test")
test_trie <- trie_insert(trie = test_trie, key = "caa", value = "test")
test_trie

test_trie


bab <- key_find("bab", trie = test_trie)
bab$parent

test_trie$b$a$b$s

test_trie$b$c$fail

typeof(test_trie)
