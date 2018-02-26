require(data.tree)
require(stringr)
require(purrr)

cdr <- function(x){
  tail(x, length(x) - 1)
}

cdr_char <- function(atom_char){
  stringr::str_sub(atom_char, 2, nchar(atom_char))
}

trie_find <- function(key, trie){
  key <- purrr::flatten_chr(str_split(key, pattern = ""))
  for(i in seq_along(key)){
    if(key[i] %in% names(trie$children)) {
      trie <- trie$children[[key[i]]]
      } else {
      return(NULL)
      }
    }
  trie
  }

trie_insert <- function(trie, key, value){

  trie_build <- function(key, trie){
    if(length(key) == 1){
      trie$AddChild(key)
      } else {
        trie$AddChild(key[1])
        trie_build(cdr(key), trie$children[[key[1]]])
      }
  }

  char_key <- purrr::flatten_chr(str_split(key, pattern = ""))

  if(head(char_key, 1) %in% names(trie$children)){
    if(length(cdr(char_key)) == 0){
      trie
    } else {
      trie_insert(trie$children[[char_key[1]]], cdr(char_key), value = NULL)
      }
    } else {
      trie_build(char_key, trie)
    }
  node <- trie_find(key, trie)
  node$output <- key
  node$value <- value
  return(trie)
  }


trie_create <- function(keys, values){

  trie <- Node$new("trie_root")
  purrr::map2(
    .x = keys,
    .y = values,
    .f = function(x, y) trie_insert(trie, key = x, value = y)
    )

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

test <- c("a", "ab", "bab", "bc", "bca", "c", "caa")

test_trie <- trie_create(test, value = rep("test", length(test)))

test_trie

bab <- trie_find("bab", trie = test_trie)
bab$parent

test_trie$b$a$b$s
trie_find("ca", test_trie)
