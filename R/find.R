#rie_find <- function(document, trie){
# characters <- purrr::flatten_chr(str_split(document, ""))
# index <- seq_along(characters)
# result <- list()
# for(i in index){
#   character <- characters[i]
#   if(character %in% names(trie$children)){
#     trie <- trie[[character]]
#     result[[i]] <- list(
#       value = trie$value,
#       output = trie$output,
#       index = i)
#   } else {
#     trie <- trie$fail
#     if(character %in% names(trie$children)){
#       trie <- trie[[character]]
#       result[[i]] <- list(
#         value = trie$value,
#         output = trie$output,
#         index = i)
#     } else {
#       NULL
#     }
#   }
# }
# result

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
      } else if(!is.null(trie$fail)){
        trie <- trie$fail
        if(character %in% names(trie$children)){
          trie <- trie[[character]]
          result[[i]] <- list(
            outputs = get_attribute(trie, "output"),
            value = get_attribute(trie, "value"),
            index = i)
          }
      } else {
        if(is.null(trie$output)){
          result[[i]] <- list(
            outputs = get_attribute(trie, "output"),
            value = get_attribute(trie, "value"),
            index = i)
        }
      }
    }
  result
  }

#problem with getting stuck at rootnode and not progressing forward.
get_attribute <- function(self, attribute) {
  if(is.null(self[[attribute]])){
    NULL
  } else {
    this <- self[[attribute]]
    c(this, get_attribute(self$fail, attribute))
    }
  }

keys <- c("a", "ab", "bab", "bc", "bca", "c", "caa")
values <- c("foo", "bar", "foo", "bar", "foo", "bar", "foo")
example_trie <- trie_create(keys = keys, value = values)
find_string <- "abccab"
temp <- trie_find(find_string, example_trie)

example_trie$c$fail
