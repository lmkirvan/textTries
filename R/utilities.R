cdr <- function(x){
  tail(x, length(x) - 1)
}

cdr_char <- function(atom_char){
  stringr::str_sub(atom_char, 2, nchar(atom_char))
}

split_flat <- function(x){
  purrr::flatten_chr(stringr::str_split(x, ""))
}

enqueue <- function(nodes, queue){
  if(!is.null(nodes)){
    c(queue, nodes)
  } else {
    queue
  }

}

dequeue <- function(queue){
  cdr(queue)
}


add_rootnodes <- function(trie){
  for(child in trie$children){
    child$fail <- trie$root
  }
}



#' This finds attributes, like output or value, for a particular node by
#' traversing fails and collecting the named attribute
#'
#' @param self
#' @param attribute
#'
#' @return a character vector of the node attributes
#' @export
#'
#' @examples
#'keys <- c("a", "ab", "bab", "bc", "bca", "c", "caa", "abc")
#'values <- c("foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar")
#'example_trie <- trie_create(keys, values)
#'get_attribute(example_trie$b$a$b, "output")
get_attribute <- function(self, attribute) {
  if(is.null(self[[attribute]])){
    NULL
  } else {
    this <- self[[attribute]]
    c(this, get_attribute(self$fail, attribute))
  }

}


