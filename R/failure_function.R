#test_trie
fail_node <- function(node){
  if(is.null(node$children)){
    NULL
  } else {
    for(child in node$children){
      check_for_fails(child)
    }
  }
}

check_for_dicts <- function(self, current_fail){
  if(is.null(current_fail)){
    NULL
  } else if(!is.null(current_fail$output)){
    self$output <- current_fail$output
    self$value <- current_fail$value
  } else {
    current_fail <- current_fail$fail
    check_for_dicts(self, current_fail = current_fail)
  }
}


check_for_fails <- function(self, current_fail = self$parent$fail){
  if(is.null(current_fail)){
    self$fail <- self$root
  } else if(self$name %in% names(current_fail)){
      self$fail <- current_fail$children[[self$name]]
      if(!is.null(current_fail$children[[self$name]]$output)){
        if(is.null(self$output)){
          self$output <- current_fail$children[[self$name]]$output
          self$value <- current_fail$children[[self$name]]$value
        }
      } else {
          check_for_dicts(self, current_fail = current_fail$fail)
        }
    } else {
      current_fail <- current_fail$fail
      check_for_fails(self, current_fail)
    }
}

add_fails <- function(trie) {
  add_rootnodes(trie)
  queue <- NULL
  queue <- enqueue(trie$root$children, queue)
  while(length(queue) > 0){
    fail_node(queue[[1]])
    queue <- enqueue(queue = queue, nodes = queue[[1]]$children)
    queue <- dequeue(queue = queue)
  }
}




