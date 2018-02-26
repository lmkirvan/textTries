#test_trie
add_rootnodes <- function(trie){
  for(child in trie$children){
    child$fail <- trie$root
  }
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

fail_node <- function(node){
  if(is.null(node$children)){
    NULL
  } else {
    for(child in node$children){
      check_for_fails(child)
    }
  }
}

check_for_fails <- function(self){
  if(self$name %in% names(self$parent$fail$children)){
      self$fail <- self$parent$fail$children[[self$name]]
      if(!is.null(self$parent$fail$children[[self$name]]$output)){
        self$output <- self$parent$fail$children[[self$name]]$output
      }
    } else {
      self$fail <- NULL
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
    print(length(queue))
  }
}

#queue <- NULL
#queue <- enqueue(test_trie$root$children, queue = queue)
#fail_node(queue[[1]])

add_fails(test_trie)



