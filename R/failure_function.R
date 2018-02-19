#test_trie
add_rootnodes <- function(trie){
  for(child in trie$children){
    child$Set(fail = "trie_root")
  }
}

enqueue <- function(nodes, queue){
  c(queue, nodes)
}

dequeue <- function(queue){
  queue <<- cdr(queue)
}

fail_node <- function(node){
  for(child in seq_along(node$children)){
    check_for_failure(node$children[[child]], node)
  }
}

check_for_failure <- function(node_child, node){
  fail <- node$failure
  d
  if(is.null(node$children)){
    NULL
  } else if(node_child$name %in% names(fail[[1]]$children)){
    node_child$Set(fail = fail[[1]]$children[[node_child$name]])
    #is it okay to assign null values here?
    if(!is.null(fail[[1]]$children[[node_child$name]]$output)){
      node_child$Set(key_link = fail[[1]]$children[[node_child$name]]$output)
    } else {
      break()
    }
  } else {
    check_for_failure(node_child, node$fail)
    }
  }

add_fails <- function(trie) {
  add_rootnodes(trie)
  queue <- list()
  queue <- map(trie$root$children, .f = enqueue, queue = queue)

  while(length(queue) > 0){
    fail_node(queue[[1]])
    print(length(queue))
    dequeue(queue = queue)
  }

  }


