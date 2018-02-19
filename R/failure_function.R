#test_trie
add_rootnodes <- function(trie){
  for(child in trie$children){
    child$Set(fail = trie$root$path, traversal = "ancestor")
  }
}

enqueue <- function(nodes, queue){
  c(queue, nodes)
}

dequeue <- function(queue){
  queue <<- cdr(queue)
}

fail_node <- function(node){
  if(is.null(node$children)){
    NULL
  } else {
    for(child in node$children){
        check_for_failure(child, node)
    }
  }
}

check_for_failure <- function(node_child, node){

  if(is.null(node$children)){
    NULL
  } else if(node_child$name  %in% fail_children){
    path_to_fail <- node$fail$children[[name(node_child)]]$path
    node_child$Set(fail = path_to_fail)
  } else {
    check_for_failure(node_child, node$fail)
    }
  }


add_fails <- function(trie) {
  add_rootnodes(trie)
  queue <- NULL
  queue <- enqueue(trie$root$children, queue)

  while(length(queue) > 0){
    fail_node(queue[[1]])
    enqueue(queue = queue, nodes = queue[[1]]$children)
    dequeue(queue = queue)
  }

  }


add_fails(test_trie)
