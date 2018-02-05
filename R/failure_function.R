#test_trie

bfs_list <- list(
  q = vector(test_trie$children),
  current = vector()
)

add_children <- function(q, children){
  q <- c(q,children)
}

progress <- function(current, q){
}

