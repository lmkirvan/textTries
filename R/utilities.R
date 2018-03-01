cdr <- function(x){
  tail(x, length(x) - 1)
}

cdr_char <- function(atom_char){
  stringr::str_sub(atom_char, 2, nchar(atom_char))
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


.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    devtools.path = "~/R-dev",
    devtools.install.args = "",
    devtools.name = "Lewis Kirvan",
    devtools.desc.author = "Lewis Kirvan lewis.kirvan@cfpb.gov [aut, cre]",
    devtools.desc.license = "MIT",
    devtools.desc.suggests = NULL,
    devtools.desc = list()
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])
  
  invisible()
}