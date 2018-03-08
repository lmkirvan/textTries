testthat::context("matching strings")
require(textTries)

test_that("does matching work",{
  keys <- c("a", "ab", "bab", "bc", "bca", "c", "caa", "abc")
  values <- c("foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar")
  example_trie <- trie_create(keys, values)
  search_string <- "baby"
  expect_equal(trie_find("baby", example_trie)$output, c("a","bab","ab"))
})



