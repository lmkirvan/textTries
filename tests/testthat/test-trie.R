testthat::context("creating trie")
require(data.tree)


test_that("values are correct",{
  keys <- c("a", "ab", "bab", "bc", "bca", "c", "caa", "abc")
  values <- c("foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar")
  example_trie <- trie_create(keys, values)
  expect_equal(example_trie$a$b$value, "bar")
  expect_equal(example_trie$a$value, "foo")
  expect_equal(example_trie$c$a$a$value, example_trie$a$value)
})



test_that("fails are correct", {
  keys <- c("a", "ab", "bab", "bc", "bca", "c", "caa", "abc")
  values <- c("foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar")
  example_trie <- trie_create(keys, values)
  example_trie
  example_trie$a$b$fail
  expect_equal(example_trie$c$a$fail, example_trie$a)
  expect_null(example_trie$c$a$a$fail)
  expect_equal(example_trie$a$fail, example_trie)
})


