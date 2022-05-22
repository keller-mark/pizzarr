library(pizzarr)

test_that("Zarr json_to_raw", {
  res <- json_to_raw(list(
    hello = c("world", "!")
  ))
  expect_equal(res, as.raw(c(
    0x7b, 0x22, 0x68, 0x65, 0x6c, 0x6c, 0x6f, 0x22,
    0x3a, 0x5b, 0x22, 0x77, 0x6f, 0x72, 0x6c, 0x64,
    0x22, 0x2c, 0x22, 0x21, 0x22, 0x5d, 0x7d
  )))
})

test_that("Zarr MemoryStore, can get and set a value at the top level", {
  store <- MemoryStore$new()

  store_contains <- store$contains_item("hello")
  expect_equal(store_contains, FALSE)

  store$set_item("hello", c(0xbeef))

  store_contains <- store$contains_item("hello")
  expect_equal(store_contains, TRUE)

  value <- store$get_item("hello")
  expect_equal(value, c(0xbeef))
})

test_that("Zarr MemoryStore, throws error if setting recursively over a non-list value", {
  should_error <- function() {
    store <- MemoryStore$new()
    store$set_item("hello", c(0xbeef))
    store$set_item("hello/there", c(0xdead))
  }
  expect_error(should_error())
})

test_that("Zarr MemoryStore, can set and get nested values", {
  store <- MemoryStore$new()
  store$set_item("hello/there", c(0xbeef))
  store$set_item("hello/world", c(0xdead))

  value_there <- store$get_item("hello/there")
  expect_equal(value_there, c(0xbeef))

  value_world <- store$get_item("hello/world")
  expect_equal(value_world, c(0xdead))
})
test_that("Zarr MemoryStore, can set and get twice nested values", {
  store <- MemoryStore$new()
  store$set_item("hello/there/a", c(0xbeef))
  store$set_item("hello/there/b", c(0xbeef))
  store$set_item("hello/world/a", c(0xdead))
  store$set_item("hello/world/b", c(0xdead))

  value_there <- store$get_item("hello/there")
  expect_equal(value_there, list(a = 0xbeef, b = 0xbeef))

  value_world <- store$get_item("hello/world")
  expect_equal(value_world, list(a = 0xdead, b = 0xdead))
})
test_that("Zarr matrix_to_zarr, no compression", {
  store <- MemoryStore$new()

  m <- matrix(c(1, 2, 3, rep(0, 9)), nrow = 3, ncol= 4)
  rows <- c("c1", "c2", "c3")
  cols <- c("g1", "g2", "g3", "g4")

  matrix_to_zarr(m, rows, cols, store)

  expect_equal(store$contains_item(".zarray"), TRUE)
  expect_equal(store$contains_item(".zattrs"), TRUE)
  expect_equal(store$contains_item("0.0"), TRUE)
  expect_equal(store$contains_item("0.1"), FALSE)

  zattrs_raw <- store$get_item(".zattrs")
  expect_equal(rawToChar(zattrs_raw), "{\"rows\":[\"c1\",\"c2\",\"c3\"],\"cols\":[\"g1\",\"g2\",\"g3\",\"g4\"]}")

  chunk_raw <- store$get_item("0.0")
  expect_equal(length(chunk_raw), 12)
})
