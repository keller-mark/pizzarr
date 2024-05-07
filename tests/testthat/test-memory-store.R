library(pizzarr)

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

test_that("Zarr MemoryStore, can listdir", {
  store <- MemoryStore$new()
  store$set_item("hello/there/a", c(0xbeef))
  store$set_item("hello/there/b", c(0xbeef))
  store$set_item("hello/world/a", c(0xdead))
  store$set_item("hello/world/b", c(0xdead))

  expect_equal(store$listdir("hello"), c("there", "world"))
  expect_equal(store$listdir("hello/there"), c("a", "b"))
})
