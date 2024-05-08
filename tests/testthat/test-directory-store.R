library(pizzarr)

test_that("Zarr DirectoryStore, can listdir", {
  store <- DirectoryStore$new(tempdir())
  store$set_item("hello/there/a", c(0xbeef))
  store$set_item("hello/there/b", c(0xbeef))
  store$set_item("hello/world/a", c(0xdead))
  store$set_item("hello/world/b", c(0xdead))

  expect_true("hello" %in% store$listdir())
  expect_true("there" %in% store$listdir("hello"))
  expect_true("world" %in% store$listdir("hello"))
  expect_true("a" %in% store$listdir("hello/there"))
  expect_true("b" %in% store$listdir("hello/there"))
})

test_that("Zarr DirectoryStore, can rmdir", {
  store <- DirectoryStore$new(tempdir())
  store$set_item("hello/there/a", c(0xbeef))
  store$set_item("hello/there/b", c(0xbeef))
  store$set_item("hello/world/a", c(0xdead))
  store$set_item("hello/world/b", c(0xdead))

  store$rmdir("hello/there")
  f <- function() store$listdir("hello/there")
  expect_error(f())
  expect_true("world" %in% store$listdir("hello"))
  expect_false("there" %in% store$listdir("hello"))

  store$rmdir("hello")
  expect_false("hello" %in% store$listdir())
})
