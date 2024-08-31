library(pizzarr)

test_that("normalize_storage_path changes backslashes to forward slashes", {
  res <- normalize_storage_path("test\\nested\\dirs")
  expect_equal(res, "test/nested/dirs")
})

test_that("normalize_storage_path without leading nor trailing slashes", {
  res <- normalize_storage_path("test")
  expect_equal(res, "test")
})

test_that("normalize_storage_path with leading slash", {
  res <- normalize_storage_path("/test")
  expect_equal(res, "test")
})

test_that("normalize_storage_path with trailing slash", {
  res <- normalize_storage_path("test/")
  expect_equal(res, "test")
})

test_that("normalize_storage_path with both leading and trailing slashes", {
  res <- normalize_storage_path("/test//")
  expect_equal(res, "test")
})

test_that("normalize_storage_path collapses repeated slashes", {
  res <- normalize_storage_path("/test//this")
  expect_equal(res, "test/this")
})

test_that("normalize_resize_args throws error when different number of dimensions", {
  f <- function() normalize_resize_args(list(1, 2, 3), list(4, 5))
  expect_error(f())
})

test_that("normalize_resize_args", {
  res <- normalize_resize_args(c(1, 2), c(2, 1))
  expect_equal(res, list(2, 1))
})
test_that("normalize_resize_args with int argument", {
  res <- normalize_resize_args(c(1), 2)
  expect_equal(res, list(2))
})
test_that("normalize_resize_args with int argument", {
  res <- normalize_resize_args(c(1), list(2))
  expect_equal(res, list(2))
})

test_that("normalize_integer_selection with valid input", {
  # Reference: https://github.com/gzuidhof/zarr.js/blob/292804/test/core/indexing.test.ts#L12
  res <- normalize_integer_selection(1, 100)
  expect_equal(res, as_scalar(1))
  res <- normalize_integer_selection(-1, 100)
  expect_equal(res, as_scalar(99))
})

test_that("normalize_integer_selection with invalid input", {
  # Reference: https://github.com/gzuidhof/zarr.js/blob/292804/test/core/indexing.test.ts#L12
  
  # TODO: should normalization be performed based  R indexing integers or python ?
  # f1 <- function() normalize_integer_selection(100, 100)
  # expect_error(f1())
  normalize_integer_selection(100, 100)
  f2 <- function() normalize_integer_selection(1000, 100)
  expect_error(f2())
  f3 <- function() normalize_integer_selection(-1000, 100)
  expect_error(f3())
})

test_that("normalize_chunks", {
  res <- normalize_chunks(c(10), c(100), 1)
  expect_equal(res, c(10))

  res <- normalize_chunks(as_scalar(10), c(100), 1)
  expect_equal(res, c(10))

  res <- normalize_chunks(c(10, 10), c(100, 10), 1)
  expect_equal(res, c(10, 10))

  res <- normalize_chunks(as_scalar(10), c(100, 10), 1)
  expect_equal(res, c(10, 10))

  res <- normalize_chunks(c(10, NA), c(100, 10), 1)
  expect_equal(res, c(10, 10))

  res <- normalize_chunks(as_scalar(30), c(100, 20, 10), 1)
  expect_equal(res, c(30, 30, 30))

  res <- normalize_chunks(c(30), c(100, 20, 10), 1)
  expect_equal(res, c(30, 20, 10))

  res <- normalize_chunks(c(30, NA), c(100, 20, 10), 1)
  expect_equal(res, c(30, 20, 10))

  res <- normalize_chunks(c(30, NA, NA), c(100, 20, 10), 1)
  expect_equal(res, c(30, 20, 10))

  res <- normalize_chunks(c(30, 20, NA), c(100, 20, 10), 1)
  expect_equal(res, c(30, 20, 10))

  res <- normalize_chunks(c(30, 20, 10), c(100, 20, 10), 1)
  expect_equal(res, c(30, 20, 10))

  f1 <- function() normalize_chunks("foo", c(100), 1)
  expect_error(f1())

  f2 <- function() normalize_chunks(c(100, 10), c(100), 1)
  expect_error(f2())

  # test auto-chunking
  res <- normalize_chunks(NA, c(100), 1)
  expect_equal(res, c(100))

  res <- normalize_chunks(as_scalar(-1), c(100), 1)
  expect_equal(res, c(100))

  res <- normalize_chunks(c(30, -1, NA), c(100, 20, 10), 1)
  expect_equal(res, c(30, 20, 10))
})

