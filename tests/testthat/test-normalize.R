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
  expect_equal(res, 1)
  res <- normalize_integer_selection(-1, 100)
  expect_equal(res, 99)
})

test_that("normalize_integer_selection with invalid input", {
  # Reference: https://github.com/gzuidhof/zarr.js/blob/292804/test/core/indexing.test.ts#L12
  f1 <- function() normalize_integer_selection(100, 100)
  expect_error(f1())
  f2 <- function() normalize_integer_selection(1000, 100)
  expect_error(f2())
  f3 <- function() normalize_integer_selection(-1000, 100)
  expect_error(f3())
})