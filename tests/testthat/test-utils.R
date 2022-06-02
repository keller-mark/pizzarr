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

test_that("create_zarray_meta does not throw error when simple dtype is valid", {
  res <- create_zarray_meta(dtype = "|u1", order = "C", fill_value = 0, dimension_separator = ".")
  expect_equal(class(res$dtype)[[1]], "scalar")
  expect_equal(as.character(res$dtype), "|u1")
})

test_that("create_zarray_meta throws error when simple dtype byteorder is invalid", {
  f <- function() create_zarray_meta(dtype = ":u1", order = "C", fill_value = 0, dimension_separator = ".")
  expect_error(f())
})

test_that("create_zarray_meta throws error when simple dtype basic type is invalid", {
  f <- function() create_zarray_meta(dtype = "<B1", order = "C", fill_value = 0, dimension_separator = ".")
  expect_error(f())
})

test_that("create_zarray_meta throws error when order is invalid", {
  f <- function() create_zarray_meta(dtype = "|u1", order = "A", fill_value = 0, dimension_separator = ".")
  expect_error(f())
})

test_that("create_zarray_meta throws error when dimension separator is invalid", {
  f <- function() create_zarray_meta(dtype = "|u1", order = "C", fill_value = 0, dimension_separator = "___")
  expect_error(f())
})


test_that("create_zarray_meta throws error when fill_value is invalid for float dtype", {
  f <- function() create_zarray_meta(dtype = "<f2", order = "C", fill_value = 0)
  expect_error(f())
})

