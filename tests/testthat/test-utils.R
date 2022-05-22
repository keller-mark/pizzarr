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

