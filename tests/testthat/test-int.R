library(pizzarr)

test_that("int", {

  # is_integer
  expect_equal(is_integer(2), TRUE)
  expect_equal(is_integer(2.2), FALSE)
  expect_equal(is_integer(c(1,2,3)), FALSE)
  expect_equal(is_integer(list(1,2,3)), FALSE)
  
  # is_integer_vec
  expect_equal(is_integer_vec(c(2,3)), TRUE)
  expect_equal(is_integer_vec(c(2,3.3)), FALSE)
  expect_equal(is_integer_vec(2), FALSE)
  expect_equal(is_integer_vec(list(2,3)), FALSE)
  
  # is_integer_list
  expect_equal(is_integer_list(list(2,3)), TRUE)
  expect_equal(is_integer_list(c(2,3)), FALSE)
  expect_equal(is_integer_list(list(2,3.3)), FALSE)
})