
test_that("selection functionality", {
  
  a <- zarr_volcano()$get_item("volcano")
  
  sub_a <- a$get_item(list(slice(1, 10), "..."))
  
  expect_equal(sub_a$shape, c(10, a$get_shape()[2]))
  
  sub_a <- a$get_item(list(":", slice(1, 10)))
  
  expect_equal(sub_a$shape, c(a$get_shape()[1], 10))
  
  sub_a <- a$get_item(list(1, "..."))
  
  expect_equal(sub_a$shape, c(1, a$get_shape()[2]))
  
  sub_a <- a$get_item(list("...", slice(1, 10)))
  
  expect_equal(sub_a$shape, c(a$get_shape()[1], 10))
  
})

test_that("checking pure fancy indexing", {
  
  # check integer input, list vector or scalar
  expect_equal(is_pure_fancy_indexing(list(1:3, 1, 1:2)), TRUE)
  expect_equal(is_pure_fancy_indexing(list(1:5, 1, 1:2)), TRUE)
  expect_equal(is_pure_fancy_indexing(list(3, 1, 10)), TRUE)
  expect_equal(is_pure_fancy_indexing(c(1,5,2)), TRUE)
  expect_equal(is_pure_fancy_indexing(list(3, list(1,2,3), 10)), TRUE)
  
  # checking slices
  expect_equal(is_pure_fancy_indexing(list(1:5, 1, slice(1,2))), FALSE)
  expect_equal(is_pure_fancy_indexing(list(1:5, slice(1,3), slice(1,2))), FALSE)
  
  # checking non-integers
  expect_equal(is_pure_fancy_indexing(list(1:5, 1.2, 1:2)), FALSE)
  expect_equal(is_pure_fancy_indexing(c(1,5,2.4)), FALSE)
  expect_equal(is_pure_fancy_indexing(list(3, list(1,2,3.3), 10)), FALSE)
  
})

test_that("Order class", {
  
  # fields
  expect_equal(Order$public_fields$UNKNOWN, 0)
  expect_equal(Order$public_fields$INCREASING,1)
  expect_equal(Order$public_fields$DECREASING,2)
  expect_equal(Order$public_fields$UNORDERED,3)
  
  # methods
  expect_equal(Order$public_methods$check(1:5),1)
  expect_equal(Order$public_methods$check(seq(2,10,2)),1)
  expect_equal(Order$public_methods$check(10:1),2)
  expect_equal(Order$public_methods$check(c(1,5,2,6)),3)
  expect_error(Order$public_methods$check(c("a", 2)))
})