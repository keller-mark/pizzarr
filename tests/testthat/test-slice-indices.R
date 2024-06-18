library(pizzarr)

test_that("Slice$indices() behavior matches Python slice().indices()", {
    # Python slice.indices() only returns (start, stop, step).
    # (Zarr.js returns (start, stop, step, adjusted_length).)
    expect_equal(Slice$new(0, 10, 1)$indices(10)[1:3], c(0, 10, 1))
    expect_equal(Slice$new(0, 10, 1)$indices(11)[1:3], c(0, 10, 1))
    expect_equal(Slice$new(0, 10, 1)$indices(12)[1:3], c(0, 10, 1))
    expect_equal(Slice$new(0, 10, 2)$indices(12)[1:3], c(0, 10, 2))

    expect_equal(Slice$new(1, 10, 2)$indices(5)[1:3], c(1, 5, 2))

    expect_equal(Slice$new(9, 10, 2)$indices(5)[1:3], c(5, 5, 2))
    expect_equal(Slice$new(10, 10, 2)$indices(5)[1:3], c(5, 5, 2))
    expect_equal(Slice$new(10, 10, 2)$indices(10)[1:3], c(10, 10, 2))
    expect_equal(Slice$new(10, 10, 2)$indices(11)[1:3], c(10, 10, 2))

    expect_equal(Slice$new(0, 10, -1)$indices(12)[1:3], c(0, 10, -1))

    expect_equal(Slice$new(-1, 10, -1)$indices(14)[1:3], c(13, 10, -1))

    expect_equal(Slice$new(0, 1, 1)$indices(1)[1:3], c(0, 1, 1))
})

test_that("zb_slice gets converted to Slice", {
    s1 <- zb_slice(0, 10, 1)
    s2 <- Slice$new(0, 10, 1)
    expect_equal(s1$start, s2$start)
    expect_equal(s1$stop, s2$stop)
    expect_equal(s1$step, s2$step)
})

test_that("slice gets converted to zb_slice", {
    s1 <- slice(1, 9, 1)
    s2 <- zb_slice(0, 9, 1)
    expect_equal(s1$start, s2$start)
    expect_equal(s1$stop, s2$stop)
    expect_equal(s1$step, s2$step)
})

test_that("step size greater than 1", {
  g <- zarr_volcano()
  
  s1 <- slice(1, 11, 1)
  s2 <- slice(1, 11, 2)
  s3 <- slice(1, 11, 3)
  
  d1 <- g$get_item("volcano")$get_item(list(s1, s1))$data
  d2 <- g$get_item("volcano")$get_item(list(s2, s2))$data
  d3 <- g$get_item("volcano")$get_item(list(s3, s3))$data
  
  expect_equal(dim(d1), c(11, 11))
  expect_equal(dim(d2), c(6, 6))
  expect_equal(dim(d3), c(4, 4))

  # ground truth for the volcano section with step size 2
  a2 <- t(array(data=c(
    100, 101, 101, 101, 100, 101,
    102, 103, 103, 103, 102, 103,
    104, 105, 105, 105, 104, 104,
    105, 106, 107, 107, 106, 105,
    107, 108, 109, 109, 108, 108,
    109, 110, 111, 111, 110, 112
  ), dim=c(6, 6)))
  expect_equal(d2, a2)

  # ground truth for the volcano section with step size 3
  a3 <- t(array(data=c(
    100, 101, 101, 100,
    103, 104, 104, 103,
    105, 107, 107, 105,
    108, 110, 110, 108
  ), dim=c(4, 4)))
  expect_equal(d3, a3)
})