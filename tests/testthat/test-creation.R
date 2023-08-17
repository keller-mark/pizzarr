library(pizzarr)

test_that("empty", {
  z <- zarr_create_empty(100, chunks=10)
  shape <- z$get_shape()
  chunks <- z$get_chunks()
  
  expect_equal(shape, c(100))
  expect_equal(chunks, c(10))
})


test_that("zeros", {
  z <- zarr_create_zeros(100, chunks=10)
  shape <- z$get_shape()
  chunks <- z$get_chunks()
  
  expect_equal(shape, c(100))
  expect_equal(chunks, c(10))

  #array_contents <- z$get_item()
  #expect_equal(array_contents, rep(0, 100))
})

test_that("create zero-dimensional", {
    a <- as_scalar(42)
    z <- zarr_create(shape=dim(a), dtype="<f4", fill_value=NA)
    expect_equal(z$get_shape(), NULL)
})
