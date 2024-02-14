library(pizzarr)

test_that("zarr_create_empty", {
  z <- zarr_create_empty(100, chunks=10)
  shape <- z$get_shape()
  chunks <- z$get_chunks()
  
  expect_equal(shape, c(100))
  expect_equal(chunks, c(10))
})


test_that("zarr_create_zeros", {
  z <- zarr_create_zeros(100, chunks=10)
  shape <- z$get_shape()
  chunks <- z$get_chunks()
  
  expect_equal(shape, c(100))
  expect_equal(chunks, c(10))
})

test_that("zarr_create zero-dimensional", {
    a <- as_scalar(42)
    z <- zarr_create(shape=dim(a), dtype="<f4", fill_value=NA)
    expect_equal(z$get_shape(), NULL)
})

test_that("zarr_save_array and zarr_open_array", {
    a <- array(data=1:20, dim=c(2, 10))
    z <- zarr_create_array(data=a, shape=dim(a), dtype="<f4", fill_value=NA)
    zarr_save_array(file.path("test_data", "test_zarr_save_array.zarr"), z, overwrite = TRUE)

    z2 <- zarr_open_array(file.path("test_data", "test_zarr_save_array.zarr"))
    selection <- z2$get_item("...")

    expect_equal(z$get_shape(), c(2, 10))
    expect_equal(z2$get_shape(), c(2, 10))

    expect_equal(a, selection$data)

    unlink(file.path("test_data", "test_zarr_save_array.zarr"))
})