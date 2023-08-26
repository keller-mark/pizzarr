library(pizzarr)

test_that("Create Zarr objects", {

  #Get array dummy data
  a <- array(data = 1:20, dim = c(4,5))

  #Create Zarr with lower level function
  z <- zarr_create(shape = dim(a), dtype = "<i4", fill_value = NA)
  z$set_item("...", a)

  #Compare to zarr and as_zarr
  expect_equal(zarr(a), z)
  expect_equal(as_zarr(a), z)

  #Get vector dummy data
  a <- c(1:10)

  #Create Zarr with lower level function
  z <- zarr_create(shape = 10, dtype = "<i4", fill_value = NA)
  z$set_item("...", as.array(a))

  #Compare to zarr and as_zarr
  expect_equal(zarr(a), z)

  #Get matrix dummy data
  a <- matrix(1:10, nrow = 2)

  #Create Zarr with lower level function
  z <- zarr_create(shape = c(2,5), dtype = "<i4", fill_value = NA)
  z$set_item("...", as.array(a))

  #Compare to zarr and as_zarr
  expect_equal(zarr(a), z)

  #Check custom errors
  expect_error(zarr(data.frame(x = 1:10, y = 1:10)),
    "Zarr arrays can only be created from arrays, matrices or vectors")
})
