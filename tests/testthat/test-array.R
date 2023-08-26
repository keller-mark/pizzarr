library(pizzarr)

test_that("Zarr Array can load .zarray metadata", {
  store <- MemoryStore$new()

  zarray_meta <- store$metadata_class$encode_array_metadata(create_zarray_meta(
    dtype = "|u1",
    order = "C",
    fill_value = 0,
    shape = c(1, 2),
    chunks = c(3, 4),
    dimension_separator = "."
  ))

  store$set_item(".zarray", zarray_meta)
  
  array <- ZarrArray$new(store = store)
  shape <- array$get_shape()
  
  expect_equal(shape, c(1, 2))
})

test_that("Zarr Array can be resized", {
  store <- MemoryStore$new()
  
  zarray_meta <- store$metadata_class$encode_array_metadata(create_zarray_meta(
    dtype = "|u1",
    order = "C",
    fill_value = 0,
    shape = c(4, 5),
    chunks = c(3, 4),
    dimension_separator = "."
  ))
  
  store$set_item(".zarray", zarray_meta)
  
  array <- ZarrArray$new(store = store)
  old_shape <- array$get_shape()
  expect_equal(old_shape, c(4, 5))
  array$resize(1, 2)
  
  new_shape <- array$get_shape()
  expect_equal(new_shape, c(1, 2))
})

test_that("S3 methods of Zarr object", {
  a <- array(data = 1:20, dim = c(4,5))
  z <- zarr(a)

  #subetting with brackets `[`
  expect_equal(z[4,5], z$get_item(list(slice(4, 4), slice(5, 5))))
  expect_equal(z[1:3,5], z$get_item(list(slice(1, 3), slice(5, 5))))
  #TODO slicing by step size is not yet supported
  expect_error(z[1,seq(1,5,2)])
  expect_equal(z[1,seq(1,5)], z$get_item(list(slice(1, 1), slice(1, 5))))
  # expect_equal(z[1,seq(1,5,2)], z$get_item(list(slice(1, 1), slice(1, 5, 2))))
  expect_equal(z[1,], z$get_item(list(slice(1, 1))))
  expect_error(z[1], "This Zarr object has 2 dimensions, 1 were supplied")

  #Overwriting values with brackets `[`
  expect_error(z[1,1] <- 10,
    "Updating values using bracket subsetting is currently not possible")
  
  #Converting Zarr to array
  expect_equal(as.array(z), a)
})

test_that("S3 methods for NestedArray object", {
  a <- array(data = 1:20, dim = c(4,5))
  z <- zarr(a)
 
  #Converting Zarr to array
  expect_equal(as.array(z[1,1:2]), array(c(1,5), dim = c(1,2)))
})
