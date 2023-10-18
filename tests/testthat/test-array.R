library(pizzarr)

test_that("Zarr Array can load .zarray metadata", {
  store <- MemoryStore$new()

  zarray_meta <- store$metadata_class$encode_array_metadata(create_zarray_meta(
    dtype = Dtype$new("|u1"),
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
    dtype = Dtype$new("|u1"),
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
