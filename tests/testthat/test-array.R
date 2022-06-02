library(pizzarr)

test_that("Zarr Array class can load .zarray metadata", {
  store <- MemoryStore$new()

  zarray_meta_char <- jsonlite::toJSON(create_zarray_meta(
    dtype = "|u1",
    order = "C",
    fill_value = 0,
    shape = c(1, 2),
    chunks = c(3, 4),
    dimension_separator = "."
  ))

  store$set_item(".zarray", charToRaw(zarray_meta_char))
  
  array <- Array$new(store = store)
  shape <- array$get_shape()
  
  expect_equal(shape, c(1, 2))
})
