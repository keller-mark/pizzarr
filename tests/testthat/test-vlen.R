library(pizzarr)

MockArray <- R6::R6Class("MockArray",
    public = list(
        shape = NULL,
        chunks = NULL,
        initialize = function(shape, chunks) {
            self$shape <- shape
            self$chunks <- chunks
        },
        get_shape = function() {
            return(self$shape)
        },
        get_chunks = function() {
            return(self$chunks)
        }
    )
)

test_that("VLenUTF8 codec can decode - raw to array", {
  codec <- VLenUtf8Codec$new()
  zarr_arr <- MockArray$new(c(4), c(4))
  # ['a', 'b', 'cc', 'd']
  str_arr <- codec$decode(as.raw(c(
    4, 0, 0, 0,
    1, 0, 0, 0,
    97, 1, 0, 0,
    0, 98, 2, 0,
    0, 0, 99, 99,
    1, 0, 0, 0,
    100
  )), zarr_arr)

  expect_equal(str_arr, c("a", "b", "cc", "d"))
})

test_that("VLenUTF8 codec can encode - array to raw", {
  codec <- VLenUtf8Codec$new()
  zarr_arr <- MockArray$new(c(4), c(4))
  # ['a', 'b', 'cc', 'd']
  raw_vec <- codec$encode(c("a", "b", "cc", "d"), zarr_arr)

  expect_equal(raw_vec, as.raw(c(
    4, 0, 0, 0,
    1, 0, 0, 0,
    97, 1, 0, 0,
    0, 98, 2, 0,
    0, 0, 99, 99,
    1, 0, 0, 0,
    100
  )))
})

test_that("VLenUTF8 codec can encode with high level API", {
  store <- MemoryStore$new()
  object_codec <- VLenUtf8Codec$new()

  string <- LETTERS[1:5]
  dims <- length(string)
  data <- array(data = string, dim = dims)

  z <- pizzarr::zarr_create_array(data, store = store, path = "string", dtype = "|O", object_codec = object_codec, shape = dims)
  
  sel <- z$get_item("...")
  expect_equal(as.character(sel$data), string)
})
