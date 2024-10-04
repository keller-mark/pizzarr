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

test_that("basic indexer for array that spans multiple chunks", {
    z <- MockArray$new(shape = c(10, 10), chunks = c(5, 5))

    expect_equal(z$get_shape(), c(10, 10))
    expect_equal(z$get_chunks(), c(5, 5))

    bi <- BasicIndexer$new(list(zb_slice(0, 11), zb_slice(0, 11)), z)

    expect_equal(as.numeric(bi$shape), c(10, 10))

    expect_equal(length(bi$dim_indexers), 2)

    expect_equal("SliceDimIndexer" %in% class(bi$dim_indexers[[1]]), TRUE)
    expect_equal("SliceDimIndexer" %in% class(bi$dim_indexers[[2]]), TRUE)
    
    sdi1 <- bi$dim_indexers[[1]]
    expect_equal(sdi1$start, 0)
    expect_equal(sdi1$stop, 10)
    expect_equal(sdi1$step, 1)
    expect_equal(sdi1$dim_len, 10)
    expect_equal(sdi1$dim_chunk_len, 5)
    expect_equal(sdi1$num_items, 10)
    expect_equal(sdi1$num_chunks, 2)

    sdi2 <- bi$dim_indexers[[2]]
    expect_equal(sdi2$start, 0)
    expect_equal(sdi2$stop, 10)
    expect_equal(sdi2$step, 1)
    expect_equal(sdi2$dim_len, 10)
    expect_equal(sdi2$dim_chunk_len, 5)
    expect_equal(sdi2$num_items, 10)
    expect_equal(sdi2$num_chunks, 2)
})

test_that("basic indexer for array that spans multiple chunks where shape is not a multiple", {
    z <- MockArray$new(shape = c(10, 10), chunks = c(3, 3))

    expect_equal(z$get_shape(), c(10, 10))
    expect_equal(z$get_chunks(), c(3, 3))

    bi <- BasicIndexer$new(list(zb_slice(0, 11), zb_slice(0, 11)), z)

    expect_equal(as.numeric(bi$shape), c(10, 10))

    expect_equal(length(bi$dim_indexers), 2)

    expect_equal("SliceDimIndexer" %in% class(bi$dim_indexers[[1]]), TRUE)
    expect_equal("SliceDimIndexer" %in% class(bi$dim_indexers[[2]]), TRUE)
    
    sdi1 <- bi$dim_indexers[[1]]
    expect_equal(sdi1$start, 0)
    expect_equal(sdi1$stop, 10)
    expect_equal(sdi1$step, 1)
    expect_equal(sdi1$dim_len, 10)
    expect_equal(sdi1$dim_chunk_len, 3)
    expect_equal(sdi1$num_items, 10)
    expect_equal(sdi1$num_chunks, 4)

    sdi2 <- bi$dim_indexers[[2]]
    expect_equal(sdi2$start, 0)
    expect_equal(sdi2$stop, 10)
    expect_equal(sdi2$step, 1)
    expect_equal(sdi2$dim_len, 10)
    expect_equal(sdi2$dim_chunk_len, 3)
    expect_equal(sdi2$num_items, 10)
    expect_equal(sdi2$num_chunks, 4)
})

test_that("basic indexer for array that spans multiple chunks where shape is not a multiple with offsets", {
    z <- MockArray$new(shape = c(10, 10), chunks = c(3, 3))

    expect_equal(z$get_shape(), c(10, 10))
    expect_equal(z$get_chunks(), c(3, 3))

    bi <- BasicIndexer$new(list(zb_slice(5, 10), zb_slice(5, 10)), z)

    expect_equal(as.numeric(bi$shape), c(5, 5))
    
    expect_equal(length(bi$dim_indexers), 2)

    expect_equal("SliceDimIndexer" %in% class(bi$dim_indexers[[1]]), TRUE)
    expect_equal("SliceDimIndexer" %in% class(bi$dim_indexers[[2]]), TRUE)
    
    sdi1 <- bi$dim_indexers[[1]]
    expect_equal(sdi1$start, 5)
    expect_equal(sdi1$stop, 10)
    expect_equal(sdi1$step, 1)
    expect_equal(sdi1$dim_len, 10)
    expect_equal(sdi1$dim_chunk_len, 3)
    expect_equal(sdi1$num_items, 5)
    expect_equal(sdi1$num_chunks, 4)

    sdi2 <- bi$dim_indexers[[2]]
    expect_equal(sdi2$start, 5)
    expect_equal(sdi2$stop, 10)
    expect_equal(sdi2$step, 1)
    expect_equal(sdi2$dim_len, 10)
    expect_equal(sdi2$dim_chunk_len, 3)
    expect_equal(sdi2$num_items, 5)
    expect_equal(sdi2$num_chunks, 4)
})

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