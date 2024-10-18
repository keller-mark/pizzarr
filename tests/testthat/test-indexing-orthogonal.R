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
  
  # OrthogonalIndexer supports SliceDimIndexer
  bi <- OrthogonalIndexer$new(list(zb_slice(0, 11), zb_slice(0, 11)), z)
  expect_equal(as.numeric(bi$shape), c(10, 10))
  expect_equal(length(bi$dim_indexers), 2)
  expect_equal("SliceDimIndexer" %in% class(bi$dim_indexers[[1]]), TRUE)
  
  # combination of IntArrayDimIndexer and SliceDimIndexer
  bi <- OrthogonalIndexer$new(list(1:10, zb_slice(0, 11)), z)
  expect_equal("IntArrayDimIndexer" %in% class(bi$dim_indexers[[1]]), TRUE)
  expect_equal("SliceDimIndexer" %in% class(bi$dim_indexers[[2]]), TRUE)
  
  # unordered integer vector is a IntArrayDimIndexer
  bi <- OrthogonalIndexer$new(list(1:10, c(2,3,5,4,1)), z)
  expect_equal("IntArrayDimIndexer" %in% class(bi$dim_indexers[[2]]), TRUE)
  
  # empty dimension is a slice
  bi <- OrthogonalIndexer$new(list(1), z)
  expect_equal("SliceDimIndexer" %in% class(bi$dim_indexers[[2]]), TRUE)
  
  # TODO: add tests for adding BoolArrayDimIndexer
})

test_that("basic indexer for array that spans multiple chunks where shape is not a multiple", {
  z <- MockArray$new(shape = c(10, 10), chunks = c(3, 3))
  
  expect_equal(z$get_shape(), c(10, 10))
  expect_equal(z$get_chunks(), c(3, 3))
  
  # OrthogonalIndexer supports SliceDimIndexer
  bi <- OrthogonalIndexer$new(list(zb_slice(0, 11), zb_slice(0, 11)), z)
  expect_equal(as.numeric(bi$shape), c(10, 10))
  expect_equal(length(bi$dim_indexers), 2)
  expect_equal("SliceDimIndexer" %in% class(bi$dim_indexers[[1]]), TRUE)
  
  # combination of IntArrayDimIndexer and SliceDimIndexer
  bi <- OrthogonalIndexer$new(list(1:10, zb_slice(0, 11)), z)
  expect_equal("IntArrayDimIndexer" %in% class(bi$dim_indexers[[1]]), TRUE)
  expect_equal("SliceDimIndexer" %in% class(bi$dim_indexers[[2]]), TRUE)
  
  # unordered integer vector is a IntArrayDimIndexer
  bi <- OrthogonalIndexer$new(list(1:10, c(2,3,5,4,1)), z)
  expect_equal("IntArrayDimIndexer" %in% class(bi$dim_indexers[[2]]), TRUE)
  
  # empty dimension is a slice
  bi <- OrthogonalIndexer$new(list(1), z)
  expect_equal("SliceDimIndexer" %in% class(bi$dim_indexers[[2]]), TRUE)
  
  # TODO: add tests for adding BoolArrayDimIndexer
})

test_that("int array dimension indexer", {
  
  # ordered int array index
  iad <- IntArrayDimIndexer$new(1:10, 10, 3)
  expect_equal(iad$dim_sel, 1:10)
  expect_equal(iad$dim_chunk_ixs, c(1,2,3,4))
  expect_equal(iad$dim_len, 10)
  expect_equal(iad$dim_chunk_len, 3)
  expect_equal(iad$num_chunks, 4)
  expect_equal(iad$chunk_nitems, c(3,3,3,1))
  expect_equal(iad$order, 1)
  
  # unordered int array index
  iad <- IntArrayDimIndexer$new(c(2,3,5,1,2), 6, 3)
  expect_equal(iad$dim_sel, c(2,3,1,2,5))
  expect_equal(iad$dim_chunk_ixs, c(1,2))
  expect_equal(iad$dim_len, 6)
  expect_equal(iad$dim_chunk_len, 3)
  expect_equal(iad$num_chunks, 2)
  expect_equal(iad$chunk_nitems, c(4,1))
  expect_equal(iad$order, 3)
  
  # error for wrong dimension length
  expect_error(IntArrayDimIndexer$new(1:10, 6, 3))
  
  # missing chunk size
  expect_error(IntArrayDimIndexer$new(1:10, 10))
  
})