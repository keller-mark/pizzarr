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