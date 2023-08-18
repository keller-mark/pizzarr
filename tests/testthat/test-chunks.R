library(pizzarr)

test_that("can get array that spans multiple chunks", {
    a <- array(data=1:100, dim=c(10, 10))
    #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    # [1,]    1    3    5    7    9   11   13   15   17    19
    # [2,]    2    4    6    8   10   12   14   16   18    20
    z <- zarr_create_array(data = a, shape=dim(a), chunks=c(5, 5), dtype="<f4", fill_value=NA)

    expect_equal(z$get_shape(), c(10, 10))
    expect_equal(z$get_chunks(), c(5, 5))

    sel <- z$get_item(list(slice(1, 10), slice(1, 10)))
    expect_equal(a, sel$data)
})