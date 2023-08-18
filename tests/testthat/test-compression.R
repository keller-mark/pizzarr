library(pizzarr)


test_that("can get/set with compressor involved", {
    a <- array(data=as.double(1:20), dim=c(2, 10))
    #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    # [1,]    1    3    5    7    9   11   13   15   17    19
    # [2,]    2    4    6    8   10   12   14   16   18    20
    compressor <- get_codec(list(id="zstd", level=1))
    z <- zarr_create(shape=dim(a), dtype="<f8", fill_value=NA, compressor = compressor)

    expect_equal(z$get_shape(), c(2, 10))
    expect_equal(z$get_chunks(), c(2, 10))

    z$set_item("...", a)

    sel <- z$get_item(list(slice(1, 2), slice(1, 5)))

    expected_out <- array(data=1:10, dim=c(2, 5))
    expect_equal(expected_out, sel$data)
})