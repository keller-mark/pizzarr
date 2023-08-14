library(pizzarr)


test_that("get_basic_selection_zd", {
    # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0e6cdc04c6413e14f57f61d389972ea937c/zarr/tests/test_indexing.py#L70
    a <- as.scalar(42)
    z <- create(shape=dim(a), dtype="<f4", fill_value=NA)

    expect_equal(z$get_shape(), NULL)

    z$set_item("...", a)

    sel <- z$get_item("...")

    expect_equal(as.numeric(a), as.numeric(sel))
})

test_that("get_basic_selection_1d", {
    # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0e6cdc04c6413e14f57f61d389972ea937c/zarr/tests/test_indexing.py#L70
    a <- array(data=42)
    z <- create(shape=dim(a), dtype="<f4", fill_value=NA)

    expect_equal(z$get_shape(), 1)

    z$set_item("...", a)

    sel <- z$get_item("...")

    expect_equal(a, sel$data)
})

test_that("get_basic_selection_2d", {
    a <- array(data=1:20, dim=c(2, 10))
    #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    # [1,]    1    3    5    7    9   11   13   15   17    19
    # [2,]    2    4    6    8   10   12   14   16   18    20
    z <- create(shape=dim(a), dtype="<f4", fill_value=NA)

    expect_equal(z$get_shape(), c(2, 10))

    z$set_item("...", a)

    sel <- z$get_item("...")
    print(sel$data)

    expect_equal(a, sel$data)
})