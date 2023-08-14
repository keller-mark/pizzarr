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

test_that("get_basic_selection_2d - can set_item with array", {
    a <- array(data=1:20, dim=c(2, 10))
    #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    # [1,]    1    3    5    7    9   11   13   15   17    19
    # [2,]    2    4    6    8   10   12   14   16   18    20
    z <- create(shape=dim(a), dtype="<f4", fill_value=NA)

    expect_equal(z$get_shape(), c(2, 10))

    expect_equal(z$get_chunks(), c(2, 10))

    z$set_item("...", a)

    sel <- z$get_item("...")

    expect_equal(a, sel$data)
})

test_that("get_basic_selection_2d - can set_item with NestedArray", {
    a <- array(data=1:20, dim=c(2, 10))
    #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    # [1,]    1    3    5    7    9   11   13   15   17    19
    # [2,]    2    4    6    8   10   12   14   16   18    20
    z <- create(shape=dim(a), dtype="<f4", fill_value=NA)

    expect_equal(z$get_shape(), c(2, 10))

    expect_equal(z$get_chunks(), c(2, 10))

    z$set_item("...", NestedArray$new(data = a, shape = dim(a)))

    sel <- z$get_item("...")

    expect_equal(a, sel$data)
})

test_that("get_basic_selection_2d - can set_item for subset", {
    z <- create(shape=c(2, 10), dtype="<f4", fill_value=NA)

    expect_equal(z$get_shape(), c(2, 10))

    expect_equal(z$get_chunks(), c(2, 10))

    a <- array(data=1:10, dim=c(2, 5))
    #      [,1] [,2] [,3] [,4] [,5]
    # [1,]    1    3    5    7    9
    # [2,]    2    4    6    8   10

    z$set_item(list(slice(0, 2), slice(0, 5)), a)

    sel <- z$get_item("...")


    expected_out <- array(data=NA, dim=c(2, 10))
    expected_out[1, 1:5] <- c(1, 3, 5, 7, 9)
    expected_out[2, 1:5] <- c(2, 4, 6, 8, 10)

    expect_equal(expected_out, sel$data)
})

test_that("get_basic_selection_2d - can get_item for subset", {
    a <- array(data=1:20, dim=c(2, 10))
    #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    # [1,]    1    3    5    7    9   11   13   15   17    19
    # [2,]    2    4    6    8   10   12   14   16   18    20
    z <- create(shape=dim(a), dtype="<f4", fill_value=NA)

    expect_equal(z$get_shape(), c(2, 10))

    expect_equal(z$get_chunks(), c(2, 10))

    z$set_item("...", a)

    sel <- z$get_item(list(slice(0, 2), slice(0, 5)))

    expected_out <- array(data=1:10, dim=c(2, 5))
    expect_equal(expected_out, sel$data)
})

test_that("get_basic_selection_2d - can get_item for subset with one offset", {
    a <- array(data=1:20, dim=c(2, 10))
    #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    # [1,]    1    3    5    7    9   11   13   15   17    19
    # [2,]    2    4    6    8   10   12   14   16   18    20
    z <- create(shape=dim(a), dtype="<f4", fill_value=NA)

    expect_equal(z$get_shape(), c(2, 10))

    expect_equal(z$get_chunks(), c(2, 10))

    z$set_item("...", a)

    sel <- z$get_item(list(slice(1, 2), slice(0, 5)))


    expected_out <- array(data=1:10, dim=c(2, 5))
    expect_equal(expected_out, sel$data)
})