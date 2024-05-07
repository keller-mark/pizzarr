library(pizzarr)


test_that("get_basic_selection_zd", {
    # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0e6cdc04c6413e14f57f61d389972ea937c/zarr/tests/test_indexing.py#L70
    a <- as_scalar(42)
    z <- zarr_create(shape=dim(a), dtype="<f4", fill_value=NA)

    expect_equal(z$get_shape(), NULL)

    z$set_item("...", a)

    sel <- z$get_item("...")

    expect_equal(as.numeric(a), as.numeric(sel$data))
})

test_that("get_basic_selection_zd with anndataR IntScalar fixture", {
    root <- system.file("extdata", "fixtures", "v2", "example.zarr", package="pizzarr")
    
    store <- DirectoryStore$new(root)
    z <- zarr_open_array(store, path = "uns/IntScalar")

    expect_equal(z$get_shape(), integer(0))

    sel <- z$get_item("...")
    scalar <- as.numeric(sel$data)

    expect_equal(scalar, 1)
})

test_that("get_basic_selection_zd with anndataR StringScalar fixture", {
    root <- system.file("extdata", "fixtures", "v2", "example.zarr", package="pizzarr")
    
    store <- DirectoryStore$new(root)
    z <- zarr_open_array(store, path = "uns/StringScalar")

    expect_equal(z$get_shape(), integer(0))

    sel <- z$get_item("...")
    scalar <- as.character(sel$data)

    expect_equal(scalar, "A string")
})

test_that("get_basic_selection_1d", {
    # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0e6cdc04c6413e14f57f61d389972ea937c/zarr/tests/test_indexing.py#L70
    a <- array(data=42)
    z <- zarr_create(shape=dim(a), dtype="<f4", fill_value=NA)

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
    z <- zarr_create(shape=dim(a), dtype="<f4", fill_value=NA)

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
    z <- zarr_create(shape=dim(a), dtype="<f4", fill_value=NA)

    expect_equal(z$get_shape(), c(2, 10))
    expect_equal(z$get_chunks(), c(2, 10))

    z$set_item("...", NestedArray$new(data = a, shape = dim(a)))

    sel <- z$get_item("...")

    expect_equal(a, sel$data)
})

test_that("get_basic_selection_2d(zero-based) - can set_item for subset", {
    z <- zarr_create(shape=c(2, 10), dtype="<f4", fill_value=NA)

    expect_equal(z$get_shape(), c(2, 10))
    expect_equal(z$get_chunks(), c(2, 10))

    a <- array(data=1:10, dim=c(2, 5))
    #      [,0] [,1] [,2] [,3] [,4]
    # [0,]    1    3    5    7    9
    # [1,]    2    4    6    8   10

    z$set_item(list(zb_slice(0, 2), zb_slice(0, 5)), a)

    sel <- z$get_item("...")

    expected_out <- array(data=NA, dim=c(2, 10))
    expected_out[1, 1:5] <- c(1, 3, 5, 7, 9)
    expected_out[2, 1:5] <- c(2, 4, 6, 8, 10)

    expect_equal(expected_out, sel$data)
})

test_that("get_basic_selection_2d(zero-based) - can get_item for subset", {
    a <- array(data=1:20, dim=c(2, 10))
    #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    # [1,]    1    3    5    7    9   11   13   15   17    19
    # [2,]    2    4    6    8   10   12   14   16   18    20
    z <- zarr_create(shape=dim(a), dtype="<f4", fill_value=NA)

    expect_equal(z$get_shape(), c(2, 10))
    expect_equal(z$get_chunks(), c(2, 10))

    z$set_item("...", a)

    sel <- z$get_item(list(zb_slice(0, 2), zb_slice(0, 5)))

    expected_out <- array(data=1:10, dim=c(2, 5))
    expect_equal(expected_out, sel$data)
})

test_that("get_basic_selection_2d(zero-based) - can get_item for subset with one offset on first axis", {
    a <- array(data=1:20, dim=c(2, 10))
    #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    # [1,]    1    3    5    7    9   11   13   15   17    19
    # [2,]    2    4    6    8   10   12   14   16   18    20
    z <- zarr_create(shape=dim(a), dtype="<f4", fill_value=NA)

    expect_equal(z$get_shape(), c(2, 10))
    expect_equal(z$get_chunks(), c(2, 10))

    z$set_item("...", a)

    sel <- z$get_item(list(zb_slice(1, 3), zb_slice(0, 5)))

    expected_out <- array(data=NA, dim=c(1, 5))
    expected_out[1,] <- c(2, 4, 6, 8, 10)
    expect_equal(expected_out, sel$data)
})

test_that("get_basic_selection_2d(zero-based) - can get_item for subset with one offset on both axes", {
    a <- array(data=1:20, dim=c(2, 10))
    #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    # [1,]    1    3    5    7    9   11   13   15   17    19
    # [2,]    2    4    6    8   10   12   14   16   18    20
    z <- zarr_create(shape=dim(a), dtype="<f4", fill_value=NA)

    expect_equal(z$get_shape(), c(2, 10))
    expect_equal(z$get_chunks(), c(2, 10))

    z$set_item("...", a)

    sel <- z$get_item(list(zb_slice(1, 2), zb_slice(1, 5)))

    expected_out <- array(data=NA, dim=c(1, 4))
    expected_out[1,] <- c(4, 6, 8, 10)
    expect_equal(expected_out, sel$data)
})

test_that("get_basic_selection_2d(one-based) - can get_item for subset with same index for start/stop", {
    a <- array(data=1:20, dim=c(2, 10))
    #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    # [1,]    1    3    5    7    9   11   13   15   17    19
    # [2,]    2    4    6    8   10   12   14   16   18    20
    z <- zarr_create(shape=dim(a), dtype="<f4", fill_value=NA)

    expect_equal(z$get_shape(), c(2, 10))
    expect_equal(z$get_chunks(), c(2, 10))

    z$set_item("...", a)

    sel <- z$get_item(list(slice(2, 2), slice(2, 5)))

    expected_out <- array(data=NA, dim=c(1, 4))
    expected_out[1,] <- c(4, 6, 8, 10)
    expect_equal(expected_out, sel$data)
})

test_that("get_basic_selection_2d(one-based) - can get_item for subset with one offset on both axes", {
    a <- array(data=1:20, dim=c(2, 10))
    #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    # [1,]    1    3    5    7    9   11   13   15   17    19
    # [2,]    2    4    6    8   10   12   14   16   18    20
    z <- zarr_create(shape=dim(a), dtype="<f4", fill_value=NA)

    expect_equal(z$get_shape(), c(2, 10))
    expect_equal(z$get_chunks(), c(2, 10))

    z$set_item("...", a)

    sel <- z$get_item(list(slice(1, 2), slice(1, 5)))

    expected_out <- array(data=1:10, dim=c(2, 5))
    expect_equal(expected_out, sel$data)
})

test_that("get_basic_selection_3d(one-based) - can get_item for three-dimensional array", {
    a <- array(data=1:24, dim=c(2, 3, 4))
    # , , 1
    #
    #      [,1] [,2] [,3]
    # [1,]    1    3    5
    # [2,]    2    4    6
    #
    # , , 2
    #
    #      [,1] [,2] [,3]
    # [1,]    7    9   11
    # [2,]    8   10   12
    #
    # , , 3
    #
    #      [,1] [,2] [,3]
    # [1,]   13   15   17
    # [2,]   14   16   18
    #
    # , , 4
    #
    #      [,1] [,2] [,3]
    # [1,]   19   21   23
    # [2,]   20   22   24
    z <- zarr_create(shape=dim(a), dtype="<f4", fill_value=NA)

    expect_equal(z$get_shape(), c(2, 3, 4))
    expect_equal(z$get_chunks(), c(2, 3, 4))

    z$set_item("...", a)

    sel <- z$get_item(list(slice(1, 2), slice(1, 2), slice(1, 2)))

    expected_out <- array(data=c(1, 2, 3, 4, 7, 8, 9, 10), dim=c(2, 2, 2))
    # , , 1
    #
    #      [,1] [,2]
    # [1,]    1    3
    # [2,]    2    4
    #
    # , , 2
    #
    #      [,1] [,2]
    # [1,]    7    9
    # [2,]    8   10
    expect_equal(expected_out, sel$data)
})
