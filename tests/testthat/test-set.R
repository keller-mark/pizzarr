library(pizzarr)

test_that("can set a subset of an array using a NestedArray", {
    a_orig <- array(data=1:20, dim=c(2, 10))
    #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    # [1,]    1    3    5    7    9   11   13   15   17    19
    # [2,]    2    4    6    8   10   12   14   16   18    20
    z <- zarr_create_array(data = a_orig, shape=dim(a_orig), dtype="<f4")

    expect_equal(z$get_shape(), c(2, 10))
    expect_equal(z$get_chunks(), c(2, 10))

    s1 <- z$get_item("...")
    expect_equal(a_orig, s1$data)

    a_new <- NestedArray$new(
        data = array(data=c(96, 97, 98, 99), dim=c(2, 2)),
        shape = c(2, 2),
        dtype = "<f4"
    )

    z$set_item(list(slice(1, 2), slice(2, 3)), a_new)

    expected_out <- array(data=1:20, dim=c(2, 10))
    expected_out[1:2, 2:3] <- c(96, 97, 98, 99)
    
    s3 <- z$get_item("...")

    expect_equal(expected_out, s3$data)
})

test_that("can set a subset of an array using a base R array", {
    a_orig <- array(data=1:20, dim=c(2, 10))
    #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    # [1,]    1    3    5    7    9   11   13   15   17    19
    # [2,]    2    4    6    8   10   12   14   16   18    20
    z <- zarr_create_array(data = a_orig, shape=dim(a_orig), dtype="<f4")

    expect_equal(z$get_shape(), c(2, 10))
    expect_equal(z$get_chunks(), c(2, 10))

    s1 <- z$get_item("...")
    expect_equal(a_orig, s1$data)

    a_new <- array(data=c(96, 97, 98, 99), dim=c(2, 2))

    z$set_item(list(slice(1, 2), slice(2, 3)), a_new)

    expected_out <- array(data=1:20, dim=c(2, 10))
    expected_out[1:2, 2:3] <- c(96, 97, 98, 99)
    
    s3 <- z$get_item("...")

    expect_equal(expected_out, s3$data)
})
