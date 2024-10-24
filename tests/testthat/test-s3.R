library(pizzarr)

test_that("can use as.array on NestedArray", {
    a <- array(data=1:20, dim=c(2, 10))
    #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    # [1,]    1    3    5    7    9   11   13   15   17    19
    # [2,]    2    4    6    8   10   12   14   16   18    20
    z <- zarr_create(shape=dim(a), dtype="<f4", fill_value=NA)

    expect_equal(z$get_shape(), c(2, 10))
    expect_equal(z$get_chunks(), c(2, 10))

    z$set_item("...", a)

    sel <- z$get_item("...")

    expect_equal(a, as.array(sel))
})

test_that("can use as.array on ZarrArray", {
    a <- array(data=1:20, dim=c(2, 10))
    #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    # [1,]    1    3    5    7    9   11   13   15   17    19
    # [2,]    2    4    6    8   10   12   14   16   18    20
    z <- zarr_create(shape=dim(a), dtype="<f4", fill_value=NA)

    expect_equal(z$get_shape(), c(2, 10))
    expect_equal(z$get_chunks(), c(2, 10))

    z$set_item("...", a)

    expect_equal(a, as.array(z))
})

test_that("can use bracket notation to get values from ZarrArray", {
    a <- array(data=1:20, dim=c(2, 10))
    #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    # [1,]    1    3    5    7    9   11   13   15   17    19
    # [2,]    2    4    6    8   10   12   14   16   18    20
    z <- zarr_create(shape=dim(a), dtype="<f4", fill_value=NA)

    expect_equal(z$get_shape(), c(2, 10))
    expect_equal(z$get_chunks(), c(2, 10))

    z$set_item("...", a)

    sel <- z[1:2, 1:3]

    b <- array(data=c(1, 2, 3, 4, 5, 6), dim=c(2, 3))

    expect_equal(b, as.array(sel))
})

test_that("S3 methods of Zarr object", {
    a <- array(data=1:20, dim=c(2, 10))
    #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    # [1,]    1    3    5    7    9   11   13   15   17    19
    # [2,]    2    4    6    8   10   12   14   16   18    20
    z <- zarr_create(shape=dim(a), dtype="<f4", fill_value=NA)

    expect_equal(z$get_shape(), c(2, 10))
    expect_equal(z$get_chunks(), c(2, 10))

    z$set_item("...", a)

    #subetting with brackets `[`
    expect_equal(z[4,5], z$get_item(list(slice(4, 4), slice(5, 5))))
    # TODO: decide if this is ultimately how we want 1:3 (stop beyond end of dimension) to behave
    expect_equal(z[1:3,5], z$get_item(list(slice(1, 3), slice(5, 5))))
    
    # with seq
    # for now seq(from, to, by) does not use slice(start,stop,step)
    expect_equal(
        z[1:2,seq(1,5,2)]$data,
        array(data=c(1, 2, 5, 6, 9, 10), dim=c(2, 3))
    )
    

    # arbitrary indices
    expect_equal(
        z[c(2,1),c(10,1,6)]$data,
        array(data=c(20, 19, 2, 1, 12, 11), dim=c(2, 3))
    )
      
    # compare regular slicing and step size
    expect_error(z[1], "This Zarr object has 2 dimensions, 1 were supplied")
    
    # test `[.ZarrArray` against get_item
    expect_equal(z[1:2,5], z$get_item(list(slice(1, 3), slice(5, 5))))
    expect_equal(z[1,seq(1,5)], z$get_item(list(slice(1, 1), slice(1, 5))))
    expect_equal(z[1,seq(1,5,2)], z$get_item(list(slice(1, 1), slice(1, 5, 2))))
    expect_equal(z[1,], z$get_item(list(slice(1, 1))))
    
    #Converting Zarr to array
    expect_equal(as.array(z), a)
})
