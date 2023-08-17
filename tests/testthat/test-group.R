library(pizzarr)


test_that("create group", {
    z <- zarr_create_group()

    name <- z$get_name()

    expect_equal(name, "/")
})

test_that("create nested groups", {
    g1 <- zarr_create_group()
    g2 <- g1$create_group("foo")
    g3 <- g2$create_group("bar")

    name <- g3$get_name()
    expect_equal(name, "/foo/bar")
})

test_that("create array within a nested group", {
    g1 <- zarr_create_group()
    g2 <- g1$create_group("foo")
    g3 <- g2$create_group("bar")

    data <- array(data=1:10, dim=c(2, 5))
    a <- g3$create_dataset("baz", data=data, shape=dim(data))

    expect_equal(a$get_shape(), c(2, 5))
    expect_equal(a$get_name(), "/foo/bar/baz")

    zb_sel <- a$get_item(list(zb_slice(0, 2), zb_slice(0, 2)))
    ob_sel <- a$get_item(list(slice(1, 2), slice(1, 2)))

    expect_equal(zb_sel$data, ob_sel$data)
})