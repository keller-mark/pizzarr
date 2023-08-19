library(pizzarr)

test_that("NestedArray can be accessed using list of slices", {
    a <- array(1:20, dim = c(2, 10))
    na <- NestedArray$new(a, shape = c(2, 10), dtype = "<f4")

    expect_equal(na$data, a)

    # R-based indexing
    r_sel <- na$get(list(slice(1, 2), slice(1, 3)))
    # print(r_sel$data)

    # Python-based indexing

    python_sel <- na$get(list(zb_slice(0, 2), zb_slice(0, 3)))
    # print(python_sel$data)

    expect_equal(r_sel$data, python_sel$data)
})

test_that("NestedArray can be set using list of slices", {
    a <- array(1:20, dim = c(2, 10))
    na <- NestedArray$new(a, shape = c(2, 10), dtype = "<f4")

    expect_equal(na$data, a)

    new_a <- array(1:6, dim = c(2, 3))
    new_na <- NestedArray$new(new_a, shape = c(2, 3), dtype = "<f4")

    # R-based indexing
    na$set(list(slice(1, 2), slice(1, 3)), new_na)
    # print(r_sel$data)

    r_sel <- na$get(list(slice(1, 2), slice(1, 3)))

    expect_equal(r_sel$data, new_a)
})

test_that("NestedArray can be set using list of zb_slices", {
    a <- array(1:20, dim = c(2, 10))
    na <- NestedArray$new(a, shape = c(2, 10), dtype = "<f4")

    expect_equal(na$data, a)

    new_a <- array(1:6, dim = c(2, 3))
    new_na <- NestedArray$new(new_a, shape = c(2, 3), dtype = "<f4")

    # R-based indexing
    na$set(list(zb_slice(0, 2), zb_slice(0, 3)), new_na)
    # print(r_sel$data)

    python_sel <- na$get(list(zb_slice(0, 2), zb_slice(0, 3)))

    expect_equal(python_sel$data, new_a)
})