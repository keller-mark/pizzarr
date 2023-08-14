library(pizzarr)


# test_that("get_basic_selection_zd", {
#     # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0e6cdc04c6413e14f57f61d389972ea937c/zarr/tests/test_indexing.py#L70
#     # setup
#     a <- as.scalar(42)
#     z <- create(shape=dim(a), dtype="<f4", fill_value=NA)

#     expect_equal(z$get_shape(), NULL)

#     z$set_item("...", a)

#     sel <- z$get_item("...")

#     expect_equal(a, sel)
# })

test_that("get_basic_selection_1d", {
    # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0e6cdc04c6413e14f57f61d389972ea937c/zarr/tests/test_indexing.py#L70
    # setup
    a <- array(data=42)
    z <- create(shape=dim(a), dtype="<f4", fill_value=NA)

    expect_equal(z$get_shape(), 1)

    z$set_item("...", a)

    sel <- z$get_item("...")

    print(sel)

    expect_equal(a, sel$data)
})