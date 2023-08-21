library(pizzarr)

test_that("Can open Zarr group using convenience function", {

    root <- system.file("extdata", "fixtures", "v2", "data.zarr", package="pizzarr")
    
    g <- zarr_open_group(root)
    a <- g$get_item("1d.contiguous.lz4.i2")

    expect_equal(a$get_shape(), c(4))
})

test_that("Can open Zarr group and read a 1D 2-byte integer array with LZ4 compression", {

    root <- system.file("extdata", "fixtures", "v2", "data.zarr", package="pizzarr")
    
    store <- DirectoryStore$new(root)
    g <- ZarrGroup$new(store)
    a <- g$get_item("1d.contiguous.lz4.i2")

    expect_equal(a$get_shape(), c(4))
    expect_equal(a$get_chunks(), c(4))

    expect_equal(as.character(a$get_compressor()$get_config()$id), "lz4")

    selection <- a$get_item("...")

    expect_equal(dim(selection$data), c(4))
    expect_equal(selection$data, array(data=c(1, 2, 3, 4), dim=c(4)))
})

test_that("Can open Zarr group and read a 1D 2-byte integer array with Zstd compression", {

    root <- system.file("extdata", "fixtures", "v2", "data.zarr", package="pizzarr")
    
    store <- DirectoryStore$new(root)
    g <- ZarrGroup$new(store)
    a <- g$get_item("1d.contiguous.zstd.i2")

    expect_equal(a$get_shape(), c(4))
    expect_equal(a$get_chunks(), c(4))

    expect_equal(as.character(a$get_compressor()$get_config()$id), "zstd")

    selection <- a$get_item("...")

    expect_equal(dim(selection$data), c(4))
    expect_equal(selection$data, array(data=c(1, 2, 3, 4), dim=c(4)))
})

test_that("Can open Zarr group and read a 1D 2-byte integer array with Blosc compression", {

    root <- system.file("extdata", "fixtures", "v2", "data.zarr", package="pizzarr")
    
    store <- DirectoryStore$new(root)
    g <- ZarrGroup$new(store)
    a <- g$get_item("1d.contiguous.blosc.i2")

    expect_equal(a$get_shape(), c(4))
    expect_equal(a$get_chunks(), c(4))

    expect_equal(as.character(a$get_compressor()$get_config()$id), "blosc")

    selection <- a$get_item("...")

    expect_equal(dim(selection$data), c(4))
    expect_equal(selection$data, array(data=c(1, 2, 3, 4), dim=c(4)))
})

test_that("Can open Zarr group and read a 1D 2-byte integer array with Zlib compression", {

    root <- system.file("extdata", "fixtures", "v2", "data.zarr", package="pizzarr")
    
    store <- DirectoryStore$new(root)
    g <- ZarrGroup$new(store)
    a <- g$get_item("1d.contiguous.zlib.i2")

    expect_equal(a$get_shape(), c(4))
    expect_equal(a$get_chunks(), c(4))

    expect_equal(as.character(a$get_compressor()$get_config()$id), "zlib")

    selection <- a$get_item("...")

    expect_equal(dim(selection$data), c(4))
    expect_equal(selection$data, array(data=c(1, 2, 3, 4), dim=c(4)))
})

test_that("Can open Zarr group and read a 1D 2-byte integer array with no compression", {

    root <- system.file("extdata", "fixtures", "v2", "data.zarr", package="pizzarr")
    
    store <- DirectoryStore$new(root)
    g <- ZarrGroup$new(store)
    a <- g$get_item("1d.contiguous.raw.i2")

    expect_equal(a$get_shape(), c(4))
    expect_equal(a$get_chunks(), c(4))

    expect_equal(is.na(a$get_compressor()), TRUE)

    selection <- a$get_item("...")

    expect_equal(dim(selection$data), c(4))
    expect_equal(selection$data, array(data=c(1, 2, 3, 4), dim=c(4)))
})