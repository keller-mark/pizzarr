library(pizzarr)

test_that("dtype regexes work for one-digit number of bytes", {
    dtype_parts <- get_dtype_parts("|u4")
    expect_equal(dtype_parts$basic_type, "u")
    expect_equal(dtype_parts$byte_order, "|")
    expect_equal(dtype_parts$num_bytes, 4)
})

test_that("dtype regexes work for multi-digit number of bytes", {
    dtype_parts <- get_dtype_parts("<f99")
    expect_equal(dtype_parts$basic_type, "f")
    expect_equal(dtype_parts$byte_order, "<")
    expect_equal(dtype_parts$num_bytes, 99)
})

test_that("check_dtype_support throws error for non-matched regex", {
    dtype_parts <- get_dtype_parts("bad")

    expect_equal(is_na(dtype_parts), TRUE)

    f <- function() {
        check_dtype_support(dtype_parts)
    }
    expect_error(f())
})

test_that("get_dtype_rtype works", {
    dtype_rtype <- get_dtype_rtype("<f8")
    expect_equal(dtype_rtype, double())

    dtype_rtype <- get_dtype_rtype("<u4")
    expect_equal(dtype_rtype, integer())

    dtype_rtype <- get_dtype_rtype("|b1")
    expect_equal(dtype_rtype, logical())
})

test_that("get_dtype_endianness works", {
    dtype_endianness <- get_dtype_endianness("<f8")
    expect_equal(dtype_endianness, "little")

    dtype_endianness <- get_dtype_endianness(">f8")
    expect_equal(dtype_endianness, "big")

    dtype_endianness <- get_dtype_endianness("|b1")
    expect_equal(dtype_endianness, "nr")
})

test_that("get_dtype_numbytes works", {
    numbytes <- get_dtype_numbytes("<f8")
    expect_equal(numbytes, 8)

    numbytes <- get_dtype_numbytes("|S12")
    expect_equal(numbytes, 12)
})

test_that("get_dtype_signed works", {
    is_signed <- get_dtype_signed("<f8")
    expect_equal(is_signed, TRUE)

   is_signed <- get_dtype_signed("<u4")
    expect_equal(is_signed, FALSE)
})

test_that("get_dtype_from_array works", {
    zarr_dtype <- get_dtype_from_array(array(data=as.double(1:10), dim=c(2, 5)))
    check_dtype_support(get_dtype_parts(zarr_dtype))
    zarr_dtype <- get_dtype_from_array(array(data=as.integer(1:10), dim=c(2, 5)))
    check_dtype_support(get_dtype_parts(zarr_dtype))

    zarr_dtype <- get_dtype_from_array(array(data=as.logical(1, 0, 1, 0), dim=c(2, 2)))
    check_dtype_support(get_dtype_parts(zarr_dtype))
    expect_equal(TRUE, TRUE)
})

test_that("get_dtype_asrtype works", {
    astype_func <- get_dtype_asrtype("<f8")
    a <- astype_func(as.integer(1:10))
    expect_equal(typeof(a), "double")

    astype_func <- get_dtype_asrtype("<i4")
    a <- astype_func(as.double(1:10))
    expect_equal(typeof(a), "integer")

    astype_func <- get_dtype_asrtype("|S8")
    a <- astype_func(as.double(1:10))
    expect_equal(typeof(a), "character")
})
