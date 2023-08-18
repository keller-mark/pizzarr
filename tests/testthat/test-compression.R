library(pizzarr)

test_that("can convert a NestedArray to a raw vector with lz4 compression", {
    a <- array(data=1:10, dim=c(2, 5))
    na <- NestedArray$new(data = a, shape = dim(a), dtype = "<u4")

    # Get un-compressed raw vector.
    na_raw <- na$flatten_to_raw()

    expect_equal(length(na_raw), 40)
    expected_out <- as.raw(c(
        0x01, 0x00, 0x00, 0x00,
        0x02, 0x00, 0x00, 0x00,
        0x03, 0x00, 0x00, 0x00,
        0x04, 0x00, 0x00, 0x00,
        0x05, 0x00, 0x00, 0x00,
        0x06, 0x00, 0x00, 0x00,
        0x07, 0x00, 0x00, 0x00,
        0x08, 0x00, 0x00, 0x00,
        0x09, 0x00, 0x00, 0x00,
        0x0a, 0x00, 0x00, 0x00
    ))
    expect_equal(na_raw, expected_out)

    # Compress the raw vector.
    compressor <- LZ4$new(acceleration = 1)

    compressed_raw <- compressor$encode(na_raw)

    # Un-compress the compressed vector.
    uncompressed_raw <- compressor$decode(compressed_raw)

    expect_equal(uncompressed_raw, expected_out)
})

test_that("can convert a NestedArray to a raw vector with zstd compression", {
    a <- array(data=1:10, dim=c(2, 5))
    na <- NestedArray$new(data = a, shape = dim(a), dtype = "<u4")

    # Get un-compressed raw vector.
    na_raw <- na$flatten_to_raw()

    expect_equal(length(na_raw), 40)
    expected_out <- as.raw(c(
        0x01, 0x00, 0x00, 0x00,
        0x02, 0x00, 0x00, 0x00,
        0x03, 0x00, 0x00, 0x00,
        0x04, 0x00, 0x00, 0x00,
        0x05, 0x00, 0x00, 0x00,
        0x06, 0x00, 0x00, 0x00,
        0x07, 0x00, 0x00, 0x00,
        0x08, 0x00, 0x00, 0x00,
        0x09, 0x00, 0x00, 0x00,
        0x0a, 0x00, 0x00, 0x00
    ))
    expect_equal(na_raw, expected_out)

    # Compress the raw vector.
    compressor <- Zstd$new(level = 1)

    compressed_raw <- compressor$encode(na_raw)

    # Un-compress the compressed vector.
    uncompressed_raw <- compressor$decode(compressed_raw)

    expect_equal(uncompressed_raw, expected_out)
})
