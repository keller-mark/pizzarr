library(pizzarr)

test_that("Slice$indices() behavior matches Python slice().indices()", {
    # Python slice.indices() only returns (start, stop, step).
    # (Zarr.js returns (start, stop, step, adjusted_length).)
    expect_equal(Slice$new(0, 10, 1)$indices(10)[1:3], c(0, 10, 1))
    expect_equal(Slice$new(0, 10, 1)$indices(11)[1:3], c(0, 10, 1))
    expect_equal(Slice$new(0, 10, 1)$indices(12)[1:3], c(0, 10, 1))
    expect_equal(Slice$new(0, 10, 2)$indices(12)[1:3], c(0, 10, 2))

    expect_equal(Slice$new(1, 10, 2)$indices(5)[1:3], c(1, 5, 2))

    expect_equal(Slice$new(9, 10, 2)$indices(5)[1:3], c(5, 5, 2))
    expect_equal(Slice$new(10, 10, 2)$indices(5)[1:3], c(5, 5, 2))
    expect_equal(Slice$new(10, 10, 2)$indices(10)[1:3], c(10, 10, 2))
    expect_equal(Slice$new(10, 10, 2)$indices(11)[1:3], c(10, 10, 2))

    expect_equal(Slice$new(0, 10, -1)$indices(12)[1:3], c(0, 10, -1))

    expect_equal(Slice$new(-1, 10, -1)$indices(14)[1:3], c(13, 10, -1))

    expect_equal(Slice$new(0, 1, 1)$indices(1)[1:3], c(0, 1, 1))
})

test_that("zb_slice gets converted to Slice", {
    s1 <- zb_slice(0, 10, 1)
    s2 <- Slice$new(0, 10, 1)
    expect_equal(s1$start, s2$start)
    expect_equal(s1$stop, s2$stop)
    expect_equal(s1$step, s2$step)
})

test_that("slice gets converted to zb_slice", {
    s1 <- slice(1, 9, 1)
    s2 <- zb_slice(0, 9, 1)
    expect_equal(s1$start, s2$start)
    expect_equal(s1$stop, s2$stop)
    expect_equal(s1$step, s2$step)
})