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

test_that("zero_based_to_one_based", {
  
  s <- list(slice(1, 11, 1))

  expect_equal(zero_based_to_one_based(s, shape = c(20, 20)), 
               list(c(1:11)))
  
  s <- list(slice(1, 11, 2))
  
  expect_equal(zero_based_to_one_based(s, shape = c(20, 20)), 
               list(seq(1, 11, 2)))
})

test_that("set array values", {

  d <- zarr_volcano()

  a <- d$get_item("volcano")

  vals <- a[1:10, 1:20]$as.array()

  new_vals <- vals * 10

  sub <- a[1:10, 1:20]

  sub$set(list(slice(1,10), slice(1,20)), new_vals)

  expect_equal(sub$as.array(),
               new_vals)

  # TODO: remove this expect-error once implemented.
  expect_error(a[1:10, 1:20]$set("...", new_vals),
                "Unsupported selection type")

  a[1:10, 1:20]$set(list(slice(1,10), slice(1,20)), new_vals)

  # it does not update the original array -- should it?
  expect_equal(a[1:10, 1:20]$as.array(),
               vals)

  a$set_item(list(slice(1,10), slice(1,20)), new_vals)

  expect_equal(a[1:10, 1:20]$as.array(),
               new_vals)
})