library(pizzarr)

test_that("Can get attributes for empty store (without throwing a KeyError)", {
    store <- MemoryStore$new()
    g <- zarr_open_group(store)
    
    expect_equal(g$get_attrs()$to_list(), obj_list())
})

test_that("Can set attributes", {
    store <- MemoryStore$new()
    g <- zarr_open_group(store)
    attrs <- g$get_attrs()

    attrs$set_item("foo", "bar")
    expect_equal(attrs$to_list(), obj_list(foo = "bar"))

    attrs$set_item("baz", "qux")
    expect_equal(attrs$to_list(), obj_list(foo = "bar", baz = "qux"))
})

test_that("Can update attributes", {
    store <- MemoryStore$new()
    g <- zarr_open_group(store)
    attrs <- g$get_attrs()

    attrs$set_item("foo", "bar")
    expect_equal(attrs$to_list(), obj_list(foo = "bar"))

    attrs$set_item("baz", "qux")
    expect_equal(attrs$to_list(), obj_list(foo = "bar", baz = "qux"))

    attrs$set_item("foo", "baz")
    expect_equal(attrs$to_list(), obj_list(foo = "baz", baz = "qux"))
})

test_that("Can delete attributes", {
    store <- MemoryStore$new()
    g <- zarr_open_group(store)
    attrs <- g$get_attrs()

    attrs$set_item("foo", "bar")
    expect_equal(attrs$to_list(), obj_list(foo = "bar"))

    attrs$set_item("baz", "qux")
    expect_equal(attrs$to_list(), obj_list(foo = "bar", baz = "qux"))

    attrs$del_item("foo")
    expect_equal(attrs$to_list(), obj_list(baz = "qux"))
    
    attrs$del_item("baz")
    expect_equal(attrs$to_list(), obj_list())
})

test_that("Can set attributes with cache FALSE", {
    store <- MemoryStore$new()
    g <- zarr_open_group(store)
    attrs <- g$get_attrs()
    attrs$cache <- FALSE

    attrs$set_item("foo", "bar")
    expect_equal(attrs$to_list(), obj_list(foo = "bar"))

    attrs$set_item("baz", "qux")
    expect_equal(attrs$to_list(), obj_list(foo = "bar", baz = "qux"))
})

test_that("Can update attributes with cache FALSE", {
    store <- MemoryStore$new()
    g <- zarr_open_group(store)
    attrs <- g$get_attrs()
    attrs$cache <- FALSE

    attrs$set_item("foo", "bar")
    expect_equal(attrs$to_list(), obj_list(foo = "bar"))

    attrs$set_item("baz", "qux")
    expect_equal(attrs$to_list(), obj_list(foo = "bar", baz = "qux"))

    attrs$set_item("foo", "baz")
    expect_equal(attrs$to_list(), obj_list(foo = "baz", baz = "qux"))
})

test_that("Can delete attributes with cache FALSE", {
    store <- MemoryStore$new()
    g <- zarr_open_group(store)
    attrs <- g$get_attrs()
    attrs$cache <- FALSE

    attrs$set_item("foo", "bar")
    expect_equal(attrs$to_list(), obj_list(foo = "bar"))

    attrs$set_item("baz", "qux")
    expect_equal(attrs$to_list(), obj_list(foo = "bar", baz = "qux"))

    attrs$del_item("foo")
    expect_equal(attrs$to_list(), obj_list(baz = "qux"))
    
    attrs$del_item("baz")
    expect_equal(attrs$to_list(), obj_list())
})