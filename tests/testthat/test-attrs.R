library(pizzarr)

test_that("Can get attributes for empty store (without throwing a KeyError)", {
    store <- MemoryStore$new()
    g <- zarr_open_group(store)
    
    expect_equal(g$get_attrs()$to_list(), obj_list())
})
