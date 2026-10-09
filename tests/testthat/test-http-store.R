if (!requireNamespace("vcr", quietly = TRUE) ||
    !requireNamespace("crul", quietly = TRUE)) {
  return(invisible())
}

vcr::use_cassette("http_base", {

  test_that("http store", {
    skip_if_not_installed("blosc")

    url <- "https://uk1s3.embassy.ebi.ac.uk/idr/zarr/v0.4/idr0062A/6001240.zarr"

    z <- pizzarr::HttpStore$new(url)
    g <- zarr_open_group(z)
    expect_equal(class(g), c("ZarrGroup", "R6"))

    attrs <- g$get_attrs()$to_list()

    expect_equal(names(attrs), c("_creator", "multiscales", "omero"))

    resolution_paths <- attrs$multiscales[[1]]$datasets[[1]]$path
    first_resolution <- resolution_paths[[1]]

    zarr_arr <- g$get_item(first_resolution)

    expect_equal(zarr_arr$get_shape(),
                 c(2L, 236L, 275L, 271L))

    expect_message(listdir_output <- z$listdir(), "Consolidated metadata not found for this http store. Can't listdir")

    expect_null(listdir_output)

    z_index <- 118

    nested_arr <- zarr_arr$get_item(list(slice(1, 2),
                                         slice(z_index, z_index),
                                         slice(1, 10),
                                         slice(1, 10)))

    expect_equal(nested_arr$shape,
                 c(2L, 1L, 10L, 10L))

    arr <- nested_arr$data

    expect_equal(dim(arr), nested_arr$shape)

  })

})

vcr::use_cassette("http_listdir", {

  test_that("http listdir and zmeta", {

    url<- "https://raw.githubusercontent.com/DOI-USGS/rnz/main/inst/extdata/bcsd.zarr"

    z <- pizzarr::HttpStore$new(url)
    expect_equal(class(z), c("HttpStore", "Store", "R6"))

    z$set_cache_time_seconds(1234)

    expect_equal(z$get_cache_time_seconds(), 1234)

    vars <- z$listdir()

    expect_equal(vars,
                 c("latitude", "longitude", "pr", "tas", "time"))

    # listdir() accepts the path argument the base Store class defines.
    expect_equal(z$listdir(NA), vars)

    # An array contains only dot-prefixed metadata keys, which are filtered.
    expect_equal(z$listdir("pr"), character(0))

    g <- pizzarr::zarr_open_group(z)

    # Member counting goes through listdir(path); it used to error and
    # silently report zero for every HTTP-backed group.
    expect_output(print(g), "No\\. members : 5")

    expect_equal(length(names(g$get_attrs()$to_list())), 30)

    expect_equal(g$get_item("latitude")$get_attrs()$get_item("units"), "degrees_north")

    expect_equal(names(g$get_store()$get_consolidated_metadata()$metadata),
                 names(z$get_consolidated_metadata()$metadata))

    expect_equal(dim(g$get_item("pr")$as.array()), c(12, 33, 81))
  })

})

vcr::use_cassette("http_listdir_v3", {

  test_that("http listdir from v3 consolidated metadata", {

    # xarray tutorial store, see tutorial.xarray.dev/intermediate/hierarchical_zarr_store.html
    url <- "https://pub-45a1d62ac8d94c4c89f4dc63681a98ed.r2.dev/precipitation.zarr"

    z <- pizzarr::HttpStore$new(url)

    expect_equal(z$listdir(), c("observed", "reanalysis", "time"))

    expect_equal(z$listdir("observed"), c("lat", "lon", "precipitation"))

    expect_equal(z$listdir("time"), character(0))

    g <- pizzarr::zarr_open_group(z)

    expect_output(print(g), "No\\. members : 3")

    arr <- g$get_item("observed")$get_item("precipitation")

    expect_equal(arr$get_chunks(), c(5L, 160L, 75L))

    expect_equal(arr$get_attrs()$get_item("units"), "mm/hr")

    # get_item() serves consolidated nodes as bytes that parse back unchanged
    key <- "observed/precipitation/zarr.json"
    expect_identical(try_fromJSON(rawToChar(z$get_item(key))),
                     z$get_consolidated_metadata()$metadata[[key]])

    # chunk keys are not in consolidated metadata, so they are checked over http
    expect_true(z$contains_item("time/c/0"))
    expect_false(z$contains_item("time/c/1"))
  })

})

test_that("http broken", {

  url<- "https://nogonnawork.zarr"

  expect_warning(z <- pizzarr::HttpStore$new(url), "Can't proceed, web request failed.")
  expect_equal(class(z), c("HttpStore", "Store", "R6"))

  expect_message(vars <- z$listdir(), "not found for this http store")

  expect_null(vars)

  w <- capture_warnings(g <- pizzarr::zarr_open_group(z))

  expect_true(all(grepl("Can't proceed", w)))
})

vcr::use_cassette("http_github_pattern", {

  test_that("http github pattern", {

    url <- "https://raw.githubusercontent.com/DOI-USGS/rnz/main/inst/extdata/bcsd.zarr"

    z <- zarr_open(url)

    s <- pizzarr::HttpStore$new(url)
    expect_equal(class(s), c("HttpStore", "Store", "R6"))

    g <- zarr_open_group(s, mode = "r", path = NA)

    expect_equal(z$get_attrs()$to_list(), g$get_attrs()$to_list())

    zarr_arr <- g$get_item("pr")

    expect_equal(zarr_arr$get_shape(), c(12L, 33L, 81L))

  })

})
