
vcr::use_cassette("http_base", {
  
  test_that("http store", {
    
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
     
    expect_message(listdir_output <- z$listdir(), ".zmetadata not found for this http store. Can't listdir")
    
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
  
  test_that("http_listdir", {
    
    url<- "https://usgs.osn.mghpcc.org/mdmf/gdp/hawaii_present.zarr"
    
    z <- pizzarr::HttpStore$new(url)
    expect_equal(class(z), c("HttpStore", "Store", "R6"))
    
    z$set_cache_time_seconds(1234)
    
    expect_equal(z$get_cache_time_seconds(), 1234)
    
    vars <- z$listdir()
    
    expect_equal(vars, 
                 c("CFRACL", "CFRACT", "FGDP", "GLW", "GRDFLX", "GSW", "HFX", 
                   "HGT", "I_RAINNC", "LAI", "LANDMASK", "LH", "LU_INDEX", "LWP", 
                   "PSFC", "Q2", "RAINNC", "SNOW", "SNOWC", "SNOWH", "T2", "TSK", 
                   "Time", "U10", "V10", "XLAT", "XLONG", "crs"))
    
  })
  
})

vcr::use_cassette("http_github_pattern", {
  
  test_that("http_listdir", {
    
    url <- "https://raw.githubusercontent.com/keller-mark/pizzarr/main/inst/extdata/dog.ome.zarr"
    
    z <- zarr_open(url)
    
    s <- pizzarr::HttpStore$new(url)
    expect_equal(class(s), c("HttpStore", "Store", "R6"))
    
    g <- zarr_open_group(s, mode = "r", path = NA)
    
    expect_equal(z, g)
    
    attrs <- g$get_attrs()$to_list()
    
    expect_equal(names(attrs), c("multiscales", "omero"))
    
    resolution_paths <- attrs$multiscales[[1]]$datasets[[1]]$path
    first_resolution <- resolution_paths[[1]]
    
    zarr_arr <- g$get_item(first_resolution)
    
    expect_equal(zarr_arr$get_shape(),
                 c(3L, 500L, 750L))
    
  })
  
})
