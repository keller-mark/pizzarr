setup({

})
teardown({

})

zarr_volcano <- function() {
  dir <- file.path(tempdir(TRUE), "volcano.zarr")
  
  unlink(dir, recursive = TRUE, force = TRUE)
  
  z <- DirectoryStore$new(dir)
  
  a <- volcano
  
  za <- zarr_create(dim(volcano), path = "volcano", store = z, overwrite = TRUE)
  
  za$set_item("...", a)
  
  g <- zarr_open_group(z)
  
  g$get_attrs()$set_item("tile", "volcano")
  
  g
}
