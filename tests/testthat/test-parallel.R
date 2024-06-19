library(pizzarr)

SlowGettingDirectoryStore <- R6::R6Class("SlowGettingDirectoryStore",
  inherit = DirectoryStore,
  public = list(
    get_item = function(key) {
      # Simulate a slow read such as an HTTP request.
      Sys.sleep(1.0/50.0)
      return(super$get_item(key))
    }
  )
)

SlowSettingDirectoryStore <- R6::R6Class("SlowSettingDirectoryStore",
  inherit = DirectoryStore,
  public = list(
    set_item = function(key, value) {
      # Simulate a slow write such as an HTTP request.
      Sys.sleep(1.0/50.0)
      return(super$set_item(key, value))
    }
  )
)

get_dog_arr <- function(slow_setting = FALSE) {
  # The path to the root of the OME-NGFF Zarr store.
  root <- pizzarr_sample("dog.ome.zarr")

  # Open the OME-NGFF as a DirectoryStore.
  if(slow_setting) {
    store <- SlowSettingDirectoryStore$new(root)
  } else {
    store <- SlowGettingDirectoryStore$new(root)
  }

  g <- zarr_open_group(store)

  # Using the OME metadata, get the path to the first resolution of the image pyramid.
  attrs <- g$get_attrs()$to_list()
  resolution_paths <- attrs$multiscales[[1]]$datasets[[1]]$path
  first_resolution <- resolution_paths[[1]]

  # Load the 3-dimensional array of RGB pixels (as a ZarrArray instance).
  zarr_arr <- g$get_item(first_resolution)

  return(zarr_arr)
}

run_parallel_get <- function(num_workers) {
  options(pizzarr.parallel_read_enabled = TRUE)
  doParallel::registerDoParallel(num_workers)
  
  zarr_arr <- get_dog_arr()
  arr <- zarr_arr$get_item("...")$data

  doParallel::stopImplicitCluster()
  options(pizzarr.parallel_read_enabled = FALSE)

  return(sum(arr))
}


run_parallel_set <- function(num_workers) {
  options(pizzarr.parallel_write_enabled = TRUE)
  doParallel::registerDoParallel(num_workers)

  zarr_arr <- get_dog_arr(slow_setting = TRUE)
  arr <- zarr_arr$get_item("...")$data

  # Set the contents of the array to be twice the original value.
  zarr_arr$set_item("...", arr * 2.0)

  doubled_arr <- zarr_arr$get_item("...")$data
  
  doParallel::stopImplicitCluster()
  options(pizzarr.parallel_write_enabled = FALSE)

  return(sum(doubled_arr))
}

test_that("can run get_item() in parallel", {

  bench_df <- bench::mark(
    run_parallel_get(1),
    run_parallel_get(2),
    run_parallel_get(4),
    iterations = 10,
    memory = FALSE,
    filter_gc = FALSE
  )

  expect_equal(unlist(bench_df$result), rep(134538481, 3))
  expect_equal(bench_df$total_time[[1]] > bench_df$total_time[[2]], TRUE)
  expect_equal(bench_df$total_time[[2]] > bench_df$total_time[[3]], TRUE)
  
})

test_that("can run set_item() in parallel", {

  bench_df <- bench::mark(
    run_parallel_set(1),
    run_parallel_set(2),
    run_parallel_set(4),
    iterations = 10,
    memory = FALSE,
    filter_gc = FALSE
  )

  expect_equal(unlist(bench_df$result), rep(134538481*2.0, 3))
  expect_equal(bench_df$total_time[[1]] > bench_df$total_time[[2]], TRUE)
  expect_equal(bench_df$total_time[[2]] > bench_df$total_time[[3]], TRUE)
  
})