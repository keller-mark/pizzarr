#' The Zarr Group class.
#' @title Group Class
#' @docType class
#' @description
#' Instantiate a group from an initialized store.
#' Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/hierarchy.py#L39
#'
#' @rdname Group
#' @export
Group <- R6::R6Class("Group",
  public = list(
    #' @description
    #' Create a new Group instance.
    #' @param store Group store, already initialized.
    #' @return A `Group` instance.
    initialize = function(store, path = NA, read_only = FALSE, chunk_store = NA, cache_attrs = TRUE, synchronizer = NA) {
      # TODO
    }
  )
)
