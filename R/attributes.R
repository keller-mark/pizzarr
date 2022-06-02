#' The Zarr Attributes class.
#' @title Attributes Class
#' @docType class
#' @description
#' Class providing access to user attributes on an array or group.
#' Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/attrs.py#L7
#'
#' @rdname Attributes
#' @export
Attributes <- R6::R6Class("Attributes",
  public = list(
    #' @description
    #' Create a new Attributes instance.
    #' @param store Attributes store, already initialized.
    #' @return An `Attributes` instance.
    initialize = function(store, key = ".zattrs", read_only = FALSE, cache = TRUE, synchronizer = NA) {
      # TODO
    }
  )
)
