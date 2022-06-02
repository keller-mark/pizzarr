#' The Zarr Attributes class.
#' @title Store Class
#' @docType class
#' @description
#' Class providing access to user attributes on an array or group.
#'
#' @rdname Store
#' @export
Attributes <- R6::R6Class("Attributes",
  public = list(
    #' @description
    #' Create a new LZ4 compressor.
    #' @param store Array store, already initialized.
    #' @return An `Array` instance.
    initialize = function(store, key = ".zattrs", read_only = FALSE, cache = TRUE, synchronizer = NA) {
      # TODO
    }
  )
)
