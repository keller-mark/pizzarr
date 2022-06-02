#' The Zarr OIndex class.
#' @title OIndex Class
#' @docType class
#' @description
#' @keywords internal
#' Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/indexing.py#L655
#'
#' @rdname OIndex
#' @export
OIndex <- R6::R6Class("OIndex",
  public = list(
    #' @field array
    #' @keywords internal
    array = NULL,
    #' @description
    #' Create a new OIndex instance.
    #' @return An `OIndex` instance.
    initialize = function(array) {
      self$array <- array
    }
  )
)

#' The Zarr VIndex class.
#' @title VIndex Class
#' @docType class
#' @description
#' @keywords internal
#' Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/indexing.py#L811
#'
#' @rdname VIndex
#' @export
VIndex <- R6::R6Class("VIndex",
  public = list(
    #' @field array
    #' @keywords internal
    array = NULL,
    #' @description
    #' Create a new VIndex instance.
    #' @return A `VIndex` instance.
    initialize = function(array) {
      self$array <- array
    }
  )
)
