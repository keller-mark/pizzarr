# TODO: int() and zb_int() now being used but do we need 'Int' Class ? 

#' Abstract Int object
#' @title Int Class
#' @docType class
#' @description
#' Class representing an indexing of a ZARR store
#' @noRd
#' @keywords internal
Int <- R6::R6Class("Int",
                     public = list(
                       #' @field start start index
                       index = NULL,
                       #' @description Create a new `Int` object
                       #' @param index integer index
                       initialize = function(index) {
                         self$index <- index
                       },
                       #' @description
                       #' This method takes a single integer argument `length_param` and computes information about the
                       #' slice that the slice object would describe if applied to a sequence of `length_param` items.
                       #' It returns a tuple of three integers; respectively these are the start and stop indices
                       # and the step or stride length of the slice. Missing or out-of-bounds indices are handled
                       # in a manner consistent with regular slices.
                       #' @param length_param integer length parameter for calculation of integer indices
                       indices = function(length_param) {
                         
                         # check length_param
                         index <- self$index[self$index <= length_param]
                         
                         # return
                         return(c(index, length_param))
                       }
                     )
)

#' Convenience function for the internal Int class constructor.
#' @param index The integer index.
#' @param zero_based The index of the dimension. By default, FALSE for R-like behavior.
#' @return A Int instance with the specified parameters.
#' @export
int <- function(index, zero_based = FALSE) {
  index_offset <- ifelse(zero_based, 0, -1)
  if(!is_na(index) && is.numeric(index)) {
    index <- index + index_offset
  }
  # Assumed to be zero-based
  # and stop-inclusive
  # return(Int$new(
  #   index = index
  # ))
  index
}

#' Convenience function for the internal Int class constructor
#' with zero-based indexing
#' @param index integer index
#' @export
zb_int <- function(index) {
  return(int(index, zero_based = TRUE))
}