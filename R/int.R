# Reference: https://github.com/gzuidhof/zarr.js/blob/292804/src/core/slice.ts#L78

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
                       #' indices method
                       indices = function() {
                         return(self$index)
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
  return(Int$new(
    index = index
  ))
}

#' Convenience function for the internal Int class constructor 
#' with zero-based indexing 
#' @param index integer index
#' @export
zb_int <- function(index) {
  return(int(index, zero_based = TRUE))
}

#' Check if a value is a Slice instance.
#' @param s The value to check.
#' @return TRUE if the value is a Slice instance, FALSE otherwise.
#' @export
is_slice <- function(s) {
  if(class(s)[[1]] == "Slice") {
    return(TRUE)
  }
  return(FALSE)
}