# Reference: https://github.com/jeroen/jsonlite/blob/6a30ac/R/unbox.R

#' Convert a value to a scalar to opt-out of R default vector casting behavior.
#' This uses the `jsonlite::unbox` function to "tag" the value as a scalar.
#' @param obj The value to convert.
#' @return The value wrapped as a scalar.
#' @export
as_scalar <- function(obj) {
  return(jsonlite::unbox(obj))
}

#' Check if a value is a scalar (i.e., a one-element vector that was converted with as_scalar).
#' @param s The value to check.
#' @return TRUE if the value is a scalar, FALSE otherwise.
#' @export
is_scalar <- function(s) {
  if(class(s)[[1]] == "scalar") {
    return(TRUE)
  }
  return(FALSE)
}

#' Check if a value is an integer R vector or scalar.
#' @keywords internal
is_integer <- function(s) {
  if(is.atomic(s) && is.numeric(s) && all(s %% 1 == 0) && length(s) == 1) {
    return(TRUE)
  }
  return(FALSE)
}

#' Check if a value is both a scalar (tagged by as_scalar) and an integer.
#' @keywords internal
is_integer_scalar <- function(s) {
  if(is_scalar(s) && is_integer(s)) {
    return(TRUE)
  }
  return(FALSE)
}

#' Check that a value is a vector of one or more integers and has not been
#' explicitly tagged as a scalar.
#' @keywords internal
is_integer_vec <- function(s) {
  if(!is_scalar(s) && is.vector(s) && !is.list(s) && all(sapply(s,is_integer))) {
    if(length(s) > 1){
      return(TRUE)
    }
  }
  return(FALSE)
}

#' Check that a value is a list of one or more integers.
#' @keywords internal
is_integer_list <- function(s) {
  if(is.list(s) && is_integer_vec(unlist(s))) {
    return(TRUE)
  }
  return(FALSE)
}

#' Ensure that scalars and lists of integers are converted to
#' an R vector of integers.
#' @keywords internal
ensure_integer_vec <- function(selection) {
  if(is_integer_scalar(selection)) {
    return(as.numeric(selection))
  } else if(is_integer_vec(selection)) {
    return(selection)
  } else if(is.list(selection)) {
    return(as.numeric(unlist(selection)))
  }
  return(as.numeric(selection))
}

#' Ensure that scalars, single slices, and R integer vectors are converted
#' to a list containing either R integer vectors or slice instances as values.
#' @keywords internal
ensure_list <- function(selection) {
  if(is_slice(selection)) {
    return(list(selection))
  } else if(is_integer_scalar(selection)) {
    return(as.list(as.numeric(selection)))
  } else if(is.list(selection)) {
    return(selection)
  }
  return(as.list(selection))
}
