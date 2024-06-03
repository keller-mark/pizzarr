# Reference: https://github.com/jeroen/jsonlite/blob/6a30ac/R/unbox.R

#' Convert a value to a scalar to opt-out of R default vector casting behavior.
#' @param obj The value to convert.
#' @return The value wrapped as a scalar.
#' @export
as_scalar <- function(obj) {
  return(jsonlite::unbox(obj))
}

#' Check if a value is a scalar.
#' @param s The value to check.
#' @return TRUE if the value is a scalar, FALSE otherwise.
#' @export
is_scalar <- function(s) {
  if(is.atomic(s) && length(s) == 1L && !is.character(s) && Im(s)==0) {
    return(TRUE)
  }
  return(FALSE)
}

#' @keywords internal
is_integer <- function(s) {
  if(is_scalar(s) && is.numeric(s)) {
    return(TRUE)
  }
  return(FALSE)
}

#' @keywords internal
is_integer_vec <- function(s) {
  if(!is_scalar(s) && is.numeric(s)) {
    return(TRUE)
  }
  return(FALSE)
}

#' @keywords internal
is_integer_list <- function(s) {
  if(is.list(s) && is.numeric(unlist(s))) {
    return(TRUE)
  }
  return(FALSE)
}

#' @keywords internal
ensure_vec <- function(selection) {
  if(is_integer(selection)) {
    return(as.numeric(selection))
  } else if(is.numeric(selection)) {
    return(selection)
  } else if(is.list(selection)) {
    return(as.numeric(unlist(selection)))
  }
  return(as.numeric(selection))
}

#' @keywords internal
ensure_list <- function(selection) {
  if(is_slice(selection)) {
    return(list(selection))
  } else if(is_integer(selection)) {
    return(as.list(as.numeric(selection)))
  } else if(is.list(selection)) {
    return(selection)
  }
  return(as.list(selection))
}
