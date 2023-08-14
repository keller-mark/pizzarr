# Reference: https://github.com/jeroen/jsonlite/blob/6a30ac/R/unbox.R

#' @description
#' Convert a value to a scalar to opt-out of R default vector casting behavior.
#' @param obj The value to convert.
#' @return The value wrapped as a scalar.
as.scalar <- function(obj) {
  return(jsonlite::unbox(obj))
}

#' @description
#' Check if a value is a scalar.
#' @param s The value to check.
#' @return TRUE if the value is a scalar, FALSE otherwise.
is.scalar <- function(s) {
  if(class(s)[[1]] == "scalar") {
    return(TRUE)
  }
  return(FALSE)
}

#' @keywords internal
is_integer <- function(s) {
  if(is.scalar(s) && is.numeric(s)) {
    return(TRUE)
  }
  return(FALSE)
}

#' @keywords internal
is_integer_vec <- function(s) {
  if(!is.scalar(s) && is.numeric(s)) {
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
