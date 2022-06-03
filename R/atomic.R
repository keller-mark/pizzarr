# Reference: https://github.com/jeroen/jsonlite/blob/6a30ac/R/unbox.R

as.scalar <- function(obj) {
  return(jsonlite::unbox(obj))
}

is.scalar <- function(s) {
  if(class(s)[[1]] == "scalar") {
    return(TRUE)
  }
  return(FALSE)
}

is_integer <- function(s) {
  if(is.scalar(s) && is.numeric(s)) {
    return(TRUE)
  }
  return(FALSE)
}

is_integer_vec <- function(s) {
  if(!is.scalar(s) && is.numeric(s)) {
    return(TRUE)
  }
  return(FALSE)
}

is_integer_list <- function(s) {
  if(is.list(s) && is.numeric(unlist(s))) {
    return(TRUE)
  }
  return(FALSE)
}

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
