# Reference: https://github.com/gzuidhof/zarr.js/blob/292804/src/core/slice.ts#L78

# Shortcut for Slice$new
slice <- function(start, stop = NA, step = NA) {
  return(Slice$new(
    start = start,
    stop = stop,
    step = step
  ))
}

is_slice <- function(s) {
  if(class(s)[[1]] == "Slice") {
    return(TRUE)
  }
  return(FALSE)
}

is_contiguous_slice <- function(s) {
  if(is_slice(s) && (s$step == NA || s$step == 1)) {
    return(TRUE)
  }
  return(FALSE)
}

is_positive_slice <- function(s) {
  if(is_slice(s) && (s$step == NA || s$step >= 1)) {
    return(TRUE)
  }
  return(FALSE)
}

is_basic_selection <- function(selection) {
  selection <- ensure_list(selection)
  # Reference: https://github.com/gzuidhof/zarr.js/blob/master/src/core/indexing.ts#L170
  for(i in seq_along(selection)) {
    s <- selection[i]
    if(!(is.numeric(s) || is_positive_slice(s))) {
      return(FALSE)
    }
  }
  return(TRUE)
}

adjust_indices <- function(start, stop, step, length_param) {
  if(start < 0) {
    start <- start + length_param
    if(start < 0) {
      if(step < 0) {
        start <- -1
      } else {
        start <- 0
      }
    }
  } else if(start >= length_param) {
    if(step < 0) {
      start <- length_param - 1
    } else {
      start <- length_param
    }
  }
  if(stop < 0) {
    stop <- stop + length_param
    if(stop < 0) {
      if(step < 0) {
        stop <- -1
      } else {
        stop <- 0
      }
    }
  } else if(stop >= length_param) {
    if(step < 0) {
      stop <- length_param - 1
    } else {
      stop <- length_param
    }
  }
  if(step < 0) {
    if(stop < start) {
      length_param <- floor((start - stop - 1) / (-step) + 1)
      return(c(start, stop, step, length_param))
    }
  } else {
    if(start < stop) {
      length_param <- floor((stop - start - 1) / step + 1)
      return(c(start, stop, step, length_param))
    }
  }
  return(c(start, stop, step, 0))
}

slice_indices <- function(slice_param, length_param) {
  start <- 0
  stop <- 0
  step <- 0

  MAX_SAFE_INTEGER <- .Machine$integer.max

  if(is.na(slice_param$step)) {
    step <- 1
  } else {
    step <- slice_param$step
  }

  if(is.na(slice_param$start)) {
    if(step < 0) {
      start <- MAX_SAFE_INTEGER
    } else {
      0
    }
  } else {
    start <- slice_param$start
    if(start < 0) {
      start <- start + length_param
    }
  }

  if(is.na(slice_param$stop)) {
    if(step < 0) {
      stop <- -MAX_SAFE_INTEGER
    } else {
      stop <- MAX_SAFE_INTEGER
    }
  } else {
    stop <- slice_param$stop
    if(stop < 0) {
      stop <- stop + length_param
    }
  }

  s <- adjust_indices(start, stop, step, length_param)
  start <- s[1]
  stop <- s[2]
  step <- s[3]
  length_param <- s[4]

  if(step == 0) {
    stop("step size 0 is invalid")
  }
  return(c(start, stop, step, length_param))
}