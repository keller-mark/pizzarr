# Reference: https://github.com/gzuidhof/zarr.js/blob/292804/src/core/slice.ts#L78

#' @keywords internal
Slice <- R6::R6Class("Slice",
  public = list(
    start = NULL,
    stop = NULL,
    step = NULL,
    initialize = function(start, stop = NA, step = NA) {
      if(is.na(stop)) {
        stop <- start
        start <- NA
      }
      if(!is.na(start) && start == ":") {
        start <- NA
      }
      if(!is.na(stop) && stop == ":") {
        stop <- NA
      }
      self$start <- start
      self$stop <- stop
      self$step <- step
    },
    #' @description
    #' This method takes a single integer argument `length` and computes information about the
    #' slice that the slice object would describe if applied to a sequence of `length` items.
    #' It returns a tuple of three integers; respectively these are the start and stop indices
    # and the step or stride length of the slice. Missing or out-of-bounds indices are handled
    # in a manner consistent with regular slices.
    indices = function(length_param) {
      start <- 0
      stop <- 0
      step <- 0

      MAX_SAFE_INTEGER <- .Machine$integer.max

      if(is.na(self$step)) {
        step <- 1
      } else {
        step <- self$step
      }

      if(is.na(self$start)) {
        if(step < 0) {
          start <- MAX_SAFE_INTEGER
        } else {
          0
        }
      } else {
        start <- self$start
        if(start < 0) {
          start <- start + length_param
        }
      }

      if(is.na(self$stop)) {
        if(step < 0) {
          stop <- -MAX_SAFE_INTEGER
        } else {
          stop <- MAX_SAFE_INTEGER
        }
      } else {
        stop <- self$stop
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
  )
)

#' Shortcut for Slice$new() constructor.
#' @param start The start index.
#' @param stop The stop index.
#' @param step The step size.
#' @param zero_based The index of the dimension. By default, FALSE for R-like behavior.
#' @return A Slice instance with the specified parameters.
slice <- function(start, stop = NA, step = NA, zero_based = FALSE) {
  start_offset <- ifelse(zero_based, 0, -1)
  stop_offset <- ifelse(zero_based, 0, 0)
  if(!is_na(start) && is.numeric(start)) {
    start <- start + start_offset
  }
  if(!is_na(stop) && is.numeric(stop)) {
    stop <- stop + stop_offset
  }
  # Assumed to be zero-based
  # and stop-inclusive
  return(Slice$new(
    start = start,
    stop = stop,
    step = step
  ))
}

#' Shortcut for Slice$new() constructor with zero-based indexing and exclusive stop index.
#' @param start The start index.
#' @param stop The stop index.
#' @param step The step size.
#' @keywords internal
zb_slice <- function(start, stop = NA, step = NA) {
  return(slice(start, stop, step, zero_based = TRUE))
}

#' Check if a value is a Slice instance.
#' @param s The value to check.
#' @return TRUE if the value is a Slice instance, FALSE otherwise.
is_slice <- function(s) {
  if(class(s)[[1]] == "Slice") {
    return(TRUE)
  }
  return(FALSE)
}

#' @keywords internal
is_positive_slice <- function(s) {
  if(is_slice(s) && (is_na(s$step) || s$step >= 1)) {
    return(TRUE)
  }
  return(FALSE)
}

#' @keywords internal
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

#' @keywords internal
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

#' @keywords internal
is_total_slice <- function(item, shape) {
  # Reference: https://github.com/gzuidhof/zarr.js/blob/15e3a3f00eb19f0133018fb65f002311ea53bb7c/src/util.ts#L129

  if (is.null(item) || is_na(item)) {
    return(TRUE)
  }
  if (is_scalar(item)) {
    item <- as.numeric(item)
  }

  for (i in seq_len(min(length(item), length(shape)))) {
    it <- item[i]

    if (is.null(it) || is_na(it)) {
      # continue
    } else {
      if (is_slice(it)) {
        s <- it
        is_step_one <- s$step == 1 || is.null(s$step) || is_na(s$step)

        if ((is.null(s$start) || is_na(s$start)) && (is.null(s$stop) || is_na(s$stop)) && is_step_one) {
          # continue
        } else {
          if ((as.numeric(s$stop) - as.numeric(s$start)) == shape[i] && is_step_one) {
            # continue
          } else {
            return(FALSE)
          }
        }
      } else {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

#' @keywords internal
is_contiguous_slice <- function(s) {
  # Reference: https://github.com/gzuidhof/zarr.js/blob/15e3a3f00eb19f0133018fb65f002311ea53bb7c/src/core/indexing.ts#L149
  if(is_slice(s) && (is_na(s$step) || s$step == 1)) {
    return(TRUE)
  }
  return(FALSE)
}

#' @keywords internal
is_contiguous_selection <- function(selection) {
  # Reference: https://github.com/gzuidhof/zarr.js/blob/15e3a3f00eb19f0133018fb65f002311ea53bb7c/src/core/indexing.ts#L157
  selection <- ensure_list(selection)
  for(i in seq_len(length(selection))) {
    s <- selection[[i]]
    if(!(is_integer_vec(s) || is_contiguous_slice(s) || s == "...")) {
      return(FALSE)
    }
  }
  return(TRUE)
}