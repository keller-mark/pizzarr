#' Convert a string into a character vector.
#' 
#' @keywords internal
#' @param s The string.
#' @return A vector where each element is an individual character.
str_to_vec <- function(s) {
  return(stringr::str_split(s, pattern = "")[[1]])
}

#' Create a list of zarray metadata.
#' 
#' @keywords internal
#' @param shape
#' @param chunks
#' @param dtype
#' @param compressor
#' @param fill_value
#' @param order
#' @param filters
#' @param dimension_separator
#' @return A list.
create_zarray_meta <- function(shape = NA, chunks = NA, dtype = NA, compressor = NA, fill_value = NA, order = NA, filters = NA, dimension_separator = NA) {
  # Reference: https://zarr.readthedocs.io/en/stable/spec/v2.html#metadata
  if(is.na(dimension_separator)) {
    dimension_separator <- "."
  } else if(!(dimension_separator %in% c(".", "/"))) {
    stop("dimension_separator must be '.' or '/'.")
  }
  if(is_na(compressor)) {
    compressor <- jsonlite::unbox(compressor)
  } else if(!is_na(compressor) && !("id" %in% names(compressor))) {
    stop("compressor must contain an 'id' property when not null.")
  }
  if(is_na(filters)) {
    filters <- jsonlite::unbox(filters)
  }
  if(!(order %in% c("C", "F"))) {
    stop("order must be 'C' or 'F'.")
  }
  is_simple_dtype <- (length(dtype) == 1)
  if(is_simple_dtype) {
    dtype_vec <- str_to_vec(dtype)
    dtype_byteorder <- dtype_vec[1]
    dtype_basictype <- dtype_vec[2]
    # TODO: validate dtype param's numbytes and time units
    if(!(dtype_byteorder %in% c("<", ">", "|"))) {
      stop("dtype byteorder must be <, >, or |.")
    }
    if(!(dtype_basictype %in% c("b", "i", "u", "f", "c", "m", "M", "S", "U", "V"))) {
      stop("dtype basic type invalid.")
    }
    
    if(dtype_basictype == "f") {
      if(!is.numeric(fill_value) && !(fill_value %in% c("NaN", "Infinity", "-Infinity"))) {
        stop("fill_value must be NaN, Infinity, or -Infinity when dtype is float")
      }
    }
    if(dtype_basictype == "S" && !is.na(fill_value)) {
      # TODO: validate that fill_value is encoded as an ASCII string using the standard Base64 alphabet.
    }
  } else {
    # TODO: validate structured dtypes
  }

  if(is.null(shape)) {
    shape <-jsonlite::unbox(NA)
  }

  
  # TODO: validate shape param
  # TODO: validate chunks param
  # TODO: validate filters param
  zarray_meta <- list(
    zarr_format = jsonlite::unbox(2),
    shape = shape,
    chunks = chunks,
    dtype = jsonlite::unbox(dtype),
    compressor = compressor,
    fill_value = jsonlite::unbox(fill_value),
    order = jsonlite::unbox(order),
    filters = filters,
    dimension_separator = jsonlite::unbox(dimension_separator)
  )
  return(zarray_meta)
}

#' Create an empty named list
#'
#' A helper function to construct an empty list which converts to a JSON object rather than a JSON array.
#'
#' @param ... A variable number of list entries.
#' @return A named list.
#'
#' @keywords internal
#' @export
#' @examples
#' default_window <- obj_list(
#'   min = 0,
#'   max = 255
#' )
obj_list <- function(...) {
  retval <- stats::setNames(list(), character(0))
  param_list <- list(...)
  for(key in names(param_list)) {
    retval[[key]] = param_list[[key]]
  }
  retval
}


zip_numeric <- function(a, b) {
  result <- list()
  for(i in seq_len(length(a))) {
    result <- append(result, list(c(a[i], b[i])))
  }
  return(result)
}

check_selection_length <- function(selection, shape) {
  if(length(selection) > length(shape)) {
    stop('TooManyIndicesError')
  }
}

# Returns both the sliceIndices per dimension and the output shape after slicing.
# Reference: https://github.com/gzuidhof/zarr.js/blob/master/src/core/indexing.ts#L22
selection_to_slice_indices <- function(selection, shape) {
  slice_indices_result <- list()
  out_shape <- c()

  for(i in seq_along(selection)) {
    s <- selection[i]
    if(is.numeric(s)) {
      slice_indices_result <- append(slice_indices_result, s)
    } else {
      x <- slice_indices(s, shape[i])
      dim_length <- x[4]
      out_shape <- c(out_shape, dim_length)
      slice_indices_result <- append(slice_indices_result, x)
    }
  }
  return(list(
    slice_indices_result = slice_indices_result,
    out_shape = out_shape
  ))
}

#' @keywords internal
filter_list <- function(l, pred) {
  result <- list()
  for(item in l) {
    if(pred(item)) {
      result <- append(result, item)
    }
  }
  return(result)
}

#' Convert user selections, potentially containing "...", to a list of slices
#' that can be used internally.
#' @keywords internal
#' @param selection The user-provided selection list.
#' @param shape The shape of the array, to be used to fill in ellipsis values.
#' @returns A list of selections with ellipsis values converted to NA.
replace_ellipsis <- function(selection, shape) {
  # Reference: https://github.com/gzuidhof/zarr.js/blob/master/src/core/indexing.ts#L67

  selection <- ensure_list(selection)
  
  ellipsis_index <- 0
  num_ellipsis <- 0
  for(i in seq_along(selection)) {
    if(selection[i] == "...") {
      ellipsis_index <- i
      num_ellipsis <- num_ellipsis + 1
    }
  }

  if(num_ellipsis > 1) {
    stop("RangeError(an index can only have a single ellipsis ('...'))")
  }

  if(num_ellipsis == 1) {
    # Count how many items to left and right of ellipsis
    num_items_left <- ellipsis_index - 1
    num_items_right <- length(selection) - (num_items_left + 1)
    num_items <- length(selection) - 1 # All non-ellipsis items
    if(num_items >= length(shape)) {
      # Ellipsis does nothing, just remove it
      selection <- filter_list(selection, function(item) {
        if(is.na(item) || item != "...") {
          return(TRUE)
        }
        return(FALSE)
      })
    } else {
      # Replace ellipsis with as many slices are needed for the number of dims
      num_new_items <- length(shape) - num_items
      new_item <- selection[seq_len(num_items_left)]
      for(i in seq_len(num_new_items)) {
        new_item <- append(new_item, NA)
      }
      if(num_items_right > 0) {
        new_item_right <- selection[seq(from = length(selection) - num_items_right + 1, to = length(selection))]
        for(i in seq_along(new_item_right)) {
          new_item <- append(new_item, new_item_right[i])
        }
      }
      selection <- new_item
    }
  }
  # Fill out seleciton if not  completely specified
  if(length(selection) < length(shape)) {
    num_missing <- length(shape) - length(selection)
    for(i in seq_len(num_missing)) {
      selection <- append(selection, NA)
    }
  }

  check_selection_length(selection, shape)
  return(selection)
}

#' @keywords internal
#' @param shape A shape vector
#' @returns The product of shape elements.
compute_size <- function(shape) {
  result <- 1
  for(val in shape) {
    result <- result * val
  }
  return(result)
}

#' Check if a value, potentially a vector, is NA
#'
#' @keywords internal
#' @param val The value to check
#' @return Whether the value is NA
is_na <- function(val) {
  if(length(val) > 1) {
    return(FALSE)
  } else {
    return(is.na(val))
  }
}

#' Fill in an R array with a single scalar value.
#' @keywords internal
#' @param chunk The R array to fill.
#' @param value The scalar value (after is_scalar() check).
chunk_fill <- function(chunk, value) {
  # Chunk is an R array()
  # Value is a scalar (after is_scalar() check)

  # Need to do equivalent of chunk.fill(value) in JS
  chunk[] <- value
}

#' Check if an error is a KeyError.
#' @param e The error to check.
#' @return TRUE if the error is a KeyError, FALSE otherwise.
is_key_error <- function(e) {
  return(grepl("KeyError", e$message))
}

#' @keywords internal
get_list_product_aux <- function(dim_indexer_iterables, i, partial_results) {
  dim_results <- dim_indexer_iterables[[i]]
  result <- list()
  for(d in dim_results) {
    if(length(partial_results) == 0) {
      result <- append(result, list(d))
    } else {
      for(p in partial_results) {
        result <- append(result, list(append(p, list(d))))
      }
    }
  }
  return(result)
}

#' Generate a product of lists.
#' @param dim_indexer_iterables A list of lists.
#' @keywords internal
#' @return A list of lists.
get_list_product <- function(dim_indexer_iterables) {
  # Reference: https://docs.python.org/3/library/itertools.html#itertools.product
  partial_results <- list()
  for(i in seq_len(length(dim_indexer_iterables))) {
    partial_results <- get_list_product_aux(dim_indexer_iterables, i, partial_results)
  }
  return(partial_results)
}

#' Convert the R data type notation to Zarr one
#'
#' @param x data type to check
#' @param isArray Default = T. When true, an array is expected, otherwise a 
#' string describing the data type
#'
#' @return Zarr data type notation as string
get_np_dataFormat = function(x, isArray = T){
  #https://zarr.readthedocs.io/en/stable/spec/v2.html#data-type-encoding
  
  #For now only integer and double are supported
  r2pType = c("integer" = "<i4", "double" = "<f8")
  # r2pType = c("integer" = "<i4", "double" = "<f8", "character" = "<U", 
  #             "complex" = "<c8", "logical" = "|b1")
  
  msg = "The data type of the array is not valid or not implemented yet"
  if(isArray){
    format = r2pType[typeof(x)]
    if(is.na(format)) stop(msg)
    format = ifelse(format != "<U", format,
                    paste0("<U",max(nchar(x, type = "bytes"))))
    
  } else {
    format = r2pType[x]
    if(is.na(format)) stop(msg)
  }
  
  return(unname(format))
}
