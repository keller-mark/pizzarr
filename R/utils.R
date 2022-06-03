#' Convert a JSON-like list to a raw type.
#' 
#' @keywords internal
#' @param json_as_list An R list to be converted to JSON.
#' @return The raw value.
json_to_raw <- function(json_as_list) {
  json_str <- jsonlite::toJSON(json_as_list)
  json_raw <- charToRaw(json_str)
  return(json_raw)
}

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
  if(is.na(compressor)) {
    compressor <- jsonlite::unbox(compressor)
  } else if(!is.na(compressor) && !("id" %in% names(compressor))) {
    stop("compressor must contain an 'id' property when not null.")
  }
  if(is.na(filters)) {
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
      if(!(fill_value %in% c("NaN", "Infinity", "-Infinity"))) {
        stop("fill_value must be NaN, Infinity, or -Infinity when dtype is float")
      }
    }
    if(dtype_basictype == "S" && !is.na(fill_value)) {
      # TODO: validate that fill_value is encoded as an ASCII string using the standard Base64 alphabet.
    }
  } else {
    # TODO: validate structured dtypes
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

#' Decode zarray metadata.
#' 
#' @keywords internal
#' @param meta_bytes The metadata in raw bytes format.
#' @return The metadata after conversion to an R list.
decode_array_meta <- function(meta_bytes) {
  meta_char <- rawToChar(meta_bytes)
  meta_list <- jsonlite::fromJSON(meta_char)
  return(meta_list)
}

#' Encode zarray metadata.
#' 
#' @keywords internal
#' @param meta The metadata as a named list.
#' @return The metadata after conversion to an R list.
encode_array_meta <- function(meta) {
  meta_char <- jsonlite::toJSON(meta)
  meta_bytes <- charToRaw(meta_char)
  return(meta_bytes)
}

#' Write an R matrix to a Zarr store (one chunk, no compression).
#' 
#' @keywords internal
#' @param matrix The matrix as an R matrix.
#' @param rows A vector of row names.
#' @param cols A vector of column names.
#' @param store The Zarr store.
#' @param compressor The compressor config. Optional.
matrix_to_zarr <- function(matrix, rows, cols, store, compressor = NA) {

  num_rows <- nrow(matrix)
  num_cols <- ncol(matrix)

  raw_matrix <- as.raw(matrix)

  compressor_meta <- jsonlite::unbox(NA)
  if(R6::is.R6(compressor)) {
    raw_matrix <- compressor$encode(raw_matrix)
    compressor_meta <- compressor$get_meta()
  }

  zattrs <- list(
    rows = rows,
    cols = cols
  )
  zarray <- create_zarray_meta(
    # TODO: set chunk size to something smaller if multiple chunks
    chunks = c(num_rows, num_cols),
    compressor = compressor_meta,
    dtype = "|u1",
    fill_value = 0,
    order = "C",
    shape = c(num_rows, num_cols)
  )

  store$set_item(ATTRS_KEY, json_to_raw(zattrs))
  store$set_item(ARRAY_META_KEY, json_to_raw(zarray))
  # TODO: chunks
  store$set_item("0.0", raw_matrix)
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

filter_list <- function(l, pred) {
  result <- list()
  for(item in l) {
    if(pred(item)) {
      result <- append(result, item)
    }
  }
  return(result)
}

# Reference: https://github.com/gzuidhof/zarr.js/blob/master/src/core/indexing.ts#L67
replace_ellipsis <- function(selection, shape) {
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

compute_size <- function(shape) {
  result <- 1
  for(val in shape) {
    result <- result * val
  }
  return(result)
}