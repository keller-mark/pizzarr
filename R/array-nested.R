

#' @keywords internal
create_array_from_raw <- function(buf, dtype, shape, offset = 0) {
  # TODO
  # Reference: https://github.com/gzuidhof/zarr.js/blob/292804/src/nestedArray/index.ts#L134
  stop("TODO: implement create_array_from_raw()")
}

#' @keywords internal
zero_based_to_one_based <- function(selection, shape) {
  selection_list <- list()
  for(i in seq_len(length(selection))) {
    sel <- selection[[i]]
    # We assume the selection uses zero-based indexing,
    # and internally convert to R-based / 1-based indexing
    # before accessing data on the internal self$data.
    sel_start <- sel$start + 1
    sel_stop <- sel$stop + 1 # Do not add one, since R indexing is inclusive.
    # TODO: convert these warnings to errors once we know internals do indexing correctly
    if(sel_start < 1) {
      sel_start <- 1
      message("IndexError: NestedArray$get() received slice with start index out of bounds - too low")
    }
    if(sel_start > shape[i]) {
      sel_start <- shape[i]
      message("IndexError: NestedArray$get() received slice with start index out of bounds - too high")
    }
    if(sel_stop < 1) {
      sel_stop <- 1
      message("IndexError: NestedArray$get() received slice with stop index out of bounds - too low")
    }
    if(sel_stop > shape[i]) {
      sel_stop <- shape[i]
      message("IndexError: NestedArray$get() received slice with stop index out of bounds - too high")
    }
    selection_list <- append(selection_list, list(c(sel_start:sel_stop))) # TODO: support non-1 step
  }
  return(selection_list)
}

# Reference: https://github.com/gzuidhof/zarr.js/blob/292804/src/nestedArray/index.ts#L9

#' The Zarr NestedArray class.
#' @title NestedArray Class
#' @docType class
#' @description
#' Represents a multi-dimensional array that can be
#' accessed and subsetted via list of Slice instances.
#' @rdname NestedArray
#' @export
NestedArray <- R6::R6Class("NestedArray",
  public = list(
    #' @field shape The shape of the array.
    shape = NULL,
    #' @field dtype The Zarr dtype of the array, as a string like ">f8".
    dtype = NULL,
    #' @field data The array contents as a base R array.
    data = NULL,
    #' @description
    #' Create a new NestedArray instance.
    #' @param data The data to initialize the array with.
    #' Either NULL, base R array, base R vector (numeric/logical),
    #' scalar, or raw vector.
    #' @param shape The shape of the array.
    #' @param dtype The Zarr dtype of the array, as a string like ">f8".
    #' @return A `NestedArray` instance.
    initialize = function(data, shape = NA, dtype = NA) {
      if(is.null(shape) || is_na(shape)) {
        shape <- dim(data)
      } else {
        shape <- normalize_shape(shape)
      }
      if(is_na(dtype)) {
        dtype <- get_dtype_from_array(data)
      } else {
        dtype <- normalize_dtype(dtype)
      }
      self$shape <- shape
      self$dtype <- dtype
      if(is.null(data)) {
        self$data <- array(data=get_dtype_rtype(dtype), dim=shape)
      } else if(is.null(self$shape)) {
        self$data <- data # TODO?
      } else if(is.array(data) || is.numeric(data) || is.logical(data)) {
        num_shape_elements <- compute_size(shape)
        # TODO: check that data array has same shape as expected
        if(!is.null(dim(data)) && all(ensure_vec(dim(data)) == ensure_vec(shape))) {
          self$data <- data
        } else {
          astype_func <- get_dtype_asrtype(dtype)
          self$data <- array(data=as.array(astype_func(data)), dim=shape)
        }
        
      } else if(is.raw(data)) {
        buf <- data
        # Create from ArrayBuffer or Buffer
        num_shape_elements <- compute_size(shape)
        num_data_elements <- length(buf) / get_dtype_numbytes(dtype)
        if (num_shape_elements != num_data_elements) {
          stop('Buffer has ${numDataElements} of dtype ${dtype}, shape is too large or small')
        }
        self$data <- create_array_from_raw(buf, dtype, shape)
      } else {
        buf_len <- compute_size(shape) * get_dtype_numbytes(dtype) 
        buf <- raw(length = buf_len)
        # TODO?
        stop("Unexpected type for data in NestedArray$initialize()")
      }
    },
    #' @description
    #' Subset the array.
    #' @param selection A list of slices.
    #' @return A new NestedArray (potentially a subset) representing the selection.
    get = function(selection) {
      selection_list <- zero_based_to_one_based(selection, self$shape)
      
      # Using do.call here seems to work the same as `abind::asub(self$data, selection_list)`
      # so we can use do.call to avoid the extra dependency.
      subset_arr <- do.call("[", append(list(self$data), selection_list))
      subset_nested_array <- NestedArray$new(subset_arr, shape = dim(subset_arr), dtype = self$dtype)
      return(subset_nested_array)
    },
    #' @description
    #' Set a subset of the array.
    #' @param selection A list of slices.
    #' @param value A NestedArray or a base R array.
    set = function(selection, value) {
      # value should be a NestedArray.
      selection_list <- zero_based_to_one_based(selection, self$shape)

      value_data <- value$data

      if("NestedArray" %in% class(value)) {
        value_data <- value$data
      } else if(is_scalar(value)) {
        value_data <- value
      } else {
        message(value)
        stop("Got unexpected type for value in NestedArray$set()")
      }

      # Cannot figure out how to dynamically set values in an array
      # of arbitrary dimensions.
      # Tried: abind::afill <- but it doesn't seem to work with arbitrary dims or do.call
      if(length(selection_list) == 1) {
        self$data[selection_list[[1]]] <- value_data
      } else if(length(selection_list) == 2) {
        self$data[selection_list[[1]], selection_list[[2]]] <- value_data
      } else if(length(selection_list) == 3) {
        self$data[selection_list[[1]], selection_list[[2]], selection_list[[3]]] <- value_data
      } else if(length(selection_list) == 4) {
        self$data[selection_list[[1]], selection_list[[2]], selection_list[[3]], selection_list[[4]]] <- value_data
      } else if(length(selection_list) == 5) {
        self$data[selection_list[[1]], selection_list[[2]], selection_list[[3]], selection_list[[4]], selection_list[[5]]] <- value_data
      } else if(length(selection_list) == 6) {
        self$data[selection_list[[1]], selection_list[[2]], selection_list[[3]], selection_list[[4]], selection_list[[5]], selection_list[[6]]] <- value_data
      } else {
        stop("NestedArray$set() can only handle up to 6D arrays at the moment. Please make a feature request if you need to handle more dims.")
      }
    },
    #' @description
    #' Flatten the array contents.
    #' @returns The data as a flat vector.
    flatten = function() {
      # TODO: pass ordering C/F as argument.
      # TODO: transpose first (if needed, based on the ordering).
      return(as.vector(self$data))
    }
  )
)
