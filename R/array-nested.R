get_dtype_rtype <- function(dtype) {
    # Reference: https://github.com/gzuidhof/zarr.js/blob/292804/src/nestedArray/types.ts#L32
  DTYPE_RTYPE_MAPPING <- list(
    "|b" = logical(),
    "|u1" = integer(),
    "|i1" = integer(),
    "<b" = logical(),
    "<u1" = integer(),
    "<i1" = integer(),
    "<u2" = integer(),
    "<i2" = integer(),
    "<u4" = integer(),
    "<i4" = integer(),
    "<f4" = double(),
    "<f8" = double(),
    ">b" = logical(),
    ">u1" = integer(),
    ">i1" = integer(),
    ">u2" = integer(),
    ">i2" = integer(),
    ">u4" = integer(),
    ">i4" = integer(),
    ">f4" = double(),
    ">f8" = double()
  )
  return(DTYPE_RTYPE_MAPPING[[dtype]])
  # TODO: handle errors
  #stop('Dtype not recognized or not supported in pizzarr')
}


get_dtype_endianness <- function(dtype) {
  # TODO: use regexes to figure this stuff out
  DTYPE_ENDIANNESS_MAPPING <- list(
    "|b" = "nr",
    "|u1" = "nr",
    "|i1" = "nr",
    "<b" = "little",
    "<u1" = "little",
    "<i1" = "little",
    "<u2" = "little",
    "<i2" = "little",
    "<u4" = "little",
    "<i4" = "little",
    "<f4" = "little",
    "<f8" = "little",
    ">b" = "big",
    ">u1" = "big",
    ">i1" = "big",
    ">u2" = "big",
    ">i2" = "big",
    ">u4" = "big",
    ">i4" = "big",
    ">f4" = "big",
    ">f8" = "big"
  )
  return(DTYPE_ENDIANNESS_MAPPING[[dtype]])
}

get_dtype_numbytes <- function(dtype) {
  # TODO: use regexes to figure this stuff out
  DTYPE_NUMBYTES_MAPPING <- list(
    "|b" = 1,
    "|u1" = 1,
    "|i1" = 1,
    "<b" = 1,
    "<u1" = 1,
    "<i1" = 1,
    "<u2" = 2,
    "<i2" = 2,
    "<u4" = 4,
    "<i4" = 4,
    "<f4" = 4,
    "<f8" = 8,
    ">b" = 1,
    ">u1" = 1,
    ">i1" = 1,
    ">u2" = 2,
    ">i2" = 2,
    ">u4" = 4,
    ">i4" = 4,
    ">f4" = 4,
    ">f8" = 8
  )
  return(DTYPE_NUMBYTES_MAPPING[[dtype]])
}

get_typed_array_ctr <- function(dtype) {
  rtype <- get_dtype_rtype(dtype)
  return(function(dim) array(data = rtype, dim = dim))
}

# Reference: https://github.com/gzuidhof/zarr.js/blob/292804/src/nestedArray/index.ts#L134
create_array_from_raw <- function(buf, dtype, shape, offset = 0) {
  # TODO
}

get_dtype_from_array <- function(a) {
  TYPEOF_RTYPE_MAPPING <- list(
    "logical" = logical(),
    "integer" = integer(),
    "double" = double()
  )
  RTYPE_DTYPE_MAPPING <- list(
    "logical" = "|b",
    "integer" = "<i4",
    "double" = "<f8"
  )
  rtype_str <- typeof(a)
  return(RTYPE_DTYPE_MAPPING[[rtype_str]])
}

get_dtype_asrtype <- function(dtype) {
    # Reference: https://github.com/gzuidhof/zarr.js/blob/292804/src/nestedArray/types.ts#L32
  DTYPE_RTYPE_MAPPING <- list(
    "|b" = as.logical,
    "|u1" = as.integer,
    "|i1" = as.integer,
    "<b" = as.logical,
    "<u1" = as.integer,
    "<i1" = as.integer,
    "<u2" = as.integer,
    "<i2" = as.integer,
    "<u4" = as.integer,
    "<i4" = as.integer,
    "<f4" = as.double,
    "<f8" = as.double,
    ">b" = as.logical,
    ">u1" = as.integer,
    ">i1" = as.integer,
    ">u2" = as.integer,
    ">i2" = as.integer,
    ">u4" = as.integer,
    ">i4" = as.integer,
    ">f4" = as.double,
    ">f8" = as.double
  )
  return(DTYPE_RTYPE_MAPPING[[dtype]])
  # TODO: handle errors
  #stop('Dtype not recognized or not supported in pizzarr')
}

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

#' The Zarr NestedArray class.
#' @title NestedArray Class
#' @docType class
#' @description
#' TODO
#' Reference: https://github.com/gzuidhof/zarr.js/blob/292804/src/nestedArray/index.ts#L9
#' @rdname NestedArray
#' @export
NestedArray <- R6::R6Class("NestedArray",
  public = list(
    shape = NULL,
    dtype = NULL,
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

      subset_arr <- abind::asub(self$data, selection_list)
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
      } else if(is.scalar(value)) {
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
      return(as.vector(self$data))
    }
  )
)
