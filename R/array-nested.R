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
    #' @return A `NestedArray` instance.
    initialize = function(data, shape = NA, dtype = NA) {
      if(is_na(shape)) {
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
      } else if(is.array(data)) {
        num_shape_elements <- compute_size(shape)
        # TODO: check that data array has same shape as expected
        self$data <- data
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
      }
    },
    get = function(selection) {
      # TODO
      print("get")
      print(selection)

      return(self) # TODO: should return a new NestedArray for the selection
    },
    set = function(selection, value) {
      # TODO
      # value should be a NestedArray.
      print("set")
      print(selection)
      print(value)
    },
    flatten = function() {
      # TODO
    },
    arange = function(size, dtype) {
      # TODO
    }
  )
)
