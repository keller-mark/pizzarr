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
create_nested_array <- function(buf, dtype, shape, offset = 0) {
  # TODO
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
    initialize = function(data, shape, dtype) {
      shape <- normalize_shape(shape)
      self$shape <- shape
      self$dtype <- dtype
      
     if(length(self$shape) == 0) {
        self$data <- typed_array_ctr(c(1))
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
        self$data <- create_nested_array(buf, dtype, shape)
      } else {
        buf_len <- compute_size(shape) * get_dtype_numbytes(dtype) 
        buf <- raw(length = buf_len)
        # TODO
      }
    },
    get = function(selection) {
      # TODO
    },
    set = function(selection, value) {
      # TODO
    },
    flatten = function() {
      # TODO
    },
    arange = function(size, dtype) {
      # TODO
    }
  )
)
