# Reference: https://github.com/gzuidhof/zarr.js/blob/292804/src/nestedArray/types.ts#L32
DTYPE_TYPEDARRAY_MAPPING <- list(
  "|b" = function(dim) array(data = logical(), dim = dim),
  "|u1" = function(dim) array(data = integer(), dim = dim),
  "|i1" = function(dim) array(data = integer(), dim = dim),
  "<b" = function(dim) array(data = logical(), dim = dim),
  "<u1" = function(dim) array(data = integer(), dim = dim),
  "<i1" = function(dim) array(data = integer(), dim = dim),
  "<u2" = function(dim) array(data = integer(), dim = dim),
  "<i2" = function(dim) array(data = integer(), dim = dim),
  "<u4" = function(dim) array(data = integer(), dim = dim),
  "<i4" = function(dim) array(data = integer(), dim = dim),
  "<f4" = function(dim) array(data = double(), dim = dim),
  "<f8" = function(dim) array(data = double(), dim = dim),
  ">b" = function(dim) array(data = logical(), dim = dim),
  ">u1" = function(dim) array(data = integer(), dim = dim),
  ">i1" = function(dim) array(data = integer(), dim = dim),
  ">u2" = function(dim) array(data = integer(), dim = dim),
  ">i2" = function(dim) array(data = integer(), dim = dim),
  ">u4" = function(dim) array(data = integer(), dim = dim),
  ">i4" = function(dim) array(data = integer(), dim = dim),
  ">f4" = function(dim) array(data = double(), dim = dim),
  ">f8" = function(dim) array(data = double(), dim = dim)
)

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

get_typed_array_ctr <- function(dtype) {
  if(dtype %in% names(DTYPE_TYPEDARRAY_MAPPING)) {
    return(DTYPE_TYPEDARRAY_MAPPING[[dtype]])
  }
  stop('Dtype not recognized or not supported in pizzarr')
}

# Reference: https://github.com/gzuidhof/zarr.js/blob/292804/src/nestedArray/index.ts#L134
create_nested_array <- function(buf, dtype, shape, offset = 0) {
  # TODO
}

# Reference: https://github.com/gzuidhof/zarr.js/blob/292804/src/nestedArray/index.ts#L9
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
      
      if(!is.na(data)) {
        self$data <- data
      } else {
        typed_array_ctr <- get_typed_array_ctr(dtype)
        if(length(self$shape) == 0) {
          self$data <- typed_array_ctr(c(1))
        } else {
          # Create from ArrayBuffer or Buffer
          num_shape_elements <- compute_size(shape)
          buf_len <- 1 # TODO
          buf <- raw(length = buf_len)
          self$data <- create_nested_array(buf, dtype, shape)
        }
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
