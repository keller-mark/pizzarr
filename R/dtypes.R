# Internal utility functions for converting between Zarr and R data types.


#' @keywords internal
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

#' @keywords internal
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

#' @keywords internal
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

#' @keywords internal
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

#' @keywords internal
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

#' @keywords internal
get_typed_array_ctr <- function(dtype) {
  rtype <- get_dtype_rtype(dtype)
  return(function(dim) array(data = rtype, dim = dim))
}