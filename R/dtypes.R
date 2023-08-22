# Internal utility functions for converting between Zarr and R data types.

is_structured_dtype <- function(dtype) {
  if(is.character(dtype) && length(dtype) == 1) {
    return(FALSE)
  }
  return(TRUE)
}

get_dtype_parts <- function(dtype) {
  # Check for string dtype
  # "S": string (fixed-length sequence of char)
  dtype_regex <- "^(\\||>|<)(b|i|u|f|c|m|M|S|U|V)(\\d+)"
  if(stringr::str_detect(dtype, dtype_regex)) {
    dtype_matches <- stringr::str_match(dtype, dtype_regex)
    basic_type <- dtype_matches[1,3]
    if(basic_type == "U") {
      byte_multiplier <- 4
    } else {
      byte_multiplier <- 1
    }
    num_items <- as.integer(dtype_matches[1,4])
    result <- list(
      dtype_str = dtype,
      byte_order = dtype_matches[1,2],
      basic_type = dtype_matches[1,3],
      num_bytes = num_items * byte_multiplier,
      num_items = num_items
    )
    return(result)
  } else {
    return(NA)
  }
}

check_dtype_support <- function(dtype_parts) {
  if(!is_na(dtype_parts) && dtype_parts$basic_type %in% c("b", "i", "u", "f", "S", "U")) {
    return(TRUE)
  }
  stop(paste("Unsupported dtype:", dtype_parts))
  return(FALSE)
}



#' @keywords internal
get_dtype_rtype <- function(dtype) {
  dtype_parts <- get_dtype_parts(dtype)
  check_dtype_support(dtype_parts)

  # Reference: https://github.com/gzuidhof/zarr.js/blob/292804/src/nestedArray/types.ts#L32
  BASICTYPE_RTYPE_MAPPING <- list(
    "b" = logical(),
    "u" = integer(),
    "i" = integer(),
    "f" = double(),
    "S" = character(),
    "U" = character()
  )

  return(BASICTYPE_RTYPE_MAPPING[[dtype_parts$basic_type]])
}

#' @keywords internal
get_dtype_endianness <- function(dtype) {
  dtype_parts <- get_dtype_parts(dtype)
  check_dtype_support(dtype_parts)

  DTYPE_ENDIANNESS_MAPPING <- list(
    "|" = "nr",
    "<" = "little",
    ">" = "big"
  )
  return(DTYPE_ENDIANNESS_MAPPING[[dtype_parts$byte_order]])
}

#' @keywords internal
get_dtype_numbytes <- function(dtype) {
  dtype_parts <- get_dtype_parts(dtype)
  check_dtype_support(dtype_parts)
  return(dtype_parts$num_bytes)
}

#' @keywords internal
get_dtype_signed <- function(dtype) {
  dtype_parts <- get_dtype_parts(dtype)
  check_dtype_support(dtype_parts)

  DTYPE_SIGNED_MAPPING <- list(
    "b" = FALSE,
    "u" = FALSE,
    "i" = TRUE,
    "f" = TRUE,
    "S" = FALSE, # TODO: is this correct?
    "U" = FALSE  # TODO: is this correct?
  )
  return(DTYPE_SIGNED_MAPPING[[dtype_parts$basic_type]])
}

#' @keywords internal
get_dtype_from_array <- function(a) {
  RTYPE_DTYPE_MAPPING <- list(
    "logical" = "|b1",
    "integer" = "<i4",
    "double" = "<f8",
    "character" = "|S8" # TODO: how many bytes to use here?
  )
  rtype_str <- typeof(a)
  return(RTYPE_DTYPE_MAPPING[[rtype_str]])
}

#' @keywords internal
get_dtype_asrtype <- function(dtype) {
  dtype_parts <- get_dtype_parts(dtype)
  check_dtype_support(dtype_parts)

  # Reference: https://github.com/gzuidhof/zarr.js/blob/292804/src/nestedArray/types.ts#L32
  DTYPE_RTYPE_MAPPING <- list(
    "b" = as.logical,
    "u" = as.integer,
    "i" = as.integer,
    "f" = as.double,
    "S" = as.character,
    "U" = as.character
  )
  return(DTYPE_RTYPE_MAPPING[[dtype_parts$basic_type]])
}

#' @keywords internal
get_typed_array_ctr <- function(dtype) {
  rtype <- get_dtype_rtype(dtype)
  return(function(dim) array(data = rtype, dim = dim))
}