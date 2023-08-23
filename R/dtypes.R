# Internal utility functions for converting between Zarr and R data types.

is_structured_dtype <- function(dtype) {
  if(is.character(dtype) && length(dtype) == 1) {
    return(FALSE)
  }
  return(TRUE)
}

get_dtype_parts <- function(dtype) {
  # TODO: support object dtype (without digits required in regex)
  dtype_regex <- "^(\\||>|<)(b|i|u|f|c|m|M|S|U|V|O)(\\d+)"
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

#' The Zarr Dtype class.
#' @title Dtype Class
#' @docType class
#' @description
#' 
#' @rdname Dtype
#' @keywords internal
Dtype <- R6::R6Class("Dtype",
  public = list(
    dtype = NULL,
    byte_order = NULL,
    basic_type = NULL,
    num_bytes = NULL,
    num_items = NULL,
    #' @description
    #' Create a new Dtype instance.
    #' @return A `Dtype` instance.
    initialize = function(dtype, object_codec) {
      self$dtype <- dtype

      # TODO: support dtype_str == "|O" for object dtypes

      dtype_parts <- get_dtype_parts(dtype)
      check_dtype_support(dtype_parts)
      self$byte_order <- dtype_parts$byte_order
      self$basic_type <- dtype_parts$basic_type
      self$num_bytes <- dtype_parts$num_bytes
      self$num_items <- dtype_parts$num_items

      # TODO: port code from normalize_dtype in zarr-python
      
      self$object_codec <- object_codec
    },
    get_asrtype = function() {
      return(get_dtype_asrtype(self$dtype))
    },
    get_rtype = function() {
      return(get_dtype_rtype(self$dtype))
    },
    is_signed = function() {
      return(get_dtype_signed(self$dtype))
    },
    is_structured = function() {
      return(is_structured_dtype(self$dtype))
    },
    is_object = function() {
      return(self$basic_type == "O")
    },
    get_byte_order = function() {
      return(self$byte_order)
    },
    get_basic_type = function() {
      return(self$basic_type)
    },
    get_num_bytes = function() {
      return(self$num_bytes)
    },
    get_num_items = function() {
      return(self$num_items)
    }
  )
)
