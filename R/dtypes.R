# Internal utility functions for converting between Zarr and R data types.

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
is_structured_dtype <- function(dtype) {
  if(is.character(dtype) && length(dtype) == 1) {
    return(FALSE)
  }
  return(TRUE)
}

#' @keywords internal
get_dtype_parts <- function(dtype) {
  # TODO: support object dtype (without digits required in regex)
  dtype_regex <- "^(\\||>|<)(b|i|u|f|c|m|M|S|U|V|O)(\\d+)?"
  if(stringr::str_detect(dtype, dtype_regex)) {
    dtype_matches <- stringr::str_match(dtype, dtype_regex)
    basic_type <- dtype_matches[1,3]
    if(basic_type == "U") {
      byte_multiplier <- 4
    } else {
      byte_multiplier <- 1
    }
    if(!is.null(dtype_matches[1,4])) {
      num_items <- as.integer(dtype_matches[1,4])
      num_bytes <- num_items * byte_multiplier
    } else {
      # Support object dtype
      num_items <- NA
      num_bytes <- NA
    }
    result <- list(
      dtype_str = dtype,
      byte_order = dtype_matches[1,2],
      basic_type = dtype_matches[1,3],
      num_bytes = num_bytes,
      num_items = num_items
    )
    return(result)
  } else {
    return(NA)
  }
}

#' @keywords internal
check_dtype_support <- function(dtype_parts) {
  if(!is_na(dtype_parts) && dtype_parts$basic_type %in% c("b", "i", "u", "f", "S", "U", "O")) {
    return(TRUE)
  }
  stop(paste("Unsupported dtype:", dtype_parts))
  return(FALSE)
}

#' @keywords internal
get_dtype_rtype <- function(basic_type) {

  # Reference: https://github.com/gzuidhof/zarr.js/blob/292804/src/nestedArray/types.ts#L32
  BASICTYPE_RTYPE_MAPPING <- list(
    "b" = logical(),
    "u" = integer(),
    "i" = integer(),
    "f" = double(),
    "S" = character(),
    "U" = character(),
    "O" = character() # TODO: will object always be character?
  )

  return(BASICTYPE_RTYPE_MAPPING[[basic_type]])
}

#' @keywords internal
get_dtype_endianness <- function(dtype) {
  dtype_parts <- get_dtype_parts(dtype)

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
  return(dtype_parts$num_bytes)
}

#' @keywords internal
get_dtype_signed <- function(dtype) {
  dtype_parts <- get_dtype_parts(dtype)

  DTYPE_SIGNED_MAPPING <- list(
    "b" = FALSE,
    "u" = FALSE,
    "i" = TRUE,
    "f" = TRUE,
    "S" = FALSE, # TODO: is this correct?
    "U" = FALSE,  # TODO: is this correct?
    "O" = FALSE
  )
  return(DTYPE_SIGNED_MAPPING[[dtype_parts$basic_type]])
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
    "U" = as.character,
    "O" = as.character
  )
  return(DTYPE_RTYPE_MAPPING[[dtype_parts$basic_type]])
}


# Reference: https://numpy.org/doc/stable/reference/arrays.dtypes.html

#' The Zarr Dtype class.
#' @title Dtype Class
#' @docType class
#' @description
#' 
#' @rdname Dtype
#' @export
Dtype <- R6::R6Class("Dtype",
  public = list(
    #' @field dtype The original dtype string, like "<f4".
    dtype = NULL,
    #' @field byte_order The byte order of the dtype, either "little", "big", or "nr".
    byte_order = NULL,
    #' @field basic_type The basic type of the dtype, like "f".
    basic_type = NULL,
    #' @field num_bytes The number of bytes of the dtype.
    num_bytes = NULL,
    #' @field num_items The number of items of the dtype.
    num_items = NULL,
    #' @field is_signed Whether the dtype is signed. Logical/boolean.
    is_signed = NULL,
    #' @field is_structured Whether the dtype is structured. Logical/boolean.
    is_structured = NULL,
    #' @field is_object Whether the dtype is an object. Logical/boolean.
    is_object = NULL,
    #' @field object_codec The object codec instance.
    object_codec = NULL,
    #' @description
    #' Create a new Dtype instance.
    #' @param dtype The original dtype string, like "<f4".
    #' @param object_codec The object codec instance.
    #' @return A `Dtype` instance.
    initialize = function(dtype, object_codec = NA) {
      self$dtype <- dtype

      dtype_parts <- get_dtype_parts(dtype)
      check_dtype_support(dtype_parts)
      self$byte_order <-  get_dtype_endianness(dtype)
      self$basic_type <- dtype_parts$basic_type
      self$num_bytes <- dtype_parts$num_bytes
      self$num_items <- dtype_parts$num_items

      self$is_signed <- get_dtype_signed(dtype)
      self$is_structured <- is_structured_dtype(dtype)
      self$is_object <- (self$basic_type == "O")

      self$object_codec <- object_codec
    },
    get_asrtype = function() {
      return(get_dtype_asrtype(self$dtype))
    },
    get_rtype = function() {
      return(get_dtype_rtype(self$basic_type))
    },
    get_typed_array_ctr = function() {
      rtype <- self$get_rtype()
      return(function(dim) array(data = rtype, dim = dim))
    }
  )
)
