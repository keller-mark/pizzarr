#' @keywords internal
char_vec_to_raw <- function(char_vec, basic_type, num_bytes, byte_order) {
  # Reference: https://stat.ethz.ch/R-manual/R-devel/library/base/html/iconv.html
  if(basic_type == "S") {
    iconv_to <- "UTF-8"
    max_bytes_per_char <- 1
  } else if(basic_type == "U") {
    if(byte_order == "little") {
      iconv_to <- "UTF-32LE"
    } else if(byte_order == "big") {
      iconv_to <- "UTF-32BE"
    } else {
      stop("Got unexpected byte_order in char_vec_to_raw()")
    }
    max_bytes_per_char <- 4
  } else {
    stop("Unexpected basic_type in char_vec_to_raw()")
  }

  list_of_raw <- iconv(char_vec, to = iconv_to, toRaw = TRUE)

  buf <- raw(length = length(list_of_raw) * num_bytes)

  for(i in seq_len(length(list_of_raw))) {
    raw_vec_i <- list_of_raw[[i]]

    if(length(raw_vec_i) > num_bytes * max_bytes_per_char) {
      stop("Unexpected length of raw_vec_i in char_vec_to_raw(): string probably too long for specified dtype")
    }

    offset_i_start <- (i-1) * num_bytes + 1
    offset_i_stop <- offset_i_start + length(raw_vec_i) - 1

    # TODO: take into account byte_order?
    buf[offset_i_start:offset_i_stop] <- raw_vec_i
  }

  return(buf)
}

raw_to_char_vec <- function(raw_vec, basic_type, num_bytes, byte_order) {
  # Reference: https://stat.ethz.ch/R-manual/R-devel/library/base/html/iconv.html
  if(basic_type == "S") {
    iconv_from <- "UTF-8"
    max_bytes_per_char <- 1
  } else if(basic_type == "U") {
    if(byte_order == "little") {
      iconv_from <- "UTF-32LE"
    } else if(byte_order == "big") {
      iconv_from <- "UTF-32BE"
    } else {
      stop("Got unexpected byte_order in raw_to_char_vec()")
    }
    max_bytes_per_char <- 4
  } else {
    stop("Unexpected basic_type in raw_to_char_vec()")
  }

  null_byte <- as.raw(rep(0x00, times = max_bytes_per_char))

  if(max_bytes_per_char == 1) {
    num_chars_per_item <- num_bytes
    remainder_null_byte <- as.raw(0x00)
  } else {
    num_chars_per_item <- ceiling(num_bytes / max_bytes_per_char)
    remainder_null_byte <- as.raw(rep(0x00, times = num_bytes %% max_bytes_per_char))
  }

  list_of_raw <- list()
  for(i in seq_len(length(raw_vec) / num_bytes)) {

    offset_i_start <- (i-1) * num_bytes + 1
    offset_i_stop <- offset_i_start + num_bytes - 1

    # We get the raw vector for each string in the array.
    raw_vec_i <- raw_vec[offset_i_start:offset_i_stop]

    if(max_bytes_per_char == 1) {
      # Since every character only uses one byte, we can simply remove all null bytes.
      raw_vec_i <- raw_vec_i[raw_vec_i != null_byte]
    } else {
      # Since each character uses multiple bytes, we need to remove only those characters
      # for which _all_ bytes are null bytes.

      # Convert the raw vector to an int32 vector and remove zeros, then convert back to raw,
      # so that we only remove characters that contain all null bytes.
      int_vec_i <- readBin(
        con = raw_vec_i,
        what = "integer",
        size = max_bytes_per_char,
        n = num_chars_per_item,
        signed = TRUE,
        endian = byte_order
      )
      int_vec_i <- int_vec_i[int_vec_i != 0]
      raw_vec_i <- writeBin(
        object = int_vec_i,
        con = raw(),
        size = max_bytes_per_char,
        endian = byte_order
      )
    }

    # We append the raw vector for this string to the list of raw vectors.
    # This is the input format required by `iconv`.
    list_of_raw <- append(list_of_raw, list(raw_vec_i))
  }
  char_vec <- iconv(list_of_raw, from = iconv_from, to = "UTF-8", toRaw = FALSE)
  return(char_vec)
}

#' @keywords internal
zero_based_to_one_based <- function(selection, shape) {
  selection_list <- list()
  for(i in seq_len(length(selection))) {
    sel <- selection[[i]]
    # We assume the selection uses zero-based indexing,
    # and internally convert to R-based / 1-based indexing
    # before accessing data on the internal self$data.
    sel_start <- sel$start + 1 # Add one, since R indexing is zero-based.
    sel_stop <- sel$stop # Do not subtract one, since R indexing is inclusive.
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
  private = list(
    is_zero_dim = NULL,
    basic_type = NULL,
    byte_order = NULL,
    num_bytes = NULL
  ),
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
    #' @param order The order of the array, either "C" or "F". Only used
    #' when `data` is a raw vector. Optional.
    #' @return A `NestedArray` instance.
    initialize = function(data, shape = NA, dtype = NA, order = NA) {
      if(is.null(shape) || (!is.list(shape) && is_na(shape))) {
        if(is.raw(data)) {
          stop("Cannot infer shape from raw data, please provide shape explicitly")
        }
        shape <- dim(data)
      } else {
        shape <- normalize_shape(shape)
      }
      if(is_na(dtype) && (is.numeric(data) || is.logical(data))) {
        dtype <- get_dtype_from_array(data)
      } else {
        dtype <- normalize_dtype(dtype)
      }
      self$shape <- shape
      self$dtype <- dtype

      dtype_parts <- get_dtype_parts(dtype)
      private$basic_type <- dtype_parts$basic_type
      private$byte_order <- dtype_parts$byte_order
      private$num_bytes <- dtype_parts$num_bytes

      private$is_zero_dim <- (is.null(shape) || length(shape) == 0)

      if(is.null(data)) {
        # Create empty array.

        self$data <- array(data=get_dtype_rtype(dtype), dim=shape)
      } else if(!is.raw(data) && is.null(self$shape)) {
        # Create zero-dimensional array.

        self$data <- data # TODO?
      } else if(!is.raw(data) && (is.array(data) || is.vector(data)) && is.atomic(data)) {
        # Create array from R atomic vector or array().

        num_shape_elements <- compute_size(shape)
        # Check that data array has same shape as expected
        if(!is.null(dim(data)) && all(ensure_vec(dim(data)) == ensure_vec(shape))) {
          self$data <- data
        } else {
          # Data array did not have the expected shape, so we need to reshape it.
          astype_func <- get_dtype_asrtype(dtype)
          self$data <- array(data=as.array(astype_func(data)), dim=shape)
        }
      } else if(is.raw(data)) {
        # Create array from a raw vector.

        num_shape_elements <- compute_size(shape)

        # Reference: https://github.com/gzuidhof/zarr.js/blob/292804/src/nestedArray/index.ts#L134
        buf <- data
        # Create from ArrayBuffer or Buffer
        
        dtype_size <- private$num_bytes
        num_data_elements <- length(buf) / dtype_size
        if (num_shape_elements != num_data_elements) {
          stop('Buffer has ${numDataElements} of dtype ${dtype}, shape is too large or small')
        }

        dtype_rtype <- get_dtype_rtype(dtype)
        dtype_signed <- get_dtype_signed(dtype)
        if(!dtype_signed && !(dtype_size == 1 || dtype_size == 2)) {
          # readBin will warn "signed = FALSE is only valid for integers of sizes 1 and 2"
          dtype_signed <- TRUE
        }

        endian <- get_dtype_endianness(self$dtype)
        # Normalize to only "little" or "big" since this is what writeBin accepts.
        if(endian == "nr") {
          endian <- "little"
        }

        if(private$basic_type %in% c("S", "U")) {
          vec_from_raw <- raw_to_char_vec(
            buf,
            private$basic_type,
            dtype_size,
            endian
          )
        } else {
          vec_from_raw <- readBin(
            con = buf,
            what = dtype_rtype,
            size = dtype_size,
            n = num_shape_elements,
            signed = dtype_signed,
            endian = endian
          )
        }
        
        if(private$is_zero_dim) {
          array_from_vec <- array(data = vec_from_raw, dim = c(1))
        } else {
          if(!is_na(order) && order == "C") {
            # Either “C” or “F”, defining the layout of bytes within each chunk of the array.
            # “C” means row-major order, i.e., the last dimension varies fastest;
            # “F” means column-major order, i.e., the first dimension varies fastest.
            # Reference: https://zarr.readthedocs.io/en/stable/spec/v2.html#metadata
            ordered_shape <- shape[rev(seq_len(length(shape)))]
            array_from_vec <- array(data = vec_from_raw, dim = ordered_shape)
            array_from_vec <- aperm(array_from_vec, rev(seq_len(length(shape))))
          } else {
            array_from_vec <- array(data = vec_from_raw, dim = shape)
          }
        }
        
        self$data <- array_from_vec
      } else if(is_scalar(data)) {
        # Create array from a scalar value.
        astype_func <- get_dtype_asrtype(dtype)
        if(private$is_zero_dim) {
          self$data <- array(data=get_dtype_rtype(dtype), dim=c(1))
        } else {
          self$data <- array(data=get_dtype_rtype(dtype), dim=shape)
        }
        self$data[] <- astype_func(data)
      } else {
        #buf_len <- compute_size(shape) * get_dtype_numbytes(dtype) 
        #buf <- raw(length = buf_len)
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
    #' @param order Either "C", "F", or NA.
    #' @returns The data as a flat vector.
    flatten = function(order = NA) {
      # Transpose first (if needed, based on the ordering).

      # “C” means row-major order, i.e., the last dimension varies fastest;
      # “F” means column-major order, i.e., the first dimension varies fastest.
      # Reference: https://zarr.readthedocs.io/en/stable/spec/v2.html#metadata

      if(!is_na(order) && order == "C" && !private$is_zero_dim) {
        ordered_data <- aperm(self$data, rev(seq_len(length(self$shape))))
      } else {
        ordered_data <- self$data
      }
      return(as.vector(ordered_data))
    },
    #' @description
    #' Flatten the array contents and convert to a raw vector.
    #' @param order Either "C", "F", or NA.
    #' @returns The data as a flat raw vector.
    flatten_to_raw = function(order = NA) {
      data_as_vec <- self$flatten(order = order)

      endian <- get_dtype_endianness(self$dtype)
      # Normalize to only "little" or "big" since this is what writeBin accepts.
      if(endian == "nr") {
        endian <- "little"
      }

      # "If writeBin is called with con a raw vector, it is just an indication that a raw vector should be returned."
      # Reference: https://stat.ethz.ch/R-manual/R-devel/library/base/html/readBin.html
      if(private$basic_type %in% c("S", "U")) {
        buf <- char_vec_to_raw(
          data_as_vec,
          private$basic_type,
          private$num_bytes,
          endian
        )
      } else {
        buf <- writeBin(
          data_as_vec,
          con = raw(),
          size = private$num_bytes,
          endian = endian
        )
      }
      return(buf)
      
    }
  )
)
