#' Abstract compressor for Zarr
#' @title Codec Class
#' @docType class
#' @description
#' Abstract class representing a compressor.
#'
#' @rdname Codec
#' @export
Codec <- R6::R6Class("Codec",
   public = list(
     #' @description
     #' Compress data.
     #' @param buf The un-compressed data.
     #' @param zarr_arr The ZarrArray instance.
     #' @return Compressed data.
     encode = function(buf, zarr_arr) {
       return(buf)
     },
     #' @description
     #' Decompress data.
     #' @param buf The compressed data.
    #' @param zarr_arr The ZarrArray instance.
     #' @return Un-compressed data.
     decode = function(buf, zarr_arr) {
       return(buf)
     },
     get_config = function() {
       return(jsonlite::unbox(NA))
     }
   )
)


#' ZSTD compressor for Zarr
#' @title ZstdCodec Class
#' @docType class
#' @description
#' Class representing a ZSTD compressor
#'
#' @rdname ZstdCodec
#' @export
ZstdCodec <- R6::R6Class("ZstdCodec",
  inherit = Codec,
  public = list(
    #' @field level The compression level.
    #' @keywords internal
    level = NULL,
    #' @description
    #' Create a new ZSTD compressor.
    #' @param level The compression level, between 1 and 22.
    #' @return A new `ZstdCodec` object.
    initialize = function(level = 1) {
      self$level <- level
    },
    encode = function(buf, zarr_arr) {
      # Reference: https://github.com/traversc/qs/blob/84e30f4/R/RcppExports.R#L16
      result <- qs::zstd_compress_raw(buf, self$level)
      return(result)
    },
    decode = function(buf, zarr_arr) {
      result <- qs::zstd_decompress_raw(buf)
      return(result)
    },
    get_config = function() {
      meta <- list(
        id = jsonlite::unbox("zstd"),
        level = jsonlite::unbox(self$level)
      )
      return(meta)
    }
  )
)

#' LZ4 compressor for Zarr
#' @title Lz4Codec Class
#' @docType class
#' @description
#' Class representing a LZ4 compressor
#'
#' @rdname Lz4Codec
#' @export
Lz4Codec <- R6::R6Class("Lz4Codec",
   inherit = Codec,
   public = list(
     #' @field acceleration The compression level.
     #' @keywords internal
     acceleration = NULL,
     #' @description
     #' Create a new LZ4 compressor.
     #' @param acceleration The compression level.
     #' @return A new `Lz4Codec` object.
     initialize = function(acceleration = 1) {
       self$acceleration <- acceleration
     },
     encode = function(buf, zarr_arr) {
       # Reference: https://github.com/traversc/qs/blob/84e30f4/R/RcppExports.R#L24
       body <- qs::lz4_compress_raw(buf, self$acceleration)

       # The compressed output includes a 4-byte header storing the original size
       # of the decompressed data as a little-endian 32-bit integer.
       # Reference: https://numcodecs.readthedocs.io/en/stable/lz4.html#numcodecs.lz4.compress
       orig_size <- length(buf)
       header <- writeBin(orig_size, con = raw(), size = 4, endian = "little")

       result <- c(header, body)

       return(result)
     },
     decode = function(buf, zarr_arr) {
      body <- buf[5:length(buf)]

      result <- qs::lz4_decompress_raw(body)
      return(result)
     },
     get_config = function() {
       meta <- list(
         id = jsonlite::unbox("lz4"),
         acceleration = jsonlite::unbox(self$acceleration)
       )
       return(meta)
     }
   )
)

#' Zlib compressor for Zarr
#' @title ZlibCodec Class
#' @docType class
#' @description
#' Class representing a zlib compressor
#'
#' @rdname ZlibCodec
#' @export
ZlibCodec <- R6::R6Class("ZlibCodec",
   inherit = Codec,
   public = list(
      #' @field level The compression level.
    level = NULL,
     #' @description
     #' Create a new Zlib compressor.
     #' @return A new `ZlibCodec` object.
     initialize = function(level = 6) {
      self$level <- level
      # No config options for zlib.
     },
     encode = function(buf, zarr_arr) {
      if(self$level != 6) {
        stop("Only system default compression level (normally 6) is enabled for writing.")
      }
       # References:
       # - https://github.com/grimbough/Rarr/blob/684541a86b0313a6f354282b60a08dd0ea0a747d/R/write_data.R#L385
       # - https://stat.ethz.ch/R-manual/R-devel/library/base/html/memCompress.html
      result <- memCompress(buf, type = "gzip")
      return(result)
     },
     decode = function(buf, zarr_arr) {
      # References:
       # - https://github.com/grimbough/Rarr/blob/684541a86b0313a6f354282b60a08dd0ea0a747d/R/read_data.R#L356C5-L370C6
       # - https://stat.ethz.ch/R-manual/R-devel/library/base/html/memCompress.html
      result <- memDecompress(buf, type = "gzip", asChar = FALSE)
      return(result)
     },
     get_config = function() {
       meta <- list(
         id = jsonlite::unbox("zlib"),
         level = jsonlite::unbox(as.integer(self$level))
       )
       return(meta)
     }
   )
)

#' Gzip compressor for Zarr
#' @title GzipCodec Class
#' @docType class
#' @description
#' Class representing a gzip compressor
#'
#' @rdname GzipCodec
#' @export
GzipCodec <- R6::R6Class("GzipCodec",
   inherit = Codec,
   public = list(
      #' @field level The compression level.
      level = NULL,
     #' @description
     #' Create a new Gzip compressor.
     #' @return A new `GzipCodec` object.
     initialize = function(level = 6) {
      # No config options for gzip.
      self$level <- level
     },
     encode = function(buf, zarr_arr) {
      if(self$level != 6) {
        stop("Only system default compression level (normally 6) is enabled for writing.")
      }
       # References:
       # - https://github.com/grimbough/Rarr/blob/684541a86b0313a6f354282b60a08dd0ea0a747d/R/read_data.R#L356C5-L370C6
       # - https://stat.ethz.ch/R-manual/R-devel/library/base/html/memCompress.html
      result <- memCompress(buf, type = "gzip")
      return(result)
     },
     decode = function(buf, zarr_arr) {
      result <- memDecompress(buf, type = "gzip", asChar = FALSE)
      return(result)
     },
     get_config = function() {
       meta <- list(
          id = jsonlite::unbox("gzip"),
          level = jsonlite::unbox(as.integer(self$level))
       )
       return(meta)
     }
   )
)

#' Bz2 compressor for Zarr
#' @title Bz2Codec Class
#' @docType class
#' @description
#' Class representing a bz2 compressor
#'
#' @rdname Bz2Codec
#' @export
Bz2Codec <- R6::R6Class("Bz2Codec",
   inherit = Codec,
   public = list(
    #' @field level The compression level.
      level = NULL,
     #' @description
     #' Create a new Bz2 compressor.
     #' @return A new `Bz2Codec` object.
     initialize = function(level = 6) {
      # No config options for bz2.
      self$level <- level
     },
     encode = function(buf, zarr_arr) {
       # References:
       # - https://github.com/grimbough/Rarr/blob/684541a86b0313a6f354282b60a08dd0ea0a747d/R/read_data.R#L356C5-L370C6
       # - https://stat.ethz.ch/R-manual/R-devel/library/base/html/memCompress.html
      if(self$level != 6) {
        stop("Only bzip2 compression level 6 is enabled for writing.")
      }
      result <- memCompress(buf, type = "bzip2")
      return(result)
     },
     decode = function(buf, zarr_arr) {
      result <- memDecompress(buf, type = "bzip2", asChar = FALSE)
      return(result)
     },
     get_config = function() {
       meta <- list(
         id = jsonlite::unbox("bz2"),
         level = jsonlite::unbox(as.integer(self$level))
       )
       return(meta)
     }
   )
)

#' Lzma compressor for Zarr
#' @title LzmaCodec Class
#' @docType class
#' @description
#' Class representing a lzma compressor
#'
#' @rdname LzmaCodec
#' @export
LzmaCodec <- R6::R6Class("LzmaCodec",
   inherit = Codec,
   public = list(
      #' @field level The compression level.
      level = NULL,
      #' @field format The compression format.
      format = NULL,
     #' @description
     #' Create a new lzma compressor.
     #' @return A new `LzmaCodec` object.
     initialize = function(level = 9, format = 1) {
      # No config options for lzma.
      self$level <- level
      self$format <- format
      if(format != 1) {
        stop("Only format 1 is supported for lzma compression")
      }
     },
     encode = function(buf, zarr_arr) {
       # References:
       # - https://github.com/grimbough/Rarr/blob/684541a86b0313a6f354282b60a08dd0ea0a747d/R/read_data.R#L356C5-L370C6
      if(self$level != 9) {
        stop("Only lzma compression level 9 is enabled for writing.")
      }
       # - https://stat.ethz.ch/R-manual/R-devel/library/base/html/memCompress.html
      result <- memCompress(buf, type = "xz")
      return(result)
     },
     decode = function(buf, zarr_arr) {
      result <- memDecompress(buf, type = "xz", asChar = FALSE)
      return(result)
     },
     get_config = function() {
       meta <- list(
         id = jsonlite::unbox("lzma"),
          level = jsonlite::unbox(as.integer(self$level)),
          format = jsonlite::unbox(as.integer(self$format))
       )
       return(meta)
     }
   )
)

#' Blosc compressor for Zarr
#' @title BloscCodec Class
#' @docType class
#' @description
#' Class representing a Blosc compressor
#'
#' @rdname BloscCodec
#' @export
BloscCodec <- R6::R6Class("BloscCodec",
  inherit = Codec,
  public = list(
    #' @field cname The compression algorithm to use.
    cname = NULL,
    #' @field clevel The compression level.
    clevel = NULL,
    #' @field shuffle The shuffle filter to use.
    shuffle = NULL,
    #' @field blocksize The block size.
    blocksize = NULL,
    #' @description
    #' Create a new Blosc compressor.
    #' @return A new `BloscCodec` object.
    initialize = function(cname = "lz4", clevel = 5, shuffle = TRUE, blocksize = NA) {
      self$cname <- cname
      self$clevel <- clevel
      self$shuffle <- shuffle
      self$blocksize <- blocksize # TODO: use
      # No config options for blosc.
      if (!require("Rarr", quietly = TRUE)) {
        stop("Rarr package must be installed to use the Blosc codec. Install with BiocManager::install('Rarr')")
      }
    },
    encode = function(buf, zarr_arr) {
      if(self$cname != "lz4" || self$clevel != 5) {
        stop("Only lz4 compression level 5 is enabled for writing.")
      }
      dtype_str <- zarr_arr$get_dtype()
      dtype_size <- get_dtype_numbytes(dtype_str)
       result <- .Call(
        "compress_chunk_BLOSC",
        buf,
        as.integer(dtype_size),
        PACKAGE = "Rarr"
      )
      return(result)
    },
    decode = function(buf, zarr_arr) {
      result <- .Call(
        "decompress_chunk_BLOSC",
        buf,
        PACKAGE = "Rarr"
      )
      return(result)
    },
    get_config = function() {
       meta <- list(
         id = jsonlite::unbox("blosc"),
         cname = jsonlite::unbox(as.character(self$cname)),
         clevel = jsonlite::unbox(as.integer(self$clevel)),
         shuffle = jsonlite::unbox(as.integer(self$shuffle))
       )
       return(meta)
    }
  )
)

#' Get a codec instance from the registry.
#'
#' @param config A codec config as a named list.
#' @return The instance of the codec.
get_codec <- function(config) {
  result <- Codec$new()
  if(!is_na(config)) {
    codec_id <- config$id
    config$id <- NULL
    if(codec_id == "lz4") {
      result <- do.call(Lz4Codec$new, config)
    } else if(codec_id == "zstd") {
      result <- do.call(ZstdCodec$new, config)
    } else if(codec_id == "zlib") {
      result <- do.call(ZlibCodec$new, config)
    } else if(codec_id == "gzip") {
      result <- do.call(GzipCodec$new, config)
    } else if(codec_id == "lzma") {
      result <- do.call(LzmaCodec$new, config)
    }  else if(codec_id == "blosc") {
      result <- do.call(BloscCodec$new, config)
    } else {
      stop(paste("Unknown codec", codec_id))
    }
  }
  return(result)
}

#' Get the default compressor.
#' @keywords internal
#' @returns A Zstd compressor instance.
get_default_compressor <- function() {
  return(ZstdCodec$new())
}