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
                       #' @return Compressed data.
                       encode = function(buf) {
                         return(buf)
                       },
                       #' @description
                       #' Decompress data.
                       #' @param buf The compressed data.
                       #' @return Un-compressed data.
                       decode = function(buf) {
                         return(buf)
                       },
                       get_meta = function() {
                         return(jsonlite::unbox(NA))
                       }
                     )
)


#' ZSTD compressor for Zarr
#' @title Zstd Class
#' @docType class
#' @description
#' Class representing a ZSTD compressor
#'
#' @rdname Zstd
#' @export
Zstd <- R6::R6Class("Zstd",
                    inherit = Codec,
                    public = list(
                      #' @field level The compression level.
                      #' @keywords internal
                      level = NULL,
                      #' @description
                      #' Create a new ZSTD compressor.
                      #' @param level The compression level, between 1 and 22.
                      #' @return A new `Zstd` object.
                      initialize = function(level = 1) {
                        self$level <- level
                      },
                      encode = function(buf) {
                        # Reference: https://github.com/traversc/qs/blob/84e30f4/R/RcppExports.R#L16
                        result <- qs::zstd_compress_raw(buf, self$level)
                        return(result)
                      },
                      decode = function(buf) {

                      },
                      get_meta = function() {
                        meta <- list(
                          id = jsonlite::unbox("zstd"),
                          level = jsonlite::unbox(self$level)
                        )
                        return(meta)
                      }
                    )
)

#' LZ4 compressor for Zarr
#' @title LZ4 Class
#' @docType class
#' @description
#' Class representing a LZ4 compressor
#'
#' @rdname LZ4
#' @export
LZ4 <- R6::R6Class("LZ4",
                   inherit = Codec,
                   public = list(
                     #' @field acceleration The compression level.
                     #' @keywords internal
                     acceleration = NULL,
                     #' @description
                     #' Create a new LZ4 compressor.
                     #' @param acceleration The compression level.
                     #' @return A new `LZ4` object.
                     initialize = function(acceleration = 1) {
                       self$acceleration <- acceleration
                     },
                     encode = function(buf) {
                       # Reference: https://github.com/traversc/qs/blob/84e30f4/R/RcppExports.R#L24
                       body <- qs::lz4_compress_raw(buf, self$acceleration)

                       # The compressed output includes a 4-byte header storing the original size
                       # of the decompressed data as a little-endian 32-bit integer.
                       # Reference: https://numcodecs.readthedocs.io/en/stable/lz4.html#numcodecs.lz4.compress
                       orig_size <- length(buf)
                       header <- as.raw(c(
                         bitwAnd(orig_size, 0xff),
                         bitwAnd(bitwShiftR(orig_size, 8), 0xff),
                         bitwAnd(bitwShiftR(orig_size, 16), 0xff),
                         bitwAnd(bitwShiftR(orig_size, 24), 0xff)
                       ))

                       result <- c(header, body)

                       return(result)
                     },
                     decode = function(buf) {

                     },
                     get_meta = function() {
                       meta <- list(
                         id = jsonlite::unbox("lz4"),
                         acceleration = jsonlite::unbox(self$acceleration)
                       )
                       return(meta)
                     }
                   )
)
