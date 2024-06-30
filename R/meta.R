

# Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0e6cdc04c6413e14f57f61d389972ea937c/zarr/meta.py#L14
#' @keywords internal
Metadata2 <- R6::R6Class("Metadata2",
    private = list(
        ZARR_FORMAT = 2
    ),
    public = list(
        decode_metadata = function(s, auto_unbox=FALSE) {
            if(is.list(s)) {
                return(s)
            } else {
                return(try_fromJSON(rawToChar(s), simplifyVector = FALSE))
            }
        },
        encode_metadata = function(meta, auto_unbox=FALSE) {
            return(charToRaw(jsonlite::toJSON(meta, auto_unbox = auto_unbox)))
        },
        decode_array_metadata = function(s) {
            meta <- self$decode_metadata(s)
            if(!is.null(meta)) validate_v2_meta(meta)
            return(meta)
        },
        decode_group_metadata = function(s) {
            meta <- self$decode_metadata(s)
            if(!is.null(meta)) validate_v2_meta(meta)
            return(meta)
        },
        encode_array_metadata = function(meta) {
            clean_meta <- meta
            clean_meta[['zarr_format']] <- jsonlite::unbox(private$ZARR_FORMAT)
            # TODO: clean up meta even further
            return(self$encode_metadata(clean_meta))
        },
        encode_group_metadata = function(meta = NA) {
            meta <- obj_list()
            meta[['zarr_format']] <- jsonlite::unbox(private$ZARR_FORMAT)
            return(self$encode_metadata(meta))
        }
    )
)

validate_v2_meta <- function(meta) {
  if(meta$zarr_format != 2) stop("unsupported zarr format ", meta$zarr_format)
}

# TODO: v3 metadata