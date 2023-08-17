

# Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0e6cdc04c6413e14f57f61d389972ea937c/zarr/meta.py#L14
Metadata2 <- R6::R6Class("Metadata2",
    private = list(
        ZARR_FORMAT = 2
    ),
    public = list(
        parse_metadata = function(s) {
            if(is.list(s)) {
                return(s)
            } else {
                return(jsonlite::fromJSON(s))
            }
        },
        decode_array_metadata = function(s) {
            meta <- self$parse_metadata(s)
            # TODO: check zarr format is v2
            return(meta)
        },
        decode_group_metadata = function(s) {
            meta <- self$parse_metadata(s)
            # TODO: check zarr format is v2
            return(meta)
        },
        encode_array_metadata = function(meta) {
            clean_meta <- meta
            clean_meta[['zarr_format']] <- private$ZARR_FORMAT
            # TODO: clean up meta even further
            return(jsonlite::toJSON(clean_meta))
        },
        encode_group_metadata = function(meta = NA) {
            meta <- obj_list()
            meta[['zarr_format']] <- private$ZARR_FORMAT
            return(jsonlite::toJSON(meta))
        }
    )
)

# TODO: v3 metadata