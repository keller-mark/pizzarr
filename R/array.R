#' The Zarr Array class.
#' @title Array Class
#' @docType class
#' @description
#' Instantiate an array from an initialized store.
#' Reference: https://zarr.readthedocs.io/en/stable/_modules/zarr/core.html#Array
#'
#' @rdname Array
#' @export
Array <- R6::R6Class("Array",
  private = list(
    #' @field key_prefix
    #' @keywords internal
    key_prefix = NULL,
    #' @field is_view
    #' @keywords internal
    is_view = NULL,
    #' @field attrs
    #' @keywords internal
    attrs = NULL,
    #' @field meta
    #' @keywords internal
    meta = NULL,
    #' @field shape
    #' @keywords internal
    shape = NULL,
    #' @field chunks
    #' @keywords internal
    chunks = NULL,
    #' @field dtype
    #' @keywords internal
    dtype = NULL,
    #' @field fill_value
    #' @keywords internal
    fill_value = NULL,
    #' @field order
    #' @keywords internal
    order = NULL,
    #' @field dimension_separator
    #' @keywords internal
    dimension_separator = NULL,
    #' @field compressor
    #' @keywords internal
    compressor = NULL,
    #' @field filters
    #' @keywords internal
    filters = NULL,
    #' @description
    #' Get an item from the store.
    #' @param key The item key.
    #' @return The item data in a vector of type raw.
    load_metadata = function() {
      mkey <- paste0(private$key_prefix, ARRAY_META_KEY)
      meta_bytes <- self$store$get_item(mkey)
      meta <- decode_array_metadata(meta_bytes)
      private$meta <- meta
      private$shape <- meta$shape
      private$chunks <- meta$chunks
      private$dtype <- meta$dtype
      private$fill_value <- meta$fill_value
      private$order <- meta$order
      if("dimension_separator" %in% names(meta) && !is.na(meta$dimension_separator) && !is.null(meta$dimension_separator)) {
        private$dimension_separator <- meta$dimension_separator
      } else {
        # TODO: check whether store has a dimension separator before reverting to "."
        private$dimension_separator <- "."
      }
      if(is.na(meta$compressor) || is.null(meta$compressor)) {
        private$compressor <- NA
      } else {
        private$compressor <- get_codec(meta$compressor)
      }
      if(is.na(meta$filters) || is.null(meta$filters)) {
        private$filters <- NA
      } else {
        private$filters <- list()
        for(config in meta$filters) {
          append(private$filters, get_codec(config))
        }
      }
    },
    refresh_metadata = function() {
      if(!self$cache_metadata) {
        private$load_metadata()
      }
    }
  ),
  public = list(
    #' @field store Array store, already initialized.
    store = NULL,
    #' @field chunk_store Separate storage for chunks. If not provided, `store` will be used for storage of both chunks and metadata.
    chunk_store = NULL,
    #' @field path Storage path. String, optional.
    path = NULL,
    #' @field read_only True if array should be protected against modification.
    read_only = NULL,
    #' @field synchronizer Array synchronizer. Object, optional.
    synchronizer = NULL,
    #' @field cache_metadata If True (default), array configuration metadata will be cached. If False, metadata will be reloaded prior to all data access and modification.
    cache_metadata = NULL,
    #' @field cache_attrs If True (default), user attributes will be cached. If False, attributes will be reloaded prior to all data access and modification.
    cache_attrs = NULL,
    #' @field write_empty_chunks If True, all chunks will be stored regardless of their contents. If False (default), each chunk is compared to the array's fill value prior to storing. If a chunk is uniformly equal to the fill value, then that chunk is not be stored, and the store entry for that chunk's key is deleted.
    write_empty_chunks = NULL,

    #' @description
    #' Create a new LZ4 compressor.
    #' @param store Array store, already initialized.
    #' @return An `Array` instance.
    initialize = function(store, path = NA, read_only = FALSE, chunk_store = NA, synchronizer = NA, cache_metadata = TRUE, cache_attrs = TRUE, write_empty_chunks = TRUE) {
      self$store <- store
      self$path <- normalize_storage_path(path)
      if(!is.na(path)) {
        private$key_prefix <- paste0(self$path, "/")
      } else {
        private$key_prefix <- ""
      }
      self$read_only <- read_only
      self$synchronizer <- synchronizer
      self$cache_metadata <- cache_metadata
      self$cache_attrs <- cache_attrs
      private$is_view <- FALSE
      self$write_empty_chunks <- write_empty_chunks

      private$load_metadata()

      akey <- paste0(private$key_prefix, ATTRS_KEY)
      private$attrs <- Attributes$new(store, key = akey)
    },
    #' @description
    #' Get the array shape.
    #' @return The shape metadata value.
    get_shape = function() {
      return(private$shape)
    }
  )
)
