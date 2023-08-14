#' The Zarr Array class.
#' @title Array Class
#' @docType class
#' @description
#' Instantiate an array from an initialized store.
#' Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/core.py#L51
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
    #' @field vindex
    #' @keywords internal
    vindex = NULL,
    #' @field oindex
    #' @keywords internal
    oindex = NULL,
    #' @description
    #' (Re)load metadata from store.
    load_metadata_nosync = function() {
      mkey <- paste0(private$key_prefix, ARRAY_META_KEY)
      meta_bytes <- self$store$get_item(mkey)
      meta <- decode_array_meta(meta_bytes)
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
      if(is_na(meta$compressor) || is.null(meta$compressor)) {
        private$compressor <- NA
      } else {
        private$compressor <- get_codec(meta$compressor)
      }
      if(is_na(meta$filters) || is.null(meta$filters)) {
        private$filters <- NA
      } else {
        private$filters <- list()
        for(config in meta$filters) {
          append(private$filters, get_codec(config))
        }
      }
    },
    load_metadata = function() {
      private$load_metadata_nosync()
      # TODO: support for synchronization
    },
    refresh_metadata_nosync = function() {
      if(!self$cache_metadata && !private$is_view) {
        private$load_metadata_nosync()
      }
    },
    refresh_metadata = function() {
      if(!self$cache_metadata) {
        private$load_metadata()
      }
    },
    flush_metadata_nosync = function() {
      if(private$is_view) {
        stop("Operation not permitted for views")
      }
      if(!is.na(private$compressor)) {
        compressor_config <- private$compressor$get_config()
      } else {
        compressor_config <- NA
      }
      if(!is.na(private$filters)) {
        filters_config <- list()
        for(filter in private$filters) {
          append(filters_config, filter$get_config)
        }
      } else {
        filters_config <- NA
      }
      meta <- list(
        shape = private$shape,
        chunks = private$chunks,
        dtype = private$dtype,
        compressor = compressor_config,
        fill_value = private$vill_value,
        order = private$order,
        filters = filters_config
      )
      mkey <- paste0(private$key_prefix, ARRAY_META_KEY)
      self$store$set_item(mkey, encode_array_meta(meta))
    },
    chunk_key = function(chunk_coords) {
      # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/core.py#L2063
      return(paste0(private$key_prefix, do.call(paste, c(as.list(chunk_coords), sep = private$dimension_separator))))
    },
    compute_cdata_shape = function() {
      # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/core.py#L428
      if(is.null(private$shape)) {
        return(1)
      }
      shape <- private$shape
      chunks <- private$chunks
      cdata_shape <- list()
      for(i in seq_len(length(shape))) {
        s <- shape[i]
        c <- chunks[i]
        cdata_shape <- append(cdata_shape, ceiling(s / c))
      }
      cdata_shape <- as.numeric(cdata_shape)
      return(cdata_shape)
    },
    resize_nosync = function(...) {
      # Note: When resizing an array, the data are not rearranged in any way.
      # If one or more dimensions are shrunk, any chunks falling outside the
      # new array shape will be deleted from the underlying store.
      # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/core.py#L2340
      args <- list(...)
      old_shape <- private$shape
      new_shape <- as.numeric(normalize_resize_args(old_shape, args))
      old_cdata_shape <- private$compute_cdata_shape()

      # Update metadata
      private$shape <- new_shape
      private$flush_metadata_nosync()

      # Determine the new number and arrangement of chunks
      chunks <- private$chunks
      new_cdata_shape <- list()
      for(i in seq_len(length(new_shape))) {
        s <- new_shape[i]
        c <- chunks[i]
        new_cdata_shape <- append(new_cdata_shape, ceiling(s / c))
      }
      new_cdata_shape <- as.numeric(new_cdata_shape)

      # Remove any chunks not within range
      chunk_store <- self$get_chunk_store()
      cidx_df <- do.call(expand.grid, lapply(old_cdata_shape, seq_len))
      for(row_idx in seq_len(dim(cidx_df)[1])) {
        cidx <- as.numeric(cidx_df[row_idx, ])
        if(all(as.logical(lapply(zip_numeric(cidx, new_cdata_shape), function(v) v[1] < v[2])))) {
          # pass; keep the chunk
        } else {
          key <- private$chunk_key(cidx)
          # TODO: try to delete the key from chunk_store
          message(paste("TODO: delete chunk", jsonlite::toJSON(cidx)))
        }
      }
    },
    get_basic_selection_zd = function(selection = NA, out = NA, fields = NA) {
      # Special case basic selection for zero-dimensional array
      # Check selection is valid
      if(!is.null(selection) && selection != "...") {
        stop("err_too_many_indices(selection, ())")
      }
      selection <- ensure_list(selection)  # TODO
      # Obtain encoded data for chunk
      ckey <- private$chunk_key(c(0))
      cdata <- self$get_chunk_store()$get_item(ckey)
      # TODO: use try-catch
      chunk <- private$decode_chunk(cdata)

      # Handle fields
      if(!is.na(fields)) {
        chunk <- chunk[fields]
      }

      # Handle selection of the scalar value via empty tuple
      if(is.na(out)) {
        out <- chunk[selection]
      } else {
        out[selection] <- chunk[selection]
      }
      return(out)
    },
    get_basic_selection_nd = function(selection = NA, out = NA, fields = NA) {
      indexer <- BasicIndexer$new(selection, self)
      return(private$get_selection(indexer, out = out, fields = fields))
    },
    get_selection = function(indexer, out = NA, fields = NA) {
      # Reference: https://github.com/gzuidhof/zarr.js/blob/292804/src/core/index.ts#L304
      # We iterate over all chunks which overlap the selection and thus contain data
      # that needs to be extracted. Each chunk is processed in turn, extracting the
      # necessary data and storing into the correct location in the output array.

      out_dtype <- private$dtype
      out_shape <- indexer$shape
      out_size <- compute_size(indexer$shape)

      if(!is.na(out)) {
        # TODO: handle out provided as parameter
      } else {
        out <- NestedArray$new(NULL, out_shape, out_dtype)
      }

      if(out_size == 0) {
        return(out)
      }

      # TODO: use queue to handle async iterator
      for(proj in indexer$iter()) {
        private$chunk_getitem(proj$chunk_coords, proj$chunk_sel, out, proj$out_sel, drop_axes = indexer$drop_axes)
      }

      # Return scalar instead of zero-dimensional array.
      if(length(out$shape) == 0) {
        return(out$data[0])
      }
      return(out)

    },
    set_basic_selection_zd = function(selection, value, fields = NA) {
      # TODO
      print(paste("set_basic_selection_zd", selection, value))
    },
    set_basic_selection_nd = function(selection, value, fields = NA) {
      indexer <- BasicIndexer$new(selection, self)
      return(private$set_selection(indexer, value = value, fields = fields))
    },
    set_selection = function(indexer, value, fields = NA) {
      # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/core.py#L1682
      # Reference: https://github.com/gzuidhof/zarr.js/blob/15e3a3f00eb19f0133018fb65f002311ea53bb7c/src/core/index.ts#L566

      # // We iterate over all chunks which overlap the selection and thus contain data
      # // that needs to be replaced. Each chunk is processed in turn, extracting the
      # // necessary data from the value array and storing into the chunk array.

      # // N.B., it is an important optimisation that we only visit chunks which overlap
      # // the selection. This minimises the number of iterations in the main for loop.

      selection_shape <- indexer$shape

      # Check value shape
      if (length(selection_shape) == 0) {
        # Setting a single value
      } else if (is.scalar(value)) {
        # Setting a scalar value
      } else if(class(value) == "array") {
        if (dim(value) != selection_shape) {
          stop("Shape mismatch in source array and set selection: ${dim(value)} and ${selectionShape}")
        }
        value <- NestedArray$new(value)
      } else if ("NestedArray" %in% class(value)) {
        if (value$shape != selection_shape) {
          stop("Shape mismatch in source NestedArray and set selection: ${value.shape} and ${selectionShape}")
        }
      } else {
        # // TODO(zarr.js) support TypedArrays, buffers, etc
        stop("Unknown data type for setting :(")
      }

      # TODO: use queue to handle async iterator
      for (proj in indexer$iter()) {
        chunk_value <- private$get_chunk_value(proj, indexer, value, selection_shape)
        private$chunk_setitem(proj$chunk_coords, proj$chunk_sel, chunk_value)
      }
    },
    process_chunk = function(out, cdata, chunk_selection, drop_axes, out_is_ndarray, fields, out_selection, partial_read_decode = FALSE) {
      # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/core.py#L1755
      # TODO
    },
    get_chunk_value = function(proj, indexer, value, selection_shape) {
      # Reference: https://github.com/gzuidhof/zarr.js/blob/15e3a3f00eb19f0133018fb65f002311ea53bb7c/src/core/index.ts#L550
      
      # value is the full NestedArray representing the value to be set.
      # we call value.get() to get the value for the current chunk selection,
      # since the full value might span multiple chunks.

      print(value)

      if (length(selection_shape) == 0) {
        chunk_value <- value
      } else if (is.scalar(value)) {
        chunk_value <- value
      } else {
        chunk_value <- value$get(proj$out_sel)
        if (isTRUE(indexer$drop_axes)) {
          stop("Handling drop axes not supported yet")
        }
      }
      return(chunk_value)
    },
    chunk_getitem = function(chunk_coords, chunk_selection, out, out_selection, drop_axes = NA, fields = NA) {
      # TODO
      # Reference: https://github.com/gzuidhof/zarr.js/blob/15e3a3f00eb19f0133018fb65f002311ea53bb7c/src/core/index.ts#L380

    },
    chunk_getitems = function(lchunk_coords, lchunk_selection, out, lout_selection, drop_axes = NA, fields = NA) {
      # TODO
    },
    chunk_setitem = function(chunk_coords, chunk_selection, value, fields = NA) {
      # Reference: https://github.com/gzuidhof/zarr.js/blob/15e3a3f00eb19f0133018fb65f002311ea53bb7c/src/core/index.ts#L625

      if (private$order == "F" && self$get_ndim() > 1) {
        stop("Setting content for arrays in F-order is not supported.")
      }

      # Obtain key for chunk storage
      chunk_key <- private$chunk_key(chunk_coords)

      chunk <- NULL

      dtype_constr = get_typed_array_ctr(private$dtype)
      chunk_size <- compute_size(private$chunks)

      if (is_total_slice(chunk_selection, private$chunks)) {
        # Totally replace chunk

        # Optimization: we are completely replacing the chunk, so no need
        # to access the existing chunk data

        if (is.scalar(value)) {
          # TODO get the right type here
          chunk <- dtype_constr(chunk_size)
          chunk_fill(chunk, value)
        } else {
          # value is a NestedArray
          chunk <- value$flatten()
        }
      } else {
        # partially replace the contents of this chunk

        # Existing chunk data
        #let chunkData: TypedArray;

        chunk_data <- tryCatch({

          # Chunk is initialized if this does not error
          chunk_store_data <- self$get_chunk_store()$get_item(chunk_key)
          dbytes <- private$decode_chunk(chunk_store_data)
          return(private$to_typed_array(dbytes))
        }, error = function(cond) {
          if (is_key_error(cond)) {
            # Chunk is not initialized
            chunk_data <- dtype_constr(chunk_size)
            if (!is.null(private$fill_value)) { # TODO: should this be is.na
              chunk_fill(chunk_data, private$fill_value)
            }
            return(chunk_data)
          } else {
            # // Different type of error - rethrow
            stop("throw error;")
          }
        })

        chunk_nested_array <- NestedArray$new(
          chunk_data,
          private$chunks,
          private$dtype
        )
        chunk_nested_array$set(chunk_selection, value)
        chunk <- chunk_nested_array$flatten()
      }
      chunk_data <- private$encode_chunk(chunk)
      self$get_chunk_store()$set_item(chunk_key, chunk_data)
    },
    to_typed_array = function(buffer) {
      ctr <- get_typed_array_ctr(private$dtype)
      return(ctr(buffer))
    },
    chunk_setitem_nosync = function(chunk_coords, chunk_selection, value, fields = NA) {
      # TODO
    },
    chunk_setitems = function(lchunk_coords, lchunk_selection, values, fields = NA) {
      # TODO
    },
    process_for_setitem = function(ckey, chunk_selection, value, fields = NA) {
      # TODO
    },
    chunk_delitem = function(ckey) {
      # TODO
    },
    chunk_delitems = function(ckeys) {
      # TODO
    },
    decode_chunk = function(cdata, start = NA, nitems = NA, expected_shape = NA) {
      # TODO
    },
    encode_chunk = function(chunk) {
      # TODO
    },
    append_nosync = function(data, axis = 0) {
      # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/core.py#L2141
      # TODO
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
    #' Create a new Array instance.
    #' @param store Array store, already initialized.
    #' @return An `Array` instance.
    initialize = function(store, path = NA, read_only = FALSE, chunk_store = NA, synchronizer = NA, cache_metadata = TRUE, cache_attrs = TRUE, write_empty_chunks = TRUE) {
      self$store <- store
      self$chunk_store <- chunk_store
      if(!is.na(path)) {
        self$path <- normalize_storage_path(path)
        private$key_prefix <- paste0(self$path, "/")
      } else {
        self$path <- NA
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

      private$vindex <- OIndex$new(self)
      private$oindex <- VIndex$new(self)
    },
    get_store = function() {
      return(self$store)
    },
    get_path = function() {
      return(self$path)
    },
    get_name = function() {
      if(!is.na(self$path)) {
        name <- self$path
        name_vec <- str_to_vec(name)
        if(name_vec[1] != "/") {
          name <- paste0("/", name)
        }
        return(name)
      }
      return(NA)
    },
    get_basename = function() {
      name <- self$get_name()
      if(!is.na(name)) {
        name_segments <- stringr::str_split(name, "/")[[1]]
        return(name_segments[length(name_segments)])
      }
      return(NA)
    },
    get_read_only = function() {
      return(self$read_only)
    },
    set_read_only = function(val) {
      self$read_only <- val
    },
    get_chunk_store = function() {
      if(is.na(self$chunk_store)) {
        return(self$store)
      } else {
        return(self$chunk_store)
      }
    },
    get_shape = function() {
      private$refresh_metadata()
      return(private$shape)
    },
    set_shape = function(value) {
      self$resize(value)
    },
    #' @description
    #' Change the shape of the array by growing or shrinking one or more dimensions.
    resize = function(...) {
      args <- list(...)
      do.call(private$resize_nosync, args)
    },
    get_chunks = function() {
      return(private$chunks)
    },
    get_dtype = function() {
      return(private$dtype)
    },
    get_compressor = function() {
      return(private$compressor)
    },
    get_fill_value = function() {
      return(private$fill_value)
    },
    set_fill_value = function(val) {
      private$fill_value <- val
      private$flush_metadata_nosync()
    },
    get_order = function() {
      return(private$order)
    },
    get_filters = function() {
      return(private$filters)
    },
    get_synchronizer = function() {
      return(self$synchronizer)
    },
    get_attrs = function() {
      return(private$attrs)
    },
    get_ndim = function() {
      return(length(private$shape))
    },
    get_size = function() {
      # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/core.py#L383
      # TODO
    },
    get_itemsize = function() {
      # TODO
    },
    get_nbytes = function() {
      private$refresh_metadata()
      return(self$get_size() * self$get_itemsize())
    },
    get_nbytes_stored = function() {
      # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/core.py#L413
      # TODO
    },
    get_cdata_shape = function() {
      private$refresh_metadata()
      return(private$compute_cdata_shape())
    },
    get_nchunks = function() {
      # TODO
    },
    get_nchunks_initialized = function() {
      # TODO
    },
    get_is_view = function() {
      return(private$is_view)
    },
    get_oindex = function() {
      return(private$oindex)
    },
    get_vindex = function() {
      return(private$vindex)
    },
    get_write_empty_chunks = function() {
      return(private$write_empty_chunks)
    },
    equals = function(other) {
      return(all(c(
        class(other)[[1]] == "Array",
        # TODO: check store equality also
        self$read_only == other$get_read_only(),
        self$path == other$get_path(),
        !private$is_view
      )))
    },
    islice = function(start = NA, end = NA) {
      # TODO
    },
    length = function() {
      if(private$shape) {
        return(private$shape[1])
      } else {
        # 0-dimensional array, same error message as numpy
        stop("length of unized object")
      }
    },
    #' @param selection Selections are lists containing either scalars, strings, or Slice objects.
    get_item = function(selection) {
      # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/core.py#L580
      # Reference: https://github.com/gzuidhof/zarr.js/blob/master/src/core/index.ts#L266
      return(self$get_basic_selection(selection))
    },
    get_basic_selection = function(selection = NA, out = NA, fields = NA) {
      # Refresh metadata
      if(!self$cache_metadata) {
        private$load_metadata()
      }
      # Handle zero-dimensional arrays
      if(is.null(private$shape)) {
        return(private$get_basic_selection_zd(selection, out = out, fields = fields))
      }
      return(private$get_basic_selection_nd(selection, out = out, fields = fields))
    },
    get_orthogonal_selection = function(selection = NA, out = NA, fields = NA) {
      # TODO
    },
    get_coordinate_selection = function(selection = NA, out = NA, fields = NA) {
      # TODO
    },
    get_mask_selection = function(selection = NA, out = NA, fields = NA) {
      # TODO
    },
    set_item = function(selection, value) {
      self$set_basic_selection(selection, value)
    },
    set_basic_selection = function(selection, value, fields = NA) {
      # Handle zero-dimensional arrays
      if(is.null(private$shape)) {
        return(private$set_basic_selection_zd(selection, value = value, fields = fields))
      }
      return(private$set_basic_selection_nd(selection, value = value, fields = fields))
    },
    set_orthogonal_selection = function(selection, value, fields = NA) {
      # TODO
    },
    set_coordinate_selection = function(selection, value, fields = NA) {
      # TODO
    },
    set_mask_selection = function(selection, value, fields = NA) {
      # TODO
    },
    get_info = function() {
      # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/core.py#L2141
      # TODO
    },
    get_digest = function(hashname = "sha1") {
      # TODO
    },
    get_hexdigest = function(hashname = "sha1") {
      # TODO
    },
    append = function(data, axis = 0) {
      private$append_nosync(data, axis)
    },
    view = function(shape = NA, chunks = NA, dtype = NA, fill_value = NA, filters = NA, read_only = NA, synchronizer = NA) {
      # TODO
    },
    astype = function(dtype) {
      # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/core.py#L2586
      # TODO
    }
  )
)
