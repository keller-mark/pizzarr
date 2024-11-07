# Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/core.py#L51

#' The Zarr Array class.
#' @title ZarrArray Class
#' @docType class
#' @description
#' Instantiate an array from an initialized store.
#' @param selection Selections are lists containing either scalars, strings, or Slice objects. Two character
#' literals are supported: "..." selects all remaining array dimensions and ":" selects all of a specific 
#' array dimension.
#'
#' @rdname ZarrArray
#' @export
ZarrArray <- R6::R6Class("ZarrArray",
  private = list(
    # store Array store, already initialized.
    #' @keywords internal
    store = NULL,
    #' chunk_store Separate storage for chunks. If not provided, `store` will be used for storage of both chunks and metadata.
    #' @keywords internal
    chunk_store = NULL,
    #' path Storage path. String, optional.
    #' @keywords internal
    path = NULL,
    #' read_only True if array should be protected against modification.
    #' @keywords internal
    read_only = NULL,
    #' synchronizer Array synchronizer. Object, optional.
    #' @keywords internal
    synchronizer = NULL,
    #' cache_metadata If True (default), array configuration metadata will be cached. If False, metadata will be reloaded prior to all data access and modification.
    #' @keywords internal
    cache_metadata = NULL,
    #' cache_attrs If True (default), user attributes will be cached. If False, attributes will be reloaded prior to all data access and modification.
    #' @keywords internal
    cache_attrs = NULL,
    #' write_empty_chunks If True, all chunks will be stored regardless of their contents. If False (default), each chunk is compared to the array's fill value prior to storing. If a chunk is uniformly equal to the fill value, then that chunk is not be stored, and the store entry for that chunk's key is deleted.
    #' @keywords internal
    write_empty_chunks = NULL,
    #' key_prefix TODO
    #' @keywords internal
    key_prefix = NULL,
    #' is_view TODO
    #' @keywords internal
    is_view = NULL,
    #' attrs TODO
    #' @keywords internal
    attrs = NULL,
    #' meta TODO
    #' @keywords internal
    meta = NULL,
    #' shape TODO
    #' @keywords internal
    shape = NULL,
    #' chunks TODO
    #' @keywords internal
    chunks = NULL,
    #' dtype TODO
    #' @keywords internal
    dtype = NULL,
    #' fill_value TODO
    #' @keywords internal
    fill_value = NULL,
    #' order TODO
    #' @keywords internal
    order = NULL,
    #' dimension_separator TODO
    #' @keywords internal
    dimension_separator = NULL,
    #' compressor TODO
    #' @keywords internal
    compressor = NULL,
    #' filters TODO
    #' @keywords internal
    filters = NULL,
    #' vindex TODO
    #' @keywords internal
    vindex = NULL,
    #' oindex TODO
    #' @keywords internal
    oindex = NULL,
    #' method_description
    #' (Re)load metadata from store without synchronization (file locking).
    load_metadata_nosync = function() {

      mkey <- paste0(private$key_prefix, ARRAY_META_KEY)
      
      meta <- try_from_zmeta(mkey, private$store)
      
      if(is.null(meta)) {
        meta_bytes <- private$store$get_item(mkey)
        if(!is.null(meta_bytes))
          meta <- private$store$metadata_class$decode_array_metadata(meta_bytes)
      }
      
      private$meta <- meta
      
      if(is.list(meta$shape)) {
        private$shape <- as.integer(meta$shape)
      } else {
        # meta$shape might be null.
        private$shape <- meta$shape
      }
      if(is.list(meta$chunks)) {
        private$chunks <- as.integer(meta$chunks)
      } else {
        # meta$chunks might be null.
        private$chunks <- meta$chunks
      }
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
      object_codec <- NA
      if(is_na(meta$filters) || is.null(meta$filters)) {
        private$filters <- NA
        object_codec <- NA
      } else {
        private$filters <- list()
        for(config in meta$filters) {
          private$filters <- append(private$filters, get_codec(config))
        }
        if(length(private$filters) == 1) {
          object_codec <- private$filters[[1]]
        }
      }
      private$dtype <- normalize_dtype(meta$dtype, object_codec = object_codec)
    },
    #' method_description
    #' Load or reload metadata from store.
    load_metadata = function() {
      private$load_metadata_nosync()
      # TODO: support for synchronization
    },
    #' method_description
    #' Referesh metadata if not cached without synchronization (file locking).
    refresh_metadata_nosync = function() {
      if(!private$cache_metadata && !private$is_view) {
        private$load_metadata_nosync()
      }
    },
    #' method_description
    #' Refresh metadata from store if not cached.
    refresh_metadata = function() {
      if(!private$cache_metadata) {
        private$load_metadata()
      }
    },
    #' method_description
    #' Write metadata to store without synchronization (file locking).
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
      zarray_meta <- list(
        shape = private$shape,
        chunks = private$chunks,
        dtype = private$dtype$dtype,
        compressor = compressor_config,
        fill_value = private$vill_value,
        order = private$order,
        filters = filters_config
      )
      mkey <- paste0(private$key_prefix, ARRAY_META_KEY)

      encoded_meta <- private$store$metadata_class$encode_array_metadata(zarray_meta)
      private$store$set_item(mkey, encoded_meta)
    },
    #' method_description
    #' TODO
    chunk_key = function(chunk_coords) {
      # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/core.py#L2063
      return(paste0(private$key_prefix, do.call(paste, c(as.list(chunk_coords), sep = private$dimension_separator))))
    },
    #' method_description
    #' TODO
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
    #' method_description
    #' Resize an array without synchronization (file locking)
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
    #' method_description
    #' TODO
    get_basic_selection_zd = function(selection = NA, out = NA, fields = NA) {
      # Special case basic selection for zero-dimensional array
      # Check selection is valid
      if(!is.null(selection) && selection != "...") {
        stop("err_too_many_indices(selection, ())")
      }
      selection <- ensure_list(selection)  # TODO
      # Obtain encoded data for chunk
      c_key <- private$chunk_key(c(0))

      chunk_nested_array <- tryCatch({
        c_data <- self$get_chunk_store()$get_item(c_key)
        chunk_inner <- private$decode_chunk(c_data)
        # From raw.
        NestedArray$new(chunk_inner, shape = private$chunks, dtype = private$dtype, order = private$order)
      }, error = function(cond) {
        if(is_key_error(cond)) {
          # chunk not initialized
          as_dtype_func <- private$dtype$get_asrtype()
          chunk_inner <- as_dtype_func(private$fill_value)
          # From array().
          return(NestedArray$new(chunk_inner, shape = private$chunks, dtype = private$dtype, order = private$order))
        } else {
          print(cond$message)
          stop("rethrow")
        }
      })      

      # TODO: Handle fields
      # if(!is.na(fields)) {
      #   chunk <- chunk[fields]
      # }

      # Handle selection of the scalar value via empty tuple
      if(is_na(out)) {
        out <- chunk_nested_array
      } else {
        # TODO
        #out[selection] <- as_scalar(chunk)
      }
      return(out)
    },
    #' method_description
    #' TODO
    get_basic_selection_nd = function(selection = NA, out = NA, fields = NA) {
      indexer <- BasicIndexer$new(selection, self)
      return(private$get_selection(indexer, out = out, fields = fields))
    },
    #' method_description
    #' TODO
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
        out <- NestedArray$new(NULL, shape = out_shape, dtype = out_dtype, order = private$order)
      }

      if(out_size == 0) {
        return(out)
      }

      parallel_option <- getOption("pizzarr.parallel_read_enabled")
      cl <- parse_parallel_option(parallel_option)
      is_parallel <- is_truthy_parallel_option(cl)
      
      apply_func <- lapply
      if(is_parallel) {
        if(!requireNamespace("pbapply", quietly = TRUE)) {
          stop("Parallel reading requires the 'pbapply' package.")
        }
        apply_func <- pbapply::pblapply
        if(is.integer(cl) & .Platform$OS.type == "windows") {
          # See #105
          cl <- parallel::makeCluster(cl)
          on.exit(parallel::stopCluster(cl))
        }
      }

      parts <- indexer$iter()
      part1_results <- apply_func(parts, function(proj, cl = NA) {
        private$chunk_getitem_part1(proj$chunk_coords, proj$chunk_sel, out, proj$out_sel, drop_axes = indexer$drop_axes)
      }, cl = cl)

      for(i in seq_along(parts)) {
        proj <- parts[[i]]
        part1_result <- part1_results[[i]]
        private$chunk_getitem_part2(part1_result, proj$chunk_coords, proj$chunk_sel, out, proj$out_sel, drop_axes = indexer$drop_axes)
      }

      # Return scalar instead of zero-dimensional array.
      if(length(out$shape) == 0) {
        return(out$data[0])
      }
      return(out)

    },
    #' method_description
    #' TODO
    set_basic_selection_zd = function(selection, value, fields = NA) {
      # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0e6cdc04c6413e14f57f61d389972ea937c/zarr/core.py#L1625

      # check selection is valid
      selection <- ensure_list(selection)
      if(!(length(selection) == 0 || selection == "...")) {
        stop("err_too_many_indices(selection, self._shape)")
      }

      # TODO: check fields
      #check_fields(fields, self._dtype)
      #fields = check_no_multi_fields(fields)

      # obtain key for chunk
      c_key <- private$chunk_key(c(0))

      # setup chunk
      # chunk <- tryCatch({
      #   # obtain compressed data for chunk
      #   c_data <- self$get_chunk_store()$get_item(c_key)
      #   # decode chunk
      #   chunk_inner <- private$decode_chunk(c_data)
      #   return(chunk_inner)
      # }, error = function(cond) {
      #   if(is_key_error(cond)) {
      #     # chunk not initialized
      #     as_dtype_func = get_dtype_asrtype(private$dtype)
      #     chunk_inner <- as_dtype_func(private$fill_value)
      #     return(chunk_inner)
      #   } else {
      #     print(cond$message)
      #     stop("rethrow")
      #   }
      # })
      
      # TODO
      # set value
      # if fields:
      #     chunk[fields][selection] = value
      # else:
      #     chunk[selection] = value

      # TODO
      # remove chunk if write_empty_chunks is false and it only contains the fill value
      # if (not self.write_empty_chunks) and all_equal(self.fill_value, chunk):
      #     try:
      #         del self.chunk_store[ckey]
      #         return
      #     except Exception:  # pragma: no cover
      #         # deleting failed, fallback to overwriting
      #         pass
      # else:

      # encode and store

      chunk_nested_array <- NestedArray$new(as_scalar(value), shape = NULL, dtype = private$dtype, order = private$order)
      chunk_raw <- chunk_nested_array$flatten_to_raw(order = private$order)

      c_data <- private$encode_chunk(chunk_raw)
      self$get_chunk_store()$set_item(c_key, c_data)
    },
    #' method_description
    #' TODO
    set_basic_selection_nd = function(selection, value, fields = NA) {
      indexer <- BasicIndexer$new(selection, self)
      return(private$set_selection(indexer, value = value, fields = fields))
    },
    #' method_description
    #' TODO
    set_selection = function(indexer, value, fields = NA) {
      # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/core.py#L1682
      # Reference: https://github.com/gzuidhof/zarr.js/blob/15e3a3f00eb19f0133018fb65f002311ea53bb7c/src/core/index.ts#L566

      # // We iterate over all chunks which overlap the selection and thus contain data
      # // that needs to be replaced. Each chunk is processed in turn, extracting the
      # // necessary data from the value array and storing into the chunk array.

      # // N.B., it is an important optimisation that we only visit chunks which overlap
      # // the selection. This minimises the number of iterations in the main for loop.

      selection_shape <- indexer$shape
      selection_shape_vec <- ensure_integer_vec(indexer$shape)

      if(sum(as.numeric(selection_shape)) > 0) {
        # Check value shape
        if (length(selection_shape) == 0) {
          # Setting a single value
        } else if (is_scalar(value)) {
          # Setting a scalar value
        } else if("array" %in% class(value)) {
          if (!all(ensure_integer_vec(dim(value)) == selection_shape_vec)) {
            stop("Shape mismatch in source array and set selection: ${dim(value)} and ${selectionShape}")
          }
          value <- NestedArray$new(value, shape = selection_shape_vec, dtype=private$dtype, order = private$order)
        } else if ("NestedArray" %in% class(value)) {
          if (!all(ensure_integer_vec(value$shape) == selection_shape_vec)) {
            stop("Shape mismatch in source NestedArray and set selection: ${value.shape} and ${selectionShape}")
          }
        } else {
          # // TODO(zarr.js) support TypedArrays, buffers, etc
          stop("Unknown data type for setting :(")
        }

        parallel_option <- getOption("pizzarr.parallel_write_enabled")
        cl <- parse_parallel_option(parallel_option)
        is_parallel <- is_truthy_parallel_option(cl)
        
        apply_func <- lapply
        if(is_parallel) {
          if(!requireNamespace("pbapply", quietly=TRUE)) {
            stop("Parallel writing requires the 'pbapply' package.")
          }
          apply_func <- pbapply::pblapply
          if(is.integer(cl) & .Platform$OS.type == "windows") {
            # See #105
            cl <- parallel::makeCluster(cl)
            on.exit(parallel::stopCluster(cl))
          }
        }

        parts <- indexer$iter()
        apply_func(parts, function(proj, cl = NA) {
          chunk_value <- private$get_chunk_value(proj, indexer, value, selection_shape)
          private$chunk_setitem(proj$chunk_coords, proj$chunk_sel, chunk_value)
          NULL
        }, cl = cl)
        
        return()
      }
    },
    #' method_description
    #' TODO
    process_chunk = function(out, cdata, chunk_selection, drop_axes, out_is_ndarray, fields, out_selection, partial_read_decode = FALSE) {
      # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/core.py#L1755
      # TODO
    },
    #' method_description
    #' TODO
    get_chunk_value = function(proj, indexer, value, selection_shape) {
      # Reference: https://github.com/gzuidhof/zarr.js/blob/15e3a3f00eb19f0133018fb65f002311ea53bb7c/src/core/index.ts#L550
      
      # value is the full NestedArray representing the value to be set.
      # we call value.get() to get the value for the current chunk selection,
      # since the full value might span multiple chunks.
      if (length(selection_shape) == 0) {
        chunk_value <- value
      } else if (is_scalar(value)) {
        chunk_value <- value
      } else {
        chunk_value <- value$get(proj$out_sel)
        if (isTRUE(indexer$drop_axes)) {
          stop("Handling drop axes not supported yet")
        }
      }
      return(chunk_value)
    },
    #' method_description
    #' TODO
    chunk_buffer_to_raw_array = function(decoded_chunk) {
      # TODO
    },
    #' method_description
    #' For parallel usage
    chunk_getitem_part1 = function(chunk_coords, chunk_selection, out, out_selection, drop_axes = NA, fields = NA) {
      if(length(chunk_coords) != length(private$chunks)) {
        stop("Inconsistent shapes: chunkCoordsLength: ${chunkCoords.length}, cDataShapeLength: ${this.chunkDataShape.length}")
      }
      c_key <- private$chunk_key(chunk_coords)

      result <- tryCatch({
        c_data <- self$get_chunk_store()$get_item(c_key)
        decoded_chunk <- private$decode_chunk(c_data)
        chunk_nested_arr <- NestedArray$new(decoded_chunk, shape=private$chunks, dtype=private$dtype, order = private$order)
        return(list(
          status = "success",
          value = chunk_nested_arr
        ))
      }, error = function(cond) {
        return(list(status = "error", value = cond))
      })
      return(result)
    },
    #' method_description
    #' For parallel usage
    chunk_getitem_part2 = function(part1_result, chunk_coords, chunk_selection, out, out_selection, drop_axes = NA, fields = NA) {
      c_key <- private$chunk_key(chunk_coords)

      if(part1_result$status == "success") {
        chunk_nested_arr <- part1_result$value

        if("NestedArray" %in% class(out)) {
          if(is_contiguous_selection(out_selection) && is_total_slice(chunk_selection, private$chunks) && is.null(private$filters)) {
            out$set(out_selection, chunk_nested_arr)
            return(TRUE)
          }

          # Decode chunk
          chunk <- chunk_nested_arr
          tmp <- chunk$get(chunk_selection)

          if(!is_na(drop_axes)) {
            stop("Drop axes is not supported yet")
          }
          out$set(out_selection, tmp)
        } else {
          # RawArray
          # Copies chunk by index directly into output. Doesn't matter if selection is contiguous
          # since store/output are different shapes/strides.
          #out$set(out_selection, private$chunk_buffer_to_raw_array(decoded_chunk), chunk_selection)
          stop("TODO: support out for chunk_getitem")
        }
      } else {
        # There was an error - this corresponds to the Catch statement in the non-parallel version.
        cond <- part1_result$value
        if(is_key_error(cond)) {
          # fill with scalar if cKey doesn't exist in store
          if(!is_na(private$fill_value)) {
            out$set(out_selection, as_scalar(private$fill_value))
          }
        } else {
          print(cond$message)
          stop("Different type of error - rethrow")
        }
      }
    },
    #' method_description
    #' For non-parallel usage
    chunk_getitem = function(chunk_coords, chunk_selection, out, out_selection, drop_axes = NA, fields = NA) {
      # TODO
      # Reference: https://github.com/gzuidhof/zarr.js/blob/15e3a3f00eb19f0133018fb65f002311ea53bb7c/src/core/index.ts#L380

      if(length(chunk_coords) != length(private$chunks)) {
        stop("Inconsistent shapes: chunkCoordsLength: ${chunkCoords.length}, cDataShapeLength: ${this.chunkDataShape.length}")
      }
      c_key <- private$chunk_key(chunk_coords)

      tryCatch({
        c_data <- self$get_chunk_store()$get_item(c_key)
        decoded_chunk <- private$decode_chunk(c_data)

        if("NestedArray" %in% class(out)) {
          if(is_contiguous_selection(out_selection) && is_total_slice(chunk_selection, private$chunks) && is.null(private$filters)) {
            out$set(out_selection, NestedArray$new(decoded_chunk, shape=private$chunks, dtype=private$dtype, order = private$order))
            return(TRUE)
          }

          # Decode chunk
          chunk <- NestedArray$new(decoded_chunk, shape=private$chunks, dtype=private$dtype, order = private$order)
          tmp <- chunk$get(chunk_selection)

          if(!is_na(drop_axes)) {
            stop("Drop axes is not supported yet")
          }
          out$set(out_selection, tmp)
        } else {
          # RawArray
          # Copies chunk by index directly into output. Doesn't matter if selection is contiguous
          # since store/output are different shapes/strides.
          #out$set(out_selection, private$chunk_buffer_to_raw_array(decoded_chunk), chunk_selection)
          stop("TODO: support out for chunk_getitem")
        }
      }, error = function(cond) {
        if(is_key_error(cond)) {
          # fill with scalar if cKey doesn't exist in store
          if(!is_na(private$fill_value)) {
            out$set(out_selection, as_scalar(private$fill_value))
          }
        } else {
          print(cond$message)
          stop("Different type of error - rethrow")
        }
      })
    },
    #' method_description
    #' TODO
    chunk_getitems = function(lchunk_coords, lchunk_selection, out, lout_selection, drop_axes = NA, fields = NA) {
      # TODO
    },
    #' method_description
    #' TODO
    chunk_setitem = function(chunk_coords, chunk_selection, value, fields = NA) {
      # Reference: https://github.com/gzuidhof/zarr.js/blob/15e3a3f00eb19f0133018fb65f002311ea53bb7c/src/core/index.ts#L625
      
      if (private$order == "C" && self$get_ndim() > 1) {
        stop("Setting content for arrays in C-order is not yet supported.")
      }

      # Obtain key for chunk storage
      chunk_key <- private$chunk_key(chunk_coords)

      dtype_constr = private$dtype$get_typed_array_ctr()
      chunk_size <- compute_size(private$chunks)

      if (is_total_slice(chunk_selection, private$chunks)) {
        # Totally replace chunk

        # Optimization: we are completely replacing the chunk, so no need
        # to access the existing chunk data

        if (is_scalar(value)) {
          chunk <- NestedArray$new(
            value,
            shape = private$chunks,
            dtype = private$dtype,
            order = private$order
          )
        }
        # value was already a NestedArray
        chunk_raw <- value$flatten_to_raw(order = private$order)
      } else {
        # partially replace the contents of this chunk

        # Existing chunk data
        chunk_nested_array <- tryCatch({

          # Chunk is initialized if this does not error
          chunk_store_data <- self$get_chunk_store()$get_item(chunk_key)
          dbytes <- private$decode_chunk(chunk_store_data)
          # From raw.
          NestedArray$new(
            dbytes,
            shape = private$chunks,
            dtype = private$dtype,
            order = private$order
          )
        }, error = function(cond) {
          if (is_key_error(cond)) {
            # Chunk is not initialized
            chunk_data <- dtype_constr(chunk_size)
            if (!is.null(private$fill_value)) { # TODO: should this be is.na
              chunk_fill(chunk_data, private$fill_value)
            }
            # From base R array.
            return(NestedArray$new(
              chunk_data,
              shape = private$chunks,
              dtype = private$dtype,
              order = private$order
            ))
          } else {
            print(cond$message)
            # // Different type of error - rethrow
            stop("throw error;")
          }
        })
        
        # Now that we have the existing chunk data,
        # we set the new value by using the chunk_selection
        # to specify which subset to replace.
        chunk_nested_array$set(chunk_selection, value)
        chunk_raw <- chunk_nested_array$flatten_to_raw(order = private$order)
      }
      # We encode the new chunk and set it in the chunk store.
      chunk_data <- private$encode_chunk(chunk_raw)
      self$get_chunk_store()$set_item(chunk_key, chunk_data)
    },
    #' method_description
    #' TODO
    chunk_setitem_nosync = function(chunk_coords, chunk_selection, value, fields = NA) {
      # TODO
    },
    #' method_description
    #' TODO
    chunk_setitems = function(lchunk_coords, lchunk_selection, values, fields = NA) {
      # TODO
    },
    #' method_description
    #' TODO
    process_for_setitem = function(ckey, chunk_selection, value, fields = NA) {
      # TODO
    },
    chunk_delitem = function(ckey) {
      # TODO
    },
    #' method_description
    #' TODO
    chunk_delitems = function(ckeys) {
      # TODO
    },
    #' method_description
    #' TODO
    decode_chunk = function(cdata, start = NA, nitems = NA, expected_shape = NA) {
      # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0e6cdc04c6413e14f57f61d389972ea937c/zarr/core.py#L2066
      # decompress
      if(!is_na(private$compressor)) {
        # TODO: only decode requested items
        # if (
        #     all(x is not None for x in [start, nitems])
        #     and self._compressor.codec_id == "blosc"
        # ) and hasattr(self._compressor, "decode_partial"):
        #     chunk = self._compressor.decode_partial(cdata, start, nitems)
        # else:
        chunk <- private$compressor$decode(cdata, self)
      } else {
        chunk <- cdata
      }

      # apply filters
      if(!is_na(private$filters)) {
        for (f in rev(private$filters)) {
          chunk <- f$decode(chunk)
        }
      }

      # TODO: view as numpy array with correct dtype
      # chunk <- ensure_ndarray(chunk)
      # special case object dtype, because incorrect handling can lead to
      # segfaults and other bad things happening
      # if self._dtype != object:
      #    chunk = chunk.view(self._dtype)
      # elif chunk.dtype != object:
          # If we end up here, someone must have hacked around with the filters.
          # We cannot deal with object arrays unless there is an object
          # codec in the filter chain, i.e., a filter that converts from object
          # array to something else during encoding, and converts back to object
          # array during decoding.
          # raise RuntimeError('cannot read object array without object codec')

      # ensure correct chunk shape
      return(chunk)
    },
    #' method_description
    #' TODO
    encode_chunk = function(chunk_as_raw) {
      # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0e6cdc04c6413e14f57f61d389972ea937c/zarr/core.py#L2105

      chunk <- chunk_as_raw

      # apply filters
      if(!is_na(private$filters)) {
        for(f in private$filters) {
          chunk <- f$encode(chunk)
        }
      }

      # TODO: check object encoding
      #if ensure_ndarray(chunk).dtype == object:
      #    raise RuntimeError('cannot write object array without object codec')

      # compress
      if(!is_na(private$compressor)) {
        cdata <- private$compressor$encode(chunk, self)
      } else {
        cdata <- chunk
      }

      # TODO: ensure in-memory data is immutable and easy to compare
      #if isinstance(self.chunk_store, MutableMapping):
      #    cdata = ensure_bytes(cdata)

      return(cdata)
    },
    #' method_description
    #' TODO
    append_nosync = function(data, axis = 0) {
      # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/core.py#L2141
      # TODO
    }
  ),
  public = list(
    #' @description
    #' Create a new ZarrArray instance.
    #' @param store Array store already initialized.
    #' @param path character path
    #' @param read_only logical read only?
    #' @param chunk_store TODO
    #' @param synchronizer TODO
    #' @param cache_metadata logical cache metadata?
    #' @param cache_attrs logical cache attributes?
    #' @param write_empty_chunks logical write empty chunks?
    #' @return An `Array` instance.
    initialize = function(store, path = NA, read_only = FALSE, 
                          chunk_store = NA, synchronizer = NA, 
                          cache_metadata = TRUE, cache_attrs = TRUE, 
                          write_empty_chunks = TRUE) {
      private$store <- store
      private$chunk_store <- chunk_store
      if(!is.na(path)) {
        private$path <- normalize_storage_path(path)
        private$key_prefix <- paste0(private$path, "/")
      } else {
        private$path <- NA
        private$key_prefix <- ""
      }
      private$read_only <- read_only
      private$synchronizer <- synchronizer
      private$cache_metadata <- cache_metadata
      private$cache_attrs <- cache_attrs
      private$is_view <- FALSE
      private$write_empty_chunks <- write_empty_chunks

      private$load_metadata()

      akey <- paste0(private$key_prefix, ATTRS_KEY)
      private$attrs <- Attributes$new(store, key = akey)

      private$vindex <- OIndex$new(self)
      private$oindex <- VIndex$new(self)
    },
    #' @description
    #' get store from array.
    get_store = function() {
      return(private$store)
    },    
    #' @description
    #' get array path
    get_path = function() {
      return(private$path)
    },
    #' @description
    #' get full array name
    get_name = function() {
      if(!is.na(private$path)) {
        name <- private$path
        name_vec <- str_to_vec(name)
        if(name_vec[1] != "/") {
          name <- paste0("/", name)
        }
        return(name)
      }
      return(NA)
    },
    #' @description
    #' get the basename of an array
    get_basename = function() {
      name <- self$get_name()
      if(!is.na(name)) {
        name_segments <- stringr::str_split(name, "/")[[1]]
        return(name_segments[length(name_segments)])
      }
      return(NA)
    },
    #' @description
    #' get the read only property of an array (TRUE/FALSE)
    get_read_only = function() {
      return(private$read_only)
    },
    #' @description
    #' set the read only property of an array
    #' @param val value to set
    set_read_only = function(val) {
      private$read_only <- val
    },
    #' @description
    #' get the chunk store for an array
    get_chunk_store = function() {
      if(is_na(private$chunk_store)) {
        return(private$store)
      } else {
        return(private$chunk_store)
      }
    },
    #' @description
    #' get the shape of an array
    get_shape = function() {
      private$refresh_metadata()
      return(private$shape)
    },
    #' @description
    #' set or reset the size of an array
    #' @param value numeric size to set
    set_shape = function(value) {
      self$resize(value)
    },
    #' @description
    #' Change the shape of the array by growing or shrinking one or more dimensions.
    #' @param ... arguments for do.call
    resize = function(...) {
      args <- list(...)
      do.call(private$resize_nosync, args)
    },
    #' @description
    #' get the chunk metadata of an array
    get_chunks = function() {
      return(private$chunks)
    },
    #' @description
    #' get the Dtype of an array
    get_dtype = function() {
      return(private$dtype)
    },
    #' @description
    #' get the compressor of an array
    get_compressor = function() {
      return(private$compressor)
    },
    #' @description
    #' get the fill value of an array
    get_fill_value = function() {
      return(private$fill_value)
    },
    #' @description
    #' set the fill value of an array
    #' @param val fill value to use
    set_fill_value = function(val) {
      private$fill_value <- val
      private$flush_metadata_nosync()
    },
    #' @description
    #' get the storage order metadata of an array.
    get_order = function() {
      return(private$order)
    },
    #' @description
    #' get the filters metadata of an array
    get_filters = function() {
      return(private$filters)
    },
    #' @description
    #' get the synchronizer of an array TODO: not implemented
    get_synchronizer = function() {
      return(private$synchronizer)
    },
    #' @description
    #' get attributes of array
    get_attrs = function() {
      return(private$attrs)
    },
    #' @description
    #' get number of dimensions of array
    get_ndim = function() {
      return(length(private$shape))
    },
    #' @description
    #' get size of an array TODO: not implemented
    get_size = function() {
      # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/core.py#L383
      # TODO
    },
    #' @description
    #' TODO: not implemented
    get_itemsize = function() {
      # TODO
    },
    #' @description
    #' get number of bytes of an array
    get_nbytes = function() {
      private$refresh_metadata()
      return(self$get_size() * self$get_itemsize())
    },
    #' @description
    #' TODO
    get_nbytes_stored = function() {
      # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/core.py#L413
      # TODO
    },
    #' @description
    #' TODO
    get_cdata_shape = function() {
      private$refresh_metadata()
      return(private$compute_cdata_shape())
    },
    #' @description
    #' TODO
    get_nchunks = function() {
      # TODO
    },
    #' @description
    #' TODO
    get_nchunks_initialized = function() {
      # TODO
    },
    #' @description
    #' get is_view metadata of array
    get_is_view = function() {
      return(private$is_view)
    },
    #' @description
    #' get orthogonal index of array
    get_oindex = function() {
      return(private$oindex)
    },
    #' @description
    #' get vectorized index of array
    get_vindex = function() {
      return(private$vindex)
    },
    #' @description
    #' get write empty chunks setting of array
    get_write_empty_chunks = function() {
      return(private$write_empty_chunks)
    },
    #' @description
    #' check if another object refers to the same array. does not check array data
    #' @param other other object to check
    equals = function(other) {
      return(all(c(
        class(other)[[1]] == "Array",
        # TODO: check store equality also
        private$read_only == other$get_read_only(),
        private$path == other$get_path(),
        !private$is_view
      )))
    },
    #' @description
    #' TODO
    #' @param start start of slice
    #' @param end end of slice
    islice = function(start = NA, end = NA) {
      # TODO
    },
    #' @description
    #' TODO
    length = function() {
      if(private$shape) {
        return(private$shape[1])
      } else {
        # 0-dimensional array, same error message as numpy
        stop("length of unized object")
      }
    },
    #' @description
    #' Subset the array.
    #' @returns A subset of the array, as a NestedArray instance.
    get_item = function(selection) {
      # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/core.py#L580
      # Reference: https://github.com/gzuidhof/zarr.js/blob/master/src/core/index.ts#L266
      
      if(is_pure_fancy_indexing(selection)){
        # TODO: implement vindex further for vectorized indexing
        stop("vectorized indexing is not supported yet")
        # return(self$get_vindex()$get_item(selection))
      } else {
        return(self$get_basic_selection(selection)) 
      }
    },
    #' @description
    #' get a selection of an array based on a "basic"list of slices

    #' @param out TODO
    #' @param fields TODO
    get_basic_selection = function(selection = NA, out = NA, fields = NA) {
      # Refresh metadata
      if(!private$cache_metadata) {
        private$load_metadata()
      }
      # Handle zero-dimensional arrays
      if(is.null(private$shape) || length(private$shape) == 0) {
        return(private$get_basic_selection_zd(selection, out = out, fields = fields))
      }
      return(private$get_basic_selection_nd(selection, out = out, fields = fields))
    },
    #' @description
    #' TODO

    #' @param out TODO
    #' @param fields TODO
    get_orthogonal_selection = function(selection = NA, out = NA, fields = NA) {
      
      # Refresh metadata
      if(!private$cache_metadata) {
        private$load_metadata()
      }
      
      indexer <- OrthogonalIndexer$new(selection, self)
      return(private$get_selection(indexer, out = out, fields = fields))
    },
    #' @description
    #' TODO

    #' @param out TODO
    #' @param fields TODO
    get_coordinate_selection = function(selection = NA, out = NA, fields = NA) {
      # TODO
    },
    #' @description
    #' TODO

    #' @param out TODO
    #' @param fields TODO
    get_mask_selection = function(selection = NA, out = NA, fields = NA) {
      # TODO
    },
    #' @description
    #' Set a subset of the array.

    #' @param value The value to set, as an R array() or a Zarr NestedArray instance.
    set_item = function(selection, value) {
      self$set_basic_selection(selection, value)
    },
    #' @description
    #' TODO

    #' @param value TODO
    #' @param fields TODO
    set_basic_selection = function(selection, value, fields = NA) {
      # Handle zero-dimensional arrays
      if(is.null(private$shape) || length(private$shape) == 0) {
        return(private$set_basic_selection_zd(selection, value = value, fields = fields))
      }
      return(private$set_basic_selection_nd(selection, value = value, fields = fields))
    },
    #' @description
    #' TODO

    #' @param value TODO
    #' @param fields TODO
    set_orthogonal_selection = function(selection, value, fields = NA) {
      # TODO
    },
    #' @description
    #' TODO

    #' @param value TODO
    #' @param fields TODO
    set_coordinate_selection = function(selection, value, fields = NA) {
      # TODO
    },
    #' @description
    #' TODO

    #' @param value TODO
    #' @param fields TODO
    set_mask_selection = function(selection, value, fields = NA) {
      # TODO
    },
    #' @description
    #' TODO
    get_info = function() {
      # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/core.py#L2141
      # TODO
    },
    #' @description
    #' TODO
    #' @param hashname name of hash
    get_digest = function(hashname = "sha1") {
      # TODO
    },
    #' @description
    #' TODO
    #' @param hashname name of hash
    get_hexdigest = function(hashname = "sha1") {
      # TODO
    },
    #' @description
    #' TODO
    #' @param data data to append
    #' @param axis axis to append
    append = function(data, axis = 0) {
      private$append_nosync(data, axis)
    },
    #' @description
    #' TODO
    #' @param shape TODO
    #' @param chunks TODO
    #' @param dtype TODO
    #' @param fill_value TODO
    #' @param filters TODO
    #' @param read_only TODO
    #' @param synchronizer TODO
    view = function(shape = NA, chunks = NA, dtype = NA, fill_value = NA, 
                    filters = NA, read_only = NA, synchronizer = NA) {
      # TODO
    },
    #' @description
    #' TODO
    #' @param dtype TODO
    astype = function(dtype) {
      # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/core.py#L2586
      # TODO
    },
    #' @description
    #' TODO
    get_dimension_separator = function() {
      return(private$dimension_separator)
    },
    #' @description
    #' Set values for a selection using bracket notation (for S3 method).
    #'
    #' @param ... Contains the slicing parameters, one for each dimension.
    #' Use empty space to get whole dimension e.g. \code{[1:5,,]}
    #'
    #' @return Sliced Zarr object
    #' @keywords internal
    `[` = function(...) {
      filters <- substitute(...())
      if(length(filters) != length(private$shape)) {
        stop("This Zarr object has ", length(private$shape), " dimensions, ", length(filters), " were supplied")
      }

      # update filters for orthogonal_selection
      filters <- manage_filters(filters)

      # return orthogonal selection upon `[.ZarrArray`
      return(self$get_orthogonal_selection(filters))
    },
    #' @description
    #' Assign values for a selection using bracket notation (for S3 method).
    #' @param ... Contains the slicing parameters, one for each dimension.
    #' Use empty space to get whole dimension e.g. \code{[1:5,,]}
    #' @keywords internal
    `[<-` = function(...) {
      stop("Assignment using bracket notation is not yet supported - use set_item() directly")
    },
    #' @description
    #' Convert Zarr object to R array (for S3 method). Note that this loads all data into memory.
    #'
    #' @return array
    as.array = function() {
      return(self$get_item("...")$data)
    }
  )
)

#' S3 method for custom bracket subsetting
#'
#' @param obj object
#' @param ... dots
#' @keywords internal 
#' @export
`[.ZarrArray` <- function(obj, ...) {
  obj$`[`(...)
}

#' S3 method for custom bracket assignment
#'
#' @param value array or ZarrArray
#' @keywords internal 
#' @export
`[<-.ZarrArray` <- function(obj, ..., value) {
  obj$`[<-`(value)
}

#' S3 method for as.array
#'
#' @param x object 
#' @param ... not used
#' @keywords internal
#' @export
as.array.ZarrArray = function(x, ...) {
  x$as.array()
}
