#' @keywords internal
path_to_prefix <- function(path) {
    # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0e6cdc04c6413e14f57f61d389972ea937c/zarr/_storage/store.py#L134
    # assume path already normalized
    if(!is.na(path) && stringr::str_length(path) > 0) {
        prefix <- paste0(path, '/')
    } else {
        prefix <- ""
    }
    return(prefix)
}

#' @keywords internal
contains_array <- function(store, path=NA) {
    # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0e6cdc04c6413e14f57f61d389972ea937c/zarr/storage.py#L91
    # Return True if the store contains an array at the given logical path.
    path <- normalize_storage_path(path)
    prefix <- path_to_prefix(path)
    key <- paste0(prefix, ARRAY_META_KEY)
    return(store$contains_item(key))
}

#' @keywords internal
contains_group <- function(store, path=NA) {
    # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0e6cdc04c6413e14f57f61d389972ea937c/zarr/storage.py#L99
    # Return True if the store contains a group at the given logical path.
    path <- normalize_storage_path(path)
    prefix <- path_to_prefix(path)
    key <- paste0(prefix, GROUP_META_KEY)
    return(store$contains_item(key))
}

#' @keywords internal
rmdir <- function(store, path=NA) {
    # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0e6cdc04c6413e14f57f61d389972ea937c/zarr/storage.py#L130

    # Remove all items under the given path. If `store` provides a `rmdir` method,
    # this will be called, otherwise will fall back to implementation via the
    # `Store` interface.

    path <- normalize_storage_path(path)
    if("rmdir" %in% names(store) && store$is_erasable()) {
        store$rmdir(path)
    }
}

#' @keywords internal
init_array_metadata <- function(
    store,
    shape,
    chunks=NA,
    dtype=NA,
    compressor=NA,
    fill_value=NA,
    order=NA,
    overwrite=FALSE,
    path=NA,
    chunk_store=NA,
    filters=NA,
    object_codec=NA,
    dimension_separator=NA
) {
    # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0e6cdc04c6413e14f57f61d389972ea937c/zarr/storage.py#L358
    if(is_na(compressor)) {
        compressor <- "default"
    }
    if(is.na(order)) {
        order <- "C"
    }

    # guard conditions
    if (overwrite) {
        # attempt to delete any pre-existing items in store
        store$rmdir(path)
        if(!is.na(chunk_store)) {
            chunk_store$rmdir(path)
        }
    } else if (contains_array(store, path)) {
        stop("ContainsArrayError(path)")
    } else if (contains_group(store, path)) {
        stop("ContainsGroupError(path)")
    }

    # normalize metadata
    dtype <- normalize_dtype(dtype)

    # object_codec <- normalize_object_codec(dtype, object_codec) # TODO

    # shape = normalize_shape(shape) + dtype.shape
    # dtype = dtype.base

    shape <- normalize_shape(shape)

    dtype_itemsize <- get_dtype_numbytes(dtype)
    chunks <- normalize_chunks(chunks, shape, dtype_itemsize)
    order <- normalize_order(order)
    fill_value <- normalize_fill_value(fill_value, dtype)

    # # optional array metadata
    # if dimension_separator is None:
    #     dimension_separator = getattr(store, "_dimension_separator", None)
    # dimension_separator = normalize_dimension_separator(dimension_separator)

    # # compressor prep
    # if shape == ():
    #     # no point in compressing a 0-dimensional array, only a single value
    #     compressor = None
    # elif compressor == 'none':
    #     # compatibility
    #     compressor = None
    if (compressor == "default") {
        compressor <- get_default_compressor()
    }

    # obtain compressor config
    compressor_config = NA
    if(!is_na(compressor)) {
        # TODO: wrap in try/catch
        # try
        compressor_config <- compressor$get_config()
        # catch
        #   stop("BadCompressorError(compressor)")
    }

    # obtain filters config
    filters_config <- list()
    if(!is_na(filters)) {
        for(f in filters) {
            append(filters_config, f$get_config())
        }
    }

    # TODO: deal with object encoding
    # if dtype.hasobject:
    #     if object_codec is None:
    #         if not filters:
    #             # there are no filters so we can be sure there is no object codec
    #             raise ValueError('missing object_codec for object array')
    #         else:
    #             # one of the filters may be an object codec, issue a warning rather
    #             # than raise an error to maintain backwards-compatibility
    #             warnings.warn('missing object_codec for object array; this will raise a '
    #                           'ValueError in version 3.0', FutureWarning)
    #     else:
    #         filters_config.insert(0, object_codec.get_config())
    # elif object_codec is not None:
    #     warnings.warn('an object_codec is only needed for object arrays')

    # use null to indicate no filters
    if (length(filters_config) == 0) {
        filters_config <- NA
    }

    # initialize metadata
    zarray_meta <- create_zarray_meta(
        shape=shape,
        chunks=chunks,
        dtype=dtype,
        compressor=compressor_config,
        fill_value=fill_value,
        order=order,
        filters=filters_config,
        dimension_separator=dimension_separator
    )
    key <- paste0(path_to_prefix(path), ARRAY_META_KEY)

    store$set_item(key, encode_array_meta(zarray_meta))
}

#' @keywords internal
init_group_metadata <- function(
    store,
    overwrite = FALSE,
    path = NA,
    chunk_store = NA
) {
    # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0e6cdc04c6413e14f57f61d389972ea937c/zarr/storage.py#L493
    
    # guard conditions
    if(overwrite) {
        # attempt to delete any pre-existing items in store
        store$rmdir(path)
        if(!is.na(chunk_store)) {
            chunk_store$rmdir(path)
        }
    } else if (contains_array(store, path)) {
        stop("ContainsArrayError(path)")
    } else if (contains_group(store, path)) {
        stop("ContainsGroupError(path)")
    }

    # initialize metadata
    # N.B., currently no metadata properties are needed, however there may
    # be in future
    zgroup_meta <- obj_list()
    key <- paste0(path_to_prefix(path), GROUP_META_KEY)

    store$set_item(key, json_to_raw(zgroup_meta))
}

#' @keywords internal
require_parent_group <- function(
    path,
    store,
    chunk_store,
    overwrite
) {
    # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0e6cdc04c6413e14f57f61d389972ea937c/zarr/storage.py#L206
    # assume path is normalized
    if(!is.na(path)) {
        segments <- stringr::str_split(path, "/")[[1]]
        for(i in seq_len(length(segments) - 1)) {
            p <- paste(segments[0:i], sep="/")
            if(contains_array(store, p)) {
                # Parent contained an array, so overwrite as group.
                init_group_metadata(store, path=p, chunk_store=chunk_store, overwrite=overwrite)
            } else if (!contains_group(store, p)) {
                init_group_metadata(store, path=p, chunk_store=chunk_store)
            }
        }
    }

} 



#' @description
#' Initialize an array store with the given configuration. Note that this is a low-level
#' function and there should be no need to call this directly from user code.
#' @param store : Store
#'     A mapping that supports string keys and bytes-like values.
#' @param shape : int or tuple of ints
#'     Array shape.
#' @param chunks : bool, int or tuple of ints, optional
#'     Chunk shape. If True, will be guessed from `shape` and `dtype`. If
#'     False, will be set to `shape`, i.e., single chunk for the whole array.
#' @param dtype : string or dtype, optional
#'     NumPy dtype.
#' @param compressor : Codec, optional
#'     Primary compressor.
#' @param fill_value : object
#'     Default value to use for uninitialized portions of the array.
#' @param order : {'C', 'F'}, optional
#'     Memory layout to be used within each chunk.
#' @param overwrite : bool, optional
#'     If True, erase all data in `store` prior to initialisation.
#' @param path : string, bytes, optional
#'     Path under which array is stored.
#' @param chunk_store : Store, optional
#'     Separate storage for chunks. If not provided, `store` will be used
#'     for storage of both chunks and metadata.
#' @param filters : sequence, optional
#'     Sequence of filters to use to encode chunk data prior to compression.
#' @param object_codec : Codec, optional
#'     A codec to encode object arrays, only needed if dtype=object.
#' @param dimension_separator : {'.', '/'}, optional
#'     Separator placed between the dimensions of a chunk.
#' @keywords internal
init_array <- function(
    store,
    shape,
    chunks = TRUE,
    dtype=NA,
    compressor=NA,
    fill_value=NA,
    order = NA,
    overwrite = FALSE,
    path = NA,
    chunk_store = NA,
    filters=NA,
    object_codec=NA,
    dimension_separator=NA
) {
    if(is_na(compressor)) {
        compressor <- "default"
    }
    if(is.na(order)) {
        order <- "C"
    }

    # normalize path
    path <- normalize_storage_path(path)

    # ensure parent group initialized
    require_parent_group(path, store=store, chunk_store=chunk_store, overwrite=overwrite)

    init_array_metadata(
        store,
        shape=shape,
        chunks=chunks,
        dtype=dtype,
        compressor=compressor,
        fill_value=fill_value,
        order=order,
        overwrite=overwrite,
        path=path,
        chunk_store=chunk_store,
        filters=filters,
        object_codec=object_codec,
        dimension_separator=dimension_separator
    )

}


#' @description
#' Create an array
#' @param shape : int or tuple of ints
#'     Array shape.
#' @param chunks : int or tuple of ints, optional
#'     Chunk shape. If True, will be guessed from `shape` and `dtype`. If
#'     False, will be set to `shape`, i.e., single chunk for the whole array.
#'     If an int, the chunk size in each dimension will be given by the value
#'     of `chunks`. Default is True.
#' @param dtype : string or dtype, optional
#'     NumPy dtype.
#' @param compressor : Codec, optional
#'     Primary compressor.
#' @param fill_value : object
#'     Default value to use for uninitialized portions of the array.
#' @param order : {'C', 'F'}, optional
#'     Memory layout to be used within each chunk.
#' @param store : MutableMapping or string
#'     Store or path to directory in file system or name of zip file.
#' @param synchronizer : object, optional
#'     Array synchronizer.
#' @param overwrite : bool, optional
#'     If True, delete all pre-existing data in `store` at `path` before
#'     creating the array.
#' @param path : string, optional
#'     Path under which array is stored.
#' @param chunk_store : MutableMapping, optional
#'     Separate storage for chunks. If not provided, `store` will be used
#'     for storage of both chunks and metadata.
#' @param filters : sequence of Codecs, optional
#     Sequence of filters to use to encode chunk data prior to compression.
#' @param cache_metadata : bool, optional
#'     If True, array configuration metadata will be cached for the
#'     lifetime of the object. If False, array metadata will be reloaded
#'     prior to all data access and modification operations (may incur
#'     overhead depending on storage and data access pattern).
#' @param cache_attrs : bool, optional
#'     If True (default), user attributes will be cached for attribute read
#'     operations. If False, user attributes are reloaded from the store prior
#'     to all attribute read operations.
#' @param read_only : bool, optional
#'     True if array should be protected against modification.
#' @param object_codec : Codec, optional
#'     A codec to encode object arrays, only needed if dtype=object.
#' @param dimension_separator : {'.', '/'}, optional
#'     Separator placed between the dimensions of a chunk.
#' @param write_empty_chunks : bool, optional
#'     If True (default), all chunks will be stored regardless of their
#'     contents. If False, each chunk is compared to the array's fill value
#'     prior to storing. If a chunk is uniformly equal to the fill value, then
#'     that chunk is not be stored, and the store entry for that chunk's key
#'     is deleted. This setting enables sparser storage, as only chunks with
#'     non-fill-value data are stored, at the expense of overhead associated
#'     with checking the data of each chunk.
#' @returns z : zarr.core.Array
create <- function(
    shape,
    chunks=TRUE,
    dtype=NA,
    compressor=NA,
    fill_value=NA,
    order=NA,
    store=NA,
    synchronizer=NA,
    overwrite=FALSE,
    path=NA,
    chunk_store=NA,
    filters=NA,
    cache_metadata=TRUE,
    cache_attrs=TRUE,
    read_only=FALSE,
    object_codec=NA,
    dimension_separator=NA,
    write_empty_chunks=TRUE
) {
    # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0e6cdc04c6413e14f57f61d389972ea937c/zarr/creation.py#L18
    if(is_na(compressor)) {
        compressor <- "default"
    }
    if(is.na(fill_value)) {
        fill_value <- 0
    }
    if(is.na(order)) {
        order <- "C"
    }

    # handle polymorphic store arg
    store <- normalize_store_arg(store)

    # initialize array metadata
    init_array(store, shape=shape, chunks=chunks, dtype=dtype, compressor=compressor,
               fill_value=fill_value, order=order, overwrite=overwrite, path=path,
               chunk_store=chunk_store, filters=filters, object_codec=object_codec,
               dimension_separator=dimension_separator)

    
    # instantiate array
    z <- Array$new(
        store,
        path=path,
        chunk_store=chunk_store,
        synchronizer=synchronizer,
        cache_metadata=cache_metadata,
        cache_attrs=cache_attrs,
        read_only=read_only,
        write_empty_chunks=write_empty_chunks
    )
    return(z)
}

#' @description
#' Create an array filled with NAs.
#' @param shape : int or tuple of ints
#' @param ... The params of create()
#' @returns Array
empty <- function(shape, ...) {
    return(create(shape=shape, fill_value=NA, ...))
}

#' @description
#' Create an array filled with zeros.
#' @param shape : int or tuple of ints
#' @param ... The params of create()
#' @returns Array
zeros <- function(shape, ...) {
    return(create(shape=shape, fill_value=0, ...))
}
