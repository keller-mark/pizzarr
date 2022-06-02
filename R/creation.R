contains_array <- function(store, path) {

}

contains_group <- function(store, path) {

}

init_array_metadata <- function(store, shape, chunks, dtype, compressor, fill_value, order, overwrite, path, chunk_store, filters, object_codec, dimension_separator) {
  # guard conditions
  if(overwrite) {
    # attempt to delete any pre-existing items in store
    rmdir(store, path)
    if(!is.na(chunk_store)) {
      rmdir(chunk_store, path)
    }
  } else if(contains_array(store, path)) {
    stop("ContainsArrayError")
  } else if(contains_group(store, path)) {
    stop("ContainsGroupError")
  }
}

init_group_metadata <- function() {

}

require_parent_group <- function(path, store, chunk_store, overwrite) {
  # assume path is normalized
  if(!is.na(path)) {
    segments <- str_split(path, "/", simplify = TRUE)
    for(i in 1:length(segments)) {
      p <- paste(segments[1:i], sep = "/")
      if(contains_array(store, p)) {
        init_group_metadata(store=store, path=p, chunk_store=chunk_store, overwrite=overwrite)
      } else if(!contains_group(store, p)) {
        init_group_metadata(store=store, path=p, chunk_store=chunk_store)
      }
    }
  }
}

init_array <- function(store, shape, chunks, dtype, compressor, fill_value, order, overwrite, path, chunk_store, filters, object_codec, dimension_separator) {
  require_parent_group(path = path, store = store, chunk_store = chunk_store, overwrite = overwrite)
  init_array_metadata(store=store, shape=shape, chunks=chunks, dtype=dtype,
                      compressor=compressor, fill_value=fill_value,
                      order=order, overwrite=overwrite, path=path,
                      chunk_store=chunk_store, filters=filters,
                      object_codec=object_codec,
                      dimension_separator=dimension_separator)
}

#' Create an array.
#'
#' @param shape Array shape.
#' @param chunks Chunk shape. If TRUE, will be guessed from `shape` and `dtype`. If FALSE, a single chunk will be used for the whole array. If an int, the chunk size in each dimension will be given by the value of chunks. By default, TRUE.
#' @param dtype Data type.
#' @param compressor The primary compressor.
#' @param fill_value Default value to use for uninitialized portions of the array.
#' @param order Memory layout to be used within each chunk.
#' @param store Store or path to directory in file system.
#' @param synchronizer Array synchronizer. Optional.
#' @param overwrite If TRUE, delete all pre-existing data in `store` at `path` before creating the array.
#' @param path Path under which the array is stored.
#' @param chunk_store Separate storage for chunks. If not provided, `store` will be used for storage of both chunks and metadata.
#' @param filters Sequence of filters to use to encode chunk data prior to compression.
#' @param cache_metadata If TRUE, array configuration metadata will be cached for the lifetime of the object. If FALSE, array metadata will be reloaded prior to all data access and modification operations.
#' @param cache_attrs If TRUE, user attributes will be cached for attribute read operations. If FALSE, user attributes are reloaded from the store prior to all attribute read operations.
#' @param read_only If TRUE, array is protected against modification.
#' @param object_codec A codec to encode object arrays, only needed if `dtype` is object.
#' @param dimension_separator Separator placed between the dimensions of a chunk.
#' @return Returns a pizzarr `Array` object.
create <- function(shape, chunks = TRUE, dtype = NA, compressor = NA, fill_value = 0, order = "C", store = NA, synchronizer = NA, overwrite = FALSE, path = NA, chunk_store = NA, filters = NA, cache_metadata = TRUE, cache_attrs = TRUE, read_only = FALSE, object_codec = NA, dimension_separator = NA) {

  stopifnot(!is.na(store))
  if(is.na(compressor)) {
    compressor <- Codec$new()
  }
  # Not using:
  # - dimension_separator: should be a property of the store, not a param here

  # initialize array metadata
  init_array(store=store, shape=shape, chunks=chunks, dtype=dtype, compressor=compressor,
             fill_value=fill_value, order=order, overwrite=overwrite, path=path,
             chunk_store=chunk_store, filters=filters, object_codec=object_codec,
             dimension_separator=dimension_separator)

  # instantiate array
  z <- Array$new(store=store, path=path, chunk_store=chunk_store, synchronizer=synchronizer,
                 cache_metadata=cache_metadata, cache_attrs=cache_attrs, read_only=read_only,
                 write_empty_chunks=write_empty_chunks)

  return(z)
}
