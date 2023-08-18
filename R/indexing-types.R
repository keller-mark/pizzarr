# Reference: https://github.com/gzuidhof/zarr.js/blob/master/src/core/types.ts

# Mapping from chunk to output array for a single dimension.
#' @keywords internal
ChunkDimProjection <- R6::R6Class("ChunkDimProjection",
  public = list(
    # Index of chunk
    dim_chunk_index = NULL,
    # Selection of items from chunk array
    dim_chunk_sel = NULL,
    # Selection of items in target (output) array
    dim_out_sel = NULL,
    initialize = function(dim_chunk_index, dim_chunk_sel, dim_out_sel) {
      self$dim_chunk_index <- dim_chunk_index
      self$dim_chunk_sel <- dim_chunk_sel
      self$dim_out_sel <- dim_out_sel
    }
  )
)

# A mapping of items from chunk to output array. Can be used to extract items from the
# chunk array for loading into an output array. Can also be used to extract items from a
# value array for setting/updating in a chunk array.
#' @keywords internal
ChunkProjection <- R6::R6Class("ChunkProjection",
  public = list(
    # Indices of chunk.
    chunk_coords = NULL,
    # Selection of items from chunk array.
    chunk_sel = NULL,
    # Selection of items in target (output) array.
    out_sel = NULL,
    initialize = function(chunk_coords, chunk_sel, out_sel) {
      self$chunk_coords <- chunk_coords
      self$chunk_sel <- chunk_sel
      self$out_sel <- out_sel
    }
  )
)

#' @keywords internal
Indexer <- R6::R6Class("Indexer",
  public = list(
    shape = NULL,
    drop_axes = NULL,
    iter = function() {
      # To be implemented in child class
    }
  )
)

#' @keywords internal
DimIndexer <- R6::R6Class("DimIndexer",
  public = list(
    num_items = NULL,
    iter = function() {
      # To be implemented in child class
    }
  )
)