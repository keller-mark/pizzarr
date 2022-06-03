#' The Zarr OIndex class.
#' @title OIndex Class
#' @docType class
#' @description
#' @keywords internal
#' Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/indexing.py#L655
#'
#' @rdname OIndex
#' @export
OIndex <- R6::R6Class("OIndex",
  public = list(
    #' @field array
    #' @keywords internal
    array = NULL,
    #' @description
    #' Create a new OIndex instance.
    #' @return An `OIndex` instance.
    initialize = function(array) {
      self$array <- array
    }
  )
)

#' The Zarr VIndex class.
#' @title VIndex Class
#' @docType class
#' @description
#' @keywords internal
#' Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/indexing.py#L811
#'
#' @rdname VIndex
#' @export
VIndex <- R6::R6Class("VIndex",
  public = list(
    #' @field array
    #' @keywords internal
    array = NULL,
    #' @description
    #' Create a new VIndex instance.
    #' @return A `VIndex` instance.
    initialize = function(array) {
      self$array <- array
    }
  )
)

#' The Zarr IntDimIndexer class.
#' @title IntDimIndexer Class
#' @docType class
#' @description
#' @keywords internal
#' Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/indexing.py#L138
#'
#' @rdname IntDimIndexer
#' @export
IntDimIndexer <- R6::R6Class("IntDimIndexer",
  public = list(
    #' @field dim_sel
    #' @keywords internal
    dim_sel = NULL,
    #' @field dim_len
    #' @keywords internal
    dim_len = NULL,
    #' @field dim_chunk_len
    #' @keywords internal
    dim_chunk_len = NULL,
    #' @field nitems
    #' @keywords internal
    nitems = NULL,
    #' @description
    #' Create a new IntDimIndexer instance.
    #' @return A `IntDimIndexer` instance.
    initialize = function(dim_sel, dim_len, dim_chunk_len) {
      # Normalize
      dim_sel <- normalize_integer_selection(dim_sel, dim_len)

      self$dim_sel <- dim_sel
      self$dim_len <- dim_len
      self$dim_chunk_len <- dim_chunk_len
      self$nitems <- 1
    }
  )
)

#' The Zarr SliceDimIndexer class.
#' @title SliceDimIndexer Class
#' @docType class
#' @description
#' @keywords internal
#' Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/indexing.py#L163
#'
#' @rdname SliceDimIndexer
#' @export
SliceDimIndexer <- R6::R6Class("SliceDimIndexer",
  public = list(
    #' @field array
    #' @keywords internal
    array = NULL,
    #' @description
    #' Create a new SliceDimIndexer instance.
    #' @return A `SliceDimIndexer` instance.
    initialize = function(dim_sel, dim_len, dim_chunk_len) {
      # Reference: https://github.com/gzuidhof/zarr.js/blob/292804/src/core/indexing.ts#L311
      si <- slice_indices(dim_sel, dim_len)
      self$start <- si$start
      self$stop <- si$stop
      self$step <- si$step
      if(self$step < 1) {
        stop('NegativeStepError')
      }
      # TODO
    }
  )
)

#' The Zarr BasicIndexer class.
#' @title BasicIndexer Class
#' @docType class
#' @description
#' @keywords internal
#' Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/indexing.py#L326
#'
#' @rdname BasicIndexer
#' @export
BasicIndexer <- R6::R6Class("BasicIndexer",
  public = list(
    #' @field dim_indexers
    #' @keywords internal
    dim_indexers = NULL,
    #' @field shape
    #' @keywords internal
    shape = NULL,
    #' @field drop_axes
    #' @keywords internal
    drop_axes = NULL,
    #' @description
    #' Create a new VIndex instance.
    #' @return A `VIndex` instance.
    initialize = function(selection, array) {
      shape <- array$get_shape()
      chunks <- array$get_chunks()
      #selection <- replace_ellipsis(selection, shape) # TODO
      
      # Setup per-dimension indexers
      dim_indexers <- list()
      for(i in seq_len(length(selection))) {
        dim_sel <- selection[i]
        dim_len <- shape[i]
        dim_chunk_len <- chunks[i]

        if(is.null(dim_sel)) {
          dim_sel <- slice(NA)
        }

        if(!is.list(dim_sel)) {
          dim_indexer <- IntDimIndexer$new(dim_sel, dim_len, dim_chunk_len)
        } else if(is.list(dim_sel)) {
          dim_indexer <- SliceDimIndexer$new(dim_sel, dim_len, dim_chunk_len)
        } else {
          stop('Unsupported selection item for basic indexing, expected integer or slice')
        }
        dim_indexers <- append(dim_indexers, dim_indexer)
      }
      self$shape <- list()
      for(d in dim_indexers) {
        if(class(d)[[1]] == "SliceDimIndexer") {
          self$shape <- append(self$shape, d$num_items)
        }
      }
      self$drop_axes <- NA

      self$dim_indexers <- dim_indexers
    }
  )
)
