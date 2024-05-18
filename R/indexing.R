# Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/indexing.py#L655

#' The Zarr OIndex class.
#' @title OIndex Class
#' @docType class
#' @description
#'  TODO
#' @rdname OIndex
#' @keywords internal
OIndex <- R6::R6Class("OIndex",
  public = list(
    #' @field array array
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

# Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/indexing.py#L811

#' The Zarr VIndex class.
#' @title VIndex Class
#' @docType class
#' @description
#'  TODO
#' @rdname VIndex
#' @keywords internal
VIndex <- R6::R6Class("VIndex",
  public = list(
    #' @field array array
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

# Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/indexing.py#L138

#' The Zarr IntDimIndexer class.
#' @title IntDimIndexer Class
#' @docType class
#' @description
#'  TODO
#' @rdname IntDimIndexer
#' @keywords internal
IntDimIndexer <- R6::R6Class("IntDimIndexer",
  inherit = DimIndexer,
  public = list(
    #' @field dim_sel TODO
    #' @keywords internal
    dim_sel = NULL,
    #' @field dim_len TODO
    #' @keywords internal
    dim_len = NULL,
    #' @field dim_chunk_len TODO
    #' @keywords internal
    dim_chunk_len = NULL,
    #' @description
    #' Create a new IntDimIndexer instance.
    #' @return A `IntDimIndexer` instance.
    initialize = function(dim_sel, dim_len, dim_chunk_len) {
      # Normalize
      dim_sel <- normalize_integer_selection(dim_sel, dim_len)

      self$dim_sel <- dim_sel
      self$dim_len <- dim_len
      self$dim_chunk_len <- dim_chunk_len
      self$num_items <- 1
    },
    iter = function() {
      # TODO: use generator/yield features from async package
      dim_chunk_index <- floor(self$dim_sel / self$dim_chunk_len)
      dim_offset <- dim_chunk_index * self$dim_chunk_len
      dim_chunk_sel <- self$dim_sel - dim_offset
      dim_out_sel <- NA
      return(list(
        ChunkDimProjection$new(
          dim_chunk_index,
          dim_chunk_sel,
          dim_out_sel
        )
      ))
    }
  )
)

# Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/indexing.py#L163

#' The Zarr SliceDimIndexer class.
#' @title SliceDimIndexer Class
#' @docType class
#' @description
#'  TODO
#' @rdname SliceDimIndexer
#' @keywords internal
SliceDimIndexer <- R6::R6Class("SliceDimIndexer",
  inherit = DimIndexer,
  public = list(
    dim_len = NULL,
    dim_chunk_len = NULL,
    num_chunks = NULL,
    start = NULL,
    stop = NULL,
    step = NULL,
    
    #' @description
    #' Create a new SliceDimIndexer instance.
    #' @return A `SliceDimIndexer` instance.
    initialize = function(dim_sel, dim_len, dim_chunk_len) {
      # Reference: https://github.com/gzuidhof/zarr.js/blob/292804/src/core/indexing.ts#L311
      si <- dim_sel$indices(dim_len)
      self$start <- si[1]
      self$stop <- si[2]
      self$step <- si[3]
      if(self$step < 1) {
        stop('NegativeStepError')
      }
      self$dim_len <- dim_len
      self$dim_chunk_len <- dim_chunk_len
      self$num_items <- max(0, ceiling((self$stop - self$start) / self$step))
      self$num_chunks <- ceiling(self$dim_len / self$dim_chunk_len)
    },
    iter = function() {
      # TODO: use generator/yield features from async package
      dim_chunk_index_from <- floor(self$start / self$dim_chunk_len)
      dim_chunk_index_to <- ceiling(self$stop / self$dim_chunk_len)

      # START R-SPECIFIC
      if(dim_chunk_index_from == dim_chunk_index_to) {
        dim_chunk_index_to <- dim_chunk_index_to + 1
      }
      # END R-SPECIFIC

      # Iterate over chunks in range
      result <- list()
      for(dim_chunk_index in seq(from = dim_chunk_index_from, to = (dim_chunk_index_to - 1), by = 1)) {

        # Compute offsets for chunk within overall array
        dim_offset <- dim_chunk_index * self$dim_chunk_len
        dim_limit <- min(self$dim_len, (dim_chunk_index + 1) * self$dim_chunk_len)

        # Determine chunk length, accounting for trailing chunk
        dim_chunk_len <- dim_limit - dim_offset

        dim_chunk_sel_start <- 0
        dim_chunk_sel_stop <- 0
        dim_out_offset <- 0

        if(self$start < dim_offset) {
          # Selection starts before the current chunk

          dim_chunk_sel_start <- 0
          remainder <- (dim_offset - self$start) %% self$step
          if(remainder > 0) {
            dim_chunk_sel_start <- dim_chunk_sel_start + (self$step - remainder)
          }
          # Compute number of previous items, provides offset into output array
          dim_out_offset <- ceiling((dim_offset - self$start) / self$step)
        } else {
          # Selection starts within the current chunk
          dim_chunk_sel_start <- self$start - dim_offset
          dim_out_offset <- 0
        }

        if(self$stop > dim_limit) {
          # Selection ends after current chunk
          dim_chunk_sel_stop <- self$dim_chunk_len
        } else {
          # Selection ends within current chunk
          dim_chunk_sel_stop <- self$stop - dim_offset
        }

        dim_chunk_sel <- zb_slice(dim_chunk_sel_start, dim_chunk_sel_stop, self$step)
        dim_chunk_num_items <- ceiling((dim_chunk_sel_stop - dim_chunk_sel_start) / self$step)
        dim_out_sel <- zb_slice(dim_out_offset, dim_out_offset + dim_chunk_num_items)

        result <- append(result, ChunkDimProjection$new(
          dim_chunk_index,
          dim_chunk_sel,
          dim_out_sel
        ))
      }
      return(result)
    }
  )
)

# Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/indexing.py#L326

#' The Zarr BasicIndexer class.
#' @title BasicIndexer Class
#' @docType class
#' @description
#'  TODO
#' @rdname BasicIndexer
#' @keywords internal
BasicIndexer <- R6::R6Class("BasicIndexer",
  inherit = Indexer,
  public = list(
    #' @field dim_indexers TODO
    #' @keywords internal
    dim_indexers = NULL,
    #' @description
    #' Create a new VIndex instance.
    #' @return A `VIndex` instance.
    initialize = function(selection, array) {
      shape <- array$get_shape()
      chunks <- array$get_chunks()

      selection <- normalize_list_selection(selection, shape)
      
      # Setup per-dimension indexers
      dim_indexers <- list()
      for(i in seq_along(selection)) {
        dim_sel <- selection[[i]]
        dim_len <- shape[i]
        dim_chunk_len <- chunks[i]

        if(is.null(dim_sel)) {
          dim_sel <- zb_slice(NA)
        }

        if(is_integer(dim_sel)) {
          dim_indexer <- IntDimIndexer$new(dim_sel, dim_len, dim_chunk_len)
        } else if(is_slice(dim_sel)) {
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
    },
    iter = function() {
      # TODO: use generator/yield features from async package
      result <- list()

      # dim_indexers is a list of DimIndexer objects.
      # dim_indexer_iterables is a list (one per dimension)
      # of lists of IntDimIndexer or SliceDimIndexer objects.
      dim_indexer_iterables <- lapply(self$dim_indexers, function(di) di$iter())
      dim_indexer_product <- get_list_product(dim_indexer_iterables)


      for(row_i in seq_len(length(dim_indexer_product))) {
        dim_proj <- dim_indexer_product[[row_i]]

        chunk_coords <- list()
        chunk_sel <- list()
        out_sel <- list()

        if(!is.list(dim_proj)) {
          dim_proj <- list(dim_proj)
        }

        for(p in dim_proj) {
          chunk_coords <- append(chunk_coords, p$dim_chunk_index)
          chunk_sel <- append(chunk_sel, p$dim_chunk_sel)
          if(!is_na(p$dim_out_sel)) {
            out_sel <- append(out_sel, p$dim_out_sel)
          }
        }

        result <- append(result, ChunkProjection$new(
          chunk_coords,
          chunk_sel,
          out_sel
        ))
      }

      return(result)
    }
  )
)
