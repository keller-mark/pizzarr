is_pure_fancy_indexing <- function(selection, ndim) {
    
  if(ndim == 1){
    if(length(selection) > 1){
      return(TRUE)
    }
  } 
  
  # check if there are any slice objects 
  no_slicing <- (length(selection) == ndim) & !(any(sapply(selection, function(s) inherits(s, "Slice"))))
  
  # return
  return(no_slicing)
}


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
    #' @param array array
    #' @return An `OIndex` instance.
    initialize = function(array) {
      self$array <- array
    },
    #' @description
    #' get method for the Oindex instance
    #' @param selection selection
    #' @return An `OIndex` instance.
    get_item = function(selection) {
      # self$array <- array$get
      self$array$get_orthogonal_selection(selection)
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
    #' @param array array
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
                               #' @param dim_sel integer dimention selection
                               #' @param dim_len integer dimension length
                               #' @param dim_chunk_len integer dimension chunk length
                               #' @return A `IntDimIndexer` instance.
                               initialize = function(dim_sel, dim_len, dim_chunk_len) {
                                 
                                 # Normalize
                                 dim_sel <- normalize_integer_selection(dim_sel, dim_len)
                                 
                                 self$dim_sel <- dim_sel
                                 self$dim_len <- dim_len
                                 self$dim_chunk_len <- dim_chunk_len
                                 self$num_items <- 1
                               },
                               #' @description 
                               #' TODO
                               #' @return a `ChunkDimProjection` instance
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
    #' @field dim_len dimension length
    #' @keywords internal
    dim_len = NULL,
    #' @field dim_chunk_len dimension chunk length
    #' @keywords internal
    dim_chunk_len = NULL,
    #' @field num_chunks number of chunks
    #' @keywords internal
    num_chunks = NULL,
    #' @field start start
    #' @keywords internal
    start = NULL,
    #' @field stop stop
    #' @keywords internal
    stop = NULL,
    #' @field step step
    #' @keywords internal
    step = NULL,
    #' @description
    #' Create a new SliceDimIndexer instance.
    #' @param dim_sel integer dimention selection
    #' @param dim_len integer dimension length
    #' @param dim_chunk_len integer dimension chunk length
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
    #' @description 
    #' TODO
    #' @return TODO
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
#'  An indexer class to normalize a selection of an array and provide an iterator 
#'  of indexes over the dimensions of an array.
#' @param selection selection as with ZarrArray, scalar, string, or Slice. "..." and ":" supported for string 
#' @param array ZarrArray object that will be indexed
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
        # if(is_int(dim_sel)) {
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
    #' @description 
    #'   An iterator over the dimensions of an array
    #' @return A list of ChunkProjection objects
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

# Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0e6cdc04c6413e14f57f61d389972ea937c/zarr/indexing.py#L585

#' The Zarr OrthogonalIndexer class.
#' @title OrthogonalIndexer Class
#' @docType class
#' @description
#'  An indexer class to normalize a selection of an array and provide an iterator 
#'  of indexes over the dimensions of an array.
#' @param selection selection as with ZarrArray, scalar, string, or Slice. "..." and ":" supported for string 
#' @param array ZarrArray object that will be indexed
#' @rdname OrthogonalIndexer
#' @keywords internal
OrthogonalIndexer <- R6::R6Class("OrthogonalIndexer",
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
                                
                                # Normalize
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
                                  
                                  if(length(dim_sel) == 1) {
                                    dim_indexer <- IntDimIndexer$new(dim_sel, dim_len, dim_chunk_len)
                                  } else if(is_slice(dim_sel)) {
                                    dim_indexer <- SliceDimIndexer$new(dim_sel, dim_len, dim_chunk_len)
                                  } else if(length(dim_sel) > 1) {
                                    dim_indexer <- IntArrayDimIndexer$new(dim_sel, dim_len, dim_chunk_len)
                                  } else if(is_slice(dim_sel)) {
                                    dim_indexer <- BoolArrayDimIndexer$new(dim_sel, dim_len, dim_chunk_len)
                                  } else {
                                    stop('Unsupported selection item for basic indexing, expected integer, slice, vector of integer or boolean')
                                  }
                                  dim_indexers <- append(dim_indexers, dim_indexer)
                                }
                                self$shape <- list()
                                for(d in dim_indexers) {
                                  if(class(d)[[1]] != "IntDimIndexer") {
                                    self$shape <- append(self$shape, d$num_items)
                                  }
                                }
                                self$drop_axes <- NA
                                
                                self$dim_indexers <- dim_indexers
                                
                              },
                              #' @description 
                              #'   An iterator over the dimensions of an array
                              #' @return A list of ChunkProjection objects
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
                                    # chunk_sel <- append(chunk_sel, p$dim_chunk_sel)
                                    chunk_sel <- append(chunk_sel, list(p$dim_chunk_sel))
                                    if(!is_na(p$dim_out_sel)) {
                                      # out_sel <- append(out_sel, p$dim_out_sel)
                                      out_sel <- append(out_sel, list(p$dim_out_sel))
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

# Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0e6cdc04c6413e14f57f61d389972ea937c/zarr/indexing.py#L424

#' The Order class.
#' @title Order Class
#' @docType class
#' @description
#'  TODO
#' @rdname Order
#' @keywords internal
Order <- R6::R6Class("Order",
                     public = list(
                       #' @field UNKNOWN UNKNOWN
                       #' @keywords internal
                       UNKNOWN = 0,
                       #' @field INCREASING INCREASING
                       #' @keywords internal
                       INCREASING = 1,
                       #' @field DECREASING DECREASING
                       #' @keywords internal
                       DECREASING = 2,
                       #' @field UNORDERED UNORDERED
                       #' @keywords internal
                       UNORDERED = 3,
                       #' @description
                       #' checking order of numbers
                       #' @param a vector of numbers
                       check = function(a){
                         diff_a <- diff(a)
                         diff_positive <- diff_a >= 0
                         n_diff_positive <- sum(diff_positive)
                         all_increasing <- n_diff_positive == length(diff_positive)
                         any_increasing <- n_diff_positive > 0
                         if(all_increasing){
                           return(Order$public_fields$INCREASING)
                         } else if(any_increasing) {
                           return(Order$public_fields$UNORDERED)
                         } else{
                           return(Order$public_fields$DECREASING)
                         }
                       })
                     )

# Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0e6cdc04c6413e14f57f61d389972ea937c/zarr/indexing.py#L457

#' The Zarr IntArrayDimIndexer class.
#' @title IntArrayDimIndexer Class
#' @docType class
#' @description
#'  TODO
#' @rdname IntArrayDimIndexer
#' @keywords internal
IntArrayDimIndexer <- R6::R6Class("IntArrayDimIndexer",
                               inherit = DimIndexer,
                               public = list(
                                 #' @field dim_len dimension length
                                 #' @keywords internal
                                 dim_len = NULL,
                                 #' @field dim_chunk_len dimension chunk length
                                 #' @keywords internal
                                 dim_chunk_len = NULL,
                                 #' @field num_chunks number of chunks
                                 #' @keywords internal
                                 num_chunks = NULL,
                                 #' @field dim_sel selection on dimension
                                 #' @keywords internal
                                 dim_sel = NULL,
                                 #' @field dim_out_sel TODO
                                 #' @keywords internal
                                 dim_out_sel = NULL,
                                 #' @field order order
                                 #' @keywords internal
                                 order = NULL,
                                 #' @field chunk_nitems number of items per chunk
                                 #' @keywords internal
                                 chunk_nitems = NULL, 
                                 #' @field dim_chunk_ixs chunks that should be visited
                                 #' @keywords internal
                                 dim_chunk_ixs = NULL,
                                 #' @field chunk_nitems_cumsum offsets into the output array
                                 #' @keywords internal
                                 chunk_nitems_cumsum = NULL, 
                                 #' @description
                                 #' Create a new IntArrayDimIndexer instance.
                                 #' @param dim_sel integer dimention selection
                                 #' @param dim_len integer dimension length
                                 #' @param dim_chunk_len integer dimension chunk length
                                 #' @param sel_order order
                                 #' @return A `IntArrayDimIndexer` instance.
                                 initialize = function(dim_sel, dim_len, dim_chunk_len, sel_order = Order$public_fields$UNKNOWN) {

                                   # Normalize
                                   dim_sel <- sapply(dim_sel, normalize_integer_selection, dim_len = dim_len)
                                   self$dim_sel <- dim_sel
                                   
                                   # store attributes 
                                   self$dim_len <- dim_len
                                   self$dim_chunk_len <- dim_chunk_len
                                   self$num_items <- length(dim_sel)
                                   self$num_chunks <- ceiling(self$dim_len / self$dim_chunk_len)
                                   
                                   dim_sel_chunk <- ceiling(dim_sel / dim_chunk_len)
                                   
                                   # determine order of indices
                                   if(sel_order == Order$public_fields$UNKNOWN)
                                     sel_order <- Order$public_methods$check(dim_sel)
                                   self$order <- sel_order
                                   
                                   if(self$order == Order$public_fields$INCREASING){
                                     self$dim_sel <-  dim_sel
                                   } else if(self$order == Order$public_fields$DECREASING) {
                                     self$dim_sel = rev(dim_sel)
                                     # self$dim_out_sel = rev(seq(1,self$num_items))
                                     self$dim_out_sel = rev(seq(0,self$num_items-1)) # Python based indexing
                                   } else {
                                     # sort indices to group by chunk
                                     self$dim_out_sel = order(dim_sel_chunk)
                                     self$dim_sel <- dim_sel[self$dim_out_sel]
                                     self$dim_out_sel <- self$dim_out_sel - 1 # Python based indexing
                                   }
                                   
                                   # precompute number of selected items for each chunk
                                   self$chunk_nitems <- tabulate(dim_sel_chunk, nbins = self$num_chunks)
                                   
                                   # find chunks that we need to visit
                                   self$dim_chunk_ixs = which(self$chunk_nitems != 0)
                                   
                                   # compute offsets into the output array
                                   self$chunk_nitems_cumsum = cumsum(self$chunk_nitems)
                                   
                                 },
                                 #' @description 
                                 #'   An iterator over the dimensions of an array
                                 #' @return A list of ChunkProjection objects
                                 iter = function() {
                                   
                                   # Iterate over chunks in range
                                   result <- list()
                                   for(dim_chunk_ix in self$dim_chunk_ixs) {
                                     
                                     # find region in output
                                     # if (dim_chunk_ix == 0) {
                                     if (dim_chunk_ix == 1) {
                                       start <- 0
                                     } else {
                                       start <- self$chunk_nitems_cumsum[dim_chunk_ix - 1]
                                     }
                                     stop <- self$chunk_nitems_cumsum[dim_chunk_ix]
                                     
                                     # START R-SPECIFIC
                                     if(start == stop) {
                                       stop <- stop + 1
                                     }
                                     # END R-SPECIFIC
                                     
                                     if (self$order == Order$public_fields$INCREASING) {
                                       dim_out_sel <- seq(start, stop - 1)
                                     } else {
                                       dim_out_sel <- self$dim_out_sel[(start + 1):stop]
                                     }
                                     # dim_out_sel <- self$dim_out_sel[(start + 1):stop]
                                     
                                     # START R-SPECIFIC
                                     dim_chunk_ix <- dim_chunk_ix - 1
                                     # END R-SPECIFIC
                                     
                                     # find region in chunk
                                     dim_offset <- dim_chunk_ix * self$dim_chunk_len
                                     # dim_chunk_sel <- self$dim_sel[(start + 1):stop] - dim_offset 
                                     # dim_chunk_sel <- self$dim_sel[(start + 1):stop] - dim_offset + 1
                                     dim_chunk_sel <- self$dim_sel[(start + 1):stop] - dim_offset - 1
                                     
                                     # # START R-SPECIFIC
                                     # dim_chunk_ix <- dim_chunk_ix - 1
                                     # # END R-SPECIFIC

                                     result <- append(result, ChunkDimProjection$new(
                                       dim_chunk_ix,
                                       dim_chunk_sel,
                                       dim_out_sel
                                     ))
                                   }
                                   return(result)
                                 }
                               )
)
