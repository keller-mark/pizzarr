# Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/attrs.py#L7

#' The Zarr Attributes class.
#' @title Attributes Class
#' @docType class
#' @description
#' Class providing access to user attributes on an array or group.
#'
#' @rdname Attributes
#' @export
Attributes <- R6::R6Class("Attributes",
  private = list(

    cached_aslist = NULL,

    get_nosync = function() {
      attrs_list <- tryCatch({
        return(self$store$metadata_class$decode_metadata(self$store$get_item(self$key), auto_unbox = TRUE))
      }, error = function(cond) {
        if(is_key_error(cond)) {
          return(obj_list())
        }
        stop(cond)
      })
      return(attrs_list)
    },
    put_nosync = function(d) {
      self$store$set_item(self$key, self$store$metadata_class$encode_metadata(d, auto_unbox = TRUE))
      if(self$cache) {
        private$cached_aslist <- d
      }
    },
    set_item_nosync = function(item, value) {
      d <- private$get_nosync()
      d[[item]] <- value
      private$put_nosync(d)
    },
    del_item_nosync = function(item) {
      d <- private$get_nosync()
      d[[item]] <- NULL
      private$put_nosync(d)
    }
  ),
  public = list(
    #' @field store Attributes store, already initialized.
    store = NULL,
    #' @field key The key under which the attributes will be stored.
    key = NULL,
    #' @field read_only If True, attributes cannot be modified.
    read_only = NULL,
    #' @field cache If True (default), attributes will be cached locally.
    cache = NULL,
    #' @field synchronizer Only necessary if attributes may be modified from multiple threads or processes.
    synchronizer = NULL,
    #' @description
    #' Create a new Attributes instance.
    #' @param store Attributes store, already initialized.
    #' @param key description key to use for attributes (.attrs is default)
    #' @param read_only logical
    #' @param cache logical
    #' @param synchronizer object
    #' @return An `Attributes` instance.
    initialize = function(store, key = NA, read_only = FALSE, cache = TRUE, synchronizer = NA) {
      if(is_na(key)) {
        key <- ATTRS_KEY
      }
      self$store <- store
      self$key <- key
      self$read_only <- read_only
      self$cache <- cache
      private$cached_aslist <- NA
      self$synchronizer <- synchronizer
    },
    #' @description
    #' convert attributes to list
    #' @return list
    to_list = function() {
      if(self$cache && !is_na(private$cached_aslist)) {
        return(private$cached_aslist)
      }
      d <- private$get_nosync()
      if(self$cache) {
        private$cached_aslist <- d
      }
      return(d)
    },
    #' @description
    #' refresh attributes
    #' @return None
    refresh = function() {
      if(self$cache) {
        new_val <- private$get_nosync()
        
        private$cached_aslist <- new_val
      }
    },
    #' @description
    #' check if object contains item
    #' @param x object
    #' @return logical
    contains = function(x) {
      return(x %in% names(self$to_list()))
    },
    #' @description
    #' get attribute
    #' @param item character
    #' @return item as list
    get_item = function(item) {
      return(self$to_list()[[item]])
    },
    #' @description
    #' set attribute
    #' @param item character
    #' @param value value to add or update
    #' @return none
    set_item = function(item, value) {
      # TODO: support synchronizer
      private$set_item_nosync(item, value)
    },
    #' @description
    #' delete attribute
    #' @param item character
    #' @return none
    del_item = function(item) {
      # TODO: support synchronizer
      private$del_item_nosync(item)
    }
  )
)
