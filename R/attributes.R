#' The Zarr Attributes class.
#' @title Attributes Class
#' @docType class
#' @description
#' Class providing access to user attributes on an array or group.
#' Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/attrs.py#L7
#'
#' @rdname Attributes
#' @export
Attributes <- R6::R6Class("Attributes",
  private = list(
    #' @field cached_aslist The attributes cached as a list.
    cached_aslist = NULL,

    get_nosync = function() {

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
    #' @return An `Attributes` instance.
    initialize = function(store, key = ".zattrs", read_only = FALSE, cache = TRUE, synchronizer = NA) {
      self$store <- store
      self$key <- key
      self$read_only <- read_only
      self$cache <- cache
      private$cached_aslist <- NA
      self$synchronizer <- synchronizer
    },
    asdict = function() {

    },
    refresh = function() {

    },
    contains = function(x) {

    },
    get_item = function(item) {

    },
    set_item = function(item, value) {

    }
  )
)
