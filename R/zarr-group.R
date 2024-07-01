# Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/hierarchy.py#L39

#' The Zarr Group class.
#' @title ZarrGroup Class
#' @docType class
#' @description
#' Instantiate a group from an initialized store.
#'
#' @rdname ZarrGroup
#' @export
ZarrGroup <- R6::R6Class("ZarrGroup",
  private = list(
    # store TODO
    #' @keywords internal
    store = NULL,
    #' path TODO
    #' @keywords internal
    path = NULL,
    #' read_only TODO
    #' @keywords internal
    read_only = NULL,
    #' chunk_store TODO
    #' @keywords internal
    chunk_store = NULL,
    #' cache_attrs TODO
    #' @keywords internal
    cache_attrs = NULL,
    #' synchronizer TODO
    #' @keywords internal
    synchronizer = NULL,
    #' key_prefix TODO
    #' @keywords internal
    key_prefix = NULL,
    #' meta TODO
    #' @keywords internal
    meta = NULL,
    #' attrs TODO
    #' @keywords internal
    attrs = NULL,
    item_path = function(item) {
      # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0e6cdc04c6413e14f57f61d389972ea937c/zarr/hierarchy.py#L302
      # Check if the first character is a forward slash
      is_absolute <- !is_na(item) && is.character(item) && substr(item, 1, 1) == "/"
      path <- normalize_storage_path(item)
      if(!is_absolute && !is_na(private$path)) {
        path <- paste0(private$key_prefix, path)
      }
      return(path)
    },
    create_group_nosync = function(name, overwrite = FALSE) {
      path <- private$item_path(name)

      # create terminal group
      init_group(
        private$store,
        path = path,
        overwrite = overwrite,
        chunk_store = self$get_chunk_store()
      )

      # create group instance
      return(ZarrGroup$new(
        private$store,
        path = path,
        read_only = private$read_only,
        chunk_store = self$get_chunk_store(),
        synchronizer = private$synchronizer,
        cache_attrs = private$attrs$cache
      ))
    },
    create_dataset_nosync = function(name, data = NA, ...) {
      path <- private$item_path(name)
      
      kwargs <- list(...)

      if(is.null(kwargs[['synchronizer']])) {
        synchronizer <- private$synchronizer
      } else {
        synchronizer <- kwargs[['synchronizer']]
      }
      if(is.null(kwargs[['cache_attrs']])) {
        cache_attrs <- private$attrs$cache
      } else {
        cache_attrs <- kwargs[['cache_attrs']]
      }

      if(is_na(data)) {
        a <- zarr_create(
          store = private$store,
          path = path,
          chunk_store = self$get_chunk_store(),
          synchronizer = synchronizer,
          cache_attrs = cache_attrs,
          ...
        )
      } else {
        a <- zarr_create_array(
          store = private$store,
          path = path,
          data = data,
          chunk_store = self$get_chunk_store(),
          synchronizer = synchronizer,
          cache_attrs = cache_attrs,
          ...
        )
      }
      return(a)
    }
  ),
  public = list(
    #' @description
    #' Create a new ZarrGroup instance.
    #' @param store Group store, already initialized.
    #' @param path character path
    #' @param read_only logical read only?
    #' @param chunk_store TODO
    #' @param synchronizer TODO
    #' @param cache_metadata logical cache metadata?
    #' @param cache_attrs logical cache attributes?
    #' @return A `ZarrGroup` instance.
    initialize = function(store, path = NA, read_only = FALSE, 
                          chunk_store = NA, cache_attrs = TRUE, 
                          synchronizer = NA) {
      private$store <- store
      private$path <- normalize_storage_path(path)
      if(!is_na(private$path) && private$path != "") {
        private$key_prefix <- paste0(private$path, "/")
      } else {
        private$key_prefix <- ""
      }
      private$read_only <- read_only
      private$chunk_store <- chunk_store

      # guard conditions
      if(contains_array(store, path)) {
        stop("ContainsArrayError(path)")
      }

      m_key <- paste0(private$key_prefix, GROUP_META_KEY)
      
      # use consolidated metadata if exists
      meta <- try_from_zmeta(m_key, store)
      
      if(!is.null(meta)) {
        
        private$meta <- meta
        
      } else {
        
        # initialize metadata
        meta_bytes <- tryCatch({
          
          meta_bytes <- store$get_item(m_key)
          
        }, error = function(cond) {
          if(is_key_error(cond)) {
            stop("GroupNotFoundError(path) in Group$new")
          } else {
            stop(cond$message)
          }
        })
        
        if(!is.null(meta_bytes))
          private$meta <- private$store$metadata_class$decode_group_metadata(meta_bytes)
        
      }
      
      # setup attributes
      a_key <- paste0(private$key_prefix, ATTRS_KEY)
      private$attrs <- Attributes$new(store, key = a_key, read_only = read_only, cache = cache_attrs, synchronizer = synchronizer)
    },
    #' @description
    #' Get group store
    get_store = function() {
      return(private$store)
    },
    #' @description
    #' Get group path
    get_path = function() {
      return(private$path)
    },
    #' @description
    #' Get group metadata
    get_meta = function() {
      return(private$meta)
    },
    #' @description
    #' Get group name
    get_name = function() {
      if(!is_na(private$path)) {
        name <- private$path
        if(substr(name, 1, 1) != "/") {
          name <- paste0("/", name)
        }
        return(name)
      }
      return("/")
    },
    #' @description
    #' Is store read only?
    get_read_only = function() {
      return(private$read_only)
    },
    #' @description
    #' Get group chunk store
    get_chunk_store = function() {
      if(is_na(private$chunk_store)) {
        return(private$store)
      }
      return(private$chunk_store)
    },
    #' @description
    #' Get group synchronizer
    get_synchronizer = function() {
      return(private$synchronizer)
    },
    #' @description
    #' Get group attributes
    get_attrs = function() {
      return(private$attrs)
    },
    #' @description
    #' Test for group membership.
    #' @param item character item to test for
    contains_item = function(item) {
      path <- private$item_path(item)
      return(contains_array(private$store, path) || contains_group(private$store, path))
    },
    #' @description
    #' Obtain a group member.
    #' @param item character item to test for
    get_item = function(item) {
      if(is.null(item)) {
        #for case with no internet
        stop("item can not be null")
      }
      path <- private$item_path(item)
      if(contains_array(private$store, path)) {
        return(ZarrArray$new(
          private$store,
          path = path,
          read_only = private$read_only,
          chunk_store = self$get_chunk_store(),
          synchronizer = private$synchronizer,
          cache_attrs = private$attrs$cache
        ))
      } else if(contains_group(private$store, path)) {
        return(ZarrGroup$new(
          private$store,
          path = path,
          read_only = private$read_only,
          chunk_store = self$get_chunk_store(),
          synchronizer = private$synchronizer,
          cache_attrs = private$attrs$cache
        ))
      } else {
        stop("KeyError: item")
      }
    },
    #' @description
    #' create a group
    #' @param name character group name
    #' @param overwrite logical overwrite?
    create_group = function(name, overwrite = FALSE) {
      return(private$create_group_nosync(name, overwrite = overwrite))
    },
    #' @param name character group name
    #' @param data data to add to group
    #' @param ... Extra arguments to pass to zarr_create() or array().
    create_dataset = function(name, data = NA, ...) {
      return(private$create_dataset_nosync(name, data = data, ...))
    }
    # TODO: convenience functions like zeros, ones, empty, full, ...
  )
)
