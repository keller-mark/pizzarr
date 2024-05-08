#' Abstract store for Zarr
#' @title Store Class
#' @docType class
#' @description
#' Class representing an abstract store
#'
#' @rdname Store
#' @export
Store <- R6::R6Class("Store",
   private = list(
      #' @field readable
      #' @keywords internal
      readable = NULL,
      #' @field writable
      #' @keywords internal
      writeable = NULL,
      #' @field erasable
      #' @keywords internal
      erasable = NULL,
      #' @field listable
      #' @keywords internal
      listable = NULL,
      #' @field store_version
      #' @keywords internal
      store_version = NULL,
      #' @keywords internal
      listdir_from_keys = function(path) {
        # TODO
      },
      #' @keywords internal
      rename_from_keys = function() {
        # TODO
      },
      #' @keywords internal
      rmdir_from_keys = function(path) {
        # TODO
      }
   ),
   public = list(
    #' @field metadata_class
    #' @keywords internal
    metadata_class = NULL,
    initialize = function() {
      private$readable <- TRUE
      private$writeable <- TRUE
      private$erasable <- TRUE
      private$listable <- TRUE
      private$store_version <- 2
      self$metadata_class <- Metadata2$new()
    },
    is_readable = function() {
      return(private$readable)
    },
    is_writeable = function() {
      return(private$writeable)
    },
    is_erasable = function() {
      return(private$erasable)
    },
    is_listable = function() {
      return(private$listable)
    },
    close = function() {
      # Do nothing by default
    },
    listdir = function(path=NA) {
      if(is.na(path)) {
        path <- ""
      }
      path <- normalize_storage_path(path)
      return(private$listdir_from_keys(path))
    },
    rename = function(src_path, dst_path) {
      if(!self$is_erasable()) {
        stop("Store is not erasable, cannot call 'rename'")
      }
      private$rename_from_keys(src_path, dst_path)
    },
    rmdir = function(path) {
      if(!self$is_erasable()) {
        stop("Store is not erasable, cannot call 'rmdir'")
      }
      path <- normalize_storage_path(path)
      private$rmdir_from_keys(path)
    },
     #' @description
     #' Get an item from the store.
     #' @param key The item key.
     #' @return The item data in a vector of type raw.
     get_item = function(key) {

     },
     #' @description
     #' Set an item in the store.
     #' @param key The item key.
     #' @param value The item value as a vector of type raw.
     set_item = function(key, value) {

     },
     #' @description
     #' Determine whether the store contains an item.
     #' @param key The item key.
     #' @return A boolean value.
     contains_item = function(key) {

     }
   )
)

# Reference: https://github.com/zarr-developers/zarr_implementations/blob/c0bd932/generate_data/js/src/fsstore.js#L7

#' DirectoryStore for Zarr
#' @title DirectoryStore Class
#' @docType class
#' @description
#' Store class using directories and files on a standard file system.
#'
#' @rdname DirectoryStore
#' @export
DirectoryStore <- R6::R6Class("DirectoryStore",
  inherit = Store,
  public = list(
    #' @field root The path to the root of the store.
    #' @keywords internal
    root = NULL,
    #' @description
    #' Create a new file system store.
    #' @param root The path to the root of the store.
    #' @return A new `DirectoryStore` object.
    initialize = function(root) {
      super$initialize()
      self$root <- root
      if(!dir.exists(root)) {
        dir.create(root, recursive = TRUE, showWarnings = FALSE)
      }
    },
    #' @description
    #' Get an item from the store.
    #' @param key The item key.
    #' @return The item data in a vector of type raw.
    get_item = function(key) {
      fp <- file.path(self$root, key)
      if(!file.exists(fp)) {
        stop("KeyError:", key)
      }
      fp_size <- file.info(fp)$size
      fp_pointer <- file(fp, "rb")
      fp_data <- readBin(fp_pointer, what = "raw", n = fp_size)
      close(fp_pointer)
      return(fp_data)
    },
    #' @description
    #' Set an item in the store.
    #' @param key The item key.
    #' @param value The item value as a vector of type raw.
    set_item = function(key, value) {
      fp <- file.path(self$root, key)
      dir.create(dirname(fp), recursive = TRUE, showWarnings = FALSE)
      fp_pointer <- file(fp, "wb")
      writeBin(value, fp_pointer)
      close(fp_pointer)
    },
    #' @description
    #' Determine whether the store contains an item.
    #' @param key The item key.
    #' @return A boolean value.
    contains_item = function(key) {
      fp <- file.path(self$root, key)
      return(file.exists(fp))
    },
    rmdir = function(path=NA) {
      path <- normalize_storage_path(path)
      dir_path <- self$root
      if(!is.na(path)) {
        dir_path <- file.path(self$root, path)
      }
      if(dir.exists(dir_path)) {
        unlink(dir_path, recursive = TRUE)
      }
    },
    listdir = function(key=NA) {
      if(is_na(key)) {
        dir_path <- self$root
      } else {
        dir_path <- file.path(self$root, key)
      }
      if(!dir.exists(dir_path)) {
        stop("KeyError:", key)
      }
      dir_list <- sort(list.files(dir_path, full.names = FALSE, all.files = TRUE, include.dirs = TRUE))
      dir_list <- dir_list[!dir_list %in% c(".", "..")]
      return(dir_list)
    }
  )
)


# Reference: https://github.com/zarr-developers/zarr-python/blob/a5dfc3b4/zarr/storage.py#L512

#' MemoryStore for Zarr
#' @title MemoryStore Class
#' @docType class
#' @description
#' Store class that uses a hierarchy of list objects,
#' thus all data will be held in main memory.
#'
#' @rdname MemoryStore
#' @export
MemoryStore <- R6::R6Class("MemoryStore",
   inherit = Store,
   public = list(
     #' @field root The root list for the store.
     #' @keywords internal
     root = NULL,
     #' @description
     #' Create a new memory store.
     #' @return A new `MemoryStore` object.
     initialize = function() {
      super$initialize()
       self$root <- obj_list()
     },
     #' @description
     #' Get the parent of an item.
     #' @keywords internal
     #' @param item The item key.
     #' @return A list with the keys `parent` and `key`.
     get_parent = function(item) {
       parent <- self$root
       segments <- strsplit(item, "/")[[1]]
       for(k in segments[1:length(segments)-1]) {
         parent <- parent[[k]]
         if(!is.list(parent)) {
           stop("KeyError:", item)
         }
       }
       return(list(parent = parent, key = segments[length(segments)]))
     },
     #' @description
     #' Get an item from the store.
     #' @param item The item key.
     #' @return The item data in a vector of type raw.
     get_item = function(item=NA) {
       if(is_na(item)) {
        return(self$root)
       }
       parent_and_key <- self$get_parent(item)
       parent <- parent_and_key$parent
       key <- parent_and_key$key

       if(key %in% names(parent)) {
         value <- parent[[key]]
       } else {
         stop("KeyError:", item)
       }
       return(value)
     },
     #' @description
     #' Set an item in the store.
     #' @param item The item key.
     #' @param value The item value as a vector of type raw.
     set_item = function(item, value) {
       segments <- strsplit(item, "/")[[1]]
       if(length(segments) > 1) {
         for(i in 1:(length(segments)-1)) {
           k <- segments[i]
           if(i == 1 && k %in% names(self$root)) {
             if(!is.list(self$root[[k]])) {
               stop("KeyError:", item)
             }
           } else if(i > 1 && k %in% names(self$root[[segments[1:(i-1)]]])) {
             if(!is.list(self$root[[segments[1:i]]])) {
               stop("KeyError:", item)
             }
           } else {
             self$root[[segments[1:i]]] <- obj_list()
           }
         }
       }
       self$root[[segments]] <- value
     },
     #' @description
     #' Determine whether the store contains an item.
     #' @param item The item key.
     #' @return A boolean value.
     contains_item = function(item) {
       result <- tryCatch({
         parent_and_key <- self$get_parent(item)
         parent <- parent_and_key$parent
         key <- parent_and_key$key
         if(key %in% names(parent)) {
           value <- parent[[key]]
           return(!is.list(value))
         } else {
           return(FALSE)
         }
       }, error = function(e) {
         return(FALSE)
       })
       return(result)
     },
     listdir = function(key=NA) {
      item <- self$get_item(key)
      if(!is.list(item)) {
        stop("KeyError:", key)
      }
      return(sort(names(item)))
     },
     rmdir = function(item) {
      self$set_item(item, NULL)
     }
   )
)

# Reference: https://github.com/manzt/zarrita.js/blob/main/packages/storage/src/fetch.ts

#' HttpStore for Zarr
#' @title HttpStore Class
#' @docType class
#' @description
#' Store class that uses HTTP requests.
#' Read-only. Depends on the `crul` package.
#'
#' @rdname HttpStore
#' @export
HttpStore <- R6::R6Class("HttpStore",
  inherit = Store,
  private = list(
    url = NULL,
    base_path = NULL,
    domain = NULL,
    options = NULL,
    headers = NULL,
    client = NULL,
    make_request = function(item) {
      # Remove leading slash if necessary.
      if(substr(item, 1, 1) == "/") {
        key <- substr(item, 2, length(item))
      } else {
        key <- item
      }
      
      res <- private$client$get(paste(private$base_path, key, sep="/"))
      return(res)
    }
  ),
  public = list(
    initialize = function(url, options = NA, headers = NA) {
      super$initialize()
      # Remove trailing slash if necessary.
      if(substr(url, length(url), length(url)) == "/") {
        private$url <- substr(url, 1, length(url)-1)
      } else {
        private$url <- url
      }
      private$options <- options
      private$headers <- headers

      segments <- stringr::str_split(private$url, "/")[[1]]
      private$domain <- paste(segments[1:3], collapse="/")
      private$base_path <- paste(segments[4:length(segments)], collapse="/")

      private$client <- crul::HttpClient$new(
        url = private$domain,
        opts = private$options,
        headers = private$headers
      )
    },
    #' @description
    #' Get an item from the store.
    #' @param item The item key.
    #' @return The item data in a vector of type raw.
    get_item = function(item) {
      res <- private$make_request(item)
      return(res$content)
    },
    contains_item = function(item) {
      res <- private$make_request(item)
      return(res$status_code == 200)
    }
  )
)