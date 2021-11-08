#' Abstract store for Zarr
#' @title Store Class
#' @docType class
#' @description
#' Class representing an abstract store
#'
#' @rdname Store
#' @export
Store <- R6::R6Class("Store",
                     public = list(
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


#' DirectoryStore for Zarr
#' @title DirectoryStore Class
#' @docType class
#' @description
#' Store class using directories and files on a standard file system.
#' Adapted from https://github.com/zarr-developers/zarr_implementations/blob/c0bd932/generate_data/js/src/fsstore.js#L7
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
                                }
                              )
)


#' MemoryStore for Zarr
#' @title MemoryStore Class
#' @docType class
#' @description
#' Store class that uses a hierarchy of list objects,
#' thus all data will be held in main memory.
#' Reference: https://github.com/zarr-developers/zarr-python/blob/a5dfc3b4/zarr/storage.py#L512
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
                             get_item = function(item) {
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
                             }
                           )
)


