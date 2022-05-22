#' Convert a JSON-like list to a raw type.
#'
#' @param json_as_list An R list to be converted to JSON.
#' @return The raw value.
json_to_raw <- function(json_as_list) {
  json_str <- jsonlite::toJSON(json_as_list)
  json_raw <- charToRaw(json_str)
  return(json_raw)
}


#' Write an R matrix to a Zarr store (one chunk, no compression).
#'
#' @param matrix The matrix as an R matrix.
#' @param rows A vector of row names.
#' @param cols A vector of column names.
#' @param store The Zarr store.
matrix_to_zarr <- function(matrix, rows, cols, store, compressor = NA) {

  num_rows <- nrow(matrix)
  num_cols <- ncol(matrix)

  raw_matrix <- as.raw(matrix)

  compressor_meta <- jsonlite::unbox(NA)
  if(R6::is.R6(compressor)) {
    raw_matrix <- compressor$encode(raw_matrix)
    compressor_meta <- compressor$get_meta()
  }

  zattrs <- list(
    rows = rows,
    cols = cols
  )
  zarray <- list(
    # TODO: set chunk size to something smaller if multiple chunks
    chunks = c(num_rows, num_cols),
    compressor = compressor_meta,
    dtype = jsonlite::unbox("|u1"),
    fill_value = jsonlite::unbox(0),
    filters = jsonlite::unbox(NA),
    order = jsonlite::unbox("C"),
    shape = c(num_rows, num_cols),
    zarr_format = jsonlite::unbox(2)
  )

  store$set_item(".zattrs", json_to_raw(zattrs))
  store$set_item(".zarray", json_to_raw(zarray))
  # TODO: chunks
  store$set_item("0.0", raw_matrix)
}

#' Create an empty named list
#'
#' A helper function to construct an empty list which converts to a JSON object rather than a JSON array.
#'
#' @param ... A variable number of list entries.
#' @return An empty named list.
#' 
#' @keywords internal
#' @export
#' @examples
#' default_window <- obj_list(
#'   min = 0,
#'   max = 255
#' )
obj_list <- function(...) {
  retval <- stats::setNames(list(), character(0))
  param_list <- list(...)
  for(key in names(param_list)) {
    retval[[key]] = param_list[[key]]
  }
  retval
}
