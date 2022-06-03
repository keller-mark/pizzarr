# Reference: https://github.com/gzuidhof/zarr.js/blob/292804/src/core/indexing.ts#L45
normalize_list_selection <- function(selection, shape, convert_integer_selection_to_slices = FALSE) {
  selection <- replace_ellipsis(selection, shape)

  for(i in seq_along(selection)) {
    dim_sel <- selection[i]
    if(is_integer(dim_sel)) {
      if(convert_integer_selection_to_slices) {
        selection[i] <- slice(dim_sel, dim_sel + 1, 1)
      } else {
        selection[i] <- normalize_integer_selection(dim_sel, shape[i])
      }
    } else if(is_integer_list(dim_sel)) { # TODO: should this be is_integer_vec?
      stop('TypeError(Integer array selections are not supported (yet))')
    } else if(is.na(dim_sel) || dim_sel == ":") {
      selection[i] <- slice(NA, NA, 1)
    }
  }
  return(selection)
}

# Reference: https://github.com/gzuidhof/zarr.js/blob/master/src/core/indexing.ts#L110
normalize_integer_selection <- function(dim_sel, dim_len) {
  # Normalize type to int
  dim_sel <- as.scalar(dim_sel)

  # handle wraparound
  if(dim_sel < 0) {
    dim_sel <- dim_len + dim_sel
  }

  # Handle out of bounds
  if(dim_sel >= dim_len || dim_sel < 0) {
    stop('BoundsCheckError(dim_len)')
  }

  return(dim_sel)
}

normalize_resize_args <- function(old_shape, args) {
  if(length(args) == 1) {
    new_shape <- args[1]
  } else {
    new_shape <- args
  }
  new_shape <- as.list(new_shape)
  if(length(new_shape) != length(old_shape)) {
    stop('new shape must have same number of dimensions')
  }
  return(new_shape)
}

normalize_storage_path <- function(path) {
  # Reference: https://github.com/gzuidhof/zarr.js/blob/29280463ff2f275c31c1fa0f002daa947b8f09b2/src/util.ts#L32

  if(!is.na(path)) {
    # convert backslash to forward slash
    path <- gsub("\\\\", "/", path)
    path_list <- str_to_vec(path)

    # ensure no leading slash
    while(length(path_list) > 0 && path_list[1] == '/') {
      path <- stringr::str_sub(path, start = 2)
      path_list <- str_to_vec(path)
    }

    # ensure no trailing slash
    while(length(path_list) > 0 && path_list[length(path_list)] == "/") {
      path <- stringr::str_sub(path, start = 1, end = length(path_list) - 1)
      path_list <- str_to_vec(path)
    }

    # collapse any repeated slashes
    path <- gsub("/+", "/", path)

    # don't allow path segments with just '.' or '..'
    path_segments <- stringr::str_split(path, "/")[[1]]
    for(segment in path_segments) {
      if(segment == "." || segment == "..") {
        stop("path containing '.' or '..' segment not allowed")
      }
    }

  } else {
    path <- ""
  }
  return(path)
}

# Reference: https://github.com/gzuidhof/zarr.js/blob/292804/src/util.ts#L69
normalize_shape <- function(shape) {
  shape <- ensure_vec(shape)
  return(floor(shape))
}