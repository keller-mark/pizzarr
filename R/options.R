# Adapted from https://github.com/IRkernel/IRkernel/blob/master/R/options.r

#' pizzarr_option_defaults
#' @description 
#' * pizzarr.http_store_cache_time_seconds how long to cache web requests
#' * pizzarr.parallel_backend "future", a cluster object, or an integer (if not on windows)
#' * pizzarr.parallel_write_enabled logical, whether to use parallel backend for writing
#' * pizzarr.progress_bar logical whether to use `pbapply` to emit a progress bar
#' @export
pizzarr_option_defaults <- list(
    pizzarr.http_store_cache_time_seconds = 3600,
    pizzarr.parallel_backend = NA,
    pizzarr.parallel_write_enabled = FALSE,
    pizzarr.progress_bar = FALSE
)

#' @keywords internal
parse_parallel_option <- function(val) {
  
  if(is.na(val)) return(val)
  
  if(inherits(val, "cluster")) {
    return(val)
  }
  
  if(!is.na(val) && val == "future") {
    return("future")
  }
  
  logical_val <- suppressWarnings(as.logical(val))
  integer_val <- suppressWarnings(as.integer(val))

  if(is.na(integer_val)) {
    return(logical_val)
  }
  if(integer_val <= 1) {
    return(as.logical(integer_val))
  }
  return(integer_val)
}

#' @keywords internal
from_env <- list(
    PIZZARR_HTTP_STORE_CACHE_TIME_SECONDS = as.integer,
    PIZZARR_PARALLEL_BACKEND = parse_parallel_option,
    PIZZARR_PARALLEL_WRITE_ENABLED = as.logical,
    PIZZARR_PROGRESS_BAR = as.logical
)

# converts e.g. jupyter.log_level to JUPYTER_LOG_LEVEL
#' @keywords internal
opt_to_env <- function(nms) {
    gsub('.', '_', toupper(nms), fixed = TRUE)
}

# called in .onLoad
#' @keywords internal
init_options <- function() {
    for (opt_name in names(pizzarr_option_defaults)) {
        # skip option if it is already set, e.g. in the Rprofile
        if (is.null(getOption(opt_name))) {
            # prepare `options` call from the default
            call_arg <- pizzarr_option_defaults[opt_name]  # single [] preserve names
            
            # if an env var is set, get value from it.
            env_name <- opt_to_env(opt_name)
            convert <- from_env[[env_name]]
            env_val <- Sys.getenv(env_name, unset = NA)
            if (!is.null(convert) && !is.na(env_val)) {
                call_arg[[opt_name]] <- convert(env_val)
            }
            
            do.call(options, call_arg)
        }
    }
}