manage_filters <- function(filters) {
  lapply(filters, function(x) {
    # Proceed based on type of filter
    if(typeof(x) == "symbol") {
      # When empty dimension, return everything
      if(x == "") {
        return(NULL)
      } else {
        stop("Unsupported filter '", as.character(x), "' supplied") 
      }
      
    } else if(typeof(x) == "double") {
      # Return single value for dimension
      # TODO: do we need slicing for this case ?
      # return(slice(x, x))
      return(x)
    } else if(typeof(x) == "language") {
      x <- as.list(x)
      
      # Return a range (supplied via : or seq())
      if(x[[1]] == ":") {
        # return(slice(x[[2]], x[[3]]))
        # TODO: do we need slicing for this case ?
        return(x[[2]]:x[[3]])
      } else if(x[[1]] == "seq") {
        # TODO: do we need slicing for this case ?
        arg_names <- names(x)
        from <- ifelse("from" %in% arg_names, x[[which("from" == arg_names)]], x[[2]])
        to <- ifelse("to" %in% arg_names, x[[which("to" == arg_names)]], x[[3]])
        if(length(x) > 3) {
          by <- ifelse("by" %in% arg_names, x[[which("by" == arg_names)]], x[[4]])
          return(seq(from, to, by))
        } else {
          # by <- NA
          return(seq(from, to))
        }
        return(seq())
        # return(slice(from, to, by))
        # custom vector slicing
      } else if(x[[1]] == "c") {
        check_func <- sapply(x, function(y) {
          !is.function(eval(y))
        })
        return(floor(unlist(x[check_func])))
      } else {
        stop("Unsupported filter '", as.character(x), "' supplied")
      }
    } else {
      stop("Unsupported filter '", as.character(x), "' supplied")
    }
  })
}