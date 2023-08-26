#' Convert an R vector, matrix or array into a Zarr object
#'
#' @param array Array to convert
#' @param ... Any additional parameters that go with the lower-level zarr_create function
#'
#' @return A Zarr object
#' @export
as_zarr = function(array, ...){
  if(!is.atomic(array)){
    stop("Zarr arrays can only be created from arrays, matrices or vectors")
  }
  argList = list(...)
  #Infer the array shape
  if(is.null(argList[["shape"]])){
    #TODO zero dim array can be created but not retrieved
    if(!is.vector(array)){
      argList[["shape"]] = dim(array)
    } else{
      argList[["shape"]] = length(array)
    }
  }
  #Infer the data type
  if(is.null(argList[["dtype"]])){
    argList[["dtype"]] = get_np_dataFormat(array)
  } else {
    argList[["dtype"]] = get_np_dataFormat(argList[["dtype"]], isArray = F)
  }
  #Create the zarr object
  z = do.call(zarr_create, argList)
  z$set_item("...", as.array(array))
  return(z)
}

#' Create a Zarr object
#'
#' @param array Array to convert (can be a vector of matrix as well)
#' @param ... Any additional parameters that go with the lower-level zarr_create function
#'
#' @return A Zarr object
#' @export
zarr = function(array, ...){
  as_zarr(array, ...)
}
