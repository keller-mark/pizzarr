library(pizzarr)

test_that("orthogonal selection", {
  a <- array(data=1:20, dim=c(4, 10))
  #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
  # [1,]    1    3    5    7    9   11   13   15   17    19
  # [2,]    2    4    6    8   10   12   14   16   18    20
  z <- zarr_create(shape=dim(a), dtype="<f4", fill_value=NA)
  z$set_item("...", a)
  
  # indices
  z$get_orthogonal_selection(list(c(1,2), c(1,2,3)))$data
  z$get_orthogonal_selection(list(c(2,1),c(10,1,6)))$data
  expect_error(z$get_orthogonal_selection(list(c(2,1),c(12,1,6)))$data)
  
  # TODO: should this be allowed ? 
  # expect_error(z$get_orthogonal_selection(list(c(-1,1),c(12,1,6)))$data) 
  
  # mix integer vector with slices
  z$get_orthogonal_selection(list(slice(1,2),c(10,6)))$data
  z$get_orthogonal_selection(list(c(2,1), slice(1,3)))$data
})