library(pizzarr)

test_that("orthogonal selection", {
  a <- array(data=1:20, dim=c(2, 10))
  #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
  # [1,]    1    3    5    7    9   11   13   15   17    19
  # [2,]    2    4    6    8   10   12   14   16   18    20
  z <- zarr_create(shape=dim(a), dtype="<f4", fill_value=NA)
  z$set_item("...", a)
  
  # indices
  z$get_orthogonal_selection(list(c(0,1), c(1,2,3)))$data
  z$get_orthogonal_selection(list(c(1,0),c(9,1,6)))$data
  expect_error(z$get_orthogonal_selection(list(c(1,0),c(12,1,6)))$data)
  
  # int and zb_int
  z$get_orthogonal_selection(list(int(1:2), c(1,2,3)))$data
  expect_equal(
    z$get_orthogonal_selection(list(int(1:2), c(1,2,3)))$data,
    z$get_orthogonal_selection(list(zb_int(0:1), c(1,2,3)))$data
  )
  z$get_orthogonal_selection(list(int(c(1,0)), c(1,2,3)))$data
  
  # TODO: should this be allowed ? 
  # expect_error(z$get_orthogonal_selection(list(c(-1,1),c(12,1,6)))$data) 
  
  # mix integer vector with slices
  z$get_orthogonal_selection(list(slice(1,2),c(9,6)))$data
  z$get_orthogonal_selection(list(c(1,0), slice(1,3)))$data
})