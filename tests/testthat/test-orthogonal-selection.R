library(pizzarr)

test_that("orthogonal selection", {
  a <- array(data=1:20, dim=c(2, 10))
  #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
  # [1,]    1    3    5    7    9   11   13   15   17    19
  # [2,]    2    4    6    8   10   12   14   16   18    20
  z <- zarr_create(shape=dim(a), dtype="<f4", fill_value=NA)
  z$set_item("...", a)
  
  # indices
  expect_equal(
    z$get_orthogonal_selection(list(c(0,1), c(1,2,3)))$data,
    array(data=c(3, 4, 5, 6, 7, 8), dim=c(2, 3))
  )
  expect_equal(
    z$get_orthogonal_selection(list(c(1,0),c(9,1,6)))$data,
    array(data=c(20, 19, 4, 3, 14, 13), dim=c(2, 3))
  )
  expect_error(z$get_orthogonal_selection(list(c(1,0),c(12,1,6)))$data)
  
  # int and zb_int
  expect_equal(
    z$get_orthogonal_selection(list(zb_int(c(0,1)), zb_int(c(1,2,3))))$data,
    z$get_orthogonal_selection(list(int(c(1, 2)), int(c(2,3,4))))$data,
  )

  # TODO: do not expect error once negative indexing has been implemented.
  expect_error(z$get_orthogonal_selection(list(c(-1,1),c(12,1,6)))$data) 
  
  # mix integer vector with slices
  expect_equal(
    z$get_orthogonal_selection(list(slice(1,2),c(9,6)))$data,
    array(data=c(19, 20, 13, 14), dim=c(2, 2))
  )
  expect_equal(
    z$get_orthogonal_selection(list(c(1,0), slice(1,3)))$data,
    array(data=c(2, 1, 4, 3, 6, 5), dim=c(2, 3))
  )
})