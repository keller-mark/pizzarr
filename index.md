<br/>
Pizzarr is an R implmentation for Zarr.

## Getting Started

The ``pizzarr`` package includes:

* TODO

## Installation

Installation requires R 4.0.0 or greater.

```r
install.packages("devtools")
devtools::install_github("keller-mark/pizzarr")
```

## Examples


```r
library(pizzarr)

a <- array(data=1:20, dim=c(2, 10))
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
# [1,]    1    3    5    7    9   11   13   15   17    19
# [2,]    2    4    6    8   10   12   14   16   18    20
z <- create(shape=dim(a), dtype="<f4", fill_value=NA)

z$set_item("...", a)

sel <- z$get_item(list(slice(1, 2), slice(1, 5)))

print(sel$data)
#      [,1] [,2] [,3] [,4] [,5]
# [1,]    1    3    5    7    9
# [2,]    2    4    6    8   10
```
