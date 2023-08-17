<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/8/86/Grandma-pizza-01.jpg/640px-Grandma-pizza-01.jpg" width="400" align="right"/> 

# pizzarr

A Zarr implementation for R.

🚧 [work in progress](https://github.com/keller-mark/pizzarr/search?q=TODO) 🚧

## Installation

Installation requires R 4.0.0 or greater.

```r
install.packages("devtools")
devtools::install_github("keller-mark/pizzarr")
```

## Usage

```r
library(pizzarr)

a <- array(data=1:20, dim=c(2, 10))
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
# [1,]    1    3    5    7    9   11   13   15   17    19
# [2,]    2    4    6    8   10   12   14   16   18    20
z <- zarr_create(shape=dim(a), dtype="<f4", fill_value=NA)

z$set_item("...", a)

selection <- z$get_item(list(slice(1, 2), slice(1, 5)))

print(selection$data)
#      [,1] [,2] [,3] [,4] [,5]
# [1,]    1    3    5    7    9
# [2,]    2    4    6    8   10
```


## Development


```r
setwd("path/to/pizzarr")
install.packages("devtools")
devtools::install()
devtools::load_all()
```

## Testing

```r
devtools::check()
devtools::test()
```

## Documentation

```r
install.packages("devtools")
install.packages("pkgdown")
devtools::document()
pkgdown::build_site()
```

## Resources

- R package development
  - [R packages](https://r-pkgs.org/)
  - [roxygen2 syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html)
  - [R6](https://r6.r-lib.org/index.html)
  - [R6 roxygen2 syntax](https://www.tidyverse.org/blog/2019/11/roxygen2-7-0-0/#r6-documentation)
  - [pkgdown](https://pkgdown.r-lib.org/)
- Zarr implementation
  - [zarr_implementations](https://github.com/zarr-developers/zarr_implementations)
  - [zarr-python](https://github.com/zarr-developers/zarr-python)
  - [LZ4 and ZSTD compression in R](https://github.com/traversc/qs)
  - [zarr.js](https://github.com/gzuidhof/zarr.js)
  - [zarrita.js](https://github.com/manzt/zarrita.js)
  - [v2 spec](https://zarr.readthedocs.io/en/stable/spec/v2.html)
  
## Notes

### Selections

Must be of type `list` with values `"..."`, `Slice`, or `scalar`.

For `slice()`, `start` and `stop` are intended to work like R `arr[start:stop]`: one-based indexing, inclusive start, inclusive stop.
For `zb_slice()`, `start` and `stop` are intended to work like Python `arr[start:stop]`: zero-based indexing, inclusive start, exclusive stop.


See the `slice()` and `as_scalar` functions.

### "array buffers"

In JavaScript, there is such thing as an ArrayBuffer, which is used as the internal representation for TypedArrays.
I don't think there is an analogous concept in R, but we can try to use raw vectors as array buffers:

```R
readBin(con = as.raw(c(
  0x7b, 0x22, 0x68, 0x65, 0x6c, 0x6c, 0x6f, 0x22,
  0x3a, 0x5b, 0x22, 0x77, 0x6f, 0x72, 0x6c, 0x64,
  0x22, 0x2c, 0x22, 0x21, 0x22, 0x5d, 0x7d, 0x12
)), integer(), size = 1, n = 24, signed = FALSE)
readBin(con = as.raw(c(
  0x7b, 0x22, 0x68, 0x65, 0x6c, 0x6c, 0x6f, 0x22,
  0x3a, 0x5b, 0x22, 0x77, 0x6f, 0x72, 0x6c, 0x64,
  0x22, 0x2c, 0x22, 0x21, 0x22, 0x5d, 0x7d, 0x12
)), integer(), size = 2, n = 12, signed = FALSE)
readBin(con = as.raw(c(
  0x7b, 0x22, 0x68, 0x65, 0x6c, 0x6c, 0x6f, 0x22,
  0x3a, 0x5b, 0x22, 0x77, 0x6f, 0x72, 0x6c, 0x64,
  0x22, 0x2c, 0x22, 0x21, 0x22, 0x5d, 0x7d, 0x12
)), double(), size = 8, n = 3, signed = TRUE)
```


