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

## Current status

(This section has been adapted from the [Rarr](https://github.com/grimbough/Rarr) project README).

### Stores

| Store             | Status<br/>(reading / writing) |
|-------------------|:------------------------------:|
| `MemoryStore`     |             ✔ / ✔              |
| `DirectoryStore`  |             ✔ / ✔              |
| `HttpStore`       |             ✔ / ❌             |

### Data types

| Zarr Data Type        | Status<br/>(reading / writing) | Notes                                                                                                                                                                           |
|-----------------------|:------------------------------:|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `boolean`             |             ✔ / ✔             |                                                                                                                                                                                 |
| `int8`                |             ✔ / ✔             |                                                                                                                                                                                 |
| `uint8`               |             ✔ / ✔             |                                                                                                                                                                                 |
| `int16`               |             ✔ / ✔             |                                                                                                                                                                                 |
| `uint16`              |             ✔ / ✔             |                                                                                                                                                                                 |
| `int32`               |             ✔ / ✔             |                                                                                                                                                                                 |
| `uint32`              |             ✔ / ✔             |  |
| `int64`               |             ✔ / ✔             |  |
| `uint64`              |             ✔ / ✔             | |
| `half` / `float16`    |             ✔ / ✔             | Converted to `double` in R. No effort is made to assess loss of precision due to conversion.                                                                                    |
| `single` / `float32`  |             ✔ / ✔             | Converted to `double` in R. No effort is made to assess loss of precision due to conversion.                                                                                    |
| `double` / `float64`  |             ✔ / ✔             |                                                                                                                                                                                 |
| `complex`             |            ❌ / ❌             |                                                                                                                                                                                 |
| `timedelta`           |            ❌ / ❌             |                                                                                                                                                                                 |
| `datetime`            |            ❌ / ❌             |                                                                                                                                                                                 |
| `string`              |            ❌ / ❌             | [On roadmap](https://github.com/keller-mark/pizzarr/issues/22)                |
| `Unicode`             |            ❌ / ❌             | [On roadmap](https://github.com/keller-mark/pizzarr/issues/22)                |
| `void *`              |            ❌ / ❌             |                                                                                                                                                                                 |
| Structured data types |            ❌ / ❌             |                                                                                                                                                                                 |

### Compression tools

| Data Type     | Status<br/>(reading / writing) | Notes                                                                                               |
|---------------|:------------------------------:|-----------------------------------------------------------------------------------------------------|
| `zlib / gzip` |             ✔ / ✔              | Only system default compression level 6 is enabled for writing.                                     |
| `bzip2`       |             ✔ / ✔              | Only system default compression level 6 is enabled for writing.                                     |
| `blosc`       |             ✔ / ✔              | Only system default compression level 5 is enabled for writing.                                     |
| `LZMA`        |             ✔ / ✔              | Only system default compression level 9 is enabled for writing.                                     |
| `LZ4`         |             ✔ / ✔              |   |
| `Zstd`        |             ✔ / ✔              |   |


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

- [Discussion of Zarr in R](https://github.com/zarr-developers/community/issues/18)
- [Rarr](https://github.com/grimbough/Rarr)
  - Note: `pizzarr` has an optional dependency on Rarr for Blosc (de)compression.
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
