% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/atomic.R
\name{as_scalar}
\alias{as_scalar}
\title{Convert a value to a scalar to opt-out of R default vector casting behavior.
This uses the \code{jsonlite::unbox} function to "tag" the value as a scalar.}
\usage{
as_scalar(obj)
}
\arguments{
\item{obj}{The value to convert.}
}
\value{
The value wrapped as a scalar.
}
\description{
Convert a value to a scalar to opt-out of R default vector casting behavior.
This uses the \code{jsonlite::unbox} function to "tag" the value as a scalar.
}
