% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{pizzarr_sample}
\alias{pizzarr_sample}
\title{pizzarr demo data}
\usage{
pizzarr_sample(
  dataset = NULL,
  outdir = file.path(tools::R_user_dir("pizzarr"), "pizzarr_sample")
)
}
\arguments{
\item{dataset}{character defining which demo dataset is desired,
If NULL, all are returned}

\item{outdir}{character directory path to store sample zarr stores}
}
\value{
path to ready to use zarr store
}
\description{
pizzarr demo data
}
\details{
For directory stores, unzips the store to a temporary directory
and returns the resulting path.
}
\examples{

sample_dir <- tools::R_user_dir("pizzarr")

clean <- !dir.exists(sample_dir)

zarr_samples <- pizzarr_sample(outdir = sample_dir)

#printing without system path for example
gsub(sample_dir, "...", zarr_samples, fixed = TRUE)

# clean up if you don't want to keep them for next time
if(clean) unlink(sample_dir, recursive = TRUE)

}
