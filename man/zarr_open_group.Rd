% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/creation.R
\name{zarr_open_group}
\alias{zarr_open_group}
\title{Open a group using file-mode-like semantics.}
\usage{
zarr_open_group(
  store = NA,
  mode = NA,
  cache_attrs = TRUE,
  synchronizer = NA,
  path = NA,
  chunk_store = NA,
  storage_options = NA
)
}
\arguments{
\item{store}{: MutableMapping or string, optional
Store or path to directory in file system or name of zip file.}

\item{mode}{: {'r', 'r+', 'a', 'w', 'w-'}, optional
Persistence mode: 'r' means read only (must exist); 'r+' means
read/write (must exist); 'a' means read/write (create if doesn't
exist); 'w' means create (overwrite if exists); 'w-' means create
(fail if exists).}

\item{cache_attrs}{: bool, optional
If True (default), user attributes will be cached for attribute read
operations. If False, user attributes are reloaded from the store prior
to all attribute read operations.}

\item{synchronizer}{: object, optional
Array synchronizer.}

\item{path}{: string, optional
Group path within store.}

\item{chunk_store}{: MutableMapping or string, optional
Store or path to directory in file system or name of zip file.}

\item{storage_options}{: dict
If using an fsspec URL to create the store, these will be passed to
the backend implementation. Ignored otherwise.}
}
\value{
ZarrGroup
}
\description{
Open a group using file-mode-like semantics.
}
