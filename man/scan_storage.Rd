% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scan_storage.R
\name{scan_storage}
\alias{scan_storage}
\title{Scan Storage}
\usage{
scan_storage(data_dir = NULL, save_dir = NULL)
}
\arguments{
\item{data_dir}{Directory where to scan for files.}

\item{save_dir}{Directory where to save the results from the storage scan.}
}
\description{
Get name and size for all files in a user defined root directory and subdirectories.
}
\examples{
scan_storage(data_dir="/mnt/d/", save_dir="/mnt/d/results/")
scan_storage(data_dir="c:/ProjectX/Data/")
}
