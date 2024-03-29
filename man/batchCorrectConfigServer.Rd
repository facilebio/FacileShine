% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/batchCorrectConfig-module.R
\name{batchCorrectConfigServer}
\alias{batchCorrectConfigServer}
\title{A module to configure batch correction parameters for assay data.}
\usage{
batchCorrectConfigServer(id, rfds, ..., debug = FALSE)
}
\description{
This module enables the user to select covariates that should be regressed
out in the data. At the same time a covariate can be selected that indicates
structure in the data to preserve as the "main" covariate.
}
\details{
Covariates entered in \code{batch} are removed from the selections available in
\code{main}.
}
