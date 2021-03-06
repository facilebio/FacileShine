% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quantitativeTraitSelect-module.R
\name{quantitativeTraitSelect}
\alias{quantitativeTraitSelect}
\alias{quantitativeTraitSelectUI}
\title{Provides a widget to select quantitative traits from samples}
\usage{
quantitativeTraitSelect(
  input,
  output,
  session,
  rfds,
  ...,
  .exclude = NULL,
  .reactive = TRUE
)

quantitativeTraitSelectUI(id, ...)
}
\description{
This module enables the user to pull quantitative traits of all flavors
(ie. from assay data, or quantitative pData) from a \code{FacileDataStore}.
This module doesn't return the actual quantitative values selected for, but
rather parameterizations of the widget that the caller can use to extract
the values for the \code{FacileDataStore}.
}
\details{
We assume that there are two sources of quanitative traits:
\enumerate{
\item From assays (usually, dense matrix types of things (rnaseq)). These data
are accessed within this module using the \code{\link[=assayFeatureSelect]{assayFeatureSelect()}}
module; or
\item A quantitative sample covariate (RIN score, for instance). These data are
accessed within using the \code{\link[=quantitativeSampleCovariateSelect]{quantitativeSampleCovariateSelect()}} module.
}
}
