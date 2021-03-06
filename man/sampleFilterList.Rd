% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampleFilterList-module.R
\name{sampleFilterList}
\alias{sampleFilterList}
\title{A dynamic list of sampleFilter modules.}
\usage{
sampleFilterList(
  input,
  output,
  session,
  rfds,
  default_covariate = NULL,
  ...,
  debug = FALSE
)
}
\value{
A \code{"FacileSampleFilterList"}. Use \code{active_samples(this)} to retrieve
the currently specified cohort of samples.
}
\description{
This module provides an interface that enables the user to add an abirtray
amount of sampleFilters that are AND-chained together to specify a cohort
of samples to focus analysis on. Sample filters can only be manipulated
(changed or removed) in a LIFO order.
}
\details{
The \verb{dataset,sample_id} sample selector that has been narrowed within the
returned \code{SampleFilterList} can be retrieved using the \code{active_samples()}
function, ie.\preformatted{sfl <- callModule(sampleFilterList, rfds)
samples <- reactive(active_samples(sfl))
}
}
\section{Implementation Notes}{

This is implemented by dynammicaly materializing \code{sampleFilter} modules
and inserting them into the application using \code{\link[shiny:insertUI]{shiny::insertUI()}} and
\code{\link[shiny:insertUI]{shiny::removeUI()}}.

This implementation largely inspired from the \code{filter_module.R} file in Joe
Cheng's rpharma-demo shiny app and a post from Barret Schloerke in the
RStudio community forums (cf. the "See Also" section).

Note that the UI that generated by \code{insertUI(ui = sampleFilterUI(ns(id)))}
was seemingly detached from its sister \code{callModule(sampleFilter, id, ...)}
server-side code until the \verb{insertUI,immediate} parameter was set to \code{TRUE}.
When \code{immediate = FALSE}, the select inputs in the \code{sampleFilterUI} element
were not initialized/updated from within \code{sampleFilter} module.
}

\seealso{
\code{\link[=FilteredReactiveDataStore]{FilteredReactiveDataStore()}},
\href{https://github.com/jcheng5/rpharma-demo}{Joe Cheng's rpharm-demo app},
\href{https://community.rstudio.com/t/22617/5}{Barret's Post}
}
