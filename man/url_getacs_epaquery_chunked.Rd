% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/URL_FUNCTIONS_part2.R
\name{url_getacs_epaquery_chunked}
\alias{url_getacs_epaquery_chunked}
\title{URL functions - url_getacs_epaquery_chunked - experimental/ work in progress: in chunks, get ACS data via EPA API}
\usage{
url_getacs_epaquery_chunked(
  objectIds = 1:3,
  servicenumber = 7,
  outFields = NULL,
  returnGeometry = FALSE,
  justurl = FALSE,
  chunksize = 200,
  ...
)
}
\arguments{
\item{objectIds}{see API}

\item{servicenumber}{see API}

\item{outFields}{see API}

\item{returnGeometry}{see API}

\item{justurl}{see API}

\item{chunksize}{eg 200 for chunks of 200 each request}

\item{...}{passed to url_getacs_epaquery()}
}
\value{
table
}
\description{
URL functions - url_getacs_epaquery_chunked - experimental/ work in progress: in chunks, get ACS data via EPA API
}
\examples{
\donttest{
 # x <- list() # chunked chunks. best not to ask for all these:
 # x[[1]] <- url_getacs_epaquery_chunked(   1:1000, chunksize = 100)
 # x[[2]] <- url_getacs_epaquery_chunked(1001:5000, chunksize = 100)
 # xall <- do.call(rbind, x)
 } 
}
\keyword{internal}
