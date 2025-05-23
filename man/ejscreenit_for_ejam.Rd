% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ejscreenit_for_ejam.R
\name{ejscreenit_for_ejam}
\alias{ejscreenit_for_ejam}
\title{Get EJScreen results directly from EJScreen servers via their API}
\usage{
ejscreenit_for_ejam(
  sitepoints,
  radius = 3,
  fips = NULL,
  shapefile = NULL,
  namestr = "",
  nosave = TRUE,
  nosee = TRUE,
  fillmissingcolumns = FALSE,
  ...
)
}
\arguments{
\item{sitepoints}{table with lat and lon columns}

\item{radius}{in miles}

\item{fips}{fips code (or possibly a vector of fips codes)}

\item{shapefile}{not implemented}

\item{namestr}{optional text}

\item{nosave}{passed to \code{\link[=ejscreenit]{ejscreenit()}}}

\item{nosee}{passed to \code{\link[=ejscreenit]{ejscreenit()}}}

\item{fillmissingcolumns}{passed to \code{\link[=ejscreenapi2ejam_format]{ejscreenapi2ejam_format()}}}

\item{...}{other parameters passed to \code{\link[=ejscreenit]{ejscreenit()}}}
}
\value{
a data.table that looks like output of ejamit()$results_bysite
}
\description{
A wrapper for EJAM to use ejscreenit()
}
\seealso{
\code{\link[=ejscreen_vs_ejam]{ejscreen_vs_ejam()}} \code{\link[=ejscreenapi2ejam_format]{ejscreenapi2ejam_format()}} which it uses.
}
