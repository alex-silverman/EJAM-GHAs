% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ejscreenapi.R
\name{ejscreenapi}
\alias{ejscreenapi}
\title{Use EJScreen API to get stats on one or more circular buffers}
\usage{
ejscreenapi(
  lon,
  lat,
  radius = 3,
  unit = "miles",
  wkid = 4326,
  fips = NULL,
  shapefile = NULL,
  namestr = "",
  report_every_n = 1000,
  save_when_report = FALSE,
  format_report_or_json = "pjson",
  on_server_so_dont_save_files = FALSE,
  ipurl = "ejscreen.epa.gov",
  updateProgress = NULL,
  drop_redundant_indicators = TRUE,
  nicenames = FALSE,
  verbose = TRUE,
  getstatefromplacename = TRUE
)
}
\arguments{
\item{lon}{Longitude numeric vector}

\item{lat}{Latitude numeric vector}

\item{radius}{radius of circular buffer (uses a default if none specified)}

\item{unit}{"miles" (default) or "kilometers"}

\item{wkid}{optional spatial reference code, if default is not what is needed}

\item{fips}{if used instead of lon,lat it should be a character FIPS code vector
(counties, tracts, or blockgroups)}

\item{shapefile}{not implemented}

\item{namestr}{optional text}

\item{report_every_n}{Should it report ETA snd possibly save interim file after every n points}

\item{save_when_report}{optional, write .rdata file to working directory
with results so far, after ever n points, to have most results even if it crashes}

\item{format_report_or_json}{default is pjson but could modify to allow it to be report to get just a pdf URL
but that also can be gotten via \code{\link[=url_ejscreen_report]{url_ejscreen_report()}}}

\item{on_server_so_dont_save_files}{FALSE by default, but TRUE prevents saving any progress or crash-related files}

\item{ipurl}{which URL or IP to try}

\item{updateProgress}{Used to create progress bar in Shiny app}

\item{drop_redundant_indicators}{Set to FALSE if you do not want to exclude from results the indicators that appear twice
just under 2 different names, like RAW_D_LIFEEXP and RAW_HI_LIFEEXPPCT which are identical.}

\item{nicenames}{Set it to TRUE if you want to have it rename headers as long friendly plain English not R variable names
but note downstream functions mostly expect rname format
that uses \code{\link[=ejscreenapi1]{ejscreenapi1()}} and \code{\link[=ejscreenRESTbroker]{ejscreenRESTbroker()}}  and \code{\link[=ejscreenRESTbroker2table]{ejscreenRESTbroker2table()}}}

\item{verbose}{whether to print to console / viewer / plot}

\item{getstatefromplacename}{set to FALSE if you need the exact output of API and
TRUE if you want to try to extract ST abbrev and statename from the placename field,
which is more likely to be correct than the stateAbbr and stateName fields in the API output.}
}
\description{
Get a data.table of EJScreen report results for one or multiple circular buffers.
}
\details{
Specify a radius and vector of latitude longitude points,
and get for a buffer the population weighted mean value of each raw indicator
like percent low-income, and total population count, and percentiles for those
raw indicator scores, all from EJScreen, as in an EJScreen standard report.

The functions \code{\link[=ejscreenapi_plus]{ejscreenapi_plus()}} and \code{\link[=ejscreenit]{ejscreenit()}} are higher-level functions
that provide renamed variables in their outputs.
Column names returned by ejscreenapi() are those provided by the EJScreen API,
e.g., RAW_D_INCOME, not the renamed variables like pctlowinc etc. or Percent Low Income

\code{\link[=ejscreenRESTbroker]{ejscreenRESTbroker()}} is the lowest level function here for access to the API.

\code{\link[=ejscreenRESTbroker2table]{ejscreenRESTbroker2table()}} converts that to a table.

\code{ejscreenRESTbroker2table(ejscreenRESTbroker())}
returns the same 1-row data.frame as ejscreenapi1()
except the latter drops the percent signs and makes those values numeric,
converting text like 45\% to the number 45.

This also drops redundant columns where the same numbers had been returned from API
using the normal name and a synonym name, as with TOTALPOP and "totalPop"

To compare API variable names and renamed versions:

\if{html}{\out{<div class="sourceCode">}}\preformatted{x <- ejscreenapi(-100, 40, 1)
cbind(names(x), fixnames(names(x)))
}\if{html}{\out{</div>}}

Note this and related functions could be recoded using httr2 and some
best practices, as described here: \url{https://httr.r-lib.org/articles/api-packages.html}.

This relies on \code{\link[=ejscreenapi1]{ejscreenapi1()}} to request URL of pdf report on each site
via the API, and does some error checking, but like \code{\link[=ejscreenapi1]{ejscreenapi1()}} it does a GET
request via API and then parses the JSON results from the GET request, cleans it up,
adds URLs as links, compiles it as a data.table,
enables a progress bar, etc.

Note that this API is fairly slow, so it is fine for 10 sites, but not large numbers.
It varies, but can run about 1k to 10k sites per hour,
for circular buffers of 1 or 3 mile radius.
It sometimes fails to obtain results,
which may be caused by unreliable results from the API
rather than the code requesting results via the API.

See  \url{https://web.archive.org/web/20250118193121/https://www.epa.gov/ejscreen/ejscreen-api}
}
\examples{
 
 \donttest{
 # Specify size of buffer circle and pick random points as example data
 myradius <- 1
 pts2 <- data.frame(lon = c(-111.9040233, -73.7917865), lat = c(33.5604162, 41.0613821))
 pts5 <- testpoints_5
 
 out <- ejscreenapi(lon = pts2$lon, lat = pts2$lat, radius = myradius)
 head(t(out))
 outnice <- ejscreenapi(lon = pts2$lon, lat = pts2$lat, radius = myradius, nicenames = TRUE)
 head(t(outnice), 24)
 }
 
}
\seealso{
\code{\link[=ejscreenit]{ejscreenit()}} \code{\link[=ejscreenapi_plus]{ejscreenapi_plus()}} \code{\link[=ejscreenapi]{ejscreenapi()}}
}
\keyword{internal}
