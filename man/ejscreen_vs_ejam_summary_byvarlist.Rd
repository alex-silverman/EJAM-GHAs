% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ejscreen_vs_ejam_.R
\name{ejscreen_vs_ejam_summary_byvarlist}
\alias{ejscreen_vs_ejam_summary_byvarlist}
\title{EJAM/EJSCREEN comparisons - see summary stats after ejscreen_vs_ejam() for JUST 1 VARLIST at a time, just 2-3 key stats
What percent of sites have agreement to +-1\%? etc.}
\usage{
ejscreen_vs_ejam_summary_byvarlist(
  vs = NULL,
  vsum = NULL,
  myvars = colnames(vs$EJAM),
  tol = 0.01
)
}
\arguments{
\item{vs}{from ejscreen_vs() can be provided (if vsum is not provided)}

\item{vsum}{from ejscreen_vs_ejam_summary() can be provided if vs is not but then tol is ignored
because it was already defined in creating vsum}

\item{tol}{fraction of 1, the percentage tolerance for agreement so 0.01 means agree to +/-1\%}
}
\value{
table of a few rounded metrics like what percent of sites agree to within 1\%? returned invisibly
for the long list of indicators
}
\description{
EJAM/EJSCREEN comparisons - see summary stats after ejscreen_vs_ejam() for JUST 1 VARLIST at a time, just 2-3 key stats
What percent of sites have agreement to +-1\%? etc.
}
\keyword{internal}
