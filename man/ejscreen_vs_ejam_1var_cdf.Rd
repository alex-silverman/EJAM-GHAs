% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ejscreen_vs_ejam_.R
\name{ejscreen_vs_ejam_1var_cdf}
\alias{ejscreen_vs_ejam_1var_cdf}
\title{EJAM/EJSCREEN comparisons - see cumulative distribution for 1 variable after using ejscreen_vs_ejam()
Plot distribution of absolute values of Percent Differences in 1 indicator at tested sites}
\usage{
ejscreen_vs_ejam_1var_cdf(vs, varname = "pop")
}
\arguments{
\item{vs}{output of ejscreen_vs() or similar}

\item{varname}{like "pop" or any of colnames(testoutput_ejamit_10pts_1miles$results_bysite)
that are among those in
\code{colnames(ejscreen_vs_ejam(testpoints_10[1:2,], save_ejscreen_output = F ))}}
}
\value{
CDF plot
}
\description{
EJAM/EJSCREEN comparisons - see cumulative distribution for 1 variable after using ejscreen_vs_ejam()
Plot distribution of absolute values of Percent Differences in 1 indicator at tested sites
}
\keyword{internal}
