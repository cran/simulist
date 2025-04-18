% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{.add_missing}
\alias{.add_missing}
\title{Introduce user-specified proportion of custom missing values into a
\verb{<data.frame>}}
\usage{
.add_missing(linelist, .args)
}
\arguments{
\item{linelist}{Line list \verb{<data.frame>} output from \code{\link[=sim_linelist]{sim_linelist()}}.}

\item{.args}{A list of setting from \code{\link[=messy_linelist]{messy_linelist()}}.}
}
\value{
A line list \verb{<data.frame>}
}
\description{
Introduce user-specified proportion of custom missing values into a
\verb{<data.frame>}
}
\keyword{internal}
