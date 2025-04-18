% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkers.R
\name{.check_df}
\alias{.check_df}
\title{Check if \verb{<data.frame>} defining either age-stratified hospitalisation or
death risk, or defining age structure of population is correct}
\usage{
.check_df(x, df_type = c("risk", "age"), age_range = NULL)
}
\arguments{
\item{x}{A \verb{<data.frame>}.}

\item{df_type}{A \code{character} string, either \code{"risk"} or \code{"age"} to specify
which input \verb{<data.frame>} is being checked.}

\item{age_range}{A \code{numeric} vector of length 2. Only required when
\code{df_type = risk}, \code{NULL} by default.}
}
\value{
A \verb{<data.frame>}, also called for error side-effects when input is
invalid.
}
\description{
Check if \verb{<data.frame>} defining either age-stratified hospitalisation or
death risk, or defining age structure of population is correct
}
\keyword{internal}
