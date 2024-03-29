% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/miscUtils.R
\name{lastFullYear}
\alias{lastFullYear}
\title{Find the Last Full Year}
\usage{
lastFullYear(dateTxt, leap = FALSE, out = c("character", "logical"))
}
\arguments{
\item{dateTxt}{A character vector giving a date in POSIX format.}

\item{leap}{Logical if \code{TRUE}, the last full leap year
will be returned.}

\item{out}{Character. When given the value \code{"character"}, a
character vector extracted from \code{dataeTxt} is
returned. When the value \code{logical} is given, a logical
vector is returned, indicating the elements in \code{dateTxt}.}
}
\value{
A logical vector that can be used to subset.
}
\description{
Find the last full year in a character vector representing
successive dates.
}
\details{
By "full" year, we mean a year with at least 365 days.
}
