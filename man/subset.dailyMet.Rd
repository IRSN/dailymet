% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dailyMet.R
\name{subset.dailyMet}
\alias{subset.dailyMet}
\title{Subset the data part of a \code{dailyMet} object}
\usage{
\method{subset}{dailyMet}(x, ...)
}
\arguments{
\item{x}{An objects with class \code{"dailyMet"}.}

\item{...}{Arguments passed to the \code{subset} method of
the \code{"data.frame"} class.}
}
\value{
An object with class \code{"dailyMet"} corresponding to
    the subsetted data.
}
\description{
Subset the data part of a \code{dailyMet} object
}
\section{Caution}{
 this method is only intended to be used to
    \code{subset} (observations), not to \code{select}
    (variables). It works by giving a "subsetting condition" as
    illustrated in \bold{Examples}.
}

\examples{
Rennes1 <- subset(Rennes, Date < as.Date("2020-01-01"))

}
