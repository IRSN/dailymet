% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplotStuff.R
\name{autoplot.phasesMatrix}
\alias{autoplot.phasesMatrix}
\title{Autoplots a \code{phasesMatrix} Object}
\usage{
\method{autoplot}{phasesMatrix}(object, tauRef = 0.95, ampl = TRUE, ...)
}
\arguments{
\item{object}{An object with class \code{"phasesMatrix"}.}

\item{tauRef}{Value of the probability \code{tau} to be used as
reference in the comparison.}

\item{ampl}{Logical. If \code{TRUE} the phases are shown through
the sine waves weighted by their amplitude \eqn{\gamma_k}. If
\code{FALSE} the sine waves are not weighted. The later option
can be used to better asses the variation of phase for the
higher harmonics since these usually have smaller amplitudes.}

\item{...}{Further arguments to be passed to \code{geom_line}.}
}
\value{
An object inheriting from \code{"ggplot"}.
}
\description{
Autoplots a \code{phasesMatrix} Object
}
