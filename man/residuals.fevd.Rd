% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fevdMethods.R
\name{residuals.fevd}
\alias{residuals.fevd}
\title{Generalized Residuals for some \code{fevd} Objects}
\usage{
\method{residuals}{fevd}(object, type = c("exp", "unif"), ...)
}
\arguments{
\item{object}{An object of class \code{"fevd"} representing a
fitted POT model, either Poisson-GP if \code{object$type} is
\code{"GP"} or a Non-Homogeneous Poisson Process if
\code{object$type} is \code{"PP"}.}

\item{type}{Character giving the target distribution: \code{"exp"}
for the standard exponential or \code{"unif"} for the standard
exponential.}

\item{...}{Not used yet}
}
\value{
A numeric vector with the generalized residuals.
}
\description{
Compute the generalized residuals for some objects with class
\code{"fevd"} representing fitted POT models, especially
non-stationary ones.
}
\note{
For Poisson-GP POT models, the residuals will \code{NA} when
    the observation \eqn{y} does not exceed the threshold \eqn{u}
    i.e., when \eqn{y \leq u}.
}
