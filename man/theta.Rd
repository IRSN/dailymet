% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fevdMethods.R
\name{theta}
\alias{theta}
\title{Compute the GP or GEV Coefficients for an Object with Class
    \code{"fevd"}}
\usage{
theta(object, data = NULL)
}
\arguments{
\item{object}{The object. It must represent the result of a POT
model fitting.}

\item{data}{A data frame containing the data for which the model
will be used.}
}
\value{
A matrix with two or three columns representing the
coefficients.
}
\description{
Compute the GP or GEV coefficients as a matrix for an object with
class \code{"fevd"} as created using \code{extRemes::fevd}.
}
\note{
This method essentially does the same thing as
    \code{\link[extRemes]{findpars}}. However \code{findpars} will
    try to give the GPD \code{location} i.e. the threshold for
    some \code{fevd} objects at least.
}
\examples{
data(Fort)
tau <- 0.97
u0 <- quantile(Fort$Prec, prob = tau)
fit0 <- fevd(x = Prec, data = Fort, threshold = u0, type = "GP")
theta(fit0)

}
\seealso{
\code{\link[extRemes]{findpars}}
}
