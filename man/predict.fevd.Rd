% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fevdMethods.R
\name{predict.fevd}
\alias{predict.fevd}
\title{Predicted Values based on a \code{fevd} Object}
\usage{
\method{predict}{fevd}(object, newdata = NULL, level = 0.99, threshold = NULL, trace = 1, ...)
}
\arguments{
\item{object}{An object with class \code{"fevd"} representing a
non-stationary POT model, either Poisson-GP (\code{type =
"GP"}) or NHPP (\code{type = "PP"}).}

\item{newdata}{A data frame containing the covariates.}

\item{level}{Confidence level.}

\item{threshold}{The threshold. This can be either a suitable
vector of an object representing a quantile regression,
inheriting from \code{"rq"}.}

\item{trace}{Integer level of verbosity.}

\item{...}{Not used yet}
}
\value{
A matrix with its rows matching those of \code{newdata}
    and its columns matching the parameters of the marginal
    distribution, Generalized Pareto (GP) or Generalized
    Extreme-Value (GEV).
}
\description{
This method computes the parameters of the marginal
    distribution (\code{GP} or \code{GEV}) for the response. This
    marginal distribution depends on the covariates used in
    `object`, the values of which are to be given in
    \code{newdata}.
}
\section{Caution}{
 This method is highly experimental since the
    \code{extRemes::fevd} package does not store all all the
    information required to make the prediction. The vector of
    coefficients for a covariate-dependent threshold is not
    stored.
}

\examples{
## use centimetres as precipitation unit
data(Fort) 
Fort <- within(Fort, Prec <- 2.54 * Prec)
tau <- 0.97; u0 <- quantile(Fort$Prec, prob = tau)
fit0 <- fevd(x = Prec, data = Fort, threshold = u0, type = "GP")
p0 <- predict(fit0)
rq <- rq(Prec ~ cos(2 * pi * tobs / 365.25) + sin(2 * pi * tobs / 365.25),
         data = Fort, tau = tau)
summary(rq)
## compute the threshold and extract the fitted coefficients
u <- predict(rq, newdata = Fort)
beta_u <- coef(rq)
fit1 <- fevd(x = Prec, data = Fort, type = "GP",
             scale.fun = ~ cos(2 * pi * tobs / 365.25) + sin(2 * pi * tobs / 365.25),
             threshold = beta_u,
             threshold.fun = ~ cos(2 * pi * tobs / 365.25) + sin(2 * pi * tobs / 365.25))
p1 <- predict(fit1)

}
