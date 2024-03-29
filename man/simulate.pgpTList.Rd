% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pgpTListPredSim.R
\name{simulate.pgpTList}
\alias{simulate.pgpTList}
\title{Simulate a `pgpTList` Object.}
\usage{
\method{simulate}{pgpTList}(
  object,
  nsim = 1,
  seed = NULL,
  newdata,
  lastFullYear = FALSE,
  tailOnly = TRUE,
  tau = NULL,
  trace = 0,
  how = c("list", "vector", "data.frame"),
  ...
)
}
\arguments{
\item{object}{A \code{pgpTList} object representing a list of
Poisson-GP fitted models}

\item{nsim}{Number of simulation wanted.}

\item{seed}{Not used yet.}

\item{newdata}{An optional object that giving the "new" dates for
which the simulation will be done. It can simply be an object
with class \code{"Date"} or an object with class
\code{"dailyMet"}. Depending on the class of \code{newdata},
the variables required for the prediction (such as sine waves)
will be recomputed or not. When \code{newdata} is not given or
is \code{NULL}, \code{object$data} is used.}

\item{lastFullYear}{Logical, used only when \code{newdata} is not
provided or is \code{NULL}. When \code{TRUE}, only the last
full year in the data used when fitting the object will be
used.}

\item{tailOnly}{Logical. If \code{TRUE} the simulation will be
only for tail events. More precisely only for each of the
threshold \eqn{u_i(t)} corresponding to a non-exceedance
probability \eqn{\tau_i} the maximum \eqn{v_i:=\max_t
u_i(t)} of the the thresholds over the predicted period is
computed, and the simulated events correspond to exceedances
over \eqn{v_i}.}

\item{tau}{If provided, the simulation is only made for the
provided value of \code{tau}.}

\item{trace}{Integer level of verbosity.}

\item{how}{A technical argument telling how the big data frame on
output is to be constructed by concatenation. The
\code{"list"} method (default and recommended) makes a list
containing a large number of data frames, one by simulation
and then makes use of the very efficient
\code{\link[data.table]{rbindlist}} function. With
\code{"vector"}, once concatenates vectors, and with
\code{"data.frame"} one rbinds data fames at each
simulation. The tow later options are by far slower when
\code{n} is large, say \code{n > 100}.}

\item{...}{Not used yet.}
}
\value{
An object with class \code{"summary.pgpTList"}, inheriting
    from the \code{"data.frame"} class. This is a data frame in
    long format containing the simulated events with the
    corresponding simulated values of the meteorological variable.
}
\description{
Simulate from a `pgpTList` Object. For each of the thresholds of
the object, a collection of \code{n} random drawings of the
exeedance events and the related values are provided.
}
\details{
The thresholds and hence the exceedance events are related to a
probability \code{tau} of non-exceedance which is quite high, say
0.95 or more. Note that even if the GP model for the excesses over
the threshold is perfectly adequate, the simulated exceedance
events come at a rate which is smaller than the real rate because
the simulation is based on \emph{declustered} exceedance
events. So for a given date, if a large number \code{n} of
simulations is used, the frequency of the exceedance will be
smaller than the real one.
}
\note{
In the returned data frame, the (factor) column \code{Sim}
    gives the simulation number. It can happen that a simulation
    generates no event, especially if \code{tau} is close to 1
    and if the simulation period is short.
}
\section{Caution}{
 the returned data frame can be very large since
    \code{n} simulations are made for each value of \code{tau},
    and each simulation can will typically give several dozens of
    events.
}

\examples{
RqU <- rqTList(dailyMet = Rennes, tau = c(0.94, 0.95, 0.96, 0.97, 0.98))
Pgp1 <- pgpTList(dailyMet = Rennes, thresholds = RqU, declust = TRUE,
                 fitLambda = TRUE, logLambda.fun = ~YearNum - 1)
Date <- seq(from = as.Date("2022-01-01"), to = as.Date("2043-01-01"), by = "day")
st <- system.time(sim <- simulate(Pgp1, nsim = 8, newdata = Date, trace = 1))
autoplot(subset(sim, tau == 0.95))
## more realistic example
\dontrun{
    pred <- predict(Pgp1, newdata = Date)
    st <- system.time(sim <- simulate(Pgp1, nsim = 1000, newdata = Date, trace = 1))
    ## compute the maximum 'M' on the period by simulation and by threshold
    M <- with(sim, tapply(TX, list(Sim, tau), max))
    ## quantile over the simulation (by threshold)
    apply(M, 2, quantile, prob = 0.99, na.rm = TRUE)
    ## compare with the computation
    quantile(pred, p = 0.99)
}
}
