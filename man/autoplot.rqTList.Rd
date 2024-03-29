% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplotStuff.R
\name{autoplot.rqTList}
\alias{autoplot.rqTList}
\title{Autoplot Method for the Class \code{"rqTList"}.}
\usage{
\method{autoplot}{rqTList}(object, lastFullYear = TRUE, ...)
}
\arguments{
\item{object}{The \code{rqTList} object.}

\item{lastFullYear}{Logical. If \code{TRUE} the prediction will be
computed only on the last full year of the data used in the
fit. See \code{\link{predict.rqTList}}.}

\item{...}{Arguments passed to \code{\link{geom_line}} such as
\code{linetype}, \code{size}.}
}
\description{
Create an object inheriting from the \code{"ggplot"} class showing
the quantile regression curves (i.e. the threshold curves) against
the date.
}
\examples{
\dontrun{ 
    RqRennes <- rqTList(dailyMet = Rennes)
    g <- autoplot(RqRennes) 
    stat <- findStationMF("bordeaux-me")
    ## you may here have to use `Sys.setenv(metData = xxx)`
    Bordeaux <- readMet(stat)
    RqBordeaux <- rqTList(dailyMet = Bordeaux)
    g <- g + autolayer(RqBordeaux, linetype = "dashed", size = 1.2) +
         ggtitle(paste0("Quantile regression for Rennes (solid thin line) ",
                        "and Bordeaux (dashed thick line)"))
    g
}
}
