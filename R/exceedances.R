
##  ****************************************************************************

##' Pseudo-generic (S3) function for which methods can be implemented.
##' 
##' @title Exceedances Over Threshold(s)
##'
##' @param object An object from which exceedances can be exctracted.
##'
##' @param ... Arguments for further methods.
##' 
##' @return Some information on the exceedances over the threshold(s).
##'
##' @export
##' 
exceed <- function(object, ...) {
    UseMethod("exceed")
}

## *****************************************************************************

##' Extracts the exceedances from a \code{pgpTList} object. The
##' typical use of the result is in a ggplot showing the temporal
##' evolution of the number of exceedances. The annual number of
##' exceedances can be plotted along with the rate of a time-varying
##' Poisson process inasmuch this rate is expressed in inverse year
##' \eqn{\text{yr}^{-1}}{yr^{-1}}.
##'
##' Even when \code{byYear} is \code{TRUE}, the returned data frame
##' has a \code{Date} column that can be used in plots.
##' 
##' @title Exceedances Over Threshold(s)
##'
##' @param object The object from which the exceedances are to be
##'     exctraced..
##'
##' @param byYear Logical. If \code{TRUE}, the exceedances are
##'     summarised as a number by year.
##' 
##' @param ... Not used.
##'
##' @return A data frame in "long format" row-binding timeseries
##'     relative to the exceedances for each of the thresholds used in
##'     \code{object}. 
##'
##' @export
##'
##' @method exceed pgpTList
##' 
exceed.pgpTList <- function(object, byYear = TRUE,  ... ) {

    TX <- u <- Year <- NULL ## avoid warnings due to symbols for variables

    if (!byYear) stop("only `byYear = TRUE`is available for now")
    
    tau1 <- tau(object$thresholds)
    df <- object$data
    IndLambda <- object$IndLambda

    pu <- predict(object$thresholds, lastFullYear = FALSE)
    
    for (i in seq_along(tau1)) {

        ind <- IndLambda[[i]]
        MetWithu <- data.frame(object$data,
                               subset(pu, tau == tau1[i]))
        MetWithu <- within(MetWithu, Ex <- TX > u)
        if (i == 1) {
            ex <- with(MetWithu[ind, ], tapply(Ex, Year, sum))
            dfExceed <- data.frame(tau = unname(tau1[i]),
                                   Year = unique(MetWithu[ind, ]$Year),
                                   Nb = ex) 
        } else {
            ex <- with(MetWithu[ind, ], tapply(Ex, Year, sum))
            dfExceed <- rbind(dfExceed,
                              data.frame(tau = unname(tau1[i]),
                                         Year = unique(MetWithu[ind, ]$Year),
                                         Nb = ex))
        }
    }

    ## add some columns and reorder the columns
    dfExceed <- within(dfExceed, tau <- as.factor(tau))
    dfExceed <- within(dfExceed, Date <- as.Date(sprintf("%4d-06-01", Year)))
    dfExceed <- dfExceed[ , c("Year", "Date", "tau", "Nb")]
    rownames(dfExceed) <- NULL
    dfExceed
}

    

