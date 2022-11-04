
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

##' @title Exceedances Over Threshold(s)
##'
##' @param object The object to plot.
##'
##' @param ... Not used.
##'
##' @export
##'
##' @method exceed pgpTList
##' 
exceed.pgpTList <- function(object, ... ) {

    TX <- u <- Year <- NULL ## avoid warnings due to symbols for variables
      
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
    
    dfExceed <- within(dfExceed, tau <- as.factor(tau))
    dfExceed <- within(dfExceed, Date <- as.Date(sprintf("%4d-06-01", Year)))
    dfExceed <- dfExceed[ , c("Year", "Date", "tau", "Nb")]
    rownames(dfExceed) <- NULL
    dfExceed
}

    

