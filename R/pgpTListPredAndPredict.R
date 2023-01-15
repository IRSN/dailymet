##  ============================================================================

##' When using a 'time varying' model, some functions of the
##' \code{Date} variable are required to compute the prediction. The
##' \code{makeNewData} method can be used for that goal.
##' 
##' @title Prepare a Data Object for a Prediction
##'
##' @param object An object from a class having a \code{predict}
##'     method.
##'
##' @param ... Arguments for methods.
##' 
##' @return A "data" object that can be used as value for the
##'     \code{newdata} argument of the \code{predict} method.
##'
##' @export 
##' 
makeNewData <- function(object, ...) {
    UseMethod("makeNewData")
}

## =============================================================================

##' The object given in \code{object} can be used to compute a
##' prediction on  "new" data. 
##'
##' @title Prepare a Data Object for a Prediction
##'
##' @param object An object with class \code{"PgpTList"}
##'
##' @param newdata A "sketch of data" allowing the construction. This
##'     can be a \code{dailyMet} object, a data frame or simply a
##'     vector with class \code{"Date"}. With the default value, the
##'     new data is taken as the \code{dailyMet} object which is
##'     attached to \code{object}.
##'
##' @param trace Integer level of verbosity.
##' 
##' @param ... Arguments for methods.
##' 
##' @return A \code{dailyMet} object with the suitable variables.
##'
##' @method makeNewData pgpTList
##' 
##' @export
##'
makeNewData.pgpTList <- function(object, newdata = NULL, trace = 0, ...) {
    
    if (inherits(newdata, "data.frame")) {
        
        if (inherits(newdata, "dailyMet")) {
            if (trace) {
                cat("'newData' has class \"dailyMet\"")
            }
        } else {
            nm <- names(newdata)
            if (!(attr(object$dailyMet, "metVar") %in% nm)) {
                newdata <- cbind(newdata, .XXX = NA) 
                nm(newdata) <- c(nm, attr(object$dailyMet, "metVar"))
            }
            newdata <- dailyMet(data = newdata,
                                dateVar = "Date", 
                                metVar = attr(object$dailyMet, "metVar"),
                                station = attr(object$dailyMet, "station"),
                                id = attr(object$dailyMet, "id"),
                                trace = trace) 
        } 
    } else if (inherits(newdata, "Date")) {
        
        Date <- seq(from = min(newdata), to = max(newdata), by = "day")
        
        newdata <- data.frame(Date = Date, MetVar = as.numeric(NA))
        names(newdata) <- c("Date", attr(object$dailyMet, "metVar"))
        
        newdata <- dailyMet(data = newdata,
                            dateVar = "Date", 
                            metVar = attr(object$dailyMet, "metVar"),
                            station = attr(object$dailyMet, "station"),
                            id = attr(object$dailyMet, "id"),
                            trace = trace)
        
    }

    tau <- tau(object$threshold)
    indRef <- match(object$tauRef, tau(object$threshold))
    if (is.na(indRef)) {
        stop("'tauRef' must be found in tau(object$threshold)")
    }
    threshold.fun <- formula(object$threshold)
    
    Kthresh <- checkTrigNames(threshold.fun)
    trigDesign <- tsDesign(dt = newdata$Date,
                           type = "trigo", df = 2 * Kthresh + 1)
    newdata <- data.frame(newdata, trigDesign$X)
    
    ## Kscale <- checkTrigNames(scale.fun)
    ##  Kshape <- checkTrigNames(shape.fun)
    ##  K <- max(c(Kscale, Kshape))
    K <- 3
    
    if (trace) cat("o Adding new variables \n")
    Phi <- phases(coef(object$threshold))
    
    if (K) {
        phi <- Phi[indRef, ]
        if (trace) {
            cat("o Using K =", K, "and the following phases\n")
            print(round(phi, digits = 2))
        }
        
        sinDesignX <- sinBasis(dt = newdata[["Date"]],
                              df = 2 * K + 1,
                              phi = phi)
        newdata <- data.frame(newdata, sinDesignX)
        
    } else {        
        if (trace) {
            cat("No trigonometric variables needed.\n")
        }
    }
    ## Now prepare the time variables

    class(newdata) <- c("dailyMet", "data.frame")
    newdata
    
}


## *****************************************************************************

##'
##' @title Predict a `pgpTList` Object.
##' 
##' @param object A \code{pgpTList} object representing a list of
##'     Poisson-GP fitted models
##'
##' @param newdata \bold{Not implemented}. Now taken as
##'     \code{object@data}.
##' 
##' @param lastFullYear Logical. When \code{TRUE}, only the last full
##'     year in \code{newdata} will be used.
##'
##' @param trace Integer level of verbosity.
##' 
##' @param ... Not used yet.
##'
##' @return An object with class \code{"predict.pgpTList"} A data
##'     frame in long format. Among the columns we find \code{Date},
##'     \code{tau} and \code{u} and the NHPP parameters \code{muStar},
##'     \code{sigmaStar} and \code{xiStar}. The column \code{sigma}
##'     contains the GP scale parameter.
##'
##' @note Remind that the NHPP parameters do not depend on the
##'     threshold, although their estimates obviously do. They can be
##'     used to assess the sensitivity too the threshold choice.
##'
##' @section Caution: this method still may change.
##'
##' @method predict pgpTList
##'
##' @importFrom stats terms model.frame delete.response 
##' 
##' @export
##' 
predict.pgpTList <- function(object, newdata = NULL,
                             lastFullYear = FALSE,
                             trace = 0,
                             ...) {
    TX <- u <- NULL 
    
    if (!missing(newdata) && !is.null(newdata)) {
        missNewData <- FALSE
        warning("'newdata' is still experimental")
        newdata <- makeNewData(object, newdata = newdata,
                               trace = trace)
    } else {
        missNewData <- TRUE
        newdata <- object$data
    }
        
    pu <- predict(object$thresholds, newdata = newdata,
                  lastFullYear = lastFullYear)

    if (lastFullYear) {
        indLY <- lastFullYear(format(newdata$Date, "%Y-%m-%d"),
                                         out = "logical")
        newdata <- newdata[indLY, , drop = FALSE]
    } else {
        indLY <- rep(TRUE, nrow(newdata))
    }
    
    LambdaHat <- MuStar <- SigmaStar <- RL100 <- nExceed <- list()
    lambdaBar <- numeric(0)
    tau1 <- object$tau

    ## Non homogeneous case 
    if (object$fitLambda) {
        lambdaNH <- !(all.equal(object$logLambda.fun, ~1) == TRUE)
        if (lambdaNH) {
            Xtime <- model.matrix(object$logLambda.fun, data = newdata)
            Xtime <- cbind("b0" = rep(1, nrow(newdata)), Xtime)
            timeNH <- TRUE
        } 
    } 
    
    for (i in seq_along(tau1)) {

        ## ind <- object$IndLambda[[i]]
        
        MetWithu <- data.frame(newdata,
                               subset(pu, tau == tau1[i]))
        MetWithu <- within(MetWithu, Ex <- TX > u)

        if (missNewData) {
            ## mind that we need  a correction.
            LambdaHat[[i]] <- object$timePoisson[[i]]@lambdafit /
                mean(object$timePoisson[[i]]@lambdafit) * object$lambdaBar[i]
            
            LambdaHat[[i]] <- LambdaHat[[i]][indLY]
        } else { 
            if (object$fitLambda) {
                if (lambdaNH) {
                    LambdaHat[[i]] <- exp(Xtime %*% object$timePoisson[[i]]@coef) /
                        mean(object$timePoisson[[i]]@lambdafit) * object$lambdaBar[i]
                } else {
                    LambdaHat[[i]] <- rep(exp(object$timePoisson[[i]]@coef),
                                          nrow(newdata)) /
                        mean(object$timePoisson[[i]]@lambdafit) * object$lambdaBar[i]
                } 
            } else {
                LambdaHat[[i]] <- rep(object$lambdaBar[i], nrow(newdata))
            }
        }
        
        Theta <- theta(object$GP[[i]], data = newdata)
        MuStar[[i]] <- MetWithu$u + (LambdaHat[[i]]^Theta[ , "shape"] - 1) /
            Theta[ , "shape"] * Theta[ , "scale"]

        n1 <- length(MuStar[[i]])
        ## MuStar[[i]][1] <- MuStar[[i]][n1] <- NA
        
        SigmaStar[[i]] <- LambdaHat[[i]]^Theta[ , "shape"] * Theta[ , "scale"]
        
        RL100[[i]] <- MuStar[[i]] + SigmaStar[[i]] *
            ((-log(1 - 1 / 100))^(-Theta[ , "shape"]) - 1) / Theta[ , "shape"]
        
        dfRebuildi <- data.frame(TX = MetWithu$TX,
                                 Date = MetWithu$Date,
                                 DateRef = newdata$DateRef,
                                 Year = newdata$Year,
                                 Day = newdata$Day,
                                 u = MetWithu$u,
                                 tau = MetWithu$tau,
                                 lambda = LambdaHat[[i]],
                                 muStar = MuStar[[i]],
                                 sigma = Theta[ , "scale"],
                                 sigmaStar = SigmaStar[[i]],
                                 xiStar = Theta[ , "shape"],
                                 RL100 = RL100[[i]])

        ## Computations on the exceedances are moved into the `exceed`
        ## method
        
        if (i == 1) {
            dfRebuild <- dfRebuildi
            ## ex <- with(MetWithu[ind, ], tapply(Ex, Year, sum))
            ## dfExceed <- data.frame(tau = unname(tau1[i]),
            ##                        Year = unique(MetWithu[ind, ]$Year),
            ##                        Nb = ex)
            
        } else {
            dfRebuild <- rbind(dfRebuild, dfRebuildi, deparse.level = 0)
            ## ex <- with(MetWithu[ind, ], tapply(Ex, Year, sum))
            ## dfExceed <- rbind(dfExceed,
            ##                   data.frame(tau = unname(tau1[i]),
            ##                              Year = unique(MetWithu[ind, ]$Year),
            ##                              Nb = ex))
        }
        
        # nExceed[[i]] <- with(dfRebuildi, tapply(TX > u, Year, sum))
    
    }
    
    ## dfExceed <- within(dfExceed, tau <- as.factor(tau))
    ## dfExceed <- within(dfExceed, Date <- as.Date(sprintf("%4d-06-01", Year)))

    ## names(nExceed) <- paste0("tau=", format(tau1))
    
    ## list(ParamAndRL = dfRebuild,
    ##      Exceed = dfExceed,
    ##      nExceed = nExceed)

    attr(dfRebuild, "lastFullYear") <- lastFullYear
    attr(dfRebuild, "metVar") <- attr(object$dailyMet, "metVar")
    attr(dfRebuild, "station") <- attr(object$dailyMet, "station")
    attr(dfRebuild, "id") <- attr(object$dailyMet, "id")
    
    class(dfRebuild) <- c("predict.pgpTList", "data.frame")
    dfRebuild
    
}


##' @method print predict.pgpTList
##' @export
print.predict.pgpTList <- function(x, ...) {
    cat("Predicted values from a 'pgpTList' object\n")
    cat(sprintf("variable: \"%s\", station: \"%s\", id:  \"%s\"\n",
                attr(x, "metVar"), attr(x, "station"), attr(x, "id")))
}


## *****************************************************************************

##' Simulate from a `pgpTList` Object. F>or each of the thresholds of
##' the object, a collection of \code{n} random drawings of the
##' exeedance events and the related values are provided.
##'
##' The thresholds and hence the exceedance events are related to a
##' probability \code{tau} of non-exceedance which is quite high, say
##' 0.95 or more. Note that even if the GP model for the excesses over
##' the threshold is perfectly adequate, the simulated exceedance
##' events come at a rate which is smaller than the real rate because
##' the simulation is based on \emph{declustered} exceedance
##' events. So for a given date, if a large number \code{n} of
##' simulations is used, the frequency of the exceedance will be
##' smaller than the real one.
##' 
##' @title Simulate a `pgpTList` Object.
##' 
##' @param object A \code{pgpTList} object representing a list of
##'     Poisson-GP fitted models
##'
##' @param newdata \bold{Not implemented}. Now taken as
##'     \code{object@data}.
##' 
##' @param lastFullYear Logical used only when \code{newdata} is not
##'     provided. When \code{TRUE}, only the last full year in the
##'     data used when fitting the object will be used.
##'
##' @param trace Integer level of verbosity.
##' 
##' @param ... Not used yet.
##'
##' @return An object with class \code{"summary.pgpTList"}, inheriting
##'     from the \code{"data.frame"} class. This is a data frame in
##'     long format containing the simulated events with the
##'     corresponding simulated values of the meteorological variable.
##'
##' @section Caution: this method still may change.
##'
##' @method simulate pgpTList
##' 
##' @export
##' 
simulate.pgpTList <- function(object, n = 1,
                              newdata,
                              lastFullYear = FALSE,
                              tau = NULL,
                              trace = 0,
                              ...) {
    
    if (!missing(tau) && !is.null(tau)) {
        if (!all(tau %in% object$tau)) {
            stop("When given, 'tau' must provide values that are ",
                 "found in 'object$tau'")
        }
    } else {
        tau <- object$tau
    }

    ## XXX pass tau here as well???
    pred <- predict(object, newdata = newdata, lastFullYear = lastFullYear,
                    ...)
    iAll <- 1
    
    for (taui in tau) {
        
        predTau <- subset(pred, tau == taui)
        Lambda <- c(0, with(predTau, cumsum(lambda) / 365.25))
        nSim <- 3 * max(Lambda)
    
        ## ====================================================================
        ## 'TH' are the events of a Homogeneous PP on (0, n) with unit rate.
        ## 'TNH' are the events of the NHPP obtained by using the reciprocal
        ## function of the cumulated rate 'Lambda' according to
        ## 'TNH := Lambda^{-1}(TH)'.
        ## =====================================================================
        
        for (i in 1:n) {

            TH <- cumsum(rexp(nSim))
            TH <- TH[TH < Lambda[length(Lambda)]]

            if (length(TH)) {

                ## Caution: what about a zero value in TNH?
                TNH <- floor(approx(x = Lambda, y = seq_along(Lambda), xout = TH)$y)
                ## print(TNH)
                dup <- duplicated(TNH)
                
                if ((sum(dup) / length(TNH)) > 0.1) {
                    warning("More than 10% of the events are duplicated")
                }
                
                TNH <- TNH[!dup]
                
                if (trace) {
                    cat(sprintf("n = %d, tau = %4.2f, sim = %d, nb event = %d\n",
                                n, taui, i, length(TNH)))
                }
                
                ## here we could select a smaller number of variables
                resi <- predTau[TNH, ]
                
                ## relace 'TX' by a simulated value
                resi$TX <- resi$u + nieve::rGPD2(n = 1,
                                                 scale = resi$sigma,
                                                 shape = resi$xi)
                resi$Sim <- i
                resi
                
                if (iAll == 1) {
                    res <- resi
                } else {
                    res <- rbind(res, resi, deparse.level = 0)
                }
            }
            
            iAll <- iAll + 1
        }

        ## length(TNH) / (as.numeric(diff(range(p$Date))) / 365.25)
    }

    res <- within(res, Sim <- factor(Sim))
    attr(dfRebuild, "lastFullYear") <- lastFullYear
    attr(dfRebuild, "nSim") <- attr(object$dailyMet, "nSim")
    attr(dfRebuild, "metVar") <- attr(object$dailyMet, "metVar")
    attr(dfRebuild, "station") <- attr(object$dailyMet, "station")
    attr(dfRebuild, "id") <- attr(object$dailyMet, "id")

    
    class(res) <- c("simulate.pgpTList", "data.frame")
    res

}

##' @method print simulate.pgpTList
##' @export
print.simulate.pgpTList <- function(x, ...) {
    cat("Simulated values from a 'pgpTList' object\n")
    cat(sprintf("Number of simulation: %d", attr(x, "nSim")))
    cat(sprintf("variable: \"%s\", station: \"%s\", id:  \"%s\"\n",
                attr(x, "metVar"), attr(x, "station"), attr(x, "id")))
}
