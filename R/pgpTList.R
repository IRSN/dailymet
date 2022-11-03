## *****************************************************************************

##' Fit a non-stationary Poisson-GP model for several thresholds given
##' in the \code{rqTList} object \code{thresholds} corresponding to a
##' vector of probability.
##'
##' For each probability \code{tau[i]}
##' \itemize{
##'     \item{ }{
##'         Find the exceedances over the threshold corresponding
##'         to the probability \code{tau[i]}, and decluster these
##'         if wanted.
##'     }
##'     \item{ }{
##'         Fit a non-stationary GP model using
##'         \code{extRemes::fevd} with \code{type = "GP"} and
##'         with the formulas prescribed.
##'      }
##'      \item{ }{
##'         Fit a non-stationary Poisson process model using
##'         \code{NHPoisson::xxx} with \code{type = "GP"} 
##'      }
##' }
##'
##' @title Fit a non-stationary Poisson-GP Model using several
##'     Thresholds computed by Quantile Regression.
##'
##' @param dailyMet An object with class \code{"dailyMet"} containing
##'     the data.
##'
##' @param thresholds An object with class \code{"rqTList"} containing
##'     the thresholds.
##'
##' @param declust Logical. If \code{TRUE} the exceedances over each
##'     threshold will be declustered.
##'
##' @param tauRef The reference probability defining the quantile
##'     regression fit that will be used to define the phases of the
##'     sine waves.
##'
##' @param fitLambda Logical. If \code{TRUE} the temporal Poisson
##'     process is fitted, using the formula \code{logLambda.fun}.
##' 
##' @param logLambda.fun Formula for the log-rate of the time Poisson 
##'     process. For now, there are only a few possibilities. See
##'     \bold{Details}. Mind that
##'     \bold{only numeric covariates can be used}.
##'
##' @param scale.fun,shape.fun Formulas for the GP scale and shape as
##' in \code{\link[extRemes]{fevd}}.
##' 
##' @param trace Integer level of verbosity. 
##'
##' @return An object with class \code{"pgpTList"}. This is a list with
##'     the following elements
##'     \itemize{
##'         \item{\code{tau} }{
##'
##'             The vector of probabilities extracted from
##'             \code{thresholds}.
##' 
##'         }
##'         \item{\code{thresholds} }{
##'
##'             A copy of \code{thresholds} as given on entry.
##'
##'         }
##'         \item{\code{GP} }{
##'
##'             A list with class \code{\link{fevdTList}} 
##'
##'         }
##'         \item{\code{logLambda.fun}, \code{scale.fun}, \code{scale.fun} }{
##' 
##'              The formulas used for the "GP part" of the model.
##' 
##'         }
##'     }
##'
##' @export
##'
##' @seealso \code{\link{rqTList}} for the list of \code{rq} objects
##'     as used in \code{thresholds}, and \code{\link{fevdTList}} for
##'     the list of \code{fevd} objects as returned in the \code{GP}
##'     element of the result.
##'
##' @section Caution: At the time being, a \code{pgpTList} object is
##'     not a list of \code{pgp} objects. The output is likely to be
##'     re-designed, so a \code{pgpTList} object is better used via
##'     methods.
##' 
##' @examples
##' ## define the thresholds with the default seasonality, see 'rqTList'
##' Rq <- rqTList(dailyMet = Rennes,
##'               tau = c(0.94, 0.95, 0.96, 0.97, 0.98, 0.99))
##'
##' ## fit
##' Pgp1 <- pgpTList(dailyMet = Rennes, thresholds = Rq,
##'                  declust = TRUE,
##'                  fitLambda = TRUE, logLambda.fun = ~ YearNum - 1)
##'
##' ## try a varying GP shape 'xi'
##' Pgp2 <- pgpTList(dailyMet = Rennes, thresholds = Rq,
##'                  declust = TRUE, 
##'                  shape.fun = ~ Cst + sinjPhi1 + sinjPhi2 + sinjPhi3 - 1,
##'                  fitLambda = TRUE, logLambda.fun = ~ YearNum - 1)
##' 
pgpTList <- function(dailyMet,
                     subset = NULL,
                     thresholds,
                     declust = TRUE,
                     tauRef = 0.95,
                     fitLambda = FALSE,
                     logLambda.fun = ~1,
                     scale.fun = ~ Cst + sinjPhi1 + sinjPhi2 + sinjPhi3 - 1,
                     shape.fun = ~1,
                     trace = 1) {

    dailyMetBAK <- dailyMet
    metVar <- attr(dailyMet, "metVar")
    duration <- summary(dailyMet)$duration
    ## XX to be removed
    if (metVar != "TX") {
        stop("For now 'dailMet' can only have  \"TX\" as metVar") 
    }
    
    if (trace) {
        cat(sprintf("o Using meteorological variable : \"%s\"\n", metVar))
    }
    
    ## =========================================================================
    ## Check that 'thresholds' is as expected and find 
    ## the index of tauRef in tau(thresholds)
    ## =========================================================================
    
    if (!inherits(thresholds, "rqTList")) {
        stop("'thresholds' must be an object with class ",
             "\"rqTList\"")
    }
    tau <- tau(thresholds)
    indRef <- match(tauRef, tau)
    if (is.na(indRef)) {
        stop("'tauRef' must be found in tau(thresholds)")
    }
    threshold.fun <- formula(thresholds)
    
    ## =========================================================================
    ## add new variables id needed. Note that we can only use the harmonics that
    ## have been used in the formula for the threshold
    ## =========================================================================

    Kthresh <- checkTrigNames(formula(Rq))
    trigDesign <- tsDesign(dt = dailyMet$Date,
                           type = "trigo", df = 2 * Kthresh + 1)
    dailyMet <- data.frame(dailyMet, trigDesign$X)
    
   ## Kscale <- checkTrigNames(scale.fun)
   ##  Kshape <- checkTrigNames(shape.fun)
   ##  K <- max(c(Kscale, Kshape))
    K <- 3
    
    if (trace) cat("o Adding new variables \n")
    Phi <- phases(coef(thresholds))
    
    if (K) {
        phi <- Phi[indRef, ]
        if (trace) {
            cat("o Using K =", K, "and the following phases\n")
            print(round(phi, digits = 2))
        }
        
        sinDesignX <- sinBasis(dt = dailyMet[["Date"]],
                              df = 2 * K + 1,
                              phi = phi)
        dailyMet <- data.frame(dailyMet, sinDesignX)
        
    } else {        
        
        if (trace) {
            cat("No trigonometric variables needed.\n")
        }
    }
    
    tun <- sprintf("%6.2f/year", nrow(dailyMet) / duration)
    cat(sprintf("o Sampling rate : %s\n", tun))
    
    U <- FitGP <- FitLambda <- list()
    lambdaBar <- rep(NA, length(tau))
    names(lambdaBar) <- paste0("tau=", format(tau))
    
    
    if (trace) {
        cat(sprintf("o Looping on %d thresholds\n", length(tau)))
    }
    for (i in seq_along(tau)) {
        
        if (trace > 1) {
            cat("Fitting with threshold for tau =", tau[i], "\n")
        }
        
        U[[i]] <- predict(thresholds[[i]],
                          newdata = dailyMet, na.action = na.pass)
        
        ind <- (1:nrow(dailyMet))
        res <-  clusters3(Date = dailyMet[["Date"]][ind],
                          y = dailyMet[[metVar]][ind],
                          u = U[[i]][ind])

        ## =====================================================================
        ## now add the threshold and decluster if required.
        ## =====================================================================
        
        Met2 <- data.frame(dailyMet, u = U[[i]])
        
        if (declust){
            ## Met2 <- Met2[res$IndClust, ]
            indClust <- res$IndClust
            indLambda <- res$IndClust
        } else {
            indClust <- 1:nrow(Met2)
            indLambda <- (1:nrow(Met2))[Met2[[metVar]] > U[[i]]]
        }

        Met2 <- within(Met2, Excess <- TX - u)

        if (!is.null(subset)) {
            if (trace) cat("Using 'subset'\n")
        }

        ## =====================================================================
        ## fit the GP part. Note that in the threshold part we give
        ## the vector of coefficients and the formula.
        ## =====================================================================
        
        FitGP[[i]] <- fevd(x = Met2[[metVar]][indClust],
                           data = Met2[indClust, ],
                           threshold = coef(thresholds[[i]]),
                           threshold.fun = threshold.fun,
                           type = "GP",
                           scale.fun = scale.fun,
                           shape.fun = shape.fun,
                           time.units = tun)

        lambdaBar[i] <- nrow(Met2[indLambda, ]) / duration
        
        ## =====================================================================
        ## fit the Poisson (temporal) part
        ## =====================================================================
        
        if (fitLambda) {
            
            if (trace) cat("o Fit the temporal Poisson process\n")

            Covs <- model.matrix(logLambda.fun, data = Met2)
            L <- rep(0, ncol(Covs))
            names(L) <-  paste0("b", 1:ncol(Covs))
            
            FitLambda[[i]] <- fitPP.fun(tind = TRUE,
                                        covariates = Covs, 
                                        posE = indLambda,
                                        dplot = FALSE,
                                        start = c(list(b0 = 10), as.list(L)))   
        }
    }

    names(FitGP) <- names(thresholds)
    
    res <- list(tau = tau,
                lambdaBar = lambdaBar,
                dailyMet = dailyMetBAK,
                data = dailyMet,
                indLambda = indLambda,
                thresholds = thresholds,
                logLambda.fun = logLambda.fun,
                scale.fun = scale.fun,
                shape.fun = shape.fun,
                timePoisson = FitLambda,
                GP = as.fevdTList(FitGP))
    
    class(res) <- "pgpTList"
    res

}

##' @method predict pgpTList
##' @export

predict.pgpTList <- function(object, newdata, lastFullYear = FALSE,
                             ...) {
    
    if (!missing(newdata)) {
        stop("'newdata' can not be given for now")
    }
    newdata <- object$data
    ind <- object$indLambda
 
    pu <- predict(object$thresholds, newdata = newdata,
                  lastFullYear = lastFullYear)
    
    LambdaHat <- MuStar <- SigmaStar <- RL100 <- nExceed <- list()
    lambdaBar <- numeric(0)
    tau1 <- object$tau
    
    for (i in seq_along(tau1)) {
        
        MetWithu <- data.frame(newdata,
                               subset(pu, tau == tau1[i]))
        MetWithu <- within(MetWithu, Ex <- TX > u)
        
        LambdaHat[[i]] <- object$timePoisson[[i]]@lambdafit /
            mean(object$timePoisson[[i]]@lambdafit) * object$lambdaBar[i]

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
                                 sigmaStar = SigmaStar[[i]],
                                 xiStar = Theta[ , "shape"],
                                 RL100 = RL100[[i]])
        
        if (i == 1) {
            dfRebuild <- dfRebuildi
            ex <- with(MetWithu[ind, ], tapply(Ex, Year, sum))
            dfExceed <- data.frame(tau = unname(tau1[i]),
                                   Year = unique(MetWithu[ind, ]$Year),
                                   Nb = ex) 
        } else {
            dfRebuild <- rbind(dfRebuild, dfRebuildi, deparse.level = 0)
            ex <- with(MetWithu[ind, ], tapply(Ex, Year, sum))
            dfExceed <- rbind(dfExceed,
                              data.frame(tau = unname(tau1[i]),
                                         Year = unique(MetWithu[ind, ]$Year),
                                         Nb = ex))
        }
        
        nExceed[[i]] <- with(dfRebuildi, tapply(TX > u, Year, sum))
    
    }

    dfExceed <- within(dfExceed, tau <- as.factor(tau))
    dfExceed <- within(dfExceed, Date <- as.Date(sprintf("%4d-06-01", Year)))

    names(nExceed) <- paste0("tau=", format(tau1))
    
    ## list(dfRebuild = dfRebuild,
    ##      nExceed = nExceed)
    list(dfRebuild = dfRebuild,
         dfExceed = dfExceed,
         nExceed = nExceed)
    
}