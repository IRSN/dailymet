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
##'     new data is taken as \code{object$data}.
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
                names(newdata) <- c(nm, attr(object$dailyMet, "metVar"))
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

    ## =========================================================================
    ## Now prepare the extra design  variables if needed
    ## =========================================================================
    
    if (!is.null(object$extraDesign)) {
        if (trace) cat("o Adding variables relates to extra design\n")
        X <- designVars(designList = object$extraDesign, dt = newdata$Date,
                        trace = trace)
        newdata <- data.frame(newdata, X)
    }
   
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
##' @param newdata An optional object giving the "new" dates for which
##'     the simulation will be done. It can simply be an object with
##'     class \code{"Date"} or an object with class
##'     \code{"dailyMet"}. Depending on the class of \code{newdata},
##'     the variables required for the prediction (such as sine waves)
##'     will be recomputed or not. When \code{newdata} is not given or
##'     is \code{NULL}, \code{object$data} is used.
##' 
##' @param lastFullYear Logical, used only when \code{newdata} is not
##'     provided or is \code{NULL}. When \code{TRUE}, only the last
##'     full year in \code{newdata} will be used.
##'
##' @param which The kind of result wanted. 
##' 
##' @param probExcM A vector of (small) probabilities of exceedance
##'     for the maximum \eqn{M} of the marks on the prediction
##'     period. For each probability \eqn{p}, the value of the
##'     quantile \eqn{m} of \eqn{M} with probability \eqn{1 - p} will
##'     be computed. Note that the quantile may be \code{NA} if the
##'     value \eqn{F_M(m)} of distribution function of \eqn{M} is
##'     lower than \eqn{1 -p} when \eqn{m := \textrm{max}_t\{u_t\}}{m
##'     := max_t _(t)} because the POT model can only provide the tail
##'     distribution of \eqn{M}.
##'
##' @param ... Not used yet.
##'
##' @param trace Integer level of verbosity.
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
##' @section Caution: This method still may change.
##'
##' @method predict pgpTList
##'
##' @importFrom stats terms model.frame delete.response as.formula rexp
##' @importFrom stats approx
##' 
##' @export
##' 
predict.pgpTList <- function(object, newdata = NULL,
                             lastFullYear = FALSE,
                             ## tau = NULL,  XXX to be added later?
                             trace = 0,
                             which = c("param", "max"),
                             probExcM = c(0.05, 0.01, 0.005, 0.001, 0.0005, 0.0001),
                             ...) {
    
    TX <- u <- lambda <- v <- xiStar <- sigma <- NULL
    which <- match.arg(which)
    
    if (!missing(newdata) && !is.null(newdata)) {
        missNewData <- FALSE
        ## warning("'newdata' is still experimental")
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

    ## Compute "IndPred"
    IndPred <- rep(TRUE, nrow(newdata))

    if (!is.null(object$subset)) {
        Ind <- 1:nrow(newdata)
        newdata <- data.frame(newdata, .Ind = Ind)
        IndKeep <- subset(newdata,
                          subset = eval(parse(text = object$subset)))$.Ind
        IndPred[setdiff(Ind, IndKeep)] <- FALSE
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
                        mean(object$timePoisson[[i]]@lambdafit) *
                        object$lambdaBar[i]
                } else {
                    LambdaHat[[i]] <- rep(exp(object$timePoisson[[i]]@coef),
                                          nrow(newdata)) /
                        mean(object$timePoisson[[i]]@lambdafit) *
                        object$lambdaBar[i]
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

        if (!is.null(object$subset)) {
            MuStar[[i]][!IndPred] <- NA
            RL100[[i]][!IndPred] <- NA
            SigmaStar[[i]][!IndPred] <- NA
            Theta[!IndPred, ] <- NA
        }
        
        dfRebuildi <- data.frame(TX = MetWithu$TX,
                                 Date = MetWithu$Date,
                                 DateRef = newdata$DateRef,
                                 Year = newdata$Year,
                                 Day = newdata$Day,
                                 u = MetWithu$u,
                                 tau = MetWithu$tau,
                                 lambda = LambdaHat[[i]],
                                 IndPred = IndPred,
                                 muStar = MuStar[[i]],
                                 sigma = Theta[ , "scale"],
                                 sigmaStar = SigmaStar[[i]],
                                 xiStar = Theta[ , "shape"],
                                 RL100 = RL100[[i]])

        ## The computations on the exceedances have been moved into
        ## the `exceed` method
        
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
    
    }

    ind <- as.integer(dfRebuild$tau)
    dfRebuild$v <- tapply(dfRebuild$u, INDEX = dfRebuild$tau, FUN = max)[ind]

    lambdav <- rep(NA, nrow(dfRebuild))
    lambdav[IndPred] <- dfRebuild$lambda[IndPred] *
        (1 - nieve::pGPD2((dfRebuild$v - dfRebuild$u)[IndPred],
                          dfRebuild$sigma[IndPred],
                          dfRebuild$xiStar[IndPred]))
    dfRebuild$lambdav <- lambdav
    
    dfRebuild <- within(dfRebuild, {
        sigmav <- sigma  + xiStar * (v - u);
        omega <- ifelse(xiStar < 0, u - sigma / xiStar, Inf)})

    dfRebuild$sigmav[dfRebuild$sigmav <= 0.0] <- NA 
    
    attr(dfRebuild, "lastFullYear") <- lastFullYear
    attr(dfRebuild, "metVar") <- attr(object$dailyMet, "metVar")
    attr(dfRebuild, "station") <- attr(object$dailyMet, "station")
    attr(dfRebuild, "id") <- attr(object$dailyMet, "id")
    attr(dfRebuild, "tau") <- tau1
    
    class(dfRebuild) <- c("predict.pgpTList", "data.frame")
    dfRebuild
    
}



### # *****************************************************************************

## ##' The Poisson-GP model given in \code{x} only describes the tail of
## ##' the distribution of the maximum. So when a probability is too
## ##' small the quantile may be \code{NA}.
## ##'
## ##' The computation of the quantiles without any inference result can
## ##' be performed at a lower cost by using the results of a
## ##' \code{predict} step it these have already been computed. Of course
## ##' the "new" period will be that which was used in the \code{predict}
## ##' step and can not be changed.
## ##' 
## ##' @title Compute Quantiles for the Maximum the Marks of a Poisson-GP
## ##'     Model
## ##'
## ##' @param x An object with class \code{"predict.pgpTList"} as created
## ##'     by applying the \code{predict} method on an object with class
## ##'     \code{"pgpTList"}.
## ##' 
## ##' @param newdata A "new" data frame or \code{Date} vector used to
## ##'     define the "new" period. The quantiles will be the those of
## ##'     the random maximum \eqn{M} of the marks on this period.
## ##' 
## ##' @param prob Vector of probabilities.
## ##' 
## ##' @param level The confidence level.
## ##' 
## ##' @param ... Not used yet.
## ##' 
## ##' @return A data frame with columns \code{Prob} and \code{Quant}.
## ##'
## ##' @importFrom stats qnorm
## ##' @method quantile pgpTList
## ##' @export
## ##'
## ##' @seealso \code{\link{predict.pgpTList}}.
## ##'
## ##' @examples
## ##' RqU <- rqTList(dailyMet = Rennes, tau = c(0.94, 0.95, 0.96, 0.97, 0.98, 0.99))
## ##' Pgp1 <- pgpTList(dailyMet = Rennes, thresholds = RqU, declust = TRUE,
## ##'                  fitLambda = TRUE, logLambda.fun = ~YearNum - 1)
## ##' Date <- seq(from = as.Date("2020-01-01"), to = as.Date("2050-01-01"), by = "day")
## ##' ## compute the quantile for the maximum on the "new" period
## ##' qMax <- quantile(Pgp1, newdata = Date)
## ##' autoplot(qMax)
## ##'
## ##'
## quantile.pgpTList <- function(x,
##                               newdata = NULL,
##                               prob = NULL,
##                               level = 0.95,
##                               ...) {
##     Prob <- ProbExc <- NULL
    
##     if (!is.null(x$subset)) {
##         stop("The computation of quantiles is not possible ",
##              "when 'x$subset' is not 'NULL'")
##     }

##     pred <- predict(x, newdata = newdata, ...)

##     modMat <- modelMatrices(x, newdata = newdata)
##     covMat <- vcov(x)
##     parInfo <- parInfo(x)
    
##     tau1 <- attr(pred, "tau")
##     nM <- 40
##     L <- list()
##     pred <- as.data.frame(pred)
    
##     if (is.null(prob)) {
##         prob <- 1 - c(0.06, 0.05, 0.04, 0.03, 0.02, 0.01,
##                       0.0075,  0.005, 0.0025,
##                       0.001, 0.002, 0.005)
##     }
    
##     indScale <- 1:parInfo$length$GP["scale"]
##     indShape <- parInfo$length$GP["scale"] + (1:parInfo$length$GP["shape"])
##     indLambda <- parInfo$length$GP["scale"] + parInfo$length$GP["shape"] +
##         (1:parInfo$length$timePoisson)
##     p <- parInfo$length$GP["scale"] + parInfo$length$GP["shape"] +
##         parInfo$length$timePoisson

    
##     for (i in seq_along(tau1)) {

##         predi <- subset(pred, tau == tau1[i])
##         mL <- max(predi$v, na.rm = TRUE)
##         mU <- max(predi$omega, na.rm = TRUE)
##         m <- seq(from = mL,  to = mU, length.out = nM)
        
##         FM <- fM <- se <- rep(NA, nM)
        
##         dFBeta <- array(NA, dim = c(nM, p))
            
##         for (j in seq_along(m)) {

##             FVec <- nieve::pGPD2(m[j] - predi$u,
##                                  scale = predi$sigma,
##                                  shape = predi$xiStar,
##                                  deriv = TRUE)
            
##             lambdaS <- predi$lambda * (1 - FVec)
##             attr(lambdaS, "gradient") <- NULL
##             FM[j] <- exp(-sum(lambdaS) / 365.25)
##             fM[j] <- FM[j] * sum(predi$lambda *
##                                  nieve::dGPD2(m[j] - predi$u,
##                                               scale = predi$sigma,
##                                               shape = predi$xiStar)) / 365.25
            
##             ## =================================================================
##             ## In any model matrix, the rows match times and the
##             ## columns match the 'beta' parameters for the
##             ## corresponding Poisson-GP parameter. The sum over the
##             ## times can be obtained with a matrix multiplication.
##             ## =================================================================
            
##             mat <- modMat[["scale"]]
##             weights <- predi$lambda * attr(FVec, "gradient")[ , "scale"]
            
##             dFBeta[j, indScale] <- - drop(t(mat) %*% weights) * FM[j] / 365.25
            
##             mat <- modMat[["shape"]]
##             weights <- predi$lambda * attr(FVec, "gradient")[ , "shape"] 
##             dFBeta[j, indShape] <- - drop(t(mat) %*% weights) * FM[j] / 365.25
            
##             mat <- modMat[["logLambda"]]
##             dFBeta[j, indLambda] <- - drop(t(mat) %*% lambdaS) * FM[j] / 365.25
            
##             se[j] <- sqrt(t(dFBeta[j, ]) %*% covMat[[i]] %*% dFBeta[j, ]) / fM[j]
            
##         }
        
##         probL <- (1 - level) / 2
##         probU <- 1 - probL
##         qLU <- qnorm(cbind(probL, probU), mean = 0.0, sd = 1.0)
##         if (FALSE) {
##             dfi <- data.frame(tau = unname(tau1[i]),
##                               Prob = prob,
##                               ProbExc = 1 - prob,
##                               Quant = approx(x = FM, y = m, xout = prob)$y)
##         } else {
            
##             dfi <- data.frame(tau = unname(tau1[i]),
##                               Prob = FM,
##                               ProbExc = 1 - FM,
##                               Quant = m)
            
##             dfi <- cbind(dfi, 
##                          L = dfi$Quant + qLU[1] * se,
##                          U = dfi$Quant + qLU[2] * se)

##             if (TRUE) {
##                 roundQuant <- approx(x = dfi$Prob, y = dfi$Quant, xout = prob)$y
##                 roundL <- approx(x = dfi$Prob, y = dfi$L, xout = prob)$y
##                 roundU <- approx(x = dfi$Prob, y = dfi$U, xout = prob)$y
##                 roundDfi <- data.frame(tau = unname(tau1[i]),
##                                    Prob = prob,
##                                    ProbExc = 1 - prob,
##                                    Quant = roundQuant,
##                                    L = roundL,
##                                    U = roundU)
                
##                 dfi <- rbind(dfi, roundDfi)
##                 dfi <- dfi[order(dfi$tau, dfi$Prob), ]
##             }
##         }
        
##         L[[i]] <- dfi 
        
##     }
    
##     res <- data.table::rbindlist(L)
##     res$tau <- factor(res$tau)
##     res <- subset(res, Prob <= 1 - 1e-5 & Prob > 1e-5)
##     attr(res, "level") <- level
##     class(res) <- c("quantile.pgpTList", "data.frame")
##     res
    
## }

## BAKquantile.pgpTList <- function(x,
##                               prob = NULL,
##                               ...) {
##     pred <- predict(x, ...)
##     quantile(pred, prob = prob)
    
## }


## ##' @method quantile predict.pgpTList
## ##' @export
## quantile.predict.pgpTList <- function(x,
##                                       prob = NULL,
##                                       ...) {
##     dots <- list(...)
##     pm <- pmatch("newdata", names(dots))
##     if (!is.na(pm)) {
##         stop("'newdata' can not be given when the quantiles ",
##              "are computed from predictions. Use either the ",
##              "'rqTList' object in the formal 'x' of the 'quantile' ",
##              "method, or use a new 'predict' step")
##     }
    
##     tau1 <- attr(x, "tau")
##     nm <- 40
##     L <- list()
##     x <- as.data.frame(x)
    
##     if (is.null(prob)) {
##         prob <- 1 - c(0.06, 0.05, 0.04, 0.03, 0.02, 0.01,
##                       0.0075,  0.005, 0.0025,
##                       0.001, 0.002, 0.005)
##     }

##     for (i in seq_along(tau1)) {
##         predi <- subset(x, tau == tau1[i])
##         m <- seq(from = min(predi$v), to = max(predi$omega), length.out = nm)
##         Fm <- rep(NA, nm)
##         for (j in seq_along(m)) {
##             Fm[j] <- exp(-sum(predi$lambda *
##                           (1 - nieve::pGPD2(m[j] - predi$u,
##                                             scale = predi$sigma,
##                                             shape = predi$xiStar))) / 365.25)
##         }
##         L[[i]] <- data.frame(tau = unname(tau1[i]),
##                              Prob = prob,
##                              ProbExc = 1 - prob,
##                              Quant = approx(x = Fm, y = m, xout = prob)$y)
        
##     }
    
##     res <- data.table::rbindlist(L)
##     res$tau <- factor(res$tau)
##     class(res) <- c("quantile.pgpTList", "data.frame")
##     res
    
## }

## ##' Round the values of the quantiles and confidence limits and select
## ##' those corresponding to "round" exceedance probabilities.
## ##'
## ##' It is often needed to show a table of quantiles corresponding to
## ##' round exceedance probabilities such as \code{0.01},
## ##' \code{0.001}. Moreover the quantiles and confidence limits as
## ##' printed by default with number of digits which may be higher than
## ##' needed. Most often, a number of digits between 0 and 2 is hight
## ##' enough in relation with the precision of the data used to compute
## ##' the quantiles.
## ##'
## ##' @title Round and Format Quantiles
## ##'
## ##' @param x An object with class \code{"quantile.pgpgTList"} as
## ##'     computed by the \code{quantile} method of the
## ##'     \code{"pgpgTList"} class.
## ##' 
## ##' @param digits The number of digits to be used for the colums
## ##'     \code{Quant}, \code{L} and \code{U} columns representing the
## ##'     quantile and the confidence limits.
## ##' 
## ##' @param probExc A numeric vector giving the exceedances
## ##'     probabilities to be displayed among those available in
## ##'     \code{x}. The default values correspond to 12 values
## ##'     \code{0.5}, \code{0.2}, \code{0.1}, ..., \code{1e-4}.
## ##' 
## ##' @param ... Not used.
## ##'
## ##' @return A data frame with the rounded quantiles and confidence
## ##'     limits at the" chosen "round" exceedance probabilities.
## ##'
## ##' @export
## ##' @method format quantile.pgpTList
## ##' 
## format.quantile.pgpTList <- function(x,
##                                      digits = 2,
##                                      probExc = as.vector(
##                                          outer(c(5, 2, 1),
##                                                c(1e-1, 1e-2, 1e-3, 1e-4))),
##                                      ...) {

##     ProbExc <- NULL
    
##     if (!is.null(probExc)) {
##         xp <- subset(as.data.frame(x), subset = round(ProbExc, digits = 6) %in% probExc)
##     } else {
##         xp <- as.data.frame(x)
##     }
##     xp <- within(xp, {
##         Quant <- round(Quant, digits = digits);
##         L <- round(L, digits = digits);
##         U <- round(U, digits = digits);
##     })
    
##     xp
## }


##' @method print predict.pgpTList
##' @export
print.predict.pgpTList <- function(x, ...) {
    cat("Predicted values from a 'pgpTList' object\n")
    cat(sprintf("variable: \"%s\", station: \"%s\", id:  \"%s\"\n",
                attr(x, "metVar"), attr(x, "station"), attr(x, "id")))
}

##' @method subset predict.pgpTList
##' @export
subset.predict.pgpTList <- function(x, ...) {

    newx <- subset(as.data.frame(x), ...)
    for (attNm in c("lastFullYear", "metVar", "station", "id", "tau")) {
        attr(newx, attNm) <- attr(x, attNm)
    }
    class(newx) <- c("predict.pgpTList", "data.frame")
    newx
    
}

##' @importFrom utils head
##' @method head predict.pgpTList
##' @export
head.predict.pgpTList <- function(x, n = 6L, ...) {

    head(as.data.frame(x), n = n, ...)

}

##' @importFrom utils tail
##' @method tail predict.pgpTList
##' @export
tail.predict.pgpTList <- function(x, n = 6L, ...) {

    tail(as.data.frame(x), n = n, ...)
    
}

## *****************************************************************************

##' Simulate from a `pgpTList` Object. For each of the thresholds of
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
##' @param nsim Number of simulation wanted.
##'
##' @param seed Not used yet.
##' 
##' @param newdata An optional object that giving the "new" dates for
##'     which the simulation will be done. It can simply be an object
##'     with class \code{"Date"} or an object with class
##'     \code{"dailyMet"}. Depending on the class of \code{newdata},
##'     the variables required for the prediction (such as sine waves)
##'     will be recomputed or not. When \code{newdata} is not given or
##'     is \code{NULL}, \code{object$data} is used.
##' 
##' @param lastFullYear Logical, used only when \code{newdata} is not
##'     provided or is \code{NULL}. When \code{TRUE}, only the last
##'     full year in the data used when fitting the object will be
##'     used.
##'
##' @param tailOnly Logical. If \code{TRUE} the simulation will be
##'     only for tail events. More precisely only for each of the
##'     threshold \eqn{u_i(t)} corresponding to a non-exceedance
##'     probability \eqn{\tau_i} the maximum \eqn{v_i:=\max_t
##'     u_i(t)} of the the thresholds over the predicted period is
##'     computed, and the simulated events correspond to exceedances
##'     over \eqn{v_i}.
##' 
##' @param tau If provided, the simulation is only made for the
##'     provided value of \code{tau}.
##' 
##' @param trace Integer level of verbosity.
##'
##' @param how A technical argument telling how the big data frame on
##'     output is to be constructed by concatenation. The
##'     \code{"list"} method (default and recommended) makes a list
##'     containing a large number of data frames, one by simulation
##'     and then makes use of the very efficient
##'     \code{\link[data.table]{rbindlist}} function. With
##'     \code{"vector"}, once concatenates vectors, and with
##'     \code{"data.frame"} one rbinds data fames at each
##'     simulation. The tow later options are by far slower when
##'     \code{n} is large, say \code{n > 100}.
##' 
##' @param ... Not used yet.
##'
##' @return An object with class \code{"summary.pgpTList"}, inheriting
##'     from the \code{"data.frame"} class. This is a data frame in
##'     long format containing the simulated events with the
##'     corresponding simulated values of the meteorological variable.
##'
##' @section Caution: the returned data frame can be very large since
##'     \code{n} simulations are made for each value of \code{tau},
##'     and each simulation can will typically give several dozens of
##'     events.
##'
##' @note In the returned data frame, the (factor) column \code{Sim}
##'     gives the simulation number. It can happen that a simulation
##'     generates no event, especially if \code{tau} is close to 1
##'     and if the simulation period is short.
##' 
##' @method simulate pgpTList
##' 
##' @export
##'
##' @importFrom data.table rbindlist
##' @importFrom stats simulate
##' 
##' @examples
##' RqU <- rqTList(dailyMet = Rennes, tau = c(0.94, 0.95, 0.96, 0.97, 0.98))
##' Pgp1 <- pgpTList(dailyMet = Rennes, thresholds = RqU, declust = TRUE,
##'                  fitLambda = TRUE, logLambda.fun = ~YearNum - 1)
##' Date <- seq(from = as.Date("2022-01-01"), to = as.Date("2043-01-01"), by = "day")
##' st <- system.time(sim <- simulate(Pgp1, nsim = 8, newdata = Date, trace = 1))
##' autoplot(subset(sim, tau == 0.95))
##' ## more realistic example
##' \dontrun{
##'     pred <- predict(Pgp1, newdata = Date)
##'     st <- system.time(sim <- simulate(Pgp1, nsim = 1000, newdata = Date, trace = 1))
##'     ## compute the maximum 'M' on the period by simulation and by threshold
##'     M <- with(sim, tapply(TX, list(Sim, tau), max))
##'     ## quantile over the simulation (by threshold)
##'     apply(M, 2, quantile, prob = 0.99, na.rm = TRUE)
##'     ## compare with the computation
##'     quantile(pred, p = 0.99)
##' }
simulate.pgpTList <- function(object,
                              nsim = 1,
                              seed = NULL,
                              newdata,
                              lastFullYear = FALSE,
                              tailOnly = TRUE,
                              tau = NULL,
                              trace = 0,
                              how = c("list", "vector", "data.frame"),
                              ...) {
    ## library(tibble)
    how <- match.arg(how)
    n <- nsim
    
    if (!missing(tau) && !is.null(tau)) {
        if (!all(tau %in% object$tau)) {
            stop("When given, 'tau' must provide values that are ",
                 "found in 'object$tau'")
        }
    } else {
        tau <- object$tau
    }

    if (trace) print(tau)
    
    ## XXX pass tau here as well???
    pred <- predict(object, newdata = newdata, lastFullYear = lastFullYear,
                    ...)
    iAll <- 1    
    
    for (taui in tau) {

        if (trace) {
            cat(sprintf("tau = %5.2f\n", taui))
        }
        ind <- pred$tau == taui
        predTau <- pred[ind, , drop = FALSE]
        ## predTau <- as_tibble(predTau)
        if (tailOnly) {
            Lambda <- c(0, with(predTau, cumsum(lambdav) / 365.25))
        } else {
            Lambda <- c(0, with(predTau, cumsum(lambda) / 365.25))
        }
        nSim <- 3 * max(Lambda)
        
        ## ====================================================================
        ## 'TH' are the events of a Homogeneous PP on (0, n) with unit rate.
        ## 'TNH' are the events of the NHPP obtained by using the reciprocal
        ## function of the cumulated rate 'Lambda' according to
        ## 'TNH := Lambda^{-1}(TH)'.
        ## =====================================================================
        
        for (i in 1:n) {

            if (trace >= 2 && i %% 100 == 0) {
                cat(sprintf("    i = %d\n", i))
            }
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
                
                if (trace >= 3) {
                    cat(sprintf("n = %d, tau = %4.2f, sim = %d, nb event = %d\n",
                                n, taui, i, length(TNH)))
                }
                
                ## here we could select a smaller number of variables
                
                resi <- predTau[TNH, ]
                ## print(class(resi))
                ## print(head(resi$sigma))
                ## print(head(resi$xi))
                ## relace 'TX' by a simulated value
                
                if (tailOnly) {
                    resi$TX <- resi$v + drop(nieve::rGPD2(n = 1,
                                                          scale = resi$sigmav,
                                                          shape = resi$xiStar))
                } else {
                    resi$TX <- resi$u + drop(nieve::rGPD2(n = 1,
                                                          scale = resi$sigma,
                                                          shape = resi$xiStar))
                }
                resi$Sim <- i
                resi
                
                if (iAll == 1) {
                    if (how == "data.frame") {
                        res <- resi
                    } else if (how == "vector") {
                        resSim <- rep(i, length(TNH))
                        resDate <- predTau[["Date"]][TNH]
                        resTau <- predTau[["tau"]][TNH]
                        resTX <- resi$TX
                    } else if (how == "list") {
                        L <- list(as.data.frame(resi[ , c("Sim", "Date", "tau", "TX")]))
                    }
                } else {
                    if (how == "data.frame") {
                        ## res <- rbind(res, resi, deparse.level = 0)
                        ## res <- dplyr::bind_rows(res, resi)
                        res <- data.table::rbindlist(list(res, resi))
                    } else if (how == "vector") {
                        resSim <- c(resSim, rep(i, length(TNH)))
                        resDate <- c(resDate, predTau[["Date"]][TNH])
                        resTau <- c(resTau, predTau[["tau"]][TNH])
                        resTX <- c(resTX, resi$TX)
                    } else if (how == "list") {
                        L[[iAll]] <- as.data.frame(resi[ , c("Sim", "Date", "tau", "TX")])
                    }
                }
            }
            iAll <- iAll + 1
        }

        ## length(TNH) / (as.numeric(diff(range(p$Date))) / 365.25)
    }
    
    if (how == "data.frame") {
        ## res <- within(res, Sim <- factor(Sim, levels = 1:n))
        res$Sim <- factor(res$Sim, levels = 1:n)
    } else if (how == "vector") {
        res <- data.frame(Sim = factor(resSim, levels = 1:n),
                          Date = resDate,
                          tau = resTau,
                          TX = resTX)
    } else if (how == "list") {
        res <- data.table::rbindlist(L)
        res$Sim <- factor(res$Sim, levels = 1:n)
    }
        
    attr(res, "lastFullYear") <- lastFullYear
    attr(res, "nSim") <- attr(object$dailyMet, "nSim")
    attr(res, "metVar") <- attr(object$dailyMet, "metVar")
    attr(res, "station") <- attr(object$dailyMet, "station")
    attr(res, "id") <- attr(object$dailyMet, "id")

    
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

##' @method head simulate.pgpTList
##' @export
head.simulate.pgpTList <- function(x, ...) {
    head(as.data.frame(x), ...)
}

##' @method modelMatrices pgpTList
##' @export
modelMatrices.pgpTList <- function(object, 
                                   newdata = NULL,
                                   trace = 0,
                                   ...) {
   
    if (!missing(newdata) && !is.null(newdata)) {
        missNewData <- FALSE
        ## warning("'newdata' is still experimental")
        newdata <- makeNewData(object, newdata = newdata,
                               trace = trace)
    } else {
        missNewData <- TRUE
        newdata <- object$data
    }

    ## we MUST pass 'threshold' here because object$GP[[1]] does not
    ## stroe the threshold !
    L <- modelMatrices(object$GP[[1]],
                       newdata = newdata,
                       threshold = object$thresholds[[1]]$formula)
    

    if (object$fitLambda) {
        lambdaNH <- !(all.equal(object$logLambda.fun, ~1) == TRUE)
        if (lambdaNH) {
            Xtime <- model.matrix(object$logLambda.fun, data = newdata)
            Xtime <- cbind("b0" = rep(1, nrow(newdata)), Xtime)
            timeNH <- TRUE
        } else {
            Xtime <- cbind("b0" = rep(1, nrow(newdata)))
        }
        L[["logLambda"]] <- Xtime
    }

    L
    
}


##' This S3 generic function is defined in order to allow the
##' implementation of methods.
##' 
##' @title Provide Information about the Parameters of a Fitted Model
##'     Object
##'
##' @export
##' 
##' @param object The object for which the information will be shown.
##'
##' @param ...  Other arguments for methods.
##' 
parInfo <- function(object, ...) {
    UseMethod("parInfo")
}


##' @title Provide Information about the Parameters of a
##'     \code{pgpTList} Object.
##'
##' @param object A \code{pgpTList} object.
##'
##' @param ... Not used.
##' 
##' @return A list with two elements
##' \itemize{
##'     \item{length }{
##'         A list containing the lengths of the blocks.
##'     }
##'     \item{names }{
##'         A list containing the names of the blocks and their
##'         elements.
##'     }
##'  }
##'
##' @method parInfo pgpTList
##' @export
##' 
parInfo.pgpTList <- function(object, 
                             ...) {

    coLambda <- object$timePoisson[[1]]@coef
        
    p <- list()
    p$GP <- unlist(object$GP[[1]]$results$num.pars)
    p$timePoisson <- length(coLambda)
    
    pn <- list()
    pn$GP <- list()
    n <- 0
    allNames <- names(coef(object$GP[[1]]))
    for (i in seq_along(p$GP)) {
        pn$GP[[i]] <- allNames[(n + 1):(n + p$GP[i])]
        n <- n + p$GP[i]
    }
    pn$GP[[3]] <- allNames
    names(pn$GP) <- c(names(p$GP), "all")
    pn$timePoisson[["all"]] <- names(coLambda)

    list(length = p,
         names = pn) 
    
}

##' @method modelMatrices pgpTList
##' @export
vcov.pgpTList <- function(object, 
                          ...) {

    covList <- list()
    
    pI <- parInfo(object)
    pLengthTot <- sum(sapply(pI$length, sum))
    pNamesTot <- c(pI$names$GP[["all"]],
                   pI$names$timePoisson[["all"]])
    indGP <- 1:sum(pI$length$GP)  
    indTimePoisson <- sum(pI$length$GP) + (1:pI$length$timePoisson)    
    for (i in seq_along(object$GP)) {
        mat <- array(0,
                     dim = c(pLengthTot, pLengthTot),
                     dimnames = list(pNamesTot,  pNamesTot))
        mat[indGP, indGP] <- vcov(object$GP[[i]])
        mat[indTimePoisson, indTimePoisson] <- object$timePoisson[[i]]@vcov
        covList[[i]] <- mat                              
    }
    names(covList) <- names(object$GP)
    covList
    
}
