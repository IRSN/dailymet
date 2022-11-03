## *****************************************************************************
##' @importFrom stats formula
##'
##' @export
##' 
##' @method formula fevd
##' 
formula.fevd <- function(x, ...) {

    if (!(x$type %in% c("GP", "PP"))) {
        stop("'type' can only be \"GP\" or \"PP\"") 
    }
    
    ## is this good?
    distName <- c("GP" = "GPD", "PP"= "GEV")
    parNames <- list("GP" = c("location", "scale", "shape"),
                     "PP" = c("location", "scale", "shape"))
    prefixes <- list("GP" = c("location" = "mu", "scale" = "sigma", "shape" = "xi"),
                     "PP" = c("location" = "mu", "scale" = "sigma", "shape" = "xi"))
    
    parNames <- parNames[[x$type]]
    distName <- distName[x$type]
    prefixes <- prefixes[[x$type]]
    fms <- list()
    for (pn in parNames) fms[[pn]] <- x$par.models[[pn]]
    fms
    
}

##'
##' @importFrom stats coef
##' @export 
##' @method coef fevd
##' 
coef.fevd <- function(object, ...) {
    object$results$par
}

## *****************************************************************************
##' Extracts the coefficients and the corresponding standard deviation
##' for the estimated coefficients of fitted model.
##'
##' @title Estimated Coefficients with Standard Errors
##'
##' @param object A fitted model.
##'
##' @param ... Further arguments for methods.
##' 
##' @return A character vector. The method is essentially designed to
##'     be used to see the result in an interactive session as usually
##'     \code{\link[base]{summary}}.
##'
##' @export
##' 
coSd <- function(object, ...) {
    UseMethod("coSd")
}

## *****************************************************************************
##' Extracts and show the coefficients of a \code{fevd} object along
##' with their standard deviations.
##' 
##' @title Coefficients and Standard Errors
##' 
##' @param object An object with class \code{"fevd"}.
##'
##' @param sOnly Logical. If \code{TRUE} only the standard deviation
##'     (a.k.a standard errors) wille be show.
##'
##' @param ... Not used.
##'
##' @seealso The \code{\link[extRemes]{distill.fevd}} method from the
##'     \pkg{extRemes} package.
##' 
##' @export
##' @method coSd fevd
##'
##' @examples
##' tau <- 0.97
##' library(extRemes)
##' data(Fort)
##' u0 <- quantile(Fort$Prec, prob = tau)
##' fit0 <- fevd(x = Prec, data = Fort, threshold = u0, type = "GP")
##' coSd(fit0)
##'  
coSd.fevd <- function(object, sOnly = FALSE, ...) {
    if (is.null(C <- parcov.fevd(object))) {
        s <- rep(NA, length(object$results$par))
    } else {
        s <- sqrt(diag(C))
    }
    if (sOnly) return(s)
    hat <- object$results$par
    text <- apply(cbind(hat, s), MARGIN = 1,
                  FUN = function(x) sprintf("%6.3f [%5.3f]", x[1], x[2]))
    names(text) <- names(object$results$par)
    noquote(text)
}


##' @importFrom stats vcov
##' @export
##' @method vcov fevd
##' 
vcov.fevd <- function(object, ...) {
    parcov.fevd(x = object)
}

##' @importFrom stats logLik
##' @export
##' @method logLik fevd
##' 
logLik.fevd <- function(object, ...) {
    -object$results$value
}


## *****************************************************************************
##' @description This method computes the parameters of the marginal
##'     distribution (\code{GP} or \code{GEV}) for the response. This
##'     marginal distribution depends on the covariates used in
##'     `object`, the values of which are to be given in
##'     \code{newdata}.
##' 
##' @title Predicted Values based on a \code{fevd} Object
##'
##' @param object An object with class \code{"fevd"} representing a
##'     non-stationary POT model, either Poisson-GP (\code{type =
##'     "GP"}) or NHPP (\code{type = "PP"}).
##' 
##' @param newdata A data frame containing the covariates.
##'
##' @param level Confidence level.
##' 
##' @param threshold The threshold. This can be either a suitable
##'     vector of an object representing a quantile regression,
##'     inheriting from \code{"rq"}.
##'
##' @param trace Integer level of verbosity.
##'
##' @param ... Not used yet
##'
##' 
##' @return A matrix with its rows matching those of \code{newdata}
##'     and its columns matching the parameters of the marginal
##'     distribution, Generalized Pareto (GP) or Generalized
##'     Extreme-Value (GEV).
##'
##' @section Caution: This method is highly experimental since the
##'     \code{extRemes::fevd} package does not store all all the
##'     information required for the prediction. The vector of
##'     coefficients for a covariate-dependent threshold is not
##'     stored.
##'
##' @importFrom stats predict model.matrix
##' 
##' @method predict fevd
##'
##' @export
##' 
##' @examples
##' ## use centimetres as precipitation unit
##' data(Fort) 
##' Fort <- within(Fort, Prec <- 2.54 * Prec)
##' tau <- 0.97; u0 <- quantile(Fort$Prec, prob = tau)
##' fit0 <- fevd(x = Prec, data = Fort, threshold = u0, type = "GP")
##' p0 <- predict(fit0)
##' rq <- rq(Prec ~ cos(2 * pi * tobs / 365.25) + sin(2 * pi * tobs / 365.25),
##'          data = Fort, tau = tau)
##' summary(rq)
##' ## compute the threshold and extract the fitted coefficients
##' u <- predict(rq, newdata = Fort)
##' beta_u <- coef(rq)
##' fit1 <- fevd(x = Prec, data = Fort, type = "GP",
##'              scale.fun = ~ cos(2 * pi * tobs / 365.25) + sin(2 * pi * tobs / 365.25),
##'              threshold = beta_u,
##'              threshold.fun = ~ cos(2 * pi * tobs / 365.25) + sin(2 * pi * tobs / 365.25))
##' p1 <- predict(fit1)
##' 
predict.fevd <- function(object, newdata = NULL,
                         ## out = c("coef", "data.frame"),  ## for later ???
                         level = 0.99,
                         threshold = NULL,
                         trace = 1,
                         ...) {

    if (!(object$type %in% c("GP", "PP"))) {
        stop("'type' can only be \"GP\" or \"PP\"") 
    }

    ## is this good?
    distName <- c("GP" = "GPD", "PP"= "GEV")
    parNames <- list("GP" = c("location", "scale", "shape"),
                     "PP" = c("location", "scale", "shape"))
    prefixes <- list("GP" = c("location" = "mu", "scale" = "sigma", "shape" = "xi"),
                     "PP" = c("location" = "mu", "scale" = "sigma", "shape" = "xi"))
    
    parNames <- parNames[[object$type]]
    distName <- distName[object$type]
    prefixes <- prefixes[[object$type]]
    
    if (is.null(newdata)) { 
        newdata <- object$cov.data
    }
   
    newn <- nrow(newdata)
    varNames <- object$par.models$term.names
    modelMatrices <- list()

    ## =========================================================================
    ## Manage the threshold
    ## =========================================================================
    
    cstThreshold <- FALSE
    
    if (!length(varNames$threshold)) {
        
        if (length(object$threshold) > 1) {
            stop("'object$threshold' has length > 1 and is not given ",
                 "by a formula. So we can not compute this threshold ",
                 "on any new data")
        } else {
            cstThreshold <- TRUE
            newThreshold <- rep(object$threshold, newn)
        }
        
    } else {

        if (is.null(threshold)) {
            
            ## The also work with the defaut formula ~1
            modelMatrices[["threshold"]] <-
                model.matrix(object$par.models[["threshold"]], data = newdata)
            
            ## XXX not robust. Yet we can not do anything better because
            ## the given value of 'threshold' is not stored in the object.  
            threshold <- eval(object$call$threshold)
            
            if (ncol(modelMatrices[["threshold"]]) != length(threshold)) {
                stop("'object$threshold' is not compliant with ",
                     "'object$threshold.fun'")
            }
            
            newThreshold  <- modelMatrices[["threshold"]] %*% threshold

        } else {
            if (is.numeric(threshold)) {
                if (trace) {
                    cat("Use the threshold provided in `threshold`\n")
                }
                if (length(threshold) != nrow(newdata)) {
                    stop("'when threshold is given as a numeric vector ",
                         "its length must match the number of rows in ",
                         "'newdata'")
                } else {
                    warning("'threshold' is provided as a numeric vector. ",
                            "Make sure that it is compliant with the one used ",
                            "when fitting the 'fevd' model.")
                }
                newThreshold <- threshold
            } else if (inherits(threshold, "rq")) {
                if (trace) {
                    cat("Predict the threshold from the provided 'rq' object\n")
                }
                newThreshold <- predict(threshold, newdata = newdata)
                if (trace) {
                    cat("Range of the threshold:", range(newThreshold), "\n")
                }
            }

        }
    }

    ## =========================================================================
    ## Check covariables names Caution, some variables such as 'pi' may not
    ## be variables
    ## =========================================================================
    
    varNamesU <- unique(unlist(varNames))
    ## m <- match(varNamesU, names(Met2))
    m <- match(varNamesU, names(newdata))
    
    ## if (any(is.na(m))) {
    ##     stop("Variables ", paste(varNamesU[m[is.na(m)]], collapse = ", "),
    ##         " can not be found in 'newdata'")
    ##}

    ## =========================================================================
    ## For each parameter we find the design matrix and compute the
    ## vector of parameter values
    ## =========================================================================
    
    nPar <- length(parNames)
    pp <- rep(NA, nPar)
    names(pp) <- parNames
    newTheta <- array(0.0, dim = c(newn, 3),
                      dimnames = list(rownames(newdata), parNames))
    ppc <- 0
    
    for (pn in parNames) {
        
        ## this can only happen for the location parameter
        ## in the "GP" case
        if (length(object$results$num.pars[[pn]]) == 0) {
            pp[pn] <- 0
            modelMatrices[[pn]] <- matrix(nrow = newn, ncol = 0)
        } else {
            pp[pn] <- object$results$num.pars[[pn]]
            modelMatrices[[pn]] <-
                model.matrix(object$par.models[[pn]], data = newdata)
            newTheta[ , pn] <- modelMatrices[[pn]] %*%
                object$results$par[ppc + 1:pp[pn]]
            ppc <- ppc + pp[pn]
        }

        if (pn == "scale" && object$par.models$log.scale) {
            newTheta[ , pn] <- exp(newTheta[ , pn])
        }
        
        if (object$type == "GP" && pn == "location") {
            newTheta[ , pn] <- newThreshold + newTheta[ , pn]
        }
       
    }

    attr(newTheta, "distName") <- distName 
    newTheta
    ## data.frame(newdata, threshold = newThreshold)
    
}


##' Compute the generalized residuals for some objects with class
##' \code{"fevd"} representing fitted POT models, especially
##' non-stationary ones.
##'
##' @title Generalized Residuals for some \code{fevd} Objects
##' 
##' @param object An object of class \code{"fevd"} representing a
##'     fitted POT model, either Poisson-GP if \code{object$type} is
##'     \code{"GP"} or a Non-Homogeneous Poisson Process if
##'     \code{object$type} is \code{"PP"}.
##' 
##' @param type Character giving the target distribution: \code{"exp"}
##'     for the standard exponential or \code{"unif"} for the standard
##'     exponential.
##'
##' @param ... Not used yet
##'
##' @return A numeric vector with the generalized residuals.
##'
##' @note For Poisson-GP POT models, the residuals will \code{NA} when
##'     the observation \eqn{y} does not exceed the threshold \eqn{u}
##'     i.e., when \eqn{y \leq u}.
##'
##' @method residuals fevd
##' 
##' @importFrom stats residuals
##'
##' @importFrom potomax pGPD2
##'
##' @export
##' 
residuals.fevd <- function (object,
                            type = c("exp", "unif"),
                            ...) {

    type <- match.arg(type)
    
    if (!(object$type %in% c("GP", "PP"))) {
        stop("'type' can only be \"GP\" or \"PP\"") 
    }
    
    ## is this good?
    distName <- c("GP" = "GPD", "PP"= "GEV")
    parNames <- list("GP" = c("location", "scale", "shape"),
                     "PP" = c("location", "scale", "shape"))
    prefixes <- list("GP" = c("location" = "mu",
                              "scale" = "sigma",
                              "shape" = "xi"),
                     "PP" = c("location" = "mu",
                              "scale" = "sigma",
                              "shape" = "xi"))
    
    parNames <- parNames[[object$type]]
    distName <- distName[object$type]
    prefixes <- prefixes[[object$type]]

    p <- predict(object)
    y <- object$x
    
    if (object$type == "GP") {
        e <- rep(as.numeric(NA), length(y))
        ind <- y > p[ , "location"]
        if (any(ind)) {
            e[ind] <- potomax::pGPD2(y[ind] - p[ind, "location"],
                                     scale = p[ind, "scale"],
                                     shape = p[ind, "shape"])
        }
    } else if (object$type == "PP") {
        e <- NSGEV::pGEV(y, loc = p[ , "location"],
                         scale = p[ , "scale"], shape = p[ , "shape"]) 
    }

    if (type == "exp") {
        e <- -log(1 - e)
    }

    names(e) <- rownames(object$cov.data)
    attr(e, "type") <- type
    class(e) <- "resid.fevd"
    e
    
}

## ****************************************************************************
##' Compute the GP or GEV coefficients as a matrix for an object with
##' class \code{"fevd"} as created using \code{extRemes::fevd}.
##' 
##' @title Compute the GP or GEV Coefficients for an Object with Class
##'     \code{"fevd"}
##'
##' @param object The object. It must represent the result of a POT
##' model fitting.
##'
##' @param data A data frame containing the data for which the model
##' will be used.
##'
##' @return A matrix with two or three columns representing the
##' coefficients.
##'
##' @note This method essentially does the same thing as
##'     \code{\link[extRemes]{findpars}}. However \code{findpars} will
##'     try to give the GPD \code{location} i.e. the threshold for
##'     some \code{fevd} objects at least.
##'
##' @seealso \code{\link[extRemes]{findpars}}
##'
##' @importFrom stats model.matrix
##' 
##' @export
##' 
##' @examples
##' data(Fort)
##' tau <- 0.97
##' u0 <- quantile(Fort$Prec, prob = tau)
##' fit0 <- fevd(x = Prec, data = Fort, threshold = u0, type = "GP")
##' theta(fit0)
##' 
theta <- function(object, data = NULL) {

    if (!inherits(object, "fevd")) {
        stop("'object' must inherit from the class \"fevd\"")
    }
    
    if (object$type == "PP") {   
        nms <- c("loc", "scale", "shape")
        nmls <- c("location", "scale", "shape")
        syms <- c("loc" = "mu", "scale" = "sigma", "shape" = "xi")
    } else if (object$type == "GP") {
        nms <- nmls <- c("scale", "shape")
        syms <- c("scale" = "sigma", "shape" = "xi")
    } else {
        stop("Bad 'type' in 'object'. Must be \"pp\" or \"gp\"") 
    }
        
    co <- object$results$par
    
    if (is.null(data)) {
        data <- object$cov.data
        n  <- object$n
    } else {
        n <- nrow(data)
    }
    Theta <- matrix(NA, nrow = n, ncol = length(nms),
                    dimnames = list(NULL, nms))
    
    if (object$results$convergence != 0) return(Theta)
    
    for (inm in seq_along(nms)) {
        nm <-  nms[inm]
        nml <- nmls[inm]
        if (object[[paste0("const.", nm)]]) {
            Theta[ , nm] <- rep(co[nml], n)
        } else {
            ind <- grep(syms[nm], names(co))
            X <- model.matrix(object$par.models[[nml]],
                              data = data)
            Theta[ , nm] <- X %*% co[ind]
        }
    }
    Theta
}

## ****************************************************************************
##' Compute the quantile for Non Stationary Extreme-Value
##' models fitted by using \code{extRemes::fevd}.
##'
##' @title Compute Quantile for \code{fevd} Objects
##'
##' @param x An object with class \code{"fevd"}.
##'
##' @param probs See \code{\link[NSGEV]{quantile.TVGEV}}
##'
##' @param data A data frame containing the covariates needed.
##'
##' @param ... Not used yet.
##' 
##' @return An object inheriting from \code{"matrix"}.
##'
##' @importFrom stats quantile
##'
##' @method quantile fevd
##'
##' @export
##' 
##' @importFrom NSGEV qGEV
##' 
quantile.fevd <- function(x, probs = c(0.9, 0.95, 0.99),
                          data = NULL, ...) {
      
    if (!(x$type %in% c("PP"))) {
        stop("'type' can only be \"PP\"") 
    }
    
    if (is.null(data)) data <- x$cov.data
    n <- nrow(data)
    quant <- array(NA, dim = c(n, length(probs)),
                   dimnames = list(rownames(data), 
                       paste("Q", formatPerc(probs), sep = "")))
    theta <- theta(x, data = data)
    
    for (i in seq_along(probs)) {
        quant[, i] <- NSGEV::qGEV(probs[i],
                                  loc = theta[, 1L],
                                  scale = theta[ , 2L],
                                  shape = theta[, 3L])
    }
    attr(quant, "p") <- probs
    quant

}
