
## *****************************************************************************

##' The Poisson-GP model given in \code{x} only describes the tail of
##' the distribution of the maximum. So when a probability is too
##' small the quantile may be \code{NA}.
##'
##' The computation of the quantiles without any inference result can
##' be performed at a lower cost by using the results of a
##' \code{predict} step it these have already been computed. Of course
##' the "new" period will be that which was used in the \code{predict}
##' step and can not be changed.
##' 
##' @title Compute Quantiles for the Maximum the Marks of a Poisson-GP
##'     Model
##'
##' @param x An object with class \code{"predict.pgpTList"} as created
##'     by applying the \code{predict} method on an object with class
##'     \code{"pgpTList"}.
##' 
##' @param newdata A "new" data frame or \code{Date} vector used to
##'     define the "new" period. The quantiles will be the those of
##'     the random maximum \eqn{M} of the marks on this period.
##' 
##' @param prob Vector of probabilities.
##' 
##' @param level The confidence level.
##' 
##' @param ... Not used yet.
##' 
##' @return A data frame with columns \code{Prob} and \code{Quant}.
##'
##' @importFrom stats qnorm
##' @method quantile pgpTList
##' @export
##'
##' @seealso \code{\link{predict.pgpTList}}.
##'
##' @section Caution: This method is planned to be renamed as
##'     \code{quantMax}, to make a clearer difference with the
##'     marginal quantiles. The \code{\link{quantMax.pgpTList}}
##'     should thus be used.
##' 
##' @examples
##' RqU <- rqTList(dailyMet = Rennes, tau = c(0.94, 0.95, 0.96, 0.97, 0.98, 0.99))
##' Pgp1 <- pgpTList(dailyMet = Rennes, thresholds = RqU, declust = TRUE,
##'                  fitLambda = TRUE, logLambda.fun = ~YearNum - 1)
##' Date <- seq(from = as.Date("2020-01-01"), to = as.Date("2050-01-01"), by = "day")
##' ## compute the quantile for the maximum on the "new" period
##' qMax <- quantile(Pgp1, newdata = Date)
##' autoplot(qMax)
##'
##'
quantile.pgpTList <- function(x,
                              newdata = NULL,
                              prob = NULL,
                              level = 0.95,
                              ...) {

    warning("Please use the `quantMax` method to compute the quantiles ",
            "of the maximum.\n Since the name 'quantile' creates an ambiguity ",
            "with the marginal quantiles,\n this function might change in the ",
            "future")
    
    Prob <- ProbExc <- NULL
    
    if (!is.null(x$subset)) {
        stop("The computation of quantiles is not possible ",
             "when 'x$subset' is not 'NULL'")
    }

    pred <- predict(x, newdata = newdata, ...)

    modMat <- modelMatrices(x, newdata = newdata)
    covMat <- vcov(x)
    parInfo <- parInfo(x)
    
    tau1 <- attr(pred, "tau")
    nM <- 40
    L <- list()
    pred <- as.data.frame(pred)
    
    if (is.null(prob)) {
        prob <- 1 - c(0.06, 0.05, 0.04, 0.03, 0.02, 0.01,
                      0.0075,  0.005, 0.0025,
                      0.001, 0.002, 0.005)
    }
    
    indScale <- 1:parInfo$length$GP["scale"]
    indShape <- parInfo$length$GP["scale"] + (1:parInfo$length$GP["shape"])
    indLambda <- parInfo$length$GP["scale"] + parInfo$length$GP["shape"] +
        (1:parInfo$length$timePoisson)
    p <- parInfo$length$GP["scale"] + parInfo$length$GP["shape"] +
        parInfo$length$timePoisson

    
    for (i in seq_along(tau1)) {

        predi <- subset(pred, tau == tau1[i])
        mL <- max(predi$v, na.rm = TRUE)
        mU <- max(predi$omega, na.rm = TRUE)
        m <- seq(from = mL,  to = mU, length.out = nM)
        
        FM <- fM <- se <- rep(NA, nM)
        
        dFBeta <- array(NA, dim = c(nM, p))
            
        for (j in seq_along(m)) {

            FVec <- nieve::pGPD2(m[j] - predi$u,
                                 scale = predi$sigma,
                                 shape = predi$xiStar,
                                 deriv = TRUE)
            
            lambdaS <- predi$lambda * (1 - FVec)
            attr(lambdaS, "gradient") <- NULL
            FM[j] <- exp(-sum(lambdaS) / 365.25)
            fM[j] <- FM[j] * sum(predi$lambda *
                                 nieve::dGPD2(m[j] - predi$u,
                                              scale = predi$sigma,
                                              shape = predi$xiStar)) / 365.25
            
            ## =================================================================
            ## In any model matrix, the rows match times and the
            ## columns match the 'beta' parameters for the
            ## corresponding Poisson-GP parameter. The sum over the
            ## times can be obtained with a matrix multiplication.
            ## =================================================================
            
            mat <- modMat[["scale"]]
            weights <- predi$lambda * attr(FVec, "gradient")[ , "scale"]
            
            dFBeta[j, indScale] <- - drop(t(mat) %*% weights) * FM[j] / 365.25
            
            mat <- modMat[["shape"]]
            weights <- predi$lambda * attr(FVec, "gradient")[ , "shape"] 
            dFBeta[j, indShape] <- - drop(t(mat) %*% weights) * FM[j] / 365.25
            
            mat <- modMat[["logLambda"]]
            dFBeta[j, indLambda] <- - drop(t(mat) %*% lambdaS) * FM[j] / 365.25
            
            se[j] <- sqrt(t(dFBeta[j, ]) %*% covMat[[i]] %*% dFBeta[j, ]) / fM[j]
            
        }
        
        probL <- (1 - level) / 2
        probU <- 1 - probL
        qLU <- qnorm(cbind(probL, probU), mean = 0.0, sd = 1.0)
        if (FALSE) {
            dfi <- data.frame(tau = unname(tau1[i]),
                              Prob = prob,
                              ProbExc = 1 - prob,
                              Quant = approx(x = FM, y = m, xout = prob)$y)
        } else {
            
            dfi <- data.frame(tau = unname(tau1[i]),
                              Prob = FM,
                              ProbExc = 1 - FM,
                              Quant = m)
            
            dfi <- cbind(dfi, 
                         L = dfi$Quant + qLU[1] * se,
                         U = dfi$Quant + qLU[2] * se)

            if (TRUE) {
                roundQuant <- approx(x = dfi$Prob, y = dfi$Quant, xout = prob)$y
                roundL <- approx(x = dfi$Prob, y = dfi$L, xout = prob)$y
                roundU <- approx(x = dfi$Prob, y = dfi$U, xout = prob)$y
                roundDfi <- data.frame(tau = unname(tau1[i]),
                                   Prob = prob,
                                   ProbExc = 1 - prob,
                                   Quant = roundQuant,
                                   L = roundL,
                                   U = roundU)
                                   ## Level = level
                
                dfi <- rbind(dfi, roundDfi)
                dfi <- dfi[order(dfi$tau, dfi$Prob), ]
            }
        }
        
        L[[i]] <- dfi 
        
    }
    
    res <- data.table::rbindlist(L)
    res$tau <- factor(res$tau)
    res <- subset(res, Prob <= 1 - 1e-5 & Prob > 1e-5)
    attr(res, "level") <- level
    class(res) <- c("quantile.pgpTList", "data.frame")
    res
    
}


##' @method quantile predict.pgpTList
##' @export
quantile.predict.pgpTList <- function(x,
                                      prob = NULL,
                                      ...) {
    dots <- list(...)
    pm <- pmatch("newdata", names(dots))
    if (!is.na(pm)) {
        stop("'newdata' can not be given when the quantiles ",
             "are computed from predictions. Use either the ",
             "'rqTList' object in the formal 'x' of the 'quantile' ",
             "method, or use a new 'predict' step")
    }
    
    tau1 <- attr(x, "tau")
    nm <- 40
    L <- list()
    x <- as.data.frame(x)
    
    if (is.null(prob)) {
        prob <- 1 - c(0.06, 0.05, 0.04, 0.03, 0.02, 0.01,
                      0.0075,  0.005, 0.0025,
                      0.001, 0.002, 0.005)
    }

    for (i in seq_along(tau1)) {
        predi <- subset(x, tau == tau1[i])
        m <- seq(from = min(predi$v), to = max(predi$omega), length.out = nm)
        Fm <- rep(NA, nm)
        for (j in seq_along(m)) {
            Fm[j] <- exp(-sum(predi$lambda *
                          (1 - nieve::pGPD2(m[j] - predi$u,
                                            scale = predi$sigma,
                                            shape = predi$xiStar))) / 365.25)
        }
        L[[i]] <- data.frame(tau = unname(tau1[i]),
                             Prob = prob,
                             ProbExc = 1 - prob,
                             Quant = approx(x = Fm, y = m, xout = prob)$y)
        
    }
    
    res <- data.table::rbindlist(L)
    res$tau <- factor(res$tau)
    class(res) <- c("quantile.pgpTList", "data.frame")
    res
    
}

##' Round the values of the quantiles and confidence limits and select
##' those corresponding to "round" exceedance probabilities.
##'
##' It is often needed to show a table of quantiles corresponding to
##' round exceedance probabilities such as \code{0.01},
##' \code{0.001}. Moreover the quantiles and confidence limits as
##' printed by default with number of digits which may be higher than
##' needed. Most often, a number of digits between 0 and 2 is hight
##' enough in relation with the precision of the data used to compute
##' the quantiles.
##'
##' @title Round and Format Quantiles
##'
##' @param x An object with class \code{"quantile.pgpgTList"} as
##'     computed by the \code{quantile} method of the
##'     \code{"pgpgTList"} class.
##' 
##' @param digits The number of digits to be used for the colums
##'     \code{Quant}, \code{L} and \code{U} columns representing the
##'     quantile and the confidence limits.
##' 
##' @param probExc A numeric vector giving the exceedances
##'     probabilities to be displayed among those available in
##'     \code{x}. The default values correspond to 12 values
##'     \code{0.5}, \code{0.2}, \code{0.1}, ..., \code{1e-4}.
##' 
##' @param ... Not used.
##'
##' @return A data frame with the rounded quantiles and confidence
##'     limits at the" chosen "round" exceedance probabilities.
##'
##' @export
##' @method format quantile.pgpTList
##' 
format.quantile.pgpTList <- function(x,
                                     digits = 2,
                                     probExc = as.vector(
                                         outer(c(5, 2, 1),
                                               c(1e-1, 1e-2, 1e-3, 1e-4))),
                                     ...) {

    ProbExc <- NULL
    
    if (!is.null(probExc)) {
        xp <- subset(as.data.frame(x), subset = round(ProbExc, digits = 6) %in% probExc)
    } else {
        xp <- as.data.frame(x)
    }
    xp <- within(xp, {
        Quant <- round(Quant, digits = digits);
        L <- round(L, digits = digits);
        U <- round(U, digits = digits);
    })
    
    xp
}

## =============================================================================
## Prepare renaming
## =============================================================================


## ##' 
## ##' The (S3) generic function \code{quantMax} evaluates the quantiles
## ##' for a maximum as is often of interest in Extreme-Value
## ##' modelling. The methods for this function should require the
## ##' definition of a period of time, a region in space or a
## ##' spatio-temporal domain on which the (random) maximum is computed
## ##' and the probability of interest, usually close to one.
## ##'
## ##' Note that for some tasks such as building return level plots it to
## ##' have a large number of probabilities, yet one usually focus on
## ##' "pretty" or round probabilities such as \code{0.9}, \code{0.99}
## ##' and \code{0.999} to summarize the results.
## ##' 
## ##' @title Compute Quantiles for a Maximum
## ##' 
## ##' @param x An object representing a fitted model from which the
## ##'     distribution of the maximum (or at least its tail) can be
## ##'     found.
## ##' 
## ##' @param ... Arguments for methods.
## ##'
## ##' @return An object containing the wanted quantiles. As a rule this
## ##'     object could have a (S3) class extending the data frame class,
## ##'     and the returned object could have columns corresponding to
## ##'     the probability and the quantile, plus other columns such as
## ##'     confidence limits and possibly a confidence level.
## ##'
## ##' @export
## ##' 
## quantMax <- function(x, ...) {
##     UseMethod("quantMax")
## }





## *****************************************************************************

##' The Poisson-GP model given in \code{x} only describes the tail of
##' the distribution of the maximum. So when a probability is too
##' small the quantile may be \code{NA}.
##'
##' The computation of the quantiles without any inference result can
##' be performed at a lower cost by using the results of a
##' \code{predict} step it these have already been computed. Of course
##' the "new" period will be that which was used in the \code{predict}
##' step and can not be changed.
##' 
##' @title Compute Quantiles for the Maximum the Marks of a Poisson-GP
##'     Model
##'
##' @param object An object with class \code{"predict.pgpTList"} as created
##'     by applying the \code{predict} method on an object with class
##'     \code{"pgpTList"}.
##' 
##' @param newdata A "new" data frame or \code{Date} vector used to
##'     define the "new" period. The quantiles will be the those of
##'     the random maximum \eqn{M} of the marks on this period.
##' 
##' @param prob Vector of probabilities.
##' 
##' @param level The confidence level.
##' 
##' @param ... Not used yet.
##' 
##' @return A data frame with columns \code{Prob} and \code{Quant}.
##'
##' @importFrom stats qnorm
##' @importFrom NSGEV quantMax
##' 
##' @method quantMax pgpTList
##'
##' @export
##'
##' @seealso \code{\link{predict.pgpTList}}.
##' 
##' @examples
##' RqU <- rqTList(dailyMet = Rennes, tau = c(0.94, 0.95, 0.96, 0.97, 0.98, 0.99))
##' Pgp1 <- pgpTList(dailyMet = Rennes, thresholds = RqU, declust = TRUE,
##'                  fitLambda = TRUE, logLambda.fun = ~YearNum - 1)
##' Date <- seq(from = as.Date("2020-01-01"), to = as.Date("2050-01-01"), by = "day")
##' ## compute the quantile for the maximum on the "new" period
##' qMax <- quantMax(Pgp1, newdata = Date)
##' autoplot(qMax)
##' 
quantMax.pgpTList <- function(object,
                              newdata = NULL,
                              prob = NULL,
                              level = 0.95,
                              ...) {

  quantile.pgpTList(object,
                    newdata = newdata,
                    prob = prob,
                    level = level,
                    ...) 
}

##' @method quantMax predict.pgpTList
##' @export
quantMax.predict.pgpTList <- function(object,
                                      prob = NULL,
                                      ...) {

    quantile.predict.pgpTList(object,
                              prob = prob,
                              ...) 
}


##' @export
##' @method format quantMax.pgpTList
format.quantMax.pgpTList <- function(x,
                                     digits = 2,
                                     probExc = as.vector(
                                         outer(c(5, 2, 1),
                                               c(1e-1, 1e-2, 1e-3, 1e-4))),
                                     ...) {

    format.quantile.pgpTList(x,
                             digits = 2,
                             probExc = probExc,
                             ...) 
        

}
##' @export
##' @method autoplot quantile.pgpTList
##' 
autoplot.quantile.pgpTList <- function(object, facet = TRUE,
                                       ...) {
    
    g <- ggplot(data = object)
    
        ## geom_line(mapping = aes_string(x = "ProbExc", y = "Quant",
        ##                               colour = "tau"))
    
    g <- g + scale_x_continuous(trans = .gumbel_trans_p,
                                breaks = .gumBreaks_p,
                                minor_breaks = .gumBreaks_p) +
        xlab("Prob. of exceedance") + ylab("Quantile") +
        ggtitle(sprintf(paste("Tail distribution for the maximum with %2.0f %%",
                        "confidence interval"),
                        100 * attr(object, "level")))
       
    if (facet) {
        g <- g +
            geom_ribbon(
                data = object,
                mapping = aes_string(x = "ProbExc", ymin = "L", ymax = "U"),
                fill = "SteelBlue3", alpha = 0.3)
        g <- g +
            geom_line(
                data = object,
                mapping = aes_string(x = "ProbExc", y = "L"),
                colour = "SteelBlue3")
        g <- g +
            geom_line(
                data = object,
                mapping = aes_string(x = "ProbExc", y = "U"),
                colour = "SteelBlue4")
        g <- g + geom_line(mapping = aes_string(x = "ProbExc", y = "Quant",
                                                colour = "tau"), size = 0.6)
        
        g <- g + scale_colour_brewer(palette = "Set2")
        g <- g + facet_wrap(tau ~ ., labeller = label_both)
    } else {
        g <- g +
            geom_line(
                data = object,
                mapping = aes_string(x = "ProbExc", y = "L", colour = "tau"),
                linetype = "dashed")
        g <- g +
            geom_line(
                data = object,
                mapping = aes_string(x = "ProbExc", y = "U", colour = "tau"),
                linetype = "dashed")
        g <- g + geom_line(mapping = aes_string(x = "ProbExc", y = "Quant",
                                                colour = "tau"), size = 0.6)
        
        g <- g + scale_colour_brewer(palette = "Set2")
    }
   
    g

}

##' @export
##' @method autoplot quantMax.pgpTList
##' 
autoplot.quantMax.pgpTList <- function(object, facet = TRUE,
                                       ...) {
    autoplot.quantile.pgpTList(object, facet = TRUE, ...)

}
