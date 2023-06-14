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
##'         Fit a non-stationary temporal Poisson process model using
##'         \code{NHPoisson::fitPP.fun}. 
##'      }
##' }
##' The formula given in \code{logLambda.fun} will typically involve
##' \code{YearNum}. Note that the constant is alaways included by
##' the function \code{NHPoisson::fitPP.fun} so it should be discarded
##' from the formula when covariates are used. For instance in order
##' to use the covariare \code{YearNum} we must use the formla
##' \code{~ YearNum -1}.
##'
##' @title Fit a non-stationary Poisson-GP Model using several
##'     Thresholds computed by Quantile Regression.
##'
##' @param dailyMet An object with class \code{"dailyMet"} containing
##'     the data.
##'
##' @param subset A character defining a condition used to subset the
##'     data, typically to select a period within the year e.g.,
##'     summer. Note that the condition is applied \emph{after
##'     declustering} and there are side effects: The exceedances in
##'     the first two days or last two days of a period within year
##'     may be lost. \emph{NOT IMPLEMENTED YET}. Note that the
##'     condition is used only for the Extreme Value part of the
##'     model, not on the determination of the threshold. Therefore,
##'     \emph{the fitted thresholds remain unchanged} whether the the
##'     condition is used or not.
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
##'     \bold{Details}. Mind that \emph{only numeric covariates can be
##'     used}.
##'
##' @param scale.fun,shape.fun formulas for the scale and the shape
##'     parameters of the Generalized Pareto distribution for the
##'     exceedances over the threshold.
##'
##' @param extraDesign A list with a specific structure used to
##'     generate extra "design variables" that can be used in
##'     formulas, see \bold{Examples} and the help of
##'     \code{\link{designVars}} and \code{\link{rqTList}} for
##'     examples of syntax. By default a trigonometric design with
##'     three harmonics is used, along with the sinus waves basis
##'     corresponding to the phases of the reference threshold defined
##'     by \code{tauRef}. The sinus wave variables are created by
##'     using the \code{\link{sinBasis}} function with the relevant
##'     value of \code{phi}.
##' 
##' @param trace Integer level of verbosity. 
##'
##' @param scale.fun,shape.fun Formulas for the GP scale and shape as
##' in \code{\link[extRemes]{fevd}}.
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
##' @importFrom NHPoisson fitPP.fun
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
##' @note The use of \code{subset}, still experimental, is aimed to
##'     focus the estimation on a period \emph{within the year},
##'     typically summer or winter. This should not be used to use a
##'     shorter timeseries. The rate of the temporal Poisson process
##'     is computed on the basis of the full times series, not on the
##'     basis of the cumulated duration of the subset. For instance if
##'     \code{subset} selects 6 months in a year and if the timeseries
##'     duration is 60 year, then the rate is still computed on the
##'     basis of a 60-year duration, not on a 30-year duration.
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
##' \dontrun{
##'     Pgp2 <- pgpTList(dailyMet = Rennes, thresholds = Rq,
##'                      declust = TRUE, 
##'                      shape.fun = ~ Cst + sinjPhi1 + sinjPhi2 + sinjPhi3 - 1,
##'                      fitLambda = TRUE, logLambda.fun = ~ YearNum - 1)
##' }
##' ## plot one year to 
##' predYear <- predict(Pgp1, last = TRUE)
##' gYear <- autoplot(predYear, facet = FALSE)
##' gYear
##' 
##' ## show the evolution of the exceedance rate on the long-run
##' predAll <- predict(Pgp1, last = FALSE)
##' exceed <- exceed(Pgp1)
##' gAll <- autoplot(predAll, which = "lambda", size = 1.2) +
##'             geom_point(data = exceed, mapping = aes(x = Date, y = Nb)) +
##'                 ggtitle(paste("Fitted rate 'lambda' and annual number of",
##'                               "declustered exceedances"))
##' gAll
##'
##' ## Add an extra design corresponding to broken line splines. This
##' ## creates a new variable 't1_1970' that can be used in formula to
##' ## assess a possible change in the trend at the beginning of the seventies.
##' Pgp3 <-
##'     pgpTList(dailyMet = Rennes, thresholds = Rq,
##'              declust = TRUE,
##'              fitLambda = TRUE,
##'              logLambda.fun = ~ YearNum + t1_1970 - 1,
##'              extraDesign =
##'                  list("breaks" = list(what = "NSGEV::breaksX",
##'                                       args = list(breaks = c("1970-01-01", "1990-01-01")))))
##' p3 <- predict(Pgp3,
##'               newdata = data.frame(Date = seq(from = as.Date("2024-01-01"),
##'                                    to = as.Date("2054-01-01"),
##'                                    by = "day")))
##' autoplot(p3, facet = FALSE)
##'
##' ## simulate 
##' s3 <- simulate(Pgp3, nsim = 10,
##'               newdata = data.frame(Date = seq(from = as.Date("2024-01-01"),
##'                                    to = as.Date("2054-01-01"),
##'                                    by = "day")))
##' autoplot(s3)
##' 
pgpTList <- function(dailyMet,
                     subset = NULL,
                     thresholds,
                     declust = TRUE,
                     tauRef = 0.95,
                     fitLambda = FALSE,
                     logLambda.fun = ~1,
                     scale.fun = ~Cst + sinjPhi1 + sinjPhi2 + sinjPhi3 - 1,
                     shape.fun = ~1,
                     extraDesign = NULL,
                     trace = 1) {
    
    .Ind <- NULL
                         
    
    logLambda.fun <- as.formula(logLambda.fun)
    
    TX <- u <- NULL ## avoid warnings at check
    
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
    
    Kthresh <- checkTrigNames(threshold.fun)
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

    ## =========================================================================
    ## Add extra design variables if needed.
    ## 
    ## =========================================================================

    if (!is.null(extraDesign)) {
        X <- designVars(designList = extraDesign, dt = dailyMet$Date, trace = trace)
        dailyMet <- data.frame(dailyMet, X)
    }
   
    tun <- sprintf("%6.2f/year", nrow(dailyMet) / duration)
    cat(sprintf("o Sampling rate : %s\n", tun))
    
    U <- Clusters <- FitGP <- FitLambda <- IndLambda <- list()
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
        
        Clusters[[i]] <- res
        
        ## =====================================================================
        ## now add the threshold and decluster if required.
        ## =====================================================================
        
        Met2 <- data.frame(dailyMet, u = U[[i]])
        
        if (declust) {
            ## Met2 <- Met2[res$IndClust, ]
            indClust <- res$IndClust
            IndLambda[[i]] <- res$IndClust
        } else {
            indClust <- 1:nrow(Met2)
            IndLambda[[i]] <- (1:nrow(Met2))[Met2[[metVar]] > U[[i]]]
        }
        
        Met2 <- within(Met2, Excess <- TX - u)
        
        if (!is.null(subset)) {
            
            if (trace) cat("\no Using 'subset'\n")
            if (trace) cat("    o Number of rows before:    ", nrow(Met2), "\n")
            
            Met2 <- data.frame(Met2, .Ind = 1L:nrow(Met2))
            Met2 <- subset(Met2, subset = eval(parse(text = subset)))
            indClust <- (1L:nrow(Met2))[indClust %in% Met2$.Ind]
            IndLambda[[i]] <- (1L:nrow(Met2))[IndLambda[[i]] %in% Met2$.Ind]
            
            Met2 <- subset(Met2, select = - .Ind)
            if (trace) cat("    o Number of rows after:     ", nrow(Met2), "\n\n")
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
        
        lambdaBar[i] <- nrow(Met2[IndLambda[[i]], ]) / duration
        
        ## =====================================================================
        ## fit the Poisson (temporal) part
        ## =====================================================================
        
        if (fitLambda) {

            ## we must use 'all.equal' here and not 'identical', because
            ## the two formulas do not have the same environement!

            lambdaNH <- !(all.equal(logLambda.fun, ~1) == TRUE)
            
            if (lambdaNH) {

                if (trace) cat("\no Fit the temporal Poisson process:",
                               " non-homogeneous\n")
                
                Covs <- model.matrix(logLambda.fun, data = Met2)
                L <- rep(0, ncol(Covs))
                names(L) <-  paste0("b", 1:ncol(Covs))
            
                FitLambda[[i]] <-
                    NHPoisson::fitPP.fun(tind = TRUE,
                                         covariates = Covs, 
                                         posE = IndLambda[[i]],
                                         dplot = FALSE,
                                         start = c(list(b0 = 10), as.list(L)))   
            } else {

                if (trace) cat("\no Fit the temporal Poisson process:",
                               " homogeneous\n")
          
                FitLambda[[i]] <-
                    NHPoisson::fitPP.fun(tind = TRUE,
                                         nobs = nrow(Met2),
                                         covariates = NULL, 
                                         posE = IndLambda[[i]],
                                         dplot = FALSE,
                                         start = list(b0 = 10)) 
            }
        }
        
    }

    ## names of the GP list
    if (!is.null(names(thresholds))) {
        names(Clusters) <- names(FitGP) <- names(FitLambda) <- names(thresholds)
    } else {
        names(Clusters) <- names(FitGP) <- names(FitLambda) <-
            paste0("tau=", format(tau))
    }

    clusterSize <- sapply(Clusters, function(x) mean(x$end- x$start + 1))
    
    res <- list(tau = tau,
                tauRef = tauRef,
                lambdaBar = lambdaBar,
                dailyMet = dailyMetBAK,
                subset = subset,
                data = dailyMet,
                IndLambda = IndLambda,
                thresholds = thresholds,
                declust = declust,
                clusters = Clusters,
                clusterSize = clusterSize,
                logLambda.fun = logLambda.fun,
                scale.fun = scale.fun,
                shape.fun = shape.fun,
                extraDesign = extraDesign,
                fitLambda = fitLambda,
                timePoisson = FitLambda,
                GP = as.fevdTList(FitGP))
    
    class(res) <- c("pgpTList", "list")
    res

}

## *****************************************************************************

##'
##' @title Summary Method for `pgpTList` Objects.
##' 
##' @param object A \code{pgpTList} object representing a list of
##'     Poisson-GP fitted models
##'
##'
##' @param ... Not used yet.
##'
##' @return An object with class \code{"pgpTList"}.
##'
##' @method summary pgpTList
##'
##' 
##' @export
summary.pgpTList <- function(object, ...) {
    res <- object
    if (object$declust) {
        CL <- sapply(res$clusters, function(x) x$end- x$start + 1)
        res$clustersStat <-
            data.frame(mean = round(sapply(CL, mean), digits = 2),
                       min  = sapply(CL, min),
                       max  = sapply(CL, max))
    } 
    class(res) <- "summary.pgpTList"
    res
}

## *****************************************************************************
##' @method print summary.pgpTList
##' @export
##' 
print.summary.pgpTList <- function(x, ...) {
    cat("Object with class \"pgpTList\"\n")
    cat("o Threshold part\n")
    print(x$thresholds)
    if (x$declust) {
        cat("o Clusters\n")
        print(x$clustersStat)
    }
    cat("\no \"GP\" part: coefficients for the Generalized Pareto",
        " with standard errors\n")
    print(coSd(x$GP))
    cat("\no \"Time\" part: coefficients for log-rate with standard errors\n")
    print(noquote(t(sapply(x$timePoisson, coSd))))
}

## *****************************************************************************
##' @method print pgpTList
##' @export
##' 
print.pgpTList <- function(x, ...) {
    cat("Object with class \"pgpTList\"\n\n")
    cat("o \"Threshold\" part:\n")
    print(x$thresholds)
    cat("\no Formulas\n")
    cat("    o GP scale:      ", noquote(format(x$scale.fun)), "\n")
    cat("    o GP shape:      ", noquote(format(x$shape.fun)), "\n")
    cat("    o log-rate:      ", noquote(format(x$logLambda.fun)), "\n")
    cat("Use 'summary' for more information\n")
}

## *****************************************************************************
##'
##' @title logLik Method for `pgpTList` Objects.
##' 
##' @param object A \code{pgpTList} object representing a list of
##'     Poisson-GP fitted models
##'
##' @param ... Not used yet.
##'
##' @return A numeric matrix with two columns containing the
##'     log-likelihoods for the "GP" part and the "time Poisson"
##'     part. This matrix has as a \code{"df"} attribute a vector with
##'     length 2 giving the degrees of freedom used by the two parts
##'     of the model. While the log-likelihoods differ across
##'     thresholds, the degrees of freedom are the same by
##'     construction.
##'
##' @section Caution: One should not compare log-likelihoods for POT
##'     models using different thresholds. So the comparison only
##'     makes sense when one use two \code{pgpTlist} objects using the
##'     same probabilities \code{tau} and the same formulas in the
##'     quantile regression as well as the same data.
##' 
##' @importFrom stats logLik
##' @method logLik  pgpTList
##' @export
##' 
logLik.pgpTList <- function(object, ...) {
    
    logLik <- array(as.numeric(NA), dim = c(length(object$tau), 2),
                    dimnames = list(names(object$tau),
                                    c("GP", "timePoisson")))
    df <- rep(NA, 2)
    names(df) <- c("GP", "timePoisson")
    
    for (i in seq_along(object$tau)) {
        if (object$GP[[i]]$results$convergence == 0) {
            logLik[i, 1] <- -object$GP[[i]]$results$value
        }
        if (object$timePoisson[[i]]@convergence == 0) {
            logLik[i, 2] <- - object$timePoisson[[i]]@min
        }
    }
    
    df[1] <- length(object$GP[[1]]$parnames)
    df[2] <-  object$timePoisson[[1]]@npar
    attr(logLik, "df") <- df

    logLik
}

## *****************************************************************************
##'
##' @title Checks that two \code{pgpTList} Objects correspond to
##'     Nested Models.
##'
##' @param object,object1 Two \code{pgpTlist} objects for which we
##'     want to know if \code{object} is nested in \code{object1}.
##'
##' @param ... Not used
##'
##' @return Either the logical \code{TRUE} or a character message
##'     indicating the source from which one can conclude that the
##'     models are not nested.
##'
##' @export
##' 
isNested <- function(object, object1, ...) {
    
    if (!all.equal(object$dailyMet, object1$dailyMet)) {
        return("The 'dailyMet' parts of the objects differ") 
    }
    if (!all.equal(object$tau, object1$tau)) {
        return("The 'tau' parts of the objects differ") 
    }
    for (i in seq_along(object$tau)) {
        if (!all.equal(object$thresholds[[i]], object1$thresholds[[i]])) {
            return("quantile regression models for tau = ", tau, "differ") 
        }       
    }
    
    tl <- attr(terms(object$scale.fun), "term.labels")
    tl1 <- attr(terms(object1$scale.fun), "term.labels")
    if (!all(tl %in% tl)) {
        return("'scale.fun' indicates that 'object' is not nested in",
             " 'object1'") 
    }
    tl <- attr(terms(object$shape.fun), "term.labels")
    tl1 <- attr(terms(object1$shape.fun), "term.labels")
    if (!all(tl %in% tl)) {
        return("'scale.fun' indicates that 'object' is not nested in",
             " 'object1'") 
    }
    TRUE
}

## *****************************************************************************
##' 
##' @title Log-Likelihood Ratio for \code{pgpTList} objects 
##'
##' @param object,object1 Two \code{pgpTlist} objects for with
##'     \code{object} nested in \code{object1}.
##'
##' @param which The log-likelihood to be used. With the values
##'     \code{"GP"} and \code{"timePoisson"}, only the corresponding
##'     part of the Poisson-GP model is used. With the value
##'     \code{"total"} the tola log-likelihood is used.
##' 
##' @param ... Not used yet.
##'
##' @return A matrix with the results likelihood-ratio tests, one by
##'     threshold.
##'
##' @importFrom stats pchisq
##' 
anova.pgpTList <- function(object, object1,
                           which = c("total", "GP", "timePoisson"),
                           ...) {

    which <- match.arg(which)

    if (res <- isNested(object, object1) != TRUE) stop(res) 
    
    if (which == "total") which <- c("GP", "timePoisson") 
    
    ll <- logLik(object)
    df <- sum(attr(ll, "df")[which])
    ll <- apply(ll[ , which, drop = FALSE], 1, sum)
    
    ll1 <- logLik(object1)
    df1 <- sum(attr(ll1, "df")[which])
    ll1 <- apply(ll1[ , which, drop = FALSE], 1, sum)
    
    ddf <- df1 - df
    stat <- 2 * (ll1 - ll)
    pVal <- pchisq(stat, df = ddf, lower.tail = FALSE)
    cbind("logLik0" = round(ll, digits = 2),
          df = df,
          "logLik1" = round(ll1, digits = 2),
          df1 = df1, 
          "stat" = round(stat, digits = 2),
          "p-value" = pVal)
    
}
