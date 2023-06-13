##' Create a \code{qrList} object by calling \code{\link{rq}} for each
##' of the probabilities given in \code{tau}, using the same formula
##' and data for all fits.
##'
##' When \code{extradesign} is used, any design function used should
##' fulfill the following requirements.
##' 
##' \enumerate{
##'     \item The first argument must be the date. The name of this
##'       argument will not be used in the call.
##'     \item  The returned value should be an object inheriting from
##'        the \code{"matrix"} class with suitable column names. It can also
##'        be a list containing the design
##'        matrix as its element named \code{"X"}.
##' }
##' The call to a design function should mention \code{Date} as the
##' first argument. This call refers to the \code{dailyMet} object
##' used, from which the \code{Date} column will be used.
##' 
##' @title Create A \code{rqTList} Object by Repeated Calls to
##'     \code{rq}
##'
##' @param formula The formula that will be used in
##'     \code{\link[quantreg]{rq}}.
##' 
##' @param dailyMet An object with class \code{"dailyMet"} that
##'     contains the variables that will be used in the formula or in
##'     the design function specified \code{design}.
##' 
##' @param tau A vector of probabilities.
##'
##' @param design A list of arguments to be passed to
##'     \code{\link{designVars}}. For each element of this list, the
##'     variables of the design are added to the data frame given in
##'     \code{dailyMet} before fiiting the model. The default creates
##'     seven trigonometric basis functions for the first three
##'     harmonics of the yearly seasonality.
##'
##' @param trace Integer level of verbosity.
##' 
##' @return An object with class \code{"rqTList"}.
##'
##' @importFrom quantreg rq
##' @export
##' 
##' @examples
##' Rq <- rqTList(dailyMet = Rennes)
##' coef(Rq)
##' autoplot(Rq)
##' if (require("NSGEV")) {
##' 
##'    Rq1 <- rqTList(formula = TX ~ Cst + cosj1 + sinj1 + cosj2 + sinj2 + t1_1970 - 1,
##'                   dailyMet = Rennes,
##'                   design = list("trigo" = list(what = "tsDesign",
##'                                                args = list(type = "trigo", df = 7)),
##'                                 "breaks"= list(what = "NSGEV::breaksX",
##'                                                args = list(breaks = c('1970-01-01', '1990-01-01')))))
##'    p1 <- predict(Rq1,
##'                   newdata = data.frame(Date = seq(from = as.Date("2024-01-01"),
##'                                        to = as.Date("2054-01-01"),
##'                                        by = "day")))
##'    autoplot(p1)
##' }
##' 
rqTList <- function(formula = TX ~ Cst + cosj1 + sinj1 + cosj2 + sinj2 + cosj3 + sinj3 - 1,
                    dailyMet,
                    tau = c(0.5, 0.70, 0.80, 0.90, 0.95, 0.97, 0.98, 0.99),
                    design = list("trigo" = list(what = "tsDesign", args = list(type = "trigo", df = 7))),
                    trace = 0) {
    
    mc <- match.call()
    
    if (is.null(mc$design)) {
        ## mc$design <- formals(rqTList)$design
        mc$design <- eval(formals(rqTList)$design)
    } else {
        mc$design <- eval(mc$design)
    }
    
    if (!inherits(dailyMet, "dailyMet")) {
        stop("'dailyMet' must be an object with class \"dailyMet\"")
    }

    attrList <- list(metVar = attr(dailyMet, "metVar"),
                     station = attr(dailyMet, "station"),
                     code = attr(dailyMet, "code"))

    if (trace) {
        cat("o Adding the design variables\n")
    }
    X <- designVars(designList = mc$design, dt = dailyMet$Date, trace = trace)
    dailyMet <- data.frame(dailyMet, X)
    
    ## for (ides in seq_along(design)) {
    ##     argList <- design[[ides]]
    ##     argList <- c(list(dt = dailyMet$Date), design[[ides]])
    ##     des <- do.call("tsDesign", argList)
    ##     dailyMet <- data.frame(dailyMet, des$X)
    ## }
    
    ## for (ides in seq_along(extraDesign)) {
    ##     if (is.character(ed <- extraDesign[[ides]])) {
    ##         X <- eval(parse(text = ed), envir = dailyMet)
    ##         if (inherits(X, "matrix")) {
    ##             X <- unclass(X)
    ##         } else if (inherits(X, "list")) {
    ##             if (inherits(X$X, "matrix")) {
    ##                 X <- X$X
    ##             } else {
    ##                 stop("Unsuitable result from design function")
    ##             }
    ##         }
    ##         dailyMet <- data.frame(dailyMet, X)
    ##     } 
    ## }
    
    fit <- u <- list()
    
    for (i in seq_along(tau)) {
        
        fit[[i]] <- rq(formula = substitute(formula), data = dailyMet,
                       tau = tau[i], na.action = "na.exclude")
        
        u[[i]] <- predict(fit[[i]], newdata = dailyMet, na.action = na.pass)

        if (FALSE) {
            dfi <- data.frame(Date = dailyMet$Date,
                              DateRef = dailyMet$DateRef,
                              Year = dailyMet$Year,
                              YearW = dailyMet$YearW,
                              DayW = dailyMet$DayW,
                              DateRefW = dailyMet$DateRefW,
                              JJA = dailyMet$JJA,
                              DJF = dailyMet$DJF,
                              tau = Tau[i],
                              TX = dailyMet$TX,
                              u = u[[i]])
            if (i == 1) dfu <- dfi
            else dfu <- rbind(dfu, dfi, deparse.level = 0)                     
        }
        
    }
    
    ## dfu <- within(dfu, tau <- as.factor(tau))
    names(fit) <- paste0("tau=", format(tau))
    
    for (nm in c("metVar", "station", "code")) {
        attr(fit, nm) <- attrList[[nm]]
    }

    attr(fit, "call") <- mc
    attr(fit, "data") <- dailyMet
    class(fit) <- c("rqTList", "list")
    
    fit

}
    
## *****************************************************************************
##' Create a \code{rqTList} object representing a collection of
##' \code{rq} objects that differ only by the value of \code{tau}.
##' This class is useful to compare the object e.g., graphically.
##'
##' @title Create a \code{rqTList} Object
##'
##' @param ... A collection of objects with class \code{"rq"}
##' with the same \code{type}
##'
##' @section Caution: This class contains lists of \code{rq} objects
##'     sharing the \emph{same formula} and only differing by the
##'     value of the probability \code{tau}.
##' 
##' @return An object with class \code{"rqTList"}.
##'
##' ##@export
##'

## rqTList <- function(...) { 
##     L <- list(...)
##     if (!all(sapply(L, class) == "rq")) {
##         stop("all objects given in `...` must have class ",
##              "\"rq\"")
##     }
##     LF <- lapply(L, formula)
##     if (length(unique(LF)) > 1) {
##         stop("all objects given in ... must have the same formula")
##     }
##     if (is.null(names(L))) {
##         Tau <- lapply(L, function(x) x$tau)
##         names(L) <- paste0("tau=", format(Tau))
##     }
##     class(L) <- c("rqTList", "list")
##     L
## }


##' @title Coerce into a \code{rqTList} object.
##'
##' @param object An object to coerce, typically a list object.
##'
##' @param ... Not used for now.
##' 
##' @return An object with S3 class \code{"rqTList"}
##'
##' @note It is a good practice to use a \emph{named} list.
##' 
##' @export
##' 
`as.rqTList` <- function(object, ...) {
    
    if (!all(sapply(object, class) == "rq")) {
        stop("all items in 'object' must have class ",
             "\"rq\"")
    }
    
    LF <- lapply(object, formula)
    if (length(unique(LF)) > 1) {
        stop("all element of `object` must have the same formula")
    }
    
    if (is.null(names(object))) {
        Tau <- lapply(object, function(x) x$tau)
        names(object) <- paste0("tau=", format(Tau))
    }
    
    class(object) <- c("rqTList", "list")
    object
}

#'
##' @export
##' @method coef rqTList
##' 
`coef.rqTList` <- function(object, ...) {
    t(sapply(object, coef))
}

##' Extracts and show the coefficients of a \code{rqTList} object
##' along with their standard deviations.
##' 
##' @title Coefficients and Standard Errors
##' 
##' @param object An object with class \code{"rq"}.
##'
##' @param sOnly Logical. If \code{TRUE} only the standard deviation
##'     (a.k.a standard errors) wille be show.
##'
##' @param ... Not used.
##'
##' @note This method is very slow, because it calls
##'     \code{\link[quantreg]{summary.rq}}.
##' 
##' @export
##' @method coSd rqTList
##'
##' @examples
##' library(extRemes)
##' data(Fort)
##' tau <- c(0.95, 0.97, 0.98)
##' Rq <- list()
##' for (i in 1:3) {
##'    Rq[[i]] <- rq(Prec ~ year, data = Fort, tau = tau[i])
##' }
##' Rq <- as.rqTList(Rq)
##' coSd(Rq)
##' 
`coSd.rqTList` <- function(object, sOnly = FALSE, ...) {
    res <- sapply(object, coSd, sOnly)
    res <- t(res)
    if (!sOnly) res <- noquote(res)
    res
}

# *****************************************************************************
##' @importFrom stats formula
##' @export
##' @method formula rqTList
##' 
formula.rqTList <- function(x, ...) {

    LF <- lapply(x, formula)
    if (length(uF <- unique(LF)) > 1) {
        stop("all element of `x` must have the same formula")
    }
    uF[[1]]
    
}

# *****************************************************************************
##'
##' @title Probability for a Quantile Regression Object 
##'
##' @param object Quantile regression object.
##' 
##' @param ... Arguments for further methods.
##'
##' @return The vector of (non-exceedance) probabilitiies.
##'
##' @export
##' 
tau <- function(object, ...) {
    UseMethod("tau")
}


# *****************************************************************************
##' @export
##'
##' @method tau rqTList
##'
##' @rdname tau
##' 
tau.rqTList <- function(object, ...) {
    sapply(object, function(x) x$tau)
}

##  *****************************************************************************
##' Predict from a \code{rqTList} Object.
##' 
##' @param object A \code{rqTList} object representing a list of
##'     regression quantile models as created by the
##'     \code{\link[quantreg]{rq}} function of the \pkg{quantreg}
##'     package.
##'
##' @param newdata New data frame.
##'
##' @param lastFullYear Logical. If \code{TRUE}, only the last full
##'     year in the data used to fit the quantile regressions will be
##'     used. If the regression quantile uses only functions of the
##'     date that have the one-year periodicity, the one should use
##'     \code{lastFullYear = TRUE}.
##'    
##' @param out Character giving the format of the output. If
##'     \code{"short"} a numeric matrix is returned with the formated
##'     date as its rownames. If \code{"long"} a data frame with
##'     columns \code{Date}, \code{u} and \code{tau} is returned.  The
##'     long format is useful when ggplots areto be produced.
##'
##' @param na.action See \code{\link[quantreg]{predict.rq}}.
##'
##' @param trace Integer level of verbosity.
##' 
##' @param ... Further arguments passed to the \code{predict} method
##'     of the class \code{"rq"}.
##' 
##' @return A matrix or a data frame, depending on the value or
##'     \code{out}.
##'
##' @section Caution: For now, a \code{rqTList} object does not store
##'     the calls to design functions used at creation time, although
##'     these designs functions may have to be re-called on
##'     \code{newdata}. A quick and dirty solution is used, based on
##'     the variables names. It works only for trigonometric bases
##'     created by using `tsDesign` with `type = "trigo"`.
##' 
##' @importFrom stats na.pass
##' @export
##' 
##' @method predict rqTList
##'
##' @examples
##' RqRennes <- rqTList(dailyMet = Rennes)
##' ## use a matrix
##' pRennes <- predict(RqRennes, out = "short")
##' \dontrun{
##'     stat <- findStationMF("bordeaux-me")
##'     ## you may here have to use `Sys.setenv(metData = xxx)`
##'     Bordeaux <- readMet(stat)
##'     RqBordeaux <- rqTList(dailyMet = Bordeaux)
##'     pBordeaux <- predict(RqBordeaux, out = "short")
##'     pBordeaux - pRennes
##'     autoplot(as.ts(pBordeaux - pRennes)) +
##'        ggtitle("differences in the quantiles Bordeaux - Rennes")
##' }
predict.rqTList <- function(object,
                            newdata,
                            lastFullYear,
                            out = c("long", "short"),
                            na.action = na.pass,
                            trace = 0,
                            ...)  {

    TX <- u <- NULL
    
    out <- match.arg(out)
    nq <- length(object)

    if (missing(newdata)) {
        newdata <- attr(object, "data")
        if (missing(lastFullYear)) lastFullYear <- TRUE
    } else {

        ## =====================================================================
        ## XXX We check the existence of the variables in 'newdata'
        ## before (possibly re-) creating them. Yet this should be
        ## made more general since some other designs can be
        ## used. Hopefully, if some variables are still missing, the
        ## error message thrown will be easy to understand.
        ## =====================================================================
        
        if (missing(lastFullYear)) lastFullYear <- FALSE
        fmOK <- try(model.frame(delete.response(terms(formula(object))),
                                newdata), silent = TRUE)
        if (inherits(fmOK, "try-error")) {
            K <- checkTrigNames(formula(object))
            if (K) {
                des <- tsDesign(dt = newdata$Date,
                                type = "trigo", df = 2 * K + 1)
                newdata <- data.frame(newdata, des$X)
            }
        }
        
        mc <- attr(object, "call")
        
        if (!is.null(mc$design)) {
            X <- designVars(designList = mc$design, dt = newdata$Date, trace = trace)
            newdata <- data.frame(newdata, X)
        }
        
    }
    
    ## ==========================================================================
    ## XXX should be done before for the sake of efficiency
    ## ==========================================================================
    
    DateTxt <- format(newdata$Date, "%Y-%m-%d")
    
    if (lastFullYear) {
        ind <- lastFullYear(DateTxt, out = "logical")
    } else ind <- rep(TRUE, length(DateTxt))
   
    Date <- as.Date(DateTxt[ind])

    if (out == "short") {
        pred <- array(NA, dim = c(length(Date), ncol = nq),
                      dimnames = list(DateTxt[ind], names(object)))   
        for (i in 1:nq) {
            pred[ , i] <- predict(object[[i]],
                                  newdata = newdata,
                                  na.action = na.action)[ind]
        }
    } else {
        for (i in 1:nq) {
            p <- predict(object[[i]],
                         newdata = newdata,
                         na.action = na.action)[ind]
            if (i == 1) {
                pred <- data.frame(Date = Date, u = p, tau = object[[i]]$tau)  
            } else {
                newPred <- data.frame(Date = Date, u = p, tau = object[[i]]$tau)
                rownames(newPred) <- NULL
                pred <- rbind(pred, newPred, deparse.level = 0 )
            }
        }
        pred <- within(pred, tau <- as.factor(tau))
        class(pred) <- c("predict.rqTList", "data.frame")
    }

    attr(pred, "lastFullYear") <- lastFullYear
    
    pred
    
}

##' @export
##' @method print rqTList
##' 
print.rqTList <- function(x, ...) {
    cat("List of Quantile Regression results\n")
    cat("\no Probabilities used\n")
    print(tau(x))
    cat("\no Formula used\n")
    print(formula(x))
    cat("\no Estimated coefficients\n")
    print(round(coef(x), digits = 3))
}

## *****************************************************************************
##'
##' @title Compute an Estimate of the Tail Coefficient 'xi'
##'
##' @param object Object from which the estimation is to be done.
##' 
##' @param ... Arguments for further methods.
##'
##' @return Estimation(s) of \eqn{\xi} in a vector or data frame.
##'
##' @export
##' 
xi <- function(object, ...) {
    UseMethod("xi")
}

## *****************************************************************************
##' Estimate the tail coefficient \eqn{\xi} from quantiles.
##' 
##' Let \eqn{\tau_1 < \tau_2 < \tau_3} be the probabilities and
##' \eqn{T_1 < T_2 < T_3} be
##' the corresponding return periods \eqn{T_i = 1 / (1 - \tau_i)}. Let
##' \eqn{q_1}, \eqn{q_2}, \eqn{q_3} be the corresponding quantiles as computed
##' by quantile regression. Then we can find \eqn{\xi} by solving the equation
##' \deqn{\frac{T_3^\xi - T_1^\xi}{T_2^\xi - T_1^\xi} =
##'    \frac{q_3 - q_1}{q_2 - q_1}.}{[T_3^xi - T_1^xi] / [T_2^\xi - T_1^\xi] =
##'  [q_3 - q_1] / [q_2 - q_1].} 
##' The value of \eqn{\xi} then depends on the covariates used in the quantile
##' regression. We only consider here the case where the quantiles depend on
##' the date, with emphasis on the case where the quantile are periodic
##' functions of the date with one-year period. The the value of \eqn{\xi}
##' has also the one-year periodicity, and we can assess its variation
##' on a one-year period which can be the last full year in the period used
##' to estimate the quantiles.
##' 
##' @title Compute an Estimate of the Tail Coefficient 'xi' using
##'     Quantile Regression Results
##'
##' @param object An object with class \code{"rqTList"}.
##'
##' @param tau A vector of 3 probabilities. This vector must be in
##'     strictly increasing order and contain values that are found in
##'     \code{tau(object)}. By default the three largest values in
##'     \code{tau(object)} are used.
##'
##' @param lastFullYear Logical. It \code{TRUE} the value of the
##'     estimated \code{xi} is computed for the last full year of the
##'     data used to create \code{data}. See
##'     \code{\link{predict.rqTList}}.
##'
##' @param plot Logical. If \code{TRUE} a (gg)plot is shown.
##' 
##' @param ... Not used yet.
##' 
##' @return A data frame with a \code{xiHat} column containing the
##'     estimation.
##'
##' @section Caution: There seems to be a huge uncertainty on this
##'     estimate, and it seems that the variations are somewhat
##'     exaggerated. An alternative method is estimating \eqn{\xi} by
##'     using a moving time window in the year. Also the question
##'     arises as whether declustering should be taking into account.
##'
##' @importFrom stats uniroot
##'
##' @export
##'
##' @method xi rqTList
##' 
xi.rqTList <- function(object, tau = NULL, lastFullYear = TRUE,
                       plot = FALSE, ...)  {
    
    tauObj <- tau(object)
    if (missing(tau)) {
        tau <- tauObj[length(object) - c(2, 1, 0)]
    }
    m <- match(tau, tauObj)
    
    if (any(is.na(m))) {
        stop("the vector 'tau' given must have only values in ",
             "`tau(object)`")
    }
    
    if (length(tau) != 3 || is.unsorted(tau)) {
        stop("'tau must have length 3 and be in sorted")
    }
    
    ## return periods
    T <- 1.0 / (1 - tau)
    
    f <- function(xi, val) {
        (T[3]^xi - T[1]^xi) / (T[2]^xi - T[1]^xi) - val
    }

    ## matrix with thresholds as its columns
    U <- predict(object, lastFullYear = lastFullYear, out = "short")
    R <- (U[ , m[3]] - U[ , m[1]]) / (U[ , m[2]] - U[ , m[1]])
    
    xiHat <- rep(NA, 365)
    for (i in 1:nrow(U)) {
        resi <- try(uniroot(f = f, interval = c(-2, 2), val = R[i]))
        if (!inherits(resi, "try-error")) {
            xiHat[i] <- resi$root
        }
    }
    
    res <- data.frame(Date = as.Date(rownames(U)), R = R, xiHat = xiHat)
    
    if (plot) {
        g <- ggplot(data = res) +
            geom_line(mapping = aes_string(x = "Date", y = "xiHat")) +
            ggtitle(sprintf(paste0("Tail coefficient 'xi' estimated from ",
                                   "the three quantiles with prob. %s"),
                            paste(format(tau), collapse = ", "))) +
            xlab("") + scale_x_date(breaks = "month",
                                    labels = date_format("%m")) 

        print(g)
    }

    res
}


##' @export
##' @method summary rqTList
summary.rqTList <- function(object, ...) {
    lapply(object, summary)
}
