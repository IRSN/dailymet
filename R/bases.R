## *****************************************************************************

##' Check whether the names of trigonometric terms are used in a
##' character vector or formula. A name is a trigonometric term if it
##' is obtained by pasting \code{"cosj"} or \code{"sinj"} with an
##' integer giving the harmonic. We expect that when a trigonometric
##' \code{"cosj"} or \code{"sinj"} is found, it companion
##' \code{"sinj"} or \code{"cosj"} is also found.
##'
##' @title Check a Vector of Names for Trigonometric Components
##' 
##' @param names A character vector containing names of variables,
##'     possibly embeding trigonometric variables. Can also be a
##'     formula from which the names are extracted.
##'
##' @return The positive integer \code{K} such that the names
##'     \code{"cosj1"}, \code{"sinj1"}, ..., \code{"cosjK"},
##'     \code{"sinjK"} are found in \code{name}. If no trigonometric
##'     term is found then the value \code{0} is returned.
##'
##' @section Caution: This fonction is likely to be come an internal
##'     (non-exported) function. Mind the \code{'j'} in \code{"cosj"} os
##'     \code{"sinj"}.
##' 
##' @export
##' 
##' @examples
##' nm <- c("Cst", "sinj1", "cosj1", "sinj2", "cosj2", "sinj3", "cosj3")
##' checkTrigNames(nm)
##' ## error: no 'sin' companion
##' try(checkTrigNames(c("Cst", "cosj1")))
##' ## Missing 'j': no trigonometric term
##' checkTrigNames(c("Cst", "cos1"))
##' checkTrigNames(c("Cst", "Prec"))
##' 
checkTrigNames <- function(names) {

    if (inherits(names, "formula")) {
        names <- attr(terms(names), "term.labels") 
    }
    
    cosInd <- grep("cosj[1-9]*", names)
    hasCos <- length(cosInd)
    if (hasCos) {
        cosHarm <- sort(as.integer(gsub("cosj", "", names[cosInd])))
        if (any(is.na(cosHarm))) stop("bad 'cos' term name")
    } 
    
    sinInd <- grep("sinj[1-9]*", names)
    hasSin <- length(sinInd)
    if (hasSin) {
        sinHarm <- sort(as.integer(gsub("sinj", "", names[sinInd])))
        if (any(is.na(sinHarm))) stop("bad 'sin' term name")
    }

    if (hasCos != hasSin) {
        stop("Expect to find cos AND sin names or none of these")
    }
    
    if (!hasCos) return(0)

    ## now 'hasCos' is TRUE ...
    if (!hasSin || !all.equal(cosHarm, sinHarm)) {
        stop("mismatch between 'cos' and 'sin' harmonics")
    }
    K <- max(sinHarm)
    if (!all.equal(sinHarm, 1:K)) {
        stop("The vector of harmonics must have the form 1:K")
    }
    K
}

## *****************************************************************************
##' Find the phases \eqn{\phi_k} and the amplitudes \eqn{\gamma_k}
##' for \eqn{k = 1}, ..., \eqn{K} such that
##' \deqn{ 
##'  \alpha_ 0 + \sum_{k=1}^K \alpha_k \cos\{\omega_k t\} +
##'    \beta_k \sin\{\omega_k t \}
##'  = \textrm{Cst} + \sum_{k=1}^K \gamma_k \sin\{ \omega_k [t - \phi_k] \}
##' }
##' where \eqn{\omega_k = 2 \pi k /365.25} and where the coefficients
##' \eqn{\alpha_k} and \eqn{\beta_k} are given in the \code{object}
##' vector.
##'  
##' @title Phases of Sine Waves from the Trigonometric Coefficients
##'
##' @param object A numeric vector or matrix having suitable names
##'     related to the trigonometric basis \code{\link{tsDesign}}, or
##'     a numeric matrix having suitable colnames. This object will
##'     most often be given by applying the \code{coef} method for the
##'     \code{"rq"} or the \code{"rqTList"} class, see
##'     \bold{Examples}.
##' 
##' @return An object with S3 class \code{"phasesMatrix"} inheriting
##'     from \code{"matrix"}. This matrix contains the phases
##'     \eqn{\phi_k} as its rows. This matrix has as attribute
##'     \code{"amplitude"} another numeric matrix with \eqn{K} rows,
##'     containing the amplitudes \eqn{\gamma_k} as its rows.  Some
##'     methods are available for the class \code{"phasesMatrix"} such
##'     as \code{print} and \code{autoplot}.
##'
##' @section Caution: When a vector is given in \code{object}, it must
##'     be \emph{named} with suitable element names in order to allow a
##'     reliable extraction of the coefficients \eqn{\alpha_k} and
##'     \eqn{\beta_k}. These correspond to the names
##'     \itemize{
##'        \item{"cosj1", "cosj2", ... }{coefficients for the cosine terms
##'            \eqn{\alpha_1}, \eqn{\alpha_2}, ..., \eqn{\alpha_K}}
##'        \item{"sinj1", "sinj2", ... }{coefficients for the sine terms
##'            \eqn{\beta_1}, \eqn{\beta_2}, ..., \eqn{\beta_K}}
##'     }
##'     Some other named elements can be present e.g. for the constant
##'     or for trend terms: They will be ignored. Similarly when a
##'     numeric matrix of coefficients is given the colnames must be
##'     as before. The rownames will be re-used as rowanmes for the
##'     result.
##' 
##' @export
##'
##' @seealso \code{\link{autoplot.phasesMatrix}}
##' 
##' @examples
##' Rq <- rqTList(dailyMet = Rennes)
##' co <- coef(Rq)
##' phases(co)
##' ## for a vector
##' phases(co[1 , ])
##' ## change the order: the result is the same
##' phases(co[1, sample(1:7, size = 7)])
##' autoplot(phases(co))
##' 
phases <- function(object) {
    
    if (!is.numeric(object)) stop("'object' must be numeric")
    
    ## Note that we corerce a vector into a matrix and then
    ## coerce the matrix result into a vector... Yet we will
    ## most often use a matrix of coefficients.
    if (!is.null(d <- dim(object))) {
        if (length(d) != 2) {
            stop("'object' must be a numeric vector or matrix")
        }
        vec <- FALSE
        K <- try(checkTrigNames(colnames(object)))
        nco <- d[1]
    } else {
        vec <- TRUE
        K <- try(checkTrigNames(names(object)))
        nco <- 1L
        object <- array(object, dim = c(1L, length(object)),
                        dimnames = list(NULL, names(object)))
    }
    if (inherits(K, "try-error")) {
        stop("'object' must be a numeric vector/matrix ",
             "with suitable names/colnames")
    }
    gamma <- phi <- array(0.0, dim = c(nco, K))
    for (j in 1:K) {
        omegaj <- 2 * pi * j / 365.25 
        alpha <- object[ , paste0("cosj", j)]
        beta <- object[ , paste0("sinj", j)]
        gamma[ , j] <- sqrt(alpha^2 + beta^2)
        phi[ , j] <- - asin(alpha / gamma[ , j])
        ind <- (beta <= 0)
        phi[ind, j] <- pi - phi[ind, j]
        phi[ , j] <- phi[ , j] / omegaj
    }
    colnames(phi) <- colnames(gamma) <- paste0("sinjPhi", 1:K)
    ## if (vec) {
    ##    phi <- drop(phi)
    ##    gamma <- drop(gamma)
    ## } else {
    rownames(phi) <- rownames(gamma) <- rownames(object)
    class(phi) <- c("phasesMatrix", "matrix")
    ##}
    attr(phi, "degree") <- K
    attr(phi, "amplitude") <- gamma
    phi
}

## no longer used, except for tests...
## 
sinPhasesOld <- function(object)  {
    df <- length(co <- object)
    if (!(df %% 2)) {
        stop("'df' must be an odd integer")
    }
    K <- (df - 1) / 2
    nm <- "Cst"
    nmSin <- character(0)
    for(j in 1:K) {
        nm <- c(nm, paste0(c("cosj", "sinj"), j))
        nmSin <- c(nmSin, paste0("sinjPhi", j))
    }
    if (length(setdiff(names(object), nm))) {
        stop("Up to the order 'names(object)' should be ",
             paste0("c(", paste(paste0("\'", nm, "\'"),
                                collapse = ", "), ")"))
    }
    gamma <- phi <- rep(0.0, K)
    for (j in 1:K) {
        omegaj <- 2 * pi * j / 365.25 
        ind <- paste0(c("cosj", "sinj"), j)
        alphaBeta <- co[ind]
        gamma[j] <- sqrt(alphaBeta[1]^2 + alphaBeta[2]^2)
        phi[j] <- - asin(alphaBeta[1] / gamma[j]) 
        if (alphaBeta[2] <= 0) {
            phi[j] <- pi - phi[j]
        }
        phi[j] <- phi[j] / omegaj
        ## phi[j] <-  atan(alphaBeta[1] / alphaBeta[2]) / omegaj
    }
    names(phi) <- names(gamma) <- paste0("sinjPhi", 1:K)
    attr(phi, "amplitude") <- gamma
    phi
}


## *****************************************************************************

##' @title Print a \code{phasesMatrix} Object
##'
##' @param x The \code{phasesMatrix} object.
##'
##' @param digits Number of digits to show.
##'
##' @param ... Not used.
##' 
##' @return Nothing.
##'
##' @export
##'  
##' @method print phasesMatrix
##' 
print.phasesMatrix <- function(x, digits = 3, ...) {

    mat1 <- x
    mat2 <- attr(x, "amplitude")
    nms <- colnames(mat1)
    res <- cbind(mat1, mat2)
    gsub("sinj", "", nms)
    colnames(res) <- c(gsub("sinjP", "p", nms),
                       gsub("sinjPhi", "gamma", nms))
    print(res, digits = digits)
    
}


## ****************************************************************************

##' Create designs for time series regression (linear regression or
##' quantile regression) for meteorological time series.
##'
##' The choice is for now between polynomial functions that can be
##' used to describe a trend) and trigonometric functions with period
##' one year (365.25 days) that can be used for the seasonality.
##'
##' \itemize{
##'  
##'     \item{"polynom" }{
##'
##'         Can be used to describe a polynomial with degree
##'         \code{<= df - 1}.  The \code{df} basis functions are named
##'         \code{"Cst"}, then: \code{"t1"}, \code{"t2"}, ...
##' 
##'     }
##'
##'     \item{"trigo" }{
##' 
##'          The basis contains \code{df = 2 * K + 1} trigonometric
##'          functions given in the order of increasing
##'          harmonics. These are: the constant function \code{"Cst"}
##'          and then the \code{K} couples of one cosine function and
##'          one sine function, with names \code{"cosj1"},
##'          \code{"sinj1"}, \code{"cosj2"}, \code{"sinj2"}, ...
##'
##'     }
##'
##'     \item{"sinwave" }{
##'
##'          With \code{K = df \%/\% 2}, the \code{K + 1} basis
##'          functions are the constant \code{"Cst"}, then \code{K}
##'          sine wave functions in the order of increasing harmonics
##'          with names \code{"sinjPhi1"}, \code{"sinjPhi2"}, ...
##'
##'     } 
##' }
##'
##' @title Designs For Time Series Regression
##' 
##' @param dt A \code{POSIXct} or \code{Date} object.
##' 
##' @param type Type of design.
##'
##' @param df Number of basis functions, or "degree of freedom".
##'
##' @param period Not used. Can only be one year and is used only in
##'     the trigonometric case.
##'
##' @param phi Numeric vector of phases used only when \code{type} is
##'     \code{"sinwave"}.
##'
##' @param keepTrig Logical. Used only when \code{type} is
##'     \code{"sinwave"}. If \code{TRUE} the base trigonometric
##'     functions \code{sinj1}, \code{sinj2}, ..., \code{cosj1},
##'     \code{cosj2}, ... are kept in the result, along with the
##'     functions \code{sinjPhi1}, \code{sinjPhi2}, ...
##' 
##' @param origin An origin for the time. Used only in the polynomial
##'     case. The same value must be used in fits and predictions.
##'
##' @return A list with an element named \code{X} containing the
##'     matrix.
##'
##' @note For both designs \code{"trigo"} and \code{"sinwave"},
##'     \code{df} must be taken as an \emph{odd} integer, in order to
##'     allow the use of an arbitrary phase for each of the harmonics.
##'     Mind that the corresponding bases are (nearly) orthogonal only
##'     when full years are used.
##' 
##' @export
##'
##'
tsDesign <- function(dt,
                     type = c("polynom", "trigo", "sinwave"),
                     df = 3,
                     period = "year",
                     phi = NULL,
                     keepTrig = FALSE,
                     origin = NULL) {
    
    ## dtNum <- as.numeric(diff(head(dt)), units = "days")

    mc <- match.call()
    
    dt <- as.POSIXct(dt, tz = "GMT")
    type <- match.arg(type)
    
    if (type == "polynom") {
        if (is.null(origin)) origin <- mean(dt)
        t <- as.numeric(difftime(dt, origin, units = "days")) / 365.25
        e <- 0:(df - 1)
        X <- outer(t, e, FUN = "^")
        colnames(X) <- c("Cst", paste("t", 1:(df - 1), sep = ""))
        rownames(X) <- format(dt , "%Y-%m-%d")
    } else if (type == "trigo") {

        if (!(df %% 2)) stop("'df' must be an odd integer")
        origin <- as.POSIXct(NA)
        K <- floor(df / 2)
        J <- as.numeric(format(dt, "%j"))
        nJ <- length(J)
        tt <- outer(2 * pi * J / 365.25, 1:K, FUN = "*")
        X <- cbind(cos(tt), sin(tt))
        colnames(X) <- c(paste("cosj", 1:K, sep = ""),
                         paste("sinj", 1:K, sep = ""))
        rownames(X) <- format(dt , "%Y-%m-%d")
        ## put the columns in order cos, sin, cos, sin, ... with
        ## increasing harmonics
        ind <- rep(1:K, each = 2)
        ev <- seq(2, 2 * K, by = 2)
        ind[ev] <- ind[ev] + K
        X <- X[ , ind, drop = FALSE]
        ## add a constant column
        X <- cbind("Cst" = rep(1, nJ), X)
    } else if (type == "sinwave") {
        if (!(df %% 2)) stop("'df' must be an odd integer")
        origin <- as.POSIXct(NA)
        K <- floor(df / 2)
        if ((length(phi) != K) || !is.numeric(phi)) {
            stop("'phi' must be a numeric vector with length ",
                 "(df - 1) / 2 = ", K)
        }
        X <- sinBasis(dt = dt, df = df, phi, keepTrig = FALSE) 
    }

    L <- list(call = mc,
              origin = origin,
              X = X)
    
    class(L) <- c("tDesign", "list")
    
    L
    
}

## *****************************************************************************
##' Basis of sine wave functions \eqn{s_k(t)} with the phases
##' \eqn{\phi_k} given in \code{phi} corresponding to the periods
##' \eqn{365.25 / k}.
##'
##' For a time \eqn{t} the value of the \eqn{k}-th function is given
##' by
##' 
##'  \deqn{s_k(t) := \sin\{2 \pi k [j_t - \phi_k] / 365.25)}{
##'    s_k(t) := sin(2 * pi * k * [j_t - \phi_k] / 365.25)}
##'
##' for \eqn{k=1} to {K}, where \eqn{j_t} is the Julian day
##' corresponding to \eqn{t}. This function is useful to provide a
##' basis of functions with yearly seasonality that are suitable for
##' meteorological variables. For instance the phase for the first
##' harmonic of the daily temperature in France is such that the
##' annual maximal temperature "normally" happen at the end of July.
##' 
##' @title Create a Basis of Sine Waves with Given Phases
##'
##' @param dt A vector with class \code{"Date"} or \code{"POSIXct"}
##'     representing the days at which the sine waves basis functions
##'     are to be evaluated.
##' 
##' @param df Odd number as in \code{\link{tsDesign}}.
##'
##' @param phi Vector of \eqn{K} phases where \eqn{K} is equal to
##'     \code{(df - 1) / 2}.
##'
##' @param cst Logical. If \code{TRUE} a constant column with value
##'     1.0 is added as a first column.
##' 
##' @param keepTrig Logical. If \code{TRUE}, the cos-sin functions are
##'     joined as columns of the result. So the returned matrix then
##'     does not correspond to a basis. Used mainly for tests.
##' 
##' @return A numeric matrix containing as columns the \eqn{K} basis
##'     functions \eqn{s_k(t)}, and with its rows corresponding to the
##'     time \eqn{t} given by \code{Date}. The names of the \eqn{K}
##'     sine wave function have the prefix \code{"sinjPhi"} so are
##'     \code{"sinjPhi1"}, \code{"sinPhij2"}, ... This is aimed to
##'     recall that the phases \eqn{\phi_k} were given. If \code{cst}
##'     is \code{TRUE} and \code{keepTrig} a constant column with
##'     value 1.0 is added, and if \code{keepTrig} is \code{TRUE} the
##'     trigonometric functions (with phase zero) are added as well.
##'
##' @note This functions uses the \code{\link{tsDesign}} function with
##'     \code{type = "trigo"}.
##'
##' @section Caution: This function is likely to be removed. It is
##'     safer to use \code{\link{tsDesign}} with the argument
##'     \code{phi}. 
##'
##' @export
##' 
##' @examples
##' ## Use only full years to maintain near orthogonality
##' df <- subset(Rennes, Year <= 2020 & Year >= 1960)
##' K <- 3
##' ## Design of trigonometric functions
##' desTrig <- tsDesign(dt = df$Date, df = 2 * K + 1, type = "trigo")
##' fit <- lm(formula = TX ~ Cst + cosj1 + sinj1 + cosj2 + sinj2 + cosj3 + sinj3 - 1,
##'           data = data.frame(df, desTrig$X))
##' betaHat <- coef(fit)
##'
##' ## find the phases
##' phiHat <- phases(betaHat)
##' gamma <- attr(phiHat, "amplitude")
##'
##' ## Design of sine waves with prescribed phases
##' desSin <- tsDesign(dt = df$Date, df = 2 * K + 1,
##'                    type = "sinwave", phi = phiHat)
##' fit2 <- lm(formula = TX ~ Cst + sinjPhi1 + sinjPhi2 + sinjPhi3 -1,
##'            data = data.frame(df, desSin$X))
##' ## should be zero
##' max(abs(predict(fit) - predict(fit2)))
##'
##' ## the amplitude and the coef
##' rbind(coef(fit2), c(NA, gamma))
##' j <- 3
##' indTrig <- paste0(c("cosj", "sinj"), j)
##' indSin <- paste0("sinjPhi", j)
##' plot(desTrig$X[1:366 , indTrig] %*% betaHat[indTrig], type = "l",
##'      col = "SpringGreen3", xlab = "", ylab = paste("harmonic", j))
##' lines(desSin$X[1:366, indSin] * gamma[j], col = "red", lwd = 2, lty = "dashed")
##' 
sinBasis <- function(dt, df = 7, phi, cst = TRUE, keepTrig = FALSE) {
    if (!(df %% 2)) {
        stop("'df' must be an odd integer")
    }
    K <- (df - 1) / 2
    if (length(phi) != K) {
        stop("'phi' must be of length (df - 1) / 2")
    }
    
    trigDesign <- tsDesign(dt = dt, df = df, type = "trigo")
    sinPhi <- array(NA, dim = c(nrow(trigDesign$X), K),
                    dimnames = list(rownames(trigDesign$X),
                                paste0("sinjPhi", 1:K)))

    ## Use the addition of angles
    ##
    ## sin(angle - phi) = sin(angle) * cos(phi) - cos(angle) * sin(phi)
    ## 
    for(j in 1:K) {
        omegaj <- 2 * pi * j / 365.25 
        nm <- paste0(c("cosj", "sinj"), j)
        angle <- omegaj * phi[j]
        sinPhi[ , j] <- trigDesign$X[ , nm] %*% c(-sin(angle), cos(angle))
    }
    
    if (keepTrig) {
        res <- cbind(trigDesign$X, sinPhi)
    } else {
        res <- sinPhi
        if (cst) {
            res <- cbind(Cst = 1, res)
        }
    }
    
    res   
}
