## *****************************************************************************
##' Extracts and show the coefficients of a \code{rq} object along
##' with their standard deviations.
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
##' @export
##' @method coSd rq
##'
##'  
coSd.rq <- function(object, sOnly = FALSE, ...) {
    
    s <- summary(object)
    co <- s$coefficients
    if (sOnly) return(co[ , 2])
    text <- apply(s$coefficients[ , 1:2], MARGIN = 1,
                  FUN = function(x) sprintf("%6.3f [%5.3f]", x[1], x[2]))
    names(text) <- names(object$coefficients)
    noquote(text)
}
