## *****************************************************************************
##' Extracts and show the coefficients of a \code{fevd} object along
##' with their standard deviations.
##' 
##' @title Coefficients and Standard Errors
##' 
##' @param object An object with class \code{"mlePP"} from the
##'     \pkg{NHPoisson} package.
##'
##' @param sOnly Logical. If \code{TRUE} only the standard deviation
##'     (a.k.a standard errors) wille be show.
##'
##' @param ... Not used.
##' 
##' @export
##' @method coSd mlePP
##'  
coSd.mlePP <- function(object, sOnly = FALSE, ...) {
    hat <- object@coef
    if (is.null(C <- object@vcov)) {
        s <- rep(NA, length(hat))
    } else {
        s <- sqrt(diag(C))
    }
    if (sOnly) return(s)
    
    text <- apply(cbind(hat, s), MARGIN = 1,
                  FUN = function(x) sprintf("%6.3f [%5.3f]", x[1], x[2]))
    names(text) <- names(hat)
    noquote(text)
}



modelMatrices.mlePP <- function(object, ...) {


}
