##' Create a \code{fevdTList} object representing a collection of
##' \code{fevd} objects that can be compared e.g. graphically.
##'
##' This class is intended to be used for models differing only by
##' their threshold.
##' 
##' \itemize{
##'    \item{ }{All the objects must have the same type and \code{"GP"} or
##'      \code{"PP"}.}
##'    \item{ }{All the objects must have the same formulas:
##'         \code{threshold.fun}, \code{location.fun}, \code{scale.fun}
##'         and \code{shape.fun}}
##' }
##' 
##' @title Create a \code{fevdTList} Object
##'
##' @param ... A collection of objects with class \code{"fevd"}
##' with the same \code{type}
##'
##' @export
##' 
##' @return An object with class \code{"fevdTList"}.
##' 
fevdTList <- function(...) {
     
    L <- list(...)
    if (!all(sapply(L, class) == "fevd")) {
        stop("all objects given in `...` must have class ",
             "\"fevd\"")
    }
    ty <- function(object) object$type
    types <- unique(sapply(L, ty))
    if (length(types) > 1 || !(types[1] %in% c("GP", "PP"))) {
        stop("all objects given in ... must have the same type ",
             "and it must be \"GP\" or \"PP\"")
    }
    
    class(L) <- c("fevList", "list")
    L
}


##' @title Coerce into a \code{fevdTList} object.
##'
##' @param object An object to coerce, typically a list object.
##'
##' @return An object with S3 class \code{"fevdTList"}
##'
##' @export
##' 
`as.fevdTList` <- function(object, names) {
    
    if (!all(sapply(object, class) == "fevd")) {
        stop("all items in 'object' must have class ",
             "\"fevd\"")
    }
    ty <- function(object) object$type
    types <- unique(sapply(object, ty))
    if (length(types) > 1 || !(types[1] %in% c("GP", "PP"))) {
        stop("all items in 'object' must have the same type ",
             "and it must be \"GP\" or \"PP\"")
    }
    
    class(object) <- c("fevList", "list")
    object
}

#'
##' @export
##' @method coef fevdTList
##' 
`coef.fevdTList` <- function(object, ...) {
    t(sapply(object, coef))
}

##' Extracts and show the coefficients of a \code{fevdTList} object
##' along with their standard deviations.
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
##' @export
##' @method coSd fevdTList
##'
##' @examples
##' u0 <- quantile(Fort$Prec, prob = c(0.95, 0.97, 0.98))
##' Fits <- list()
##' for (i in 1:3) {
##'    Fits[[i]] <- fevd(x = Prec, data = Fort, threshold = u0[i], type = "GP")
##' }
##' class(Fits) <- "fevdTList"
##' coSd(Fits)
##' 
`coSd.fevdTList` <- function(object, sOnly = FALSE, ...) {
    res <- sapply(object, coSd, sOnly)
    res <- t(res)
    if (!sOnly) res <- noquote(res)
    res
}

##' @importFrom stats vcov
##' @export
##' @method vcov fevdTList
##' 
`vcov.fevdTList` <- function(object, ...) {
    lapply(object, vcov)
}
