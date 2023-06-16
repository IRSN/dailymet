##*****************************************************************************
##' Generalized Residuals for a \code{TVGEV} model.
##' 
##' @title Generalized Residuals for a \code{TVGEV} Model
##'
##' @aliases resid.pgpTList
##' 
##' @param object A \code{TVGEV} object.
##'
##' @param type The approximate distribution wanted. The choices
##'     \code{c("unif", "exp")} correspond to the standard uniform,
##'     the standard exponential and the standard Gumbel
##'     distributions. Partial matching is allowed.
##'
##' @param ... Not used yet.
##' 
##' @return A vector of generalized residuals which should
##'     \emph{approximately} be independent and \emph{approximately}
##'     follow the standard exponential or the uniform distribution,
##'     depending on the value of \code{type}.
##'
##' @references Cox, D.R. and Snell E.J. (1968) "A General Definition
##' of Residuals".  \emph{JRSS Ser. B}, \bold{30}(2), pp. 248-275.
##' 
##' @importFrom stats resid
##' @method residuals pgpTList
##' @export
##'
##' @examples
##' if (require("NSGEV")) {
##' ## build thresholds then fit
##' Rq <- rqTList(dailyMet = Rennes, tau = c(0.94, 0.95, 0.96, 0.97, 0.98, 0.99))
##' Pgp <- pgpTList(dailyMet = Rennes, thresholds = Rq, declust = TRUE,
##'                 extraDesign  = list(splines = list("what" = "NSGEV::breaksX",
##'                                                    "args" = list(breaks = "1980-01-01"))),
##'                 scale.fun = ~Cst + sinjPhi1 + sinjPhi2 + sinjPhi3 + t1_1980 - 1,
##'                 fitLambda = TRUE, logLambda.fun = ~ t1_1980 - 1)
##'
##' res <- resid(Pgp)
##' autoplot(res)
##' autoplot(res, seas = TRUE)
##' }
residuals.pgpTList <- function(object,
                           type = c("exp", "unif"),
                           ...) {
    
    type <- match.arg(type)
    pred <- predict(object)
    pred <- subset(pred, TX > u)
    
    if (type == "exp") {
        pred <- within(pred,
                       resid <- -log(1 - nieve::pGPD2(TX - u, scale = sigma, shape = xiStar)))
    } else if (type == "unif") {
        pred <- within(pred,
                       resid <- nieve::pGPD2(TX - u, scale = sigma, shape = xiStar))
    }
    
    class(pred) <- c("resid.pgpTList", "data.frame")
    pred
    
}

##' @method autoplot resid.pgpTList
##' @export
autoplot.resid.pgpTList <- function(object, seas = FALSE, ...) {

    g <- ggplot(data = object)
    if (!seas) {
        g <- g + geom_point(mapping = aes(x = Date, y = resid),
                            colour = "SpringGreen4", alpha = 0.7, size = 0.8)
        g <- g + ggtitle("Generalized residuals against date")
        
    } else {
        g <- g + geom_point(mapping = aes(x = DateRef, y = resid),
                            colour = "SpringGreen4", alpha = 0.5, size = 0.8)
        g <- g + scale_x_date(breaks = "month", labels = date_format("%m"))
        g <- g + ggtitle("Generalized residuals against day in year")

    }
        
    g <- g + facet_wrap(tau ~ ., labeller = label_both) + xlab("day in year")
    g
    
}
