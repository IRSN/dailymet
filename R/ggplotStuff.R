## *****************************************************************************

##' Autoplot a daily meteorological series, usually covering a very
##' long period of time such as several dozens of years.
##' 
##' @title Autoplot a daily meteorological series.
##'
##' @param object The object to be (gg)-ploted.
##'
##' @param group A character giving the grouping variable.
##'
##' @param subset A \code{\link[base]{subset}} method of the
##'  \code{"data.frame"} class.
##'
##' @param ... Further arguments to be passed to \code{subset}.
##' 
##' @return An object inheriting from the \code{"ggplot"} class.
##'
##' @import ggplot2 
##'
##' @importFrom scales date_format
##' @export
##'
##' @method autoplot dailyMet
##'
##' @note When required, the \code{subset} method used is that of the
##'     \code{"data.frame"} class. This method is not available for
##'     the \code{"dailyMet"} class since the regular sampling
##'     condition is usually lost by subsetting.
##' 
##' @examples
##' autoplot(Rennes)
##' autoplot(Rennes, group = "year", subset = Year >= 2010)
##' autoplot(Rennes, group = "yearW", subset = Year >= 2010 & DJF)
autoplot.dailyMet <- function(object,
                              group = c("decade", "year", "yearW", "none"),
                              subset = NULL, ...) {
    
    metVar <- attr(object, "metVar")
    group <- match.arg(group)

    e <- substitute(subset)
    if (!is.null(e)) {
        r <- eval(e,  envir = object, enclos = parent.frame())
        if (!is.logical(r)) stop("'subset' must be logical")
        r <- r & !is.na(r)
        object <- object[r, , drop = FALSE]
    }
 
    g <- ggplot(data = object)
    
    if (group == "decade") {
        g <- g + geom_line(mapping = aes_string(x = "DateRef", y = metVar,
                                                group = "Year",
                                                colour = "YearDec"),
                           alpha = 1.0)
        g <- g + facet_wrap( ~ Dec)
        g <- g + scale_colour_brewer(palette = "Set3")
        g <- g + theme(panel.grid.minor.x = element_blank(),
                       panel.grid.major.x = element_blank())
        g <- g + scale_x_date(breaks = "month", labels = date_format("%m")) +
            xlab("")
        g <- g + ggtitle(sprintf("%s in %s by decade",
                                 metVar, attr(object, "station")))
        
    } else if (group %in% c("year", "yearW")) {
        
        if (length(unique(object$Year)) > 24) {
            stop("The allowed maximum of 24 distinct years is exceeded. ",
                 "Use `subset` or some other request to select a smaller ",
                 "number of years")
        }
        
        if (group == "year") {
             g <- g + geom_line(mapping = aes_string(x = "DateRef", y = metVar))
             g <- g + scale_x_date(breaks = "month", labels = date_format("%m"))
             g <- g + scale_colour_brewer(palette = "Set2") + xlab("") +
                 ylab(metVar)
             g <- g + facet_wrap( ~ Year)
             g <- g + ggtitle(sprintf("%s in %s by year",
                                 metVar, attr(object, "station")))
             
        } else if (group == "yearW") {
            g <- g + geom_line(mapping = aes_string(x = "DateRefW", y = metVar))
            g <- g + scale_x_date(breaks = "month", labels = date_format("%m"))
            g <- g + scale_colour_brewer(palette = "Set2") + xlab("") +
                ylab(metVar)
            g <- g + facet_wrap( ~ YearW)
            g <- g + ggtitle(sprintf("%s in %s by winter year",
                                     metVar, attr(object, "station")))
        }
     
    } else {
        g <- g + geom_line(mapping = aes_string(x = "Date", y = metVar),
                           alpha = 1.0)
        
        g <- g + ggtitle(sprintf("%s in %s",
                                 metVar, attr(object, "station")))
        
    }
    
    g
}


## *****************************************************************************

##' Create a new layer on an existing ggplot object showing the
##' quantile regression curves i.e. the threshold curves against the
##' date. This is useful e.g. to show the thresholds on the same
##' ggplot as the timeseries used fo fit \code{object}.
##'
##' @title Autolayer Method for the Class \code{"rqTList"}.
##'
##' @param object The \code{rqTList} object.
##'
##' @param lastFullYear Logical. If \code{TRUE} the prediction will be
##'     computed only on the last full year of the data used in the
##'     fit. See \code{\link{predict.rqTList}}.
##' 
##' @param ... Arguments passed to \code{\link{geom_line}} such as
##' \code{linetype}, \code{size}.
##' 
##' @export
##' 
##' @method autolayer rqTList
##'
##' @seealso \code{\link{autoplot.rqTList}}.
##' 
autolayer.rqTList <- function(object, lastFullYear = TRUE, ...) {

    p <- predict(object, out = "long", lastFullYear = lastFullYear)
    geom_line(data = p,
              mapping = aes_string(x = "Date", y = "u",
                                   group = "tau", colour = "tau"),
              ...) 
   
}

## *****************************************************************************

##' Create an object inheriting from the \code{"ggplot"} class showing
##' the quantile regression curves (i.e. the threshold curves) against
##' the date.
##'
##' @title Autoplot Method for the Class \code{"rqTList"}.
##'
##' @param object The \code{rqTList} object.
##'
##' @param lastFullYear Logical. If \code{TRUE} the prediction will be
##'     computed only on the last full year of the data used in the
##'     fit. See \code{\link{predict.rqTList}}.
##' 
##' @param ... Arguments passed to \code{\link{geom_line}} such as
##'     \code{linetype}, \code{size}.
##' 
##' @export
##'
##' @method autoplot rqTList
##' 
##' @examples
##' \dontrun{ 
##'     RqRennes <- rqTList(dailyMet = Rennes)
##'     g <- autoplot(RqRennes) 
##'     stat <- findStationMF("bordeaux-me")
##'     ## you may here have to use `Sys.setenv(metData = xxx)`
##'     Bordeaux <- readMet(stat)
##'     RqBordeaux <- rqTList(dailyMet = Bordeaux)
##'     g <- g + autolayer(RqBordeaux, linetype = "dashed", size = 1.2) +
##'          ggtitle(paste0("Quantile regression for Rennes (solid thin line) ",
##'                         "and Bordeaux (dashed thick line)"))
##'     g
##' }
autoplot.rqTList <- function(object, lastFullYear = TRUE, ...) {
   
    g <- ggplot()
    p <- predict(object, out = "long", lastFullYear = lastFullYear)
    g <- g + geom_line(data = p,
                       mapping = aes_string(x = "Date", y = "u",
                                            group = "tau", colour = "tau"),
                       ...) +
        scale_colour_brewer(palette = "Set2") + xlab("") 
    if (lastFullYear) {
        g <- g + scale_x_date(breaks = "month", labels = date_format("%m")) +
            ggtitle(sprintf("Quantile regression for %s in %s Year = %s",
                            attr(object, "metVar"),
                            attr(object, "station"),
                            format(p[1, "Date"], "%Y")))
    } else {
        g <- g + ggtitle(sprintf("Quantile regression for %s in %s",
                                 attr(object, "metVar"),
                                 attr(object, "station")))
    }
    
    g
    
}


## *****************************************************************************

##' @title Autoplots a \code{phasesMatrix} Object
##'
##' @param object An object with class \code{"phasesMatrix"}.
##' 
##' @param tauRef Value of the probability \code{tau} to be used as
##'     reference in the comparison.
##'
##' @param ampl Logical. If \code{TRUE} the phases are shown through
##'     the sine waves weighted by their amplitude \eqn{\gamma_k}. If
##'     \code{FALSE} the sine waves are not weighted. The later option
##'     can be used to better asses the variation of phase for the
##'     higher harmonics since these usually have smaller amplitudes.
##'
##' @param ... Further arguments to be passed to \code{geom_line}.
##'
##' @return An object inheriting from \code{"ggplot"}.
##'
##' @method autoplot phasesMatrix
##'
##' @export
##' 
autoplot.phasesMatrix <- function(object,
                                  tauRef = 0.95,
                                  ampl = TRUE, ...) {
    tt <- 1:365
    K <- attr(object, "degree")
    Gamma <- attr(object, "amplitude")
    Phi <- unclass(object)

    Tau <- as.numeric(gsub("tau=", "", rownames(object)))
    indRef <- seq_along(Tau)[Tau == tauRef]
    if (!length(indRef)) {
        indRef <- which.min(abs(tauRef - Tau))
        tauRef <- Tau(indRef)
        warning("'tauRef' not found in 'object'. The closest",
                "value used instead")
    }
    
    tg <- taug <- ordg <- fung <- fun1g <- numeric(0)

    for (j in 1:K) {
        omegaj <- 2 * pi* j / 365.25
        for (i in 1:nrow(object)) {
            tg <- c(tg, tt)
            taug <- c(taug, rep(Tau[i], length(tt)))
            ordg <- c(ordg, rep(j, length(tt)))
            if (ampl) {
                fung <- c(fung, Gamma[i, j] *
                                sin(omegaj * (tt - Phi[i, j])))
                fun1g <- c(fun1g, Gamma[indRef, j] *
                                  sin(omegaj * (tt - Phi[indRef, j])))
            } else {
                fung <- c(fung, sin(omegaj * (tt - Phi[i, j])))
                fun1g <- c(fun1g, sin(omegaj * (tt - Phi[indRef, j])))
            }
        }
             
    }
    
    df <- data.frame(date = as.Date("1970-01-01") + tt,
                     t = tg, tau = taug, ord = ordg, fun = fung, fun1 = fun1g)
    
    df <- within(df, { tau <- as.factor(tau); ord <- as.factor(ord) })
    
    gPhStab <- ggplot(data = df)
    gPhStab <- gPhStab +
        geom_line(mapping = aes_string(x = "date", y = "fun",
                                       group = "ord", colour = "tau"),
                  ...)
    gPhStab <- gPhStab +
        geom_line(mapping = aes_string(x = "date", y = "fun1"),
                  linetype = "dashed")
    
    gPhStab <- gPhStab + facet_grid(tau ~ ord, labeller = label_both) +
        ggtitle(paste("Seasonal phases in quantile",
                      "regression. Ref. (dashed line) tau =",
                      format(tauRef)))
    gPhStab <- gPhStab + scale_x_date(date_labels = "%b") +
        theme(legend.position = "bottom")
    gPhStab <- gPhStab + xlab("") + ylab("sine waves")
    gPhStab

}
