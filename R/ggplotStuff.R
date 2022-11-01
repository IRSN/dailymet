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
##' @export
##' @method autoplot dailyMet
##'
##' @note When required, the \code{subset} method used is that of the
##'     \code{"data.frame"} class. This method is not available for
##'     the \code{"dailyMet"} class since the regular sampling
##'     condition is usually lost by subsetting.
##' 
##' @examples
##' autoplot(Rennes)
##' autoplot(Rennes, group = "year", subset = year >= 2010)
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
              mapping = aes(x = Date, y = u, group = tau, colour = tau),
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
                       mapping = aes(x = Date, y = u, group = tau, colour = tau),
                       ...) +
        scale_colour_brewer(palette = "Set2") +
        xlab("") + ggtitle(sprintf("Quantile regression for %s in %s",
                                   attr(object, "metVar"),
                                   attr(object, "station")))
    g
    
}

