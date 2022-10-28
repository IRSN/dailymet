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
                              group = c("decade", "year", "yearW"),
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
        
    } else {
        
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
        
        g
    
    }
    
    g
}
