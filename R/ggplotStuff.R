##' Autoplot a daily meteorological series, usually a very long period
##' of time, several dozens of years.
##' 
##' @title Autoplot a daily meteorological series.
##'
##' @param object The object to be (gg)-ploted.
##'
##' @param group A character giving the grouping variable.
##'
##' @return An object inheriting from the \code{"ggplot"} class.
##'
##' @import ggplot2 
##' 
##' @export
##' @method autoplot dailyMet 
##' 
autoplot.dailyMet <- function(object,
                              group = c("decade", "year")) {

    metVar <- attr(object, "metVar")
    group <- match.arg(group)
    
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
        g <- g + ggtitle(sprintf("%s by decade", metVar))
        
    } else if (group == "year") {
        g <- g + geom_line(mapping = aes_string(x = "DateRef", y = metVar))
        ## g <- g + geom_line(data = subset(dfu, Year %in% years & JJA),
        ##                    mapping = aes(x = DateRef, y = u, group = tau, colour = tau),
        ##                   alpha = 0.9)
        g <- g + facet_wrap( ~ Year)
        g <- g + scale_x_date(breaks = "month", labels = date_format("%m"))
        g <- g + scale_colour_brewer(palette = "Set2") + xlab("") + ylab("Temp")
        g <- g + ggtitle(sprintf("%s by year", metVar))
    
    }
    
    g
}
