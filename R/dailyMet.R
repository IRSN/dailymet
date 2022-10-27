##' Create an object with class \code{"dailyMet"} from a data frame.
##'
##' The \code{"dailyMet"} class essentially a daily time series with
##' precomputed time variables such as the Julian day, ...
##' \itemize{
##'    \item{\bold{Regular sampling} }{
##'       The rows must correspond to a sequence of successive days.
##'       So missing values will generally be present.
##'     }
##' 
##'     \item{\bold{Precomputed variables} }{ These variables are
##'         functions of the date that are repeatedly used, hence
##'         which can be computed only once. They include for instance
##'         the Julian day.}
##' 
##' }
##' 
##' @title Create an Object with S3 Class \code{"dailyMet"}  
##'
##' @param data An object than can be coerced into a data frame with
##'     at least: a column representing the date and a column giving
##'     the meteorological variable.
##'
##' @param dateVar Name of the date variable in \code{data}. This
##'     variable should be coercible into a vector with class
##'     \code{"Date"} by using \code{as.Date}. If no value is given
##'     but only a variable with class \code{"Date"} exists, then this
##'     variable is used and renamed into \code{"Date"}.
##'
##' @param metVar Name of the meteorological variable in `data`.
##'
##' @param idVar List defining of variables that can be found of
##'     created from those in the data.frame, and that will be kept
##'     under the prescribed name. See \bold{Examples}. \bold{NOT
##'     IMPLEMENTED}.
##' 
##' @param ... Not used yet
##'
##' @return An object with S3 class \code{"dailyMet"} inheriting from
##'     the \code{"data.frame"} class.
##'
##' @export
##' 
dailyMet <- function(data,
                     dateVar = "Date", 
                     metVar = "TX",
                     id = list("Code" = NULL),
                     trace = 0, ...) {
    
    d <- as.data.frame(data)

    if (FALSE && missing(dateVar)) {
        m <- (1:ncol(data))[sapply(data, is, class2 = "Date")]
        if (length(m) == 0) {
            stop("No variable with class \"Date\" found. ",
                 "Give `dateVar` for a coercion")
        } else if (length(m) > 1) {
            stop("several variables with class \"Date\" found. ",
                 "Give `dateVar` to choose.")
        }
    } 
    if (is.na(m <- match(dateVar, names(d)))) {
        stop("'data' does not have the wanted 'dateVar' column") 
    }
    names(d)[m] <- "Date"
    
    Date <- as.Date(d[["Date"]])
    nr <- nrow(d)
    dataAll <- data.frame(Date =  seq(from = Date[1], to = Date[nr], by = "day"))
    
    d <- merge(d, dataAll, by = "Date", all.y = TRUE)
    
    nni <- nonNaIntervals(d[[metVar]])
    periods <- data.frame(Start = d$Date[nni$Start],  End = d$Date[nni$End])
    Duration <- as.numeric(periods$End - periods$Star) / 365.25
    periods <- data.frame(periods, Duration = round(Duration, digits = 2))
    
    
    if (trace) {
        cat("Non-NA intervals\n")
        print(periods)
    }

    ## =========================================================================
    ## warn if there are missing dates, compute the available periods
    ## =========================================================================

    attr(d, "metVar") <- metVar
    attr(d, "periods") <- periods
    
    class(d) <- c("dailyMet", "data.frame")

    d
    
}

##' Summary method
##' 
##' @title Summary Method
##' @param object An object to summarize.
##' @param ... Not used.
##' @return A summary object which is printed.
##'
##' @method summary dailyMet
##' @export
##' 
##' 
summary.dailyMet <- function(object, ...) {
    
    class(object) <- "summary.dailyMet"
    object
}

##' @title Print 
##' @param x An object with class \code{"summary.dailyMet"}
##' @param ... Not used
##' @return Nothing.
##' @method print summary.dailyMet
##' @export
##' @noRd
print.summary.dailyMet <- function(x, ...) {
    cat("o Daily Meteorological Time Series\n")
    cat("\n   o Variable name: \"", attr(x, "metVar"), "\"\n")
    cat("\n   o Non-missing periods\n")
    print(attr(x, "periods"))
    cat("\n   o Summary for variable ", attr(x, "metVar"), "\n")
    print(summary(y <- x[[attr(x, "metVar")]]))
    o <- order(y)
    ind <- o[1:5]
    minInfo <- paste(sprintf("%6.1f", y[ind]), " [", x$Date[ind], "]", sep = "") 
    ind <- o[length(o) - 1:5]
    maxInfo <- paste(sprintf("%6.1f", y[ind]), " [", x$Date[ind], "]", sep = "") 
    cat("\n   o Five smallest/largest obs\n")
    print(noquote(cbind(min = minInfo, max = maxInfo)))
}
