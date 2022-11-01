## *****************************************************************************
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
##' The precomputed variables include the following ones.
##' 
##' \itemize{
##'
##'     \item{\code{Day}, \code{DateRef} }{
##'
##'          Day in the year a.k.a. Julian day, and reference Date.
##'          \code{Day} takes integer value between 1 and
##'          366. \code{DateRef} is a refrence date, corresponding to
##'          the day but in a non leap year taken as reference.  This
##'          date allows to plot the series against the day in the
##'           year still using months on the x axis.
##'     }
##' 
##'     \item{\code{DayW}, \code{DateRefW} }{
##'
##'          Similar to \code{Day} and \code{DateRef}, but for
##'          "winter years" begining on the 1-st of August and ending
##'          on the 31-th of July. Winter years are useful when the
##'          interest is on event that usually happen in winter
##'          (extreme sea levels, extreme snowfall, ...).
##' 
##'     }
##' 
##'     \item{JJA, DJF }{
##'
##'          Logical variables indicating the periods June-July-August 
##'          and December-January-Febuary which are sometimes used in the
##'          study of summer and winter extremes.
##' 
##'     }
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
##' @param idVar List defining of variables that can be found or
##'     created from those in the data.frame, and that will be kept
##'     under the prescribed name. \bold{NOT IMPLEMENTED}.
##'
##' @param station,id Character vectors with length one giving the
##'     name of the gauging station and the identifier. These will be
##'     attached to the data.
##' 
##' @param ... Not used yet
##'
##' @return An object with S3 class \code{"dailyMet"} inheriting from
##'     the \code{"data.frame"} class.
##'
##' @export
##'
##' @note We recommend to use the staion Ids and names given in
##'     \code{\link{stationsMF}}.
##' 
dailyMet <- function(data,
                     dateVar = "Date", 
                     metVar = "TX",
                     idVar = list("Id" = NULL),
                     station = NA,
                     id = NA,
                     trace = 0, ...) {
    
    dMet <- as.data.frame(data)

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
    if (is.na(m <- match(dateVar, names(dMet)))) {
        stop("'data' does not have the wanted 'dateVar' column") 
    }
    names(dMet)[m] <- "Date"
    
    Date <- as.Date(dMet[["Date"]])
    nr <- nrow(dMet)
    
    ## =========================================================================
    ## There may be missing in the file, we consider them as
    ## corresponding to NA values.
    ## =========================================================================
    
    dataAll <- data.frame(Date =  seq(from = Date[1], to = Date[nr], by = "day"))
    dMet <- merge(dMet, dataAll, by = "Date", all.y = TRUE)
    
    ## =========================================================================
    ## Compute the available (non-NA) periods
    ## =========================================================================

    nni <- nonNaIntervals(dMet[[metVar]])
    periods <- data.frame(Start = dMet$Date[nni$Start],
                          End = dMet$Date[nni$End])
    Duration <- as.numeric(periods$End - periods$Start) / 365.25
    periods <- data.frame(periods, Duration = round(Duration, digits = 2))
    
    if (trace) {
        cat("Non-NA intervals\n")
        print(periods)
    }

    dMet <- within(dMet,
    { Year <- as.numeric(format(Date, "%Y"));
        Day <- as.numeric(format(Date, "%j"));
        Dec <- as.factor(10 * floor(Year / 10));
        YearDec <- as.factor(Year %% 10) })
    dMet <- within(dMet, {
        DateRef <- as.Date(format(Date, "1972-%m-%d"));
        DateNum <- as.numeric(Date) / 365.25
    })
    
    dMet <- within(dMet, {
        YearW <- ifelse(as.numeric(format(Date, "%m")) <= 7,
                        paste(Year - 1, Year, sep = "-"),
                        paste(Year, Year + 1, sep = "-"));
        DayW <- ifelse(as.numeric(format(Date, "%m")) <= 7,
                       Day,
                       Day - as.numeric(format(as.Date(format(Date, "%Y-12-31")),
                                               "%j"))) 
    })
    
    ## Make the `DateRef` variable.  
    DateRefW <- dMet$DateRef
    ind <- as.numeric(format(dMet$Date, "%m")) > 7
    DateRefW[ind] <-  as.Date(format(dMet$Date[ind], "1971-%m-%d"))

    dMet <- data.frame(dMet, DateRefW = DateRefW)
    
    dMet <- within(dMet, JJA <- format(DateRef, "%m") %in% c("06", "07", "08"))
    dMet <- within(dMet, DJF <- format(DateRef, "%m") %in% c("12", "01", "02"))
    dMet
    
    attr(dMet, "metVar") <- metVar
    attr(dMet, "periods") <- periods
    attr(dMet, "station") <- station
    attr(dMet, "id") <- id
    
    class(dMet) <- c("dailyMet", "data.frame")

    dMet
    
}

## *****************************************************************************
##' Summary method
##' 
##' @title Summary Method
##'
##' @param object An object to summarize.
##'
##' @param ... Not used.
##' 
##' @return A summary object which is mainly used for print.  It can
##'     also be used to extract some pieces of information as list
##'     elements.
##'
##' @method summary dailyMet
##' @export
##'
##' @examples
##' summary(Rennes)
##' summary(Rennes)$station
##' 
summary.dailyMet <- function(object, ...) {

    y <- object[[attr(object, "metVar")]]

    ynn <- y[!is.na(y)]
    Datenn <- object$Date[!is.na(y)]
    o <- order(ynn)
    ind <- o[1:5]
    minInfo <- paste(sprintf("%6.1f", ynn[ind]), " [", Datenn[ind], "]",
                     sep = "") 
    ind <- o[length(o) - 0:4]
    maxInfo <- paste(sprintf("%6.1f", ynn[ind]), " [", Datenn[ind], "]",
                     sep = "") 
      
    res <- list("station" =  attr(object, "station"),
                "code" = attr(object, "code"),
                "metVar" = attr(object, "metVar"),
                "periods" = attr(object, "periods"),
                "metVarSummary" = summary(y),
                "minMax" = noquote(cbind(min = minInfo, max = maxInfo)))
   
    class(res) <- "summary.dailyMet"
    res
}

## *****************************************************************************
##' @title Print 
##' @param x An object with class \code{"summary.dailyMet"}
##' @param ... Not used
##' @return Nothing.
##' @method print summary.dailyMet
##' @export
##' @noRd
print.summary.dailyMet <- function(x, ...) {
    cat("o Daily Meteorological Time Series\n")
    cat(sprintf("   o Station:         \"%s\"\n", x$station))
    cat(sprintf("   o Code:            \"%s\"\n", x$code))
    cat(sprintf("   o Variable name:   \"%s\"\n", x$metVar))
    cat("\n   o Non-missing periods\n")
    print(x$periods)
    cat("\n   o Summary for variable ", x$metVar, "\n")
    print(x$metVarSummary)
    cat("\n   o Five smallest/largest obs\n")
    print(x$minMax)
}

## *****************************************************************************
##' @export
print.dailyMet <- function(x, ...) {
    print(summary(x, ...))
}
