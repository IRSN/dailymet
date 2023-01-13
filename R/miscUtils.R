formatPerc <- function(x,
                       digits = max(2L, getOption("digits")),
                       probability = TRUE, 
                       use.fC = length(x) < 100,
                       ...) {
    if (length(x)) {
        if (probability)  x <- 100 * x
        paste0(if (use.fC) 
                   formatC(x, format = "fg", width = 1, digits = digits)
               else format(x, trim = TRUE, digits = digits, ...), "%")
    }
    else character(0)
}

## ****************************************************************************
##' Compute clusters on the basis of a three-day period to separate
##' clusters.
##'
##' The cluster are sequences of successive dates such that: (1) any
##' exceedance falls in a cluster, (2) If two exceedances are
##' separated by three consecutive dates of non-exceedance, then they
##' are in different clusters. A \code{NA} value is assumed to be a
##' non-exceedance.
##' 
##' @title Compute Clusters
##'
##' @param Date A \code{Date} object.
##'
##' @param y A numeric vector representing a daily time series. Must
##'     have the same length as \code{Date}.
##'
##' @param u The threshold (vector). If \code{u} has length one, it
##'     will be recycled to reach the length of \code{y} and
##'     \code{Date}.
##' 
##' @return A list with the results.
##'
##' @section Caution: A quite unreliable function, which will not work
##'     when there are exceedances at the ends. There exists much
##'     better function that can be used for declustering.
##'
##' @export
##' 
##' @examples
##' data(Fort)
##' Fort <- within(subset(Fort, year == 1999), {
##'    Prec <- 2.54 * Prec;
##'    Date <- as.Date(sprintf("%4d-%02d-%02d", year, month, day));
##' })
##' u0 <- quantile(Fort$Prec, prob = 0.97)
##' cl <- clusters3(Date = Fort$Date, y = Fort$Prec, u = u0)
##' plot(Prec ~ Date, data = Fort, type = "h", lwd = 2)
##' abline(h = u0, col = "SpringGreen3")
##' abline(v = cl$DateClust , col = "orangered")
clusters3 <- function(Date, y, u) {
    
    n <- length(y)
    
    if (length(Date) != n) {
        stop("'Date' and 'y' must have the same length")
    }

    if (length(u) == 1) u <- rep(u, length.out = length(y))
        
    Prec <- FALSE
    start <- end <- integer(0)

    ## 'x' contains the states of the current day, the day before, ...
    ## upt to the wanted numbers here 4.
    
    x <- c(FALSE, FALSE, FALSE, FALSE)
    X <- matrix(NA, nrow = n, ncol = 4,
                dimnames = list(NULL, paste0("x", 1:4)))
    clust <- FALSE
    
    for (i in 1:n ) {
        xBak <- x[1]
        
        if (!is.na(y[i]) && (y[i] > u[i])) {
            if (!x[3] && !x[2] && !x[1]) {
                start <- c(start, i)
            }
            x[1] <- TRUE
        } else {
            x[1] <- FALSE
        }
        
        if (x[4] && !x[3] && !x[2] && !xBak) {
            end <- c(end, i - 4)
        }
        
        x[4] <- x[3]
        x[3] <- x[2]
        x[2] <- xBak
        X[i, ] <- x
        
    }

    if (length(end) == length(start) - 1) {
        end <- c(end, n)
    }
    
    if (length(start) != length(end)) {
        stop("'start' and 'end' do not have the same length")
    }

    for (i in seq_along(start)) {
        ind <- start[i]:end[i]
        im <- which.max(y[ind] - u[ind])
        if (i == 1) {
            IndClust <- ind[im]
            DateClust <- Date[ind[im]]
            ExceedClust <- max(y[ind] - u[ind])
        } else {
            IndClust <- c(IndClust, ind[im])
            DateClust <- c(DateClust, Date[ind[im]])
            ExceedClust <- c(ExceedClust, max(y[ind] - u[ind]))
        }
    }
    
    list(start = start, end = end,
         DateClust = DateClust,
         ExceedClust = ExceedClust,
         IndClust = IndClust)
         ## X = X)
    
}

## *****************************************************************************
##' Extract a time window in year from a data frame.
##'
##' Based on the column \code{Day} in the data frame, and the give
##' Julian day for the center we extract the corresponding
##' observations. This function is useful e.g., to investigate the
##' form of the seasonality on a parameter.
##'
##' @title Extract a Time Window in Year from a Data Frame.
##' 
##' @param data The data frame. Must have a \code{Day} column giving
##'     the Julian day.
##' 
##' @param j The center Julian day. Can be given as an integer between
##'     1 and 365 or as character string such as \code{"07-21"} for
##'     the 21-th of July.
##'
##' @param sw Semi-width (in days) of the sliding period). So \code{sw
##'     = 45} means that the fit is made on a period of 90 days.
##'
##' @return A subset data frame.
##'
##' @export
##' 
##' @examples
##' df <- seasCenter(Rennes, j = "07-21")
##' sort(unique(format(df$Date, "%m-%d")))
##' 
seasCenter <- function(data, j, sw = 45) {

    Day <- NULL
    
    if (!("Day" %in% names(data)) || any(data$Day < 1) ||
        any(data$Day > 366)) {
        stop("'data' must contain a \"Day\" column ",
             "giving the day in year a.k.a. 'Julian day' ",
             "values between 1 and 366.")
    }
    
    if (sw > 160) stop("'sw' can not exceed 160") 
    
    if (is.character(j)) {
        j <- as.numeric(format(as.Date(paste0("1970-", j)) , "%j"))
    }
    left <- j - sw
    right <- j + sw
    if (left < 0) {
        dfPer <- subset(data, Day > left + 365 | Day < right)
    } else if (right > 365) {
        dfPer <- subset(data, Day > left | Day < right - 365)
    } else {
        dfPer <- subset(data, Day > left & Day < right)
    }
    dfPer
}

##'
##' Compute the begining and the end of each non-NA interval.
##'
##' @title Compute the Beginning and the End of Each Non-NA Interval
##'
##' @param y A numeric vector assumed to represent the values of a
##'     regularly sampled time series.
##' 
##' @return A list of two vectors \code{start} and \code{end}.
##'
##' @keywords internal
##' 
nonNaIntervals <- function(y) {
    rles <- rle(is.na(y))
    rl <- rles$lengths
    rv <- rles$values
    crl <- cumsum(rl)
    ## cat("length(rl) = ", length(rl), "\n")
    ## start with a missing
    if (length(rl) == 1) {
        return(list(Start = 1, End = rl))
    }
    nrv <- length(rv)
    if (rv[1]) {
        ind <- seq(from = 1, to = length(rl), by = 2)
        start <- crl[ind] + 1
        end <- crl[ind + 1]
    } else {
        ## missing periods correspond to indices 2, 4, ...
        ## 
        ind <- seq(from = 2, to = length(rl), by = 2)
        crle <- crl[ind] + 1
        crle <- crle[crle <= length(y)]
        start <- c(1, crle)
        ind <- seq(from = 1, to = length(rl), by = 2)
        end <- crl[ind]
    }

    list(Start = start, End = end)
}

##' Find the last full year in a character vector representing
##' successive dates.
##'
##' By "full" year, we mean a year with at least 365 days.
##' 
##' @title Find the Last Full Year
##'
##' @param dateTxt A character vector giving a date in POSIX format.
##'
##' @param leap Logical if \code{TRUE}, the last full leap year
##' will be returned.
##'
##' @param out Character. When given the value \code{"character"}, a
##'     character vector extracted from \code{dataeTxt} is
##'     returned. When the value \code{logical} is given, a logical
##'     vector is returned, indicating the elements in \code{dateTxt}.
##' 
##' @return A logical vector that can be used to subset. 
##' 
lastFullYear <- function(dateTxt, leap = FALSE,
                         out = c("character", "logical")) {
    out <- match.arg(out)
    Year <- gsub("-.*$", "", dateTxt)
    tY <- table(Year)
    
    ## This may be wrong if the last year is a leap
    ## year with its last day missing... 
    if (leap) {
        ind1 <- (1:length(tY))[tY == 366]
    } else {
        ind1 <- (1:length(tY))[tY >= 365]
    }
    ind <- (Year == names(tY)[max(ind1)])

    if (out == "character") return(dateTxt[ind])
    else return(ind)
                                   
}
