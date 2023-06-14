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

##' Create names from dates. Given dates that are distinct, the
##' function chooses the shortest format for which the formated dates
##' are distinct.
##'
##' This function is useful to name the variables in a spline basis in
##' relation with the knots given as dates. Since these functions will
##' be used to describe slowly varying trends, the knots will most
##' often be chosen in different years and the year may be sufficient
##' to identify them.
##'
##' @section Caution: in general the dates can not be retrieved from
##'     the names, so these should be stored to get maintain a full
##'     description of the basis.
##' 
##' @title Create Names from Dates
##'
##' @param date A vector that can be coerced into a \code{Date}
##'     object.
##' 
##' @return A character vector of distinct formated dates.
##'
##' @export
##'
##' @keywords internal
##' 
##' @examples
##' namesFromDates(c("1970-01-01", "1990-01-01", "1991-01-01"))
##' namesFromDates(c("1970-01-01", "1990-01-01", "1990-02-01"))
##' namesFromDates(c("1970-01-01", "1990-01-01", "1990-01-02"))
##' try(namesFromDates(c("1970-01-01", "1990-01-02", "1990-01-02")))
##' 
namesFromDates <- function(date) {
    date <- as.Date(date)
    fmts <- c("%Y", "%Y-%m", "%Y-%m-%d")
    for (fmt in fmts) {
        f <- format(date, fmt)
        OK <- !any(duplicated(f))
        if (OK) break()
    }
    if (!OK) stop("'date' should not contain duplicated values")
    f
}

##' @title Find the Name of a Function within a Namespace
##'
##' @param x A character such as \code{aPkg::aFun}.
##'
##' @return A reference that can be used as 1-st argument in
##'     \code{\link{do.call}}.
##' 
##' @author MrFlick on StackOverFlow, see
##'     \url{https://stackoverflow.com/a/38984214/8836534}
##' @export
##'
##' @keywords internal
##'
##' @examples
##' getFun("lme4::lmer")
##' 
getFun <- function(x) {
    if(length(grep("::", x)) > 0) {
        parts <- strsplit(x, "::")[[1]]
        getExportedValue(parts[1], parts[2])
    } else {
        x
    }
}


##' Create variables by using "design functions" of the date such as
##' trigonometric, polynomial, splines, ... 
##'
##' By "design function" we a function building a "design matrix"
##' \code{X} with its columns being functions \eqn{X_i(t)} where
##' \eqn{t} is the time (actually a date). Each row of the matrix
##' corresponds to a value of the time. It is assumed that \emph{the
##' time is the first formal argument of the function}. The name of
##' this argument is arbitrary. This allows to use design functions
##' with different names fot the 1-st argument such as \code{"t"},
##' \code{"date"}, \code{"Date"}, ...
##' 
##' Any design function used should fulfill the following
##' requirements.
##' 
##' \enumerate{
##'     \item The first argument must be the date. The name of this
##'       argument will not be used in the call.
##'     \item
##'        The returned value should be an object inheriting from the
##'        \code{"matrix"} class with suitable column names. For
##'        compatibility reasons, it can also be a list containing the
##'        design matrix as its element named \code{"X"}.
##' }
##' The call to a design function should mention \code{Date} as the
##' first argument. This call refers to the \code{dailyMet} object
##' used, from which the \code{Date} column will be used.
##'
##'
##' @param designList A list with a specific structure describing the
##'     design functions to evaluate, and the arguments (except the
##'     date) to use. Each element of the list is itself a list with
##'     the two elements named \code{what} and \code{args} as
##'     \code{\link{do.call}}. The element \code{what} contains the
##'     name of the design function and the element \code{args}
##'     contains the named list of values for the arguments of
##'     \code{what}, \emph{with the first argument omitted}.
##'
##' @param dt A "date" vector on which the design functions will be
##'     evaluated. This should be an object with class \code{"Date"}
##'     or an object that will be coerced to this class e.g. a
##'     character vector with a suitable format.
##'
##' @param trace Integer level of verbosity 
##' 
##' @param dropDup Character giving the names of variables that must
##'     be dropped when they are duplicated across designs.
##' 
##' @return A data frame with \code{length(dt)} rows containing the
##'     new variables.
##'
##' @section Caution: When several designs are used, some conflit may
##'     arise because some names in the design matrices are
##'     identical. Unless the corresponding names are in
##'     \code{dropDup}, the latest variable will be automatically
##'     renamed as in \code{\link{data.frame}}. For instance if two
##'     variables \code{"t1"} then the second one will be renamed
##'     \code{"t1.1"}, see \bold{Examples}. This is a possible source
##'     of confusion, and good options could be either to drop this
##'     variable or to wrap the design function into another function
##'     which uses different column names.
##'
##' @note The naming of the elements of the list given in
##'     \code{designList} is arbitrary and does not impact the result.
##'     The names can be used to make code clearer.
##' 
##' @export
##' 
##' @examples
##'
##' if (require(NSGEV)) {
##'   desList <-
##'       list("sinAndCos" = list(what = "tsDesign",
##'                               args = list(type = "trigo", df = 7)),
##'            "polys" = list(what = "tsDesign",
##'                           args = list(type = "polynom", df = 3)),
##'            "breaks" = list(what = "NSGEV::breaksX",
##'                            args = list(breaks = c("1970-01-01", "1990-01-01"))))
##'
##'   des1 <- designVars(desList, dt = c("2021-01-01", "2021-01-02"), trace = 1)
##'   des2 <- designVars(desList, dt = Rennes$Date, trace = 1)
##'   des1
##'   head(des2)
##' }
designVars <- function(designList, dt, trace = 0, dropDup = "Cst") {

    dt <- as.Date(dt)
    
    for (i in seq_along(designList)) {
        ell <- designList[[i]]
        oWhat <- ell[[1]]
        what <- getFun(ell[[1]])
        argNm <- names(formals(what))[1]
        if (trace) {
            cat(sprintf("   o evaluation of `%s`. Date is added in 1-st arg `%s`\n",
                        oWhat, argNm))
        }
        ## ell[[1]] <- NULL
        ell <- ell[[2]]
        ell <- c(list(dt), ell)
        
        X <- do.call(what = what, args = ell)

        if (inherits(X, "matrix")) {
            X <- unclass(X)
        } else if (inherits(X, "list")) {
            if (inherits(X$X, "matrix")) {
                X <- X$X
            } else {
                stop("Unsuitable result from design function")
            }
        }
        if (i == 1) {
            Xall <- data.frame(X)
        } else {
            nc <- ncol(X)
            ind <- colnames(X) %in% intersect(colnames(Xall), dropDup)  
            if (any(ind)) X <- X[ , -(1:nc)[ind]]
            Xall <- data.frame(Xall, X)
        }
        
    }
    
    Xall
    
}

##' @title Rough Check for a List of Designs
##'
##' @param designList A list of (definitions of) designs as can be
##'     used in \code{\link{designVars}}.
##'
##' @param dt Date or time to check the calls. 
##'
##'
##' @param dropDup
##'
##' @return \code{TRUE} if the check is OK.
##'
##' @keywords internal
##'
##' @export
##' 
##' @examples
##' dl <- list("breaks" = list(what = "NSGEV::breaksX",
##'             args = list(breaks = c("1970-01-01", "1990-01-01"))))
##' checkDesign(dl)
##' dl <- list("breaks" = list(what = "NSGEV::breaksX",
##'             foo = list(breaks = c("1970-01-01", "1990-01-01"))))
##' checkDesign(dl)
##' 
checkDesign <- function(designList, dt, dropDup = "Cst") {

    looksDes <- function(x) {
        is(x, "list") && setequal(names(x), c("what", "args"))
    }
    
    if (!is.list(designList) || !all(sapply(designList, looksDes))) {
        return(paste0("Bad design list: expect a named list with names ",
                      "\"what\" and \"args\""))
    }
    
    TRUE
    
}
