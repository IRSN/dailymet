## ****************************************************************************
##' Read a file as provided by the ECA Data  with \code{StatId}
##'
##' @title Read an ECA File and Add Extra Varables
##'
##' @param file The file to read as in \code{\link{read.table}}.
##'
##' @param out The class of the returned object. The \code{"dailyMet"}
##'     S3 class with creator \code{\link{dailyMet}} extends the
##'     \code{data.frame} by attaching some information to the data as
##'     attributes. This information inclues the name of the
##'     meteorological variable.
##'
##' @param station,code Optional character vectors with length one
##'     that will be attached to the object when the output class is
##'     \code{"dailyMet"}.
##' 
##' @return A data frame with the temperature \code{TX}, the date
##'     \code{Date} and several functions of the Date.
##'
##' @section Caution: For now, only ECA files for so called
##'     \emph{blended} data are used, and the variable is assumed to
##'     be a temperature. Also the \code{SOUID} field identifying the
##'     source is discarded. 
##'
##' @return A data frame with the columns
##'     \itemize{
##'         \item{Date, DateRef }{
##'            The date and the "refrence" date (corresponding to
##'            the same month and day) in a reference leap year chosen
##'            as 1972, since 1970 was not a leap year.
##'          }
##'          \item{TX }{
##'            The temperature.
##'          }
##'          \item{Dec, YearDec }{
##'
##'             Factors related to the decade as required in
##'             plots. \code{Dec} is the decade, and \code{YearDec} is
##'             the year in the decade with values from \code{"0"} to
##'             \code{"9"}.
##'
##'          }
##'          \item{YearW, DayW }{
##'             "Winter" year and day. The winter year \code{YearW} is
##'              a character variable with values such as
##'              \code{"2022-23"}. The winter day is an integer
##'              between 1 (for the 1-st of August) and 366 (for the
##'              31-th of July in a leap year).
##'          }
##'          \item{JJA, DJF }{
##'              Logical variables for the periods in year
##'              \emph{June-July-August} and
##'              \emph{December-January-February}. }
##'     }
##' 
##' @export
##' 
readECA <-  function(file, out = c("dailyMet", "data.frame"),
                     station = NA, code = NA) {

    out <- match.arg(out)
    Met <- read.table(file,
                      sep = ",",
                      colClasses = c("numeric", "NULL", rep("numeric", 3)),
                      na.strings = "-9999",
                      header = TRUE, skip = 19)
    
    colnames(Met) <- c("StatId", "Date", "TX", "Code")
    Met <- within(Met, {
        Date <- as.Date(as.POSIXct(strptime(Date, "%Y%m%d"), tz = "UTC"));
        TX <- TX / 10
    })

    if (FALSE) {
        Met <- within(Met,
        { Year <- as.numeric(format(Date, "%Y"));
            Day <- as.numeric(format(Date, "%j"));
            Dec <- as.factor(10 * floor(Year / 10));
            YearDec <- as.factor(Year %% 10) })
        Met <- within(Met, {
            DateRef <- as.Date(format(Date, "1972-%m-%d"));
            DateNum <- as.numeric(Date) / 365.25
        })
        
        Met <- within(Met, {
            YearW <- ifelse(as.numeric(format(Date, "%m")) <= 7,
                            paste(Year - 1, Year, sep = "-"),
                            paste(Year, Year + 1, sep = "-"));
            DayW <- ifelse(as.numeric(format(Date, "%m")) <= 7,
                           Day,
                           Day - as.numeric(format(as.Date(format(Date, "%Y-12-31")),
                                                   "%j"))) 
        })
        
        DateRefW <- Met$DateRef
        ind <- as.numeric(format(Met$Date, "%m")) > 7
        DateRefW[ind] <-  as.Date(format(Met$Date[ind], "1971-%m-%d"))
        
        Met <- data.frame(Met, DateRefW = DateRefW)
        
        Met <- within(Met, JJA <- format(DateRef, "%m") %in% c("06", "07", "08"))
        Met <- within(Met, DJF <- format(DateRef, "%m") %in% c("12", "01", "02"))
        Met
    }

    if (out == "data.frame") return(Met)

    dMet <- dailyMet(data = Met, station = station, code = code)
    
    dMet    
        
    
}
