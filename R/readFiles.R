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
##' @param station,id Optional character vectors with length one that
##'     will be attached as attributes of the object when the output
##'     class is \code{"dailyMet"}.
##' 
##' @return A data frame with the temperature \code{TX}, the date
##'     \code{Date} and several functions of the Date.
##'
##' @section Caution: For now, only ECA files for so called
##'     \emph{blended} data are used, and the variable is assumed to
##'     be a temperature. Also the \code{SOUID} field identifying the
##'     source is discarded.
##'
##' @return A data frame with the columns read in the file and
##'     possibly also those built by the \code{\link{dailyMet}}
##'     function.
##'
##' @section Caution: The station identifier \code{StatId} used by
##'     European Climate Assessment & Dataset project (ECA&D)
##'     \url{https://www.ecad.eu/} is different from the one used by
##'     Météo France. Also, ECA&D makes a distinction between
##'     \emph{Station Id} (\code{StatId}) and \emph{Source Id}
##'     (\code{SouId}).
##' 
##' @export
##' 
readECA <-  function(file, out = c("dailyMet", "data.frame"),
                     station = NA, id = NA) {

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

    if (out == "data.frame") return(Met)

    dMet <- dailyMet(data = Met, station = station, id = id)
    dMet    
        
}

## *****************************************************************************

##' Read a csv file containing a daily meterorological timeseries as
##' a column. The file must be a \code{.csv} file (\emph{Comma
##' Separated Values}) using the semi-column \code{";"} as column
##' delimiter. It is found in the directory defined by the
##' \code{metData} environment variable.
##'
##' The fields are as follows.
##' \itemize{  
##'    \item{\code{Code} }{
##'        Character: the code to be used for the station}
##'    \item{\code{Date} }{
##'        The date in POSIX format Y-m-d}
##'    \item{TX}{
##'       The temperature in Celsius.
##'    }
##'    \item{\code{Source} }{
##'       Character code giving the source of the data, useful if
##'       several sources are used in "blended" .
##'     }
##' }
##' 
##' @title Read a csv File Containing Daily Meteorological Timeseries
##'     and Add Extra Variables
##'
##' @param station A list as returned by \code{\link{findStationMF}}. 
##'
##' @param metVar The meteorological variable.
##'
##' @param out The class of the returned object.
##' 
##' @return An object inheriting from the \code{"data.frame"} class,
##'     by default a \code{dailyMet} object.
##'
##' @note The files that can be read are located in the repository
##'     defined in the system variable \code{metData}. You may need to
##'     set this variable by using \code{\link{Sys.setenv}}, possibly
##'     in a \code{Rprofile} of \code{.Renviron} file.
##' 
##' @export
##'
##' @examples
##' \dontrun{
##'    ## read data for Bordeaux-Merignac
##'    s <- findStationMF("bordeaux-mer")
##'    Met <- readMet(s)
##' }
##' 
readMet <- function(station, metVar = "TX",
                    out = c("dailyMet", "data.frame")) {
    
    out <- match.arg(out)
    fn <- sprintf("%s_%s_TX.csv", station$Id, station$Desc)

    ## XXX check the directory
    dn <- Sys.getenv("metData")

    if (dn == "") {
        stop("The `metData` environment variable is not set. ",
             "Use `Sys.setenv(metData = x)` to set it.")
    } else  if (!dir.exists(dn)) {
        stop("The `metData` environment variable does not ",
             "define an existing directory.")
    } else {     
        fn <- sprintf("%s_%s_TX.txt", station$Id, station$Desc)
        if (!file.exists(file.path(dn, fn))) {
            stop("File '", fn, "' not found in the directory ",
                 "'metData'.")
        }
    }
    df <- read.table(file.path(dn, fn), sep = ";",
                     header = TRUE,
                     colClasses = c("character", "character", "Date",
                                    "numeric"))
    if (out == "data.frame") return(df)
    
    Met <- dailyMet(df, dataVar = "Date", metVar = "TX",
                    station = station$Desc, id = station$Id)
    Met
}
